using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using BusinessLayer.VisualBasicVba;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using CodeVortoJobQueueProcessingApi.ControllerHelperClasses;
using DataAccessLayer;
using MySql.Data.MySqlClient;
using System;
using System.Collections.Generic;
using System.Data;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class VbaProcessingVersion1Controller
    {
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();
        private readonly VisualBasicVba1 _visualBasicVba1 = new VisualBasicVba1();
        private readonly VbaProcessingHelper _vbaProcessingHelper = new VbaProcessingHelper();

        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActionWorkflows(int projectId)
        {
            try
            {
                var stringBuilder = new StringBuilder();
                //stringBuilder.AppendLine("Started process for:ProcessAllActionWorkflows: " + projectId + ")");
                _codeVortoService = new CodeVortoService();
                var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                int projectType = projectMasterData.ProjectConfigType;
                if (projectType != 10) return Ok("Extracted all action workflows for Project Id: " + projectId);

                var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectConfigData = projectConfig.GetItem(projectType);
                if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                // Means the project type is windows application and we need to read starting point from that 
                // files respective code behind file...
                string configFileName = projectConfigData.ToString();
                if (string.IsNullOrEmpty(configFileName)) return Ok("Process completed successfully");
                var allConfigFiles =
                    await _codeVortoService.FileMasterRepository.GetAllListItems(
                        f => f.FilePath.EndsWith(configFileName) && f.ProjectId == projectId).ConfigureAwait(false);
                stringBuilder.AppendLine("Total files scaned: " + allConfigFiles.Count());
                foreach (var cFile in allConfigFiles)
                {
                    stringBuilder.AppendLine("Started reading file lines file name: " + cFile.FileName +
                                             " and file path is: " + cFile.FilePath);
                    var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(cFile.FilePath);
                    var allLines = File.ReadAllLines(cFile.FilePath).ToList();
                    allLines = allLines.Select(l => l.Trim()).ToList();
                    allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && !l.StartsWith("'")).ToList();
                    stringBuilder.AppendLine("started executing next process: GetFormCaption: (" + cFile.FileName + ")");
                    string formCaption = _visualBasicVba1.GetFormCaption(allLines);
                    List<string> subFormsList = new List<string>();
                    if (fileNameWithoutExtension == null) continue;
                    string className = fileNameWithoutExtension;
                    subFormsList.Add(className);
                    stringBuilder.AppendLine("started executing next process: GetSubForms(" + cFile.FileName + ")");
                    List<string> subForms = _visualBasicVba1.GetSubForms(allLines);
                    subFormsList.AddRange(subForms);
                    stringBuilder.AppendLine("Subforms: " + string.Join(",", subFormsList));
                    if (!string.IsNullOrEmpty(formCaption))
                        className = formCaption;

                    foreach (var subForm in subFormsList.Where(s => !string.IsNullOrEmpty(s)))
                    {
                        string sForm = subForm;
                        if (sForm.Contains('.'))
                            sForm = sForm.Split('.').LastOrDefault();
                        if (string.IsNullOrEmpty(sForm)) continue;

                        var genericBlocks = GetAllMethodsForClass(sForm, projectId);

                        foreach (var statement in genericBlocks)
                        {
                            var stat = statement.OriginalStatement;

                            if (statement.MethodName.StartsWith("FloKapture_Temp_1234")
                                || stat.StartsWith("Private Sub FloKapture_Temp_1234")) continue;

                            string methodCaption = string.Empty; // statement.OriginalStatement;
                            if (stat.Contains("_DblClick") || stat.Contains("_Click") || stat.Contains("_Load"))
                            {
                                stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                                         statement.StatementId + ",8,9)");
                                var clickIngnoreCode = GetGenericBlock(statement.StatementId, 8, 9);
                                var checkCancelStatemet =
                                    clickIngnoreCode.Exists(s => s.OriginalStatement == "Cancel = True");
                                if (checkCancelStatemet)
                                    continue;
                            }

                            string methodName = statement.MethodName;
                            if (string.IsNullOrEmpty(methodName))
                                methodName = stat.Split(new[] { "Sub" }, StringSplitOptions.None).LastOrDefault();
                            if (!string.IsNullOrEmpty(methodName))
                            {
                                string controlName = methodName.Trim().Split('_').FirstOrDefault();
                                stringBuilder.AppendLine("Started executing next process: FindCaption(" +
                                                         cFile.FileName + ")");
                                methodCaption = _visualBasicVba1.FindCaption(controlName, allLines);
                            }

                            if (!cFile.FileName.StartsWith("sub"))
                            {
                                var actionWorkflow = new ActionWorkflows
                                {
                                    ActionWorkflowId = 0,
                                    CreatedBy = 1,
                                    EndPointOrService = "Service",
                                    MethodStatementId = statement.StatementId,
                                    OriginFileName = Path.GetFileName(cFile.FilePath),
                                    OriginFilePath = cFile.FilePath,
                                    ProjectId = projectId,
                                    OriginEventMethod = methodCaption,
                                    OriginObject = fileNameWithoutExtension.Trim(),
                                    WorkflowName = statement.MethodName,
                                    ServiceBaseAddress = null,
                                    ServiceContract = null,
                                    WorkflowBusinessName = className.Trim(),
                                    Processed = 0,
                                    FileId = cFile.FileId
                                };
                                await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                            }

                            var statement1 = statement;
                            var statementMaster = await
                                _codeVortoService.StatementReferenceMasterRepository
                                    .GetAllListItems(s => s.StatementId == statement1.StatementId);
                            foreach (var stateMaster in statementMaster)
                            {
                                stateMaster.AlternateName = string.IsNullOrEmpty(methodCaption)
                                    ? statement.OriginalStatement
                                    : methodCaption;
                                stateMaster.FileMaster = null;
                                stateMaster.ReferenceFileMaster = null;
                                await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateMaster);
                            }
                        }

                        if (string.IsNullOrEmpty(fileNameWithoutExtension)) continue;

                        var classStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.ClassNameDeclared == fileNameWithoutExtension)
                            .ConfigureAwait(false);
                        string alternateName = string.IsNullOrEmpty(formCaption)
                            ? fileNameWithoutExtension
                            : formCaption;
                        foreach (var sMaster in classStatementMaster)
                        {
                            var split = Regex.Replace(alternateName, "(?<=[a-z])([A-Z])", " $1",
                                RegexOptions.Compiled);
                            split = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(split.ToLower());
                            sMaster.AlternateName = split;
                            sMaster.ReferenceFileMaster = null;
                            sMaster.FileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sMaster)
                                .ConfigureAwait(false);
                        }
                    }
                }
                // Update alternate name for forms other that Form / Config files...

                stringBuilder.AppendLine("Started process update altername name: (" + projectId + ")");
                var allOtherFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(s => s.ProjectId == projectId && s.FileTypeExtensionId != 14)
                        .ConfigureAwait(false);
                foreach (var remainingFile in allOtherFiles)
                {
                    var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(remainingFile.FilePath);
                    var allLines = File.ReadAllLines(remainingFile.FilePath).ToList();
                    allLines = allLines.Select(l => l.Trim()).ToList();
                    allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && !l.StartsWith("'")).ToList();
                    string formCaption = _visualBasicVba1.GetFormCaption(allLines);
                    if (string.IsNullOrEmpty(fileNameWithoutExtension)) continue;

                    var classStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.ClassNameDeclared == fileNameWithoutExtension)
                        .ConfigureAwait(false);
                    string alternateName = string.IsNullOrEmpty(formCaption)
                        ? fileNameWithoutExtension
                        : formCaption;
                    foreach (var sMaster in classStatementMaster)
                    {
                        var split = Regex.Replace(alternateName, "(?<=[a-z])([A-Z])", " $1", RegexOptions.Compiled);
                        split = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(split.ToLower());
                        sMaster.AlternateName = split;
                        sMaster.ReferenceFileMaster = null;
                        sMaster.FileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sMaster)
                            .ConfigureAwait(false);
                    }
                }
                stringBuilder.AppendLine("Started executing next process: ProcessObjectConnectivityDiagram(" + projectId +
                                         ")");
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            // IHttpActionResult processResult = await ProcessForObjectConnectivityDiagramData(projectId);
            //var processResult = await ProcessObjectConnectivityDiagram(projectId);
            //await processResult.ExecuteAsync(CancellationToken.None);
            return Ok("Extracted all action workflows for Project Id: " + projectId);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine("Started to collect all actionworkflow for project: (" + projectId + ")");
                    var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                        .GetDataFromSqlQuery<ActionWorkflows>(
                            " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                            "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') AND Processed = 0 ; ");

                    if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    workflowRef[0].ProjectMaster = projectMaster;
                    var lstWorkflowRef = workflowRef.ToList();

                    foreach (var workflow in lstWorkflowRef)
                    {
                        if (workflow.ProjectId == null) return NotFound();
                        int workFlowProjectId = workflow.ProjectId.Value;
                        stringBuilder.AppendLine("Started executing next process: GetWorkFlowWorkSpace(" + projectId +
                                                 ")");

                        var processResult = await GetWorkFlowWorkSpace(workFlowProjectId, workflow.MethodStatementId,
                                    workflow.WorkflowBusinessName);
                        await processResult.ExecuteAsync(CancellationToken.None);
                        // Processed == 1
                        workflow.Processed = 1;
                        workflow.ProjectMaster = null;
                        workflow.FileMaster = null;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow);
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                stringBuilder.AppendLine("Started executing next process: ApplyPseudoCodeConversion(" + projectId +
                                                 ")");
                LogMessage.WriteLogMessage(stringBuilder);
                // var processPseudocode = ApplyPseudoCodeConversion(projectId);
                // await processPseudocode.ExecuteAsync(CancellationToken.None);
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, int stmtId, string workflowBusinessName)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    #region Start pre-process
                    List<TreeView> lstTreeView = new List<TreeView>();
                    List<TreeView> secondTab = new List<TreeView>();
                    var startClasses =
                        await _codeVortoService.ActionWorkflowsRepository
                            .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
                    var clsName =
                        (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList()
                            .First();
                    Expression<Func<StatementReferenceMaster, bool>> expression =
                        master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName);
                    var baseStatementMaster = await
                        _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(
                            expression, 1);
                    stringBuilder.AppendLine("Started process for get block: GetMethodBlock(" + stmtId + ")");
                    var workflowRef = GetMethodBlock(stmtId);
                    int bId = workflowRef[0].BaseCommandId;
                    int statementId = workflowRef[0].StatementId;
                    int treeNodeId = 1;
                    // This is class name where that method is defined...
                    string workflowBuiName = string.IsNullOrEmpty(workflowBusinessName)
                        ? baseStatementMaster.OriginalStatement
                        : workflowBusinessName;
                    lstTreeView.Add(new TreeView
                    {
                        GraphId = "StartNode_1",
                        GraphName = "<span class='nodeToBold'>" + workflowBuiName + "</span>",
                        HasChild = true,
                        ParentId = "-1",
                        BaseCommandId = baseStatementMaster.BaseCommandId,
                        StatementReferenceMaster = workflowRef[0],
                        SpriteCssClass = string.IsNullOrEmpty(workflowBusinessName) ? clsName : workflowBusinessName,
                        ActualStatementId = "Actual_" + baseStatementMaster.StatementId,
                        NodeId = treeNodeId,
                        CanBeParent = true,
                        IndentLevel = 0
                    });
                    // This is Method start statement...
                    lstTreeView.Add(new TreeView
                    {
                        GraphId = "MethodNode_" + statementId,
                        GraphName = "<span class='nodeToBold'>" + workflowRef[0].OriginalStatement + "</span>",
                        HasChild = true,
                        ParentId = "StartNode_1",
                        BaseCommandId = bId,
                        StatementReferenceMaster = workflowRef[0],
                        ActualStatementId = "Actual_" + statementId,
                        NodeId = ++treeNodeId,
                        CanBeParent = true,
                        IndentLevel = 1
                    });
                    workflowRef.RemoveAt(0);

                    lstTreeView.AddRange(workflowRef.Select(statementMaster => new TreeView
                    {
                        ActualStatementId = "Actual_" + statementMaster.StatementId,
                        GraphId = "Node_" + statementMaster.StatementId,
                        GraphName = statementMaster.OriginalStatement,
                        HasChild = false,
                        SpriteCssClass = "",
                        ParentId = "MethodNode_" + statementId,
                        BaseCommandId = statementMaster.BaseCommandId,
                        PrimaryCommandId = statementMaster.PrimaryCommandId,
                        ClassCalled = statementMaster.ClassCalled,
                        MethodCalled = statementMaster.MethodCalled,
                        StatementReferenceMaster = statementMaster,
                        NodeId = ++treeNodeId,
                        CanBeParent = true,
                        IndentLevel = 2
                    }));
                    var copyOfLstTreeView = new List<TreeView>();
                    copyOfLstTreeView.AddRange(lstTreeView);
                    int auto = 0;
                    int indentLevel = 3;
                    var callingAndCalled = new Dictionary<string, List<string>>();
                    foreach (var treeItem in lstTreeView)
                    {
                        try
                        {
                            if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                                continue;

                            auto++;
                            stringBuilder.AppendLine("Started process for called internal: GetCallInternalDetails(" +
                                                     projectId + "," + treeItem.StatementReferenceMaster.FileId +
                                                     ")");
                            copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView,
                                projectId,
                                treeItem.StatementReferenceMaster.FileId, indentLevel, ref auto, ref treeNodeId,
                                ref callingAndCalled);
                            if (!copyOfLstTreeView.Any()) continue;
                            treeItem.HasChild = true;
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    var newList = copyOfLstTreeView.ToList();
                    foreach (var treeItem in newList)
                    {
                        try
                        {
                            if (treeItem.BaseCommandId != 6) continue;
                            // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                            if (!string.IsNullOrEmpty(treeItem.StatementReferenceMaster.StatementComment)
                                &&
                                treeItem.StatementReferenceMaster.StatementComment.StartsWith("FloKapture_Temp_1234_")
                                && treeItem.StatementReferenceMaster.BusinessName.StartsWith("FloKapture Adjustment"))
                                continue;

                            auto++;
                            stringBuilder.AppendLine(
                                "Started process for called external: GetCallExternalDetails project: (" +
                                projectId + ")");
                            copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView,
                                projectId,
                                indentLevel, ref auto, ref treeNodeId, ref callingAndCalled);
                            if (!copyOfLstTreeView.Any()) continue;
                            treeItem.HasChild = true;
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    #region Extra blocks...

                    var indexPosition = -1;
                    //lstTreeView = copyOfLstTreeView.ToList();
                    int ifCounter = 0;
                    string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                    int tempId = 0;
                    int groupId = 1;
                    foreach (
                        var treeItem in
                            copyOfLstTreeView.FindAll(t => t.BaseCommandId == 6 || t.BaseCommandId == 5).ToList())
                    {
                        try
                        {
                            treeItem.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeItem.GraphName +
                                                 "</span>";
                            var childItems =
                                (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
                            foreach (var child in childItems)
                            {
                                if (tempId > 3)
                                    tempId = 0;
                                if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
                                {
                                    child.IndentLevel = child.IndentLevel + 1;
                                    string groupName = string.Empty;
                                    if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                                    {
                                        child.StatementReferenceMaster.MethodCalled =
                                            child.StatementReferenceMaster.MethodCalled + "()";
                                        string iOreCall = child.BaseCommandId == 6 ? "External Call" : "Internal Call";
                                        string methodCalled = child.StatementReferenceMaster.MethodCalled.Substring(0,
                                            child.StatementReferenceMaster.MethodCalled.IndexOf('('));
                                        string bName = child.StatementReferenceMaster.BusinessName;
                                        if (string.IsNullOrEmpty(bName))
                                            bName = methodCalled;

                                        groupName = "(" + bName.Trim() + ")(" +
                                                    methodCalled + ")(" + iOreCall + ")";
                                        groupId = groupId + 1;
                                    }
                                    treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] +
                                                         ";'>" +
                                                         treeItem.GraphName +
                                                         "</span>";
                                    stringBuilder.AppendLine(
                                        "Started process for assign color to childnodes: AssignColorsToChildNodes(" +
                                        projectId +
                                        ")");
                                    AssignColorsToChildNodes(child, ref copyOfLstTreeView, colorArray[tempId], groupName,
                                        groupId);
                                    tempId++;
                                }
                                else
                                    child.GraphName = "<span style='color: green;'>" + child.GraphName +
                                                      "</span>";
                            }
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                    copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                    var allSeqListItemsNew = new List<TreeView>();
                    foreach (var curItem in copyOfLstTreeView)
                    {
                        stringBuilder.AppendLine("Started process for attchchilditems: AttachChildItems(" + projectId +
                                                 ")");
                        allSeqListItemsNew.Add(curItem);
                        var childItems =
                            (from s in copyOfLstTreeView where s.ParentId == curItem.GraphId select s).ToList();
                        foreach (var cItem in childItems)
                        {
                            allSeqListItemsNew = AttachChildItems(allSeqListItemsNew, copyOfLstTreeView, cItem);
                        }
                        break;
                    }

                    lstTreeView = allSeqListItemsNew.ToList();
                    foreach (var treeItem in allSeqListItemsNew)
                    {
                        indexPosition++;
                        if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                        if (treeItem.BaseCommandId != 1 && treeItem.PrimaryCommandId != 1) continue;

                        var treeViewList = new List<TreeView>();
                        for (int i = indexPosition; i < lstTreeView.Count; i++)
                        {
                            treeViewList.Add(lstTreeView[i]);
                            if (lstTreeView[i].BaseCommandId == 1 || lstTreeView[i].PrimaryCommandId == 1)
                                ifCounter++;
                            if (lstTreeView[i].BaseCommandId == 2 || lstTreeView[i].PrimaryCommandId == 2)
                                ifCounter--;
                            if (ifCounter == 0)
                                break;
                        }
                        var prevParentId = treeViewList.First().ParentId;
                        var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
                        treeViewList.First().GraphId = graphId;
                        for (int j = 1; j < treeViewList.Count; j++)
                        {
                            if (treeViewList[j].ParentId != prevParentId) continue;
                            treeViewList[j].ParentId = graphId;

                            treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                        }
                    }


                    indexPosition = -1;
                    int loopCounter = 0;
                    foreach (var treeItem in allSeqListItemsNew)
                    {
                        indexPosition++;
                        if (treeItem.BaseCommandId != 3) continue;

                        var treeViewList = new List<TreeView>();
                        for (int i = indexPosition; i < lstTreeView.Count; i++)
                        {
                            treeViewList.Add(lstTreeView[i]);
                            if (lstTreeView[i].BaseCommandId == 3)
                                loopCounter++;
                            if (lstTreeView[i].BaseCommandId == 4)
                                loopCounter--;
                            if (loopCounter == 0)
                                break;
                        }
                        int curIndentLevel = treeViewList.First().IndentLevel;
                        var prevParentId = treeViewList.First().ParentId;
                        var graphId = "LoopStart" + indexPosition + treeItem.ActualStatementId;
                        treeViewList.First().GraphId = graphId;
                        treeViewList.First().IndentLevel = curIndentLevel + 1;
                        for (int j = 1; j < treeViewList.Count; j++)
                        {
                            if (treeViewList[j].ParentId != prevParentId) continue;
                            treeViewList[j].ParentId = graphId;

                            treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                        }
                    }


                    indexPosition = -1;
                    foreach (var treeItem in allSeqListItemsNew)
                    {
                        indexPosition++;
                        if (treeItem.BaseCommandId != 10) continue;
                        int endIfCounter = -1;

                        var treeViewList = new List<TreeView>();
                        for (var i = indexPosition; i < lstTreeView.Count; i++)
                        {
                            treeViewList.Add(lstTreeView[i]);
                            if (lstTreeView[i].BaseCommandId == 1)
                                endIfCounter--;
                            if (lstTreeView[i].BaseCommandId == 2)
                                endIfCounter++;
                            if (endIfCounter == 0)
                                break;
                        }
                        int curIndentLevel = treeViewList.First().IndentLevel;
                        var prevParentId = treeViewList.First().ParentId;
                        var graphId = "ElseBlock" + indexPosition + treeItem.ActualStatementId;
                        treeViewList.First().GraphId = graphId;
                        treeViewList.First().IndentLevel = curIndentLevel + 1;
                        for (var j = 1; j < treeViewList.Count; j++)
                        {
                            if (treeViewList[j].BaseCommandId == 2 || treeViewList[j].BaseCommandId == 9) continue;
                            if (treeViewList[j].ParentId != prevParentId) continue;

                            treeViewList[j].ParentId = graphId;
                        }
                    }

                    allSeqListItemsNew = allSeqListItemsNew.DistinctBy().ToList();

                    secondTab.Add(lstTreeView.ElementAt(0));
                    secondTab.Add(lstTreeView.ElementAt(1));

                    secondTab.AddRange(allSeqListItemsNew
                        .Where(item => (item.BaseCommandId == 6) || (item.BaseCommandId == 8)
                            || (item.BaseCommandId == 10) || (item.BaseCommandId == 1)
                            || (item.BaseCommandId == 25) || (item.BaseCommandId == 5)
                            || (item.BaseCommandId == 30) || (item.BaseCommandId == 3)
                            || (item.BaseCommandId == 4) || item.BaseCommandId == 45));

                    var tempList =
                        (from d in secondTab
                         where (d.BaseCommandId == 1)
                               || (d.BaseCommandId == 10)
                               || (d.BaseCommandId == 25) || (d.BaseCommandId == 3)
                         select d).ToList();

                    foreach (var sTab in tempList)
                    {
                        var childItems =
                            (from s in allSeqListItemsNew where s.ParentId == sTab.GraphId select s).ToList();
                        secondTab.AddRange(childItems);
                    }

                    secondTab = secondTab.Distinct().ToList();
                    secondTab = secondTab.OrderBy(k => k.NodeId).ToList();

                    #endregion

                    #region

                    var allSeqListItems = new List<TreeView>();
                    foreach (var curItem in secondTab)
                    {
                        stringBuilder.AppendLine("Started process fora attachchilditems: AttachChildItems (" + projectId +
                                                 ")");
                        allSeqListItems.Add(curItem);
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                        foreach (var cItem in childItems)
                        {
                            allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                        }
                        break;
                    }
                    var generalRepositoryNodeDetails =
                        new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                    var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(
                        "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;");
                    int nodeId = 1;
                    if (workflowMaxNode.Any())
                    {
                        nodeId = workflowMaxNode[0].MaxNodeId;
                        nodeId = nodeId + 1;
                    }

                    List<Node> listNodes = new List<Node>();
                    List<Link> listLinks = new List<Link>();
                    TreeViewData treeView = new TreeViewData(lstTreeView)
                    {
                        Nodes = listNodes,
                        Links = listLinks
                    };
                    int widthCnt = Convert.ToInt32(secondTab.First().SpriteCssClass.Length.ToString()) * 3;
                    if (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23)
                    {
                        string[] strSplit = secondTab.First().SpriteCssClass.Split('.');
                        string strName = "";
                        if (strSplit.Length > 2)
                        {
                            strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
                        }
                        treeView.Nodes.Add(new Node
                        {
                            Id = nodeId,
                            Name = strName.ToUpper(),
                            ShapeId = "Circle",
                            Color = "#ffcc00",
                            Width = widthCnt.ToString(),
                            StatementId = Int32.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                            GroupName = secondTab.First().GroupName,
                            GroupId = secondTab.First().GroupId
                        });
                    }
                    else
                    {
                        treeView.Nodes.Add(new Node
                        {
                            Id = nodeId,
                            Name = secondTab.First().SpriteCssClass,
                            ShapeId = "Circle",
                            Color = "#ffcc00",
                            Width = widthCnt.ToString(),
                            StatementId = Int32.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                            GroupName = secondTab.First().GroupName,
                            GroupId = secondTab.First().GroupId
                        });
                    }

                    allSeqListItems = allSeqListItems.Skip(1).ToList();
                    var firstItem = allSeqListItems.First();
                    var methodChildItems =
                        (from a in allSeqListItems where firstItem.GraphId == a.ParentId select a).ToList();
                    int linkSeqNumber = 1;

                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllClassNameDeclared(" +
                                             projectMaster.SolutionId + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = projectMaster.SolutionId},
                    };
                    var allClassNameDeclaredAndCalssCalledList = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNameDeclared", parameters)
                        .Result;

                    foreach (var curItem in methodChildItems)
                    {
                        if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
                        {
                            #region PrimaryCommandId == 1 || BaseCommandId == 1

                            string ifPart = Regex.Split(curItem.GraphName, "If", RegexOptions.IgnoreCase)[1];
                            var condition = ifPart.Contains("Then")
                                ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                                : ifPart;
                            nodeId++;
                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "Decision2",
                                Name = condition,
                                Color = "#ff6600",
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            treeView.Links.Add(new Link
                            {

                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                            linkSeqNumber++;
                            var childItems =
                                (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                            stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf (" +
                                                     projectId + ")");
                            foreach (var cItem in childItems)
                            {

                                treeView = _vbaProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndCalssCalledList, node, ref nodeId,
                                    ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 6)
                        {
                            #region BaseCommandId == 6

                            var item = curItem;
                            string nodeColor = "#c0c0c0";
                            string ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                            var classNameDeclared =
                                allClassNameDeclaredAndCalssCalledList.Where(
                                    s =>
                                        s.ClassNameDeclared == ss ||
                                        s.ClassNameDeclared == item.StatementReferenceMaster.ClassCalled).ToList();

                            if (classNameDeclared.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if (curItem.PrimaryCommandId == 25)
                            {
                                string[] strSplit = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                                string strName = "";
                                if (strSplit.Length > 2)
                                {
                                    strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
                                }
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = strName.ToUpper(),
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            else
                            {
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = curItem.StatementReferenceMaster.ClassCalled,
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            treeView.Nodes.Add(node);
                            string m = curItem.StatementReferenceMaster.MethodCalled;
                            if (m != null)
                            {
                                treeView.Links.Add(new Link
                                {
                                    // Origin = nodeId - 1,
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                                });
                            }
                            else
                            {
                                treeView.Links.Add(new Link
                                {
                                    // Origin = nodeId - 1,
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                            }
                            linkSeqNumber++;
                            var childItems = (from s in secondTab
                                              where s.ParentId == curItem.GraphId
                                                    && s.BaseCommandId != 25
                                              select s).ToList().Distinct();
                            stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt (" +
                                                     projectId + ")");
                            foreach (var cItem in childItems)
                            {
                                treeView = _vbaProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndCalssCalledList, node, ref nodeId, ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 5)
                        {
                            #region BaseCommandId == 5

                            var item = curItem;
                            string nodeColor = "#c0c0c0";
                            var methodCalled =
                                allClassNameDeclaredAndCalssCalledList.Where(
                                    x => x.BaseCommandId == 19 && x.FileId == curItem.StatementReferenceMaster.FileId)
                                    .ToList();
                            if (methodCalled.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = methodCalled[0].ClassNameDeclared,
                                Color = nodeColor,
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            string m = item.StatementReferenceMaster.MethodCalled;
                            if (m != null)
                            {
                                m = m + "()";
                                treeView.Links.Add(new Link
                                {
                                    //   Origin = nodeId - 1,
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                                });
                            }
                            else
                            {
                                treeView.Links.Add(new Link
                                {
                                    //Origin = nodeId - 1,
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                            }
                            linkSeqNumber++;
                            var childItems =
                                (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                            stringBuilder.AppendLine(
                                "Started process for called internal: ProcessChildItemsCallInternal(" +
                                projectId + ")");
                            foreach (var cItem in childItems)
                            {
                                treeView = _vbaProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndCalssCalledList, node, ref nodeId, ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 10)
                        {
                            #region BaseCommandId == 10

                            try
                            {
                                nodeId++;
                                var parentIf =
                                    (from p in secondTab where curItem.ParentId == p.GraphId select p).FirstOrDefault();
                                var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                                var condition = ifPart.Contains("THEN")
                                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                                    : ifPart;
                                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
                                var width = _clsUniverseBasic.CalculateWidth(condition.Length);
                                var height = _clsUniverseBasic.CalculateWidth(condition.Length);
                                var node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "Decision2",
                                    Name = condition,
                                    Color = "#ff6600",
                                    Width = width,
                                    Height = height,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                                treeView.Nodes.Add(node);
                                treeView.Links.Add(new Link
                                {
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                                linkSeqNumber++;
                                var childItems =
                               (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                                stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsElse (" +
                                                         projectId + ")");
                                foreach (var cItem in childItems)
                                {
                                    treeView = _vbaProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                        allClassNameDeclaredAndCalssCalledList, node, ref nodeId,
                                        ref linkSeqNumber);
                                }
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 45)
                        {
                            #region BaseCommandId = 45
                            nodeId++;
                            var width = _clsUniverseBasic.CalculateWidth(curItem.GraphName.Length);
                            var height = _clsUniverseBasic.CalculateWidth(curItem.GraphName.Length);
                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = curItem.GraphName,
                                Color = "#c0c0c0",
                                Width = width,
                                Height = height,
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                            linkSeqNumber++;
                            #endregion
                        }
                    }
                    stringBuilder.AppendLine("Started process for removenodes: RemoveMultipleLinks(" + projectId + ")");
                    treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                    stringBuilder.AppendLine("Started process for removehangingnodes: RemoveHangingNodes(" + projectId +
                                             ")");
                    treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());

                    #endregion

                    // ReSharper disable once RedundantAssignment
                    var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                        .GetEntityData<ActionWorkflows>(
                            s => s.MethodStatementId == statementId && s.ProjectId == projectId);

                    #region Add to database table...

                    stringBuilder.AppendLine("Started to insert workflownodes into database: (" + projectId + ")");

                    var lstWorkflowNodeDetails = treeView.Nodes.Select(node => new WorkflowNodeDetails
                    {
                        ProjectId = projectId,
                        BaseCommandId = node.BaseCommandId,
                        ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                        BusinessDescription = node.BusinessDescription,
                        BusinessName = node.BusinessName,
                        WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                        ParentId = node.ParentId,
                        Id = node.Id,
                        StatementId = node.StatementId,
                        StatementTypeId = node.StatementTypeId,
                        ChildId = node.ChildId,
                        FileId = node.FileId,
                        Width = node.Width,
                        Name = node.Name,
                        Height = node.Height,
                        ShapeId = node.ShapeId,
                        Color = node.Color,
                        MaxNodeId = nodeId
                    }).ToList();

                    await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails);

                    #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                    var statementNodes =
                        await
                            generalRepositoryNodeDetails.GetAllItems(
                                p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                    foreach (var stmt in statementNodes)
                    {
                        if (stmt.ShapeId != "Decision" && stmt.ShapeId != "Decision2") continue;

                        Regex word =
                            new Regex(@"\<span ([\w\(\)\\_\,\<\#\;\:\&\$\-\.\'\\+\=EQ\sEQUAL\sAND\sNOT\sOR\s\/]*)\>");
                        var removeHtmlTag = "";
                        foreach (Match match in word.Matches(stmt.Name))
                        {
                            removeHtmlTag = match.Value;
                            stmt.Name = stmt.Name.Replace(removeHtmlTag, "");
                        }

                        if (removeHtmlTag != "")
                        {
                            stmt.Name =
                                stmt.Name.Replace("</span>", "")
                                    .Replace(" LT", " is less than")
                                    .Replace(" GT", " is greater than")
                                    .Replace("GE", " is greater than or equal to")
                                    .Replace(" #", " not equal to") + "?";
                        }
                        else
                        {
                            stmt.Name =
                                stmt.Name.Replace("</span>", "")
                                    .Replace(" LT", " is less than")
                                    .Replace(" GT", " is greater than")
                                    .Replace("GE", " is greater than or equal to")
                                    .Replace(" #", " not equal to") + "?";
                        }

                        await
                            generalRepositoryNodeDetails.UpdateItem(stmt);
                    }

                    #endregion

                    stringBuilder.AppendLine("Started to insert workflowlink into database: (" + projectId + ")");
                    var lstWorkflowLinkDetails = treeView.Links.Select(link => new WorkflowLinkDetails
                    {
                        BaseCommandId = link.BaseCommandId,
                        ProjectId = projectId,
                        ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                        BusinessDescription = link.BusinessDescription,
                        BusinessName = link.BusinessName,
                        LinkText = link.LinkText,
                        Origin = link.Origin,
                        StatementId = link.StatementId,
                        Target = link.Target,
                        WorkflowStartStatementId = actionWorkflow.First().MethodStatementId
                    }).ToList();

                    var generalRepositoryLinkDetails = new GeneralRepository<WorkflowLinkDetails>(new AppDbContext());
                    await generalRepositoryLinkDetails.BulkInsert(lstWorkflowLinkDetails);
                    secondTab = secondTab.Distinct().ToList();
                    stringBuilder.AppendLine("Started to insert workflowtreeviewsecondtabdetails into database: (" +
                                             projectId + ")");
                    var lstWorkflowTreeviewSecondTabDetails =
                        secondTab.Select(sTab => new WorkflowTreeviewSecondTabDetails
                        {
                            BaseCommandId = sTab.BaseCommandId,
                            ProjectId = projectId,
                            ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                            ActualStatementId = sTab.ActualStatementId,
                            ClassCalled = sTab.ClassCalled,
                            GraphId = sTab.GraphId,
                            //GraphName = sTab.GraphName + "&nbsp;<img id='imgpseudo' src='../images/regex_icon.png' onclick='PseudoCodeDialog(" + sTab.ActualStatementId.Split('_')[1] + ")'/>",
                            GraphName = sTab.GraphName,
                            HasChild = sTab.HasChild ? "true" : "false",
                            MethodCalled = sTab.MethodCalled,
                            ParentId = sTab.ParentId,
                            PrimaryCommandId = sTab.PrimaryCommandId,
                            SpriteCssClass = sTab.SpriteCssClass,
                            WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                            StatementId = int.Parse(sTab.ActualStatementId.Split('_')[1]),
                            IndentLevel = sTab.IndentLevel,
                            ProgramId = 0
                        }).ToList();

                    var generalRepositoryWorkflowTreeviewSecondTabDetails =
                        new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                    await
                        generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                    #region To Updated the GraphName against the BaseCommandId

                    var statementSecondTab = await generalRepositoryWorkflowTreeviewSecondTabDetails
                        .GetAllItems(p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                    foreach (var stmt in statementSecondTab)
                    {
                        if (stmt.BaseCommandId == 1 || stmt.BaseCommandId == 5 || stmt.BaseCommandId == 6)
                        {
                            stmt.GraphName = stmt.GraphName +
                                             "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                             stmt.ActualStatementId.Split('_')[1] + ")'/>";

                            await generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                        }
                    }

                    #endregion

                    stringBuilder.AppendLine("Started to insert workflowtreeviewfirsttabdetails into database: (" +
                                             projectId + ")");
                    var lsTreeviewTabFirstDetails =
                        allSeqListItemsNew.Select(fTab => new WorkflowTreeviewTabFirstDetails
                        {
                            BaseCommandId = fTab.BaseCommandId,
                            ProjectId = projectId,
                            ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                            ActualStatementId = fTab.ActualStatementId,
                            ClassCalled = fTab.ClassCalled,
                            GraphId = fTab.GraphId,
                            GraphName = fTab.GraphName,
                            HasChild = fTab.HasChild ? "true" : "false",
                            MethodCalled = fTab.MethodCalled,
                            ParentId = fTab.ParentId,
                            PrimaryCommandId = fTab.PrimaryCommandId,
                            SpriteCssClass = fTab.SpriteCssClass,
                            WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                            IndentLevel = fTab.IndentLevel,
                            ProgramId = 0
                        }).ToList();

                    var generalRepositoryTreeviewTabFirstDetails =
                        new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());
                    await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);
                    stringBuilder.AppendLine("Workflow data collected successfully: (" + projectId + ")");
                    LogMessage.WriteLogMessage(stringBuilder);

                    #endregion
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                return Ok("Workflow data collected successfully");
            }
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int indentLevel, ref int auto, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            StringBuilder stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    if (treeView.ClassCalled == null) return lstTreeView;

                    string className = treeView.ClassCalled;
                    stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" + className +
                                             " , " +
                                             projectId + ")");

                    var callExtExpandedCode = GetGenericBlock(className, projectId);

                    if (callExtExpandedCode.Count == 0)
                    {
                        auto++;
                        var item = lstTreeView.Find(p => p.ParentId == statememtId);
                        if (item == null)
                        {
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + auto + "_" + statememtId,
                                HasChild = true,
                                GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing" + auto + "_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                    else
                    {
                        foreach (var statementMaster in callExtExpandedCode)
                        {
                            try
                            {
                                if (!treeView.MethodCalled.StartsWith(statementMaster.MethodName)) continue;
                                int blockStmtId = statementMaster.StatementId;
                                stringBuilder.AppendLine("Started process to get block of method: GetMethodBlock(" +
                                                         blockStmtId + " )");
                                var stmtsBlock = GetMethodBlock(blockStmtId);

                                var calledInternals =
                                    (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled)
                                        .ToList();
                                callingAndCalled.Add(treeView.MethodCalled + "_" + auto, calledInternals);

                                foreach (var block in stmtsBlock)
                                {
                                    auto++;
                                    if (block.BaseCommandId == 6)
                                    {
                                        lstTreeView.Add(new TreeView
                                        {
                                            ActualStatementId = "Actual_" + block.StatementId,
                                            ParentId = statememtId,
                                            GraphId = "Node" + auto + "_" + block.StatementId,
                                            HasChild = true,
                                            GraphName =
                                                "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                            BaseCommandId = block.BaseCommandId,
                                            PrimaryCommandId = block.PrimaryCommandId,
                                            MethodCalled = block.MethodCalled,
                                            ClassCalled = block.ClassCalled,
                                            StatementReferenceMaster = block,
                                            NodeId = ++treeNodeId,
                                            IndentLevel = indentLevel
                                        });
                                        indentLevel = indentLevel + 1;
                                        stringBuilder.AppendLine(
                                            "Started process for call external: GetCallExternalDetails(" + projectId +
                                            " )");
                                        lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId,
                                            lstTreeView.Last(),
                                            lstTreeView, projectId, indentLevel, ref auto,
                                            ref treeNodeId, ref callingAndCalled);
                                        indentLevel = indentLevel - 1;
                                    }
                                    else if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
                                    {
                                        auto++;
                                        lstTreeView.Add(new TreeView
                                        {
                                            ActualStatementId = "Actual_" + block.StatementId,
                                            ParentId = statememtId,
                                            GraphId = "Node" + auto + "_" + block.StatementId,
                                            HasChild = true,
                                            GraphName =
                                                "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                            BaseCommandId = block.BaseCommandId,
                                            PrimaryCommandId = block.PrimaryCommandId,
                                            MethodCalled = block.MethodCalled,
                                            ClassCalled = block.ClassCalled,
                                            StatementReferenceMaster = block,
                                            NodeId = ++treeNodeId,
                                            IndentLevel = indentLevel
                                        });
                                        indentLevel = indentLevel + 1;
                                        stringBuilder.AppendLine(
                                            "Started process for call internal: GetCallInternalDetails(" + projectId +
                                            " )");
                                        lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId,
                                            lstTreeView.Last(),
                                            lstTreeView, projectId, block.FileId, indentLevel, ref auto, ref treeNodeId,
                                            ref callingAndCalled);
                                        indentLevel = indentLevel - 1;
                                    }
                                    else
                                    {
                                        lstTreeView.Add(new TreeView
                                        {
                                            ActualStatementId = "Actual_" + block.StatementId,
                                            ParentId = statememtId,
                                            GraphId = "Node" + auto + "_" + block.StatementId,
                                            HasChild = false,
                                            GraphName = block.OriginalStatement,
                                            BaseCommandId = block.BaseCommandId,
                                            PrimaryCommandId = block.PrimaryCommandId,
                                            MethodCalled = block.MethodCalled,
                                            ClassCalled = block.ClassCalled,
                                            StatementReferenceMaster = block,
                                            NodeId = ++treeNodeId,
                                            IndentLevel = indentLevel
                                        });
                                    }
                                }
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }
                        }
                    }
                    LogMessage.WriteLogMessage(stringBuilder);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int fileId, int indentLevel, ref int autoInt, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            StringBuilder stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;
                    string methodName = treeView.MethodCalled;
                    stringBuilder.AppendLine("Called stored procedure: SpFindStatementForMethodName(8, " + projectId +
                                             " , " +
                                             methodName + ", " + fileId + ")");

                    object[] parameters =
                    {
                        new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                        new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                    };
                    var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters)
                        .Result;
                    if (stmtMaster.Count == 0)
                    {
                        autoInt++;
                        var item = lstTreeView.Find(p => p.ParentId == statememtId);
                        if (item == null)
                        {
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + autoInt + "_" + statememtId,
                                HasChild = true,
                                GraphName = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>",
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing" + autoInt + "_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                    else
                    {
                        stringBuilder.AppendLine("Started process to get block: GetGenericBlock(" +
                                                 stmtMaster[0].StatementId + "),8,9");
                        var callExtExpandedCode = GetGenericBlock(stmtMaster[0].StatementId, 8, 9);
                        if (callExtExpandedCode.Count == 0)
                        {
                            autoInt++;
                            var item = lstTreeView.Find(p => p.ParentId == statememtId);
                            if (item == null)
                            {
                                lstTreeView.Add(new TreeView
                                {
                                    ParentId = statememtId,
                                    GraphId = "111" + autoInt + "_" + statememtId,
                                    HasChild = true,
                                    GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                                    BaseCommandId = 25,
                                    StatementReferenceMaster = treeView.StatementReferenceMaster,
                                    NodeId = ++treeNodeId,
                                    ActualStatementId = "Missing" + autoInt + "_99999999",
                                    IndentLevel = indentLevel
                                });
                            }
                        }
                        else
                        {
                            var calledInternals =
                                (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                            string mName = methodName + "_" + autoInt;
                            callingAndCalled.Add(mName, calledInternals);
                            bool result = false;

                            foreach (var block in callExtExpandedCode)
                            {
                                autoInt++;
                                if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
                                {
                                    foreach (var keyValPair in callingAndCalled.Where(s => s.Key == mName))
                                    {
                                        var internalVal = keyValPair.Value;
                                        var block1 = block;
                                        foreach (var gosub in internalVal.Where(s => s.StartsWith(block1.MethodCalled)))
                                        {
                                            if (!callingAndCalled.Keys.Any(s => s.StartsWith(gosub))) continue;
                                            var gosub1 = gosub;
                                            var newList = (from d in callingAndCalled
                                                           where d.Key.StartsWith(gosub1)
                                                           select d.Value).FirstOrDefault();
                                            result = newList != null && newList.Any(s => s.StartsWith(methodName));
                                            if (result) break;
                                        }
                                    }

                                    if (result)
                                    {
                                        lstTreeView.Add(new TreeView
                                        {
                                            ActualStatementId = "Actual_" + block.StatementId,
                                            ParentId = statememtId,
                                            GraphId = "Node" + autoInt + "_" + block.StatementId,
                                            HasChild = true,
                                            GraphName =
                                                "<span class='nodeToBold'>" + block.OriginalStatement +
                                                " (Recursive call)</span>",
                                            BaseCommandId = block.BaseCommandId,
                                            PrimaryCommandId = block.PrimaryCommandId,
                                            MethodCalled = block.MethodCalled,
                                            ClassCalled = block.ClassCalled,
                                            StatementReferenceMaster = block,
                                            NodeId = ++treeNodeId,
                                            IndentLevel = indentLevel
                                        });
                                        result = false;
                                        continue;
                                    }


                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + autoInt + "_" + block.StatementId,
                                        HasChild = true,
                                        GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                    indentLevel = indentLevel + 1;
                                    stringBuilder.AppendLine(
                                        "Started process for call internal: GetCallInternalDetails(" +
                                        projectId + ")");
                                    lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                        ref autoInt,
                                        ref treeNodeId, ref callingAndCalled);
                                    indentLevel = indentLevel - 1;
                                }
                                else if (block.BaseCommandId == 6)
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + autoInt + "_" + block.StatementId,
                                        HasChild = true,
                                        GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                    indentLevel = indentLevel + 1;
                                    stringBuilder.AppendLine(
                                        "Started process for call external: GetCallExternalDetails(" +
                                        projectId + ")");
                                    lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, indentLevel, ref autoInt,
                                        ref treeNodeId, ref callingAndCalled);
                                    indentLevel = indentLevel - 1;
                                }
                                else
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + autoInt + "_" + block.StatementId,
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                }
                            }
                        }
                    }
                    LogMessage.WriteLogMessage(stringBuilder);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                return lstTreeView;
            }
        }

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string color, string groupName, int groupId)
        {
            try
            {
                treeView.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeView.GraphName +
                                     "</span>";
                var childItems =
                    (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
                foreach (var child in childItems)
                {
                    child.GraphName = "<span style='color: " + color + ";'>" + child.GraphName +
                                      "</span>";
                    child.GroupName = groupName;
                    child.GroupId = groupId;
                    child.IndentLevel = child.IndentLevel + 2;
                }
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
        }

        private List<Link> RemoveMultipleLinks(List<Link> lstLinks, List<Node> lstNodes)
        {
            var copyOfLinks = lstLinks;
            var linksToRemove = new List<Link>();
            foreach (var node in lstNodes)
            {
                try
                {
                    var childNodes = (from lnk in lstLinks
                                      where lnk.Origin == node.Id
                                      select lnk.Target
                                          into p
                                      select lstNodes.Find(n => n.Id == p)).ToList();
                    childNodes.RemoveAll(item => item == null);
                    if (childNodes.Count <= 1) continue;

                    var dupes =
                        childNodes.Where(a => childNodes.Except(new List<Node> { a }).Any(x => x.Name == a.Name)).ToList();
                    bool hasChilds = false;
                    foreach (var dup in dupes)
                    {
                        var klm = (from f in lstLinks where f.Origin == dup.Id select f).ToList();
                        if (klm.Count > 0)
                            hasChilds = true;
                    }
                    if (hasChilds) continue;
                    var tempLinks = new List<Link>();
                    if (dupes.Count <= 0 || dupes[0] == null) continue;
                    foreach (var n in dupes)
                    {
                        var link = (from h in lstLinks
                                    where h.Origin == node.Id
                                          && h.Target == n.Id
                                    select h).ToList();
                        tempLinks.AddRange(link);
                    }
                    if (!tempLinks.Any()) continue;

                    string linkText = tempLinks.Aggregate(String.Empty,
                        (current, jk) => current + jk.LinkText + ", ");
                    linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                    tempLinks.First().LinkText = linkText;

                    linksToRemove.AddRange(tempLinks.Where(l => l.LinkText != linkText));
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
            }
            foreach (var l in linksToRemove)
            {
                copyOfLinks.Remove(l);
            }
            return copyOfLinks;
        }

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab,
            TreeView curItem)
        {
            try
            {
                allSeqListItems.Add(curItem);
                var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                foreach (var cItem in childItems)
                {
                    allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                }
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return allSeqListItems;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = await _codeVortoService.ProjectMasterRepository
                        .GetItem<ProjectMaster>(p => p.ProjectId == projectId);
                    if (projectMaster == null) return Ok("Done");
                    var fileElementMaster = await _codeVortoService.FileMasterRepository
                        .GetAllItems(p => p.ProjectId == projectId).ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        });
                    Expression<Func<StatementReferenceMaster, bool>> stateExpression = e => e.ProjectId == projectId;
                    var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllItems(stateExpression).ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        });
                    List<Node> lstNodes = new List<Node>();
                    var objFlowChart = new FlowChart();
                    var languageId = projectMaster.LanguageId;

                    #region Runtime Created the flowchart

                    try
                    {
                        #region Add Nodes Logic

                        var allClassData = statementMaster.Where(p => p.ProjectId == projectId
                                                                      && p.BaseCommandId == 19
                                                                      && !string.IsNullOrEmpty(p.ClassNameDeclared))
                            .ToList();

                        foreach (var s in allClassData)
                        {
                            #region For VBA Application

                            if (string.IsNullOrEmpty(s.ClassNameDeclared)) continue;

                            var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                               && p.FileId == s.FileId &&
                                                                               p.BaseCommandId == 6).ToList();
                            var newList = callExecStatement.ToList();

                            foreach (var c in callExecStatement)
                            {
                                if (!string.IsNullOrEmpty(c.ClassCalled))
                                {
                                    string strName = c.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                    var checkClassPresentOrNot =
                                        fileElementMaster.Where(p => p.ProjectId == projectId
                                                                     && p.FileName.Contains(strName.Trim()))
                                            .ToList();

                                    if (checkClassPresentOrNot.Any())
                                    {
                                        var fileIdPresent = 0;
                                        foreach (var p in checkClassPresentOrNot)
                                        {
                                            fileIdPresent = p.FileId;
                                        }

                                        callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                                       && p.FileId == fileIdPresent &&
                                                                                       p.BaseCommandId == 6)
                                            .ToList();

                                        if (callExecStatement.Count > 0)
                                        {
                                            newList.AddRange(callExecStatement);
                                        }
                                    }
                                }

                                if (newList.Count <= 0) continue;

                                foreach (var z in newList)
                                {
                                    if (string.IsNullOrEmpty(z.ClassCalled)) continue;
                                    string strName = z.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                    var checkClassPresentOrNot =
                                        fileElementMaster.Where(p => p.ProjectId == projectId
                                                                     && p.FileName.Contains(strName.Trim()))
                                            .ToList();

                                    if (checkClassPresentOrNot.Any())
                                    {
                                        var buiName = statementMaster.Where(k => k.ClassNameDeclared == z.ClassCalled)
                                            .ToList().FirstOrDefault();
                                        string altName = buiName == null ? z.ClassCalled : buiName.AlternateName;
                                        string cName = buiName == null ? z.ClassNameDeclared : buiName.ClassNameDeclared;
                                        altName = altName + " [ " + cName + " ]";

                                        Node nodeObject1 = new Node
                                        {
                                            Id = z.StatementId,
                                            Name = altName,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#00ffff",
                                            StatementId = s.StatementId,
                                            BaseCommandId = s.BaseCommandId,
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                                          where dd.Name == nodeObject1.Name
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                            lstNodes.Add(nodeObject1);
                                    }
                                    else
                                    {
                                        var buiName = statementMaster.Where(k => k.ClassNameDeclared == z.ClassCalled)
                                            .ToList().FirstOrDefault();
                                        string altName = buiName == null ? z.ClassCalled : buiName.AlternateName;
                                        string cName = buiName == null ? z.ClassNameDeclared : buiName.ClassNameDeclared;
                                        altName = altName + " [ " + cName + " ]";
                                        Node nodeObject2 = new Node
                                        {
                                            Id = z.StatementId,
                                            Name = altName,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = s.StatementId,
                                            BaseCommandId = s.BaseCommandId,
                                            FileId = s.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                                          where
                                                              dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                            lstNodes.Add(nodeObject2);
                                    }
                                }
                            }

                            #endregion
                        }

                        objFlowChart.Nodes = lstNodes;

                        #endregion

                        #region Add Links

                        var lstLinks = new List<Link>();
                        var allClassDataLinks = allClassData.ToList();
                        foreach (var s in allClassDataLinks)
                        {
                            if (string.IsNullOrEmpty(s.ClassNameDeclared)) continue;
                            var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                               && p.FileId == s.FileId &&
                                                                               p.BaseCommandId == 6).ToList();
                            foreach (var c in callExecStatement)
                            {
                                if (string.IsNullOrEmpty(c.ClassCalled)) continue;

                                var iDofNode = s.StatementId;
                                var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "");
                                var checkClassPresentOrNotLink =
                                    statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                               && p.ClassCalled == dataObject).ToList();

                                if (checkClassPresentOrNotLink.Count <= 0) continue;
                                foreach (var f in checkClassPresentOrNotLink)
                                {
                                    var checkClassCallMethod =
                                        statementMaster.Where(
                                            p => p.ProjectId == projectId && p.BaseCommandId == 19
                                                 && p.FileId == f.FileId).ToList();

                                    if (checkClassCallMethod.Count <= 0) continue;

                                    string mulMethodName = "";

                                    for (int e = 0; e < checkClassCallMethod.Count; e++)
                                    {
                                        var getClassCalled =
                                            statementMaster.Where(
                                                p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                     &&
                                                     p.ClassCalled ==
                                                     checkClassCallMethod[e].ClassNameDeclared).ToList();

                                        if (getClassCalled.Count > 0)
                                        {
                                            for (int g = 0; g < getClassCalled.Count; g++)
                                            {
                                                var linkObj = new Link
                                                {
                                                    Origin = getClassCalled[g].StatementId,
                                                    Target = f.StatementId,
                                                    BaseCommandId = f.BaseCommandId,
                                                    StatementId = f.StatementId
                                                };

                                                var l =
                                                    (from d in lstLinks
                                                     where
                                                         d.Origin == getClassCalled[g].StatementId &&
                                                         d.Target == f.StatementId
                                                     select d).ToList();
                                                if (l.Count != 0) continue;
                                                if (f.MethodCalled != null)
                                                {
                                                    if (!f.MethodCalled.Trim().Contains("(") ||
                                                        !f.MethodCalled.Trim().EndsWith(")")) continue;

                                                    if (f.MethodCalled.Contains("=") ||
                                                        f.MethodCalled.StartsWith("<")) continue;

                                                    int indexMethod =
                                                        f.MethodCalled.IndexOf("(",
                                                            StringComparison.Ordinal);
                                                    if (indexMethod > 0)
                                                    {
                                                        if (
                                                            !mulMethodName.Contains(f.MethodCalled.Substring(0,
                                                                indexMethod)))
                                                        {
                                                            mulMethodName = mulMethodName + ", " + f.MethodCalled
                                                                .Substring(0, indexMethod);

                                                            linkObj.LinkText =
                                                                mulMethodName.Substring(1,
                                                                    mulMethodName.Length - 1).Replace("))", "");
                                                        }
                                                    }
                                                    else
                                                    {
                                                        if (
                                                            !mulMethodName.Contains(
                                                                f.MethodCalled))
                                                        {
                                                            mulMethodName = mulMethodName +
                                                                            ", " + f.MethodCalled;

                                                            linkObj.LinkText =
                                                                mulMethodName.Substring(1,
                                                                    mulMethodName.Length - 1).Replace("))", "");
                                                        }
                                                    }


                                                    if (linkObj.Origin == linkObj.Target) continue;

                                                    if (e == checkClassCallMethod.Count - 1)
                                                        lstLinks.Add(linkObj);
                                                }
                                                else
                                                {
                                                    if (linkObj.Origin == linkObj.Target) continue;

                                                    if (e == checkClassCallMethod.Count - 1)
                                                        lstLinks.Add(linkObj);
                                                }
                                            }
                                            break;
                                        }

                                        var linkObject = new Link
                                        {
                                            Origin = iDofNode,
                                            Target = f.StatementId,
                                            BaseCommandId = f.BaseCommandId,
                                            StatementId = f.StatementId
                                        };

                                        var k =
                                            (from d in lstLinks
                                             where d.Origin == iDofNode && d.Target == f.StatementId
                                             select d).ToList();
                                        if (k.Count != 0) continue;

                                        if (f.MethodCalled != null)
                                        {
                                            if (!f.MethodCalled.Trim().Contains("(") ||
                                                !f.MethodCalled.Trim().EndsWith(")")) continue;

                                            if (f.MethodCalled.Contains("=") || f.MethodCalled.StartsWith("<"))
                                                continue;

                                            int indexMethod =
                                                f.MethodCalled.IndexOf("(",
                                                    StringComparison.Ordinal);
                                            if (indexMethod > 0)
                                            {
                                                if (
                                                    !mulMethodName.Contains(
                                                        f.MethodCalled.Substring(0,
                                                            indexMethod)))
                                                {
                                                    mulMethodName = mulMethodName + ", " +
                                                                    f.MethodCalled.Substring(0, indexMethod);

                                                    linkObject.LinkText =
                                                        mulMethodName.Substring(1, mulMethodName.Length - 1)
                                                            .Replace("))", "");
                                                }
                                            }
                                            else
                                            {
                                                if (!mulMethodName.Contains(f.MethodCalled))
                                                {
                                                    mulMethodName = mulMethodName + ", " + f.MethodCalled;

                                                    linkObject.LinkText =
                                                        mulMethodName.Substring(1, mulMethodName.Length - 1)
                                                            .Replace("))", "");
                                                }
                                            }


                                            if (linkObject.Origin == linkObject.Target) continue;

                                            if (e == checkClassCallMethod.Count - 1)
                                                lstLinks.Add(linkObject);
                                        }
                                        else
                                        {
                                            if (linkObject.Origin == linkObject.Target) continue;

                                            if (e == checkClassCallMethod.Count - 1)
                                                lstLinks.Add(linkObject);
                                        }
                                    }
                                }
                            }
                        }
                        objFlowChart.Links = lstLinks;

                        #region Code to remove Missing Origin / missing target links

                        List<int> missingTargets =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets.Count > 0)
                        {
                            foreach (int link in missingTargets)
                            {
                                lstLinks.RemoveAll(x => x.Target == link);
                            }
                        }

                        List<int> missingOrigins =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins.Count > 0)
                        {
                            foreach (int l in missingOrigins)
                            {
                                lstLinks.RemoveAll(x => x.Origin == l);
                            }
                        }

                        List<int> missingTargets1 =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets1.Count > 0)
                        {
                            foreach (int t in missingTargets1)
                            {
                                lstLinks.RemoveAll(x => x.Target == t);
                            }
                        }

                        List<int> missingOrigins1 =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins1.Count > 0)
                        {
                            foreach (int t in missingOrigins1)
                            {
                                lstLinks.RemoveAll(x => x.Origin == t);
                            }
                        }

                        #endregion

                        #region Code to remove no links to the nodes

                        List<int> missingNodes =
                            lstNodes.Select(x => x.Id)
                                .ToList()
                                .Except(lstLinks.Select(y => y.Origin))
                                .ToList();
                        if (missingNodes.Count > 0)
                        {
                            foreach (int t in missingNodes)
                            {
                                List<int> iCnt = (from dd in lstLinks
                                                  where dd.Target == t
                                                  select dd.Target).ToList();
                                if (iCnt.Count == 0)
                                    lstNodes.RemoveAll(x => x.Id == t);
                            }
                        }

                        #endregion

                        #endregion

                        var startNodes = await _codeVortoService.ActionWorkflowsRepository
                            .GetDataFromSqlQuery<ActionWorkflows>(
                                " select * from actionworkFlows where ProjectId=" + projectId +
                                " and EndPointOrService is null;");
                        int tempI = 1;
                        var list =
                            startNodes.FindAll(
                                s => s.EndPointOrService != "Service");
                        foreach (var node in list)
                        {
                            Node nodeObject = new Node
                            {
                                StatementId = 99998 + tempI,
                                Id = 99998 + tempI,
                                Name = node.WorkflowName,
                                ShapeId = "Circle",
                                Height = "15",
                                Width = "100",
                                Color = "#ffcc00"
                            };
                            lstNodes.Add(nodeObject);
                            tempI++;
                        }

                        #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                        if (objFlowChart.Nodes.Count > 0)
                        {
                            var nodes = objFlowChart.Nodes.ToList();

                            if (languageId == 1)
                            {
                                foreach (var mNodes in nodes)
                                {
                                    var nodesData = new ConnectivityStepReference
                                    {
                                        Name = mNodes.Name,
                                        Shape = mNodes.ShapeId,
                                        Color = mNodes.Color,
                                        ParentId = mNodes.ParentId,
                                        StatementId = mNodes.StatementId,
                                        ProjectId = Convert.ToInt32(projectId)
                                    };
                                    await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
                                }
                            }
                            else
                            {
                                foreach (var mNodes in nodes)
                                {
                                    string strName = mNodes.Name;
                                    var nodesData = new ConnectivityStepReference
                                    {
                                        Name = strName.Trim(),
                                        Shape = mNodes.ShapeId,
                                        Color = mNodes.Color,
                                        ParentId = mNodes.ParentId,
                                        StatementId = mNodes.Id,
                                        ProjectId = Convert.ToInt32(projectId)
                                    };
                                    await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
                                }
                            }
                        }

                        if (objFlowChart.Links.Count > 0)
                        {
                            var links = objFlowChart.Links.ToList();
                            foreach (var mLinks in links)
                            {
                                try
                                {
                                    var liksData = new ConnectivityLinkReferece
                                    {
                                        Origin = mLinks.Origin,
                                        Target = mLinks.Target,
                                        LinkText = mLinks.LinkText,
                                        StatementId = mLinks.StatementId,
                                        ProjectId = Convert.ToInt32(projectId)
                                    };
                                    await _codeVortoService.ConnectivityLinkRefereceRepository.AddNewItem(liksData);
                                }
                                catch (Exception exception)
                                {
                                    LogMessage.WriteExceptionLogMessage(exception);
                                }
                            }
                        }

                        #endregion
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessObjectConnectivityDiagram(int projectId)
        {
            StringBuilder stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = await _codeVortoService.ProjectMasterRepository
                                .GetItem<ProjectMaster>(p => p.ProjectId == projectId).ConfigureAwait(false);
                    int solutionId = projectMaster.SolutionId ?? 5;
                    string sqlAllClasses = " Select * from statementreferencemaster Where BaseCommandId = 19 AND " +
                                           " ClassNameDeclared IS NOT NULL AND ProjectId IN ( Select ProjectId FROM ProjectMaster " +
                                           " WHERE SolutionId = " + solutionId + ");";
                    // Collect all required information
                    var allClassNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllClasses).ConfigureAwait(false);

                    var allCallExternals = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.ProjectId == projectId && s.BaseCommandId == 6
                                              && !string.IsNullOrEmpty(s.ClassCalled)).ConfigureAwait(false);

                    var generalRepositoryNodeDetails = new GeneralRepository<ConnectivityStepReference>(new AppDbContext());
                    var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<ConnectivityStepReference>(
                        " SELECT * FROM ConnectivityStepReference ORDER BY NodeId DESC LIMIT 1;").ConfigureAwait(false);

                    var dataDependancyList =
                      await _codeVortoService.DataDependencyRepository.GetAllListItems(x => x.ProjectId == projectId);

                    var nodeId = 1;
                    if (workflowMaxNode.Any())
                    {
                        var id = workflowMaxNode[0].NodeId;
                        nodeId = id + 1;
                    }

                    var lstNodes = new List<Node>();
                    foreach (var classNameDeclared in allClassNameDeclared)
                    {
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classNameDeclared.AlternateName + " [" + classNameDeclared.ClassNameDeclared + "]",
                            ShapeId = "RoundRect",
                            Height = "15",
                            Width = "100",
                            Color = "#00ffff", //Color.FromName("aqua").Name, // "#00ffff",
                            StatementId = classNameDeclared.StatementId,
                            BaseCommandId = classNameDeclared.BaseCommandId,
                            OriginalClassName = classNameDeclared.ClassNameDeclared,
                            FileId = classNameDeclared.FileId,
                            ProgramId = classNameDeclared.FileId
                        });
                    }

                    // Now loop through allCallExternals for ClassCalled as those might be missing in
                    // nodes list and will be colored as gray
                    foreach (var classCalled in allCallExternals)
                    {
                        if (lstNodes.Any(n => n.OriginalClassName == classCalled.ClassCalled)) continue;
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classCalled.AlternateName + " [" + classCalled.ClassCalled + "]",
                            ShapeId = "RoundRect",
                            Height = "18",
                            Width = "100",
                            Color = "#c0c0c0", //Color.FromName("gray").Name, // "#00ffff",
                            StatementId = classCalled.StatementId,
                            BaseCommandId = classCalled.BaseCommandId,
                            OriginalClassName = classCalled.ClassNameDeclared,
                            FileId = classCalled.FileId,
                            ProgramId = classCalled.FileId
                        });
                    }

                    // Now loop through all Nodes and check whether this class is used in call external...
                    // And create list of links to connect them
                    stringBuilder.AppendLine("Started to bind all links: (" + projectId + ")");
                    var lstLinks = new List<Link>();
                    foreach (var node in lstNodes)
                    {
                        string className = node.OriginalClassName;
                        var allDistinctClasses =
                            (from cls in allCallExternals where cls.ClassCalled == className select cls).ToList();
                        foreach (var callExt in allDistinctClasses)
                        {
                            var cName = (from c in allClassNameDeclared
                                         where c.FileId == callExt.FileId && c.BaseCommandId == 19
                                         select c).ToList();

                            // var originStatement = cName.Find(n => n.ClassNameDeclared == className);
                            var originNode = lstNodes.Find(n =>
                            {
                                var firstOrDefault = cName.FirstOrDefault();
                                return firstOrDefault != null && n.OriginalClassName == firstOrDefault.ClassNameDeclared;
                            });
                            var targetNode = lstNodes.Find(n => n.OriginalClassName == className);
                            if (originNode == null || targetNode == null) continue;
                            if (lstLinks.Any(s => s.Origin == originNode.Id && s.Target == targetNode.Id)) continue;

                            lstLinks.Add(new Link
                            {
                                Origin = originNode.Id,
                                Target = targetNode.Id,
                                LinkText = callExt.MethodCalled
                            });
                        }
                    }

                    #region Code to remove Missing Origin / missing target links

                    var lstMainLinkList = new List<Link>();
                    foreach (var mNode in lstNodes)
                    {
                        var nId = mNode.Id;
                        var links = (from l in lstLinks where l.Origin == nId select l).ToList();
                        lstMainLinkList.AddRange(links);
                    }

                    #endregion

                    string crudSql =
                       "SELECT da.* FROM dbcrudactivities AS da INNER JOIN FileMaster AS fm " +
                       " ON da.FileId = fm.FileId WHERE fm.ProjectId = " + projectId + ";";
                    var dataCrudActivities = await _codeVortoService.DbCrudActivityRepository.GetDataFromSqlQuery<DbCrudActivity>(crudSql)
                                       .ConfigureAwait(false);


                    var nodesList = new List<Node>();
                    foreach (var mNode in lstNodes)
                    {
                        var entitySet = (from d in dataDependancyList where d.FileId == mNode.FileId select d).ToList().DistinctByData();
                        foreach (var entity in entitySet)
                        {
                            string linkText = "";
                            var dbCrudActivity = new List<string>();
                            var activity = (from c in dataCrudActivities
                                            where c.EntityName == entity.Entity && c.FileId == entity.FileId
                                            select c).ToList();
                            foreach (var crudActivity in activity)
                            {
                                if (crudActivity.InsertOrCreate == "Y")
                                    dbCrudActivity.Add("C");
                                if (crudActivity.SelectOrRead == "Y")
                                    dbCrudActivity.Add("R");
                                if (crudActivity.Update == "Y")
                                    dbCrudActivity.Add("U");
                                if (crudActivity.Delete == "Y")
                                    dbCrudActivity.Add("D");
                            }
                            if (dbCrudActivity.Count != 0)
                                linkText = string.Join(",", dbCrudActivity);

                            nodeId++;
                            var newNode = new Node
                            {
                                Id = nodeId,
                                Name = entity.Entity,
                                Color = "#ababab",
                                ShapeId = "RoundRect",
                                Height = "18",
                                Width = "200",
                                FileId = entity.FileId,
                                ProgramId = entity.FileId
                            };
                            var newLink = new Link
                            {
                                Origin = mNode.Id,
                                Target = newNode.Id,
                                LinkText = linkText // TODO: We need to add actual CRUD activity name
                            };
                            nodesList.Add(newNode);
                            lstMainLinkList.Add(newLink);
                        }
                    }

                    lstNodes.AddRange(nodesList);
                    var flowChart = new FlowChart
                    {
                        Nodes = lstNodes,
                        Links = lstMainLinkList
                    };
                    var workflowNodeDetails = (from mNodes in flowChart.Nodes
                                               let strName = mNodes.Name
                                               select new ConnectivityStepReference
                                               {
                                                   NodeId = mNodes.Id,
                                                   Name = strName.Trim(),
                                                   Shape = mNodes.ShapeId,
                                                   Color = mNodes.Color,
                                                   ParentId = mNodes.ParentId,
                                                   StatementId = mNodes.Id,
                                                   ProjectId = Convert.ToInt32(projectId)
                                               }).ToList();
                    var workflowLinkDetails = flowChart.Links.Select(mLinks => new ConnectivityLinkReferece
                    {
                        Origin = mLinks.Origin,
                        Target = mLinks.Target,
                        LinkText = mLinks.LinkText,
                        StatementId = mLinks.StatementId,
                        ProjectId = Convert.ToInt32(projectId)
                    }).ToList();

                    var flowchartObject = new WorkFlowObjectDictionaryData
                    {
                        Nodes = workflowNodeDetails,
                        Links = workflowLinkDetails
                    };

                    #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                    stringBuilder.AppendLine("Started inserting nodes and links into database: (" + projectId +
                                             ")");
                    if (flowchartObject.Nodes.Count > 0)
                    {
                        var nodes = flowchartObject.Nodes.ToList();
                        foreach (var mNodes in nodes)
                        {
                            string strName = mNodes.Name;
                            var nodesData = new ConnectivityStepReference
                            {
                                NodeId = mNodes.NodeId,
                                Name = strName.Trim(),
                                Shape = mNodes.Shape,
                                Color = mNodes.Color,
                                ParentId = mNodes.ParentId,
                                StatementId = mNodes.StatementId,
                                ProjectId = Convert.ToInt32(projectId)
                            };
                            await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData).ConfigureAwait(false);
                        }
                    }

                    if (flowchartObject.Links.Count > 0)
                    {
                        var links = flowchartObject.Links.ToList();
                        foreach (var mLinks in links)
                        {
                            try
                            {
                                var liksData = new ConnectivityLinkReferece
                                {
                                    Origin = mLinks.Origin,
                                    Target = mLinks.Target,
                                    LinkText = mLinks.LinkText,
                                    StatementId = mLinks.StatementId,
                                    ProjectId = Convert.ToInt32(projectId)
                                };
                                await _codeVortoService.ConnectivityLinkRefereceRepository
                              .AddNewItem(liksData).ConfigureAwait(false);
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }
                        }
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                #endregion
                LogMessage.WriteLogMessage(stringBuilder);
                //stringBuilder.AppendLine("Started executing next process: GetAllStartingPoints(" + projectId + ")");
                //LogMessage.WriteLogMessage(stringBuilder);
                //IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                //await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
                // return Ok(flowChart);
            }
        }

        [HttpGet]
        public IHttpActionResult ApplyPseudoCodeConversion(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("Started processing pseudocode conversion for if statement (" + projectId + ")");
                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var languageId = projectDetails.LanguageId;

                #region Apply PseudoCode Conversion for if statement

                string strQuery =
                 " SELECT S.StatementID, S.FileId, S.ProjectId, S.BaseCommandId,S.OriginalStatement, S.PrimaryCommandId " +
                 " FROM statementreferencemaster S  WHERE S.ProjectId = " + projectId + " " +
                 " AND S.BaseCommandID = 1 and S.AlternateName is null; ";
                var appBlock = new MySqlDbConnectionBaseClass();
                DataSet dsStatementRefer = appBlock.ExecuteNonQuery(strQuery, "");
                string strQuery1 = "SELECT * FROM regexpatternmaster where LanguageId =" + languageId + "; ";
                DataSet dsRegexData = appBlock.ExecuteNonQuery(strQuery1, "");
                if (dsStatementRefer.Tables.Count > 0 && dsStatementRefer.Tables[0].Rows.Count > 0)
                {
                    for (int iCnt = 0; iCnt < dsStatementRefer.Tables[0].Rows.Count; iCnt++)
                    {
                        try
                        {
                            var strStatement = dsStatementRefer.Tables[0].Rows[iCnt]["OriginalStatement"].ToString();
                            var iStatementId = Convert.ToInt32(dsStatementRefer.Tables[0].Rows[iCnt]["StatementId"]);
                            var iBaseCommandId = Convert.ToInt32(dsStatementRefer.Tables[0].Rows[iCnt]["BaseCommandId"]);
                            DataRow[] matchedRegex = dsRegexData.Tables[0].Select("BaseCommandId=" + iBaseCommandId);
                            foreach (var row in matchedRegex)
                            {
                                try
                                {
                                    var strPattern = row["RegexPattern"].ToString();
                                    Regex regex = new Regex(strPattern);
                                    var strAlternateCodeRepresent = row["AlternateCodeRepresentation"].ToString();
                                    string strAlternateStatement = "";
                                    if (!regex.IsMatch(strStatement)) continue;
                                    int index = 1;
                                    foreach (Match match in regex.Matches(strStatement))
                                    {
                                        var aaa = match.Groups[1].Value;
                                        strAlternateStatement = strAlternateCodeRepresent.Replace("<<" + index + ">>", aaa);
                                        break;
                                    }
                                    strAlternateStatement = strAlternateStatement.Replace(" >= ", " is greater than or equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" <= ", " is less than or equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" => ", " is equal to or greater than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" =< ", " is equal to or less than ");
                                    strAlternateStatement = strAlternateStatement.Replace(">= ", " is greater than or equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace("<= ", " is less than or equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace("=> ", " is equal to or greater than ");
                                    strAlternateStatement = strAlternateStatement.Replace("=< ", " is equal to or less than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" != ", " is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" Ne ", " is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" <> ", " is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" Gt ", " is greater than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" Lt ", " is less than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" > ", " is greater than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" < ", " is less than ");
                                    strAlternateStatement = strAlternateStatement.Replace(" = ", " is equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" Not= ", " is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" NE ", " is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(")NE ", ") is not equal to ");
                                    strAlternateStatement = strAlternateStatement.Replace(" THEN", " ")
                                        .Replace(" Then", " ").Replace("then ", " ");
                                    strQuery = " UPDATE statementreferencemaster "
                                               + " SET AlterNatename = '" + strAlternateStatement.Replace("\'", "\"").Replace("\"", "") + "' "
                                               + " WHERE StatementId = " + iStatementId + " ";
                                    appBlock.ExecuteNonQuery(strQuery, "");

                                    strQuery = " UPDATE SecondTabProgramDetails "
                                               + " SET AlterNatename = '" +
                                               strAlternateStatement.Replace("\'", "\"") + "' "
                                               + " WHERE StatementId = " + iStatementId + " ";

                                    appBlock.ExecuteNonQuery(strQuery, "");

                                    strQuery = " UPDATE WorkflowTreeviewSecondTabDetails "
                                               + " SET AlterNatename = '" +
                                               strAlternateStatement.Replace("\'", "\"") + "' "
                                               + " WHERE StatementId = " + iStatementId + " ";

                                    appBlock.ExecuteNonQuery(strQuery, "");
                                    break;
                                }
                                catch (Exception exception)
                                {
                                    LogMessage.WriteExceptionLogMessage(exception);
                                }
                            }
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("done");
                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ViewSource(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId).ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    }).ConfigureAwait(false);
                foreach (var file in fileMaster)
                {
                    if (file.FilePath.EndsWith(".txt")) continue;
                    if (!File.Exists(file.FilePath)) continue;
                    var allLines = File.ReadAllLines(file.FilePath);
                    var strBuilder = new StringBuilder();
                    foreach (var stmt in allLines)
                    {
                        strBuilder.AppendLine(stmt);
                    }
                    string fileLines = strBuilder.ToString();
                    var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                    var viewsourcedata = new ViewSourceMaster
                    {
                        ViewSourceId = 0,
                        FileId = file.FileId,
                        SourceData = fileLines,
                        ProjectId = file.ProjectId
                    };
                    await viewSourceMasterRepository.AddNewItem(viewsourcedata).ConfigureAwait(false);
                }
            }
            return Ok("Processed");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForProjectInventory(int projectId, int solutionId,
            bool businessName = false, bool skipSame = false)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var entitiesToExclude = _codeVortoService.AppDbContextRepository
                    .EntitiesToExclude.ToList();

                string sqlQy = "SELECT * FROM FileMaster WHERE SolutionId = " + solutionId +
                               " AND FileTypeExtensionId != 11;";
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetDataFromSqlQuery<FileMaster>(sqlQy).ConfigureAwait(false);


                string qryDependancy = "SELECT * FROM DataDependency WHERE FileId IN " +
                                       "(SELECT FileId FROM FileMaster WHERE SolutionId = " + solutionId + ");";

                var allDataDependancies = await _codeVortoService.DataDependencyRepository
                    .GetDataFromSqlQuery<DataDependency>(qryDependancy).ConfigureAwait(false);

                var newDataDependenciesList = new List<DataDependency>();
                var excludedDependancy = new List<DataDependency>();
                foreach (var dataDependancy in allDataDependancies)
                {
                    if (entitiesToExclude.Any(e => e.FileId == dataDependancy.FileId))
                    {
                        excludedDependancy.Add(dataDependancy);
                        continue;
                    }
                    newDataDependenciesList.Add(dataDependancy);
                }
                allDataDependancies = newDataDependenciesList;

                string missingObjSql = "SELECT * FROM StatementReferenceMaster WHERE " +
                                       "ReferenceFileId = 0 AND BaseCommandId = 6 AND SolutionId = " + solutionId + ";";
                var allMissingRefs = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(missingObjSql).ConfigureAwait(false);

                string sqlAllClasses =
                    " Select * from StatementReferenceMaster Where BaseCommandId IN (19, 5, 6, 1) AND " +
                    " ProjectId IN ( Select ProjectId FROM ProjectMaster " +
                    " WHERE SolutionId = " + solutionId + ") ORDER BY FileId ASC;";
                var allClassNameDeclaredOld = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllClasses).ConfigureAwait(false);
                var allClassNameDeclaredNew = allClassNameDeclaredOld.Where(x => x.BaseCommandId == 19)
                    .Where(classNm => !string.IsNullOrEmpty(classNm.ClassNameDeclared)).ToList();

                string refSqlQuery = "SELECT * FROM StatementReferenceMaster WHERE ReferenceFileId " +
                                     "IN(SELECT FileId FROM FileMaster WHERE FileTypeExtensionId = 12); ";
                var allRefStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(refSqlQuery).ConfigureAwait(false);

                var allClassNameDeclared = new List<StatementReferenceMaster>();
                allClassNameDeclared.AddRange(allClassNameDeclaredNew);

                var allCallExternals = (from s in allClassNameDeclaredOld
                                        where s.BaseCommandId == 6 && !string.IsNullOrEmpty(s.ClassCalled)
                                        select s).ToList();

                var allCallExternalsWithOutClassCalled = (from s in allClassNameDeclaredOld
                                                          where s.BaseCommandId == 6
                                                          select s).ToList();

                var allComplexity = (from s in allClassNameDeclaredOld
                                     where s.BaseCommandId == 1 || s.BaseCommandId == 5
                                     select s).ToList();

                string qryWorkflowStatements = " Select * from WorkflowTreeViewSecondTabDetails " +
                                               " Where ProjectId = " + projectId + " AND ClassCalled IS NOT NULL;";

                var allWorkflowStatements = await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                    .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(qryWorkflowStatements)
                    .ConfigureAwait(false);

                var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);

                var universeDescriptor = await _codeVortoService.UniverseDescriptorRepository
                    .GetAllListItems(x => x.ProjectId == projectId && x.DescriptorId != 0 && x.StatementString.Contains("SUBR")).ConfigureAwait(false);
                var regEx = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);

                universeDescriptor = (from u in universeDescriptor
                                      where u.StatementString.Contains("SUBR")
                                      select u).ToList();
                /*
                 foreach (var uDescriptor in universeDescriptor)
                 {
                     string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                     uDescriptor.CompleteName = entityName;
                 }
                 */

                var universeDescriptorList = new List<UniverseDescriptorList>();
                foreach (var uDescriptor in universeDescriptor)
                {
                    string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                    var stringLstData = uDescriptor.StatementString.Split(';').ToList();
                    var dData = new List<string>();
                    foreach (var lstData in stringLstData)
                    {
                        // var regex = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);
                        var allGroups = regEx.Match(lstData).Groups;
                        int groupCnt = -1;
                        foreach (Group group in allGroups)
                        {
                            groupCnt++;
                            if (groupCnt == 0) continue;
                            string eName = group.Value;
                            eName = eName.Replace("'", "");
                            var subRoutineFile =
                            (from f in allFiles
                             where Path.GetFileNameWithoutExtension(f.FilePath) == eName && f.FileTypeExtensionId == 17
                             select f).ToList();
                            foreach (var sFile in subRoutineFile)
                            {
                                bool isPresentSubRoutine = universeDescriptorList.Any(x =>
                                    x.FileId == sFile.FileId && x.EntityName == entityName);
                                if (isPresentSubRoutine) continue;

                                universeDescriptorList.Add(new UniverseDescriptorList
                                {
                                    FileId = sFile.FileId,
                                    FileName = eName,
                                    EntityName = entityName,
                                    StatementList = uDescriptor.StatementString
                                });
                            }
                        }
                    }
                }

                #region Collect Nodes and Links...

                var nodeId = 1;
                var lstNodes = new List<Node>();
                foreach (var file in allFiles)
                {
                    // if (file.FileTypeExtensionId != 17) continue;
                    var cName = new List<StatementReferenceMaster>();
                    var name = Path.GetFileNameWithoutExtension(file.FileName);
                    var classNameDeclared = "";
                    if (solutionId == 5)
                    {
                        var pNameNew = file.ProjectMaster.ProjectName;
                        var pPathNew = file.ProjectMaster.PhysicalPath;
                        var classNameProgram = file.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(file.FileName, "").Replace("\\", ".");
                        var fileNameProgram = Path.GetFileNameWithoutExtension(file.FilePath);
                        classNameDeclared = pNameNew + "." + classNameProgram + fileNameProgram;
                        cName = (from c in allClassNameDeclared
                                 where c.FileId == file.FileId && c.BaseCommandId == 19
                                 select c).ToList();
                    }
                    else
                    {
                        cName = (from c in allClassNameDeclared
                                 where c.FileId == file.FileId && c.BaseCommandId == 19
                                 select c).ToList();
                        classNameDeclared = cName[0].ClassNameDeclared;
                    }
                    if (cName.Any())
                    {
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = name,
                            ShapeId = "RoundRect",
                            Height = "15",
                            Width = "100",
                            Color = "#00ffff", //Color.FromName("aqua").Name, // "#00ffff",
                            StatementId = cName[0].StatementId,
                            BaseCommandId = 19,
                            OriginalClassName = classNameDeclared,
                            StatementReferenceMaster = cName[0],
                            FileId = file.FileId
                        });
                        continue;
                    }
                    lstNodes.Add(new Node
                    {
                        Id = nodeId++,
                        Name = name,
                        ShapeId = "RoundRect",
                        Height = "15",
                        Width = "100",
                        Color = "#00ffff",
                        StatementId = 0,
                        BaseCommandId = 19,
                        OriginalClassName = classNameDeclared,
                        StatementReferenceMaster = null,
                        FileId = file.FileId
                    });
                }
                var lstLinks = new List<Link>();
                // lstNodes = lstNodes.OrderByDescending(s => s.FileId).ToList();
                foreach (var node in lstNodes)
                {
                    string className = node.OriginalClassName;
                    var allDistinctClasses =
                        (from cls in allCallExternals where cls.ClassCalled == className select cls).ToList();
                    foreach (var callExt in allDistinctClasses)
                    {
                        var cName = (from c in allClassNameDeclared
                                     where c.FileId == callExt.FileId && c.BaseCommandId == 19
                                     select c).ToList();
                        var originNode = lstNodes.Find(n =>
                        {
                            var firstOrDefault = cName.FirstOrDefault();
                            return firstOrDefault != null && n.OriginalClassName == firstOrDefault.ClassNameDeclared;
                        });
                        var targetNode = lstNodes.Find(n => n.OriginalClassName == className);
                        if (lstLinks.Any(s => s.Origin == originNode.Id && s.Target == targetNode.Id)) continue;

                        lstLinks.Add(new Link
                        {
                            Origin = originNode.Id,
                            Target = targetNode.Id,
                            LinkText = callExt.MethodCalled
                        });
                    }
                }

                #endregion

                var workflowDetailsForInventory = new List<ProjectInventory>();

                foreach (var node in lstNodes)
                {
                    var tempNodeList = new List<Node>();
                    var lstMainNodeList = new List<Node>();
                    var startNodes =
                        (from n in lstNodes where n.FileId == node.FileId && n.BaseCommandId == 19 select n).ToList();
                    tempNodeList.AddRange(startNodes);
                    lstMainNodeList.AddRange(startNodes);
                    foreach (var mNode in tempNodeList)
                    {
                        var origin = mNode.Id;
                        lstMainNodeList = lstLinks.GetAllChildNodesForLinkOrigin(lstNodes, origin, lstMainNodeList);
                    }

                    var file = allFiles.Find(f => f.FileId == node.FileId);
                    var allMissingCallExternals = (from s in allMissingRefs
                                                   where s.FileId == file.FileId && s.ReferenceFileId == 0 && s.BaseCommandId == 6
                                                   select s).ToList();
                    int fileTypeExtensionId = file.FileTypeExtensionId;
                    // if (fileTypeExtensionId != 17) continue;
                    bool isPgmOrSbr = fileTypeExtensionId == 9 || fileTypeExtensionId == 17 ||
                                      fileTypeExtensionId == 12;

                    var calledFrom =
                    (from c in allCallExternals
                     where c.ReferenceFileId == file.FileId
                     select c).Distinct().ToList();
                    int linesCount = allFiles.First(c => c.FileId == file.FileId).LinesCount;
                    int complexity = 0;
                    int internalCall = 0;
                    var allWorkFlowsTempList = new List<string>();
                    var userObjectTempList = new List<string>();
                    var userReportTempList = new List<string>();
                    var userQueriesTempList = new List<string>();
                    var dataDependancies = new List<string>();
                    var calledFromList = new List<string>();
                    var iDescriptorList = new List<string>();
                    var calledExternalList = new List<string>();

                    if (node.StatementReferenceMaster == null)
                        node.StatementReferenceMaster = new StatementReferenceMaster
                        {
                            BusinessName = ""
                        };

                    if (node.StatementReferenceMaster != null &&
                        string.IsNullOrEmpty(node.StatementReferenceMaster.BusinessName))
                        node.StatementReferenceMaster.BusinessName = "";
                    string description = node.StatementReferenceMaster.BusinessName
                        .Replace("PA ", "").Trim().TrimStart('-').Trim();
                    if (file.FileTypeExtensionId == 14 || file.FileTypeExtensionId == 15
                        || file.FileTypeExtensionId == 9 || file.FileTypeExtensionId == 10 ||
                        file.FileTypeExtensionId == 17)
                    {
                        complexity =
                            (from s in allComplexity where s.FileId == file.FileId && s.BaseCommandId == 1 select s)
                            .Count();
                        complexity = complexity + 1;
                    }
                    else
                    {
                        var allFileLines = await _codeVortoService.ViewSourceMasterRepository
                            .GetAllListItems(x => x.FileId == file.FileId).ConfigureAwait(false);
                        foreach (var lines in allFileLines)
                        {
                            var line =
                                lines.SourceData.Split('\n')
                                    .Where(n => !string.IsNullOrWhiteSpace(n))
                                    .Select(m => m.Trim())
                                    .ToList();
                            line.ForEach(d =>
                            {
                                complexity += d.Split(' ').Length;
                            });
                        }
                    }

                    string uObject = string.Empty;
                    string uReport = string.Empty;
                    string uQuery = string.Empty;
                    string uEntities = string.Empty;
                    string partInWorkflow = string.Empty;
                    string cFrom = string.Empty;
                    string cExternal = string.Empty;
                    if (node.StatementReferenceMaster != null)
                    {
                        var statementReferenceMaster = node.StatementReferenceMaster;
                        if (string.IsNullOrEmpty(statementReferenceMaster.ClassNameDeclared)) continue;
                        var allFileIds = lstMainNodeList.Select(nodeList => nodeList.FileId).ToList();
                        var usesObjects = (from f in allFileIds
                                           where allFiles.Any(d =>
                                               d.FileId == f && (d.FileTypeExtensionId == 14 || d.FileTypeExtensionId == 15
                                                                 || d.FileTypeExtensionId == 9 || d.FileTypeExtensionId == 10
                                                                 || d.FileTypeExtensionId == 17))
                                           select f).Distinct().ToList(); // Extension = 14
                        var usesReports =
                        (from f in allFileIds
                         where allFiles.Any(d => d.FileId == f && d.FileTypeExtensionId == 13)
                         select f).Distinct().ToList(); // Extension = 13
                        var usesQueries =
                        (from f in allFileIds
                         where allFiles.Any(d => d.FileId == f && d.FileTypeExtensionId == 16)
                         select f).Distinct().ToList(); // Extension = 16
                        var uObjects = (from t in lstNodes
                                        where usesObjects.Any(
                                            f => f == t.FileId && t.FileId != file.FileId &&
                                                 t.BaseCommandId == 19)
                                        select t).Distinct().ToList();
                        var uReports = (from t in lstNodes
                                        where usesReports.Any(f => f == t.FileId)
                                        select t).Distinct().ToList();
                        var uQueries = (from t in lstNodes
                                        where usesQueries.Any(f => f == t.FileId)
                                        select t).Distinct().ToList();
                        allFileIds.Add(file.FileId);
                        var dataDependanciesList =
                            (from d in allDataDependancies where d.FileId == file.FileId select d.Entity)
                            .Distinct().ToList();
                        foreach (var dependancy in dataDependanciesList)
                        {
                            if (dataDependancies.Any(
                                k => dependancy.ToUpper().StartsWith(k, StringComparison.OrdinalIgnoreCase)))
                                continue;
                            dataDependancies.Add(dependancy);
                        }
                        if (isPgmOrSbr)
                        {
                            var otherEntities = ExtractDataDependencyForPgmAndSbr(file, allDataDependancies,
                                allRefStatementMaster, dataDependancies);
                            dataDependancies.AddRange(otherEntities);
                        }

                        dataDependancies = dataDependancies.Distinct().ToList();
                        if (entitiesToExclude.Any(f => f.FileId == node.FileId))
                        {
                            dataDependancies = (from d in excludedDependancy
                                                where d.FileId == node.FileId
                                                select d.Entity).Distinct().ToList();
                        }

                        // Collect data for Participates in 
                        string className = node.StatementReferenceMaster.ClassNameDeclared;
                        var allClassCalled = allWorkflowStatements.Where(s => s.ClassCalled == className).ToList();
                        var allWorkflows = (from a in allActionWorkflows
                                            where allClassCalled.Any(s => s.ActionWorkflowId == a.ActionWorkflowId)
                                            select a).Distinct().ToList();
                        var fileWorkflows = (from a in allActionWorkflows
                                             where a.FileId == node.FileId
                                             select a).Distinct().ToList();
                        allWorkflows.AddRange(fileWorkflows);
                        var addedWorkflows = new List<int>();
                        foreach (var workflow in allWorkflows)
                        {
                            if (addedWorkflows.Any(s => s == workflow.ActionWorkflowId)) continue;
                            addedWorkflows.Add(workflow.ActionWorkflowId);
                            string link = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                                          workflow.MethodStatementId +
                                          "&aId=" + workflow.ActionWorkflowId;
                            var workFlowNm = "";
                            switch (solutionId)
                            {
                                case 6:
                                    workFlowNm = workflow.WorkflowName;
                                    break;
                                case 5:
                                    workFlowNm = workflow.OriginObject;
                                    break;
                                case 1:
                                    workFlowNm = workflow.OriginEventMethod;
                                    break;
                            };
                            if (string.IsNullOrEmpty(workFlowNm))
                                workFlowNm = workflow.WorkflowName;
                            var str = businessName
                                ? "<a href='javascript:void(0);' onclick='openLink(aDiv_" + workflow.ActionWorkflowId +
                                  ");' style='color: blue; text-decoration: underline;'>" +
                                  workflow.TechnicalAndBusinessName + "</a> <div style='display: none;' id='aDiv_" +
                                  workflow.ActionWorkflowId + "'>" + link + "</div> "
                                : "<a href='javascript:void(0);' onclick='openLink(aDiv_" + workflow.ActionWorkflowId +
                                  ");' style='color: blue; text-decoration: underline;'>" + workFlowNm +
                                  "</a> <div style='display: none;' id='aDiv_" +
                                  workflow.ActionWorkflowId + "'>" + link + "</div> ";
                            allWorkFlowsTempList.Add(str);
                        }
                        foreach (var uObj in uObjects)
                        {
                            if (uObj.FileId == node.FileId && skipSame) continue;
                            var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                      " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                      uObj.Name + "</a>";
                            userObjectTempList.Add(str);
                        }
                        foreach (var uObj in allMissingCallExternals)
                        {
                            if (string.IsNullOrEmpty(uObj.OriginalStatement)) continue;
                            int spaceFirstIndex = uObj.OriginalStatement.IndexOf(' ');
                            if (spaceFirstIndex < 0) spaceFirstIndex = 0;
                            string missingRef = uObj.OriginalStatement.Substring(spaceFirstIndex);
                            var str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                      "cursor: pointer; text-decoration: none;'>" + missingRef + " [ Missing ]</a>";
                            userObjectTempList.Add(str);
                        }

                        foreach (var uObj in uReports)
                        {
                            if (uObj.FileId == node.FileId && skipSame) continue;
                            var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                      " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                      uObj.Name + "</a>";
                            userReportTempList.Add(str);
                        }
                        foreach (var uObj in uQueries)
                        {
                            if (uObj.FileId == node.FileId && skipSame) continue;
                            var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                      " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                      uObj.Name + "</a>";
                            userQueriesTempList.Add(str);
                        }

                        //if (dataDependancies.Any())
                        //{
                        //    string str = "<li>" + dataDependancies[0] + "</li>";
                        //    dataDependancies[0] = str;
                        //}

                        if (userObjectTempList.Any())
                        {
                            string str = "<li>" + userObjectTempList[0] + "</li>";
                            userObjectTempList[0] = str;
                        }
                        internalCall = (from c in allComplexity
                                        where c.FileId == file.FileId && c.BaseCommandId == 5
                                        select c).Count();
                        var externalCallList = (from x in allCallExternalsWithOutClassCalled
                                                where x.FileId == file.FileId && x.BaseCommandId == 6
                                                select x).ToList();
                        var refFileIds = new List<int>();
                        foreach (var eCall in externalCallList)
                        {
                            string str;
                            int refFileId = eCall.ReferenceFileId;
                            if (refFileId == 0)
                            {
                                str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                         "cursor: pointer; text-decoration: none;'>" + eCall.OriginalStatement +
                                         " [ Missing ]</a>";
                                calledExternalList.Add(str);
                                continue;
                            }
                            var refFileMaster = allFiles.FirstOrDefault(x => x.FileId == refFileId);
                            if (refFileMaster == null) continue;
                            bool exist = refFileIds.Any(f => f == refFileMaster.FileId);
                            if (exist) continue;
                            refFileIds.Add(refFileMaster.FileId);
                            str =
                               "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                               " onclick='includeStateDialog(" + refFileMaster.FileId + ");'>" +
                              refFileMaster.FileName + "</a>";
                            calledExternalList.Add(str);
                        }

                        var addedFileId = new List<int>();
                        foreach (var uObj in calledFrom)
                        {
                            if (addedFileId.Any(f => f == uObj.FileMaster.FileId)) continue;
                            var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                      " onclick='includeStateDialog(" + uObj.FileMaster.FileId + ");'>" +
                                      uObj.FileMaster.FileName + "</a>";
                            addedFileId.Add(uObj.FileMaster.FileId);
                            calledFromList.Add(str);
                        }
                        if (calledFromList.Any())
                        {
                            string str = "<li>" + calledFromList[0] + "</li>";
                            calledFromList[0] = str;
                        }

                        if (userReportTempList.Any())
                        {
                            string str = "<li>" + userReportTempList[0] + "</li>";
                            userReportTempList[0] = str;
                        }

                        if (userQueriesTempList.Any())
                        {
                            string str = "<li>" + userQueriesTempList[0] + "</li>";
                            userQueriesTempList[0] = str;
                        }

                        if (allWorkFlowsTempList.Any())
                        {
                            string str = "<li>" + allWorkFlowsTempList[0] + "</li>";
                            allWorkFlowsTempList[0] = str;
                        }

                        if (calledExternalList.Any())
                        {
                            string str = "<li>" + calledExternalList[0] + "</li>";
                            calledExternalList[0] = str;
                        }
                        if (dataDependancies.Any())
                        {
                            string str = "<li>" + dataDependancies[0] + "</li>";
                            dataDependancies[0] = str;
                        }
                        uObject = string.Join("<li>", userObjectTempList);
                        uReport = string.Join("<li>", userReportTempList);
                        uQuery = string.Join("<li>", userQueriesTempList);
                        uEntities = string.Join("<li>", dataDependancies);
                        cFrom = string.Join("<li>", calledFromList);
                        cExternal = string.Join("<li>", calledExternalList);
                        partInWorkflow = string.Join("<li>", allWorkFlowsTempList);

                    }
                    var callFormFinal = string.Empty;
                    callFormFinal = calledFromList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                          node.Id + "'>" + cFrom + " </div> " : "-";

                    // For I-Descriptor...
                    // string sbrFileName = Path.GetFileNameWithoutExtension(file.FilePath);
                    // var regEx = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);
                    /*
                    var iDescriptor = (from u in universeDescriptor
                                       where u.StatementString.Contains(sbrFileName) && regEx.IsMatch(u.StatementString)
                                       select u).Distinct().ToList();
                    */
                    var iDescriptor = (from u in universeDescriptorList
                                       where file.FileId == u.FileId
                                       select u).Distinct().ToList();
                    int iDescpt = 0;
                    if (iDescriptor.Any())
                    {
                        foreach (var iDescptor in iDescriptor)
                        {

                            var dData = iDescptor.StatementList.Split(';').ToList();
                            var dList = new List<string>();
                            int index = 0;
                            iDescpt++;
                            foreach (var uObj in dData)
                            {
                                index++;
                                var str = index + ")&nbsp;<a href='javascript:void(0);' style='color: black; font-size: 14px; cursor: default; line-height:22px;'>" +
                                     uObj + "</a>";

                                dList.Add(str);
                            }
                            var uStatementList = string.Join("<br />", dList);

                            string divId = "iDept_" + iDescptor.FileId + "_" + iDescpt;
                            var strStatement = dList.Any() ? "<a href='javascript:void(0);' onclick=showIDespt('" + divId + "') style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                 iDescptor.EntityName + " </a> <div lang='" + iDescptor.EntityName + "' style='display: none;' id='" + divId + "'>" + uStatementList + " </div> " : "-";
                            iDescriptorList.Add(strStatement);
                        }

                        if (iDescriptorList.Any())
                        {
                            string str = "<li>" + iDescriptorList[0] + "</li>";
                            iDescriptorList[0] = str;
                        }
                        var iDescrt = string.Join("<li>", iDescriptorList);
                        int callFormCnt = 0;
                        if (iDescriptorList.Any())
                        {
                            if (callFormFinal == "-")
                            {
                                callFormCnt = iDescriptorList.Count;
                                callFormFinal = "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                                                ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" + callFormCnt +
                                                " Called From </a> <div style='display: none;' id='calledFrom_" +
                                                node.Id +
                                                "'><div style='height: 10px;margin-bottom: 20px;'><h4> I-Desc </h4></div> " +
                                                iDescrt + "  </div> ";
                            }
                            else
                            {
                                callFormCnt = calledFromList.Count + iDescriptorList.Count;
                                callFormFinal = calledFromList.Count > 0
                                    ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                                      ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                      callFormCnt +
                                      " Called From </a> <div style='display: none;' id='calledFrom_" +
                                      node.Id + "'>" + cFrom +
                                      " <div style='height: 10px;margin-bottom: 20px; margin-top: 20px;'><h4> I-Desc </h4></div> " +
                                      iDescrt + "  </div>" : "-";
                            }
                        }

                        /*
                        callFormFinal = callFormFinal == "-"
                            ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                              node.Id + "'>" + cFrom +
                              "<div style='height: 10px;margin-bottom: 25px;'><h4> I-Descriptor </h4></div> " + iDescrt + "  </div> "
                            : callFormFinal = callFormFinal+ "<div style='height: 10px;margin-bottom: 25px;'><h4> I-Descriptor </h4></div> " + iDescrt + "  </div> ";
                        */
                    }
                    var inventoryDetails = new ProjectInventory // InventoryReport
                    {
                        ObjectName =
                            "<pre><a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                            " onclick='includeStateDialog(" + node.FileId + ");'>" + node.Name + "</a></pre>",
                        Description = description,
                        ExtenstionType = file.FileTypeExtensionReference.FileTypeName,
                        LoC = linesCount,
                        Complexity = complexity,
                        InternalCall = internalCall,
                        // ExternalCall = externalCall,
                        ExternalCall = calledExternalList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvCExternal_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              calledExternalList.Count + " Call External(s) </a> <div style='display: none;' id='dvCExternal_" +
                              node.Id + "'>" + cExternal + " </div> "
                            : "-",
                        UsesObjects = userObjectTempList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvObjects_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              userObjectTempList.Count + " Object(s) </a> <div style='display: none;' id='dvObjects_" +
                              node.Id + "'>" + uObject + " </div> "
                            : "-",
                        UsesReports = userReportTempList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvReports_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              userReportTempList.Count + " Report(s) </a> <div style='display: none;' id='dvReports_" +
                              node.Id + "'>" + uReport + " </div> "
                            : "-",
                        UsesQueries = userQueriesTempList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvQuery_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              userQueriesTempList.Count + " Query(ies) </a> <div style='display: none;' id='dvQuery_" +
                              node.Id + "'>" + uQuery + " </div> "
                            : "-",
                        UsesEntities = dataDependancies.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvDataDepend_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              dataDependancies.Count +
                              " Entity(ies) </a> <div style='display: none;' id='dvDataDepend_" + node.Id + "'>" +
                              uEntities + " </div> "
                            : "-",
                        ParticipateInWorkflow = allWorkFlowsTempList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvPartInWork_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              allWorkFlowsTempList.Count +
                              " Workflow(s) </a> <div style='display: none;' id='dvPartInWork_" + node.Id + "'>" +
                              partInWorkflow + " </div> "
                            : "-",
                        CalledFrom = callFormFinal,
                        // calledFromList.Count > 0
                        //    ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                        //      ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                        //      calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                        //      node.Id + "'>" + cFrom + " </div> "
                        //    : "-",
                        ProjectId = projectId,
                        SolutionId = solutionId,
                        FileId = file.FileId,
                        CallingTo = null
                    };

                    using (var appDbContext = new AppDbContext())
                    {
                        appDbContext.Set<ProjectInventory>().Add(inventoryDetails);
                        await appDbContext.SaveChangesAsync();
                        workflowDetailsForInventory.Add(inventoryDetails);
                    }
                }
                return Ok(workflowDetailsForInventory);
                // return Ok("Done processing inventory for Project Id: " + solutionId);
            }
        }

        private List<string> ExtractDataDependencyForPgmAndSbr(FileMaster fileMaster,
            List<DataDependency> dataDependencies, List<StatementReferenceMaster> lstStatementRef,
            List<string> entitiesList)
        {
            var allIncludeRefs = (from s in lstStatementRef where s.FileId == fileMaster.FileId select s).ToList();
            foreach (var includeRef in allIncludeRefs)
            {
                var allIncludes = (from i in dataDependencies where i.FileId == includeRef.ReferenceFileId select i)
                    .ToList();

                entitiesList.AddRange(allIncludes.Select(include => include.Entity));

                foreach (var include in allIncludes)
                {
                    if (entitiesList.Any(i => i == include.Entity)) continue;
                    var newEntitiesList = ExtractDataDependencyForPgmAndSbr(include.FileMaster, dataDependencies,
                        lstStatementRef, new List<string>());
                    entitiesList.AddRange(newEntitiesList);
                }
            }
            return entitiesList;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessDataDependency(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine("Started process for data dependency:");
                    var fileMasterData =
                        await _codeVortoService.FileMasterRepository.GetAllListItems(p => p.ProjectId == projectId);
                    object[] andParameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""}
                    };
                    var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition",
                            andParameters);
                    var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;
                    var regexSql = new Regex(@"[\""]SELECT \*");
                    foreach (var constructor in fileMasterData)
                    {
                        var selectStatName = statementReferenceMasters.
                            Where(x => x.ProjectId == projectId
                            && x.FileId == constructor.FileId
                            && (x.OriginalStatement.Contains("\"Select")
                            || x.OriginalStatement.Contains("\"SELECT"))).ToList();

                        string result = string.Empty;
                        var lstDataDependency = new List<DataDependency>();
                        if (!selectStatName.Any()) continue;

                        foreach (var itemSelect in selectStatName)
                        {
                            int aaa;
                            string bbb;
                            string attrStmt;
                            string attributes = null;

                            if (regexSql.IsMatch(itemSelect.OriginalStatement.ToUpper().TrimEnd()))
                            {
                                if (itemSelect.OriginalStatement.Contains("where") ||
                                    itemSelect.OriginalStatement.Contains("WHERE"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("where",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(0, aaa).Trim();
                                    bbb = RemoveCheckSpecialChars(bbb);
                                    result = bbb.Trim().Split(' ').LastOrDefault();
                                    if (result.Contains(')'))
                                    {
                                        result = result.Replace(")", "");
                                    }
                                    attrStmt = itemSelect.OriginalStatement.Substring(aaa).Trim();
                                    string[] value = attrStmt.Split(' ');
                                    for (int i = 0; i < value.Length; i++)
                                    {
                                        if (!value[i].StartsWith("=") && value[i].Contains("="))
                                        {
                                            attributes = attributes + "," + value[i].Split('=').FirstOrDefault();
                                        }
                                        else
                                        {
                                            if (value[i] == "=" || value[i] == "=\"")
                                            {
                                                attributes = attributes + "," + value[i - 1];
                                            }
                                        }
                                    }

                                    if (!string.IsNullOrEmpty(attributes))
                                    {
                                        attributes = RemoveSpecialChars(attributes);
                                        attributes = attributes.TrimStart(',').Trim();
                                    }
                                }
                                else
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim();
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);
                                    if (result.Contains(')'))
                                    {
                                        result = result.Replace(")", "");
                                    }
                                }
                            }
                            else
                            {
                                if (itemSelect.OriginalStatement.Contains("from") ||
                                    itemSelect.OriginalStatement.Contains("FROM"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim().Replace("GROUP", "")
                                        .Replace("WHERE", "");
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);
                                    if (result.Contains(')'))
                                    {
                                        result = result.Replace(")", "");
                                    }
                                    attrStmt = itemSelect.OriginalStatement.Substring(0, aaa).Split('=')
                                        .LastOrDefault().Trim().Replace("Select", "").Replace("SELECT", "")
                                        .Replace("DISTINCTROW", "").Trim();

                                    if (attrStmt.Contains(","))
                                    {
                                        string[] value = attrStmt.Split(',');

                                        attributes = value.Aggregate(attributes,
                                            (current, t) => current + "," + t.TrimStart().TrimEnd());
                                    }
                                    else
                                    {
                                        attributes = attrStmt;
                                    }
                                    attributes = RemoveSpecialChars(attributes);
                                    attributes = attributes.TrimStart(',').Trim();
                                }
                            }
                            var datadependency = new DataDependency
                            {
                                DataDepedencyId = 0,
                                ProjectId = projectId,
                                FileId = constructor.FileId,
                                Entity = result,
                                Attributes = attributes
                            };
                            List<int> iCnt = (from dd in lstDataDependency
                                              where dd.Entity.ToLower() == result.ToLower() && dd.FileId == constructor.FileId
                                              select dd.FileId).ToList();
                            if (iCnt.Count == 0)
                            {
                                lstDataDependency.Add(datadependency);
                            }
                        }
                        await _codeVortoService.DataDependencyRepository.BulkInsert(lstDataDependency);
                    }
                    #region region  for DataDependancy Begin InputTables...
                    foreach (var constructor in fileMasterData)
                    {
                        var selectStatName = statementReferenceMasters.
                            Where(x => x.ProjectId == projectId && x.FileId == constructor.FileId
                                       && (x.OriginalStatement.StartsWith("Begin InputTables")
                                           || x.OriginalStatement.StartsWith("\"Begin InputTables"))).ToList();

                        if (!selectStatName.Any()) continue;
                        var sqlstatementReferMast =
                            " select * from statementreferencemaster where StatementId Between(select statementId from statementreferencemaster " +
                            " where OriginalStatement ='Begin InputTables' and StatementId >= " +
                            selectStatName[0].StatementId +
                            " LIMIT 1) and(select StatementId From StatementReferenceMaster Where OriginalStatement ='End' AND StatementId >= " +
                            selectStatName[0].StatementId + " LIMIT 1) And StatementId >= " +
                            selectStatName[0].StatementId +
                            "; ";
                        var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(sqlstatementReferMast);

                        if (!selectStatName.Any()) continue;
                        foreach (var itemSelect in statementReference)
                        {
                            if (!itemSelect.OriginalStatement.StartsWith("Name")) continue;
                            var mainStatement = itemSelect.OriginalStatement.Between("\"", "\"");
                            var datadependency = new DataDependency
                            {
                                DataDepedencyId = 0,
                                ProjectId = projectId,
                                FileId = constructor.FileId,
                                Entity = mainStatement,
                                Attributes = null
                            };
                            await _codeVortoService.DataDependencyRepository.AddNewItem(datadependency);
                        }
                    }
                    #endregion

                    #region region for DataDependancy for RecordSource

                    foreach (var constructor in fileMasterData)
                    {

                        var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                                  && x.FileId == constructor.FileId &&
                                                                                  (x.OriginalStatement.StartsWith(
                                                                                       "RecordSource") ||
                                                                                   x.OriginalStatement.StartsWith(
                                                                                       "\"RecordSource"))).ToList();
                        if (!selectStatName.Any()) continue;
                        foreach (var statement in selectStatName)
                        {
                            var sqlQuery = "select * from statementreferencemaster where ClassNameDeclared='" +
                                           statement.ClassCalled + "';";
                            var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                            if (statementReference.Count != 0) continue;
                            var mainStatement = statement.OriginalStatement.Between("\"", "\"");
                            if (mainStatement.StartsWith("Select") || mainStatement.StartsWith("SELECT")) continue;
                            var datadependency = new DataDependency
                            {
                                DataDepedencyId = 0,
                                ProjectId = projectId,
                                FileId = constructor.FileId,
                                Entity = mainStatement,
                                Attributes = null
                            };
                            await _codeVortoService.DataDependencyRepository.AddNewItem(datadependency);
                        }
                    }

                    #endregion
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                stringBuilder.AppendLine("Ended process of Data dependency");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("DataDepenceny process successfully!");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessDbCrudActivity(int projectId)
        {
            #region Insert data into DbCrudActivity
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var dataDependancyList =
                        await _codeVortoService.DataDependencyRepository
                            .GetAllListItems(p => p.ProjectId == projectId).ConfigureAwait(false);
                    foreach (var dataList in dataDependancyList)
                    {
                        string sqlQry = "select * from statementreferencemaster where projectId=" + projectId +
                                        " and FileId =" + dataList.FileId + " AND OriginalStatement like '%" +
                                        dataList.Entity +
                                        "%';";
                        var statementList =
                            await
                                _codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "Y";

                        var dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = dataList.Entity,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = dataList.FileId
                        };

                        foreach (var statementRef in statementList)
                        {
                            var dataObjectType = statementRef.OriginalStatement.Replace("Set ", "");
                            string variableName = dataObjectType.Split(new[] {"="}, StringSplitOptions.None)[0];
                            variableName = variableName.Trim().Replace("'", "");
                            string sqlDataIoStats =
                                " SELECT * FROM StatementReferenceMaster WHERE  FileId = " + statementRef.FileId +
                                " AND ProjectId = " + projectId + " AND ( OriginalStatement LIKE '%" + variableName +
                                ".AddNew%' " +
                                " OR OriginalStatement LIKE '%" + variableName + ".Update%' " +
                                " OR OriginalStatement LIKE '%" + variableName + ".Delete%');";
                            var allDataIoStatements = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataIoStats).ConfigureAwait(false);

                            foreach (string sss in allDataIoStatements.Select(s => s.OriginalStatement))
                            {
                                if (sss.Contains(".Update")) update = "Y";
                                if (sss.Contains(".Delete")) delete = "Y";
                                if (sss.Contains(".AddNew")) iR = "Y";
                            }
                            dbCurdActivity = new DbCrudActivity
                            {
                                EntityName = dataList.Entity,
                                SelectOrRead = select,
                                InsertOrCreate = iR,
                                Update = update,
                                Delete = delete,
                                FileId = dataList.FileId
                            };
                        }
                        string entity = dataList.Entity;
                        int fileId = dataList.FileId;
                        Expression<Func<DbCrudActivity, bool>> expression =
                            x => x.EntityName == entity && x.FileId == fileId;
                        var crud = await _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                            .ConfigureAwait(false);
                        if (crud.Count != 0) continue;
                        await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
            }
            #endregion

            return Ok("Process DbCrudActivity completed!");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateProjectStatus(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                projectMaster.Processed = 1;
                projectMaster.ProcessedDate = DateTime.Now;
                projectMaster.ProcessedTime = DateTime.Now;
                await _codeVortoService.ProjectMasterRepository.UpdateItem(projectMaster);

                return Ok(projectMaster);
            }
        }

    }

    public class WorkFlowObjectDictionaryData
    {
        public ActionWorkflows ActionWorkflows { get; set; }
        public List<ConnectivityLinkReferece> Links { get; set; }
        public List<ConnectivityStepReference> Nodes { get; set; }
    }

    public enum NodeType
    {
        Rectangle,
        Start,
        End,
        Decison,
        Decision2
    }

    public class ClsGraphMlEngine
    {
        // returns the header for filename
        public string GetHeader(string sProcessName)
        {
            var sReturn =
                "<graphml xmlns='http://graphml.graphdrawing.org/xmlns/graphml' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' ";
            sReturn = sReturn +
                      " xsi:schemaLocation = 'http://graphml.graphdrawing.org/xmlns/graphml http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd'";
            sReturn = sReturn + " xmlns:y='http://www.yworks.com/xml/graphml'>";
            sReturn = sReturn +
                      " <key id='d0' for='node' yfiles.type='nodegraphics'/> <key id='d1' for='edge' yfiles.type='edgegraphics'/>";
            sReturn = sReturn +
                      " <key id='d2' for='node' attr.name='url' attr.type='string'/> <key id='d3' for='node' attr.name='description' attr.type='string'/>";
            sReturn = sReturn + " <graph id='" + sProcessName + "' edgedefault='directed'>";

            return sReturn;
        }

        // returns footer for file
        public string GetFooter()
        {
            return " </graph> </graphml>";
        }

        //return node construct
        public string GetNodeStream(NodeType typeId, string sNodeLabel, string sNodeIdText, int width, int height,
            string color)
        {
            if (color.ToLower() == "lightgray")
            {
                color = "#d3d3d3";
            }
            else if (color.ToLower() == "lightgreen")
            {
                color = "#90ee90";
            }
            else if (color.ToLower() == "lightblue")
            {
                color = "#add8e6";
            }
            else if (color.ToLower() == "lightcyan")
            {
                color = "#e0ffff";
            }
            else if (color.ToLower() == "lightpink")
            {
                color = "#FFB6C1";
            }
            else if (color.ToLower().Contains("light"))
            {
                color = "#c0c0c0";
            }
            string sReturn = "";
            switch ((int)typeId)
            {
                case 0:
                    sReturn = "<node id='" + sNodeIdText +
                              "' statementnumber='96'> <data key='d0'> <y:ShapeNode> <y:NodeLabel fontSize='14' fontFamily='Dialog'>";
                    sReturn = sReturn + sNodeLabel + "</y:NodeLabel> ";
                    sReturn = sReturn + " <y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height +
                              "' /> <y:Shape type='roundrectangle'/> <y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/>";
                    sReturn = sReturn + "<y:Fill  color='" + color + "' transparent='false' color2='" + color +
                              "'/> <y:BorderStyle type='line' width='1.0'  color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 1:
                    sReturn = "<node id='" + sNodeIdText + "' statementnumber='96'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn + "<y:Shape type='roundrectangle'/><y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/><y:Fill  color='" + color + "' transparent='false' color2='" +
                              color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 2:
                    sReturn = "<node id='" + sNodeIdText + "' statementnumber='96'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn + "<y:Shape type='roundrectangle'/><y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/><y:Fill  color='" + color + "' transparent='false' color2='" +
                              color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 3:
                    sReturn = "<node id='" + sNodeIdText + "' statementnumber='99'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn +
                              "<y:Shape type='diamond'/> <y:DropShadow color='#c0c0c0' offsetX='6' offsetY='8'/><y:Fill   color='" +
                              color + "' transparent='false' color2='" + color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
            }

            return sReturn;
        }

        // pass lable as NONE is nothign is to be printed on the link
        //link color is in RRGGBB format
        public string ReturnLinkConstruct(string orginId, string destId, string sLinkLabel,
            string sLinkIdText = "NONE", string sLinkColor = "ffffff")
        {
            string sReturn;

            if (sLinkLabel.Trim().ToUpper() == "NONE")
            {
                // no node label provided so no need to place node text
                sReturn = "<edge id='" + sLinkIdText + "' source='" + orginId + "' target='" + destId + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn + "<y:LineStyle type='line' width='2' color='#" + sLinkColor +
                          "'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            else if (sLinkLabel.Trim().ToUpper() == "")
            {
                // no node label provided so no need to place node text
                sReturn = "<edge id='" + sLinkIdText + "' source='" + orginId + "' target='" + destId + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn +
                          "<y:LineStyle type='line' width='2' color='#000000'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            else
            {
                sReturn = "<edge id='" + sLinkIdText + "' source='" + orginId + "' target='" + destId + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn +
                          "<y:LineStyle type='line' width='2' color='#000000'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                //sReturn = sReturn + "<y:EdgeLabel backgroundColor='#000000' textColor='#ffffff' fontSize='12' fontFamily='Dialog' modelName='three_center' modelPosition='center'> " + sLinkLabel + "</y:EdgeLabel>";
                sReturn = sReturn +
                          "<y:EdgeLabel backgroundColor='#ffffff' textColor='#000000' fontSize='12' fontFamily='Dialog' modelName='three_center' modelPosition='center'> " +
                          sLinkLabel + "</y:EdgeLabel>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            return sReturn;
        }
    }
}