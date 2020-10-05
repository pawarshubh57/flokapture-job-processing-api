using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using TreeView = BusinessLayer.Models.TreeView;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicVersion2Controller
    {
        private int commanClassProjId = 9;
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("\n" + "Started to collect all actionworkflow for project: (" + projectId + ")");

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                        " and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch')" +
                        " AND CreatedBy=1 AND Processed = 0 ; ").ConfigureAwait(false);
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    if (workflow.ProjectId == null) return NotFound();
                    var workFlowProjectId = workflow.ProjectId.Value;
                    stringBuilder.AppendLine("Started executing next process: GetWorkFlowWorkSpace(" + projectId + ")");
                    await GetWorkFlowWorkSpace(workFlowProjectId, workflow.MethodStatementId,
                        workflow.ActionWorkflowId).ConfigureAwait(false);

                    workflow.Processed = 1;
                    workflow.ProjectMaster = null;
                    await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow).ConfigureAwait(false);
                }
                stringBuilder.AppendLine("Started executing next process: ProcessForActionWorkFlowDetails(" + projectId +
                                                ")");
                await ProcessForActionWorkFlowDetails(projectId).ConfigureAwait(false); //Inserted record into actionworkflowdetails table

                LogMessage.WriteLogMessage(stringBuilder);

                var responseMessage = await UpdateSecondTabAlternateNames(projectId).ConfigureAwait(false);

                return Ok(responseMessage);
            }
        }

        private async Task<HttpResponseMessage> UpdateSecondTabAlternateNames(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters = 
                {
                    new MySqlParameter("bCmdId", MySqlDbType.Int32){Value = 5},
                    new MySqlParameter("nCmdId", MySqlDbType.Int32){Value = 1},
                    new MySqlParameter("prjId", MySqlDbType.Int32){Value = projectId}
                };
                await _codeVortoService.SecondTabProgramDetailsRepository
                    .ExecuteStoreProcedure<SecondTabProgramDetails>("SpUpdateSecondTabData", parameters).ConfigureAwait(false);

                var responseMessage = new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new StringContent("Alternate names updated successfully")
                };
                return responseMessage;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForActionWorkFlowDetails(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlWorkflows = " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                                      " and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(sqlWorkflows).ConfigureAwait(false);

                var actionWorkflowDetailsWithProjectIdRepository =
                    new ActionWorkflowDetailsRepository(new AppDbContext());

                var appDbContext = new AppDbContext();
                var sqlQueery = "Select * from WorkflowTreeviewTabFirstDetails Where ProjectId = " + projectId + ";";

                var workflowTreeviewTabFirstDetails = await appDbContext.WorkflowTreeviewTabFirstDetails
                    .SqlQuery(sqlQueery).ToListAsync().ConfigureAwait(false);
                List<int> firstTab = (from details in workflowTreeviewTabFirstDetails
                                      let pId = details.ProgramId ?? 0
                                      where pId != 0
                                      select details.ProgramId ?? 0).ToList();
                firstTab = firstTab.Distinct().ToList();
                var programId = String.Join(",", firstTab);
                if (string.IsNullOrEmpty(programId))
                {
                    programId = 0.ToString();
                }
                var sqlQueryFirst = " select * from firsttabprogramdetails where BaseCommandId IN(5, 6, 1) " +
                                    " and ProgramId IN(" + programId + ")";

                var firstTabProgramDetails = await appDbContext.FirstTabProgramDetails
                    .SqlQuery(sqlQueryFirst).ToListAsync().ConfigureAwait(false);

                foreach (var workflow in workflowRef)
                {
                    var allPrograms = (from w in workflowTreeviewTabFirstDetails
                                       where w.ActionWorkflowId == workflow.ActionWorkflowId
                                             && w.ProgramId != 0
                                       select w.ProgramId).Distinct().ToList();

                    var callExternal = workflowTreeviewTabFirstDetails
                        .Count(d => (d.BaseCommandId == 6) && (d.ActionWorkflowId == workflow.ActionWorkflowId));

                    var completeProgramData = firstTabProgramDetails.AsQueryable()
                        .Where(s => allPrograms.Any(g => g == s.ProgramId)).ToList();

                    var asd = completeProgramData.Where(s => s.BaseCommandId == 6).ToList().Count;

                    callExternal = callExternal + asd;

                    var asd12 = completeProgramData.Where(s => s.BaseCommandId == 5).ToList().Count;

                    var callInternal =
                        firstTabProgramDetails.Count(
                            d => (d.BaseCommandId == 5) && (d.ActionWorkflowId == workflow.ActionWorkflowId));

                    callInternal = callInternal + asd12;

                    var decisionCount = workflowTreeviewTabFirstDetails.Count(d =>
                        (d.BaseCommandId == 1) && (d.ActionWorkflowId == workflow.ActionWorkflowId)) + 1;

                    decisionCount = decisionCount + completeProgramData.Count(d => d.BaseCommandId == 1);

                    var tspName = workflow.WorkflowName;
                    tspName = tspName.Split(new[] { "Sub", "Function" }, StringSplitOptions.None).LastOrDefault();

                    string disabled = string.Empty;
                    string btnStyle = "style='margin-top :5px;'";
                    string pageUrl = "workflow_workspace.html?prjId=" + projectId + "&stId=" +
                                     workflow.MethodStatementId + "";
                    if (workflow.Processed != null && workflow.Processed.Value == 0)
                    {
                        disabled = "disabled='disabled' title='Workflow yet to process'";
                        btnStyle = "style='margin-top :5px; background-color: #b1c8c1;'";
                        pageUrl = "#";
                    }
                    var workFlowDetails = new ActionWorkflowDetails
                    {
                        DecisionCount = decisionCount,
                        ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                        InternalCalls = callInternal > 0 ? "Yes" : "No",
                        View =
                            "<a " + disabled + " href='" + pageUrl + "'>" +
                            "<button " + btnStyle + " " + disabled + " class='btn btn-mint'>View</button> </a>" +
                            " &nbsp;<a href='#'><button style='margin-top : 5px;' class='btn btn-mint'>Rename</button> </a> ",
                        OriginObject = workflow.OriginEventMethod,
                        WorkflowName = tspName,
                        ProjectName = workflow.ProjectMaster.ProjectName, //  + "  " + count,
                        ActionWorkflowId = workflow.ActionWorkflowId,
                        ProjectId = projectId
                    };
                    await actionWorkflowDetailsWithProjectIdRepository.AddNewItem(workFlowDetails).ConfigureAwait(false);
                }
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, int stmtId, int actionWorkflowId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process
                var stringBuilder = new StringBuilder();

                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();

                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                        .GetAllItems(p => (p.MethodStatementId != 0) && (p.ProjectId == projectId) && (p.Processed == 0));
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpBaseStatementMaster(" + projectId + "," + clsName + ",)");
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = clsName},
                    new MySqlParameter("@classSplitName", MySqlDbType.VarChar)
                    {
                        Value = clsName.Split('.').LastOrDefault()
                    }
                };

                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.ExecuteStoreProcedure<StatementReferenceMaster>
                        ("SpBaseStatementMaster", param);


                string jName = baseStatementMaster[0].ClassNameDeclared;
                jName = jName.Split('.').LastOrDefault();
                _appDbContext = new AppDbContext();
                var jclMenuName = await _appDbContext.UniverseFileMenu
                    .Where(menu => menu.ActionExecuted == jName)
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.FirstOrDefault();
                    });
                stringBuilder.AppendLine("\n" + "Started process for GetMethodBlock(" + stmtId + ")");
                var workflowRef = GetMethodBlock(stmtId);

                var jclMenuTitle = jclMenuName != null ? jclMenuName.MenuTitle : jName;

                var bId = workflowRef[0].BaseCommandId;
                var statementId = workflowRef[0].StatementId;
                var treeNodeId = 1;
                // This is class name where that method is defined...
                lstTreeView.Add(new TreeView
                {
                    GraphId = "StartNode_1",
                    GraphName = "<span class='nodeToBold'>" + jclMenuTitle + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster[0].BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = jclMenuTitle,
                    ActualStatementId = "Actual_" + baseStatementMaster[0].StatementId,
                    NodeId = treeNodeId,
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
                    IndentLevel = 2
                }));

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);
                var auto = 0;
                int indentLevel = 3;
                var callingAndCalled = new Dictionary<string, List<string>>();

                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == 5);
                var fileMasters = fileMaster as IList<FileMaster> ?? fileMaster.ToList();
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMasters.ToArray();

                // ReSharper disable once RedundantAssignment
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>(
                        s => (s.MethodStatementId == statementId) && (s.ProjectId == projectId));


                foreach (var treeItem in lstTreeView)
                {
                    if ((treeItem.BaseCommandId != 5) && (treeItem.StatementReferenceMaster.OtherBaseCommandId != 5))
                        continue;

                    auto++;
                    // copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                    //    ref auto, ref treeNodeId);
                    stringBuilder.AppendLine("\n" + "Started process for called internal: GetCallInternalDetails(" + projectId + "," + treeItem.StatementReferenceMaster.FileId + ")");

                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, 0,
                        projectId,
                        treeItem.StatementReferenceMaster.FileId, indentLevel,
                        ref auto, ref treeNodeId, ref callingAndCalled);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                var newList = copyOfLstTreeView.ToList();

                foreach (var treeItem in newList)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                    // Find out which program is called. Here we will assign FileId as ProgramId to list so that no programs 
                    // will be scaned again, if its already processed.
                    // All call external within that program will considered under one programId

                    var oStatement = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                    var programName = oStatement.Split(' ')[2];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => (programName != null)
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9));

                    if (!anyFile) continue;
                    var programFilePath = copyOfFileMaster.ToList()
                        .Find(f => (programName != null)
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9));
                    int programId = programFilePath.FileId;

                    // Once we have program Id (FileId) then search for records in workflownodedetails, workflowlinkdetails
                    // workflowtreeviewsecondtabdetails, workflowtreeviewtabfirstdetails then mark those entries here with statement id,
                    // as this will help to put all statements in sequence for workflow.
                    Expression<Func<WorkflowTreeviewTabFirstDetails, bool>> expression
                        = master => master.ProgramId == programId;
                    var firstTabData = await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                        .GetAllItems(expression).ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        });
                    if (firstTabData.Any())
                    {
                        var workfloeReference = new WorkflowReferences
                        {
                            RowId = 0,
                            ProgramId = programId,
                            ActionWorkflowId = actionWorkflowId,
                            StartStatementId = actionWorkflow.First().MethodStatementId
                        };
                        await _codeVortoService.WorkflowReferencesRepository.AddNewItem(workfloeReference);
                        treeItem.ProgramId = programId;
                        continue;
                    }
                    auto++;
                    stringBuilder.AppendLine("\n" + "Started process for called external: GetCallExternalDetails(" + projectId + "," + treeItem.StatementReferenceMaster.FileId + ")");
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, programId,
                        ref auto, indentLevel, ref treeNodeId, ref callingAndCalled);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                #endregion

                #region Extra blocks...

                var indexPosition = -1;
                lstTreeView = copyOfLstTreeView.ToList();
                var ifCounter = 0;
                string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                var tempId = 0;
                var groupId = 1;
                foreach (
                    var treeItem in
                        copyOfLstTreeView.FindAll(t => (t.BaseCommandId == 6) || (t.BaseCommandId == 5)).ToList())
                {
                    //string color = "#2d5b7" + indexPosition;                    
                    treeItem.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeItem.GraphName +
                                         "</span>";
                    var childItems =
                        (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
                    foreach (var child in childItems)
                    {
                        if (tempId > 3)
                            tempId = 0;
                        if ((child.BaseCommandId == 6) || (child.BaseCommandId == 5))
                        {
                            var groupName = string.Empty;
                            if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                            {
                                var iOreCall = child.BaseCommandId == 6 ? "External Call" : "Internal Call";
                                var methodCalled = child.StatementReferenceMaster.MethodCalled.Substring(0,
                                    child.StatementReferenceMaster.MethodCalled.IndexOf('('));
                                var bName = child.StatementReferenceMaster.BusinessName;
                                if (string.IsNullOrEmpty(bName))
                                    bName = methodCalled;

                                groupName = "(" + bName.Trim() + ")(" +
                                            methodCalled + ")(" + iOreCall + ")";
                                groupId = groupId + 1;
                            }
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" +
                                                 treeItem.GraphName +
                                                 "</span>";
                            stringBuilder.AppendLine(
                                       "Started process for AssignColorsToChildNodes:(" +
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

                #endregion

                #region

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });

                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();
                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                    if (treeItem.BaseCommandId != 1) continue;

                    var treeViewList = new List<TreeView>();
                    for (var i = indexPosition; i < lstTreeView.Count; i++)
                    {
                        treeViewList.Add(lstTreeView[i]);
                        if (lstTreeView[i].BaseCommandId == 1)
                            ifCounter++;
                        if (lstTreeView[i].BaseCommandId == 2)
                            ifCounter--;
                        if (ifCounter == 0)
                            break;
                    }
                    // This is to assign child items of all IF statements
                    int curIndentLevel = treeViewList.First().IndentLevel;
                    var prevParentId = treeViewList.First().ParentId;
                    var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
                    treeViewList.First().GraphId = graphId;
                    //treeViewList.First().IndentLevel = curIndentLevel + 1;
                    //for (var j = 1; j < treeViewList.Count; j++)
                    //    if (treeViewList[j].ParentId == prevParentId)
                    //        treeViewList[j].ParentId = graphId;
                    //treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    treeViewList.First().IndentLevel = curIndentLevel + 1;
                    for (int j = 1; j < treeViewList.Count; j++)
                    {
                        if (treeViewList[j].ParentId != prevParentId) continue;
                        treeViewList[j].ParentId = graphId;

                        //if (treeViewList[j].BaseCommandId == 2) continue;
                        treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }

                indexPosition = -1;
                int loopCounter = 0;
                foreach (var treeItem in copyOfLstTreeView)
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

                        //if (treeViewList[j].BaseCommandId == 2) continue;
                        treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }

                indexPosition = -1;

                foreach (var treeItem in copyOfLstTreeView)
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
                        if (treeViewList[j].ParentId == prevParentId)
                            treeViewList[j].ParentId = graphId;
                        //  treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }

                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();
                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                secondTab.AddRange(copyOfLstTreeView
                    .Where(
                        item => (item.BaseCommandId == 6) || (item.BaseCommandId == 8) || (item.BaseCommandId == 10) ||
                                (item.BaseCommandId == 1) || (item.BaseCommandId == 25)
                                || (item.BaseCommandId == 5) || (item.BaseCommandId == 30)));
                var tempList =
                    (from d in secondTab
                     where (d.BaseCommandId == 1)
                           || (d.BaseCommandId == 10)
                           || (d.BaseCommandId == 25)
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
                    secondTab.AddRange(childItems);
                }

                secondTab = secondTab.Distinct().ToList();
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();

                #endregion

                #region

                var allSeqListItems = new List<TreeView>();
                foreach (var curItem in secondTab)
                {
                    allSeqListItems.Add(curItem);
                    var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                    stringBuilder.AppendLine("Started process for AttachChildItems:(" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                    }
                    break;
                }
                allSeqListItems = allSeqListItems.DistinctBy().ToList();

                #endregion

                #region

                var generalRepositoryNodeDetails =
                    new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;");
                var nodeId = 1;
                if (workflowMaxNode.Any())
                {
                    nodeId = workflowMaxNode[0].MaxNodeId;
                    nodeId = nodeId + 1;
                }

                var listNodes = new List<Node>();
                var listLinks = new List<Link>();
                var treeView = new TreeViewData(lstTreeView)
                {
                    Nodes = listNodes,
                    Links = listLinks
                };

                var strSplit = secondTab.First().SpriteCssClass;
                var strName = jclMenuName != null
                    ? jclMenuName.MenuTitle
                    : strSplit;
                var widthCnt = Convert.ToInt32(strName.Length.ToString()) * 3;
                if ((secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23) ||
                    (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 36))
                {
                    treeView.Nodes.Add(new Node
                    {
                        Id = nodeId,
                        Name = strName.ToUpper(),
                        ShapeId = "Circle",
                        Color = "#ffcc00",
                        Width = widthCnt.ToString(),
                        StatementId = int.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                        GroupName = secondTab.First().GroupName,
                        GroupId = secondTab.First().GroupId,
                        ProgramId = secondTab.First().ProgramId,
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
                        StatementId = int.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                        GroupName = secondTab.First().GroupName,
                        GroupId = secondTab.First().GroupId,
                        ProgramId = secondTab.First().ProgramId
                    });
                }

                allSeqListItems = allSeqListItems.Skip(1).ToList();
                // ReSharper disable once RedundantAssignment
                var firstItem = allSeqListItems.First();
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList()
                        .Distinct();

                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);

                stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetAllClassNameDeclared(" + solutionId + ",)");

                object[] parameters =
                {
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId}
                };
                var allClassNameDeclaredAndClassCalledList = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNameDeclared", parameters)
                    .ContinueWith(t => { var result = t.Result; return result.ToList(); }).ConfigureAwait(false);

                var linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    var nm = curItem.GraphName.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                    if (string.IsNullOrEmpty(nm)) continue;
                    if ((curItem.PrimaryCommandId == 1) || (curItem.BaseCommandId == 1))
                    {
                        #region PrimaryCommandId == 1 || BaseCommandId == 1

                        var ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                        var condition = ifPart.Contains("THEN")
                            ? ifPart.Substring(0,
                                ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                            : ifPart;

                        nodeId++;

                        var charCountOfText = condition.Length;
                        var width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                        var height = _clsUniverseBasic.CalculateHeight(charCountOfText);

                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "Decision2",
                            Name = condition,
                            Color = "#ff6600",
                            Width = width,
                            Height = height,
                            StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                            GroupName = curItem.GroupName,
                            GroupId = curItem.GroupId,
                            ProgramId = curItem.ProgramId
                        };
                        treeView.Nodes.Add(node);
                        treeView.Links.Add(new Link
                        {
                            Origin = nodeId - 1,
                            //Origin = treeView.Nodes.First().Id,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = curItem.ProgramId
                        });
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                        stringBuilder.AppendLine("Started process for ProcessChildItemsIf:(" +
                                                  projectId + ")");

                        foreach (var cItem in childItems)
                            treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                allClassNameDeclaredAndClassCalledList,
                                ref nodeId, ref linkSeqNumber);

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 6)
                    {
                        #region BaseCommandId == 6

                        var item = curItem;
                        if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                        {
                        }
                        else
                        {
                            var nodeColor = "#c0c0c0";
                            // ReSharper disable once RedundantAssignment
                            var ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                            // ReSharper disable once RedundantAssignment
                            var sss = item.StatementReferenceMaster.ClassCalled;
                            Expression<Func<StatementReferenceMaster, bool>> expression = master =>
                                master.BaseCommandId == 19 && master.ProjectId == projectId &&
                                (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);
                            var classNameDeclared =
                                allClassNameDeclaredAndClassCalledList.AsQueryable().Where(expression).ToList();
                            if (!classNameDeclared.Any())
                            {
                                Expression<Func<StatementReferenceMaster, bool>> expressionNew = master =>
                                    master.BaseCommandId == 19 &&
                                    (master.ProjectId == projectId || master.ProjectId == commanClassProjId) &&
                                    (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);
                                classNameDeclared = allClassNameDeclaredAndClassCalledList.AsQueryable().Where(expressionNew).ToList();
                                classNameDeclared = classNameDeclared.ToList();
                            }
                            if (classNameDeclared.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if ((curItem.PrimaryCommandId == 25) || (curItem.PrimaryCommandId == 38))
                            {
                                string[] strcCalled = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                                strName = "";
                                if (strcCalled.Length > 2)
                                    strName = strcCalled[strcCalled.Length - 2] + "." +
                                              strcCalled[strcCalled.Length - 1];
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = strName.ToUpper(),
                                    Color = nodeColor,
                                    StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId,
                                    ProgramId = curItem.ProgramId
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
                                    StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId,
                                    ProgramId = curItem.ProgramId
                                };
                            }
                            treeView.Nodes.Add(node);
                            var m = curItem.StatementReferenceMaster.MethodCalled;
                            if (m != null)
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId =
                                        int.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1]),
                                    ProgramId = curItem.ProgramId
                                });
                            else
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] ",
                                    ProgramId = curItem.ProgramId
                                });
                            linkSeqNumber++;
                            var childItems = (from s in secondTab
                                              where (s.ParentId == curItem.GraphId)
                                                    && (s.BaseCommandId != 25)
                                              select s).ToList().Distinct();
                            stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt:(" +
                                              projectId + ")");
                            foreach (var cItem in childItems)
                                treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndClassCalledList,
                                    ref nodeId, ref linkSeqNumber);
                        }

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 5)
                    {
                        #region BaseCommandId == 5

                        var item = curItem;
                        var nodeColor = "#c0c0c0";
                        var methodCalled = allClassNameDeclaredAndClassCalledList
                            .Where(s => s.FileId == curItem.StatementReferenceMaster.FileId
                                        && s.BaseCommandId == 19).ToList();

                        if (methodCalled.Count != 0)
                            nodeColor = "#00ffff";
                        nodeId++;
                        Node node;
                        if ((curItem.PrimaryCommandId == 24) || (curItem.PrimaryCommandId == 37))
                        {
                            string[] strcCalled = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                            strName = "";
                            if (strcCalled.Length > 2)
                                strName = strcCalled[strcCalled.Length - 2] + "." +
                                          strcCalled[strcCalled.Length - 1];
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = strName.ToUpper(),
                                Color = nodeColor,
                                StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId,
                                ProgramId = curItem.ProgramId
                            };
                        }
                        else
                        {
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = methodCalled[0].ClassNameDeclared,
                                Color = nodeColor,
                                StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId,
                                ProgramId = curItem.ProgramId
                            };
                        }
                        treeView.Nodes.Add(node);
                        var m = item.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                StatementId = item.StatementReferenceMaster.StatementId,
                                ProgramId = curItem.ProgramId
                            });
                        }
                        else
                        {
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();

                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal:(" +
                                             projectId + ")");
                        foreach (var cItem in childItems)
                            treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView,
                                allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 30)
                    {
                        #region BaseCommandId == 30

                        nodeId++;
                        var width = _clsUniverseBasic.CalculateWidth(curItem.GraphName.Length);
                        var height = _clsUniverseBasic.CalculateHeight(curItem.GraphName.Length);
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "Decision2",
                            Name = curItem.GraphName,
                            Color = "#ff6600",
                            Width = width,
                            Height = height,
                            StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                            GroupName = curItem.GroupName,
                            GroupId = curItem.GroupId,
                            ProgramId = curItem.ProgramId
                        };
                        treeView.Nodes.Add(node);
                        treeView.Links.Add(new Link
                        {
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = curItem.ProgramId
                        });
                        linkSeqNumber++;

                        #endregion
                    }
                }
                stringBuilder.AppendLine("Started process for RemoveMultipleLinks: (" +
                                                  projectId + ")");

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                stringBuilder.AppendLine("Started process for RemoveHangingNodes: (" +
                                               projectId + ")");
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                #region Add to database table...

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
                    Name = node.Name.Replace("</span>", "").Trim(),
                    //Name = node.Name,
                    Height = node.Height,
                    ShapeId = node.ShapeId,
                    Color = node.Color,
                    MaxNodeId = nodeId,
                    GroupName = node.GroupName,
                    GroupId = node.GroupId,
                    ProgramId = node.ProgramId
                }).ToList();
                generalRepositoryNodeDetails =
                    new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails);

                #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                string mySqlQueryNodeDetails = " Select * from WorkflowNodeDetails " +
                                               " Where ProjectId = " + projectId + " AND WorkflowStartStatementId = " +
                                               statementId +
                                               " ";
                var statementNodes =
                    await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(mySqlQueryNodeDetails);
                //var statementNodes =
                //    await generalRepositoryNodeDetails.GetAllItems(
                //            p => (p.ProjectId == projectId) && (p.WorkflowStartStatementId == statementId));

                foreach (var stmt in statementNodes)
                {
                    if ((stmt.ShapeId != "Decision") && (stmt.ShapeId != "Decision2")) continue;
                    var word =
                        new Regex(@"\<span ([\w\(\)\\_\,\<\#\;\:\&\$\-\.\'\\+\=EQ\sEQUAL\sAND\sNOT\sOR\s\/]*)\>");
                    var removeHtmlTag = "";
                    foreach (Match match in word.Matches(stmt.Name))
                    {
                        removeHtmlTag = match.Value;
                        stmt.Name = stmt.Name.Replace(removeHtmlTag, "");
                    }

                    if (removeHtmlTag != "")
                        stmt.Name =
                            stmt.Name.Replace("</span>", "")
                                .Replace(" LT", " is less than")
                                .Replace(" GT", " is greater than")
                                .Replace("GE", " is greater than or equal to")
                                .Replace(" #", " not equal to") + "?";
                    else
                        stmt.Name =
                            stmt.Name.Replace(" LT", " is less than")
                                .Replace(" GT", " is greater than")
                                .Replace("GE", " is greater than or equal to")
                                .Replace(" #", " not equal to") + "?";

                    await generalRepositoryNodeDetails.UpdateItem(stmt);
                }

                #endregion

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
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    ProgramId = link.ProgramId
                }).ToList();

                var generalRepositoryLinkDetails = new GeneralRepository<WorkflowLinkDetails>(new AppDbContext());
                await generalRepositoryLinkDetails.BulkInsert(lstWorkflowLinkDetails);

                //secondTab = secondTab.Distinct().ToList();
                var lstWorkflowTreeviewSecondTabDetails = secondTab.Select(sTab => new WorkflowTreeviewSecondTabDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel,
                    ProgramId = sTab.ProgramId
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                string mySqlQuery = " Select * from WorkflowTreeviewSecondTabDetails " +
                                    " Where ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId +
                                    " ";
                var statementSecondTab =
                    await generalRepositoryWorkflowTreeviewSecondTabDetails
                        .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(mySqlQuery);

                foreach (var stmt in statementSecondTab)
                    if ((stmt.BaseCommandId == 1) || (stmt.BaseCommandId == 5) || (stmt.BaseCommandId == 6))
                    {
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                    }

                #endregion

                var lsTreeviewTabFirstDetails = copyOfLstTreeView.Select(fTab => new WorkflowTreeviewTabFirstDetails
                {
                    BaseCommandId = fTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild.ToString(),
                    MethodCalled = fTab.MethodCalled,
                    ParentId = fTab.ParentId,
                    PrimaryCommandId = fTab.PrimaryCommandId,
                    SpriteCssClass = fTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    IndentLevel = fTab.IndentLevel,
                    ProgramId = fTab.ProgramId
                }).ToList();

                var generalRepositoryTreeviewTabFirstDetails =
                    new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());
                await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);

                #endregion

                #region Decision Chart Code

                //IHttpActionResult decisionChartResult =
                //        await GetDecisionChart(projectId, stmtId);
                //await decisionChartResult.ExecuteAsync(CancellationToken.None);

                #endregion
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Workflow data collected successfully");
            }
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList,
            ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            int commanClassProjId = 9;
            var width = "0";
            var height = "0";

            if ((treeView.BaseCommandId == 1) || (treeView.PrimaryCommandId == 1))
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                var ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart != null && ifPart.Contains("THEN"))
                    condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                else
                    condition = ifPart;

                nodeId++;
                if (condition != null)
                {
                    var charCountOfText = condition.Length;
                    width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                    height = _clsUniverseBasic.CalculateHeight(charCountOfText);
                }
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsIf: (" +
                                                           projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId,
                        ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                }
                else
                {
                    var nodeColor = "#c0c0c0";
                    var ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    var sss = item.StatementReferenceMaster.ClassCalled;
                    Expression<Func<StatementReferenceMaster, bool>> expression = master =>
                        master.BaseCommandId == 19 && master.ProjectId == projectId &&
                        (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);
                    var classNameDeclared = allClassNameDeclaredAndCalssCalledList.AsQueryable()
                        .Where(expression).ToList();

                    if (!classNameDeclared.Any())
                    {
                        Expression<Func<StatementReferenceMaster, bool>> expressionNew = master =>
                            master.BaseCommandId == 19 &&
                            (master.ProjectId == projectId || master.ProjectId == commanClassProjId) &&
                            (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);

                        classNameDeclared = allClassNameDeclaredAndCalssCalledList.AsQueryable()
                            .Where(expressionNew).ToList();
                    }
                    if (classNameDeclared.Count != 0)
                        nodeColor = "#00ffff";
                    nodeId++;

                    Node node;
                    if ((treeView.PrimaryCommandId == 25) || (treeView.PrimaryCommandId == 38))
                    {
                        var strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        var strName = "";
                        if (strSplit.Length > 2)
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    else
                    {
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = treeView.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    var m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                            StatementId = treeView.StatementReferenceMaster.StatementId,
                            ProgramId = treeView.ProgramId
                        });
                    else
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = treeView.ProgramId
                        });
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: (" +
                                                         projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                var nodeColor = "#c0c0c0";
                var methodCalled =
                    allClassNameDeclaredAndCalssCalledList.Where(
                        x => x.FileId == treeView.StatementReferenceMaster.FileId && x.BaseCommandId == 19).ToList();

                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if ((treeView.PrimaryCommandId == 24) || (treeView.PrimaryCommandId == 37))
                {
                    var strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    var strName = "";
                    if (strSplit.Length > 2)
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal: (" +
                                                         projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList,
                        ref nodeId, ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                if (parentIf.BaseCommandId != 1) return treeViewData;

                nodeId++;
                var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT " + condition + " THEN ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsElse: (" +
                                                            projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            LogMessage.WriteLogMessage(stringBuilder);
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList,
            ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            int commanClassProjId = 9;
            var width = "0";
            var height = "0";

            if ((treeView.BaseCommandId == 1) || (treeView.PrimaryCommandId == 1))
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                var ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart != null && ifPart.Contains("THEN"))
                    condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                else
                    condition = ifPart;

                nodeId++;

                if (condition != null)
                {
                    var charCountOfText = condition.Length;
                    width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                    height = _clsUniverseBasic.CalculateHeight(charCountOfText);
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsIf: (" + projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId, ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                }
                else
                {
                    var nodeColor = "#c0c0c0";
                    var ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    var sss = item.StatementReferenceMaster.ClassCalled;
                    Expression<Func<StatementReferenceMaster, bool>> expression = master =>
                        master.BaseCommandId == 19 && master.ProjectId == projectId &&
                        (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);

                    var classNameDeclared =
                        allClassNameDeclaredAndCalssCalledList.AsQueryable().Where(expression).ToList();
                    if (!classNameDeclared.Any())
                    {
                        Expression<Func<StatementReferenceMaster, bool>> expressionNew = master =>
                            master.BaseCommandId == 19 &&
                            (master.ProjectId == projectId || master.ProjectId == commanClassProjId) &&
                            (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);
                        classNameDeclared = allClassNameDeclaredAndCalssCalledList.AsQueryable().Where(expressionNew).ToList();
                    }

                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#00ffff";
                    nodeId++;

                    Node node;
                    if ((treeView.PrimaryCommandId == 25) || (treeView.PrimaryCommandId == 38))
                    {
                        var strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        var strName = "";
                        if (strSplit.Length > 2)
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    else
                    {
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = treeView.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    var m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                    {
                        m = m + "()";
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                            StatementId = treeView.StatementReferenceMaster.StatementId,
                            ProgramId = treeView.ProgramId
                        });
                    }
                    else
                    {
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = treeView.ProgramId
                        });
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: (" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId,
                            ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                var nodeColor = "#c0c0c0";
                var methodCalled =
                    allClassNameDeclaredAndCalssCalledList.Where(
                        x => x.BaseCommandId == 19 && x.FileId == treeView.StatementReferenceMaster.FileId).ToList();
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if ((treeView.PrimaryCommandId == 24) || (treeView.PrimaryCommandId == 37))
                {
                    var strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    var strName = "";
                    if (strSplit.Length > 2)
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal: (" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                if (parentIf.BaseCommandId != 1) return treeViewData;

                nodeId++;
                var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT " + condition + " THEN ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsElse: (" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            LogMessage.WriteLogMessage(stringBuilder);
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList,
            ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            var width = "0";
            var height = "0";

            if ((treeView.BaseCommandId == 1) || (treeView.PrimaryCommandId == 1))
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                var ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart != null && ifPart.Contains("THEN"))
                    condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                else
                    condition = ifPart;

                nodeId++;

                if (condition != null)
                {
                    var charCountOfText = condition.Length;
                    width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                    height = _clsUniverseBasic.CalculateHeight(charCountOfText);
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    //Origin = lastNode.Id,
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsIf: (" + projectId + ")");

                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId,
                        ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                }
                else
                {
                    var nodeColor = "#c0c0c0";
                    string ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    var classNameDeclared =
                        allClassNameDeclaredAndCalssCalledList.Where(s =>
                                s.ClassNameDeclared == ss || s.ClassNameDeclared == item.StatementReferenceMaster.ClassCalled).ToList();

                    if (classNameDeclared.Count != 0)
                        nodeColor = "#00ffff";
                    nodeId++;
                    Node node;
                    if ((treeView.PrimaryCommandId == 25) || (treeView.PrimaryCommandId == 38))
                    {
                        var strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        var strName = "";
                        if (strSplit.Length > 2)
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    else
                    {
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = treeView.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    var m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                            StatementId = treeView.StatementReferenceMaster.StatementId,
                            ProgramId = treeView.ProgramId
                        });
                    else
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = treeView.ProgramId
                        });
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: (" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId,
                            ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                var nodeColor = "#c0c0c0";
                var methodCalled =
                    allClassNameDeclaredAndCalssCalledList.Where(
                        x => x.BaseCommandId == 19 && x.FileId == treeView.StatementReferenceMaster.FileId).ToList();

                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;

                Node node;
                if ((treeView.PrimaryCommandId == 24) || (treeView.PrimaryCommandId == 37))
                {
                    var strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    var strName = "";
                    if (strSplit.Length > 2)
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }

                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();

                stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal: (" + projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList,
                        ref nodeId, ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 8)
            {
                #region BaseCommandId == 8

                var nodeColor = "#c0c0c0";
                var methodCalled = allClassNameDeclaredAndCalssCalledList.Where(s => s.BaseCommandId == 19 &&
                                                                                     s.FileId ==
                                                                                     treeView.StatementReferenceMaster
                                                                                         .FileId).ToList();

                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;

                Node node;
                if ((treeView.PrimaryCommandId == 23) || (treeView.PrimaryCommandId == 36))
                {
                    var strSplit = methodCalled[0].ClassNameDeclared.Split('.');
                    var strName = "";
                    if (strSplit.Length > 2)
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m;
                if ((treeView.PrimaryCommandId == 23) || (treeView.PrimaryCommandId == 36))
                    m = treeView.StatementReferenceMaster.MethodName;
                else
                    m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems =
                    (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                        .ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: (" + projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId,
                        ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                if (parentIf.BaseCommandId != 1) return treeViewData;

                nodeId++;
                var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT " + condition + " THEN ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsElse: (" + projectId + ")");

                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            LogMessage.WriteLogMessage(stringBuilder);
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsElse(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList,
            ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            var width = "0";
            var height = "0";

            if ((treeView.BaseCommandId == 1) || (treeView.PrimaryCommandId == 1))
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                var ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart != null && ifPart.Contains("THEN"))
                    condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                else
                    condition = ifPart;

                nodeId++;

                if (condition != null)
                {
                    var charCountOfText = condition.Length;
                    width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                    height = _clsUniverseBasic.CalculateHeight(charCountOfText);
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for ProcessChildItemsIf: (" + projectId + ")");
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId,
                        ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                }
                else
                {
                    var nodeColor = "#c0c0c0";
                    string ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    var classNameDeclared =
                        allClassNameDeclaredAndCalssCalledList.Where(
                            s => s.ClassNameDeclared == ss ||
                                s.ClassNameDeclared == item.StatementReferenceMaster.ClassCalled).ToList();

                    if (classNameDeclared.Count != 0)
                        nodeColor = "#00ffff";
                    nodeId++;

                    Node node;
                    if ((treeView.PrimaryCommandId == 25) || (treeView.PrimaryCommandId == 38))
                    {
                        var strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        var strName = "";
                        if (strSplit.Length > 2)
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    else
                    {
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = treeView.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId,
                            ProgramId = treeView.ProgramId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    var m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                    {
                        m = m + "()";
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                            StatementId = treeView.StatementReferenceMaster.StatementId,
                            ProgramId = treeView.ProgramId
                        });
                    }
                    else
                    {
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = treeView.ProgramId
                        });
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: (" + projectId + ")");

                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId,
                            ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                var nodeColor = "#c0c0c0";
                var methodCalled =
                    allClassNameDeclaredAndCalssCalledList.Where(
                        x => x.BaseCommandId == 19 && x.FileId == treeView.StatementReferenceMaster.FileId).ToList();

                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if ((treeView.PrimaryCommandId == 24) || (treeView.PrimaryCommandId == 37))
                {
                    var strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    var strName = "";
                    if (strSplit.Length > 2)
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId,
                        ProgramId = treeView.ProgramId
                    };
                }
                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId,
                        ProgramId = treeView.ProgramId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] ",
                        ProgramId = treeView.ProgramId
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal: (" + projectId + ")");

                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                if (parentIf.BaseCommandId != 1) return treeViewData;

                nodeId++;
                var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT " + condition + " THEN ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId,
                    ProgramId = treeView.ProgramId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] ",
                    ProgramId = treeView.ProgramId
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine("Started process for ProcessChildItemsElse: (" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            LogMessage.WriteLogMessage(stringBuilder);
            return treeViewData;
        }

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView, string color,
            string groupName, int groupId)
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

        private List<Link> RemoveMultipleLinks(List<Link> lstLinks, List<Node> lstNodes)
        {
            var copyOfLinks = lstLinks;
            var linksToRemove = new List<Link>();
            foreach (var node in lstNodes)
            {
                var childNodes = (from lnk in lstLinks
                                  where lnk.Origin == node.Id
                                  select lnk.Target
                                      into p
                                      select lstNodes.Find(n => n.Id == p)).ToList();
                if (childNodes.Count > 1)
                {
                    //List<Node> duplicates = childNodes.GroupBy(x => x)
                    //    .Where(g => g.Count() > 1).Select(g => g.Key).ToList();
                    var dupes =
                        childNodes.Where(a => childNodes.Except(new List<Node> { a }).Any(x => x.Name == a.Name)).ToList();
                    //List<Node> duplicates = childNodes.GetDuplicates().ToList();
                    var hasChilds = false;
                    foreach (var dup in dupes)
                    {
                        var klm = (from f in lstLinks where f.Origin == dup.Id select f).ToList();
                        if (klm.Count > 0)
                            hasChilds = true;
                    }
                    if (!hasChilds)
                    {
                        var tempLinks = new List<Link>();
                        if ((dupes.Count > 0) && (dupes[0] != null))
                        {
                            foreach (var n in dupes)
                            {
                                var link = (from h in lstLinks
                                            where (h.Origin == node.Id)
                                                  && (h.Target == n.Id)
                                            select h).ToList();
                                tempLinks.AddRange(link);
                            }
                            // var links =(from h in lstLinks where h.Origin == node.Id select h).ToList();
                            if (!tempLinks.Any()) continue;

                            var linkText = tempLinks.Aggregate(string.Empty,
                                (current, jk) => current + jk.LinkText + ", ");
                            linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                            tempLinks.First().LinkText = linkText;

                            foreach (var l in tempLinks)
                            {
                                if (l.LinkText == linkText) continue;
                                linksToRemove.Add(l);
                                //copyOfLinks.Remove(l);
                            }
                        }
                    }
                }
            }
            foreach (var l in linksToRemove)
                copyOfLinks.Remove(l);
            return copyOfLinks;
        }

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab,
            TreeView curItem)
        {
            allSeqListItems.Add(curItem);
            var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
            foreach (var cItem in childItems)
                allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
            return allSeqListItems;
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int programId, ref int auto, int indentLevel, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                if (treeView.ClassCalled == null) return lstTreeView;

                var className = treeView.ClassCalled;

                stringBuilder.AppendLine("\n" + "Started process for GetGenericBlock:(" + className + ")");
                var callExtExpandedCode = GetGenericBlock(className);

                if (callExtExpandedCode.Count == 0)
                {
                    auto++;
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item == null)
                        lstTreeView.Add(new TreeView
                        {
                            ParentId = statememtId,
                            GraphId = "111" + auto + "_" + statememtId,
                            HasChild = true,
                            GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                            BaseCommandId = 25,
                            StatementReferenceMaster = treeView.StatementReferenceMaster,
                            NodeId = ++treeNodeId,
                            ActualStatementId = "Missing_99999999",
                            IndentLevel = indentLevel,
                            ProgramId = programId
                        });
                }
                else
                {
                    foreach (var statementMaster in callExtExpandedCode)
                    {
                        if (!string.IsNullOrEmpty(treeView.MethodCalled)
                            && !string.IsNullOrEmpty(statementMaster.MethodName)
                            && (treeView.MethodCalled != statementMaster.MethodName)) continue;

                        //if(string.IsNullOrEmpty(treeView.MethodCalled))
                        //    continue;
                        //if (string.IsNullOrEmpty(statementMaster.MethodName))
                        //    continue;
                        //if (treeView.MethodCalled != statementMaster.MethodName) 
                        //    continue;

                        //if (!string.IsNullOrEmpty(treeView.MethodCalled) &&
                        //    !string.IsNullOrEmpty(statementMaster.MethodName)
                        //    && treeView.MethodCalled == statementMaster.MethodName)

                        var blockStmtId = statementMaster.StatementId;
                        stringBuilder.AppendLine("\n" + "Started process for GetMethodBlock: (" + blockStmtId + ")");
                        var stmtsBlock = GetMethodBlock(blockStmtId);
                        var chkparentIdMethod = 0;

                        var calledGoSubs =
                            (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        callingAndCalled.Add(treeView.MethodCalled + "_" + auto, calledGoSubs);

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
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                //lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref auto, ref treeNodeId, ref callingAndCalled);
                            }
                            else if ((block.BaseCommandId == 5) || (block.OtherBaseCommandId == 5))
                            {
                                auto++;
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + auto + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                indentLevel = indentLevel + 1;
                                stringBuilder.AppendLine("\n" + "Started process for called internal: GetCallInternalDetails(" + block.ProjectId + "," + block.FileId + ")");
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, programId, block.ProjectId, block.FileId, indentLevel, ref auto,
                                    ref treeNodeId,
                                    ref callingAndCalled);
                                indentLevel = indentLevel - 1;
                            }

                            else if ((block.BaseCommandId == 8) || (block.OtherBaseCommandId == 8))
                            {
                                chkparentIdMethod = 1;
                                auto++;
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + auto + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });

                                statememtId = "Node" + auto + "_" + block.StatementId;
                                //lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref auto, ref treeNodeId);
                            }
                            else
                            {
                                if (chkparentIdMethod == 1)
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
                                        IndentLevel = indentLevel,
                                        ProgramId = programId
                                    });
                                else
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
                                        IndentLevel = indentLevel,
                                        ProgramId = programId
                                    });
                            }
                        }
                        break;
                    }
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int programId,
            int projectId, int fileId, int indentLevel, ref int autoInt, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            var stringBuilder = new StringBuilder();
            // int commanClassProjId = 9;
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                var methodName = treeView.MethodCalled;

                stringBuilder.AppendLine("\n" + "Called stored procedure: SpFindStatementForMethodName(8," + projectId +
                                         "," + methodName + "," + fileId + ")");
                object[] parameters =
                {
                    new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                };
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters).Result;
                /*
                if (stmtMaster.Count == 0)
                {
                    object[] parameters1 =
                    {
                        new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = commanClassProjId},
                        new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                        new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                    };

                    stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters1)
                        .Result;
                }
                */

                if (stmtMaster.Count == 0)
                {
                    autoInt++;
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item != null) return lstTreeView;

                    #region For the only Universe Basic launguage

                    var graphName = GetIncludeStatement(projectId, fileId, methodName);
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "111" + autoInt + "_" + statememtId,
                        HasChild = true,
                        GraphName = graphName,
                        BaseCommandId = 25,
                        StatementReferenceMaster = treeView.StatementReferenceMaster,
                        NodeId = ++treeNodeId,
                        ActualStatementId = "Missing_99999999",
                        ProgramId = programId
                    });

                    #endregion
                }
                else
                {
                    var callExtExpandedCode = GetGenericBlock(stmtMaster[0].StatementId, 8, 9);
                    if (callExtExpandedCode.Count == 0)
                    {
                        autoInt++;
                        var item = lstTreeView.Find(p => p.ParentId == statememtId);
                        if (item == null)
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + autoInt + "_" + statememtId,
                                HasChild = true,
                                GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing_99999999",
                                IndentLevel = indentLevel,
                                ProgramId = programId
                            });
                    }
                    else
                    {
                        var calledGoSubs =
                            (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        if (callingAndCalled.ContainsKey(methodName))
                        {
                            foreach (var keyVal in callingAndCalled)
                            {
                                if (keyVal.Key != methodName) continue;
                                keyVal.Value.AddRange(calledGoSubs);
                                var allVals = keyVal.Value.Distinct().ToList();
                                keyVal.Value.Clear();
                                keyVal.Value.AddRange(allVals);
                                break;
                            }
                        }
                        else
                            callingAndCalled.Add(methodName, calledGoSubs);

                        var result = false;

                        foreach (var block in callExtExpandedCode)
                        {
                            autoInt++;
                            if ((block.BaseCommandId == 5) || (block.OtherBaseCommandId == 5))
                            {
                                foreach (var keyValPair in callingAndCalled.Where(s => s.Key == methodName))
                                {
                                    var goSubs = keyValPair.Value;
                                    var block1 = block;
                                    foreach (var gosub in goSubs.Where(s => s.StartsWith(block1.MethodCalled)))
                                    {
                                        if (!callingAndCalled.Keys.Any(s => s.StartsWith(gosub))) continue;
                                        var gosub1 = gosub;
                                        var newList = (from d in callingAndCalled
                                                       where d.Key.StartsWith(gosub1)
                                                       select d.Value).ToList();
                                        result = newList.Any(s => s.Any(m => m.StartsWith(methodName)));
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
                                        IndentLevel = indentLevel,
                                        ProgramId = programId
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
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                indentLevel = indentLevel + 1;
                                stringBuilder.AppendLine("\n" + "Started process for called internal: GetCallInternalDetails(" + block.ProjectId + "," + block.FileId + ")");
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, programId, block.ProjectId, block.FileId, indentLevel, ref autoInt,
                                    ref treeNodeId,
                                    ref callingAndCalled);
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
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                //lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref autoInt, ref treeNodeId, ref callingAndCalled);
                            }
                            else
                            {
                                if (stmtMaster[0].MethodName.Trim() != block.MethodName)
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
                                        IndentLevel = indentLevel,
                                        ProgramId = programId
                                    });
                            }
                        }
                    }
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return lstTreeView;
            }
        }

        private List<StatementReferenceMaster> GetMethodBlock(int stmtId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetAnyGenericBlock(" + stmtId + ",8,9)");
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return workflowRef;
            }
        }

        private List<StatementReferenceMaster> GetGenericBlock(string className)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetClassMethods(" + className + ")");
            object[] parametersExp =
            {
                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
            };
            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
                .ContinueWith(t => t.Result).Result;
            LogMessage.WriteLogMessage(stringBuilder);
            return callExtExpandedCode;
        }

        private List<StatementReferenceMaster> GetGenericBlock(int stmtId, int startBaseCommandId, int endBaseCommandId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetAnyGenericBlock(" + stmtId + "," + startBaseCommandId + "," + endBaseCommandId + ")");
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = startBaseCommandId},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = endBaseCommandId}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return workflowRef;
            }
        }

        [HttpGet]
        public async void Test()
        {
            string query =
                "Select * from StatementReferenceMaster Where OriginalStatement like '%IF FOUND THEN%' OR OriginalStatement like '%IF FOUND%' OR OriginalStatement like '%IF NOT SUCCESS THEN%' OR OriginalStatement like '%IF NOT-SUCCESS THEN%' OR OriginalStatement like '%IF SUCCESS THEN%' OR OriginalStatement like '%IF NOT FOUND THEN%' OR OriginalStatement like '%IF NOT-FOUND THEN%' OR OriginalStatement like '%IF NOT FOUND%' OR OriginalStatement like '%IF NOT-FOUND%';";
            using (_codeVortoService = new CodeVortoService())
            {
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(query);
                foreach (var statementMaster in allStatements)
                {
                    var master1 = statementMaster;
                    Expression<Func<StatementReferenceMaster, bool>>
                        expression = referenceMaster => referenceMaster.StatementId == master1.StatementId - 1
                                                        && referenceMaster.BaseCommandId != 30 &&
                                                        referenceMaster.BaseCommandId != 1;
                    var master = await _codeVortoService.StatementReferenceMasterRepository
                        .GetItem<StatementReferenceMaster>(expression, master1.StatementId - 1);
                    if (master != null)
                    {
                        if (master.BaseCommandId != 0) return;

                        master.BaseCommandId = 30;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(master);
                    }
                }
            }
        }
    }
}