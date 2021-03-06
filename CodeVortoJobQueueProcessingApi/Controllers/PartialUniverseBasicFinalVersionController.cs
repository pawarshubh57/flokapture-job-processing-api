﻿using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
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
using CodeVortoJobQueueProcessingApi.ControllerHelperClasses;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicFinalVersionController
    {
        private int commanClassProjId = 9;
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();

        [HttpGet]
        public async Task<HttpResponseMessage> StartWorkflowProcess(int solutionId, int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allProjects = await _codeVortoService.ProjectMasterRepository
                    .GetAllListItems(p => p.SolutionId == solutionId).ConfigureAwait(false);

                foreach (var project in allProjects)
                {
                    if (project.ProjectId != projectId) continue;

                    var stringBuilder = new StringBuilder();
                    stringBuilder.AppendLine("\n" + "Started ProcessPseudoCodeConversion for Project: " + project.ProjectId + " project name: " +
                                    project.ProjectName + " and project physical path is: " + project.PhysicalPath);

                    await ProcessPseudoCodeConversion(project.ProjectId).ConfigureAwait(false);


                    await OtherWorkflowsFromMenuNames(projectId);

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    stringBuilder.AppendLine("\n" + "Started GetAllStartingPoints for Project: " + project.ProjectId + " project name: " +
                                     project.ProjectName + " and project physical path is: " + project.PhysicalPath);

                    Console.WriteLine("Started GetAllStartingPoints for Project: " + project.ProjectId + " project name: " +
                                     project.ProjectName + " and project physical path is: " + project.PhysicalPath);
                    stringBuilder.AppendLine(
                       "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    await GetAllStartingPoints(project.ProjectId).ConfigureAwait(false);

                    stringBuilder.AppendLine("\n" + "Completed GetAllStartingPoints for Project:" + project.ProjectId);
                    Console.WriteLine("Completed GetAllStartingPoints for Project:" + project.ProjectId);
                    stringBuilder.AppendLine(
                       "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    stringBuilder.AppendLine("\n" + "Started UpdateWorkflowNames for project: " + project.ProjectId);

                    Console.WriteLine("Started UpdateWorkflowNames for project: " + project.ProjectId);
                    stringBuilder.AppendLine(
                       "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");

                    await UpdateWorkflowNames(project.ProjectId).ConfigureAwait(false);

                    stringBuilder.AppendLine("\n" + "Completed UpdateWorkflowNames for project:" + project.ProjectId);

                    Console.WriteLine("Completed UpdateWorkflowNames for project:" + project.ProjectId);
                    stringBuilder.AppendLine(
                       "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");

                    stringBuilder.AppendLine("\n" + "Started ProcessObjectConnectivityDiagram for Project: "
                        + project.ProjectId + " project name: " + project.ProjectName + " and project physical path is: " + project.PhysicalPath);

                    Console.WriteLine("Started ProcessObjectConnectivityDiagram for Project: " + project.ProjectId + " project name: " +
                                     project.ProjectName + " and project physical path is: " + project.PhysicalPath);
                    stringBuilder.AppendLine(
                     "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    await ProcessObjectConnectivityDiagram(project.ProjectId).ConfigureAwait(false);
                    stringBuilder.AppendLine("\n" + "Completed ProcessObjectConnectivityDiagram for Project:" + project.ProjectId);

                    Console.WriteLine("Completed ProcessObjectConnectivityDiagram for Project:" + project.ProjectId);
                    stringBuilder.AppendLine(
                      "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    LogMessage.WriteLogMessage(stringBuilder);
                }
            }

            var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
            return responseMessage;
        }

        [HttpGet]
        public async Task<HttpResponseMessage> UpdateSecondTabAlternateNames(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("baseCmdId", MySqlDbType.Int32){Value = 5},
                    new MySqlParameter("notCmdId", MySqlDbType.Int32){Value = 1},
                    new MySqlParameter("prjId", MySqlDbType.Int32){Value = projectId}
                };

                await _codeVortoService.SecondTabProgramDetailsRepository.ExecuteStoreProcedure("SpUpdateSecondTabData", parameters);

                var responseMessage = new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new StringContent("Alternate names updated successfully. Project processed successfully. Project Id: " + projectId)
                };
                return responseMessage;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine(
           "========================================================================================");
                stringBuilder.AppendLine("\n" + "Started to collect all actionworkflow for project: " + projectId);

                string selectSql = " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                                   " and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') " +
                                   " AND CreatedBy = 1 AND Processed = 0; ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(selectSql).ConfigureAwait(false);

                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    try
                    {
                        if (workflow.ActionWorkflowId <= 1372) continue;
                        if (workflow.ProjectId == null) return NotFound();
                        var workFlowProjectId = workflow.ProjectId.Value;
                        stringBuilder.AppendLine(
         "========================================================================================");
                        stringBuilder.AppendLine("\n" + "Started executing next process: GetWorkFlowWorkSpaceParallel for projectId:" +
                                                 workFlowProjectId + ", and MethodStatementId is:" + workflow.MethodStatementId + ")");

                        var actionResult =
                            await GetWorkFlowWorkSpaceParallel(workFlowProjectId, workflow.MethodStatementId)
                                .ConfigureAwait(false);
                        var dataContent = await actionResult.ExecuteAsync(CancellationToken.None).ConfigureAwait(false);
                        var responseMessage =
                            await dataContent.Content.ReadAsStringAsync().ConfigureAwait(false);

                        if (responseMessage == "\"Too Many Nodes\"")
                        {
                            Console.WriteLine("Too Many Nodes for ActionWorkflowId: " + workflow.ActionWorkflowId);
                            Console.WriteLine("Origin file path: " + workflow.OriginFilePath);
                            Console.WriteLine("=======================================================");
                            continue;
                        }

                        workflow.Processed = 1;
                        workflow.ProjectMaster = null;
                        workflow.FileMaster = null;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow).ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }
                stringBuilder.AppendLine(
       "========================================================================================");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceParallel(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine(
                        "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                stringBuilder.AppendLine("\n" + " Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);

                Console.WriteLine("Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);

                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                var solutionId = projectDetails.SolutionId;
                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                        .GetAllItems(p => (p.MethodStatementId != 0)
                                          && (p.ProjectId == projectId) && (p.Processed == 0)).ConfigureAwait(false);
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.FileId).ToList().First();
                if (clsName == 0) return Ok("Action Workflow processed successfully.");

                stringBuilder.AppendLine(
         "========================================================================================");
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpBaseStatementMaster : " + projectId + "," + clsName);
                /*
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = clsName},
                    new MySqlParameter("@classSplitName", MySqlDbType.VarChar)
                    {
                        Value = clsName.Split('.').LastOrDefault()
                    }
                };
                */
                string sqlQuery = "select * from statementreferencemaster Where FileId = " + clsName + " AND BaseCommandId = 19;";
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery).ConfigureAwait(false);

                string jName = baseStatementMaster[0].ClassNameDeclared;
                jName = jName.Split('.').LastOrDefault();
                var appDbContext = _codeVortoService.AppDbContextRepository;
                var jclMenuName = await appDbContext.UniverseFileMenu.AsQueryable()
                    .Where(menu => menu.ActionExecuted == jName)
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.FirstOrDefault();
                    }).ConfigureAwait(false);
                stringBuilder.AppendLine(
         "========================================================================================");
                stringBuilder.AppendLine("\n" + "Started process for get block: GetMethodBlock for statememtId: " + stmtId + ")");
                var workflowRef = GetMethodBlock(stmtId);

                if (!workflowRef.Any()) return Ok("Workflow data collected successfully");

                int currentFileId = workflowRef[0].FileId;
                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == currentFileId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any())
                    return Ok("Workflow data collected successfully");

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
                    AlternateName = statementMaster.AlternateName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2
                }));

                #endregion

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                #region Process for base command id = 5 and 6

                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == solutionId).ConfigureAwait(false);
                var fileMasters = fileMaster as IList<FileMaster> ?? fileMaster.ToList();
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMasters.ToArray();

                /*
                var allExecuteWaleStatements = (from c in copyOfLstTreeView
                    where c.StatementReferenceMaster.OriginalStatement
                        .StartsWith("EXECUTE ")
                    select c).ToList();
                if (!allExecuteWaleStatements.Any())
                    return Ok();
                */
                var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s", RegexOptions.IgnoreCase);
                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (regEx.IsMatch(treeItem.StatementReferenceMaster.OriginalStatement))
                    {
                        var callJclStmt = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                        if (callJclStmt.Split(' ').Length <= 1) continue;
                        var executeJcl = callJclStmt.Split(' ')[1];
                        var anyJclFile = copyOfFileMaster.ToList()
                            .Any(f => (executeJcl != null)
                                      && f.FileName.StartsWith(executeJcl)
                                      && (f.FileTypeExtensionId == 10) && f.SolutionId == solutionId);

                        if (!anyJclFile) continue;
                        var thisJclFileDetails = copyOfFileMaster.ToList()
                            .Find(f => (executeJcl != null)
                                       && f.FileName.StartsWith(executeJcl)
                                       && (f.FileTypeExtensionId == 10) && f.SolutionId == solutionId);
                        int executeJclFileId = thisJclFileDetails.FileId;
                        treeItem.ProgramId = executeJclFileId;
                        treeItem.BaseCommandId = 5;
                        continue;
                    }

                    if ((treeItem.BaseCommandId != 5) && (treeItem.StatementReferenceMaster.OtherBaseCommandId != 5))
                        continue;

                    var oStatement = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                    if (oStatement.Split(' ').Length <= 1) continue;
                    var executeJclName = oStatement.Split(' ')[1];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => (executeJclName != null)
                                  && f.FileName.StartsWith(executeJclName)
                                  && (f.FileTypeExtensionId == 10) && f.SolutionId == solutionId);

                    if (!anyFile) continue;
                    var jclFileDetails = copyOfFileMaster.ToList()
                        .Find(f => (executeJclName != null)
                                   && f.FileName.StartsWith(executeJclName)
                                   && (f.FileTypeExtensionId == 10) && f.SolutionId == solutionId);
                    int jclFileId = jclFileDetails.FileId;
                    treeItem.ProgramId = jclFileId;
                    treeItem.BaseCommandId = 5;
                }

                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 6) continue;

                    var oStatement = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                    if (oStatement.Split(' ').Length <= 2) continue;
                    var programName = oStatement.Split(' ')[2];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => (programName != null)
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.SolutionId == solutionId);

                    if (!anyFile) continue;
                    var programFilePath = copyOfFileMaster.ToList()
                        .Find(f => (programName != null)
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.SolutionId == solutionId);
                    int programId = programFilePath.FileId;
                    treeItem.MethodCalled = programName + "()";
                    treeItem.ProgramId = programId;
                }

                #region

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                var ifCounter = 0;
                var indexPosition = -1;
                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.StatementReferenceMaster != null && treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
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

                // copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));
                secondTab.AddRange(copyOfLstTreeView
                    .Where(
                        item => (item.BaseCommandId == 6)
                            || (item.BaseCommandId == 8)
                            || (item.BaseCommandId == 10)
                            || (item.BaseCommandId == 1)
                            || (item.BaseCommandId == 25)
                            || (item.BaseCommandId == 5)
                            || (item.BaseCommandId == 30)
                            || (item.BaseCommandId == 3)
                            || (item.BaseCommandId == 4)
                            || (item.BaseCommandId == 45)));
                var tempList =
                    (from d in secondTab
                     where (d.BaseCommandId == 1)
                           || (d.BaseCommandId == 10)
                           || (d.BaseCommandId == 25) || (d.BaseCommandId == 3)
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
                    secondTab.AddRange(childItems);
                }

                secondTab = secondTab.Distinct().ToList();
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();

                #endregion

                var allSeqListItems = new List<TreeView>();
                foreach (var curItem in secondTab)
                {

                    stringBuilder.AppendLine("Started process for attchchilditems: AttachChildItems(" + projectId + ")");
                    allSeqListItems.Add(curItem);
                    var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                    foreach (var cItem in childItems)
                    {
                        allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                    }
                    break;
                }
                allSeqListItems = allSeqListItems.DistinctBy().ToList();

                #endregion

                #region Process the details

                // ReSharper disable once RedundantAssignment
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>(
                        s => (s.MethodStatementId == statementId) && (s.ProjectId == projectId)).ConfigureAwait(false);

                var secondTabDetails = allSeqListItems.Select(sTab => new WorkflowTreeviewSecondTabDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    AlternateName = sTab.AlternateName,
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel,
                    ProgramId = sTab.ProgramId,
                    AnnotateStatement = null
                }).ToList();

                var generalRepositorySecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                int actionWorkFlowId = actionWorkflow.First().ActionWorkflowId;
                int methodStartStatementId = actionWorkflow.First().MethodStatementId;
                // Before inserting these records, check whether this table has already data for this action workflow id.
                var generalRepositoryFirstDetails = new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());

                var chkHasResults = await generalRepositoryFirstDetails
                    .GetAllListItems(t =>
                        t.ActionWorkflowId == actionWorkFlowId &&
                        t.WorkflowStartStatementId == methodStartStatementId).ConfigureAwait(false);

                if (!chkHasResults.Any())
                    await generalRepositorySecondTabDetails.BulkInsert(secondTabDetails).ConfigureAwait(false);

                string mySqlQry = " Select * from WorkflowTreeviewSecondTabDetails Where " +
                                  " ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId + " ";

                if (!chkHasResults.Any())
                {
                    var statementSecTab = await generalRepositorySecondTabDetails
                        .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(mySqlQry).ConfigureAwait(false);
                    foreach (var stmt in statementSecTab)
                    {
                        if ((stmt.BaseCommandId != 1) && (stmt.BaseCommandId != 5) && (stmt.BaseCommandId != 6))
                            continue;
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='pseudoCodeDialog(" +
                                         stmt.ProgramId + ")'/>";

                        await generalRepositorySecondTabDetails.UpdateItem(stmt).ConfigureAwait(false);
                    }
                }

                var lstTabFirstDetails = copyOfLstTreeView.Select(fTab => new WorkflowTreeviewTabFirstDetails
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

                if (!chkHasResults.Any())
                    await generalRepositoryFirstDetails.BulkInsert(lstTabFirstDetails).ConfigureAwait(false);

                // Forth tabs data is about Nodes and Links which are created from second tabs data, so we will add default node 
                // from seconds tabs first element, which is a starting point of workflow.

                var generalRepositoryNodeDetails =
                   new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;").ConfigureAwait(false);
                var nodeId = 1;
                if (workflowMaxNode.Any())
                {
                    nodeId = workflowMaxNode[0].MaxNodeId;
                    nodeId = nodeId + 1;
                }
                var listNodes = new List<Node>();
                var treeView = new TreeViewData
                {
                    Nodes = listNodes
                };
                var strSplit = secondTab.First().SpriteCssClass;
                var strName = jclMenuName != null
                    ? jclMenuName.MenuTitle
                    : strSplit;
                var widthCnt = Convert.ToInt32(strName.Length.ToString()) * 3;

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
                    ProgramId = secondTab.First().ProgramId
                });

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
                    Height = node.Height,
                    ShapeId = node.ShapeId,
                    Color = node.Color,
                    MaxNodeId = nodeId,
                    GroupName = node.GroupName,
                    GroupId = node.GroupId,
                    ProgramId = node.ProgramId
                }).ToList();

                generalRepositoryNodeDetails = new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());

                if (!chkHasResults.Any())
                    await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails).ConfigureAwait(false);

                // Since now we have all records about primary first and second tabs data, 
                // then check whether those programs / Jcls are processed or not.
                // If not, then process those and then insert those records into FirstTabProgramDetails and 
                // SecondTabProgramDetails tables respectively...

                // Both the tables will contain data from same program, so we will start processing from first tab data...
                #endregion

                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                    stringBuilder.AppendLine("Started GetWorkFlowWorkSpace for projectId: " + projectId +
                                      " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                    Console.WriteLine("Started GetWorkFlowWorkSpace for projectId: " + projectId +
                                      " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                    var actionResult =
                        await GetWorkFlowWorkSpace(projectId, treeItem.ClassCalled, treeItem.MethodCalled,
                            actionWorkflow[0].ActionWorkflowId).ConfigureAwait(false);
                    var dataContent = await actionResult.ExecuteAsync(CancellationToken.None).ConfigureAwait(false);
                    var responseMessage = await dataContent.Content.ReadAsStringAsync().ConfigureAwait(false);

                    if (responseMessage == "\"Too Many Nodes\"")
                    {
                        stringBuilder.AppendLine(
                            "========================================================================================");
                        Console.WriteLine(
                            "========================================================================================");
                        stringBuilder.AppendLine("Started GetWorkFlowWorkSpaceTooManyNodes for projectId: " + projectId +
                                                 " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                        Console.WriteLine("Started GetWorkFlowWorkSpaceTooManyNodes for projectId: " + projectId +
                                          " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);

                        await
                            GetWorkFlowWorkSpaceTooManyNodes(projectId, treeItem.ClassCalled, treeItem.MethodCalled,
                                actionWorkflow[0].ActionWorkflowId).ConfigureAwait(false);

                        stringBuilder.AppendLine(
                            "========================================================================================");
                        Console.WriteLine(
                            "========================================================================================");

                        stringBuilder.AppendLine("Completed GetWorkFlowWorkSpaceTooManyNodes for projectId: " + projectId +
                                                 " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                        Console.WriteLine("Completed GetWorkFlowWorkSpaceTooManyNodes for projectId: " + projectId +
                                          " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                        stringBuilder.AppendLine(
                            "========================================================================================");
                        Console.WriteLine(
                            "========================================================================================");
                        return Ok("Too Many Nodes");
                    }

                    stringBuilder.AppendLine(
                       "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");

                    stringBuilder.AppendLine("Completed GetWorkFlowWorkSpace for projectId: " + projectId +
                                    " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                    Console.WriteLine("Completed GetWorkFlowWorkSpace for projectId: " + projectId +
                                      " and ActionWorkflowId: " + actionWorkflow[0].ActionWorkflowId);
                    stringBuilder.AppendLine(
                         "========================================================================================");
                    Console.WriteLine(
                       "========================================================================================");
                }
                stringBuilder.AppendLine(
                        "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Workflow data collected successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, string className, string methodName, int actionWorkflowId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var universeProcessingHelper = new UniverseProcessingHelper();
                #region Start pre-process
                var stringBuilder = new StringBuilder();
                var secondTab = new List<TreeView>();
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                stringBuilder.AppendLine(
                     "========================================================================================");
                stringBuilder.AppendLine("Started collecting all GetAllMethodsForClass for solution: " + solutionId +
                                             " and className is: " + className + "");
                var allMethodsOfClass = GetAllMethodsForClass(className, solutionId).ToList();

                var hasAnyMethod = allMethodsOfClass.Any(t => t.MethodName == methodName);
                if (!hasAnyMethod)
                    return Ok("Workflow data collected successfully");

                int methodStatementId = allMethodsOfClass.First(t => t.MethodName == methodName).StatementId;
                int thisProgramId = allMethodsOfClass[0].FileId;

                // Check whether this file already processed or not. If yes, then do not process it again.
                // We will just check this in first tabs data...
                Console.WriteLine("========================================================================");
                Console.WriteLine("Started processing for ProgramId (FileId): " + thisProgramId);
                // if (thisProgramId == 3285) return Ok("Workflow data collected successfully");
                // if (thisProgramId == 3439) return Ok("Workflow data collected successfully");

                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == thisProgramId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any())
                    return Ok("Workflow data collected successfully");
                var generalRepositoryProductConfigruation
                   = new GeneralRepository<ProductConfiguration>(new AppDbContext());
                string qryMaxTreeNodes =
                    "select * from  productconfiguration  where PropertyName= 'Max Tree Nodes' ";

                var lstProductConfig =
                    await generalRepositoryProductConfigruation
                        .GetDataFromSqlQuery<ProductConfiguration>(qryMaxTreeNodes);

                var productConfigMaxTreeNodes =
                    lstProductConfig.FirstOrDefault(x => x.PropertyName == "Max Tree Nodes");

                int cnt = productConfigMaxTreeNodes != null ? Convert.ToInt32(productConfigMaxTreeNodes.PropertyValue) : 165000;
                /*
                var pFileMaster = await
                        _codeVortoService.FileMasterRepository.GetItem<FileMaster>(f => f.FileId == thisProgramId,
                            thisProgramId).ConfigureAwait(false);
                if (pFileMaster != null && !string.IsNullOrEmpty(pFileMaster.WorkFlowStatus))
                {
                    Console.WriteLine(pFileMaster.WorkFlowStatus);
                    Console.WriteLine("Program Name: " + pFileMaster.FilePath);
                    Console.WriteLine("=====================================================================");
                    return Ok("Too Many Nodes");
                }
                */
                stringBuilder.AppendLine(
                     "========================================================================================");
                stringBuilder.AppendLine("Started collecting all GetMethodBlock for project methodStatementId: " + methodStatementId);
                var workflowRef = GetMethodBlock(methodStatementId);

                var lstTreeView = new List<TreeView>();
                var statementId = workflowRef[0].StatementId;
                var treeNodeId = 0;

                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + statementId,
                    GraphName = "<span class='nodeToBold'>" + allMethodsOfClass[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = allMethodsOfClass[0].BaseCommandId,
                    StatementReferenceMaster = allMethodsOfClass[0],
                    ActualStatementId = "Actual_" + allMethodsOfClass[0].StatementId,
                    NodeId = ++treeNodeId,
                    IndentLevel = 1,
                    ProgramId = allMethodsOfClass[0].FileId
                });

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
                    AlternateName = statementMaster.AlternateName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2,
                    ProgramId = allMethodsOfClass[0].FileId
                }));

                #endregion

                #region Process the details

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                #region Process for base command id = 5 and 6

                var auto = 0;
                int indentLevel = 3;
                var callingAndCalled = new Dictionary<string, List<string>>();

                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == solutionId).ConfigureAwait(false);
                var fileMasters = fileMaster as IList<FileMaster> ?? fileMaster.ToList();
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMasters.ToArray();

                List<NameValue> lstAllCalledMethods = new List<NameValue>();

                foreach (var treeItem in lstTreeView)
                {
                    if ((treeItem.BaseCommandId != 5) && (treeItem.StatementReferenceMaster.OtherBaseCommandId != 5))
                        continue;

                    auto++;
                    stringBuilder.AppendLine(
      "========================================================================================");
                    stringBuilder.AppendLine("Started process GetCallInternalDetails for projectId: " +
                                                        projectId + ", and fileId is :" + treeItem.StatementReferenceMaster.FileId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, 0,
                        treeItem.StatementReferenceMaster.FileId, indentLevel, lstAllCalledMethods, ref auto,
                        ref treeNodeId, ref callingAndCalled, cnt);

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
                    int programId = 0;
                    if (treeItem.StatementReferenceMaster.OriginalStatement.StartsWith("CALL "))
                    {
                        var iName = treeItem.StatementReferenceMaster.OriginalStatement.Split('@').LastOrDefault();
                        string fName = iName.Split('(').FirstOrDefault();
                        var anyNewFile = copyOfFileMaster.Any(
                            f =>
                                !string.IsNullOrEmpty(fName) &&
                                (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                f.FileName.StartsWith(fName));
                        if (anyNewFile)
                        {
                            var icdFile = copyOfFileMaster.Single(
                                f =>
                                    !string.IsNullOrEmpty(fName) &&
                                    (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                    f.FileName.StartsWith(fName));

                            var neLink = treeItem.GraphName +
                                         "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                         icdFile.FileId + ");'>[ " + fName + " ]</a>";
                            if (!treeItem.GraphName.Contains("includeStateDialog"))
                                treeItem.GraphName = neLink;
                            continue;
                        }
                    }
                    else
                    {
                        var oStatement = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                        if (oStatement.Split(' ').Length < 2) continue;

                        var programName = oStatement.Split(' ')[2];
                        var anyFile = copyOfFileMaster.ToList()
                            .Any(f => (programName != null)
                                      && f.FileName.StartsWith(programName)
                                      && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));

                        if (!anyFile) continue;
                        var programFilePath = copyOfFileMaster.ToList()
                            .Find(f => (programName != null)
                                       && f.FileName.StartsWith(programName)
                                       && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));
                        programId = programFilePath.FileId;
                    }
                    // Once we have program Id (FileId) then search for records in workflownodedetails, workflowlinkdetails
                    // workflowtreeviewsecondtabdetails, workflowtreeviewtabfirstdetails then mark those entries here with statement id,
                    // as this will help to put all statements in sequence for workflow.

                    auto++;
                    stringBuilder.AppendLine(
    "========================================================================================");
                    stringBuilder.AppendLine("Started process GetCallExternalDetails for projectId: " +
                                                        projectId + ", and fileId is :" + programId);
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, programId,
                        lstAllCalledMethods, ref auto, indentLevel, ref treeNodeId, ref callingAndCalled, 0, cnt);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                // Check here whether treeview has too many nodes and return it. 
                // But, we need to Log it somewhere to trace it out later.
                // Belo is node structure to check it...
                /*
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "TooMany_99999",
                        HasChild = true,
                        GraphName = "Too Many Nodes",
                        BaseCommandId = 150,
                        StatementReferenceMaster = null,
                        NodeId = ++treeNodeId,
                        ActualStatementId = "TooManyNodes",
                        IndentLevel = indentLevel,
                        ProgramId = programId
                    });
                */

                //if (copyOfLstTreeView.Any( n => n.GraphId == "TooMany_99999"
                //                               && n.ActualStatementId == "TooManyNodes" &&
                //                               n.GraphName == "Too Many Nodes"))
                if (copyOfLstTreeView.Count >= cnt)
                {
                    var flMaster = await
                        _codeVortoService.FileMasterRepository.GetItem<FileMaster>(f => f.FileId == thisProgramId,
                            thisProgramId).ConfigureAwait(false);
                    if (flMaster == null) return Ok("Too Many Nodes");

                    flMaster.WorkFlowStatus = "Too many nodes to this program. Skipped for processing workflow.";
                    // Console.WriteLine("Program Name: " + pFileMaster.FilePath + " Program Id: " + flMaster.FileId);
                    flMaster.FileTypeExtensionReference = null;

                    await _codeVortoService.FileMasterRepository.UpdateItem(flMaster).ConfigureAwait(false);

                    return Ok("Too Many Nodes");
                }


                #endregion

                #region Extra blocks...

                var indexPosition = -1;
                lstTreeView = copyOfLstTreeView.ToList();
                var ifCounter = 0;
                string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                var tempId = 0;
                var groupId = 1;
                foreach (var treeItem in copyOfLstTreeView.FindAll(t => t.BaseCommandId == 6 || t.BaseCommandId == 5).ToList())
                {
                    treeItem.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeItem.GraphName + "</span>";
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
                    foreach (var child in childItems)
                    {
                        if (tempId > 3)
                            tempId = 0;
                        if ((child.BaseCommandId == 6) || (child.BaseCommandId == 5))
                        {
                            var groupName = string.Empty;
                            if (child.StatementReferenceMaster == null) continue;
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
                    if (treeItem.StatementReferenceMaster != null && treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
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


                // copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();
                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                secondTab.AddRange(copyOfLstTreeView
                    .Where(item => (item.BaseCommandId == 6)
                            || (item.BaseCommandId == 8)
                            || (item.BaseCommandId == 10)
                            || (item.BaseCommandId == 1)
                            || (item.BaseCommandId == 25)
                            || (item.BaseCommandId == 5)
                            || (item.BaseCommandId == 30)
                            || (item.BaseCommandId == 3)
                            || (item.BaseCommandId == 4)
                            || (item.BaseCommandId == 45)));
                var tempList =
                    (from d in secondTab
                     where (d.BaseCommandId == 1)
                           || (d.BaseCommandId == 10)
                           || (d.BaseCommandId == 25) || (d.BaseCommandId == 3)
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
                    foreach (var cItem in childItems)
                    {
                        stringBuilder.AppendLine(
          "========================================================================================");
                        stringBuilder.AppendLine("\n" + "Started process AttachChildItems for project: (" + projectId + ")");
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
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;").ConfigureAwait(false);
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

                // allSeqListItems = allSeqListItems.Skip(1).ToList();
                // ReSharper disable once RedundantAssignment
                var firstItem = allSeqListItems.First();
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList()
                        .Distinct();
                stringBuilder.AppendLine(
         "========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpGetAllClassNameDeclared for solution: " + solutionId);
                //var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                //if (projectDetails == null) return Ok(projectId);
                //int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                object[] parameters =
                {
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId}
                };
                var allClassNameDeclaredAndClassCalledList = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNameDeclared", parameters)
                    .ContinueWith(t => { var result = t.Result; return result.ToList(); }).ConfigureAwait(false);
                string wdth = _clsUniverseBasic.CalculateWidth(firstItem.GraphName.Length);
                treeView.Nodes.Add(new Node
                {
                    Id = nodeId,
                    Name = firstItem.GraphName,
                    ShapeId = "RoundRect",
                    Color = "#ffcc00",
                    Width = wdth,
                    StatementId = int.Parse(firstItem.ActualStatementId.Split('_')[1]),
                    GroupName = firstItem.GroupName,
                    GroupId = firstItem.GroupId,
                    ProgramId = firstItem.ProgramId
                });

                var linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    var nm = curItem.GraphName.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                    if (string.IsNullOrEmpty(nm)) continue;

                    if (curItem.BaseCommandId == 1)
                    {
                        #region PrimaryCommandId == 1 || BaseCommandId == 1

                        var ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                        var condition = ifPart.Contains("THEN")
                            ? ifPart.Substring(0,
                                ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                            : ifPart;

                        var charCountOfText = condition.Length;
                        var width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                        var height = _clsUniverseBasic.CalculateHeight(charCountOfText);

                        nodeId++;
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
                        stringBuilder.AppendLine(
         "========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsIf for projectId: " +
                                                        projectId);
                        treeView = childItems.Aggregate(treeView, (current, cItem) => universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, current, allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber));

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
                                classNameDeclared =
                                    allClassNameDeclaredAndClassCalledList.AsQueryable().Where(expressionNew).ToList();
                                classNameDeclared = classNameDeclared.ToList();
                            }
                            if (classNameDeclared.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if ((curItem.PrimaryCommandId == 25) || (curItem.PrimaryCommandId == 38))
                            {
                                string[] strcCalled = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                                string strName = "";
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
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId =
                                        int.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1]),
                                    ProgramId = curItem.ProgramId
                                });
                            else
                                treeView.Links.Add(new Link
                                {
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] ",
                                    ProgramId = curItem.ProgramId
                                });
                            linkSeqNumber++;
                            var childItems = (from s in secondTab
                                              where (s.ParentId == curItem.GraphId)
                                                    && (s.BaseCommandId != 25)
                                              select s).ToList().Distinct();
                            stringBuilder.AppendLine(
"========================================================================================");
                            stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " +
                                                            projectId);
                            foreach (var cItem in childItems)

                                treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndClassCalledList,
                                    ref nodeId, ref linkSeqNumber);
                        }

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 8)
                    {
                        #region BaseCommandId == 8

                        var nodeColor = "#c0c0c0";
                        var methodCalled = allClassNameDeclaredAndClassCalledList
                            .Where(s => s.BaseCommandId == 19 && s.FileId == curItem.StatementReferenceMaster.FileId)
                            .ToList();

                        if (methodCalled.Count != 0)
                            nodeColor = "#00ffff";

                        nodeId++;

                        Node node;
                        if ((curItem.PrimaryCommandId == 23) || (curItem.PrimaryCommandId == 36))
                        {
                            if (!methodCalled.Any()) continue;

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
                        string m;
                        if ((curItem.PrimaryCommandId == 23) || (curItem.PrimaryCommandId == 36))
                            m = curItem.StatementReferenceMaster.MethodName;
                        else
                            m = curItem.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                //Origin = lastNode.Id,
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                StatementId = curItem.StatementReferenceMaster.StatementId,
                                ProgramId = curItem.ProgramId
                            });
                        }
                        else
                        {
                            treeView.Links.Add(new Link
                            {
                                //Origin = lastNode.Id,
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab
                             where (s.ParentId == curItem.GraphId) && (s.BaseCommandId != 25)
                             select s)
                                .ToList().Distinct();
                        stringBuilder.AppendLine(
"========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " +
                                                        projectId);
                        foreach (var cItem in childItems)

                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, secondTab, cItem, treeView,
                                allClassNameDeclaredAndClassCalledList, ref nodeId,
                                ref linkSeqNumber);

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
                            string strName = "";
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
                                Origin = nodeId - 1,
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
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                        stringBuilder.AppendLine(
"========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal for projectId: " +
                                                        projectId);
                        foreach (var cItem in childItems)
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
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
                    else if (curItem.BaseCommandId == 45)
                    {
                        #region BaseCommandId == 45

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
                stringBuilder.AppendLine(
"========================================================================================");
                stringBuilder.AppendLine("Started process for RemoveMultipleLinks for projectId: " +
                                                projectId);
                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes); stringBuilder.AppendLine(
"========================================================================================");
                stringBuilder.AppendLine("Started process for RemoveHangingNodes for projectId: " +
                                                projectId);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                #region Add to database table...
                treeView.Nodes.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowNodeDetails = treeView.Nodes.Select(node => new WorkflowNodeDetails
                {
                    ProjectId = projectId,
                    BaseCommandId = node.BaseCommandId,
                    ActionWorkflowId = actionWorkflowId,
                    BusinessDescription = node.BusinessDescription,
                    BusinessName = node.BusinessName,
                    ParentId = node.ParentId,
                    Id = node.Id,
                    StatementId = node.StatementId,
                    StatementTypeId = node.StatementTypeId,
                    ChildId = node.ChildId,
                    FileId = node.FileId,
                    Width = node.Width,
                    Name = node.Name.Replace("</span>", "").Trim(),
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
                await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails).ConfigureAwait(false);

                #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                string mySqlQueryNodeDetails = " Select * from WorkflowNodeDetails " +
                                               " Where ProjectId = " + projectId + " AND WorkflowStartStatementId = " +
                                               statementId +
                                               " ";
                var statementNodes =
                    await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(mySqlQueryNodeDetails);

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
                treeView.Links.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowLinkDetails = treeView.Links.Select(link => new WorkflowLinkDetails
                {
                    BaseCommandId = link.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    BusinessDescription = link.BusinessDescription,
                    BusinessName = link.BusinessName,
                    LinkText = link.LinkText,
                    Origin = link.Origin,
                    StatementId = link.StatementId,
                    Target = link.Target,
                    WorkflowStartStatementId = actionWorkflowId,
                    ProgramId = link.ProgramId
                }).ToList();

                var generalRepositoryLinkDetails = new GeneralRepository<WorkflowLinkDetails>(new AppDbContext());
                await generalRepositoryLinkDetails.BulkInsert(lstWorkflowLinkDetails);

                #endregion

                #region First and Second tab data
                secondTab.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowTreeviewSecondTabDetails = secondTab.Select(sTab => new SecondTabProgramDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    AlternateName = sTab.AlternateName,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflowId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel,
                    ProgramId = sTab.ProgramId,
                    AnnotateStatement = null
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<SecondTabProgramDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                string mySqlQuery = " Select * from SecondTabProgramDetails Where " +
                                    " ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId + " ";
                var statementSecondTab =
                    await generalRepositoryWorkflowTreeviewSecondTabDetails
                        .GetDataFromSqlQuery<SecondTabProgramDetails>(mySqlQuery);

                foreach (var stmt in statementSecondTab)
                    if ((stmt.BaseCommandId == 1) || (stmt.BaseCommandId == 5) || (stmt.BaseCommandId == 6))
                    {
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await
                            generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                    }

                #endregion
                copyOfLstTreeView.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lsTreeviewTabFirstDetails = copyOfLstTreeView.Select(fTab => new FirstTabProgramDetails
                {
                    BaseCommandId = fTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild.ToString(),
                    MethodCalled = fTab.MethodCalled,
                    ParentId = fTab.ParentId,
                    PrimaryCommandId = fTab.PrimaryCommandId,
                    SpriteCssClass = fTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflowId,
                    IndentLevel = fTab.IndentLevel,
                    ProgramId = fTab.ProgramId
                }).ToList();

                var generalRepositoryTreeviewTabFirstDetails =
                    new GeneralRepository<FirstTabProgramDetails>(new AppDbContext());
                await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);

                #endregion

                #region Decision Chart Code

                //IHttpActionResult decisionChartResult =
                //        await GetDecisionChart(projectId, stmtId);
                //await decisionChartResult.ExecuteAsync(CancellationToken.None);

                #endregion
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Workflow data collected successfully");

                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceTooManyNodes(int projectId, string className,
            string methodName, int actionWorkflowId)
        {
            var stringBuilder = new StringBuilder();
            var universeProcessingHelper = new UniverseProcessingHelper();
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                stringBuilder.AppendLine(
                    "========================================================================================");
                stringBuilder.AppendLine("Started collecting all GetAllMethodsForClass for solution: " + solutionId +
                                             " and className is: " + className + "");
                var allMethodsOfClass = GetAllMethodsForClass(className, solutionId).ToList();

                var hasAnyMethod = allMethodsOfClass.Any(t => t.MethodName == methodName);
                if (!hasAnyMethod)
                    return Ok("Workflow data collected successfully");

                int methodStatementId = allMethodsOfClass.First(t => t.MethodName == methodName).StatementId;
                int thisProgramId = allMethodsOfClass[0].FileId;

                // Check whether this file already processed or not. If yes, then do not process it again.
                // We will just check this in first tabs data...
                Console.WriteLine("========================================================================");
                Console.WriteLine("Started processing for ProgramId (FileId): " + thisProgramId);
                // if(thisProgramId == 3285) return Ok("Workflow data collected successfully");

                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == thisProgramId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any())
                    return Ok("Workflow data collected successfully");

                /*
                var pFileMaster = await
                        _codeVortoService.FileMasterRepository.GetItem<FileMaster>(f => f.FileId == thisProgramId,
                            thisProgramId).ConfigureAwait(false);
                if (pFileMaster != null && !string.IsNullOrEmpty(pFileMaster.WorkFlowStatus))
                {
                    Console.WriteLine(pFileMaster.WorkFlowStatus);
                    Console.WriteLine("Program Name: " + pFileMaster.FilePath);
                    Console.WriteLine("=====================================================================");
                    return Ok("Too Many Nodes");
                }
                */
                stringBuilder.AppendLine(
                   "========================================================================================");
                stringBuilder.AppendLine("Started collecting all GetMethodBlock for  methodStatementId: " + methodStatementId);
                var workflowRef = GetMethodBlock(methodStatementId);

                var statementId = workflowRef[0].StatementId;
                var treeNodeId = 0;

                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + statementId,
                    GraphName = "<span class='nodeToBold'>" + allMethodsOfClass[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = allMethodsOfClass[0].BaseCommandId,
                    StatementReferenceMaster = allMethodsOfClass[0],
                    ActualStatementId = "Actual_" + allMethodsOfClass[0].StatementId,
                    NodeId = ++treeNodeId,
                    IndentLevel = 1,
                    ProgramId = allMethodsOfClass[0].FileId
                });

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
                    AlternateName = statementMaster.AlternateName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2,
                    ProgramId = allMethodsOfClass[0].FileId
                }));

                #endregion
                var generalRepositoryProductConfigruation
                    = new GeneralRepository<ProductConfiguration>(new AppDbContext());
                string qryMaxTreeNodes =
                    "select * from productconfiguration where PropertyName= 'Max Tree Nodes' ";

                var lstProductConfig =
                    await generalRepositoryProductConfigruation
                        .GetDataFromSqlQuery<ProductConfiguration>(qryMaxTreeNodes);

                var productConfigMaxTreeNodes =
                    lstProductConfig.FirstOrDefault(x => x.PropertyName == "Max Tree Nodes");

                int cnt = productConfigMaxTreeNodes != null ? Convert.ToInt32(productConfigMaxTreeNodes.PropertyValue) : 165000;
                #region Process the details

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                #region Process for base command id = 5 and 6

                var auto = 0;
                int indentLevel = 3;
                var callingAndCalled = new Dictionary<string, List<string>>();

                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == solutionId).ConfigureAwait(false);
                var fileMasters = fileMaster as IList<FileMaster> ?? fileMaster.ToList();
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMasters.ToArray();

                List<NameValue> lstAllCalledMethods = new List<NameValue>();
                foreach (var treeItem in lstTreeView)
                {
                    if ((treeItem.BaseCommandId != 5) && (treeItem.StatementReferenceMaster.OtherBaseCommandId != 5))
                        continue;

                    auto++;
                    stringBuilder.AppendLine(
   "========================================================================================");
                    stringBuilder.AppendLine("Started process GetCallInternalDetails for projectId: " +
                                                        projectId + ", and fileId is :" + treeItem.StatementReferenceMaster.FileId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, 0,
                        treeItem.StatementReferenceMaster.FileId, indentLevel, lstAllCalledMethods, ref auto, ref treeNodeId,
                        ref callingAndCalled, cnt);

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
                    int programId = 0;
                    if (treeItem.StatementReferenceMaster.OriginalStatement.StartsWith("CALL "))
                    {
                        var iName = treeItem.StatementReferenceMaster.OriginalStatement.Split('@').LastOrDefault();
                        string fName = iName.Split('(').FirstOrDefault();
                        var anyNewFile = copyOfFileMaster.Any(
                            f =>
                                !string.IsNullOrEmpty(fName) &&
                                (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                f.FileName.StartsWith(fName));
                        if (anyNewFile)
                        {
                            var icdFile = copyOfFileMaster.Single(
                                f =>
                                    !string.IsNullOrEmpty(fName) &&
                                    (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                    f.FileName.StartsWith(fName));

                            var neLink = treeItem.GraphName +
                                         "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                         icdFile.FileId + ");'>[ " + fName + " ]</a>";
                            if (!treeItem.GraphName.Contains("includeStateDialog"))
                                treeItem.GraphName = neLink;
                            continue;
                        }
                    }
                    else
                    {
                        var oStatement = treeItem.StatementReferenceMaster.OriginalStatement.Trim();
                        if (oStatement.Split(' ').Length < 2) continue;

                        var programName = oStatement.Split(' ')[2];
                        var anyFile = copyOfFileMaster.ToList()
                            .Any(f => (programName != null)
                                      && f.FileName.StartsWith(programName)
                                      && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));

                        if (!anyFile) continue;
                        var programFilePath = copyOfFileMaster.ToList()
                            .Find(f => (programName != null)
                                       && f.FileName.StartsWith(programName)
                                       && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));
                        programId = programFilePath.FileId;
                    }
                    // Once we have program Id (FileId) then search for records in workflownodedetails, workflowlinkdetails
                    // workflowtreeviewsecondtabdetails, workflowtreeviewtabfirstdetails then mark those entries here with statement id,
                    // as this will help to put all statements in sequence for workflow.

                    auto++;
                    stringBuilder.AppendLine(
"========================================================================================");
                    stringBuilder.AppendLine("Started process GetCallExternalDetails for projectId: " +
                                                        projectId + ", and fileId is :" + programId);
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, programId,
                        lstAllCalledMethods, ref auto, indentLevel, ref treeNodeId, ref callingAndCalled, 1, cnt);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                // Check here whether treeview has too many nodes and return it. 
                // But, we need to Log it somewhere to trace it out later.
                // Belo is node structure to check it...
                /*
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "TooMany_99999",
                        HasChild = true,
                        GraphName = "Too Many Nodes",
                        BaseCommandId = 150,
                        StatementReferenceMaster = null,
                        NodeId = ++treeNodeId,
                        ActualStatementId = "TooManyNodes",
                        IndentLevel = indentLevel,
                        ProgramId = programId
                    });
                */
                //if (copyOfLstTreeView.Any(n => n.GraphId == "TooMany_99999"
                //                               && n.ActualStatementId == "TooManyNodes" &&
                //                               n.GraphName == "Too Many Nodes"))
                if (copyOfLstTreeView.Count >= cnt)
                {
                    var flMaster = await
                        _codeVortoService.FileMasterRepository.GetItem<FileMaster>(f => f.FileId == thisProgramId,
                            thisProgramId).ConfigureAwait(false);
                    if (flMaster == null) return Ok("Too Many Nodes");

                    flMaster.WorkFlowStatus = "Too many nodes to this program. Skipped for processing workflow.";
                    // Console.WriteLine("Program Name: " + pFileMaster.FilePath + " Program Id: " + flMaster.FileId);
                    flMaster.FileTypeExtensionReference = null;

                    await _codeVortoService.FileMasterRepository.UpdateItem(flMaster).ConfigureAwait(false);

                    return Ok("Too Many Nodes");
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
         "========================================================================================");
                            stringBuilder.AppendLine("\n" + "Started process AssignColorsToChildNodes for project: " + projectId);
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
                                || (item.BaseCommandId == 5) || (item.BaseCommandId == 30) || (item.BaseCommandId == 45)));
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
                    stringBuilder.AppendLine(
        "========================================================================================");
                    stringBuilder.AppendLine("\n" + "Started process AttachChildItems for project: " + projectId);
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
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;").ConfigureAwait(false);
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

                // allSeqListItems = allSeqListItems.Skip(1).ToList();
                // ReSharper disable once RedundantAssignment
                var firstItem = allSeqListItems.First();
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList()
                        .Distinct();

                //var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                //if (projectDetails == null) return Ok(projectId);
                //int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                stringBuilder.AppendLine(
        "========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpGetAllClassNameDeclared for solution: " + solutionId);
                object[] parameters =
                {
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId}
                };
                var allClassNameDeclaredAndClassCalledList = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNameDeclared", parameters)
                    .ContinueWith(t => { var result = t.Result; return result.ToList(); }).ConfigureAwait(false);
                string wdth = _clsUniverseBasic.CalculateWidth(firstItem.GraphName.Length);
                treeView.Nodes.Add(new Node
                {
                    Id = nodeId,
                    Name = firstItem.GraphName,
                    ShapeId = "RoundRect",
                    Color = "#ffcc00",
                    Width = wdth,
                    StatementId = int.Parse(firstItem.ActualStatementId.Split('_')[1]),
                    GroupName = firstItem.GroupName,
                    GroupId = firstItem.GroupId,
                    ProgramId = firstItem.ProgramId
                });

                var linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    var nm = curItem.GraphName.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                    if (string.IsNullOrEmpty(nm)) continue;

                    if (curItem.BaseCommandId == 1)
                    {
                        #region PrimaryCommandId == 1 || BaseCommandId == 1

                        var ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                        var condition = ifPart.Contains("THEN")
                            ? ifPart.Substring(0,
                                ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                            : ifPart;

                        var charCountOfText = condition.Length;
                        var width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                        var height = _clsUniverseBasic.CalculateHeight(charCountOfText);

                        nodeId++;
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
                        stringBuilder.AppendLine(
      "========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsIf for projectId: " +
                                                        projectId);
                        foreach (var cItem in childItems)
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
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
                                classNameDeclared =
                                    allClassNameDeclaredAndClassCalledList.AsQueryable().Where(expressionNew).ToList();
                                classNameDeclared = classNameDeclared.ToList();
                            }
                            if (classNameDeclared.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if ((curItem.PrimaryCommandId == 25) || (curItem.PrimaryCommandId == 38))
                            {
                                string[] strcCalled = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                                string strName = "";
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
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId =
                                        int.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1]),
                                    ProgramId = curItem.ProgramId
                                });
                            else
                                treeView.Links.Add(new Link
                                {
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] ",
                                    ProgramId = curItem.ProgramId
                                });
                            linkSeqNumber++;
                            var childItems = (from s in secondTab
                                              where (s.ParentId == curItem.GraphId)
                                                    && (s.BaseCommandId != 25)
                                              select s).ToList().Distinct();
                            stringBuilder.AppendLine(
  "========================================================================================");
                            stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " +
                                                            projectId);
                            foreach (var cItem in childItems)
                                treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView,
                                    allClassNameDeclaredAndClassCalledList,
                                    ref nodeId, ref linkSeqNumber);
                        }

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 8)
                    {
                        #region BaseCommandId == 8

                        var nodeColor = "#c0c0c0";
                        var methodCalled = allClassNameDeclaredAndClassCalledList
                            .Where(s => s.BaseCommandId == 19 && s.FileId == curItem.StatementReferenceMaster.FileId)
                            .ToList();

                        if (methodCalled.Count != 0)
                            nodeColor = "#00ffff";

                        nodeId++;

                        Node node;
                        if ((curItem.PrimaryCommandId == 23) || (curItem.PrimaryCommandId == 36))
                        {
                            if (!methodCalled.Any()) continue;

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
                        string m;
                        if ((curItem.PrimaryCommandId == 23) || (curItem.PrimaryCommandId == 36))
                            m = curItem.StatementReferenceMaster.MethodName;
                        else
                            m = curItem.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                //Origin = lastNode.Id,
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                StatementId = curItem.StatementReferenceMaster.StatementId,
                                ProgramId = curItem.ProgramId
                            });
                        }
                        else
                        {
                            treeView.Links.Add(new Link
                            {
                                //Origin = lastNode.Id,
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab
                             where (s.ParentId == curItem.GraphId) && (s.BaseCommandId != 25)
                             select s)
                                .ToList().Distinct();
                        stringBuilder.AppendLine(
 "========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " +
                                                        projectId);
                        foreach (var cItem in childItems)
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, secondTab, cItem, treeView,
                                allClassNameDeclaredAndClassCalledList, ref nodeId,
                                ref linkSeqNumber);

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
                            string strName = "";
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
                                Origin = nodeId - 1,
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
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                        stringBuilder.AppendLine(
"========================================================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal for projectId: " +
                                                        projectId);
                        foreach (var cItem in childItems)
                        {
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem,
                                treeView,
                                allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);
                        }

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
                    else if (curItem.BaseCommandId == 45)
                    {
                        #region BaseCommandId == 45

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
                stringBuilder.AppendLine(
"========================================================================================");
                stringBuilder.AppendLine("Started process for RemoveMultipleLinks for projectId: " +
                                                projectId);
                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                stringBuilder.AppendLine(
"========================================================================================");
                stringBuilder.AppendLine("Started process for RemoveHangingNodes for projectId: " +
                                                projectId);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                #region Add to database table...
                treeView.Nodes.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowNodeDetails = treeView.Nodes.Select(node => new WorkflowNodeDetails
                {
                    ProjectId = projectId,
                    BaseCommandId = node.BaseCommandId,
                    ActionWorkflowId = actionWorkflowId,
                    BusinessDescription = node.BusinessDescription,
                    BusinessName = node.BusinessName,
                    ParentId = node.ParentId,
                    Id = node.Id,
                    StatementId = node.StatementId,
                    StatementTypeId = node.StatementTypeId,
                    ChildId = node.ChildId,
                    FileId = node.FileId,
                    Width = node.Width,
                    Name = node.Name.Replace("</span>", "").Trim(),
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
                await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails).ConfigureAwait(false);

                #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                string mySqlQueryNodeDetails = " Select * from WorkflowNodeDetails " +
                                               " Where ProjectId = " + projectId + " AND WorkflowStartStatementId = " +
                                               statementId +
                                               " ";
                var statementNodes =
                    await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(mySqlQueryNodeDetails);

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
                treeView.Links.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowLinkDetails = treeView.Links.Select(link => new WorkflowLinkDetails
                {
                    BaseCommandId = link.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    BusinessDescription = link.BusinessDescription,
                    BusinessName = link.BusinessName,
                    LinkText = link.LinkText,
                    Origin = link.Origin,
                    StatementId = link.StatementId,
                    Target = link.Target,
                    WorkflowStartStatementId = actionWorkflowId,
                    ProgramId = link.ProgramId
                }).ToList();

                var generalRepositoryLinkDetails = new GeneralRepository<WorkflowLinkDetails>(new AppDbContext());
                await generalRepositoryLinkDetails.BulkInsert(lstWorkflowLinkDetails);

                #endregion

                #region First and Second tab data
                secondTab.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lstWorkflowTreeviewSecondTabDetails = secondTab.Select(sTab => new SecondTabProgramDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    AlternateName = sTab.AlternateName,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflowId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel,
                    ProgramId = sTab.ProgramId,
                    AnnotateStatement = null
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<SecondTabProgramDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                string mySqlQuery = " Select * from SecondTabProgramDetails Where " +
                                    " ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId + " ";
                var statementSecondTab =
                    await generalRepositoryWorkflowTreeviewSecondTabDetails
                        .GetDataFromSqlQuery<SecondTabProgramDetails>(mySqlQuery);

                foreach (var stmt in statementSecondTab)
                    if ((stmt.BaseCommandId == 1) || (stmt.BaseCommandId == 5) || (stmt.BaseCommandId == 6))
                    {
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await
                            generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                    }

                #endregion
                copyOfLstTreeView.ForEach(n =>
                {
                    n.ProgramId = thisProgramId;
                });
                var lsTreeviewTabFirstDetails = copyOfLstTreeView.Select(fTab => new FirstTabProgramDetails
                {
                    BaseCommandId = fTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflowId,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild.ToString(),
                    MethodCalled = fTab.MethodCalled,
                    ParentId = fTab.ParentId,
                    PrimaryCommandId = fTab.PrimaryCommandId,
                    SpriteCssClass = fTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflowId,
                    IndentLevel = fTab.IndentLevel,
                    ProgramId = fTab.ProgramId
                }).ToList();

                var generalRepositoryTreeviewTabFirstDetails =
                    new GeneralRepository<FirstTabProgramDetails>(new AppDbContext());
                await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);

                #endregion

                #region Decision Chart Code

                //IHttpActionResult decisionChartResult =
                //        await GetDecisionChart(projectId, stmtId);
                //await decisionChartResult.ExecuteAsync(CancellationToken.None);

                #endregion

                return Ok("Workflow data collected successfully");

                #endregion
            }
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
                if (childNodes.Count <= 1) continue;
                var dupes = childNodes.Where(a => childNodes.Except(new List<Node> { a }).Any(x => x.Name == a.Name)).ToList();
                var hasChilds = false;
                foreach (var dup in dupes)
                {
                    var klm = (from f in lstLinks where f.Origin == dup.Id select f).ToList();
                    if (klm.Count > 0)
                        hasChilds = true;
                }
                if (hasChilds) continue;

                var tempLinks = new List<Link>();
                if ((dupes.Count <= 0) || (dupes[0] == null)) continue;
                foreach (var n in dupes)
                {
                    var link = (from h in lstLinks
                                where (h.Origin == node.Id)
                                      && (h.Target == n.Id)
                                select h).ToList();
                    tempLinks.AddRange(link);
                }
                if (!tempLinks.Any()) continue;

                var linkText = tempLinks.Aggregate(string.Empty,
                    (current, jk) => current + jk.LinkText + ", ");
                linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                tempLinks.First().LinkText = linkText;

                foreach (var l in tempLinks)
                {
                    if (l.LinkText == linkText) continue;
                    linksToRemove.Add(l);
                }
            }
            foreach (var l in linksToRemove)
                copyOfLinks.Remove(l);

            return copyOfLinks;
        }

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab,
            TreeView curItem)
        {
            // if (allSeqListItems.Count >= 200001) return allSeqListItems;
            allSeqListItems.Add(curItem);
            var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
            foreach (var cItem in childItems)
            {
                allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
            }
            return allSeqListItems;
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int programId, List<NameValue> lstAllCalledMethods, ref int auto, int indentLevel, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled, int cntNodes, int maxNodeCount)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {

                if (treeView.ClassCalled == null) return lstTreeView;

                if (cntNodes == 1 && lstTreeView.Count >= maxNodeCount)
                {
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "TooMany_99999",
                        HasChild = true,
                        GraphName = treeView.GraphName,
                        BaseCommandId = treeView.BaseCommandId,
                        ActualStatementId = "Actual_" + statememtId,
                        IndentLevel = indentLevel,
                        ProgramId = programId
                    });
                    return lstTreeView;
                    //lstTreeView.Add(new TreeView
                    //{
                    //    ParentId = statememtId,
                    //    GraphId = "TooMany_99999",
                    //    HasChild = true,
                    //    GraphName = "Too Many Nodes",
                    //    BaseCommandId = 150,
                    //    StatementReferenceMaster = null,
                    //    NodeId = ++treeNodeId,
                    //    ActualStatementId = "TooManyNodes",
                    //    IndentLevel = indentLevel,
                    //    ProgramId = programId
                    //});
                    //return lstTreeView;
                }

                var className = treeView.ClassCalled;
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
                            AlternateName = treeView.StatementReferenceMaster.AlternateName,
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

                        var blockStmtId = statementMaster.StatementId;
                        stringBuilder.AppendLine("\n" + "Started process for get block: GetMethodBlock(" + blockStmtId +
                                                 ")");

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
                                    AlternateName = block.AlternateName,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                // lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                // lstTreeView, projectId, ref auto, ref treeNodeId, ref callingAndCalled);
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
                                    AlternateName = block.AlternateName,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                indentLevel = indentLevel + 1;
                                stringBuilder.AppendLine("\n" +
                                                         "Started process for called internal: GetCallInternalDetails(" +
                                                         programId + "," + block.FileId + ")");
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, programId, block.FileId, indentLevel, lstAllCalledMethods, ref auto,
                                    ref treeNodeId, ref callingAndCalled, maxNodeCount);

                                indentLevel = indentLevel - 1;

                                if (lstTreeView.Count < maxNodeCount) continue;
                                lstTreeView.Add(new TreeView
                                {
                                    ParentId = statememtId,
                                    GraphId = "TooMany_99999",
                                    HasChild = true,
                                    GraphName = lstTreeView.Last().GraphName,
                                    BaseCommandId = lstTreeView.Last().BaseCommandId,
                                    NodeId = ++treeNodeId,
                                    ActualStatementId = "Actual_" + statememtId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                return lstTreeView;
                                //lstTreeView.Add(new TreeView
                                //{
                                //    ParentId = statememtId,
                                //    GraphId = "TooMany_99999",
                                //    HasChild = true,
                                //    GraphName = "Too Many Nodes",
                                //    BaseCommandId = 150,
                                //    StatementReferenceMaster = null,
                                //    NodeId = ++treeNodeId,
                                //    ActualStatementId = "TooManyNodes",
                                //    IndentLevel = indentLevel,
                                //    ProgramId = programId
                                //});
                                //return lstTreeView;
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
                                    AlternateName = block.AlternateName,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });

                                statememtId = "Node" + auto + "_" + block.StatementId;
                                // lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                // lstTreeView, projectId, ref auto, ref treeNodeId);
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
                                        AlternateName = block.AlternateName,
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
                                        AlternateName = block.AlternateName,
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
            int programId, int fileId, int indentLevel, ICollection<NameValue> lstAllCalledMethods, ref int autoInt,
            ref int treeNodeId, ref Dictionary<string, List<string>> callingAndCalled, int maxNodeCount)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                if (lstAllCalledMethods.Any(s => s.ProgramId == programId && s.MethodName == treeView.MethodCalled))
                {
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "111" + autoInt + "_" + statememtId,
                        HasChild = true,
                        GraphName =
                            " <span class='nodemissingelement'>&nbsp;Definition already expanded in this workflow " +
                            " &nbsp;<a href='#' style='color: #727281; text-decoration: underline;' " +
                            " onclick='getMethodStatements(" + treeView.StatementReferenceMaster.StatementId + "," +
                            " " + treeView.StatementReferenceMaster.FileId + ", '" + treeView.MethodCalled + "');' >[ Review ]</a></span>",
                        BaseCommandId = 25,
                        StatementReferenceMaster = treeView.StatementReferenceMaster,
                        NodeId = ++treeNodeId,
                        ActualStatementId = "Missing_99999999",
                        IndentLevel = indentLevel,
                        ProgramId = programId
                    });
                    return lstTreeView;
                }
                lstAllCalledMethods.Add(new NameValue { ProgramId = programId, MethodName = treeView.MethodCalled });

                if (lstTreeView.Count >= maxNodeCount)
                {
                    lstTreeView.Add(new TreeView
                    {
                        ParentId = statememtId,
                        GraphId = "TooMany_99999",
                        HasChild = true,
                        GraphName = treeView.GraphName,
                        BaseCommandId = treeView.BaseCommandId,
                        NodeId = ++treeNodeId,
                        ActualStatementId = "Actual_" + statememtId,
                        IndentLevel = indentLevel,
                        ProgramId = programId
                    });
                    return lstTreeView;
                }

                var methodName = treeView.MethodCalled;
                int thisProjectId = treeView.StatementReferenceMaster.ProjectId;
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpFindStatementForMethodName(8," +
                                         thisProjectId + "," + methodName + "," + fileId + ")");
                object[] parameters =
                {
                    new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = thisProjectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                };
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters).Result;

                if (stmtMaster.Count == 0)
                {
                    autoInt++;
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item != null) return lstTreeView;

                    #region For the only Universe Basic launguage
                    stringBuilder.AppendLine("\n" + "Started process for GetIncludeStatement:(" + treeView.StatementReferenceMaster.ProjectId + "," + fileId + "," + methodName + ")");

                    var graphName = GetIncludeStatement(treeView.StatementReferenceMaster.ProjectId, fileId, methodName);
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
                    stringBuilder.AppendLine("\n" + "Started process for GetGenericBlock(" + stmtMaster[0].StatementId + ",8,9)");
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
                                        AlternateName = block.BusinessName,
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
                                    AlternateName = block.BusinessName,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });

                                indentLevel = indentLevel + 1;
                                stringBuilder.AppendLine("\n" + "Started process for called internal: GetCallInternalDetails(" + programId + "," + block.FileId + ")");
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, programId, block.FileId, indentLevel, lstAllCalledMethods, ref autoInt,
                                    ref treeNodeId, ref callingAndCalled, maxNodeCount);
                                indentLevel = indentLevel - 1;

                                if (lstTreeView.Count < maxNodeCount) continue;
                                lstTreeView.Add(new TreeView
                                {
                                    ParentId = statememtId,
                                    GraphId = "TooMany_99999",
                                    HasChild = true,
                                    GraphName = lstTreeView.Last().GraphName,
                                    BaseCommandId = lstTreeView.Last().BaseCommandId,
                                    NodeId = ++treeNodeId,
                                    ActualStatementId = "Actual_" + statememtId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
                                return lstTreeView;
                            }
                            if (block.BaseCommandId == 6)
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
                                    AlternateName = block.AlternateName,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel,
                                    ProgramId = programId
                                });
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
                                        AlternateName = block.AlternateName,
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
            using (_codeVortoService = new CodeVortoService())
            {
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
        }

        private List<StatementReferenceMaster> GetGenericBlock(int stmtId, int startBaseCommandId, int endBaseCommandId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
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
                return workflowRef;
            }
        }

        private static IEnumerable<StatementReferenceMaster> GetAllMethodsForClass(string className, int solutionId)
        {
            var stringBuilder = new StringBuilder();
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpUbGetAllMethodsForClass(," + className + "," +
                                         solutionId + ")");
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar) {Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className},
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId}
                };
                var callExtExpandedCode = codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUbGetAllMethodsForClass", parametersExp)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return callExtExpandedCode;
            }
        }

        [HttpGet]
        public async Task<HttpResponseMessage> ProcessPseudoCodeConversion(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                //Expression<Func<StatementReferenceMaster, bool>> pExpression = e =>
                //        e.ProjectId == projectId && e.BaseCommandId == 1;


                string sqlQy =
                       "select * from statementreferencemaster where projectId =" + projectId + " and basecommandId = 1;";
                var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);

                //var fileMaster =
                //    await
                //        _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                //            .ConfigureAwait(false);
                var allRegEx = await _codeVortoService.RegexPatternMasterRepository
                    .GetAllListItems(e => e.BaseCommandId == 1).ConfigureAwait(false);
                allRegEx = allRegEx.OrderByDescending(s => s.RegexPattern.Length).ToList();
                var allAlternateStatements = new List<StatementReferenceMaster>();
                var regexOptions = RegexOptions.CultureInvariant;
                // int loopCnt = 0;
                stringBuilder.AppendLine(
                         "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                stringBuilder.AppendLine("Started Pseudo code conversion for project: " + projectId);
                Console.WriteLine("Started Pseudo code conversion for project: " + projectId);

                foreach (var regEx in allRegEx)
                {
                    Regex regex = new Regex(regEx.RegexPattern, regexOptions);
                    foreach (var statementReference in statementReferenceMasterData)
                    {
                        int index = 1;
                        if (!string.IsNullOrEmpty(statementReference.AlternateName)) continue;
                        foreach (Match match in regex.Matches(statementReference.OriginalStatement))
                        {
                            int groupIndex = -1;
                            string groupValue = string.Empty;
                            foreach (Group group in match.Groups)
                            {
                                groupIndex++;
                                if (groupIndex == 0) continue;
                                if (string.IsNullOrEmpty(group.Value)) continue;
                                groupValue += group.Value;
                                if (groupIndex == match.Groups.Count - 2) break;
                            }
                            string alternateName = regEx.AlternateCodeRepresentation.Replace("<<" + index + ">>", groupValue);

                            if (!string.IsNullOrEmpty(alternateName))
                                alternateName = ReplaceString(alternateName);

                            statementReference.AlternateName = alternateName;
                            allAlternateStatements.Add(statementReference);
                            index++;
                        }
                    }
                }

                stringBuilder.AppendLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
                Console.WriteLine("PseudoCode Statements to update: " + allAlternateStatements.Count);

                foreach (var statementMaster in allAlternateStatements)
                {
                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementMaster)
                        .ConfigureAwait(false);
                }

                stringBuilder.AppendLine("Completed Pseudo code conversion for project: " + projectId);
                Console.WriteLine("Completed Pseudo code conversion for project: " + projectId);
                stringBuilder.AppendLine(
                         "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                LogMessage.WriteLogMessage(stringBuilder);
                var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
                return responseMessage;
            }
        }

        [HttpGet]
        public async Task<HttpResponseMessage> ProcessPseudoCodeConversionIcdFile(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                string sqlQy =
                       "select * from statementreferencemaster where fileId >= 327 and fileId <= 856 " +
                       "and projectId =" + projectId + " and basecommandId = 1 and (Alternatename is  null OR Alternatename = '');";
                var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);
                string sqlSubRoutine =
                    "select * from secondtabsubroutineprogramdetails where ProgramId >= 327 and ProgramId <= 856 and basecommandId = 1  and (Alternatename is  null or Alternatename = '');";
                var secondTabSubroutine =
                    await
                        _codeVortoService.SecondTabSubRoutineProgramRepository
                            .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(sqlSubRoutine).ConfigureAwait(false);

                var allRegEx = await _codeVortoService.RegexPatternMasterRepository
                    .GetAllListItems(e => e.BaseCommandId == 1).ConfigureAwait(false);
                allRegEx = allRegEx.OrderByDescending(s => s.RegexPattern.Length).ToList();
                var allAlternateStatements = new List<StatementReferenceMaster>();
                var regexOptions = RegexOptions.CultureInvariant;
                stringBuilder.AppendLine(
                         "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                stringBuilder.AppendLine("Started Pseudo code conversion for project: " + projectId);
                Console.WriteLine("Started Pseudo code conversion for project: " + projectId);

                foreach (var regEx in allRegEx)
                {
                    Regex regex = new Regex(regEx.RegexPattern, regexOptions);
                    foreach (var statementReference in statementReferenceMasterData)
                    {
                        int index = 1;
                        foreach (Match match in regex.Matches(statementReference.OriginalStatement))
                        {
                            int groupIndex = -1;
                            string groupValue = string.Empty;
                            foreach (Group group in match.Groups)
                            {
                                groupIndex++;
                                if (groupIndex == 0) continue;
                                if (string.IsNullOrEmpty(group.Value)) continue;
                                groupValue += group.Value;
                                if (groupIndex == match.Groups.Count - 2) break;
                            }
                            string alternateName = regEx.AlternateCodeRepresentation.Replace("<<" + index + ">>", groupValue);

                            if (!string.IsNullOrEmpty(alternateName))
                                alternateName = ReplaceString(alternateName);

                            statementReference.AlternateName = alternateName;
                            allAlternateStatements.Add(statementReference);
                            index++;
                        }
                    }
                }
                stringBuilder.AppendLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
                Console.WriteLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
                int indexCnt = -1;
                foreach (var statementMaster in allAlternateStatements)
                {
                    try
                    {
                        indexCnt++;
                        string statementId = "Actual_" + statementMaster.StatementId;
                        Console.WriteLine(indexCnt + " " + statementId);
                        var secondTabSub =
                        (from s in secondTabSubroutine
                         where s.ActualStatementId == statementId && projectId == statementMaster.FileId
                         select s).FirstOrDefault();
                        if (secondTabSub != null)
                        {
                            secondTabSub.AlternateName = statementMaster.AlternateName;
                            await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(secondTabSub);
                        }
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementMaster)
                            .ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine(exception.InnerException);
                    }
                }

                stringBuilder.AppendLine("Completed Pseudo code conversion for project: " + projectId);
                Console.WriteLine("Completed Pseudo code conversion for project: " + projectId);
                stringBuilder.AppendLine(
                         "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                LogMessage.WriteLogMessage(stringBuilder);
                var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
                return responseMessage;
            }
        }

        [HttpGet]
        public async Task<HttpResponseMessage> UpdateAlternateNames(int solutionId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine(
                         "========================================================================================");
                Console.WriteLine(
                   "========================================================================================");
                stringBuilder.AppendLine("Started updateAlternateName for solution: " + solutionId);

                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.SolutionId == solutionId).ConfigureAwait(false);
                Console.WriteLine("Total files to process: " + allFiles.Count);
                foreach (var slnFile in allFiles)
                {
                    Console.WriteLine("Current file: " + slnFile.FileId);
                    var file = slnFile;
                    var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.BaseCommandId == 1
                            && !string.IsNullOrEmpty(s.AlternateName)
                            && s.FileId == file.FileId).ConfigureAwait(false);

                    foreach (var statementReference in allStatements)
                    {
                        var reference = statementReference;
                        var secondTabStatement = await _codeVortoService.SecondTabProgramDetailsRepository
                            .GetAllListItems(f => f.StatementId == reference.StatementId
                                                  && f.BaseCommandId == reference.BaseCommandId
                                                  && f.ProgramId == reference.FileId).ConfigureAwait(false);

                        Console.WriteLine("Total statements to process: " + secondTabStatement.Count);
                        foreach (var sTabStatement in secondTabStatement)
                        {
                            string alternateName = reference.AlternateName;
                            if (!string.IsNullOrEmpty(reference.AlternateName))
                                alternateName = alternateName.Replace("THEN", "").Replace("Then", "");

                            sTabStatement.AlternateName = alternateName;
                            await _codeVortoService.SecondTabProgramDetailsRepository
                                .UpdateItem(sTabStatement).ConfigureAwait(false);
                        }
                    }
                    stringBuilder.AppendLine("Completed updateAlternateName for solution: " + solutionId);
                    Console.WriteLine("Done processing file: " + slnFile.FileId);
                    Console.WriteLine("=========================================================");
                }
            }
            LogMessage.WriteLogMessage(stringBuilder);
            var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Alternate names applied successfully");
            return responseMessage;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessObjectConnectivityDiagram(int projectId)
        {
            var stringBuilder = new StringBuilder();
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

                    var dataDependancyList =
                      await _codeVortoService.DataDependencyRepository.GetAllListItems(x => x.ProjectId == projectId);


                    stringBuilder.AppendLine("Started to bind all nodes: (" + projectId + ")");

                    var generalRepositoryNodeDetails =
                   new GeneralRepository<ConnectivityStepReference>(new AppDbContext());
                    var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<ConnectivityStepReference>(
                        " SELECT * FROM connectivitystepreference ORDER BY NodeId DESC LIMIT 1;").ConfigureAwait(false);
                    var nodeId = 1;
                    if (workflowMaxNode.Any())
                    {
                        var id = workflowMaxNode[0].NodeId;
                        nodeId = id + 1;
                    }

                    var lstNodes = new List<Node>();
                    foreach (var classNameDeclared in allClassNameDeclared.Where(s => s.ProjectId == projectId))
                    {
                        string cNameDeclared = classNameDeclared.OriginalStatement;
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classNameDeclared.BusinessName + " [" + cNameDeclared + "]",
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
                        string cNameDeclared = classCalled.ClassCalled.Split('.').LastOrDefault();
                        string nodeColor = "#c0c0c0";
                        if (allClassNameDeclared.Any(c => c.ClassNameDeclared == classCalled.ClassCalled))
                            nodeColor = "#00ffff";

                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classCalled.BusinessName + " [" + cNameDeclared + "]",
                            ShapeId = "RoundRect",
                            Height = "18",
                            Width = "100",
                            Color = nodeColor, //Color.FromName("gray").Name, // "#00ffff",
                            StatementId = classCalled.StatementId,
                            BaseCommandId = classCalled.BaseCommandId,
                            OriginalClassName = classCalled.ClassCalled,
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
                        int loopCnt = 0;
                        try
                        {

                            string className = node.OriginalClassName;
                            var allDistinctClasses =
                                (from cls in allCallExternals where cls.ClassCalled == className select cls).ToList();
                            foreach (var callExt in allDistinctClasses)
                            {
                                loopCnt++;
                                var cName = (from c in allClassNameDeclared
                                             where c.FileId == callExt.FileId && c.BaseCommandId == 19
                                             select c).ToList();

                                // var originStatement = cName.Find(n => n.ClassNameDeclared == className);
                                var originNode = lstNodes.Find(n =>
                                {
                                    var firstOrDefault = cName.FirstOrDefault();
                                    return firstOrDefault != null &&
                                           n.OriginalClassName == firstOrDefault.ClassNameDeclared;
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
                        catch (Exception exception)
                        {
                            Console.WriteLine(loopCnt);
                            return InternalServerError(exception);
                        }
                    }

                    #region Code to remove Missing Origin / missing target links

                    stringBuilder.AppendLine("Started to remove Missing Origin / missing target links: (" +
                                             projectId +
                                             ")");
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

                    stringBuilder.AppendLine("Started to remove no links to the nodes: (" + projectId + ")");
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
                                LinkText = linkText
                            };
                            nodesList.Add(newNode);
                            lstLinks.Add(newLink);
                        }
                    }

                    lstNodes.AddRange(nodesList);
                    var flowChart = new FlowChart
                    {
                        Nodes = lstNodes,
                        Links = lstLinks
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

                stringBuilder.AppendLine("Started executing next process: GetAllStartingPoints(" + projectId + ")");
                LogMessage.WriteLogMessage(stringBuilder);
                //IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                //await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
                // return Ok(flowChart);
            }
        }

        public string ReplaceString(string input)
        {
            if (string.IsNullOrEmpty(input)) return input;

            input = input.Replace(" >= ", " is greater than or equal to ");
            input = input.Replace(" <= ", " is less than or equal to ");
            input = input.Replace(" => ", " is equal to or greater than ");
            input = input.Replace(" =< ", " is equal to or less than ");
            input = input.Replace(">= ", " is greater than or equal to ");
            input = input.Replace("<= ", " is less than or equal to ");
            input = input.Replace("=> ", " is equal to or greater than ");
            input = input.Replace("=< ", " is equal to or less than ");
            input = input.Replace(" != ", " is not equal to ");
            input = input.Replace(" Ne ", " is not equal to ");
            input = input.Replace(" <> ", " is not equal to ");
            input = input.Replace(" Gt ", " is greater than ");
            input = input.Replace(" Lt ", " is less than ");
            input = input.Replace(" > ", " is greater than ");
            input = input.Replace(" < ", " is less than ");
            input = input.Replace(" = ", " is equal to ");
            input = input.Replace(" Not= ", " is not equal to ");
            input = input.Replace(" NE ", " is not equal to ");
            input = input.Replace(")NE ", ") is not equal to ");
            return input;
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

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceForSubRoutine(int prgmId, int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                string qry = "SELECT * FROM SecondTabSubroutineProgramDetails " +
                             " WHERE ProjectId = " + projectId + " AND ProgramId = " + prgmId + ";";
                var subRoutine = await _codeVortoService.SecondTabSubRoutineProgramRepository
                            .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(qry).ConfigureAwait(false);
                if (subRoutine.Any()) return Ok("File already exists");
                var dataDictionarySql =
                    "SELECT * FROM StatementReferenceMaster WHERE FileId = " + prgmId + " ";
                var dataDictionaryNew = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql).ConfigureAwait(false);
                if (!dataDictionaryNew.Any()) return Ok("Record not found");
                var treeNodeId = 0;

                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + dataDictionaryNew[0].StatementId,
                    GraphName = "<span class='nodeToBold'>" + dataDictionaryNew[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = dataDictionaryNew[0].BaseCommandId,
                    StatementReferenceMaster = dataDictionaryNew[0],
                    ActualStatementId = "Actual_" + dataDictionaryNew[0].StatementId,
                    NodeId = ++treeNodeId,
                    IndentLevel = 1,
                    ProgramId = dataDictionaryNew[0].FileId
                });
                var copyofStatementReferece = dataDictionaryNew.Skip(1).ToList();
                lstTreeView.AddRange(copyofStatementReferece.Select(statementMaster => new TreeView
                {
                    ActualStatementId = "Actual_" + statementMaster.StatementId,
                    GraphId = "Node_" + statementMaster.StatementId,
                    GraphName = statementMaster.OriginalStatement,
                    HasChild = false,
                    SpriteCssClass = "",
                    ParentId = "MethodNode_" + dataDictionaryNew[0].StatementId,
                    BaseCommandId = statementMaster.BaseCommandId,
                    PrimaryCommandId = statementMaster.PrimaryCommandId,
                    ClassCalled = statementMaster.ClassCalled,
                    MethodCalled = statementMaster.MethodCalled,
                    StatementReferenceMaster = statementMaster,
                    AlternateName = !string.IsNullOrEmpty(statementMaster.AlternateName) ? statementMaster.AlternateName : statementMaster.BusinessName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2,
                    ProgramId = statementMaster.FileId
                }));

                #region Process the details

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                copyOfLstTreeView = copyOfLstTreeView.AssignColorsToMethodBlocks();
                copyOfLstTreeView = copyOfLstTreeView.IfBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.LoopBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.ElseBlockStatement(copyOfLstTreeView);

                #region Second tab data

                try
                {
                    var lstWorkflowTreeviewSecondTabDetails =
                        copyOfLstTreeView.Select(sTab => new SecondTabSubRoutineProgramDetails
                        {
                            BaseCommandId = sTab.BaseCommandId,
                            ProjectId = projectId,
                            ActionWorkflowId = 0,
                            ActualStatementId = sTab.ActualStatementId,
                            ClassCalled = sTab.ClassCalled,
                            GraphId = sTab.GraphId,
                            GraphName = sTab.GraphName,
                            HasChild = sTab.HasChild.ToString(),
                            MethodCalled = sTab.MethodCalled,
                            AlternateName = sTab.AlternateName,
                            ParentId = sTab.ParentId,
                            PrimaryCommandId = sTab.PrimaryCommandId,
                            SpriteCssClass = sTab.SpriteCssClass,
                            WorkflowStartStatementId = 0,
                            IndentLevel = sTab.IndentLevel,
                            ProgramId = sTab.ProgramId
                        }).ToList();

                    await _codeVortoService.SecondTabSubRoutineProgramRepository
                        .BulkInsert(lstWorkflowTreeviewSecondTabDetails)
                        .ConfigureAwait(false);
                   
                    #region To Updated the GraphName against the BaseCommandId

                    string mySqlQuery = " SELECT * FROM SecondTabSubroutineProgramDetails WHERE " +
                                        " ProjectId = " + projectId + " AND ProgramId = " + prgmId + " ";

                    var statementSecondTab = await _codeVortoService.SecondTabSubRoutineProgramRepository
                            .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(mySqlQuery);

                    foreach (var stmt in statementSecondTab)
                    {
                        if (stmt.BaseCommandId != 1 && stmt.BaseCommandId != 5 && stmt.BaseCommandId != 6) continue;
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(stmt);
                    }

                #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
                #endregion

                return Ok("Subroutine or Include workflow data collected successfully");

                #endregion

                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateWorkflowNames(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                using (var appDbContext = new AppDbContext())
                {
                    var allItems = await appDbContext.UniverseFileMenu.ToListAsync().ConfigureAwait(false);
                    var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                                            .GetAllListItems(e => e.ProjectId == projectId).ConfigureAwait(false);

                    foreach (var actionWorkflow in allActionWorkflows)
                    {
                        if (actionWorkflow.FileId == 0) continue;
                        if (actionWorkflow.FileMaster == null) continue;
                        var fileName = Path.GetFileNameWithoutExtension(actionWorkflow.OriginFilePath);
                        if (string.IsNullOrEmpty(fileName)) continue;

                        if (fileName.Contains("."))
                        {
                            string beforeDot = fileName.Split('.')[1];
                            var hasValue = allItems.Any(s => beforeDot != null
                                && (s.ActionExecuted.StartsWith(fileName) ||
                                s.ActionExecuted.StartsWith(beforeDot)));

                            if (!hasValue) continue;

                            var fileMenu = allItems.Find(s => beforeDot != null
                                                                && (s.ActionExecuted.StartsWith(fileName) ||
                                                                    s.ActionExecuted.StartsWith(beforeDot)));
                            actionWorkflow.WorkflowName = fileMenu.WorkflowMenuName;
                            actionWorkflow.FileMaster = null;
                            actionWorkflow.ProjectMaster = null;
                            // actionWorkflow.WorkflowBusinessName = fileMenu.WorkflowMenuName;
                            await _codeVortoService.ActionWorkflowsRepository
                                .UpdateItem(actionWorkflow).ConfigureAwait(false);
                        }
                        else
                        {
                            var hasValue = allItems.Any(s => s.ActionExecuted.StartsWith(fileName));

                            if (!hasValue) continue;

                            var fileMenu = allItems.Find(s => s.ActionExecuted.StartsWith(fileName));
                            actionWorkflow.WorkflowName = fileMenu.WorkflowMenuName;
                            actionWorkflow.FileMaster = null;
                            actionWorkflow.ProjectMaster = null;
                            // actionWorkflow.WorkflowBusinessName = fileMenu.WorkflowMenuName;

                            await _codeVortoService.ActionWorkflowsRepository
                                .UpdateItem(actionWorkflow).ConfigureAwait(false);
                        }
                    }
                }
                return Ok("Action workflows updated successfully");
            }
        }

        //[HttpGet]
        //public async Task<HttpResponseMessage> ProcessPseudoCodeConversionIcdFile(int projectId)
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {
        //        var stringBuilder = new StringBuilder();
        //        string sqlQy =
        //          "select * from statementreferencemaster where fileId >= 327 and fileId <= 856 " +
        //          "and projectId =" + projectId + " and basecommandId = 1 and (Alternatename is  null OR Alternatename = '');";
        //        var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
        //            .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);
        //        string sqlSubRoutine =
        //            "select * from secondtabsubroutineprogramdetails where ProgramId >= 327 and ProgramId <= 856 and basecommandId = 1  and (Alternatename is  null or Alternatename = '');";
        //        var secondTabSubroutine =
        //            await
        //                _codeVortoService.SecondTabSubRoutineProgramRepository
        //                    .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(sqlSubRoutine).ConfigureAwait(false);
        //        var allRegEx = await _codeVortoService.RegexPatternMasterRepository
        //     .GetAllListItems(e => e.BaseCommandId == 1).ConfigureAwait(false);
        //        allRegEx = allRegEx.OrderByDescending(s => s.RegexPattern.Length).ToList();
        //        var allAlternateStatements = new List<StatementReferenceMaster>();
        //        var regexOptions = RegexOptions.CultureInvariant;
        //        stringBuilder.AppendLine(
        //                 "========================================================================================");
        //        Console.WriteLine(
        //           "========================================================================================");
        //        stringBuilder.AppendLine("Started Pseudo code conversion for project: " + projectId);
        //        Console.WriteLine("Started Pseudo code conversion for project: " + projectId);

        //        foreach (var regEx in allRegEx)
        //        {
        //            Regex regex = new Regex(regEx.RegexPattern, regexOptions);
        //            foreach (var statementReference in statementReferenceMasterData)
        //            {
        //                int index = 1;
        //                foreach (Match match in regex.Matches(statementReference.OriginalStatement))
        //                {
        //                    int groupIndex = -1;
        //                    string groupValue = string.Empty;
        //                    foreach (Group group in match.Groups)
        //                    {
        //                        groupIndex++;
        //                        if (groupIndex == 0) continue;
        //                        if (string.IsNullOrEmpty(group.Value)) continue;
        //                        groupValue += group.Value;
        //                        if (groupIndex == match.Groups.Count - 2) break;
        //                    }
        //                    string alternateName = regEx.AlternateCodeRepresentation.Replace("<<" + index + ">>", groupValue);

        //                    if (!string.IsNullOrEmpty(alternateName))
        //                        alternateName = ReplaceString(alternateName);

        //                    statementReference.AlternateName = alternateName;
        //                    allAlternateStatements.Add(statementReference);
        //                    index++;
        //                }
        //            }
        //        }
        //        stringBuilder.AppendLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
        //        Console.WriteLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
        //        int indexCnt = -1;
        //        foreach (var statementMaster in allAlternateStatements)
        //        {
        //            try
        //            {
        //                indexCnt++;
        //                string statementId = "Actual_" + statementMaster.StatementId;
        //                Console.WriteLine(indexCnt + " " + statementId);
        //                var secondTabSub =
        //                (from s in secondTabSubroutine
        //                 where s.ActualStatementId == statementId && projectId == statementMaster.FileId
        //                 select s).FirstOrDefault();
        //                if (secondTabSub != null)
        //                {
        //                    secondTabSub.AlternateName = statementMaster.AlternateName;
        //                    await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(secondTabSub);
        //                }
        //                await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementMaster)
        //                    .ConfigureAwait(false);
        //            }
        //            catch (Exception exception)
        //            {
        //                Console.WriteLine(exception.InnerException);
        //            }
        //        }

        //        stringBuilder.AppendLine("Completed Pseudo code conversion for project: " + projectId);
        //        Console.WriteLine("Completed Pseudo code conversion for project: " + projectId);
        //        stringBuilder.AppendLine(
        //                 "========================================================================================");
        //        Console.WriteLine("========================================================================================");
        //        LogMessage.WriteLogMessage(stringBuilder);
        //        var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
        //        return responseMessage;
        //    }
        //}

        [HttpGet]
        public async Task<IHttpActionResult> UpdatePseudoCodeForIcdFile(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    string sqlQy =
                        "select * from statementreferencemaster where fileId >= 327 and fileId <= 856 and basecommandId = 1;";
                    var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);
                    string sqlSubRoutine =
                        "select * from secondtabsubroutineprogramdetails where ProgramId >= 327 and ProgramId <= 856 and basecommandId = 1  and (Alternatename is  null or Alternatename = '');";
                    var secondTabSubroutine =
                        await
                            _codeVortoService.SecondTabSubRoutineProgramRepository
                                .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(sqlSubRoutine)
                                .ConfigureAwait(false);
                    int index = -1;
                    foreach (var secondTab in secondTabSubroutine)
                    {
                        index++;
                        Console.WriteLine(index + "  " + secondTab.ActualStatementId);
                        var actualStatement = secondTab.ActualStatementId.Split('_')[1];
                        int statementId = Convert.ToInt32(actualStatement);
                        var statementRef =
                        (from s in statementReferenceMasterData
                         where s.StatementId == statementId && s.FileId == secondTab.ProgramId
                         select s).ToList();
                        foreach (var statement in statementRef)
                        {
                            var alternateName = statement.AlternateName;
                            secondTab.AlternateName = alternateName;
                            await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(secondTab);
                        }

                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("Done");
        }
    }
}