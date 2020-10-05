using BusinessLayer.CobolVersion;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingVersionBatchController
    {
        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActioWorkflows(int projectId) //, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Added the Action workflow table

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => x.ProjectId == projectId && x.FileTypeExtensionId == 7).ConfigureAwait(false);

                //if (string.IsNullOrEmpty(fileMaster.FileName) || fileMaster.FileId == 0)
                //    return Ok("Action Workflows processed successfully");
                // Action Workflows data...
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var projectType = projectMaster.ProjectConfigType;
                if (projectType != 5 && projectType != 6) return Ok("Action Workflows processed successfully");
                var projectCongfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectConfigData = projectCongfig.GetItem(projectType);
                if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                foreach (var fileMaster in fileMasters)
                {
                    try
                    {
                        var allProcessedWorkflows = await _codeVortoService.ActionWorkflowsRepository
                            .GetAllListItems(r => r.ProjectId == projectId && r.Processed == 1);

                        object[] param =
                        {
                            new MySqlParameter("@pjId", MySqlDbType.Int32) {Value = projectId},
                            new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileMaster.FileId}
                        };

                        var genericBlocks = await _codeVortoService.StatementReferenceMasterRepository
                            .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);

                        foreach (var statement in genericBlocks)
                        {
                            if (allProcessedWorkflows.Any(s => statement.StatementId == s.MethodStatementId)) continue;
                            // if(statement.BaseCommandId != 19 ) continue;
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Batch",
                                MethodStatementId = statement.StatementId,
                                OriginFileName = Path.GetFileName(fileMaster.FilePath),
                                OriginFilePath = fileMaster.FilePath,
                                ProjectId = projectId,
                                OriginObject = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                WorkflowName = statement.OriginalStatement,
                                ServiceBaseAddress = null,
                                ServiceContract = null,
                                WorkflowBusinessName = statement.BusinessName,
                                IsDeleted = 0,
                                ReasonAboutDisable = null,
                                Processed = 0,
                                FileId = statement.FileId
                            };
                            await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow)
                                .ConfigureAwait(false);
                            // break;
                        }
                    }

                    catch (Exception exception)
                    {
                        Console.WriteLine(exception.InnerException);
                    }
                }
                return Ok("Action Workflows processed successfully");
                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine("=============================");
                stringBuilder.AppendLine("\n" + "Started to collect all actionworkflow for project: " +
                                         projectMaster.ProjectId);

                string selectSql = " SELECT * FROM ActionWorkflows where ProjectId = " + projectMaster.ProjectId +
                                   " and MethodStatementId != 0 " +
                                   " AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') " +
                                   " AND CreatedBy = 1 AND Processed = 0; ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(selectSql).ConfigureAwait(false);

                if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    try
                    {
                        int workFlowProjectId = projectMaster.ProjectId;
                        stringBuilder.AppendLine("=============================================");
                        stringBuilder.AppendLine(
                            "\n" + "Started executing next process: GetWorkFlowWorkSpaceParallel for projectId:" +
                            workFlowProjectId + ", and MethodStatementId is:" + workflow.MethodStatementId + ")");
                        var actionResult = await GetWorkFlowWorkSpaceParallel(workFlowProjectId, workflow.MethodStatementId)
                                .ConfigureAwait(false);

                        var dataContent = await actionResult.ExecuteAsync(CancellationToken.None).ConfigureAwait(false);
                        var responseMessage = await dataContent.Content.ReadAsStringAsync().ConfigureAwait(false);

                        var fileMaster = workflow.FileMaster;

                        if (responseMessage == "\"Too Many Nodes\"")
                        {
                            Console.WriteLine("Too Many Nodes for ActionWorkflowId: " + workflow.ActionWorkflowId);
                            Console.WriteLine("Origin file path: " + workflow.OriginFilePath);
                            Console.WriteLine("=======================================================");

                            fileMaster.WorkFlowStatus = "Too Many Nodes";
                            fileMaster.DoneParsing = 1;
                            fileMaster.ProjectMaster = null;
                            fileMaster.FileTypeExtensionReference = null;

                            await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);

                            continue;
                        }

                        fileMaster.WorkFlowStatus = "Processed";
                        fileMaster.DoneParsing = 1;
                        fileMaster.ProjectMaster = null;
                        fileMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);

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
                stringBuilder.AppendLine("================================================");
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
                stringBuilder.AppendLine("====================================");
                Console.WriteLine("===========================================");
                stringBuilder.AppendLine("\n" + " Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);
                Console.WriteLine("Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();

                var startClasses = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(p => p.MethodStatementId != 0
                                          && p.ProjectId == projectId && p.Processed == 0).ConfigureAwait(false);

                var actionWorkflows = (from s in startClasses where s.MethodStatementId == stmtId select s).ToList().First();
                if (actionWorkflows.FileId == 0) return Ok("Action Workflow processed successfully.");

                stringBuilder.AppendLine("=====================================================");
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpBaseStatementMaster : " + projectId + "," + actionWorkflows);

                string sqlQuery = " SELECT * from statementreferencemaster " +
                                  " Where FileId = " + actionWorkflows.FileId + " AND BaseCommandId = 19;";
                var baseStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery).ConfigureAwait(false);

                stringBuilder.AppendLine("=================================================");
                stringBuilder.AppendLine("\n" + "Started process for get block: GetMethodBlock for statememtId: " + stmtId + ")");
                var workflowRef = await GetMethodBlock(stmtId).ConfigureAwait(false);

                if (!workflowRef.Any()) return Ok("Workflow data collected successfully");

                int currentFileId = actionWorkflows.FileId;
                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == currentFileId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any())
                    return Ok("Workflow data collected successfully");

                var jclMenuTitle = startClasses.First().OriginObject;

                var bId = workflowRef[0].BaseCommandId;
                var statementId = workflowRef[0].StatementId;
                var treeNodeId = 1;
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

                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 5) continue;
                    treeItem.BaseCommandId = 5;
                    treeItem.ProgramId = treeItem.StatementReferenceMaster.ReferenceFileId;
                }
                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    treeItem.MethodCalled = treeItem.StatementReferenceMaster.MethodCalled;
                    treeItem.ProgramId = treeItem.StatementReferenceMaster.ReferenceFileId;
                }

                #region Process for base command id = 5 and 6

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                copyOfLstTreeView = copyOfLstTreeView.IfBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.LoopBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.ElseBlockStatement(copyOfLstTreeView);

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));
                secondTab.AddRange(copyOfLstTreeView
                    .Where(
                        item => item.BaseCommandId == 6
                            || item.BaseCommandId == 8
                            || item.BaseCommandId == 10
                            || item.BaseCommandId == 1
                            || item.BaseCommandId == 25
                            || item.BaseCommandId == 5
                            || item.BaseCommandId == 30
                            || item.BaseCommandId == 3
                            || item.BaseCommandId == 4
                            || item.BaseCommandId == 45));
                var tempList =
                    (from d in secondTab
                     where d.BaseCommandId == 1
                           || d.BaseCommandId == 10
                           || d.BaseCommandId == 25 || d.BaseCommandId == 3
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
                        allSeqListItems = allSeqListItems.AttachChildItems(secondTab, cItem);
                    }
                    break;
                }
                allSeqListItems = allSeqListItems.DistinctBy().ToList();

                #region Process the details

                // ReSharper disable once RedundantAssignment
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>
                    (s => s.MethodStatementId == statementId && s.ProjectId == projectId).ConfigureAwait(false);

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
                    ProgramId = sTab.StatementReferenceMaster.ReferenceFileId,
                    AnnotateStatement = null
                }).ToList();
                var generalRepositorySecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                int actionWorkFlowId = actionWorkflow.First().ActionWorkflowId;
                int methodStartStatementId = actionWorkflow.First().MethodStatementId;
                // Before inserting these records, check whether this table has already data for this action workflow id.
                var generalRepositoryFirstDetails = new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());

                var chkHasResults = await generalRepositoryFirstDetails
                    .GetAllListItems(t => t.ActionWorkflowId == actionWorkFlowId &&
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
                        if (stmt.BaseCommandId != 1 && stmt.BaseCommandId != 5 && stmt.BaseCommandId != 6)
                            continue;
                        stmt.GraphName = stmt.GraphName + "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' " +
                                         "onclick='pseudoCodeDialog(" + stmt.ProgramId + ")'/>";
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
                    ProgramId = fTab.StatementReferenceMaster.ReferenceFileId
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

                var widthCnt = Convert.ToInt32(jclMenuTitle.Length.ToString()) * 3;
                treeView.Nodes.Add(new Node
                {
                    Id = nodeId,
                    Name = jclMenuTitle.ToUpper(),
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

                Console.WriteLine("====================================");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Workflow data collected successfully");
            }
        }

        private async Task<List<StatementReferenceMaster>> GetMethodBlock(int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ConfigureAwait(false);

                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<StatementReferenceMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(workflowRef, settings);
                var listData = JsonConvert.DeserializeObject<List<StatementReferenceMaster>>(json);
                return listData;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessProgramWorkflows(int projectId)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);

                    string sqlQy = "SELECT * FROM FileMaster WHERE FileTypeExtensionId IN(6, 8) " +
                                   " AND ProjectId = " + projectMaster.ProjectId + "; -- AND Processed = 1;";
                    var fileMaster = await _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                        .ConfigureAwait(false);

                    int totalCount = fileMaster.Count;
                    int loopCount = 0;
                    foreach (var copyFile in fileMaster)
                    {
                        loopCount++;
                        Console.WriteLine("Total Workflows for Subroutines to process: " + totalCount);

                        await WorkFlowWorkSpaceForProgram(copyFile).ConfigureAwait(false);

                        Console.WriteLine("Total Remaining Workflows for Subroutines to process: " + (totalCount - loopCount));
                        Console.WriteLine("=============================================================");

                    }
                    return Ok("All Subroutine files are processed successfully for Project: " + projectMaster.ProjectName);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return InternalServerError(exception);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> WorkFlowWorkSpaceForProgram(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                string qry = "SELECT * FROM SecondTabSubroutineProgramDetails " +
                             " WHERE ProjectId = " + fileMaster.ProjectId + " AND ProgramId = " + fileMaster.FileId + ";";
                var subRoutine = await _codeVortoService.SecondTabSubRoutineProgramRepository
                    .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(qry).ConfigureAwait(false);
                if (subRoutine.Any()) return Ok("File already exists");
                var dataDictionarySql =
                    "SELECT * FROM StatementReferenceMaster WHERE FileId = " + fileMaster.FileId + " ";
                var statementReferencesList = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql).ConfigureAwait(false);
                if (!statementReferencesList.Any()) return Ok("Record not found");
                var treeNodeId = 0;

                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + statementReferencesList[0].StatementId,
                    GraphName = "<span class='nodeToBold'>" + statementReferencesList[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = statementReferencesList[0].BaseCommandId,
                    StatementReferenceMaster = statementReferencesList[0],
                    ActualStatementId = "Actual_" + statementReferencesList[0].StatementId,
                    NodeId = ++treeNodeId,
                    IndentLevel = 1,
                    ProgramId = statementReferencesList[0].FileId
                });
                var copyofStatementReferece = statementReferencesList.Skip(1).ToList();
                lstTreeView.AddRange(copyofStatementReferece.Select(statementMaster => new TreeView
                {
                    ActualStatementId = "Actual_" + statementMaster.StatementId,
                    GraphId = "Node_" + statementMaster.StatementId,
                    GraphName = statementMaster.OriginalStatement,
                    HasChild = false,
                    SpriteCssClass = "",
                    ParentId = "MethodNode_" + statementReferencesList[0].StatementId,
                    BaseCommandId = statementMaster.BaseCommandId,
                    PrimaryCommandId = statementMaster.PrimaryCommandId,
                    ClassCalled = statementMaster.ClassCalled,
                    MethodCalled = statementMaster.MethodCalled,
                    StatementReferenceMaster = statementMaster,
                    AlternateName = !string.IsNullOrEmpty(statementMaster.AlternateName)
                        ? statementMaster.AlternateName : statementMaster.BusinessName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2,
                    ProgramId = statementMaster.FileId
                }));

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
                            ProjectId = fileMaster.ProjectId,
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
                                        " ProjectId = " + fileMaster.ProjectId + " AND ProgramId = " + fileMaster.FileId;

                    var statementSecondTab = await _codeVortoService.SecondTabSubRoutineProgramRepository
                        .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(mySqlQuery).ConfigureAwait(false);

                    foreach (var stmt in statementSecondTab)
                    {
                        if (stmt.BaseCommandId != 1 && stmt.BaseCommandId != 5 && stmt.BaseCommandId != 6) continue;
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(stmt).ConfigureAwait(false);
                    }

                    #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine("Issues occured in file: " + fileMaster.FileName);
                    Console.WriteLine("Project Id: " + fileMaster.ProjectId);
                    Console.WriteLine(exception.InnerException);
                    LogMessage.WriteLogMessage(new StringBuilder("Issues occured in file: " + fileMaster.FileName)
                        .Append("Project Id: " + fileMaster.ProjectId));
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                #endregion

                return Ok("SubRoutine or Include workflow data collected successfully");

                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetEntityDetails(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlQry = "SELECT * FROM StatementReferenceMaster WHERE ProjectId = " + projectId + " " +
                                " AND OriginalStatement like '%DSN%';";
                var statementRefMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                foreach (var sRefMaster in statementRefMaster)
                {
                    var originalStatement = sRefMaster.OriginalStatement;
                    Regex regex = new Regex("(DSN=.*?),", RegexOptions.IgnoreCase);
                    var matches = regex.Matches(originalStatement);
                    foreach (Match match in matches)
                    {
                        var entity = match.Groups[1].Value;
                        Console.WriteLine(entity);
                    }
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> PseudoCodeConversion(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId && string.IsNullOrEmpty(x.AlternateName))
                        .ConfigureAwait(false);
                    if (!statementReferenceMaster.Any()) return NotFound();
                    foreach (var sReferenceMaster in statementReferenceMaster)
                    {
                        var alternateName = sReferenceMaster.OriginalStatement.ConversionPesudoCode();
                        if (string.IsNullOrEmpty(alternateName)) continue;
                        sReferenceMaster.AlternateName = alternateName;
                        sReferenceMaster.FileMaster = null;
                        sReferenceMaster.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sReferenceMaster)
                            .ConfigureAwait(false);

                        Console.WriteLine(alternateName);
                    }
                    return Ok("Process of pseudo code conversion is completed successfully.");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ViewSource(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);
                var regEx = new Regex(@"^\*", RegexOptions.IgnoreCase);
                foreach (var fileMaster in fileMasters)
                {
                    var isPresent = await _codeVortoService.ViewSourceMasterRepository
                        .GetAllListItems(x => x.FileId == fileMaster.FileId).ConfigureAwait(false);
                    if (isPresent.Any()) continue;
                    // if (fileMaster.FilePath.EndsWith(".txt") || fileMaster.FilePath.EndsWith(".TXT")) continue;
                    if (!File.Exists(fileMaster.FilePath)) continue;
                    var allLines = File.ReadAllLines(fileMaster.FilePath);
                    var strBuilder = new StringBuilder();
                    foreach (var stmt in allLines)
                    {
                        strBuilder.AppendLine(stmt);
                    }

                    var viewSourceMaster = new ViewSourceMaster
                    {
                        ViewSourceId = 0,
                        FileId = fileMaster.FileId,
                        SourceData = strBuilder.ToString(),
                        ProjectId = fileMaster.ProjectId
                    };
                    await _codeVortoService.ViewSourceMasterRepository.AddNewItem(viewSourceMaster).ConfigureAwait(false);

                    string originalSource = viewSourceMaster.SourceData;

                    if (string.IsNullOrEmpty(originalSource)) originalSource = "";
                    var originalSourceList = originalSource.Split('\n').ToList();
                    originalSourceList = originalSourceList.Select(s => s.Trim()).ToList();
                    originalSourceList.RemoveAll(r => r.Length <= 0);
                    originalSourceList = originalSourceList.CombineAllBrokenLines('_');
                    originalSourceList = originalSourceList.Where(s => !regEx.IsMatch(s)).ToList();
                    originalSourceList = originalSourceList.Select(s => s.CheckCommentInStatement()).ToList();
                    if (!originalSourceList.Any()) continue;
                    // if (fileMaster.FileTypeExtensionId == 10) originalSourceList = originalSourceList.Skip(1).ToList();
                    string sourceWithoutComment = string.Join("\n", originalSourceList);
                    viewSourceMaster.SourceWithoutComments = sourceWithoutComment;

                    await _codeVortoService.ViewSourceMasterRepository.UpdateItem(viewSourceMaster).ConfigureAwait(false);
                }

                Console.WriteLine("=================================================================================\n");
                Console.WriteLine("Source Master Data imported successfully for Project: " + projectMaster.ProjectName);
                Console.WriteLine("=================================================================================\n");

                return Ok("Processed");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForCrudActivity(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var dataDependencies = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(d => d.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                var insertList = new List<string> { " INSERT ", "WRITE " };
                var updateList = new List<string> { " UPDATE ", "REWRITE " };
                var deleteList = new List<string> { " DELETE " };
                var selectList = new List<string> { " SELECT ", "OPEN ", "READ " };

                foreach (var dataDependency in dataDependencies)
                {
                    // if (dataDependency.FileId != 39) continue;
                    string entityName = dataDependency.Entity;

                    #region Added into DbCrudActivityTable

                    string iR = "N";
                    string update = "N";
                    string delete = "N";
                    string select = "N";

                    var statementList = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == dataDependency.FileId && s.OriginalStatement.Contains(entityName)
                                              && s.ProjectId == projectMaster.ProjectId && s.BaseCommandId == 45)
                        .ConfigureAwait(false);

                    if (!statementList.Any()) continue;

                    foreach (string sss in statementList.Select(s => s.OriginalStatement))
                    {
                        if (!sss.Contains(entityName)) continue;
                        if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                        if (updateList.Any(c => sss.Contains(c))) update = "Y";
                        if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                        if (selectList.Any(c => sss.Contains(c))) select = "Y";
                    }
                    var dbCurdActivity = new DbCrudActivity
                    {
                        EntityName = entityName,
                        SelectOrRead = select,
                        InsertOrCreate = iR,
                        Update = update,
                        Delete = delete,
                        FileId = dataDependency.FileId,
                        ProjectId = projectMaster.ProjectId
                    };
                    var crud = await _codeVortoService.DbCrudActivityRepository
                        .GetAllListItems(x => x.EntityName == dbCurdActivity.EntityName && x.FileId == dbCurdActivity.FileId
                                              && x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);
                    if (crud.Any()) continue;

                    await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                    #endregion
                }
                return Ok("CRUD Activity report process completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForDataInventory(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectMaster == null) return BadRequest();
                    var sqlQry = "SELECT * FROM DataDependency WHERE ProjectId = " + projectMaster.ProjectId +
                                 " GROUP BY Entity;";
                    var entityObject = await _codeVortoService.DataDependencyRepository
                        .GetDataFromSqlQuery<DataDependency>(sqlQry).ConfigureAwait(false);

                    var sqlQuery = "SELECT * FROM DataDependency WHERE ProjectId = " + projectMaster.ProjectId + ";";

                    var dataDependancy = await _codeVortoService.DataDependencyRepository
                        .GetDataFromSqlQuery<DataDependency>(sqlQuery).ConfigureAwait(false);

                    int cCount = 0;

                    foreach (var eObject in entityObject)
                    {
                        var dataDependancyList = new List<string>();
                        var dataDependant = (from d in dataDependancy where d.Entity == eObject.Entity select d).ToList();
                        cCount++;
                        var fileIds = new List<int>();
                        foreach (var dDependency in dataDependant)
                        {
                            var exist = fileIds.Exists(x => x == dDependency.FileMaster.FileId);
                            if (exist) continue;
                            fileIds.Add(dDependency.FileMaster.FileId);

                            var str =
                                "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                " onclick='includeStateDialog(" + dDependency.FileMaster.FileId + ");'>" +
                                dDependency.FileMaster.FileName + "</a>";
                            dataDependancyList.Add(str);
                        }
                        var nList = new List<string>();
                        foreach (var d in dataDependancyList)
                        {
                            var aaa = "<li>" + d + "</li>";
                            nList.Add(aaa);
                        }
                        var dDependancy = nList.Any() ? string.Join(" ", nList) : "-";
                        var dDependancyFinal = nList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(entityCalledFrom_" + cCount +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              nList.Count + " Called From </a> <div style='display: none;' id='entityCalledFrom_" +
                              cCount + "'>" + dDependancy + " </div> "
                            : "-";
                        var entityName = "<a href='javascript:void(0);' title='" + eObject.Entity + "' itemprop='" +
                                         eObject.Entity +
                                         "' onclick='showEntitySchema(this);' style='color: blue; font-size: 14px; text-decoration: underline;'> " +
                                         eObject.Entity + "</a>";
                        var dataInventory = new DataInventory
                        {
                            ObjectName = entityName,
                            ObjectType = "Entity",
                            ExternalCall = "-",
                            CalledFrom = dDependancyFinal,
                            UsesEntities = "-",
                            ObjectDescription = "-",
                            ProjectId = projectId,
                            StatementInList = "-"
                        };
                        await _codeVortoService.DataInventoryRepository.AddNewItem(dataInventory);
                    }
                    return Ok("Data Inventory processed successfully.");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBaseCommandId6ForCopyBook(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    var fileMasters = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId && x.FileTypeExtensionId == 4)
                        .ConfigureAwait(false);
                    if (projectMaster == null) return NotFound();
                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.OriginalStatement.Contains("COPY") && x.BaseCommandId == 6
                        && x.ReferenceFileId == 0 && x.ProjectId == projectId).ConfigureAwait(false);

                    if (!statementReferenceMaster.Any()) return Ok();

                    foreach (var statementReference in statementReferenceMaster)
                    {
                        var oStatement = statementReference.OriginalStatement;
                        var regexInput = new Regex(@"^COPY\s*(\w*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                        if (!regexInput.IsMatch(oStatement)) continue;
                        var match = regexInput.Match(oStatement);
                        var copyFileName = match.Groups[1].Value;
                        var filemaster = fileMasters.Find(f => Path.GetFileNameWithoutExtension(f.FilePath) == copyFileName);
                        Console.WriteLine(filemaster);
                        statementReference.BaseCommandId = 6;
                        statementReference.FileMaster = null;
                        statementReference.ReferenceFileId = filemaster?.FileId ?? 0;
                        statementReference.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference)
                            .ConfigureAwait(false);
                    }

                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception);
                }
            }
            return Ok("Updated Base Command Id for Run Program and InputLib Statements.");

        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBaseCommandId6ForInputLibAndRunProgramStatement(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectMaster == null) return NotFound();
                    var clsCobolProcess = new ClsCobolProcess();
                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => (x.OriginalStatement.Contains("INPUTLIB")
                        || x.OriginalStatement.Contains("RUN PROGRAM")) && x.ReferenceFileId == 0
                        && x.ProjectId == projectId).ConfigureAwait(false);

                    var copyOfFileMasters = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);


                    if (!statementReferenceMaster.Any()) return Ok();

                    foreach (var statementReference in statementReferenceMaster)
                    {
                        var oStatement = statementReference.OriginalStatement;
                        var regexInput = new Regex(@"INPUTLIB\(([^)]*)\)|RUN\s*PROGRAM\s*\((.*?)\)",
                            RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                        if (!regexInput.IsMatch(oStatement)) continue;
                        var regexInputPatteren = new Regex(@"INPUTLIB\(([^)]*)\)",
                            RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

                        var regexRunProgram = new Regex(@"RUN\s*PROGRAM\s*\((.*?)\)",
                            RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

                        int fileTypeExtensionId = regexInputPatteren.IsMatch(oStatement) ? 19 : 6;
                        var match = regexInput.Match(oStatement);
                        string inputLib = match.Groups[1].Value;
                        string runProgram = match.Groups[2].Value;
                        var pgmName = !string.IsNullOrEmpty(inputLib) ? inputLib : runProgram;
                        if (string.IsNullOrEmpty(pgmName)) continue;
                        var objectName = pgmName.Trim();
                        var fileMaster = clsCobolProcess.GetCurrentFile(objectName, fileTypeExtensionId, copyOfFileMasters);
                        if (fileMaster != null)
                            statementReference.BaseCommandId = 6;
                        statementReference.FileMaster = null;
                        statementReference.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference)
                            .ConfigureAwait(false);
                    }
                    /*
                    foreach (var statementReference in statementReferenceMaster)
                    {
                        var oStatement = statementReference.OriginalStatement;
                        var regexInput = new Regex(@"INPUTLIB\(([^)]*)\)|RUN\s*PROGRAM\s*\((.*?)\)",
                            RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                        if (!regexInput.IsMatch(oStatement)) continue;
                        var regexInputPatteren = new Regex(@"INPUTLIB\(([^)]*)\)",
                            RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

                        //var regexRunProgram = new Regex(@"RUN\s*PROGRAM\s*\((.*?)\)",
                        //    RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

                        int fileTypeExtensionId = regexInputPatteren.IsMatch(oStatement) ? 19 : 6;
                        var match = regexInput.Match(oStatement);
                        string inputLib = match.Groups[1].Value;
                        string runProgram = match.Groups[2].Value;
                        var pgmName = !string.IsNullOrEmpty(inputLib) ? inputLib : runProgram;
                        if (string.IsNullOrEmpty(pgmName)) continue;
                        var objectName = pgmName.Trim();
                        var fileMaster = GetCurrentFile(objectName, fileTypeExtensionId, copyOfFileMasters);

                        statementReference.BaseCommandId = 6;
                        statementReference.FileMaster = null;
                        statementReference.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference)
                            .ConfigureAwait(false);

                        if (fileMaster == null) continue;

                        var pNameNew = projectMaster.ProjectName;
                        var pPathNew = projectMaster.PhysicalPath;
                        var className = fileMaster.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(fileMaster.FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);

                        var classNameDeclared = pNameNew + "." + className + fileName;
                        statementReference.ClassCalled = classNameDeclared;
                        statementReference.MethodCalled = fileName + "()";
                        statementReference.ReferenceFileId = fileMaster.FileId;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference)
                            .ConfigureAwait(false);
                    }
                    */
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception);
                }
            }
            return Ok("Updated Base Command Id for Run Program and InputLib Statements.");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateCallExternalForIncludeMemberStmt(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var statementRefernceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId &&
                                              x.OriginalStatement.Contains("INCLUDE MEMBER")).ConfigureAwait(false);
                    var regex = new Regex(@"^INCLUDE\s+MEMBER\s*=(\s*[A-z0-9]+)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                    foreach (var statementReference in statementRefernceMaster)
                    {
                        var oStatement = statementReference.OriginalStatement;
                        if (string.IsNullOrEmpty(oStatement)) continue;
                        if (!regex.IsMatch(oStatement)) continue;
                        statementReference.BaseCommandId = 6;
                        statementReference.FileMaster = null;
                        statementReference.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference)
                            .ConfigureAwait(false);
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception);
                }
                return Ok("Updated Base Command Id for Include Member Statements.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ViewSource(int projectId, string opt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);
                var regEx = new Regex(@"^\*", RegexOptions.IgnoreCase);
                foreach (var fileMaster in fileMasters)
                {
                    // if (fileMaster.FilePath.EndsWith(".txt") || fileMaster.FilePath.EndsWith(".TXT")) continue;
                    if (!File.Exists(fileMaster.FilePath)) continue;
                    var allLines = new List<string>(File.ReadAllLines(fileMaster.FilePath));
                    var strBuilder = new StringBuilder();
                    foreach (var stmt in allLines)
                    {
                        strBuilder.AppendLine(stmt);
                    }

                    var viewSourceMaster = new ViewSourceMaster
                    {
                        ViewSourceId = 0,
                        FileId = fileMaster.FileId,
                        SourceData = strBuilder.ToString(),
                        ProjectId = fileMaster.ProjectId
                    };
                    await _codeVortoService.ViewSourceMasterRepository.AddNewItem(viewSourceMaster).ConfigureAwait(false);

                    string originalSource = viewSourceMaster.SourceData;
                    if (string.IsNullOrEmpty(originalSource)) originalSource = "";
                    var originalSourceList = originalSource.Split('\n').ToList();

                    originalSourceList = originalSourceList.PrepareSameLength();
                    originalSourceList = originalSourceList.RemoveCharacter(6, 66);
                    string[] comments = { "*", "/" };
                    originalSourceList = originalSourceList.RemoveAllCommentedLines(comments);
                    originalSourceList = originalSourceList.RemoveAll("SKIP", "EJECT");
                    originalSourceList = originalSourceList.RemoveEmptyLines();
                    originalSourceList = originalSourceList.Select(s => s.Trim()).ToList();
                    originalSourceList.RemoveAll(r => r.Length <= 0);
                    originalSourceList = originalSourceList.CombineAllBrokenLines('_');
                    originalSourceList = originalSourceList.Where(s => !regEx.IsMatch(s)).ToList();
                    originalSourceList = originalSourceList.Select(s => s.CheckCommentInStatement()).ToList();
                    if (!originalSourceList.Any()) continue;
                    // if (fileMaster.FileTypeExtensionId == 10) originalSourceList = originalSourceList.Skip(1).ToList();
                    string sourceWithoutComment = string.Join("\n", originalSourceList);
                    viewSourceMaster.SourceWithoutComments = sourceWithoutComment;

                    await _codeVortoService.ViewSourceMasterRepository.UpdateItem(viewSourceMaster).ConfigureAwait(false);
                }

                Console.WriteLine("=================================================================================\n");
                Console.WriteLine("Source Master Data imported successfully for Project: " + projectMaster.ProjectName);
                Console.WriteLine("=================================================================================\n");

                return Ok("Processed");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForCrudActivity(int projectId, string opt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var dataDependencies = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(d => d.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                var copyFileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => x.ProjectId == projectId && x.FileTypeExtensionId == 4).ConfigureAwait(false);

                var insertList = new List<string> { " INSERT ", "INSERT ", "WRITE " };
                var updateList = new List<string> { " UPDATE ", "UPDATE ", "REWRITE " };
                var deleteList = new List<string> { " DELETE ", "DELETE " };
                var selectList = new List<string> { " SELECT ", "SELECT ", "OPEN ", "READ " };
                var clsCobolProcess = new ClsCobolProcess();
                foreach (var dataDependency in dataDependencies)
                {
                    string entityName = dataDependency.Entity;

                    #region Added into DbCrudActivityTable

                    string iR = "N";
                    string update = "N";
                    string delete = "N";
                    string select = "N";

                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(x => x.FileId == dataDependency.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var newStatement = viewSourceMaster.SourceWithoutComments.Split('\n').ToList();
                    if (!newStatement.Any()) continue;

                    var includeStatements = (from n in newStatement
                                             where n.StartsWith("INCLUDE ", true, CultureInfo.InvariantCulture) ||
                                             n.StartsWith("COPY ", true, CultureInfo.InvariantCulture)
                                             select n).ToList();

                    foreach (var includeStatement in includeStatements)
                    {
                        var regexInclude = new Regex(@"^INCLUDE\s+([A-z0-9\-]+)|^COPY\s+([A-z0-9\-]+)");
                        if (!regexInclude.IsMatch(includeStatement)) continue;
                        var matchCopyFile = regexInclude.Match(includeStatement);
                        var includeStmt = matchCopyFile.Groups[1].Value;
                        var copyStmt = matchCopyFile.Groups[2].Value;
                        var objName = !string.IsNullOrEmpty(includeStmt) ? includeStmt : copyStmt;
                        if (string.IsNullOrEmpty(objName)) continue;
                        objName = objName.Trim();
                        var copyFilePath = clsCobolProcess.GetCurrentFile(objName, 4, copyFileMaster);
                        if (copyFilePath != null) continue;
                        var missingObject = new MissingObjects
                        {
                            ProjectId = projectId,
                            FileId = dataDependency.FileId,
                            FromObject = dataDependency.FileMaster.FileName,
                            Statement = includeStatement,
                            Type = "CopyBook",
                            CalledObjectName = objName
                        };
                        var missingObjectExist = await _codeVortoService.MissingObjectsRepository
                            .GetAllListItems(x => x.ProjectId == projectId &&
                                                  x.FileId == missingObject.FileId &&
                                                  x.FromObject == missingObject.FromObject &&
                                                  x.CalledObjectName == missingObject.CalledObjectName)
                            .ConfigureAwait(false);
                        if (missingObjectExist.Any()) continue;
                        await _codeVortoService.MissingObjectsRepository.AddNewItem(missingObject)
                            .ConfigureAwait(false);
                    }

                    bool isOpen = false;
                    string openStatement = string.Empty;
                    foreach (string sss in newStatement)
                    {
                        if (!sss.Contains(entityName)) continue;
                        if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                        if (updateList.Any(c => sss.Contains(c))) update = "Y";
                        if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                        if (selectList.Any(c => sss.Contains(c))) select = "Y";
                        if (sss.StartsWith("OPEN ")) isOpen = true;
                        if (sss.StartsWith("OPEN ")) openStatement = sss;
                    }
                    var dbCurdActivity = new DbCrudActivity
                    {
                        EntityName = entityName,
                        SelectOrRead = select,
                        InsertOrCreate = iR,
                        Update = update,
                        Delete = delete,
                        FileId = dataDependency.FileId,
                        ProjectId = projectMaster.ProjectId,
                        IsOpen = isOpen,
                        OpenStatement = openStatement,
                        ReferenceObjectName = ""
                    };
                    var crud = await _codeVortoService.DbCrudActivityRepository
                        .GetAllListItems(x => x.EntityName == dbCurdActivity.EntityName && x.FileId == dbCurdActivity.FileId
                                              && x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);
                    if (crud.Any()) continue;

                    await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                    #endregion
                }
                return Ok("CRUD Activity report process completed successfully.");
            }
        }

        [HttpGet]
        public IHttpActionResult ChangeFileExtensions(ProjectMaster projectMaster)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    var directories = Directory.GetDirectories(projectMaster.PhysicalPath);
                    foreach (var directory in directories)
                    {
                        var dirInfo = new DirectoryInfo(directory);
                        var lastFolderName = dirInfo.Name;
                        if (string.IsNullOrEmpty(lastFolderName)) continue;
                        var response = false;

                        if (lastFolderName.Contains("JCL") || lastFolderName.Contains("jcl"))
                            response = ChangeExtensions(directory, ".jcl", "", "jcl");

                        if (lastFolderName.Contains("Cobol") || lastFolderName.Contains("COBOL"))
                            response = ChangeExtensions(directory, ".cbl", "", "Cobol", "COBOL");

                        if (lastFolderName.Contains("Copybook") || lastFolderName.Contains("Copybooks"))
                            response = ChangeExtensions(directory, ".copy", "", "CopyBook", "CopyBooks");


                        if (lastFolderName.Contains("INPUTLIB") || lastFolderName.Contains("InputLib"))
                            response = ChangeExtensions(directory, ".lib", "", "InputLib");


                        if (lastFolderName.Contains("Proc") || lastFolderName.Contains("Procs"))
                            response = ChangeExtensions(directory, ".proc", "", "Proc", "Procs");

                        if (lastFolderName.Contains("BMS") || lastFolderName.Contains("Bms"))
                            response = ChangeExtensions(directory, ".bms", "", "Bms");

                        Console.WriteLine("Change Extensions " + lastFolderName + " : " + response);

                    }



                    return Ok();

                }

            }
            catch (Exception exception)
            {
                return InternalServerError(exception);
            }

        }

        [HttpGet]
        private bool ChangeExtensions(string rootPath, string extension, string dToSkip, params string[] directoryName)
        {
            dToSkip = dToSkip ?? "";
            var directoriesToSkip = dToSkip.Split(',').ToList();
            var directories = Directory.GetDirectories(rootPath, "*.*", SearchOption.AllDirectories).ToList();
            var files = Directory.GetFiles(rootPath, "*.*", SearchOption.AllDirectories);
            if (directories.Any())
                directories.AddRange(files);
            directoriesToSkip.RemoveAll(string.IsNullOrEmpty);


            var dirToProcess = (from d in directories
                                let dir = new DirectoryInfo(d)
                                where null != dir &&
                                      directoryName.Any(n => Regex.IsMatch(n, dir.Name, RegexOptions.IgnoreCase))
                                select d).ToList();
            if (!dirToProcess.Any())
            {
                var rootFiles = Directory.GetFiles(rootPath, "*.*").ToList();
                foreach (var file in rootFiles)
                {
                    string fileExtension = Path.GetExtension(file);
                    if (fileExtension == extension) continue;

                    var newFile = File.ReadAllLines(file);
                    var newFileWithExtension = Path.ChangeExtension(file, extension);
                    if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                    File.WriteAllLines(newFileWithExtension, newFile);
                    File.Delete(file);
                }
                Console.WriteLine(rootPath);
            }
            foreach (var directory in dirToProcess)
            {
                var currentDirectories = Directory.GetDirectories(directory, "*.*", SearchOption.AllDirectories);

                foreach (var cDir in currentDirectories)
                {
                    var allFiles = Directory.GetFiles(cDir, "*.*", SearchOption.AllDirectories);
                    foreach (var file in allFiles)
                    {
                        string fileExtension = Path.GetExtension(file);
                        if (fileExtension == extension) continue;
                        var newFile = File.ReadAllLines(file);
                        var newFileWithExtension = Path.GetFileNameWithoutExtension(file) + extension;
                        if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                        File.WriteAllLines(newFileWithExtension, newFile);
                        // File.Delete(file);
                    }
                }

                var rootFiles = Directory.GetFiles(directory, "*.*").ToList();
                foreach (var file in rootFiles)
                {
                    string fileExtension = Path.GetExtension(file);
                    if (fileExtension == extension) continue;

                    var newFile = File.ReadAllLines(file);
                    // var newFileWithExtension = file + extension;
                    var newFileWithExtension = Path.ChangeExtension(file, extension);
                    if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                    File.WriteAllLines(newFileWithExtension, newFile);
                    File.Delete(file);
                }
                Console.WriteLine(directory);
            }


            return true;
        }

        [HttpGet]
        public List<string> ConversionOfExecStatements(List<string> allLines)
        {
            //var allLines =
            //    File.ReadAllLines(@"C:\inetpub\wwwroot\flokapture\ExtractedProjects\COBOL_Demo\COBOL\dtfm164.cbl")
            //        .ToList();
            var mainBlock = new List<string>();
            if (allLines.Count <= 0) return mainBlock;
            var regex = @"^EXEC CICS RETURN|^EXEC CICS XCTL";
            foreach (string currentLine in allLines)
            {

                if (currentLine.StartsWith("EXEC CICS HANDLE AID"))
                {
                    string[] toStr = { "AID" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1];
                    var finalStatement = moveState.Split(' ');
                    foreach (var statement in finalStatement)
                    {
                        if (statement == "END-EXEC." || statement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(statement)) continue;
                        string[] state = statement.Split('(');
                        var mainLine = "IF " + state[0] + " IS PRESSED";
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall);
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }

                if (currentLine.StartsWith("EXEC CICS HANDLE CONDITION"))
                {
                    string[] toStr = { "CONDITION" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1].Trim();
                    var finalStatement = moveState.Split(')');
                    int finalStatCount = finalStatement.Length - 1;
                    int index = 0;
                    foreach (var statement in finalStatement)
                    {
                        var newStatement = statement.Trim();
                        index++;
                        if (newStatement == "END-EXEC." || newStatement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(newStatement)) continue;
                        var ifState = index == 1 ? "IF " : "ELSE-IF ";
                        if (finalStatCount > 1)
                        {
                            if (index == finalStatCount)
                            {
                                ifState = "ELSE ";
                            }
                        }
                        string[] state = newStatement.Split('(');
                        var mainLine = ifState + "CONDITION IS " + state[0];
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall.Trim());
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }

                if (Regex.IsMatch(currentLine, regex))
                {
                    var regexPattern = "@EXEC CICS\\s+[A-z0-9\\-]\\S+(\\s+[A-z0-9\\-]\\S+)";
                    var matches = Regex.Match(currentLine, regexPattern, RegexOptions.IgnoreCase);
                    var statementMatch = matches.Groups[1].Value;
                    Console.WriteLine(statementMatch);

                }
                mainBlock.Add(currentLine);

            }
            return mainBlock;
        }
    }
}
