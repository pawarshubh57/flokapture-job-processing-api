using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingController 
    {
        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActioWorkflows(int projectId, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Added the Action workflow table

                if (string.IsNullOrEmpty(fileMaster.FileName) || fileMaster.FileId == 0)
                    return Ok("Action Workflows processed successfully");
                // Action Workflows data...
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var projectType = projectMaster.ProjectConfigType;
                if (projectType != 4) return Ok("Action Workflows processed successfully");
                var projectCongfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectConfigData = projectCongfig.GetItem(projectType);
                if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                try
                {
                    var allProcessedWorkflows = await _codeVortoService.ActionWorkflowsRepository
                        .GetAllListItems(r => r.ProjectId == projectId && r.Processed == 1);

                    object[] param =
                    {
                        new MySqlParameter("@pjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileMaster.FileId}
                    };

                    var genericBlocks =
                        await
                            _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);
                    foreach (var statement in genericBlocks)
                    {
                        if (allProcessedWorkflows.Any(s => statement.StatementId == s.MethodStatementId)) continue;

                        var actionWorkflow = new ActionWorkflows
                        {
                            ActionWorkflowId = 0,
                            CreatedBy = 1,
                            EndPointOrService = "Batch",
                            MethodStatementId = statement.StatementId,
                            OriginFileName = Path.GetFileName(fileMaster.FilePath),
                            OriginFilePath = fileMaster.FilePath,
                            ProjectId = projectId,
                            OriginObject = projectMaster.ProjectName + fileMaster.FilePath.Replace(".jcl", "")
                                    .Replace(projectMaster.PhysicalPath, "").Trim().Replace("\\", "."),
                            WorkflowName = statement.OriginalStatement,
                            ServiceBaseAddress = null,
                            ServiceContract = null,
                            WorkflowBusinessName = statement.BusinessName,
                            IsDeleted = 0,
                            ReasonAboutDisable = null,
                            Processed = 0,
                            FileId = statement.FileId
                        };
                        await
                            _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow)
                                .ConfigureAwait(false);
                    }
                }

                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
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
                        if (workflow.ProjectId == null) return NotFound();
                        var workFlowProjectId = workflow.ProjectId ?? 0;
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
                        workflow.FileMaster = null;
                        workflow.Processed = 1;
                        workflow.ProjectMaster = null;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow).ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }
                // ApplyPseudoCodeConversion(projectId);
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
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;
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
    }
}
