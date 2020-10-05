

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.ActionFilters;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingController
    {
        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniverseProgramWorkflows(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var allPrograms = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.FileTypeExtensionId == 6 && f.FileTypeExtensionId == 8).ConfigureAwait(false);
                    var toProcessPrograms = allPrograms
                        .Where(p => string.IsNullOrEmpty(p.WorkFlowStatus) && p.Processed == 1).ToList();

                    foreach (var fileMaster in toProcessPrograms)
                    {
                        var master = fileMaster;
                        int solutionId = master.SolutionId ?? 0;
                        var allClassStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.BaseCommandId == 19 && s.FileId == master.FileId)
                            .ConfigureAwait(false);
                        if (!allClassStatements.Any()) continue;
                        string programMethod = Path.GetFileNameWithoutExtension(fileMaster.FilePath) + "()";
                        string className = allClassStatements.First().ClassNameDeclared;
                        var allMethods = await GetAllMethodsForClass(className, solutionId).ConfigureAwait(false);

                        foreach (var statementReference in allMethods)
                        {
                            if (statementReference.MethodName != programMethod) continue;

                            await GetWorkFlowWorkSpaceTooManyNodes(fileMaster, fileMaster.ProjectId, className,
                                statementReference.MethodName).ConfigureAwait(false);

                            break;
                        }

                        fileMaster.WorkFlowStatus = "Workflow processed successfully";
                        fileMaster.DoneParsing = 1;
                        fileMaster.ProjectMaster = null;
                        fileMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);
                    }

                    return Ok("Done");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    return InternalServerError(exception);
                }
            }
        }


        [HttpGet]
        [ActionExceptionFilter]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceTooManyNodes(FileMaster fileMaster, int projectId, string className, string methodName)
        {
            var stringBuilder = new StringBuilder();
            // var universeProcessingHelper = new UniverseProcessingHelper();
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                stringBuilder.AppendLine("===============================================");
                stringBuilder.AppendLine("Started collecting all GetAllMethodsForClass for solution: " + solutionId +
                                             " and className is: " + className + "");
                int thisProgramId = fileMaster.FileId;

                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == thisProgramId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any()) return Ok("Workflow data collected successfully");

                var allMethodsOfClass = await GetAllMethodsForClass(className, solutionId).ConfigureAwait(false);

                var hasAnyMethod = allMethodsOfClass.Any(t => t.MethodName == methodName);
                if (!hasAnyMethod) return Ok("Workflow data collected successfully");

                int methodStatementId = allMethodsOfClass.First(t => t.MethodName == methodName).StatementId;

                // Check whether this file already processed or not. If yes, then do not process it again.
                // We will just check this in first tabs data...
                Console.WriteLine("======================================");
                Console.WriteLine("Started processing for ProgramId (FileId): " + fileMaster.FileId);
                Console.WriteLine("Started processing for ProgramId (File Name): " + fileMaster.FileName);
                stringBuilder.AppendLine("=====================================================");
                stringBuilder.AppendLine("Started collecting all GetMethodBlock for  methodStatementId: " + methodStatementId);

                var workflowRef = await GetMethodBlock(methodStatementId).ConfigureAwait(false);

                var statementId = workflowRef[0].StatementId;
                var lstTreeView = new List<TreeView>();
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

                int cnt = 165000;

                #region Process the details

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                #region Process for base command id = 5 and 6

                var auto = 0;
                int indentLevel = 3;
                var callingAndCalled = new Dictionary<string, List<string>>();

                var lstAllCalledMethods = new List<NameValue>();
                foreach (var treeItem in lstTreeView)
                {
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;

                    auto++;
                    stringBuilder.AppendLine("==========================================");
                    stringBuilder.AppendLine("Started process GetCallInternalDetails for projectId: " +
                                                        projectId + ", and fileId is :" + treeItem.StatementReferenceMaster.FileId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, 0,
                        treeItem.StatementReferenceMaster.FileId, indentLevel, lstAllCalledMethods, ref auto, ref treeNodeId,
                        ref callingAndCalled, cnt);

                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    int referenceFileId = treeItem.StatementReferenceMaster.ReferenceFileId;
                    if (referenceFileId == 0) continue;
                    var referenceFile = _codeVortoService.FileMasterRepository.GetItem(referenceFileId);
                    if (referenceFile == null) continue;
                    string extProgramName = Path.GetFileNameWithoutExtension(referenceFile.FilePath);
                    var neLink = treeItem.GraphName +
                                 "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                 referenceFile.FileId + ");'>[ " + extProgramName + " ]</a>";

                    treeItem.GraphName = neLink;
                }

                if (copyOfLstTreeView.Count >= cnt)
                {
                    var flMaster = await _codeVortoService.FileMasterRepository.GetItem<FileMaster>(f => f.FileId == thisProgramId,
                            thisProgramId).ConfigureAwait(false);
                    if (flMaster == null) return Ok("Too Many Nodes");

                    flMaster.WorkFlowStatus = "Too many nodes to this program. Skipped for processing workflow.";
                    flMaster.Processed = 4;
                    flMaster.FileTypeExtensionReference = null;
                    flMaster.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(flMaster).ConfigureAwait(false);

                    return Ok("Too Many Nodes");
                }

                #endregion

                copyOfLstTreeView = copyOfLstTreeView.AssignColorsToMethodBlocks();
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();
                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                // Same GROUP(Our Own Group) elements then and then we can have chaining
                // as it CONSIDERS elements from same GROUP...
                copyOfLstTreeView = copyOfLstTreeView.IfBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.LoopBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.ElseBlockStatement(copyOfLstTreeView);

                #region

                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();
                var secondTab = new List<TreeView> { lstTreeView.ElementAt(0), lstTreeView.ElementAt(1) };

                secondTab.AddRange(copyOfLstTreeView
                    .Where(item => item.BaseCommandId == 6
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

                /*
                foreach (var sTab in secondTab)
                {
                    if(sTab.BaseCommandId != 5) continue;
                    string methodCalled = sTab.MethodCalled;

                }
                */
                #endregion

                #region This is to insert records in Nodes and Links Details...

                /*
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
                        allSeqListItems = allSeqListItems.AttachChildItems(secondTab, cItem);
                    }
                    break;
                }

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

                var firstItem = allSeqListItems.First();
                var methodChildItems = (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a)
                    .Distinct().ToList();

                stringBuilder.AppendLine("===============================================");
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
                            ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
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
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] ",
                            ProgramId = curItem.ProgramId
                        });
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                        stringBuilder.AppendLine("====================================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsIf for projectId: " + projectId);
                        foreach (var cItem in childItems)
                        {
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem,
                                treeView, allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);
                        }
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
                                    master.BaseCommandId == 19 && master.ProjectId == projectId &&
                                    (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);
                                classNameDeclared = allClassNameDeclaredAndClassCalledList.AsQueryable().Where(expressionNew).ToList();
                                classNameDeclared = classNameDeclared.ToList();
                            }
                            if (classNameDeclared.Count != 0) nodeColor = "#00ffff";

                            nodeId++;
                            Node node;
                            if (curItem.PrimaryCommandId == 25 || curItem.PrimaryCommandId == 38)
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
                                              where s.ParentId == curItem.GraphId
                                                    && s.BaseCommandId != 25
                                              select s).ToList().Distinct();
                            stringBuilder.AppendLine("================================================");
                            stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " + projectId);
                            foreach (var cItem in childItems)
                            {
                                treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems,
                                    cItem, treeView, allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);
                            }
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

                        if (methodCalled.Count != 0) nodeColor = "#00ffff";

                        nodeId++;
                        Node node;
                        if (curItem.PrimaryCommandId == 23 || curItem.PrimaryCommandId == 36)
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
                        if (curItem.PrimaryCommandId == 23 || curItem.PrimaryCommandId == 36)
                            m = curItem.StatementReferenceMaster.MethodName;
                        else
                            m = curItem.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
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
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] ",
                                ProgramId = curItem.ProgramId
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab
                             where s.ParentId == curItem.GraphId && s.BaseCommandId != 25
                             select s)
                                .ToList().Distinct();
                        stringBuilder.AppendLine("=========================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt for projectId: " + projectId);
                        foreach (var cItem in childItems)
                        {
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, secondTab, cItem,
                                treeView, allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);
                        }

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 5)
                    {
                        #region BaseCommandId == 5

                        var item = curItem;
                        var nodeColor = "#c0c0c0";
                        var methodCalled = allClassNameDeclaredAndClassCalledList
                            .Where(s => s.FileId == curItem.StatementReferenceMaster.FileId && s.BaseCommandId == 19).ToList();

                        if (methodCalled.Count != 0) nodeColor = "#00ffff";
                        nodeId++;
                        Node node;
                        if (curItem.PrimaryCommandId == 24 || curItem.PrimaryCommandId == 37)
                        {
                            string[] strcCalled = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                            string strName = "";
                            if (strcCalled.Length > 2) strName = strcCalled[strcCalled.Length - 2] + "." + strcCalled[strcCalled.Length - 1];
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
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).Distinct().ToList();
                        stringBuilder.AppendLine("===============================================");
                        stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal for projectId: " + projectId);
                        foreach (var cItem in childItems)
                        {
                            treeView = universeProcessingHelper.ProcessChildItemsIf(projectId, allSeqListItems, cItem,
                                treeView, allClassNameDeclaredAndClassCalledList, ref nodeId, ref linkSeqNumber);
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
                stringBuilder.AppendLine("=================================================");
                stringBuilder.AppendLine("Started process for RemoveMultipleLinks for projectId: " + projectId);
                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                stringBuilder.AppendLine("=====================================================");
                stringBuilder.AppendLine("Started process for RemoveHangingNodes for projectId: " + projectId);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());

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
                    if (stmt.ShapeId != "Decision" && stmt.ShapeId != "Decision2") continue;
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

                */
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
                    if (stmt.BaseCommandId == 1 || stmt.BaseCommandId == 5 || stmt.BaseCommandId == 6)
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
                    ActionWorkflowId = 0,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild.ToString(),
                    MethodCalled = fTab.MethodCalled,
                    ParentId = fTab.ParentId,
                    PrimaryCommandId = fTab.PrimaryCommandId,
                    SpriteCssClass = fTab.SpriteCssClass,
                    WorkflowStartStatementId = 0,
                    IndentLevel = fTab.IndentLevel,
                    ProgramId = fTab.ProgramId
                }).ToList();

                var generalRepositoryTreeviewTabFirstDetails =
                    new GeneralRepository<FirstTabProgramDetails>(new AppDbContext());
                await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);

                #endregion

                return Ok("Workflow data collected successfully");

                #endregion
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

                    var graphName = GetIncludeStatement(treeView.StatementReferenceMaster.ProjectId, fileId, methodName).Result;
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
                    stringBuilder.AppendLine("\n" + "Started process for GetGenericBlock(" + stmtMaster[0].StatementId + ", 8, 9)");
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
                        var calledGoSubs = (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        callingAndCalled.Add(methodName, calledGoSubs);

                        foreach (var block in callExtExpandedCode)
                        {
                            autoInt++;
                            if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
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
                                if (stmtMaster[0].MethodName.Trim() == block.MethodName)
                                {
                                    string businessName = stmtMaster[0].BusinessName;
                                    if (!string.IsNullOrEmpty(businessName))
                                        treeView.GraphName = treeView.GraphName + " - " + stmtMaster[0].BusinessName;
                                }

                                if (stmtMaster[0].MethodName.Trim() != block.MethodName)
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
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return lstTreeView;
            }
        }

        private static async Task<List<StatementReferenceMaster>> GetAllMethodsForClass(string className, int solutionId)
        {
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar) {Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className},
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId}
                };
                var callExtExpandedCode = await codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUbGetAllMethodsForClass", parametersExp)
                    .ConfigureAwait(false);
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<StatementReferenceMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(callExtExpandedCode, settings);
                var listData = JsonConvert.DeserializeObject<List<StatementReferenceMaster>>(json);
                return listData;
            }
        }

        [HttpGet]
        public async Task<string> GetIncludeStatement(int projectId, int fileId, string methodName)
        {
            string result;
            var stmt = "$INSERT";
            var insertStmtData = await GetInsertStmtData(projectId, fileId, stmt).ConfigureAwait(false);
            var stringBuilder = new StringBuilder();
            if (insertStmtData.Count > 0)
            {
                var includeFileIds = new List<int>();
                using (_codeVortoService = new CodeVortoService())
                {
                    var fileMasterData = _codeVortoService.FileMasterRepository
                        .GetAllItems(f => f.FileTypeExtensionId == 12).Result.ToList();

                    foreach (var t in insertStmtData)
                    {
                        var insertStmt = t.OriginalStatement;
                        var splitInsert = insertStmt.Split('>');
                        var fileName = splitInsert.Last();
                        var anyFile = fileMasterData.Any(
                            f => !string.IsNullOrEmpty(fileName) &&
                                 f.FileTypeExtensionId == 12 &&
                                 f.FileName.StartsWith(fileName) &&
                                 f.Processed == 1);

                        if (!anyFile) continue;

                        var icdFile = fileMasterData.Find(
                            f => !string.IsNullOrEmpty(fileName) && f.FileTypeExtensionId == 12 &&
                                 f.FileName.StartsWith(fileName) &&
                                 f.Processed == 1);
                        if (icdFile != null) includeFileIds.Add(icdFile.FileId);
                    }
                }
                var includeFIles = string.Join(",", includeFileIds);
                if (string.IsNullOrEmpty(includeFIles))
                    return "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";

                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpFindExternalCallMethodName for: (" + projectId + "," + methodName + "," + includeFIles + ")");
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Text) {Value = includeFIles}
                };

                var chkStatementExistOrNot = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindExternalCallMethodName", parameters)
                    .Result;

                if (chkStatementExistOrNot.Any())
                {
                    var statementReferenceMaster = chkStatementExistOrNot.FirstOrDefault();
                    if (statementReferenceMaster != null)
                    {
                        result = "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                            statementReferenceMaster.FileId + ");'>[ " + methodName + " ]</a>";
                        return result;
                    }
                }
            }
            else
            {
                result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                return result;
            }

            result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
            LogMessage.WriteLogMessage(stringBuilder);
            return result;
        }

        private List<StatementReferenceMaster> GetGenericBlock(int stmtId, int startBaseCommandId, int endBaseCommandId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = startBaseCommandId},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = endBaseCommandId}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
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

        private async Task<List<StatementReferenceMaster>> GetInsertStmtData(int projectId, int fileid, string insertStmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {

                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileid},
                    new MySqlParameter("@insertStmt", MySqlDbType.VarChar) {Value = insertStmt}
                };
                var dictionaryData = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUbGetInsertStatementData", parameters)
                    .ConfigureAwait(false);

                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<StatementReferenceMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(dictionaryData, settings);
                var listData = JsonConvert.DeserializeObject<List<StatementReferenceMaster>>(json);
                return listData;
            }
        }

    }
}
