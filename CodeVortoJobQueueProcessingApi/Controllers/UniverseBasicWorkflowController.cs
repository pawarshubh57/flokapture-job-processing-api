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
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.ActionFilters;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using Microsoft.VisualBasic.FileIO;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;
using SearchOption = System.IO.SearchOption;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicController
    {
        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniverseProgramWorkflows(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var allPrograms = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.FileTypeExtensionId == 9 && f.ProjectId == projectMaster.ProjectId)
                        .ConfigureAwait(false);
                    var toProcessPrograms = allPrograms
                        .Where(p => string.IsNullOrEmpty(p.WorkFlowStatus) && p.Processed == 1).ToList();
                    // stringBuilder.AppendLine("Started processing for Individual UniVerse Basic Program Workflows");
                    foreach (var fileMaster in toProcessPrograms)
                    {
                        var stringBuilder = new StringBuilder();
                        stringBuilder.AppendLine("======================================================================\n");
                        stringBuilder.AppendLine("Started processing for Program Workflow: \n");
                        stringBuilder.AppendLine("Program: " + fileMaster.FileName)
                            .AppendLine("ProgramId: " + fileMaster.FileId)
                            .AppendLine("Program Path: " + fileMaster.FilePath);
                        var master = fileMaster;
                        int solutionId = master.SolutionId ?? 0;
                        var allClassStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.BaseCommandId == 19 && s.FileId == master.FileId)
                            .ConfigureAwait(false);
                        if (!allClassStatements.Any()) continue;
                        string programMethod = Path.GetFileNameWithoutExtension(fileMaster.FilePath) + "()";
                        string className = allClassStatements.First().ClassNameDeclared;
                        var allMethods = await GetAllMethodsForClass(className, master.FileId, solutionId).ConfigureAwait(false);

                        foreach (var statementReference in allMethods)
                        {
                            if (statementReference.MethodName != programMethod) continue;
                            Console.WriteLine("====================================================================");
                            Console.WriteLine("Program: " + fileMaster.FileName);
                            Console.WriteLine("ProgramId: " + fileMaster.FileId);
                            Console.WriteLine("Program Path: " + fileMaster.FilePath);
                            Console.WriteLine("Processing...");

                            await GetWorkFlowWorkSpaceTooManyNodes(fileMaster, fileMaster.ProjectId, className,
                                statementReference.MethodName).ConfigureAwait(false);

                            Console.WriteLine("===================================================================");
                            break;
                        }
                        stringBuilder.AppendLine("Completed processing for Program Workflow: \n");
                        stringBuilder.AppendLine("=============================================================\n");
                        fileMaster.WorkFlowStatus = "Workflow processed successfully";
                        fileMaster.DoneParsing = 1;
                        fileMaster.ProjectMaster = null;
                        fileMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);
                        LogMessage.WriteLogMessage(stringBuilder);
                    }

                    Console.WriteLine("=================================================================================\n");
                    Console.WriteLine("Program Workflows processed successfully for Project: " + projectMaster.ProjectName);
                    Console.WriteLine("=================================================================================\n");

                    return Ok("Done");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    LogMessage.WriteExceptionLogMessage(exception.InnerException);

                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        [ActionExceptionFilter]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceTooManyNodes(FileMaster fileMaster, int projectId,
            string className, string methodName)
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

                var isThisProgramIdExists = await _codeVortoService.SecondTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == thisProgramId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any()) return Ok("Workflow data collected successfully");

                var allMethodsOfClass = await GetAllMethodsForClass(className, thisProgramId, solutionId)
                    .ConfigureAwait(false);

                var hasAnyMethod = allMethodsOfClass.Any(t => t.MethodName == methodName);
                if (!hasAnyMethod) return Ok("Workflow data collected successfully");

                int methodStatementId = allMethodsOfClass.First(t => t.MethodName == methodName).StatementId;

                // Check whether this file already processed or not. If yes, then do not process it again.
                // We will just check this in first tabs data...
                Console.WriteLine("======================================");
                Console.WriteLine("Started processing for ProgramId (FileId): " + fileMaster.FileId);
                Console.WriteLine("Started processing for ProgramId (File Name): " + fileMaster.FileName);
                stringBuilder.AppendLine("=====================================================");
                stringBuilder.AppendLine("Started collecting all GetMethodBlock for  methodStatementId: " +
                                         methodStatementId);

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
                                             projectId + ", and fileId is :" +
                                             treeItem.StatementReferenceMaster.FileId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, 0,
                        treeItem.StatementReferenceMaster.FileId, indentLevel, lstAllCalledMethods, ref auto,
                        ref treeNodeId,
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
                                 "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' " +
                                 " onclick='includeStateDialog(" + referenceFile.FileId + ");'>[ " + extProgramName + " ]</a>";

                    treeItem.GraphName = neLink;
                }

                if (copyOfLstTreeView.Count >= cnt)
                {
                    var flMaster = await _codeVortoService.FileMasterRepository.GetItem<FileMaster>(
                        f => f.FileId == thisProgramId,
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
                    AnnotateStatement = null,
                    StatementComment = sTab.StatementReferenceMaster != null
                        ? sTab.StatementReferenceMaster.StatementComment
                        : ""
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<SecondTabProgramDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                string mySqlQuery = " Select * from SecondTabProgramDetails Where " +
                                    " ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId +
                                    " ";
                var statementSecondTab = await generalRepositoryWorkflowTreeviewSecondTabDetails
                    .GetDataFromSqlQuery<SecondTabProgramDetails>(mySqlQuery);

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

                /*

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

                */

                #endregion

                return Ok("Workflow data collected successfully");

                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateReferenceFileIdForPrograms(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var allPrograms = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.SolutionId == projectMaster.SolutionId &&
                                          p.ProjectId == projectMaster.ProjectId
                                          && (p.FileTypeExtensionId == 9 || p.FileTypeExtensionId == 17))
                                          .ConfigureAwait(false);

                foreach (var fileMaster in allPrograms)
                {
                    var allCallExternals = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == fileMaster.FileId && s.SolutionId == projectMaster.SolutionId
                        && (s.BaseCommandId == 6 && s.ReferenceFileId == 0)).ConfigureAwait(false);
                    foreach (var callExternal in allCallExternals)
                    {
                        if (callExternal.BaseCommandId == 19) continue;
                        if (callExternal.ReferenceFileId != 0) continue;

                        string callStatement = callExternal.OriginalStatement;
                        if (callStatement.StartsWith("CALLED.", true, CultureInfo.CurrentCulture) ||
                            callStatement.StartsWith("CALL.", true, CultureInfo.CurrentCulture))
                        {
                            callExternal.BaseCommandId = 0;
                            callExternal.FileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(callExternal)
                                .ConfigureAwait(false);
                            continue;
                        }

                        string subRoutineName = callStatement.Replace("CALL ", "")
                            .Replace("@", "").Trim().Split('(').First();
                        string subRoutineFileName = subRoutineName.Trim().TrimStart(' ').TrimEnd(' ');
                        var subRoutines = (from p in allPrograms
                                           let name = Path.GetFileNameWithoutExtension(p.FilePath)
                                           where name == subRoutineFileName && (p.FileTypeExtensionId == 17
                                                                            || p.FileTypeExtensionId == 9)
                                           select p).ToList();
                        if (!subRoutines.Any())
                        {
                            callExternal.ReferenceFileId = 0;
                            callExternal.FileMaster = null;
                            callExternal.ReferenceFileMaster = null;
                            callExternal.BaseCommandId = 6;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(callExternal)
                                .ConfigureAwait(false);
                            continue;
                        }

                        foreach (var subRoutine in subRoutines)
                        {
                            var pNameNew = subRoutine.ProjectMaster.ProjectName;
                            var pPathNew = subRoutine.ProjectMaster.PhysicalPath;
                            var className = subRoutine.FilePath.Replace(pPathNew + "\\", "")
                                .Replace(subRoutine.FileName, "").Replace("\\", ".");
                            var fileName = Path.GetFileNameWithoutExtension(subRoutine.FilePath);
                            string classNameDeclared = pNameNew + "." + className + fileName;

                            callExternal.ReferenceFileId = subRoutine.FileId;
                            callExternal.ClassCalled = classNameDeclared;
                            callExternal.FileMaster = null;
                            callExternal.ReferenceFileMaster = null;
                            callExternal.BaseCommandId = 6;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(callExternal)
                                .ConfigureAwait(false);
                        }
                    }
                }
                return Ok("Updated Reference FileId For Programs of Project: " + projectMaster.ProjectName);
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

        private static async Task<List<StatementReferenceMaster>> GetAllMethodsForClass(string className, int fileId,
            int solutionId)
        {
            using (var codeVortoService = new CodeVortoService())
            {
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar) {Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className},
                    new MySqlParameter("@slnId", MySqlDbType.Int32) {Value = solutionId},
                    new MySqlParameter("@flId", MySqlDbType.Int32) {Value = fileId}
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

        [HttpGet]
        public async Task<string> GetIncludeStatement(int projectId, int fileId, string methodName)
        {
            string result;
            const string stmt = "$INSERT";
            var insertStmtData = await GetInsertStmtData(projectId, fileId, stmt).ConfigureAwait(false);
            var stringBuilder = new StringBuilder();
            if (insertStmtData.Any())
            {
                var includeFileIds = (from s in insertStmtData select s.ReferenceFileId).Distinct().ToList();

                /*
                using (_codeVortoService = new CodeVortoService())
                {
                    var fileMasterData = _codeVortoService.FileMasterRepository
                        .GetAllItems(f => f.FileTypeExtensionId == 12 && f.ProjectId == projectId).Result.ToList();

                    foreach (var t in insertStmtData)
                    {
                        var insertStmt = t.OriginalStatement;
                        var fileName = insertStmt.ExtractIncludeFileName();
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
                */
                var includeFiles = string.Join(",", includeFileIds);
                if (string.IsNullOrEmpty(includeFiles))
                    return "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";

                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpFindExternalCallMethodName for: " +
                                         "(" + projectId + "," + methodName + "," + includeFiles + ")");
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Text) {Value = includeFiles}
                };

                var chkStatementExistOrNot = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindExternalCallMethodName", parameters)
                    .ConfigureAwait(false);

                if (chkStatementExistOrNot.Any())
                {
                    var statementReferenceMaster = chkStatementExistOrNot.First();
                    result = "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                        statementReferenceMaster.FileId + ");'>[ " + statementReferenceMaster.MethodName + " - " + statementReferenceMaster.BusinessName + " ]</a>";
                    return result;
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

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForProjectInventory(int projectId, int solutionId,
            bool businessName = false, bool skipSame = false)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                // int languageId = projectMaster.LanguageId;

                var entitiesToExclude = _codeVortoService.AppDbContextRepository
                    .EntitiesToExclude.Where(p => p.ProjectId == projectMaster.ProjectId).ToList();

                var allExistingEntities = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.FileTypeExtensionId == 11 && f.ProjectId == projectMaster.ProjectId &&
                                          f.SolutionId == projectMaster.SolutionId).ConfigureAwait(false);
                var allExistingEntityNames = (from entity in allExistingEntities
                                              let eName = Path.GetFileNameWithoutExtension(entity.FilePath)
                                              select eName).ToList();

                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var allDataDependancies = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(d => d.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

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

                /*
                string missingObjSql = " SELECT * FROM StatementReferenceMaster WHERE " +
                                       " ReferenceFileId = 0 AND BaseCommandId = 6 " +
                                       " AND ProjectId = " + projectMaster.ProjectId + ";";
                var allMissingRefs = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(missingObjSql).ConfigureAwait(false);
                */

                string sqlAllClasses =
                    " SELECT * FROM StatementReferenceMaster WHERE BaseCommandId IN (19, 5, 6, 1) AND " +
                    " ProjectId = " + projectMaster.ProjectId + " ORDER BY FileId ASC;";
                var allClassNameDeclaredOld = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllClasses).ConfigureAwait(false);
                var allClassNameDeclaredNew = allClassNameDeclaredOld.Where(x => x.BaseCommandId == 19)
                    .Where(classNm => !string.IsNullOrEmpty(classNm.ClassNameDeclared)).ToList();

                /*
                string refSqlQuery = "SELECT * FROM StatementReferenceMaster WHERE ReferenceFileId " +
                                     "IN(SELECT FileId FROM FileMaster WHERE FileTypeExtensionId = 12); ";
                var allRefStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(refSqlQuery).ConfigureAwait(false);
                */

                string referenceIcdQuery =
                    " SELECT * FROM StatementReferenceMaster AS srm INNER JOIN FileMaster AS fm" +
                    " ON fm.FileId = srm.ReferenceFileId  WHERE srm.ReferenceFileId != 0 " +
                    " AND srm.ProjectId = " + projectId + " AND fm.FileTypeExtensionId = 12; ";

                var allRefStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(referenceIcdQuery).ConfigureAwait(false);

                /*
                var allRefStatementMaster = await _codeVortoService.StatementReferenceMasterRepository.GetAllListItems(
                    s => s.ProjectId == projectId && s.ReferenceFileId != 0 && s.FileMaster.FileTypeExtensionId == 12);
                */

                allRefStatementMaster = allRefStatementMaster
                    .Where(s => s.ProjectId == projectId && s.SolutionId == solutionId).ToList();

                var allClassNameDeclared = new List<StatementReferenceMaster>();
                allClassNameDeclared.AddRange(allClassNameDeclaredNew);

                var allCallExternals = (from s in allClassNameDeclaredOld
                                        where s.BaseCommandId == 6 && !string.IsNullOrEmpty(s.ClassCalled)
                                        select s).ToList();

                var allCallExternalsWithOutClassCalled = (from s in allClassNameDeclaredOld where s.BaseCommandId == 6 select s).ToList();

                var allComplexity = (from s in allClassNameDeclaredOld
                                     where s.BaseCommandId == 1 || s.BaseCommandId == 5
                                     select s).ToList();

                string qryWorkflowStatements = " SELECT * FROM WorkflowTreeViewSecondTabDetails " +
                                               " WHERE ProjectId = " + projectId + " AND ClassCalled IS NOT NULL;";

                var allWorkflowStatements = await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                    .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(qryWorkflowStatements)
                    .ConfigureAwait(false);

                var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);

                var iDescMatchDesc = new List<LstObject>();
                var universeDescriptorList = new List<UniverseDescriptorList>();

                var jclFiles = allFiles.Where(f => f.FileTypeExtensionId == 10).ToList();
                var jclDictionary = new Dictionary<FileMaster, List<string>>();
                var statementRegEx = new Regex(@"^(SELECT\s+|SSELECT\s+|LIST\s+|DOWNLOAD\s+|HEADING\s+|SORT\s+)",
                    RegexOptions.Singleline);
                foreach (var jclFile in jclFiles)
                {
                    var file = jclFile;
                    var jclFileStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(f => f.FileId == file.FileId).ConfigureAwait(false);
                    var allStatements = (from a in jclFileStatements
                                         where statementRegEx.IsMatch(a.OriginalStatement)
                                         select a.OriginalStatement).ToList();
                    if (!allStatements.Any()) continue;
                    jclDictionary.Add(jclFile, allStatements);
                }

                var allUniverseDescriptor = await _codeVortoService.UniverseDescriptorRepository
                    .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);

                foreach (var jclFile in jclDictionary)
                {
                    var lstDescriptors = new List<Descriptors>();
                    var dataEntity = (from d in allDataDependancies where d.FileId == jclFile.Key.FileId select d)
                        .ToList();
                    var universeDesc = (from d in allUniverseDescriptor
                                        where dataEntity.Any(e => d.Entity == e.Entity)
                                        select d).ToList();
                    foreach (var uDescriptor in universeDesc)
                    {
                        string spName = uDescriptor.StoredProcedureName.Trim();
                        if (spName.StartsWith("*") || spName.StartsWith(".") || spName.Contains("*")) continue;
                        string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var iDescRegEx = new Regex("(\\s" + spName + "\\s)", RegexOptions.Singleline);
                        bool isPresent = jclFile.Value.Any(s => iDescRegEx.IsMatch(s));
                        if (!isPresent) continue;

                        // Here we need find index position of entity and spName (Stored Procedure Name).
                        // Index of entity must be less than spName.
                        // int entityNameLn = uDescriptor.Entity.Length;
                        // int spNameLn = spName.Length;

                        // Get actual matching statement
                        var statementStringList = jclFile.Value.FindAll(s => iDescRegEx.IsMatch(s));
                        foreach (var statementString in statementStringList)
                        {
                            int indexOfEntity = statementString
                                .IndexOf(uDescriptor.Entity, StringComparison.CurrentCultureIgnoreCase);
                            int indexOfSpName = statementString
                                .IndexOf(spName, StringComparison.CurrentCultureIgnoreCase);

                            // If index of entity name is greater than index of spName, then continue.
                            if (indexOfEntity > indexOfSpName) continue;

                            lstDescriptors.Add(new Descriptors
                            {
                                CompleteName = entityName,
                                DescriptorId = uDescriptor.DescriptorId,
                                SpName = spName
                            });
                        }
                    }
                    if (!lstDescriptors.Any()) continue;
                    var f = new FileDescriptors
                    {
                        FileId = jclFile.Key.FileId,
                        FileName = jclFile.Key.FileName
                    };
                    iDescMatchDesc.Add(new LstObject
                    {
                        Descriptors = lstDescriptors,
                        FileDescriptors = f
                    });
                }
                var regEx = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);

                var universeDescriptor = (from u in allUniverseDescriptor
                                          where u.StatementString.Contains("SUBR")
                                          select u).ToList();

                foreach (var uDescriptor in universeDescriptor)
                {
                    string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                    var stringLstData = uDescriptor.StatementString.Split(';').ToList();
                    // var dData = new List<string>();
                    foreach (var lstData in stringLstData)
                    {
                        // var regex = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);
                        var allGroups = regEx.Match(lstData).Groups;
                        int groupCnt = -1;
                        foreach (Group group in allGroups)
                        {
                            groupCnt++;
                            if (groupCnt == 0) continue;
                            string groupName = group.Value;
                            string eName = groupName.Replace("'", "");
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
                    var name = Path.GetFileNameWithoutExtension(file.FileName);
                    var pNameNew = file.ProjectMaster.ProjectName;
                    var pPathNew = file.ProjectMaster.PhysicalPath;
                    var classNameProgram = file.FilePath.Replace(pPathNew + "\\", "")
                        .Replace(file.FileName, "").Replace("\\", ".");
                    var fileNameProgram = Path.GetFileNameWithoutExtension(file.FilePath);
                    var classNameDeclared = pNameNew + "." + classNameProgram + fileNameProgram;
                    var cName = (from c in allClassNameDeclared
                                 where c.FileId == file.FileId && c.BaseCommandId == 19
                                 select c).ToList();

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
                        OriginalClassName = !string.IsNullOrEmpty(classNameDeclared) ? classNameDeclared : name,
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
                        if (originNode == null) continue;
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

                foreach (var node in lstNodes)
                {
                    // if (node.FileId != 11556) continue;
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
                    //if(file.FileId != 1378) continue;

                    /*
                    var allMissingCallExternals = (from s in allMissingRefs
                        where s.FileId == file.FileId && s.ReferenceFileId == 0 && s.BaseCommandId == 6
                        select s).ToList();
                    */

                    int fileTypeExtensionId = file.FileTypeExtensionId;
                    // if (fileTypeExtensionId != 10) continue;
                    bool isPgmOrSbr = fileTypeExtensionId == 9 || fileTypeExtensionId == 17 ||
                                      fileTypeExtensionId == 12;
                    var calledFrom = (from c in allCallExternals where c.ReferenceFileId == file.FileId select c)
                        .Distinct().ToList();
                    int linesCount = allFiles.First(c => c.FileId == file.FileId).LinesCount;
                    int complexity = 0;
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
                    if (file.FileTypeExtensionId == 9 || file.FileTypeExtensionId == 10 ||
                        file.FileTypeExtensionId == 17 || file.FileTypeExtensionId == 11 || file.FileTypeExtensionId == 12)
                    {
                        complexity = (from s in allComplexity
                                      where s.FileId == file.FileId && s.BaseCommandId == 1
                                      select s)
                            .Count();
                        complexity = complexity + 1;
                    }
                    else
                    {
                        var allFileLines = await _codeVortoService.ViewSourceMasterRepository
                            // ReSharper disable once AccessToModifiedClosure
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

                    var statementReferenceMaster = node.StatementReferenceMaster;
                    if (string.IsNullOrEmpty(statementReferenceMaster?.ClassNameDeclared)) continue;
                    var allFileIds = lstMainNodeList.Select(nodeList => nodeList.FileId).ToList();
                    var usesObjects = (from f in allFileIds
                                       where allFiles.Any(d =>
                                           d.FileId == f && (d.FileTypeExtensionId == 12
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
                        string workFlowNm;
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
                            default:
                                workFlowNm = workflow.OriginObject;
                                break;
                        }
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
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; " +
                            "text-decoration: underline;' " +
                            "onclick='includeStateDialog(" + uObj.FileId + ");'>" + uObj.Name + "</a>";
                        userObjectTempList.Add(str);
                    }
                    /*
                    // var addedMissings = new List<string>();
                    foreach (var uObj in allMissingCallExternals)
                    {
                        if (string.IsNullOrEmpty(uObj.OriginalStatement)) continue;
                        int spaceFirstIndex = uObj.OriginalStatement.IndexOf(' ');
                        if (spaceFirstIndex < 0) spaceFirstIndex = 0;
                        string missingRef = uObj.OriginalStatement.Substring(spaceFirstIndex);
                        // if (addedMissings.Any(m => m == uObj.OriginalStatement)) continue;
                        // addedMissings.Add(uObj.OriginalStatement);

                        var str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                  "cursor: pointer; text-decoration: none;'>" + missingRef + " [ Missing ]</a>";
                        userObjectTempList.Add(str);
                    }
                    */

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
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px;" +
                                  " text-decoration: underline;' " +
                                  " onclick='includeStateDialog(" + uObj.FileId + ");'>" + uObj.Name + "</a>";
                        userQueriesTempList.Add(str);
                    }

                    if (userObjectTempList.Any())
                    {
                        string str = "<li>" + userObjectTempList[0] + "</li>";
                        userObjectTempList[0] = str;
                    }
                    var internalCall = (from c in allComplexity
                                        where c.FileId == file.FileId && c.BaseCommandId == 5
                                        select c).Count();
                    var externalCallList = (from x in allCallExternalsWithOutClassCalled
                                            where x.FileId == file.FileId && x.BaseCommandId == 6
                                            select x).ToList();
                    var refFileIds = new List<int>();
                    var addedMissings = new List<string>();
                    foreach (var eCall in externalCallList)
                    {
                        string str;
                        int refFileId = eCall.ReferenceFileId;

                        if (addedMissings.Any(m => m == eCall.OriginalStatement)) continue;

                        addedMissings.Add(eCall.OriginalStatement);

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
                    // addedMissings = new List<string>();

                    var iDescriptorExternalCall =
                        (from d in iDescMatchDesc where d.FileDescriptors.FileId == file.FileId select d.Descriptors)
                        .ToList().FirstOrDefault();
                    // var externalDescriptor = iDescriptorExternalCall.ToList();
                    //var iDescriptorExternalCall = (from d in universeJclList
                    //    where d.FileMasters.Any(x => x.FileId == file.FileId)
                    //    select d).ToList();
                    if (iDescriptorExternalCall != null)
                    {
                        foreach (var eDescriptor in iDescriptorExternalCall)
                        {
                            // var iDescriptorId = eDescriptor.DescriptorId;
                            var completeName = eDescriptor.CompleteName;
                            var str = "<a href='javascript:void(0);'>" + completeName + "</a>";
                            calledExternalList.Add(str);
                        }
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

                    // TODO: Collect all entities data from Missing Objects Table for 
                    // current file and attach those entities as Missing...
                    var allMissingEntities = await _codeVortoService.MissingObjectsRepository
                        .GetAllListItems(e => e.FileId == node.FileId && e.Type == "Entity").ConfigureAwait(false);

                    var finalDataDependency = new List<string>();
                    var missingEntities = (from m in allMissingEntities select m.EntityName).ToList();

                    dataDependancies.AddRange(missingEntities);

                    dataDependancies = dataDependancies.Distinct().ToList();

                    if (dataDependancies.Any())
                    {
                        foreach (var dataD in dataDependancies)
                        {
                            if (allMissingEntities.Any(s => s.EntityName == dataD)
                                || allExistingEntityNames.All(a => a != dataD))
                                finalDataDependency.Add("<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                                      " text-decoration: none;'> " + dataD + " [ Missing ] </a>");

                            else finalDataDependency.Add(dataD);
                        }
                    }

                    if (finalDataDependency.Any())
                    {
                        string str = "<li>" + finalDataDependency[0] + "</li>";
                        finalDataDependency[0] = str;
                    }

                    // dataDependancies.RemoveAll(e => allMissingEntities.Any(m => m.EntityName == e));
                    /*
                    foreach (var missingEntity in allMissingEntities)
                    {
                        string str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                     " text-decoration: none;'> " + missingEntity.EntityName + " [ Missing ] </a>";
                        if (!finalDataDependency.Any()) finalDataDependency.Add("<li>" + str + "</li>");

                        else finalDataDependency.Add(str);
                    }
                    */

                    var uObject = string.Join("<li>", userObjectTempList);
                    var uReport = string.Join("<li>", userReportTempList);
                    var uQuery = string.Join("<li>", userQueriesTempList);
                    string uEntities = string.Join("<li>", finalDataDependency);
                    string cFrom = string.Join("<li>", calledFromList);
                    string cExternal = string.Join("<li>", calledExternalList);
                    string partInWorkflow = string.Join("<li>", allWorkFlowsTempList);

                    var callFormFinal = calledFromList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                          node.Id + "'>" + cFrom + " </div> " : "-";

                    // For I-Descriptor...
                    // string sbrFileName = Path.GetFileNameWithoutExtension(file.FilePath);
                    // var regEx = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);
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
                        if (iDescriptorList.Any())
                        {
                            int callFormCnt;
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
                        UsesEntities = finalDataDependency.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvDataDepend_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              finalDataDependency.Count +
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
                        ProjectId = projectMaster.ProjectId,
                        SolutionId = projectMaster.SolutionId ?? 2,
                        FileId = file.FileId,
                        CallingTo = null
                    };

                    using (var appDbContext = new AppDbContext())
                    {
                        appDbContext.Set<ProjectInventory>().Add(inventoryDetails);
                        await appDbContext.SaveChangesAsync();
                    }
                }
                Console.WriteLine("=================================================================================");
                Console.WriteLine("Done processing inventory for Project Id: " + projectMaster.ProjectName);
                Console.WriteLine("=================================================================================");
                return Ok(projectMaster);
                // return Ok("Done processing inventory for Project Id: " + solutionId);
            }
        }

        private static List<string> ExtractDataDependencyForPgmAndSbr(FileMaster fileMaster,
            List<DataDependency> dataDependencies, List<StatementReferenceMaster> lstStatementRef, List<string> entitiesList)
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
        public async Task<IHttpActionResult> ViewSource(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);
                var regEx = new Regex(@"^\*", RegexOptions.IgnoreCase);
                foreach (var fileMaster in fileMasters)
                {
                    if (fileMaster.FilePath.EndsWith(".txt") || fileMaster.FilePath.EndsWith(".TXT")) continue;
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
                    if (fileMaster.FileTypeExtensionId == 10) originalSourceList = originalSourceList.Skip(1).ToList();
                    string sourceWithoutComment = string.Join("\n", originalSourceList);
                    viewSourceMaster.SourceWithoutComments = sourceWithoutComment;

                    await _codeVortoService.ViewSourceMasterRepository.UpdateItem(viewSourceMaster).ConfigureAwait(false);
                }
            }
            Console.WriteLine("=================================================================================\n");
            Console.WriteLine("Source Master Data imported successfully for Project: " + projectMaster.ProjectName);
            Console.WriteLine("=================================================================================\n");

            return Ok("Processed");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForUniverseDescriptor(ProjectMaster projectMaster, bool isCtCode)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    string descriptorsFilePath = Path.Combine(projectMaster.PhysicalPath, "I-Descriptors");
                    var allFiles = Directory.GetFiles(descriptorsFilePath, "*.csv", SearchOption.AllDirectories).ToList();
                    foreach (var file in allFiles)
                    {
                        int totalRecords = File.ReadAllLines(file).Length;
                        Console.WriteLine("================================================================================");
                        Console.WriteLine("Current I-Descriptor file: " + file);
                        Console.WriteLine("Current I-Descriptor file name: " + Path.GetFileName(file));
                        Console.WriteLine("ProjectId: " + projectMaster.ProjectId + "\nProject Name: " +
                                          projectMaster.ProjectName);
                        Console.WriteLine("Total Records: " + totalRecords);

                        #region Alternate Solution to Read CSV File...
                        /*
                        var csvHelperReader = new CsvHelper.CsvReader(new StreamReader(file), new Configuration
                        {
                            Delimiter = "\\",
                            IgnoreQuotes = true,
                            HasHeaderRecord = true,
                            QuoteAllFields = true,
                            HeaderValidated = null,
                            MissingFieldFound = null
                        });

                        var listUniverseDescriptor = new List<UniverseDescriptor>();
                        while (csvHelperReader.Read())
                        {
                            var univerDescriptor = new UniverseDescriptor
                            {
                                Entity = csvHelperReader.GetField<string>(0),
                                StoredProcedureName = csvHelperReader.GetField<string>(1),
                                Type = csvHelperReader.GetField<string>(2),
                                DefaultReportDisplayHeading = csvHelperReader.GetField<string>(3),
                                DefaultFormating = csvHelperReader.GetField<string>(4),
                                DefaultConversion = csvHelperReader.GetField<string>(5),
                                ValuedAssociation = csvHelperReader.GetField<string>(6),
                                LongDescription = csvHelperReader.GetField<string>(7),
                                StatementString = csvHelperReader.GetField<string>(8)
                            };
                            listUniverseDescriptor.Add(univerDescriptor);

                            var type = typeof(UniverseDescriptor);
                            var properties = type.GetProperties();

                            var isAllEmptyOrNull = properties.All(p =>
                                string.IsNullOrEmpty(Convert.ToString(p.GetValue(univerDescriptor)))
                                || Convert.ToString(p.GetValue(univerDescriptor)) == "0");
                            if (isAllEmptyOrNull) continue;

                            Console.WriteLine(JsonConvert.SerializeObject(univerDescriptor));

                            //await _codeVortoService.UniverseDescriptorRepository.AddNewItem(univerDescriptor)
                            //    .ConfigureAwait(false);
                        }
                        */
                        #endregion

                        int index = -1;
                        using (var csvReader = new TextFieldParser(file))
                        {
                            // This is for DC Code set...
                            /*
                                csvReader.SetDelimiters("\\");
                                csvReader.HasFieldsEnclosedInQuotes = false;
                            */

                            if (isCtCode)
                            {
                                // This is for CT Code set...
                                csvReader.SetDelimiters(",");
                                csvReader.HasFieldsEnclosedInQuotes = true;
                            }
                            else
                            {
                                csvReader.SetDelimiters("\\");
                                csvReader.HasFieldsEnclosedInQuotes = false;
                            }

                            while (!csvReader.EndOfData)
                            {
                                index++;
                                var fieldData = csvReader.ReadFields();
                                if (index == 0) continue;
                                if (fieldData == null) continue;
                                var valuesList = new List<string>
                                {
                                    fieldData.Length > 0 ?  fieldData[0] : "",
                                    fieldData.Length > 1 ?  fieldData[1] : "",
                                    fieldData.Length > 2 ?  fieldData[2] : "",
                                    fieldData.Length > 3 ?  fieldData[3] : "",
                                    fieldData.Length > 4 ?  fieldData[4] : "",
                                    fieldData.Length > 5 ?  fieldData[5] : "",
                                    fieldData.Length > 6 ?  fieldData[6] : "",
                                    fieldData.Length > 7 ?  fieldData[7] : "",
                                    fieldData.Length > 8 ?  fieldData[8] : ""
                                };
                                bool isAllEmpty = valuesList.All(string.IsNullOrEmpty);
                                if (isAllEmpty) continue;

                                var univerDescriptor = new UniverseDescriptor
                                {
                                    Entity = valuesList[0],
                                    ProjectId = projectMaster.ProjectId,
                                    StoredProcedureName = valuesList[1],
                                    Type = valuesList[2],
                                    DefaultReportDisplayHeading = valuesList[3],
                                    DefaultFormating = valuesList[4],
                                    DefaultConversion = valuesList[5],
                                    ValuedAssociation = valuesList[6],
                                    LongDescription = valuesList[7],
                                    StatementString = valuesList[8],
                                    StatementsListed = null, // fieldData[9],
                                    ExtractionNotes = null
                                };
                                var type = typeof(UniverseDescriptor);
                                var properties = type.GetProperties();

                                var isAllEmptyOrNull = properties.All(p =>
                                    string.IsNullOrEmpty(Convert.ToString(p.GetValue(univerDescriptor)))
                                    || Convert.ToString(p.GetValue(univerDescriptor)) == "0");
                                if (isAllEmptyOrNull) continue;

                                await _codeVortoService.UniverseDescriptorRepository.AddNewItem(univerDescriptor)
                                    .ConfigureAwait(false);

                                Console.WriteLine("Total I-Descriptor records remaining: " + (totalRecords - index));
                                Console.WriteLine("==========================================================");
                            }
                        }
                        Console.WriteLine("Total records inserted into database: " + (index + 1));
                        Console.WriteLine("================================================================================");
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.InnerException);
                    return InternalServerError(ex);
                }
                return Ok("I-Descriptor File(s) are processed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForDataInventory(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest();

                string sqlQry = "SELECT * FROM UniverseBasicDataDictionary " +
                                "WHERE ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName;";
                var entityObject = await _codeVortoService.DataDictionaryRepository
                    .GetDataFromSqlQuery<DataDictionary>(sqlQry).ConfigureAwait(false);
                var dataDependnacy = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId &&
                                          (x.FileTypeExtensionId == 17 || x.FileTypeExtensionId == 10))
                    .ConfigureAwait(false);

                var universeDescriptor = await _codeVortoService.UniverseDescriptorRepository
                    .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                int cCount = 0;
                foreach (var eObject in entityObject)
                {
                    var dataDependancyList = new List<string>();
                    var dataDependant = (from d in dataDependnacy where d.Entity == eObject.FileName select d);
                    cCount++;
                    var matchUniverseDescriptor = universeDescriptor
                        .Where(x => x.StatementString.Contains(eObject.FileName) && x.StatementString.Contains("TRANS"))
                        .ToList();

                    foreach (var dDependency in dataDependant)
                    {
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
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
                          cCount + "'>" + dDependancy + " </div> " : "-";

                    var usesEntList = new List<string>();
                    foreach (var mDescriptor in matchUniverseDescriptor)
                    {
                        var completeName = mDescriptor.Entity + " - " + mDescriptor.StoredProcedureName;
                        var str = "<a href='javascript:void(0);'>" + completeName + "</a>";
                        usesEntList.Add(str);
                    }
                    var nEntityList = new List<string>();
                    foreach (var d in usesEntList)
                    {
                        var aaa = "<li>" + d + "</li>";
                        nEntityList.Add(aaa);
                    }
                    var usesEntitiesList = nEntityList.Any() ? string.Join(" ", nEntityList) : "-";
                    if (nEntityList.Any())
                    {
                        int callFormCnt;
                        if (dDependancyFinal == "-")
                        {
                            callFormCnt = nEntityList.Count;
                            dDependancyFinal = nList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(entityCalledFrom_" + cCount +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  callFormCnt +
                                  " Called From </a> <div style='display: none;' id='entityCalledFrom_" +
                                  cCount +
                                  "'><div style='height: 10px;margin-bottom: 20px;'><h4> I-Desc </h4></div> " +
                                  usesEntitiesList + "  </div> "
                                : "-";
                        }
                        else
                        {
                            callFormCnt = nList.Count + nEntityList.Count;
                            dDependancyFinal = nList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(entityCalledFrom_" + cCount +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  callFormCnt +
                                  " Called From </a> <div style='display: none;' id='entityCalledFrom_" +
                                  cCount + "'>" + dDependancy +
                                  " <div style='height: 10px;margin-bottom: 20px; margin-top: 20px;'><h4> I-Desc </h4></div> " +
                                  usesEntitiesList + "  </div>"
                                : "-";
                        }
                    }

                    var entityName = "<a href='javascript:void(0);' title='" + eObject.FileName + "' itemprop='" + eObject.FileName + "' onclick='showEntitySchema(this);' style='color: blue; font-size: 14px; text-decoration: underline;'> " + eObject.FileName + "</a>";
                    var dataInventory = new DataInventory
                    {
                        ObjectName = entityName,
                        ObjectType = "Entity",
                        ExternalCall = "-",
                        CalledFrom = dDependancyFinal,
                        UsesEntities = "-",
                        ObjectDescription = eObject.Description,
                        ProjectId = projectId,
                        StatementInList = "-"
                    };

                    await _codeVortoService.DataInventoryRepository.AddNewItem(dataInventory);
                }

                var jclFiles = fileMasters.Where(f => f.FileTypeExtensionId == 10).ToList();
                var jclDictionary = new Dictionary<FileMaster, List<string>>();
                foreach (var jclFile in jclFiles)
                {
                    var jclFileStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(f => f.FileId == jclFile.FileId).ConfigureAwait(false);
                    var allStatements = (from a in jclFileStatements select a.OriginalStatement).ToList();
                    jclDictionary.Add(jclFile, allStatements);
                }

                // dataInventory = new List<DataInventory>();
                // var entityList = new List<string>();
                var cnt = 0;
                foreach (var uDescriptor in universeDescriptor)
                {
                    uDescriptor.CompleteName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                    var statementString = uDescriptor.StatementString;
                    var regEx = new Regex(@"TRANS\((.*?(?=,))", RegexOptions.Singleline);
                    var regExSubr = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);
                    string spName = uDescriptor.StoredProcedureName.Trim();
                    if (spName.StartsWith("*") || spName.StartsWith(".") || spName.Contains("*")) continue;
                    string spRegEx = @"(\s" + spName + "\\s)|\\((" + spName + "\\s)";
                    var iDescRegEx = new Regex(spRegEx, RegexOptions.Singleline);
                    var entityRegEx = new Regex(@"(\s" + uDescriptor.Entity.Trim() + "\\s)", RegexOptions.Singleline);
                    var lstJcls = new List<FileMaster>();
                    foreach (var jclFile in jclDictionary)
                    {
                        var isContains = jclFile.Value.Any(a => iDescRegEx.IsMatch(a) && entityRegEx.IsMatch(a));
                        if (!isContains) continue;
                        lstJcls.Add(jclFile.Key);
                    }

                    var stringLstData = statementString.Split(';').ToList();
                    var dependancySubrList = new List<string>();
                    var dependancyTrnsList = new List<string>();
                    var dataDependancyList = new List<string>();
                    cnt++;
                    foreach (var lstJcl in lstJcls)
                    {
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                  " onclick='includeStateDialog(" + lstJcl.FileId + ");'>" +
                                  lstJcl.FileName + "</a>";
                        dataDependancyList.Add(str);
                    }

                    var nList = new List<string>();
                    foreach (var d in dataDependancyList)
                    {
                        var aaa = "<li>" + d + "</li>";
                        nList.Add(aaa);
                    }
                    var dDependancy = nList.Any() ? string.Join(" ", nList) : "-";
                    var dDependancyFinal = dataDependancyList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(iDescptCalledFrom_" + cnt +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          nList.Count +
                          " Called From </a> <div style='display: none;' id='iDescptCalledFrom_" +
                          cnt + "'>" + dDependancy + " </div> "
                        : "-";

                    dDependancyFinal = !string.IsNullOrEmpty(dDependancyFinal) ? dDependancyFinal : "-";
                    foreach (var lData in stringLstData)
                    {
                        if (lData.Contains("TRANS"))
                        {
                            var allTransGroups = regEx.Match(lData).Groups;
                            int groupCnt = -1;
                            foreach (Group groupTrans in allTransGroups)
                            {
                                groupCnt++;
                                if (groupCnt == 0) continue;
                                string tName = groupTrans.Value;
                                tName = tName.Replace("'", "");
                                tName = "<li>" + tName + "</li>";
                                dependancyTrnsList.Add(tName);
                            }
                        }

                        if (!lData.Contains("SUBR")) continue;
                        var allGroups = regExSubr.Match(lData).Groups;
                        int groupSubrCnt = -1;
                        foreach (Group group in allGroups)
                        {
                            groupSubrCnt++;
                            if (groupSubrCnt == 0) continue;
                            string eName = @group.Value;
                            eName = eName.Replace("'", "");
                            if (eName.StartsWith("-")) continue;
                            var files = (from f in fileMasters
                                         let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                         where fName == eName && f.FileTypeExtensionId == 17
                                         select f).ToList();
                            if (!files.Any()) continue;
                            foreach (var file in files)
                            {
                                var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                          " onclick='includeStateDialog(" + file.FileId + ");'>" +
                                          file.FileName + "</a>";
                                eName = "<li>" + str + "</li>";
                            }
                            dependancySubrList.Add(eName);
                        }
                    }
                    dependancySubrList = dependancySubrList.Distinct().ToList();
                    dependancyTrnsList = dependancyTrnsList.Distinct().ToList();

                    var cExternal = dependancySubrList.Any() ? string.Join(" ", dependancySubrList) : "-";
                    var callExternalFinal = dependancySubrList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(iDescptCalledExternal_" + cnt +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          dependancySubrList.Count + " Call External </a> <div style='display: none;' id='iDescptCalledExternal_" +
                          cnt + "'>" + cExternal + " </div> " : "-";

                    var uEntities = dependancyTrnsList.Any() ? string.Join(" ", dependancyTrnsList) : "-";
                    var uEntitiesFinal = dependancyTrnsList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(iDescptUsedEntities_" + cnt +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          dependancyTrnsList.Count + " Entity(ies) </a> <div style='display: none;' id='iDescptUsedEntities_" +
                          cnt + "'>" + uEntities + " </div> " : "-";
                    var dList = new List<string>();
                    int index = 0;
                    foreach (var uObj in stringLstData)
                    {
                        index++;
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; cursor: default;line-height: 22px;'> " + index + ") " + uObj + "</a><br>";
                        dList.Add(str);
                    }
                    var uObject = string.Join("", dList);
                    var objName = dList.Any()
                         ? "<a href='javascript:void(0);' onclick='showData(dataInventoryDesc_" + uDescriptor.DescriptorId +
                           ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                           uDescriptor.CompleteName + "</a> <div lang='" + uDescriptor.CompleteName + "'" +
                           " style='display: none;' id='dataInventoryDesc_" +
                           uDescriptor.DescriptorId + "'>" + uObject + " </div> " : "-";
                    var dataInventoryNew = new DataInventory
                    {
                        ObjectName = objName,
                        ObjectType = "I-Desc",
                        ExternalCall = callExternalFinal,
                        CalledFrom = dDependancyFinal,
                        UsesEntities = uEntitiesFinal,
                        ObjectDescription = uDescriptor.LongDescription,
                        ProjectId = projectId,
                        StatementInList = "-"
                    };
                    Console.WriteLine("========================================");
                    Console.WriteLine();
                    Console.WriteLine(JsonConvert.SerializeObject(dataInventoryNew));
                    Console.WriteLine();
                    Console.WriteLine("========================================");
                    await _codeVortoService.DataInventoryRepository.AddNewItem(dataInventoryNew);
                }
                return Ok(projectMaster);
            }
        }

        // Following commented two methods are modified on Oct, 24 2018.
        // Added modified versions of these menthods exactly below commented part.

        #region Commented two methods...
        /*
        [HttpGet]
        public async Task<IHttpActionResult> ProcessCrudActivityReport(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                // int solutionId = projectMaster.SolutionId ?? 1;
                string query = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName";

                var universeDataDictionary = await _codeVortoService.DataDictionaryRepository
                    .GetDataFromSqlQuery<DataDictionary>(query)
                    .ConfigureAwait(false);

                var lstFileName = (from d in universeDataDictionary select d.FileName).Distinct();

                foreach (var fName in lstFileName)
                {
                    var fileName = fName;
                    string strQry =
                        " SELECT * FROM StatementReferenceMaster WHERE OriginalStatement LIKE '%" + fileName + "%'" +
                        " AND SolutionId = " + projectMaster.SolutionId + " AND ProjectId = " + projectMaster.ProjectId + " ;";
                    var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(strQry).ConfigureAwait(false);
                    var recordRegExMatch = new Regex(@"(R." + fileName + "\\(\\d+\\)\\<\\d+[,])|(R." + fileName + "\\(\\d+\\))|(" + fileName + ".REC\\<\\d+)|(" + fileName + ".REC\\(\\d+)", RegexOptions.IgnoreCase);
                    
                    // var regexFirst = new Regex(@"(R." + fileName + "\\(\\d+\\)\\<\\d+[,])", RegexOptions.IgnoreCase);
                    // var regexThird = new Regex(@"(" + fileName + ".REC\\<\\d+)", RegexOptions.IgnoreCase);
                    // var regexForth = new Regex(@"(" + fileName + ".REC\\(\\d+)", RegexOptions.IgnoreCase);
                    // string attributeNo = string.Empty;
                    // var lstAttributeName = new List<string>();
                   
                    var regexSecond = new Regex(@"(R." + fileName + "\\(\\d+\\))", RegexOptions.IgnoreCase);
                    string fieldNo = string.Empty;
                    var crudActivityReport = new List<CrudActivityReport>();
                    #region Statements

                    foreach (var sReference in statementReference)
                    {
                        var statement = sReference.OriginalStatement;
                        if (!recordRegExMatch.IsMatch(statement)) continue;
                        string objName = sReference.FileMaster.FileName;
                        string objType = sReference.FileMaster.FileTypeExtensionReference.FileTypeName;
                        if (!regexSecond.IsMatch(statement)) continue;
                        foreach (Match match in regexSecond.Matches(statement))
                        {
                            var groupIndex = -1;
                            foreach (Group group in match.Groups)
                            {
                                groupIndex++;
                                if (groupIndex == 0) continue;
                                if (string.IsNullOrEmpty(@group.Value)) continue;
                                var groupValue = @group.Value;
                                // Regex regex = new Regex(@"\((\d+)\)", RegexOptions.IgnoreCase);
                                var match2 = Regex.Match(groupValue, @"\((\d+)\)");
                                if (!match2.Success) continue;
                                var matchedString = match2.Groups[1].Value; // regex.Match(tempString).Value;
                                fieldNo = matchedString.TrimStart('0');
                                // if (string.IsNullOrEmpty(fieldNo)) continue;
                            }

                            var no = fieldNo;
                            var universeDataSecond = universeDataDictionary
                                .Where(x => x.ProjectId == projectMaster.ProjectId && x.FieldNo == no && x.FileName == fileName).ToList();
                            var attributeName = universeDataSecond.Any() ? universeDataSecond.First().FieldLabel : "-";

                            var newCrudActivity = new CrudActivityReport
                            {
                                EntityName = fileName,
                                ObjectName = objName,
                                ObjectType = objType,
                                AttributeName = attributeName,
                                AttributeNumber = fieldNo,
                                ProjectId = projectMaster.ProjectId
                            };

                            var crudActivity = crudActivityReport.Any(x =>
                                x.ObjectName == newCrudActivity.ObjectName && x.AttributeName == newCrudActivity.AttributeName &&
                                x.AttributeNumber == newCrudActivity.AttributeNumber && x.ObjectType == newCrudActivity.ObjectType &&
                                x.EntityName == newCrudActivity.EntityName && x.ProjectId == projectMaster.ProjectId);
                            if (crudActivity) continue;
                            crudActivityReport.Add(newCrudActivity);

                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.CrudActivityReport.Add(newCrudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                        }
                    }
                    #endregion
                }
            }

            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertSystemLevelCrudActivityForUniverse(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var insertList = new List<string> { "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var updateList = new List<string>{ "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var deleteList = new List<string> { "DELETE ", "DELETELIST ", "DELETEU " };
                var selectList = new List<string> { "MATREAD ","MATREADL ","MATREADU ",
                    "OPENCHECK ","OPENDEV ","OPENPATH ","OPENSEQ ","OPEN ","LOCATE ",
                    "READ ", "READL ", "READU ", "READV ","REAFV ","READSEQ ",
                    "READVL ","READVU ","READBLK ","READLIST ","READNEXT ","READT ",
                    "SSELECT ", "SELECT "};
                var regex = new Regex(@"^LIST\s+");
                var regexClear = new Regex(@"(^EXECUTE\s+""CLEAR.FILE)|(^EXECUTE\s+'CLEAR.FILE)|(^CLEAR.FILE\s+)|(^EXECUTE\s+CLEAR.FILE)");
                var regexEd = new Regex(@"^ED\s+");
                var sqlQry = " SELECT * FROM FileMaster WHERE ProjectId = " + projectMaster.ProjectId +
                             " AND FileTypeExtensionId = 11;";
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetDataFromSqlQuery<FileMaster>(sqlQry).ConfigureAwait(false);
                foreach (var fMaster in fileMaster)
                {
                    var entityName = Path.GetFileNameWithoutExtension(fMaster.FileName);
                    var sqlStatementQry =
                        " SELECT * FROM StatementReferenceMaster " +
                        " WHERE OriginalStatement LIKE '%" + entityName + "%' AND " +
                        " ProjectId = " + projectMaster.ProjectId + " " +
                        " AND SolutionId = " + projectMaster.SolutionId + ";";
                    var statementRefenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlStatementQry).ConfigureAwait(false);
                    if (!statementRefenceMaster.Any()) continue;
                    var fileIds = (from s in statementRefenceMaster select s.FileId).ToList();
                    var distinctFileIds = fileIds.Distinct().ToList();
                    foreach (var fId in distinctFileIds)
                    {
                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "N";
                        var currentStatememts = statementRefenceMaster.Where(x => x.FileId == fId).ToList();
                        if (!currentStatememts.Any()) continue;
                        foreach (string sss in currentStatememts.Select(s => s.OriginalStatement))
                        {
                            if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                            if (updateList.Any(c => sss.Contains(c))) update = "Y";
                            if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                            if (selectList.Any(c => sss.Contains(c))) select = "Y";
                            if (regex.IsMatch(sss)) select = "Y";
                            if (regexClear.IsMatch(sss)) delete = "Y";
                            if (regexEd.IsMatch(sss)) update = "Y";
                        }

                        if (iR == "N" && update == "N" && delete == "N" && select == "N") continue;
                        var dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = entityName,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = fId,
                            ProjectId = projectMaster.ProjectId,
                            FileMaster = null
                        };
                        string entity = entityName;
                        int fileIdNew = fId;
                        Expression<Func<DbCrudActivity, bool>> expression =
                            x => x.EntityName == entity && x.FileId == fileIdNew && x.ProjectId == projectMaster.ProjectId;
                        var crud = await _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                            .ConfigureAwait(false);
                        if (crud.Any()) continue;
                        await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);
                    }
                }
            }

            Console.WriteLine("==============================================================================");
            Console.WriteLine("Done processing System Level CRUD Activity for Project: " + projectMaster.ProjectName);
            Console.WriteLine("==============================================================================");
            return Ok("Done processing System Level CRUD Activity for Project: " + projectMaster.ProjectName);
        }
        
        */
        #endregion

        [HttpGet]
        public async Task<IHttpActionResult> ProcessCrudActivityReport(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                // if (projectMaster == null) return NotFound();

                string query = " SELECT * FROM UniverseBasicDataDictionary " +
                             " WHERE ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName";

                var universeDataDictionary = await _codeVortoService.DataDictionaryRepository
                    // .GetAllListItems(d => d.ProjectId == projectMaster.ProjectId)
                    .GetDataFromSqlQuery<DataDictionary>(query)
                    .ConfigureAwait(false);

                var lstFileName = (from d in universeDataDictionary select d.FileName).Distinct().ToList();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.FileTypeExtensionId != 11
                                          && f.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                foreach (var fileMaster in fileMasters)
                {
                    string strQry =
                        " SELECT * FROM StatementReferenceMaster " +
                        " WHERE FileId = " + fileMaster.FileId + " AND BaseCommandId = 45 AND " +
                        " SolutionId = " + projectMaster.SolutionId + " AND " +
                        " ProjectId = " + projectMaster.ProjectId + " ;";
                    var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(strQry).ConfigureAwait(false);

                    #region This is just for OPEN and MATWRITE Statements...

                    foreach (var sReference in statementReference)
                    {
                        if (sReference.OriginalStatement.StartsWith("MATWRITE "))
                        {
                            if (!sReference.OriginalStatement.Contains(" TO ")) continue;
                            var sParts = sReference.OriginalStatement.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (part != "TO") continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                entityName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(entityName)) continue;

                                var newCrudActivity = new CrudActivityReport
                                {
                                    EntityName = entityName,
                                    ObjectName = sReference.FileMaster.FileName,
                                    ObjectType = sReference.FileMaster.FileTypeExtensionReference.FileTypeName,
                                    AttributeName = "-",
                                    AttributeNumber = "-",
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false,
                                    OpenStatement = sReference.OriginalStatement,
                                    IsOpen = false
                                };

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.CrudActivityReport.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                break;
                            }
                        }
                        if (sReference.OriginalStatement.StartsWith("DELETE "))
                        {
                            var deleteRegEx = new Regex(@"DELETE\s+([\w\d\.]+)", RegexOptions.IgnoreCase);
                            var matchGroups = deleteRegEx.Match(sReference.OriginalStatement).Groups;
                            string objName = matchGroups[1].Value;
                            if (string.IsNullOrEmpty(objName)) continue;

                            var existingFiles = await _codeVortoService.FileMasterRepository
                                .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                                      f.SolutionId == projectMaster.SolutionId &&
                                                      f.FileName.StartsWith(objName)).ConfigureAwait(false);
                            var actualFiles = (from f in existingFiles
                                               let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                               where fName == objName
                                               select f).ToList();
                            var hasIndicator = !actualFiles.Any();
                            var newCrudActivity = new CrudActivityReport
                            {
                                EntityName = objName,
                                ObjectName = sReference.FileMaster.FileName,
                                ObjectType = sReference.FileMaster.FileTypeExtensionReference.FileTypeName,
                                AttributeName = "-",
                                AttributeNumber = "-",
                                ProjectId = projectMaster.ProjectId,
                                HasIndicator = hasIndicator,
                                IsOpen = true,
                                OpenStatement = sReference.OriginalStatement
                            };
                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.CrudActivityReport.Add(newCrudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                        }
                        if (!sReference.OriginalStatement.StartsWith("OPEN ")) continue;

                        var regEx = new Regex("(\'.*?\')", RegexOptions.IgnoreCase);
                        var matches = regEx.Matches(sReference.OriginalStatement);
                        // var stParts = sReference.OriginalStatement.Split(',').ToList();
                        foreach (Group part in matches)
                        {
                            if (!part.Value.StartsWith("'") && !part.Value.EndsWith("'")) continue;
                            if (string.IsNullOrEmpty(part.Value)) continue;
                            var objectName = part.Value.Replace("'", "").Replace("&", "");
                            if (string.IsNullOrEmpty(objectName) || string.IsNullOrWhiteSpace(objectName)) continue;
                            objectName = objectName.Trim().TrimStart(',').TrimEnd(',');
                            var existingFiles = await _codeVortoService.FileMasterRepository
                                .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                f.SolutionId == projectMaster.SolutionId &&
                                                      f.FileName.StartsWith(objectName)).ConfigureAwait(false);
                            var actualFiles = (from f in existingFiles
                                               let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                               where fName == objectName
                                               select f).ToList();
                            var hasIndicator = !actualFiles.Any();
                            var newCrudActivity = new CrudActivityReport
                            {
                                EntityName = objectName,
                                ObjectName = sReference.FileMaster.FileName,
                                ObjectType = sReference.FileMaster.FileTypeExtensionReference.FileTypeName,
                                AttributeName = "-",
                                AttributeNumber = "-",
                                ProjectId = projectMaster.ProjectId,
                                HasIndicator = hasIndicator,
                                IsOpen = true,
                                OpenStatement = sReference.OriginalStatement
                            };
                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.CrudActivityReport.Add(newCrudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                        }
                    }

                    #endregion

                    foreach (var fileName in lstFileName)
                    {
                        var recordRegExMatch =
                            new Regex(
                                @"(R." + fileName + "\\(\\d+\\)\\<\\d+[,])|(R." + fileName + "\\(\\d+\\))|(" + fileName + ".REC\\<\\d+)|(" + fileName + ".REC\\(\\d+)", RegexOptions.IgnoreCase);

                        var regexSecond = new Regex(@"(R." + fileName + "\\(\\d+\\))", RegexOptions.IgnoreCase);
                        string fieldNo = string.Empty;
                        var crudActivityReport = new List<CrudActivityReport>();

                        #region Statements

                        foreach (var sReference in statementReference)
                        {
                            if (sReference.ClassCalled == "Ok") continue;
                            var statement = sReference.OriginalStatement;
                            if (!recordRegExMatch.IsMatch(statement)) continue;
                            string objName = sReference.FileMaster.FileName;
                            string objType = sReference.FileMaster.FileTypeExtensionReference.FileTypeName;
                            if (!regexSecond.IsMatch(statement)) continue;
                            foreach (Match match in regexSecond.Matches(statement))
                            {
                                var groupIndex = -1;
                                foreach (Group group in match.Groups)
                                {
                                    groupIndex++;
                                    if (groupIndex == 0) continue;
                                    if (string.IsNullOrEmpty(@group.Value)) continue;
                                    var groupValue = @group.Value;
                                    // Regex regex = new Regex(@"\((\d+)\)", RegexOptions.IgnoreCase);
                                    var match2 = Regex.Match(groupValue, @"\((\d+)\)");
                                    if (!match2.Success) continue;
                                    var matchedString = match2.Groups[1].Value; // regex.Match(tempString).Value;
                                    fieldNo = matchedString.TrimStart('0');
                                    // if (string.IsNullOrEmpty(fieldNo)) continue;
                                }

                                var no = fieldNo;
                                var universeDataSecond = universeDataDictionary
                                    .Where(x => x.ProjectId == projectMaster.ProjectId && x.FieldNo == no &&
                                                x.FileName == fileName).ToList();
                                var attributeName = universeDataSecond.Any()
                                    ? universeDataSecond.First().FieldLabel
                                    : "-";

                                var newCrudActivity = new CrudActivityReport
                                {
                                    EntityName = fileName,
                                    ObjectName = objName,
                                    ObjectType = objType,
                                    AttributeName = attributeName,
                                    AttributeNumber = fieldNo,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false
                                };

                                var crudActivity = crudActivityReport.Any(x =>
                                    x.ObjectName == newCrudActivity.ObjectName &&
                                    x.AttributeName == newCrudActivity.AttributeName &&
                                    x.AttributeNumber == newCrudActivity.AttributeNumber &&
                                    x.ObjectType == newCrudActivity.ObjectType &&
                                    x.EntityName == newCrudActivity.EntityName &&
                                    x.ProjectId == projectMaster.ProjectId);

                                if (crudActivity) continue;
                                crudActivityReport.Add(newCrudActivity);

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.CrudActivityReport.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                sReference.ClassCalled = "Ok";
                            }
                        }
                        #endregion
                    }
                }

                return Ok("Completed ProcessCrudActivityReport for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessEntityAttributeUsageReport(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                string query = " SELECT * FROM UniverseBasicDataDictionary " +
                               " WHERE ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName";

                var universeDataDictionary = await _codeVortoService.DataDictionaryRepository
                    .GetDataFromSqlQuery<DataDictionary>(query)
                    .ConfigureAwait(false);

                var lstFileName = (from d in universeDataDictionary select d.FileName).Distinct().ToList();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.FileTypeExtensionId != 11
                                          && f.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                foreach (var fileMaster in fileMasters)
                {
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var allStatements = viewSourceMaster.SourceWithoutComments.Split('\n').ToList();
                    allStatements = allStatements.Distinct().ToList();

                    foreach (var fileName in lstFileName)
                    {
                        var recordRegExMatch = new Regex(@"(R.[a-zA-Z\d\.\s]+\(\d+\))|(R.[a-zA-Z\d\.\s]+\<[\d+,]+\>)");
                        var regexSecond = new Regex(@"(R." + fileName + @"[\(\<](\d+)[\)\>])", RegexOptions.IgnoreCase);
                        string fieldNo = string.Empty;
                        var crudActivityReport = new List<CrudActivityReport>();

                        #region Statements...

                        foreach (var statement in allStatements)
                        {
                            if (!recordRegExMatch.IsMatch(statement)) continue;
                            string objName = fileMaster.FileName;
                            string objType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            if (!regexSecond.IsMatch(statement)) continue;
                            foreach (Match match in regexSecond.Matches(statement))
                            {
                                var groupIndex = -1;
                                foreach (Group group in match.Groups)
                                {
                                    groupIndex++;
                                    if (groupIndex == 0) continue;
                                    if (string.IsNullOrEmpty(group.Value)) continue;
                                    var groupValue = group.Value;
                                    var match2 = Regex.Match(groupValue, @"[\(\<](\d+)[\)\>]");
                                    if (!match2.Success) continue;
                                    var matchedString = match2.Groups[1].Value;
                                    fieldNo = matchedString.TrimStart('0');
                                }

                                var no = fieldNo;
                                var universeDataSecond = await _codeVortoService.DataDictionaryRepository
                                    .GetItem<DataDictionary>(x => x.ProjectId == projectMaster.ProjectId &&
                                                                  x.FieldNo == no &&
                                                                  x.FileName == fileName).ConfigureAwait(false);
                                var attributeName = universeDataSecond != null
                                    ? universeDataSecond.FieldLabel : "";
                                var attributeDescription = universeDataSecond != null
                                    ? universeDataSecond.Description
                                    : "";
                                if (string.IsNullOrEmpty(attributeName) || string.IsNullOrEmpty(attributeDescription)) continue;

                                var newCrudActivity = new CrudActivityReport
                                {
                                    EntityName = fileName,
                                    ObjectName = objName,
                                    ObjectType = objType,
                                    AttributeName = attributeName,
                                    AttributeNumber = fieldNo,
                                    AttributeDescription = attributeDescription,
                                    OpenStatement = statement,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false
                                };

                                var crudActivity = crudActivityReport.Any(x =>
                                    x.ObjectName == newCrudActivity.ObjectName &&
                                    x.AttributeName == newCrudActivity.AttributeName &&
                                    x.AttributeNumber == newCrudActivity.AttributeNumber &&
                                    x.ProjectId == projectMaster.ProjectId);

                                if (crudActivity) continue;
                                crudActivityReport.Add(newCrudActivity);

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.CrudActivityReport.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                            }
                        }

                        #endregion
                    }
                }

                return Ok("Completed ProcessCrudActivityReport for Project: " + projectMaster.ProjectName);
            }
        }

        /*
        [HttpGet]
        public async Task<IHttpActionResult> InsertSystemLevelCrudActivityForUniverse(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var exceptEntityFileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                foreach (var fileMaster in exceptEntityFileMaster)
                {
                    // var entityName = Path.GetFileNameWithoutExtension(fMaster.FileName);
                    var sqlStatementQry =
                        " SELECT * FROM StatementReferenceMaster " +
                        " WHERE FileId = " + fileMaster.FileId + " AND BaseCommandId = 45 AND " +
                        " ProjectId = " + projectMaster.ProjectId + " " +
                        " AND SolutionId = " + projectMaster.SolutionId + ";";
                    var statementRefenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlStatementQry).ConfigureAwait(false);
                    if (!statementRefenceMaster.Any()) continue;

                    foreach (var sReference in statementRefenceMaster)
                    {
                        if (sReference.OriginalStatement.StartsWith("MATWRITE "))
                        {
                            if (!sReference.OriginalStatement.Contains(" TO ")) continue;
                            var sParts = sReference.OriginalStatement.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (part != "TO") continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                entityName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(entityName)) continue;

                                var newCrudActivity = new DbCrudActivity // CrudActivityReport
                                {
                                    EntityName = entityName,
                                    Delete = "N",
                                    InsertOrCreate = "Y",
                                    SelectOrRead = "N",
                                    Update = "Y",
                                    FileId = sReference.FileId,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false,
                                    IsOpen = false,
                                    OpenStatement = sReference.OriginalStatement
                                };

                                // using (var appDbContext = new AppDbContext())
                                // {
                                //    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                //    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                // }

                                await _codeVortoService.DbCrudActivityRepository.AddNewItem(newCrudActivity)
                                    .ConfigureAwait(false);
                                break;
                            }
                        }
                        if (!sReference.OriginalStatement.StartsWith("OPEN ")) continue;

                        var regExOpen = new Regex("(\".*?\")|(\'.*?\')", RegexOptions.IgnoreCase);
                        var beforeElseStatement = sReference.OriginalStatement
                                .Split(new[] { " ELSE " }, StringSplitOptions.None).First();
                        var openMatches = regExOpen.Matches(beforeElseStatement);
                        foreach (Group part in openMatches)
                        {
                            if (!(part.Value.StartsWith("'") || part.Value.StartsWith("\""))
                                && !(part.Value.EndsWith("'") || part.Value.StartsWith("\""))) continue;
                            if (string.IsNullOrEmpty(part.Value)) continue;
                            var objectName = part.Value.Replace("'", "").Replace("&", "").Replace("\"", "");
                            if (string.IsNullOrEmpty(objectName) || string.IsNullOrWhiteSpace(objectName)) continue;
                            objectName = objectName.Trim().TrimStart(',').TrimEnd(',');
                            var existingFiles = await _codeVortoService.FileMasterRepository
                                .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                                      f.SolutionId == projectMaster.SolutionId &&
                                                      f.FileName.StartsWith(objectName)).ConfigureAwait(false);
                            var actualFiles = (from f in existingFiles
                                               let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                               where fName == objectName
                                               select f).ToList();
                            var hasIndicator = !actualFiles.Any();

                            var newCrudActivity = new DbCrudActivity
                            {
                                EntityName = objectName,
                                IsOpen = true,
                                OpenStatement = sReference.OriginalStatement,
                                Delete = "N",
                                HasIndicator = hasIndicator,
                                InsertOrCreate = "N",
                                SelectOrRead = "Y",
                                Update = "N",
                                FileId = sReference.FileId,
                                ProjectId = sReference.ProjectId
                            };
                            // using (var appDbContext = new AppDbContext())
                            // {
                            //    appDbContext.DbCrudActivity.Add(newCrudActivity);
                            //    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            // }

                            await _codeVortoService.DbCrudActivityRepository.AddNewItem(newCrudActivity)
                                .ConfigureAwait(false);
                        }
                    }
                }
                
                Console.WriteLine("==============================================================================");
                Console.WriteLine("Done processing System Level CRUD Activity for Project: " + projectMaster.ProjectName);
                Console.WriteLine("==============================================================================");
                return Ok("Done processing System Level CRUD Activity for Project: " + projectMaster.ProjectName);
            }
        }
        */

        public ActionWorkflowDetails GiveMeActionWorkFlowDetail(List<ActionWorkflows> workflowRef, ActionWorkflows workflow,
            int parentRowId, int projectId)
        {
            var enable = workflowRef.Count(x => x.IsDeleted == 0);
            var hidden = workflowRef.Count(x => x.IsDeleted == 1);
            var count = "<b style='color: #7030a0;'>" + " (Total Workflows: " + workflowRef.Count +
                        " | Enabled: " + enable + " | Hidden: " + hidden + ")" + "</b>";
            var graphId = "graph_" + parentRowId;
            var objConnect = "btnObj_" + workflow.ActionWorkflowId;
            var download = "btnDowload_" + workflow.ActionWorkflowId;
            var fileId = workflow.FileId;

            /*
            var callExternal = lstStateRef
                .Count(d => (d.BaseCommandId == 6) && (d.FileId == workflow.FileId));
            var callInternal = lstStateRef
                .Count(d => (d.BaseCommandId == 5) && (d.FileId == workflow.FileId));

            var decisionCount = lstStateRef.Count(d => (d.BaseCommandId == 1) &&
                                                       (d.FileId == workflow.FileId)) + 1;
            */
            string disabled = string.Empty;
            string btnStyle = "style='margin-top :5px;height: 31px;'";
            string pageUrl = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                             workflow.MethodStatementId + "&aId=" + workflow.ActionWorkflowId + "";
            string downloadReqDoc = "onclick = downloadRequirementDoc(" + workflow.ActionWorkflowId + ");";
            string onClickUploadFile = "onclick=uploadFilePopupShow('" +
                                       workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") +
                                       "'," + workflow.ActionWorkflowId + ");";
            string onClickRename = "onclick=funWorkflowRename('" +
                                   workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") + "'," +
                                   workflow.ActionWorkflowId + ");";
            var disable = "<button id=" + workflow.ActionWorkflowId +
                          " style='margin-top: 5px;' class='btn btn-info btn-icon icon-lg fa fa-trash' onclick='workFlowDisable(" +
                          workflow.ActionWorkflowId + ")'></button>";
            var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                       "<button " + btnStyle + " " + disabled +
                       " class='btn btn-mint'>View</button> </a>" +
                       " &nbsp;<a href='javascript:void(0);'><button style='margin-top : 5px;height: 31px;' class='btn btn-mint' " +
                       onClickRename + " >Rename</button> </a>&nbsp;<button id=" +
                       workflow.ActionWorkflowId + " style='margin-top : 4px;' " +
                       "class='btn btn-mint btn-icon icon-lg fa fa-upload' " + onClickUploadFile +
                       " title='Upload image/file(s)'></button>&nbsp;" +
                       "<button id=" + download +
                       " style='margin-bottom: -8px; margin-top: -3px;height: 31px;'" +
                       " class='btn btn-primary btn-icon icon-lg fa fa-download' " + downloadReqDoc +
                       " title='Download Requirement Specification Document(.docx)' ></button>&nbsp;" +
                       "<button id=" + objConnect +
                       " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-success btn-icon icon-lg fa fa-sitemap' " +
                       "onclick='downloadObjectConnectivityIndividualFlowchartUniverse(" +
                       fileId + ", " + workflow.ActionWorkflowId +
                       ");' title='Download object connectivity.'></button>";

            var workflowDetails = new ActionWorkflowDetails
            {
                DecisionCount = 0, //decisionCount,
                ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                View = view,
                OriginObject = workflow.OriginObject.ToUpper(),
                WorkflowName = workflow.WorkflowName.ToUpper(),
                ProjectName = workflow.ProjectMaster.ProjectName,
                ActionWorkflowId = workflow.ActionWorkflowId,
                Disable = disable,
                IsDeleted = workflow.IsDeleted,
                ShortDetails = count,
                GraphId = graphId,
                ParentId = "-1",
                ProjectId = projectId
            };
            return workflowDetails;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessActionWorkflowDetails(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allActionWorkFlows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);
                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                int solutionId = projectDetails.SolutionId ?? 0;

                // var allClassNameDeclared = new List<StatementReferenceMaster>();
                //var allClassNameDeclaredNew = await _codeVortoService.StatementReferenceMasterRepository
                //    .GetAllListItems(s =>
                //        (s.BaseCommandId == 19 || s.BaseCommandId == 1 || s.BaseCommandId == 6 ||
                //         s.BaseCommandId == 5) && s.ProjectId == projectId).ConfigureAwait(false);
                /*
                var allClassNameDeclaredNew = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(s =>
                        (s.BaseCommandId == 19 ) && s.ProjectId == projectId).ConfigureAwait(false);
                allClassNameDeclared.AddRange(allClassNameDeclaredNew);
                var allIFStatement = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllItems(s =>
                        (s.BaseCommandId == 1) && s.ProjectId == projectId).ConfigureAwait(false);
                allClassNameDeclared.AddRange(allIFStatement);
               var callExternAndInternal =  await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(s =>
                        ( s.BaseCommandId == 6 || s.BaseCommandId == 5) && s.ProjectId == projectId).ConfigureAwait(false);
               allClassNameDeclared.AddRange(callExternAndInternal);
                */

                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                };
                var allClassNameDeclared =
                    await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllStatementForWorkFlowProcess", parameters)
                        .ConfigureAwait(false);

                // var allClassNameDeclared = new List<StatementReferenceMaster>();

                var fileMasterData = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.SolutionId == solutionId && f.ProjectId == projectDetails.ProjectId)
                    .ConfigureAwait(false);
                int graphId = 0;
                int childsGraphId = 0;

                Console.WriteLine("=============================================================");
                Console.WriteLine("Total Action Workflows to process: " + allActionWorkFlows.Count);
                Console.WriteLine("=============================================================");
                foreach (var actionWorkflow in allActionWorkFlows)
                {
                    graphId++;
                    // if(actionWorkflow.ActionWorkflowId <= 2673) continue;
                    Console.WriteLine("Action Workflows Remaining: " + (allActionWorkFlows.Count - graphId));
                    Console.WriteLine("=============================================================");

                    childsGraphId++;
                    // Console.WriteLine(graphId);
                    Thread.Sleep(100);
                    int fileMenuId = actionWorkflow.FileMenuId;
                    if (actionWorkflow.FileId == 0)
                    {
                        var emptyActionDetails = GiveMeActionWorkFlowDetail(allActionWorkFlows,
                            actionWorkflow, graphId, projectId);
                        emptyActionDetails.GraphId = "EmptyGraph_" + graphId;
                        emptyActionDetails.ParentId = "-1";
                        emptyActionDetails.ObjectType = "Menu";
                        emptyActionDetails.FileMenuId = fileMenuId;
                        await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(emptyActionDetails)
                            .ConfigureAwait(false);
                        Thread.Sleep(10);
                        continue;
                    }
                    int fileId = actionWorkflow.FileId;
                    var currentJclFile = fileMasterData.First(f => f.FileId == fileId);
                    var objectType = currentJclFile.FileTypeExtensionReference.FileTypeName;
                    // main workflow
                    var aDetails = GiveMeActionWorkFlowDetail(allActionWorkFlows, actionWorkflow,
                        graphId, projectId);
                    aDetails.GraphId = "Graph_" + graphId;
                    aDetails.ParentId = "-1";
                    aDetails.ObjectType = "Menu"; // objectType;
                    aDetails.WorkflowName = actionWorkflow.OriginEventMethod;
                    aDetails.FileMenuId = fileMenuId;
                    await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(aDetails).ConfigureAwait(false);
                    Thread.Sleep(100);
                    // main jcl
                    string disabled = string.Empty;
                    var jclGraphId = "JclGraphId_" + graphId;
                    string btnStyle = "style='margin-top :5px;height: 31px;'";
                    string pageUrl = "customview.html?prjId=" + projectId + "&fileId=" + fileId + "";
                    string view = "<a " + disabled + " href=javascript:window.open('" + pageUrl + "')>" +
                                  "<button " + btnStyle + " " + disabled + " class='btn btn-mint'>View</button>";

                    var fileStatements = File.ReadAllLines(currentJclFile.FilePath).ToList();
                    fileStatements.RemoveAll(s => s.Length <= 0);
                    fileStatements = fileStatements.Select(s => s.Trim()).ToList();
                    var fileBusinessName = fileStatements.First().Trim('*').StartsWith("PA ")
                        ? fileStatements.First().Trim('*').Replace("PA ", "").Trim()
                        : fileStatements.First().Trim('*').Trim();

                    var workflowDetails = new ActionWorkflowDetails
                    {
                        DecisionCount = 0, // aDetails.DecisionCount,
                        ExtrernalCalls = "-", // aDetails.ExtrernalCalls,
                        InternalCalls = "-", // aDetails.InternalCalls,
                        View = view,
                        OriginObject = Path.GetFileNameWithoutExtension(currentJclFile.FilePath),
                        WorkflowName = fileBusinessName, // Path.GetFileNameWithoutExtension(currentJclFile.FilePath),
                        ProjectName = currentJclFile.ProjectMaster.ProjectName,
                        ActionWorkflowId = 0,
                        ShortDetails = "0",
                        ParentId = aDetails.GraphId,
                        GraphId = jclGraphId,
                        ProjectId = projectId,
                        ObjectType = objectType,
                        FileMenuId = fileMenuId
                    };
                    await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails).ConfigureAwait(false);
                    Thread.Sleep(100);
                    var allCallExt = allClassNameDeclared
                        .Where(s => s.FileId == currentJclFile.FileId && s.BaseCommandId == 6)
                        .OrderBy(s => s.StatementId).ToList();
                    var allClasses = (from a in allCallExt select a.ClassCalled).ToList();

                    foreach (var aClass in allClasses)
                    {
                        var allFiles = (from s in allClassNameDeclared
                                        where aClass == s.ClassNameDeclared
                                        select s).ToList();

                        foreach (var file in allFiles)
                        {
                            Thread.Sleep(500);
                            if (fileMasterData.All(f => f.FileId != file.FileId)) continue;
                            var fileMaster = fileMasterData.First(f => f.FileId == file.FileId);

                            var fileStatements1 = File.ReadAllLines(fileMaster.FilePath).ToList();
                            fileStatements1.RemoveAll(s => s.Length <= 0);
                            fileStatements1 = fileStatements1.Select(s => s.Trim()).ToList();
                            var fileBusinessName1 = fileStatements1.First().Trim('*').StartsWith("PA ")
                                ? fileStatements1.First().Trim('*').Replace("PA ", "").Trim()
                                : fileStatements1.First().Trim('*').Trim();

                            if (fileMaster.FileTypeExtensionId == 9)
                            {
                                // add new actionWorkflowDetails...
                                string pageUrl1 = "customview.html?prjId=" + projectId + "&fileId=" +
                                                  fileMaster.FileId + "";
                                var view1 = "<a " + disabled + " href=javascript:window.open('" + pageUrl1 + "')>" +
                                            "<button " + btnStyle + " " + disabled +
                                            " class='btn btn-mint'>View</button>";
                                /*
                                var callExternal = allClassNameDeclared
                                    .Count(d => (d.BaseCommandId == 6) && (d.FileId == fileMaster.FileId));
                                var callInternal = allClassNameDeclared
                                    .Count(d => (d.BaseCommandId == 5) && (d.FileId == fileMaster.FileId));

                                var decisionCount = allClassNameDeclared.Count(d => (d.BaseCommandId == 1) &&
                                                                                    (d.FileId == fileMaster.FileId)) + 1;
                                */

                                objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                                var workflowDetails1 = new ActionWorkflowDetails
                                {
                                    DecisionCount = 0, //decisionCount,
                                    ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                                    InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                                    View = view1,
                                    OriginObject = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                    WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                    ProjectName = fileMaster.ProjectMaster.ProjectName,
                                    ActionWorkflowId = 0,
                                    ShortDetails = "0",
                                    ParentId = workflowDetails.GraphId,
                                    GraphId = "ProgramGraphId_" + graphId + fileMaster.FileId,
                                    ProjectId = projectId,
                                    ObjectType = objectType,
                                    FileMenuId = fileMenuId
                                };
                                // Console.WriteLine("Program");
                                await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                    .ConfigureAwait(false);
                                Thread.Sleep(1000);
                                continue;
                            } // Program
                            if (fileMaster.FileTypeExtensionId == 10)
                            {
                                // add new actionWorkflowDetails...
                                objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                                string jclFileGraphId = "NewJclGraph_" + graphId + fileMaster.FileId;
                                string pageUrl1 = "customview.html?prjId=" + projectId + "&fileId=" +
                                                  fileMaster.FileId + "";
                                var view1 = "<a " + disabled + " href=javascript:window.open('" + pageUrl1 + "')>" +
                                            "<button " + btnStyle + " " + disabled +
                                            " class='btn btn-mint'>View</button>";

                                /*
                                var callExternal = allClassNameDeclared
                                    .Count(d => (d.BaseCommandId == 6) && (d.FileId == fileMaster.FileId));
                                var callInternal = allClassNameDeclared
                                    .Count(d => (d.BaseCommandId == 5) && (d.FileId == fileMaster.FileId));

                                var decisionCount = allClassNameDeclared.Count(d => (d.BaseCommandId == 1) &&
                                                                                    (d.FileId == fileMaster.FileId)) + 1;
                                */

                                var workflowDetails1 = new ActionWorkflowDetails
                                {
                                    DecisionCount = 0, //decisionCount,
                                    ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                                    InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                                    View = view1,
                                    OriginObject = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                    WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                    ProjectName = fileMaster.ProjectMaster.ProjectName,
                                    ActionWorkflowId = 0,
                                    ShortDetails = "0",
                                    ParentId = jclGraphId,
                                    GraphId = jclFileGraphId,
                                    ProjectId = projectId,
                                    ObjectType = objectType,
                                    FileMenuId = fileMenuId
                                };
                                // Console.WriteLine("JCL");
                                await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                    .ConfigureAwait(false);

                                if (fileMaster.FileId == file.FileId) continue;

                                Thread.Sleep(600);
                                childsGraphId = await FaktJclToProgramChaCode(fileMaster, fileMasterData, allClassNameDeclared,
                                    jclFileGraphId, childsGraphId, fileMenuId).ConfigureAwait(false);
                                childsGraphId = childsGraphId + 1;

                            } // JCL
                        }
                    }
                }

                return Ok("Done");
            }
        }

        private async Task<int> FaktJclToProgramChaCode(FileMaster fileMaster, List<FileMaster> fileMasterData,
            List<StatementReferenceMaster> allClassNameDeclared, string parentId, int childsGraphId, int fileMenuId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allCallExt = await _codeVortoService.StatementReferenceMasterRepository.GetAllListItems(
                    s => s.FileId == fileMaster.FileId && s.BaseCommandId == 6).ConfigureAwait(false);
                allCallExt = allCallExt.OrderBy(s => s.StatementId).ToList();
                string btnStyle = "style='margin-top :5px;height: 31px;'";
                var allClasses = (from a in allCallExt select a.ClassCalled).ToList();

                foreach (var aClass in allClasses)
                {
                    Thread.Sleep(100);
                    var allFiles = (from s in allClassNameDeclared
                                    where aClass == s.ClassNameDeclared
                                    select s).ToList();

                    foreach (var file in allFiles)
                    {
                        Thread.Sleep(200);
                        if (fileMasterData.All(f => f.FileId != file.FileId)) continue;
                        var thisFileMaster = fileMasterData.First(f => f.FileId == file.FileId);

                        var fileStatements1 = File.ReadAllLines(fileMaster.FilePath).ToList();
                        fileStatements1.RemoveAll(s => s.Length <= 0);
                        fileStatements1 = fileStatements1.Select(s => s.Trim()).ToList();
                        var fileBusinessName1 = fileStatements1.First().Trim('*').StartsWith("PA ")
                            ? fileStatements1.First().Trim('*').Replace("PA ", "").Trim()
                            : fileStatements1.First().Trim('*').Trim();

                        if (thisFileMaster.FileTypeExtensionId == 9)
                        {
                            // add new actionWorkflowDetails...
                            string pilluChaGraphId = "PgmGraphId_" + ++childsGraphId + thisFileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + thisFileMaster.ProjectId + "&fileId=" +
                                              thisFileMaster.FileId + "";
                            var view1 = "<a href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " class='btn btn-mint'>View</button>";
                            string objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0,
                                ExtrernalCalls = "No",
                                InternalCalls = "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                ProjectName = thisFileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = parentId,
                                GraphId = pilluChaGraphId,
                                ProjectId = thisFileMaster.ProjectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("Program");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);
                            Thread.Sleep(200);
                            continue;
                        } // Program
                        if (thisFileMaster.FileTypeExtensionId == 10)
                        {
                            // add new actionWorkflowDetails...
                            string jclFileGraphId = "PilluJclGraph_" + ++childsGraphId + thisFileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + thisFileMaster.ProjectId + "&fileId=" +
                                              thisFileMaster.FileId + "";
                            var view1 = "<a  href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " class='btn btn-mint'>View</button>";
                            string objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0,
                                ExtrernalCalls = "No",
                                InternalCalls = "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                ProjectName = thisFileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = parentId,
                                GraphId = jclFileGraphId,
                                ProjectId = thisFileMaster.ProjectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("JCL");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);

                            if (thisFileMaster.FileId == file.FileId) break;

                            Thread.Sleep(500);
                            await FaktJclToProgramChaCode(thisFileMaster, fileMasterData, allClassNameDeclared,
                                jclFileGraphId, childsGraphId, fileMenuId);
                            Console.WriteLine("JCL");
                        } // JCL
                    }
                }
                return childsGraphId;
            }
        }
    }
}
