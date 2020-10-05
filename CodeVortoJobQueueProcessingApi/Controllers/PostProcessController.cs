using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityClasses;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class PostProcessController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public PostProcessController()
        {
        }

        public PostProcessController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public List<TreeViewData> GetAllStartingPoints(int projectId, int stmtId, string opt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var lstTreeViewData = new List<TreeViewData>();
                var lstNodes = new List<Node>();
                var lstLinks = new List<Link>();
                var startClasses =
                    _codeVortoService.ActionWorkflowsRepository.GetAllItems(p => p.MethodStatementId != 0).Result;
                var clsName = (from s in startClasses where s.MethodStatementId == stmtId select s).ToList().First();
                var autoNodeId = 500;
                lstNodes.Add(new Node
                {
                    Name = clsName.OriginObject,
                    Id = 1111,
                    ParentId = 0,
                    ShapeId = "RoundRect",
                    Color = "#28c965",
                    Height = "15",
                    Width = clsName.OriginObject.Length.ToString()
                });
                var workflowRef = GetMethodBlock(stmtId);

                var treeStackStart = new Stack<StatementReferenceMaster>();
                treeStackStart.Push(workflowRef.FirstOrDefault());

                workflowRef.RemoveAt(0);
                var linkCounter = 1;
                //int ifCounter = 0;
                //bool done = false;
                foreach (var statementM in workflowRef)
                {
                    //This is call external and will have its child tree from here...
                    if (statementM.BaseCommandId != 6) continue;

                    if (statementM.ClassCalled != null)
                    {
                        var className = statementM.ClassCalled.Split('.').Last();
                        var callExtExpandedCode = GetGenericBlock(className);

                        var sz1 = (from k in lstNodes where k.Name == statementM.ClassCalled select k).ToList();
                        if (!sz1.Any())
                        {
                            Node lastNode = null;
                            if (lstNodes.Last().ShapeId == "Decision")
                                lastNode = lstNodes.Last();
                            lstNodes.Add(new Node
                            {
                                Id = autoNodeId,
                                Name = statementM.ClassCalled,
                                ShapeId = "RoundRect",
                                Color = "#f5bd6a",
                                Height = "15",
                                Width = statementM.ClassCalled.Length.ToString()
                            });
                            if (lastNode != null)
                                lstLinks.Add(new Link
                                {
                                    Origin = autoNodeId - 1,
                                    Target = autoNodeId,
                                    LinkText = "[ " + linkCounter + " ] Yes"
                                });
                            else
                                lstLinks.Add(new Link
                                {
                                    Origin = 1111,
                                    Target = autoNodeId,
                                    LinkText = "[ " + linkCounter + " ] " +
                                               statementM.MethodCalled.Substring(0,
                                                   statementM.MethodCalled.IndexOf('('))
                                });
                            autoNodeId++;
                            linkCounter++;
                        }
                        else
                        {
                            var k = statementM.MethodCalled.Substring(0, statementM.MethodCalled.IndexOf('('));
                            var tnk = (from l in lstLinks
                                where (l.Origin == 1111) && (l.LinkText == k)
                                select l).ToList();
                            if (!tnk.Any())
                            {
                            }
                        }
                        // Now try to find out body for external call...
                        if (callExtExpandedCode.Count == 0)
                        {
                        }
                        else
                        {
                            foreach (var statmentM in callExtExpandedCode)
                            {
                                if (!statementM.MethodCalled.StartsWith(statmentM.MethodName)) continue;
                                var blockStmtId = statmentM.StatementId;
                                var stmtsBlocks = GetMethodBlock(blockStmtId);

                                foreach (var block in stmtsBlocks)
                                {
                                    if (block.BaseCommandId != 6) continue;
                                    var sz =
                                        (from s in lstNodes where s.Name == block.ClassCalled select s).ToList();
                                    if (!sz.Any())
                                    {
                                        lstNodes.Add(new Node
                                        {
                                            Id = autoNodeId,
                                            Name = block.ClassCalled,
                                            BaseCommandId = statementM.BaseCommandId,
                                            ShapeId = "RoundRect",
                                            Color = "#f5bd6a",
                                            Height = "15",
                                            Width = block.ClassCalled.Length.ToString()
                                        });
                                        var block1 = block;
                                        var asz =
                                            (from n in lstNodes where n.Name == block1.ClassCalled select n)
                                                .ToList();
                                        var ss =
                                            (from t in lstNodes where t.Name == statementM.ClassCalled select t)
                                                .ToList();
                                        lstLinks.Add(new Link
                                        {
                                            Origin = ss.First().Id,
                                            Target = asz.First().Id,
                                            LinkText = "[ " + linkCounter + " ] " +
                                                       block.MethodCalled.Substring(0,
                                                           block.MethodCalled.IndexOf('('))
                                        });
                                        autoNodeId++;
                                        linkCounter++;
                                    }
                                    else
                                    {
                                        var asz =
                                            (from n in lstNodes where n.Name == statementM.ClassCalled select n)
                                                .ToList();
                                        lstLinks.Add(new Link
                                        {
                                            Origin = asz.First().Id,
                                            Target = sz.First().Id,
                                            LinkText = "[ " + linkCounter + " ] " +
                                                       block.MethodCalled.Substring(0,
                                                           block.MethodCalled.IndexOf('('))
                                        });
                                        linkCounter++;
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        var stmtsBlock = GetMethodBlock(statementM.StatementId);

                        if (stmtsBlock.Count == 0)
                        {
                        }
                        linkCounter += stmtsBlock.Count(block => block.BaseCommandId == 6);
                    }
                }

                var copyOfLinks = lstLinks;
                foreach (var node in lstNodes)
                {
                    var childNodes = (from lnk in lstLinks
                        where lnk.Origin == node.Id
                        select lnk.Target
                        into p
                        select lstNodes.Find(n => n.Id == p)).ToList();
                    if (childNodes.Count > 1)
                    {
                        var duplicates = childNodes.GroupBy(x => x)
                            .Where(g => g.Count() > 1).Select(g => g.Key).ToList();
                        if ((duplicates.Count <= 0) || (duplicates[0] == null)) continue;

                        var links =
                            (from h in lstLinks
                                    where (h.Origin == node.Id) && (h.Target == duplicates.First().Id)
                                    select h)
                                .ToList();
                        if (!links.Any()) continue;

                        var linkText = links.Aggregate(string.Empty, (current, jk) => current + jk.LinkText + ", ");
                        linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                        links.First().LinkText = linkText;
                        foreach (var l in links)
                        {
                            if (l.LinkText == linkText) continue;
                            copyOfLinks.Remove(l);
                        }
                    }
                }
                var treeViewData = new TreeViewData
                {
                    Links = copyOfLinks,
                    Nodes = lstNodes
                };
                lstTreeViewData.Add(treeViewData);
                return lstTreeViewData;
            }
        }

        public void AppendNodeToExisting(string statementId,
            StatementReferenceMaster statementM, List<Link> lstLinks, List<Node> lstNodes, int autoNodeId,
            ref int linkCounter)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var className = statementM.ClassCalled.Split('.').Last();
                var callExtExpandedCode = GetGenericBlock(className);
                if (callExtExpandedCode.Count == 0)
                {
                }
                else
                {
                    foreach (var statementK in callExtExpandedCode)
                    {
                        if (!statementM.MethodCalled.StartsWith(statementK.MethodName)) continue;
                        var blockStmtId = statementK.StatementId;
                        var stmtsBlock = GetMethodBlock(blockStmtId);
                        foreach (var block in stmtsBlock)
                            if (block.BaseCommandId == 6)
                            {
                                var block1 = block;
                                var sz = (from t in lstNodes where t.Name == block1.ClassCalled select t).ToList();
                                if (!sz.Any())
                                {
                                    lstNodes.Add(new Node
                                    {
                                        Id = autoNodeId,
                                        Name = block.ClassCalled,
                                        ParentId = 0,
                                        BaseCommandId = block.BaseCommandId,
                                        ShapeId = "RoundRect",
                                        Color = "#f5bd6a",
                                        Height = "15",
                                        Width = "100"
                                    });
                                    //var sz1 = (from f in lstNodes where f.Name == statementM.ClassCalled select f).ToList();
                                    lstLinks.Add(new Link
                                    {
                                        Target = statementM.StatementId,
                                        Origin = lstNodes.Last().Id,
                                        LinkText = "[ " + linkCounter + " ] " +
                                                   statementM.MethodCalled.Substring(0,
                                                       statementM.MethodCalled.IndexOf('('))
                                    });
                                    linkCounter++;
                                }
                                else
                                {
                                    var asz = (from n in lstNodes where n.Name == statementM.ClassCalled select n)
                                        .ToList();
                                    lstLinks.Add(new Link
                                    {
                                        Origin = asz.First().Id,
                                        Target = sz.First().Id,
                                        LinkText = "[ " + linkCounter + " ] " +
                                                   block.MethodCalled.Substring(0, block.MethodCalled.IndexOf('('))
                                    });
                                    linkCounter++;
                                }
                            }
                    }
                }
            }
        }

        public async Task<IHttpActionResult> GetWorkflow(int projectId, int stmtId)
        {
            var clsWorkflowWorkspaceData = new ClsWorkflowWorkspaceData();
            var dataSet = await clsWorkflowWorkspaceData.GetWorkspaceWorkflowData(projectId, stmtId);

            var workflowNodeDetails =
                dataSet.Tables[2].AsEnumerable().Select(f => new WorkflowNodeDetails
                {
                    RowId = int.Parse(f.ItemArray[0].ToString()),
                    Id = f.Field<int>("Id"),
                    Name = f.Field<string>("Name"),
                    ShapeId = f.Field<string>("ShapeId"),
                    Height = string.IsNullOrEmpty(f.Field<string>("Height")) ? "" : f.Field<string>("Height"),
                    Width = string.IsNullOrEmpty(f.Field<string>("Width")) ? "" : f.Field<string>("Width"),
                    Color = f.Field<string>("Color"),
                    ParentId = f.Field<int>("ParentId"),
                    ChildId = f.Field<int>("ChildId"),
                    ProjectId = f.Field<int>("ProjectId"),
                    ActionWorkflowId = f.Field<int>("ActionWorkflowId"),
                    WorkflowStartStatementId = f.Field<int>("WorkflowStartStatementId"),
                    StatementTypeId = f.Field<int>("StatementTypeId"),
                    FileId = f.Field<int>("FileId"),
                    StatementId = f.Field<int>("StatementId"),
                    BaseCommandId = f.Field<int>("BaseCommandId"),
                    BusinessName =
                        string.IsNullOrEmpty(f.Field<string>("BusinessName")) ? "" : f.Field<string>("BusinessName"),
                    BusinessDescription =
                        string.IsNullOrEmpty(f.Field<string>("BusinessDescription"))
                            ? ""
                            : f.Field<string>("BusinessDescription")
                }).ToList();

            var workflowLinkDetails =
                dataSet.Tables[3].AsEnumerable().Select(f => new WorkflowLinkDetails
                {
                    RowId = f.Field<int>("RowId"),
                    Origin = f.Field<int>("Origin"),
                    Target = f.Field<int>("Target"),
                    LinkText = f.Field<string>("LinkText"),
                    StatementId = f.Field<int>("StatementId"),
                    BaseCommandId = f.Field<int>("BaseCommandId"),
                    BusinessName = f.Field<string>("BusinessName"),
                    BusinessDescription = f.Field<string>("BusinessDescription"),
                    ActionWorkflowId = f.Field<int>("ActionWorkflowId"),
                    WorkflowStartStatementId = f.Field<int>("WorkflowStartStatementId"),
                    ProjectId = f.Field<int>("ProjectId")
                }).ToList();
            var workflowTreeviewSecondTabDetails =
                dataSet.Tables[1].AsEnumerable().Select(f => new WorkflowTreeviewSecondTabDetails
                {
                    ProjectId = f.Field<int>("ProjectId"),
                    BaseCommandId = f.Field<int>("BaseCommandId"),
                    RowId = f.Field<int>("RowId"),
                    ActionWorkflowId = f.Field<int>("ActionWorkflowId"),
                    WorkflowStartStatementId = f.Field<int>("WorkflowStartStatementId"),
                    ParentId = f.Field<string>("ParentId"),
                    MethodCalled = f.Field<string>("MethodCalled"),
                    PrimaryCommandId = f.Field<int>("PrimaryCommandId"),
                    ActualStatementId = f.Field<string>("ActualStatementId"),
                    GraphName = f.Field<string>("GraphName"),
                    ClassCalled = f.Field<string>("ClassCalled"),
                    HasChild = f.Field<string>("HasChild"),
                    GraphId = f.Field<string>("GraphId"),
                    SpriteCssClass = f.Field<string>("SpriteCssClass")
                }).ToList();
            var workflowTreeviewTabFirstDetails =
                dataSet.Tables[0].AsEnumerable().Select(f => new WorkflowTreeviewTabFirstDetails
                {
                    ProjectId = f.Field<int>("ProjectId"),
                    BaseCommandId = f.Field<int>("BaseCommandId"),
                    RowId = f.Field<int>("RowId"),
                    ActionWorkflowId = f.Field<int>("ActionWorkflowId"),
                    WorkflowStartStatementId = f.Field<int>("WorkflowStartStatementId"),
                    ParentId = f.Field<string>("ParentId"),
                    MethodCalled = f.Field<string>("MethodCalled"),
                    PrimaryCommandId = f.Field<int>("PrimaryCommandId"),
                    ActualStatementId = f.Field<string>("ActualStatementId"),
                    GraphName = f.Field<string>("GraphName"),
                    ClassCalled = f.Field<string>("ClassCalled"),
                    HasChild = f.Field<string>("HasChild"),
                    GraphId = f.Field<string>("GraphId"),
                    SpriteCssClass = f.Field<string>("SpriteCssClass")
                }).ToList();
            var actionWorkflow = dataSet.Tables[4].AsEnumerable().Select(f => new ActionWorkflows
            {
                MethodStatementId = f.Field<int>("MethodStatementId"),
                ProjectId = f.Field<int>("ProjectId"),
                ActionWorkflowId = f.Field<int>("ActionWorkflowId"),
                WorkflowName = f.Field<string>("WorkflowName"),
                CreatedBy = f.Field<int?>("CreatedBy"),
                CreatedOn = f.Field<DateTime?>("CreatedOn"),
                EndPointOrService = f.Field<string>("EndPointOrService"),
                OriginEventMethod = f.Field<string>("OriginEventMethod"),
                OriginFileName = f.Field<string>("OriginFileName"),
                OriginFilePath = f.Field<string>("OriginFilePath"),
                OriginObject = f.Field<string>("OriginObject"),
                RelatedScreenControl = f.Field<string>("RelatedScreenControl"),
                ServiceBaseAddress = f.Field<string>("ServiceBaseAddress"),
                ServiceContract = f.Field<string>("ServiceContract"),
                WorkflowBusinessDescription = f.Field<string>("WorkflowBusinessDescription"),
                WorkflowBusinessName = f.Field<string>("WorkflowBusinessName")
            }).FirstOrDefault();
            var lstData = new List<WorkFlowData>();
            var treeViewDataNew = new WorkFlowData
            {
                Nodes = workflowNodeDetails,
                Links = workflowLinkDetails,
                TreeViewListFirstTab = workflowTreeviewTabFirstDetails,
                TreeViewListSecondTab = workflowTreeviewSecondTabDetails,
                ActionWorkflows = actionWorkflow
            };
            lstData.Add(treeViewDataNew);
            return Ok(lstData);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                //var lstTreeViewData = new List<TreeViewData>();
                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();

                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                        .GetAllItems(p => (p.MethodStatementId != 0) && (p.ProjectId == projectId));
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                Expression<Func<StatementReferenceMaster, bool>> expression =
                    master => (master.ProjectId == projectId) && (master.ClassNameDeclared == clsName);
                //.Split('.').LastOrDefault();
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(expression, 1);
                //lstStatementMaster.Add(baseStatementMaster);
                //lstNodes.Add(new Node { Name = clsName, Id = 1111, ParentId = 0, ShapeId = "RoundRect", Color = "#28c965", Height = "15", Width = clsName.Length.ToString() });

                var workflowRef = GetMethodBlock(stmtId);

                var bId = workflowRef[0].BaseCommandId;
                var statementId = workflowRef[0].StatementId;
                // This is class name where that method is defined...
                lstTreeView.Add(new TreeView
                {
                    GraphId = "StartNode_1",
                    GraphName = "<span class='nodeToBold'>" + baseStatementMaster.OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster.BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = clsName,
                    ActualStatementId = "Actual_" + baseStatementMaster.StatementId
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
                    ActualStatementId = "Actual_" + statementId
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
                    StatementReferenceMaster = statementMaster
                }));
                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);
                var auto = 0;

                foreach (var treeItem in lstTreeView)
                {
                    if ((treeItem.BaseCommandId != 5) && (treeItem.StatementReferenceMaster.OtherBaseCommandId != 5))
                        continue;

                    auto++;
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                        ref auto);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                var newList = copyOfLstTreeView.ToList();
                foreach (var treeItem in newList)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                    auto++;
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                        ref auto);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                    /*
                    copyOfLstTreeView.Add(new TreeView
                    {
                        ParentId = treeItem.GraphId,
                        GraphId = "CallExt_" + treeItem.StatementReferenceMaster.StatementId,
                        HasChild = true,
                        GraphName = "<span class='nodemissingelement'>&nbsp;Loading...&nbsp;</span>",
                        BaseCommandId = 500,
                        SpriteCssClass = treeItem.StatementReferenceMaster.StatementId.ToString()
                    });
                    treeItem.HasChild = true;
                    */
                }

                #region Extra blocks...

                var indexPosition = -1;
                lstTreeView = copyOfLstTreeView;
                var ifCounter = 0;
                string[] colorArray = {"#2998fb", "#1dc5d8", "#860f0f", "#b80ee0"};
                var tempId = 0;

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
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" +
                                                 treeItem.GraphName +
                                                 "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView,
                                colorArray[tempId]);
                            tempId++;
                        }
                        else
                            child.GraphName = "<span style='color: green;'>" + child.GraphName +
                                              "</span>";
                    }
                    //tempId++;
                }
                /*
                var allOtherCommand = copyOfLstTreeView.FindAll(s => s.StatementReferenceMaster.OtherBaseCommandId == 5);
                var oChildItems =
                       (from s in copyOfLstTreeView where s.ParentId == allOtherCommand[0].GraphId select s).ToList();
                foreach (var cItem in oChildItems)
                {
                    var m = cItem.GraphName;
                }
                */

                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                    if ((treeItem.BaseCommandId != 1) && (treeItem.PrimaryCommandId != 1)) continue;

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
                    var prevParentId = treeViewList.First().ParentId;
                    var graphId = "IfBlockStart_" + indexPosition;
                    treeViewList.First().GraphId = graphId;
                    for (var j = 1; j < treeViewList.Count; j++)
                        if (treeViewList[j].ParentId == prevParentId)
                            treeViewList[j].ParentId = graphId;
                }

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));
                secondTab.AddRange(copyOfLstTreeView
                    .Where(item => (item.BaseCommandId == 6) || (item.BaseCommandId == 2) ||
                                   (item.BaseCommandId == 1) || (item.BaseCommandId == 25)
                                   || (item.PrimaryCommandId == 1) || (item.BaseCommandId == 5)
                                   || (item.PrimaryCommandId == 2)));
                /*
                var tempList =
                    (from d in secondTab
                     where d.BaseCommandId == 1
                         || d.BaseCommandId == 2
                         || d.BaseCommandId == 25
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in secondTab where s.ParentId == sTab.GraphId select s).ToList();
                    if (sTab.BaseCommandId == 2 || childItems.Count == 1)
                        secondTab.Remove(sTab);
                    if (childItems.Any(k => k.BaseCommandId == 25)) continue;
                    if (!childItems.Any() && sTab.StatementReferenceMaster == null)
                        secondTab.Remove(sTab);
                }
                
                var tempListNew = secondTab.ToList().FindAll(k => k.BaseCommandId == 1);
                foreach (var sTab in tempListNew)
                {
                    var allChilds = new List<TreeView> { sTab };
                    var childItems = (from s in secondTab where s.ParentId == sTab.GraphId select s).ToList();
                    if (!childItems.Any())
                        secondTab.Remove(sTab);
                    else
                    {
                        foreach (var c in childItems)
                        {
                            allChilds = AttachChildItems(allChilds, secondTab, c);
                        }
                        var s = (from child in allChilds where child.BaseCommandId == 6 || child.BaseCommandId == 5 select child).ToList();
                        if (s.Any()) continue;
                        foreach (var c in allChilds)
                        {
                            secondTab.Remove(c);
                        }
                    }
                }
                */

                #endregion

                #region

                var allSeqListItems = new List<TreeView>();
                foreach (var curItem in secondTab)
                {
                    allSeqListItems.Add(curItem);
                    var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                    foreach (var cItem in childItems)
                        allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                    break;
                }

                var nodeId = 100;
                var listNodes = new List<Node>();
                var listLinks = new List<Link>();
                var treeView = new TreeViewData(lstTreeView)
                {
                    Nodes = listNodes,
                    Links = listLinks
                };
                var widthCnt = Convert.ToInt32(secondTab.First().SpriteCssClass.Length.ToString())*3;
                treeView.Nodes.Add(new Node
                {
                    Id = nodeId,
                    Name = secondTab.First().SpriteCssClass,
                    ShapeId = "RoundRect",
                    Color = "#28c965",
                    Width = widthCnt.ToString()
                });

                allSeqListItems = allSeqListItems.Skip(1).ToList();
                var firstItem = allSeqListItems.First();
                var methodChildItems =
                    (from a in allSeqListItems where firstItem.GraphId == a.ParentId select a).ToList();
                var linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                    if (curItem.PrimaryCommandId == 1)
                    {
                        var condition = curItem.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1]
                            .Substring(0,
                                curItem.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1].IndexOf("Then",
                                    StringComparison.InvariantCulture));
                        nodeId++;
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "Decision",
                            Name = condition,
                            Color = "#579ddb"
                        };
                        treeView.Nodes.Add(node);
                        treeView.Links.Add(new Link
                        {
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] "
                        });
                        linkSeqNumber++;
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                        foreach (var cItem in childItems)
                            treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, node, ref nodeId,
                                ref linkSeqNumber);
                    }
                    else if (curItem.BaseCommandId == 6)
                    {
                        var item = curItem;
                        var nodeColor = "#c0c0c0";
                        var classNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllItems(s => ((s.BaseCommandId == 19)
                                               && (s.ClassNameDeclared == item.StatementReferenceMaster
                                                       .ClassCalled.Split('.').LastOrDefault())) ||
                                              (s.ClassNameDeclared == item.StatementReferenceMaster
                                                   .ClassCalled));
                        if (classNameDeclared.Count() != 0)
                            nodeColor = "#f5bd6a";
                        nodeId++;
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = curItem.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor
                        };
                        treeView.Nodes.Add(node);
                        var m = curItem.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                                //LinkText = "[" + linkSeqNumber + "] " + m
                            });
                        else
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                        linkSeqNumber++;
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                        foreach (var cItem in childItems)
                            treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView, node,
                                ref nodeId, ref linkSeqNumber);
                    }

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                //Expression<Func<ActionWorkflows, bool>> actionExpression = e => e.MethodStatementId == statementId &&
                //                                                          e.ProjectId == projectId;
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>(
                        s => (s.MethodStatementId == statementId) && (s.ProjectId == projectId));
                var lstData = new List<TreeViewData>();
                var treeViewDataNew = new TreeViewData(secondTab)
                {
                    Nodes = treeView.Nodes,
                    Links = treeView.Links,
                    ActionWorkflows = actionWorkflow.First()
                };
                lstData.Add(treeViewDataNew);
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
            }
        }

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string color)
        {
            treeView.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeView.GraphName +
                                 "</span>";
            var childItems =
                (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
            foreach (var child in childItems)
                child.GraphName = "<span style='color: " + color + ";'>" + child.GraphName +
                                  "</span>";
            //return lstTreeView;
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
                        childNodes.Where(a => childNodes.Except(new List<Node> {a}).Any(x => x.Name == a.Name)).ToList();
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

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            if (treeView.PrimaryCommandId == 1)
            {
                var condition = treeView.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1]
                    .Substring(0,
                        treeView.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1].IndexOf("Then",
                            StringComparison.InvariantCulture));

                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision",
                    Name = condition,
                    Color = "#579ddb"
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = lastNode.Id,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] " + treeView.StatementReferenceMaster.MethodCalled
                                   .Substring(0, treeView.StatementReferenceMaster.MethodCalled.IndexOf('('))
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList();
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
            }
            else if (treeView.BaseCommandId == 6)
            {
                var item = treeView;
                var nodeColor = "#c0c0c0";
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .GetAllItems(s => ((s.BaseCommandId == 19) && (s.ProjectId == projectId)
                                       && (s.ClassNameDeclared == item.StatementReferenceMaster
                                               .ClassCalled.Split('.').LastOrDefault())) ||
                                      (s.ClassNameDeclared == item.StatementReferenceMaster
                                           .ClassCalled)).Result;
                if (classNameDeclared.Count() != 0)
                    nodeColor = "#f5bd6a";
                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = lastNode.Id,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] " + treeView.StatementReferenceMaster.MethodCalled
                                   .Substring(0, treeView.StatementReferenceMaster.MethodCalled.IndexOf('('))
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            var width = "0";
            var height = "0";

            if (treeView.PrimaryCommandId == 1)
            {
                var condition = treeView.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1]
                    .Substring(0,
                        treeView.GraphName.Split(new[] {"If"}, StringSplitOptions.None)[1].IndexOf("Then",
                            StringComparison.InvariantCulture));

                nodeId++;

                var charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    var widthCalculation = charCountOfText*2;
                    if ((widthCalculation > 200) || (charCountOfText > 100))
                    {
                    }
                    else
                    {
                        widthCalculation = widthCalculation*1;
                    }

                    width = widthCalculation.ToString();
                    int heightCalculation;
                    if (charCountOfText > 100)
                        heightCalculation = charCountOfText - 40;
                    else
                        heightCalculation = charCountOfText;

                    if (heightCalculation < 30)
                        heightCalculation = 35;
                    height = heightCalculation.ToString();
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision",
                    Name = condition,
                    Color = "#579ddb",
                    Width = width,
                    Height = height
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = lastNode.Id,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList();
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
            }
            else if (treeView.BaseCommandId == 6)
            {
                var item = treeView;
                var nodeColor = "#c0c0c0";
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .GetAllItems(s => ((s.BaseCommandId == 19) && (s.ProjectId == projectId)
                                       && (s.ClassNameDeclared == item.StatementReferenceMaster
                                               .ClassCalled.Split('.').LastOrDefault())) ||
                                      (s.ClassNameDeclared == item.StatementReferenceMaster
                                           .ClassCalled)).Result;
                if (classNameDeclared.Count() != 0)
                    nodeColor = "#f5bd6a";
                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor
                };
                treeViewData.Nodes.Add(node);
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                else
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
            }
            return treeViewData;
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

        [HttpGet]
        public async Task<IHttpActionResult> AppendTreeNodes(string statementId, int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var sqlQuery = "Select * from StatementReferenceMaster where StatementId = " + int.Parse(statementId);
                var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                if (statementMaster == null) return NotFound();

                var treeView = new TreeView
                {
                    ClassCalled = statementMaster[0].ClassCalled
                };
                var auto = 300;
                var listItems = GetCallExternalDetails(statementId, treeView, new List<TreeView>(), projectId, ref auto);
                return Ok(listItems);
            }
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, ref int auto)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (treeView.ClassCalled == null) return lstTreeView;

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
                            StatementReferenceMaster = treeView.StatementReferenceMaster
                        });
                }
                else
                {
                    foreach (var statementMaster in callExtExpandedCode)
                    {
                        if (!treeView.MethodCalled.StartsWith(statementMaster.MethodName)) continue;
                        var blockStmtId = statementMaster.StatementId;

                        var stmtsBlock = GetMethodBlock(blockStmtId);

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
                                    StatementReferenceMaster = block
                                });
                                lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, ref auto);
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
                                    StatementReferenceMaster = block
                                });
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, ref auto);
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
                                    StatementReferenceMaster = block
                                });
                            }
                        }
                    }
                }
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, ref int autoInt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                var methodName = treeView.MethodCalled;
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .FindItem<StatementReferenceMaster>(
                        s => (s.BaseCommandId == 8) && (s.ProjectId == projectId) && (s.MethodName == methodName))
                    .Result;
                var callExtExpandedCode = GetGenericBlock(stmtMaster.StatementId, 8, 9);

                if (callExtExpandedCode.Count == 0)
                {
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item == null)
                        lstTreeView.Add(new TreeView
                        {
                            ParentId = statememtId,
                            GraphId = "111" + autoInt + "_" + statememtId,
                            HasChild = true,
                            GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                            BaseCommandId = 25,
                            StatementReferenceMaster = treeView.StatementReferenceMaster
                        });
                }
                else
                {
                    foreach (var block in callExtExpandedCode)
                    {
                        autoInt++;
                        if ((block.BaseCommandId == 5) || (block.OtherBaseCommandId == 5))
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
                                StatementReferenceMaster = block
                            });
                            lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                lstTreeView, projectId, ref autoInt);
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
                                StatementReferenceMaster = block
                            });
                            lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                lstTreeView, projectId, ref autoInt);
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
                                StatementReferenceMaster = block
                            });
                        }
                    }
                }
                return lstTreeView;
            }
        }

        private List<StatementReferenceMaster> GetMethodBlock(int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                return workflowRef;
            }
        }

        private List<StatementReferenceMaster> GetGenericBlock(string className)
        {
            object[] parametersExp =
            {
                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
            };
            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
                .ContinueWith(t => t.Result).Result;
            return callExtExpandedCode;
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
                return workflowRef;
            }
        }
    }
}