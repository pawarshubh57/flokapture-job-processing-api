using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class WorkspaceWorkflowController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public WorkspaceWorkflowController()
        {
        }

        public WorkspaceWorkflowController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ");
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();
                return Ok(lstWorkflowRef);
            }
        }

        [HttpGet]
        //public async Task<IHttpActionResult> GetAllStartNodeDetails(int projectId, int stmtId)
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId, int stmtId, string opt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var lstTreeViewData = new List<TreeViewData>();
                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();
                var lstNodes = new List<Node>();
                var lstLinks = new List<Link>();
                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository.GetAllItems(p => p.MethodStatementId != 0);
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
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                var treeStackStart = new Stack<StatementReferenceMaster>();
                treeStackStart.Push(workflowRef.FirstOrDefault());

                const string gName = "graphName";
                var mm = 1;
                var graphId = gName + mm;
                var bId = workflowRef[0].BaseCommandId;
                lstTreeView.Add(new TreeView
                {
                    GraphId = 1.ToString(),
                    GraphName = clsName.OriginObject,
                    HasChild = true,
                    ParentId = "-1"
                });
                lstTreeView.Add(new TreeView
                {
                    GraphId = gName + mm,
                    GraphName = "<span class='nodeToBold'>" + workflowRef[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = 1.ToString(),
                    BaseCommandId = bId
                });
                workflowRef.RemoveAt(0);
                var linkCounter = 1;
                var ifCounter = 0;
                var done = false;
                foreach (var statementM in workflowRef)
                {
                    //This is call external and will have its child tree from here...
                    var autoInt = 5;
                    if (statementM.PrimaryCommandId == 1)
                        ifCounter++;
                    if ((statementM.PrimaryCommandId == 1) && !done)
                    {
                        done = true;
                        graphId = gName + mm + 1;
                        var treeView = new TreeView
                        {
                            GraphId = graphId,
                            GraphName = statementM.OriginalStatement,
                            HasChild = true,
                            SpriteCssClass = "",
                            ParentId = gName + mm,
                            BaseCommandId = 1
                        };
                        lstTreeView.Add(treeView);
                        lstNodes.Add(new Node
                        {
                            Id = autoNodeId,
                            Name = statementM.OriginalStatement,
                            ShapeId = "Decision",
                            Color = "#f5bd6a",
                            Height = "15",
                            Width = statementM.OriginalStatement.Length.ToString()
                        });
                        lstLinks.Add(new Link
                        {
                            Origin = 1111,
                            Target = autoNodeId,
                            LinkText = "[ " + linkCounter + " ] " +
                                       statementM.OriginalStatement
                        });
                        autoNodeId++;
                        linkCounter++;
                        continue;
                    }
                    if (statementM.PrimaryCommandId == 2)
                    {
                        ifCounter--;
                        if (ifCounter == 0)
                        {
                            done = true;
                            graphId = gName + mm;
                        }
                    }
                    if (statementM.BaseCommandId == 6)
                    {
                        if (statementM.ClassCalled != null)
                        {
                            var className = statementM.ClassCalled.Split('.').Last();
                            object[] parametersExp =
                            {
                                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
                            };
                            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
                                .ContinueWith(t => t.Result).Result;

                            lstTreeView.Add(new TreeView
                            {
                                ParentId = graphId,
                                GraphId = statementM.StatementId.ToString(),
                                HasChild = true,
                                GraphName = "<span class='nodeToBold'>" + statementM.OriginalStatement + "</span>",
                                BaseCommandId = statementM.BaseCommandId
                            });
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
                                //linkCounter++;
                            }

                            // Now try to find out body for external call...
                            if (callExtExpandedCode.Count == 0)
                                lstTreeView.Add(new TreeView
                                {
                                    ParentId = statementM.StatementId.ToString(),
                                    GraphId = "1111111",
                                    HasChild = true,
                                    GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                                    BaseCommandId = 25
                                });
                            else
                                foreach (var statmentM in callExtExpandedCode)
                                {
                                    if (!statementM.MethodCalled.StartsWith(statmentM.MethodName)) continue;
                                    var blockStmtId = statmentM.StatementId;

                                    object[] bParameters =
                                    {
                                        new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = blockStmtId},
                                        new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                                        new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                                    };
                                    var stmtsBlick = _codeVortoService.StatementReferenceMasterRepository
                                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock",
                                            bParameters).Result;
                                    foreach (var block in stmtsBlick)
                                        if (block.BaseCommandId == 6)
                                        {
                                            lstTreeView.Add(new TreeView
                                            {
                                                ParentId = statementM.StatementId.ToString(),
                                                GraphId = block.StatementId + "_" + autoInt,
                                                HasChild = false,
                                                GraphName = block.OriginalStatement,
                                                BaseCommandId = block.BaseCommandId
                                            });
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
                                            lstTreeView = AppendNodeToExisting(block.StatementId + "_" + autoInt,
                                                lstTreeView, block, lstLinks, lstNodes, autoNodeId, ref linkCounter);
                                            autoInt++;
                                            //lstTreeView.AddRange(result);
                                        }
                                        else
                                        {
                                            lstTreeView.Add(new TreeView
                                            {
                                                ParentId = statementM.StatementId.ToString(),
                                                GraphId = block.StatementId.ToString(),
                                                HasChild = false,
                                                GraphName = block.OriginalStatement,
                                                BaseCommandId = block.BaseCommandId
                                            });
                                        }
                                }
                        }
                        else
                        {
                            object[] bParameters =
                            {
                                new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = statementM.StatementId},
                                new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                                new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                            };
                            var stmtsBlock = _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock",
                                    bParameters).Result;
                            if (stmtsBlock.Count == 0)
                                lstTreeView.Add(new TreeView
                                {
                                    ParentId = graphId,
                                    GraphId = "222222",
                                    HasChild = false,
                                    GraphName = statementM.OriginalStatement,
                                    BaseCommandId = 25
                                });
                            foreach (var block in stmtsBlock)
                            {
                                if (block.BaseCommandId == 6)
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ParentId = graphId,
                                        GraphId = block.StatementId + "_" + autoInt,
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId
                                    });

                                    lstTreeView = AppendNodeToExisting(block.StatementId + "_" + autoInt,
                                        lstTreeView, block, lstLinks, lstNodes, autoNodeId, ref linkCounter);
                                    autoInt++;
                                    linkCounter++;
                                    //lstTreeView.AddRange(result);
                                }
                                else
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ParentId = graphId,
                                        GraphId = block.StatementId.ToString(),
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId
                                    });
                                }
                                autoInt++;
                            }
                        }
                    }
                    else
                    {
                        var treeView = new TreeView
                        {
                            GraphId = statementM.StatementId.ToString(),
                            GraphName = statementM.OriginalStatement,
                            HasChild = false,
                            SpriteCssClass = "",
                            ParentId = graphId,
                            BaseCommandId = bId
                        };
                        lstTreeView.Add(treeView);
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
                        if ((duplicates.Count > 0) && (duplicates[0] != null))
                        {
                            var links =
                                (from h in lstLinks
                                 where (h.Origin == node.Id) && (h.Target == duplicates.First().Id)
                                 select h)
                                    .ToList();
                            if (!links.Any()) continue;

                            var linkText = links.Aggregate(string.Empty,
                                (current, jk) => current + jk.LinkText + ", ");
                            linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                            links.First().LinkText = linkText;
                            foreach (var l in links)
                            {
                                if (l.LinkText == linkText) continue;
                                copyOfLinks.Remove(l);
                            }
                        }
                    }
                }


                var treeViewData = new TreeViewData(lstTreeView)
                {
                    Links = copyOfLinks,
                    Nodes = lstNodes
                };
                lstTreeViewData.Add(treeViewData);
                //var items = lstTreeView.ToList().FindAll(b => b.BaseCommandId == 6
                //    || b.BaseCommandId == 25).ToList();
                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));
                //TreeViewData treeViewDataNew = new TreeViewData(items);
                secondTab.AddRange(
                    lstTreeView.Where(
                        item => (item.BaseCommandId == 6) || (item.BaseCommandId == 25) || (item.BaseCommandId == 1)));
                var treeViewDataNew = new TreeViewData(secondTab);
                lstTreeViewData.Add(treeViewDataNew);
                return Ok(lstTreeViewData);
            }
        }


        public List<TreeView> AppendNodeToExisting(string statementId, List<TreeView> currentTreeView,
            StatementReferenceMaster statementM, List<Link> lstLinks, List<Node> lstNodes, int autoNodeId,
            ref int linkCounter)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var className = statementM.ClassCalled.Split('.').Last();
                object[] parametersExp =
                {
                    new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
                };
                var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
                    .ContinueWith(t => t.Result).Result;

                if (callExtExpandedCode.Count == 0)
                {
                    var item = currentTreeView.Find(p => p.ParentId == statementId);
                    if (item == null)
                        currentTreeView.Add(new TreeView
                        {
                            ParentId = statementId,
                            GraphId = "11d11111",
                            HasChild = true,
                            GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                            BaseCommandId = 25
                        });
                }
                else
                {
                    foreach (var statementK in callExtExpandedCode)
                    {
                        if (!statementM.MethodCalled.StartsWith(statementK.MethodName)) continue;
                        var blockStmtId = statementK.StatementId;
                        object[] bParameters =
                        {
                            new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = blockStmtId},
                            new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                            new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                        };
                        var stmtsBlock = _codeVortoService.StatementReferenceMasterRepository
                            .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", bParameters).Result;
                        foreach (var block in stmtsBlock)
                            if (block.BaseCommandId == 6)
                            {
                                currentTreeView.Add(new TreeView
                                {
                                    ParentId = statementId,
                                    GraphId = block.StatementId + "_" + statementId,
                                    HasChild = false,
                                    GraphName = block.OriginalStatement,
                                    BaseCommandId = block.BaseCommandId
                                });
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
                                currentTreeView = AppendNodeToExisting(block.StatementId + "_" + statementId,
                                    currentTreeView, block, lstLinks, lstNodes, autoNodeId, ref linkCounter);
                            }
                            else
                            {
                                currentTreeView.Add(new TreeView
                                {
                                    ParentId = statementId,
                                    GraphId = block.StatementId.ToString(),
                                    HasChild = false,
                                    GraphName = block.OriginalStatement,
                                    BaseCommandId = block.BaseCommandId
                                });
                            }
                    }
                }
                return currentTreeView;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GenerateWorkflowDiagram(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var lstNodes = new List<Node>();
                var workflowRef1 = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM codevorto.actionworkflows where ProjectId = " + projectId + " ;");
                int? startId = 0;

                if (workflowRef1 != null)
                    startId = workflowRef1[0].MethodStatementId;
                var mySqlQuery = " Select * from statementreferencemaster where  StatementId Between " +
                                 " ( Select StatementId From StatementReferenceMaster Where BaseCommandId = 8  AND StatementId >= " +
                                 startId + " LIMIT 1 ) ANd " +
                                 " (Select StatementId From StatementReferenceMaster Where BaseCommandId = 9 AND StatementId >= " +
                                 startId + " LIMIT 1) AND StatementId > " + startId + ";";

                var workflowRef = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(mySqlQuery);
                var allClassDeclarations = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "SELECT * FROM StatementReferenceMaster where ClassNameDeclared IS NOT NULL group by ClassNameDeclared;");
                var classCollection =
                    allClassDeclarations.FindAll(s => s.ClassNameDeclared != null);
                //var lstTreeView = new List<TreeView>();
                var done = false;
                foreach (var cls in classCollection)
                {
                    if (done) break;
                    foreach (var workFlow in workflowRef)
                    {
                        if (!workFlow.OriginalStatement.Contains("New " + cls.ClassNameDeclared + "()")) continue;
                        var newStartPoint = workFlow;
                        var node = new Node
                        {
                            Id = newStartPoint.StatementId,
                            Name = "New " + cls.ClassNameDeclared + "()",
                            ShapeId = "RoundRect",
                            Color = "green",
                            StatementTypeId = newStartPoint.StatementId
                        };
                        lstNodes.Add(node);
                        done = true;
                        break;
                    }
                }
                //var statementRef1 = await _codeVortoService.StatementReferenceMasterRepository
                //    .GetEntityData<StatementReferenceMaster>(s => s.StatementId == workflowRef[0].StatementId);

                var onStartSql =
                    "Select * from statementreferencemaster where MethodName = 'OnStart';";
                var stmtMaster =
                    await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(onStartSql);
                var methodQuery = " Select * from statementreferencemaster where  StatementId Between " +
                                  " ( Select StatementId From StatementReferenceMaster Where  BaseCommandId = 8  AND StatementId >= " +
                                  " " + stmtMaster[0].StatementId + " LIMIT 1 ) ANd " +
                                  " (Select StatementId From StatementReferenceMaster Where BaseCommandId = 9 AND StatementId >= " +
                                  " " + stmtMaster[0].StatementId + " LIMIT 1) AND StatementId >= " +
                                  stmtMaster[0].StatementId + " ; ";
                var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(methodQuery);
                var tempData = statementMaster;

                foreach (var statementReference in statementMaster)
                {
                    if (lstNodes.Exists(l => l.Name == statementReference.ClassCalled)) continue;
                    if (statementReference.BaseCommandId == 11) // Method Declatarion
                    {
                    }
                    if (statementReference.BaseCommandId == 5) // Call Internal
                    {
                        var node = new Node
                        {
                            Name = statementReference.ClassCalled,
                            Id = statementReference.StatementId,
                            Color = "red",
                            ShapeId = "RoundRect",
                            StatementTypeId = statementReference.StatementId
                        };
                        lstNodes.Add(node);
                    }
                    else if (statementReference.BaseCommandId == 3) // Loop
                    {
                        var node = new Node
                        {
                            Name = statementReference.VariableNameDeclared,
                            Id = statementReference.StatementId,
                            Color = "red",
                            ShapeId = "Rectangle",
                            StatementTypeId = statementReference.StatementId
                        };
                        lstNodes.Add(node);
                    }
                    else if (statementReference.BaseCommandId == 1) // IF
                    {
                        var ifString = statementReference.OriginalStatement.Split(new[] { "If " }
                            , StringSplitOptions.RemoveEmptyEntries);
                        var condition = ifString[0].Split(new[] { " Then" }, StringSplitOptions.RemoveEmptyEntries)[0];
                        var node = new Node
                        {
                            Name = condition,
                            Id = statementReference.StatementId,
                            Color = "#f4b183",
                            ShapeId = "Decision",
                            StatementTypeId = statementReference.StatementId
                        };
                        lstNodes.Add(node);
                    }
                    else if (statementReference.BaseCommandId == 6) // Call External
                    {
                        var node = new Node
                        {
                            Name = statementReference.VariableNameDeclared,
                            Id = statementReference.StatementId,
                            Color = "purple",
                            ShapeId = "RoundRect",
                            StatementTypeId = statementReference.StatementId
                        };
                        lstNodes.Add(node);
                    }
                }
                var lstLinks = new List<Link>();
                foreach (var stmtMasterCopy in tempData)
                    if (stmtMasterCopy.BaseCommandId == 8)
                    {
                        //var reference = abc;
                        //var stmtMasterNew = await _codeVortoService.StatementReferenceMasterRepository
                        //    .FindItem<StatementReferenceMaster>(s => s.MethodCalled == reference.MethodCalled);
                        var link = new Link
                        {
                            LinkText = stmtMasterCopy.MethodName,
                            Origin = lstNodes.First().Id,
                            Target = 244
                        };
                        lstLinks.Add(link);
                    }
                    else if (stmtMasterCopy.BaseCommandId == 5)
                    {
                        var destNode =
                            lstNodes.Find(n => (n.Name == stmtMasterCopy.ClassCalled) && (n.ShapeId == "RoundRect")
                                               && (n.Id != stmtMasterCopy.StatementId));
                        if (destNode == null) continue;
                        var link = new Link
                        {
                            Origin = lstNodes.First().Id,
                            Target = destNode.Id,
                            LinkText = stmtMasterCopy.MethodCalled
                        };
                        lstLinks.Add(link);
                    }
                var lstFlowcharts = new FlowChart
                {
                    Nodes = lstNodes,
                    Links = lstLinks
                };

                return Ok(lstFlowcharts);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetProjectTickersCount(int projectId)
        {
            var generalRepository =
                new GeneralRepository<ProjectTickersCount>(new AppDbContext());
            object[] parameters =
            {
                new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
            };
            var dataTable = await generalRepository
                .ExecuteStoreProcedure<ProjectTickersCount>("SpProjectDashboardTickers", parameters);
            return Ok(dataTable);
        }
       
    }
}