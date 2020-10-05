

using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;
using BusinessLayer.DbEntities;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;

namespace CodeVortoJobQueueProcessingApi.ControllerHelperClasses
{
    public class UniverseProcessingHelper
    {
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();

        public TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
         TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList,
         ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            int commanClassProjId = 9;
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

                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", condition, treeView);
                treeViewData.Nodes.Add(node);

                //var node = new Node
                //{
                //    Id = nodeId,
                //    ShapeId = "Decision2",
                //    Name = condition,
                //    Color = "#ff6600",
                //    Width = width,
                //    Height = height,
                //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                //    GroupName = treeView.GroupName,
                //    GroupId = treeView.GroupId,
                //    ProgramId = treeView.ProgramId
                //};
                treeViewData.Nodes.Add(node);
                string linkText;
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                    var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    //Origin = lastNode.Id,
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                    //    StatementId = treeView.StatementReferenceMaster.StatementId,
                    //    ProgramId = treeView.ProgramId
                    //});
                }
                else
                {
                    linkText = "[" + linkSeqNumber + "] ";
                    var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    //Origin = lastNode.Id,
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] ",
                    //    ProgramId = treeView.ProgramId
                    //});
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine(
        "========================================================================================");
                stringBuilder.AppendLine("Started process for ProcessChildItemsIf: " +
                                                                   projectId);
                foreach (var cItem in childItems)
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId, ref linkSeqNumber);

                #endregion
            }
            else if (treeView.BaseCommandId == 3)
            {
                #region BaseCommandId == 3
                nodeId++;
               
                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                treeViewData.Nodes.Add(node);

                string linkText = "[" + linkSeqNumber + "] ";
                var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                treeViewData.Links.Add(link);
                
                linkSeqNumber++;
                var childItems =
                   (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf (projectId): (" +
                                         projectId + ")");
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                        allClassNameDeclaredAndCalssCalledList, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 4)
            {
                #region BaseCommandId == 4
                nodeId++;
                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                treeViewData.Nodes.Add(node);
              
                string linkText = "[" + linkSeqNumber + "] ";
                var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                treeViewData.Links.Add(link);
               
                linkSeqNumber++;
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
                    if (!methodCalled.Any()) return treeViewData;

                    var strName = methodCalled[0].ClassNameDeclared;
                    var strSplit = methodCalled[0].ClassNameDeclared;
                    if (strSplit.Contains("."))
                    {
                        var strSplit1 = methodCalled[0].ClassNameDeclared.Split('.');
                        if (strSplit1.Length > 2)
                            strName = strSplit1[strSplit1.Length - 2] + "." + strSplit1[strSplit1.Length - 1];
                    }
                    node = _clsUniverseBasic.GetNodeDetails(nodeId, "RoundRect", nodeColor, strName.ToUpper(), treeView);
                    //node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "RoundRect",
                    //    Name = strName.ToUpper(),
                    //    Color = nodeColor,
                    //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId,
                    //    ProgramId = treeView.ProgramId
                    //};
                }
                else
                {
                    node = _clsUniverseBasic.GetNodeDetails(nodeId, "RoundRect", nodeColor, methodCalled[0].ClassNameDeclared, treeView);
                    //node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "RoundRect",
                    //    Name = methodCalled[0].ClassNameDeclared,
                    //    Color = nodeColor,
                    //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId,
                    //    ProgramId = treeView.ProgramId
                    //};
                }
                treeViewData.Nodes.Add(node);

                string linkText;
                var m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                    var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    //Origin = lastNode.Id,
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                    //    StatementId = treeView.StatementReferenceMaster.StatementId,
                    //    ProgramId = treeView.ProgramId
                    //});
                }
                else
                {
                    linkText = "[" + linkSeqNumber + "] ";
                    var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    //Origin = lastNode.Id,
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] ",
                    //    ProgramId = treeView.ProgramId
                    //});
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine(
        "========================================================================================");
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallInternal: " +
                                                               projectId);
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList, ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

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

                        node = _clsUniverseBasic.GetNodeDetails(nodeId, "RoundRect", nodeColor, strName.ToUpper(), treeView);
                        //node = new Node
                        //{
                        //    Id = nodeId,
                        //    ShapeId = "RoundRect",
                        //    Name = strName.ToUpper(),
                        //    Color = nodeColor,
                        //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        //    GroupName = treeView.GroupName,
                        //    GroupId = treeView.GroupId,
                        //    ProgramId = treeView.ProgramId
                        //};
                    }
                    else
                    {
                        node = _clsUniverseBasic.GetNodeDetails(nodeId, "RoundRect", nodeColor, treeView.StatementReferenceMaster.ClassCalled, treeView);
                        //node = new Node
                        //{
                        //    Id = nodeId,
                        //    ShapeId = "RoundRect",
                        //    Name = treeView.StatementReferenceMaster.ClassCalled,
                        //    Color = nodeColor,
                        //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        //    GroupName = treeView.GroupName,
                        //    GroupId = treeView.GroupId,
                        //    ProgramId = treeView.ProgramId
                        //};
                    }
                    treeViewData.Nodes.Add(node);
                    string linkText;

                    var m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                    {
                        m = m + "()";

                        linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                        var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //treeViewData.Links.Add(new Link
                        //{
                        //    //Origin = lastNode.Id,
                        //    Origin = nodeId - 1,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        //    StatementId = treeView.StatementReferenceMaster.StatementId,
                        //    ProgramId = treeView.ProgramId
                        //});
                    }
                    else
                    {
                        linkText = "[" + linkSeqNumber + "] ";
                        var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //treeViewData.Links.Add(new Link
                        //{
                        //    //Origin = lastNode.Id,
                        //    Origin = nodeId - 1,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] ",
                        //    ProgramId = treeView.ProgramId
                        //});
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25) select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine(
        "========================================================================================");
                    stringBuilder.AppendLine("Started process for ProcessChildItemsCallExt: " +
                                                                projectId);
                    foreach (var cItem in childItems)

                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, allClassNameDeclaredAndCalssCalledList, ref nodeId, ref linkSeqNumber);
                }

                #endregion
            }
           
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                //var node = new Node
                //{
                //    Id = nodeId,
                //    ShapeId = "Decision2",
                //    Name = treeView.GraphName,
                //    Color = "#ff6600",
                //    Width = width1,
                //    Height = height1,
                //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                //    GroupName = treeView.GroupName,
                //    GroupId = treeView.GroupId,
                //    ProgramId = treeView.ProgramId
                //};
                treeViewData.Nodes.Add(node);
                string linkText = "[" + linkSeqNumber + "] ";
                var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                treeViewData.Links.Add(link);
                //treeViewData.Links.Add(new Link
                //{
                //    Origin = nodeId - 1,
                //    Target = nodeId,
                //    LinkText = "[" + linkSeqNumber + "] ",
                //    ProgramId = treeView.ProgramId
                //});
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
                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", condition, treeView);
                //var node = new Node
                //{
                //    Id = nodeId,
                //    ShapeId = "Decision2",
                //    Name = condition,
                //    Color = "#ff6600",
                //    Width = width,
                //    Height = height,
                //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                //    GroupName = treeView.GroupName,
                //    GroupId = treeView.GroupId,
                //    ProgramId = treeView.ProgramId
                //};
                treeViewData.Nodes.Add(node);

                string linkText = "[" + linkSeqNumber + "] ";
                var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                treeViewData.Links.Add(link);
                //treeViewData.Links.Add(new Link
                //{
                //    Origin = nodeId - 1,
                //    Target = nodeId,
                //    LinkText = "[" + linkSeqNumber + "] ",
                //    ProgramId = treeView.ProgramId
                //});
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    stringBuilder.AppendLine(
        "========================================================================================");
                    stringBuilder.AppendLine("Started process for ProcessChildItemsElse: " +
                                                               projectId);
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList,
                            ref nodeId, ref linkSeqNumber);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 45)
            {
                #region BaseCommandId == 45

                nodeId++;
                var node = _clsUniverseBasic.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                //var node = new Node
                //{
                //    Id = nodeId,
                //    ShapeId = "Decision2",
                //    Name = treeView.GraphName,
                //    Color = "#ff6600",
                //    Width = width1,
                //    Height = height1,
                //    StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                //    GroupName = treeView.GroupName,
                //    GroupId = treeView.GroupId,
                //    ProgramId = treeView.ProgramId
                //};
                treeViewData.Nodes.Add(node);

                string linkText = "[" + linkSeqNumber + "] ";
                var link = _clsUniverseBasic.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                treeViewData.Links.Add(link);
                //treeViewData.Links.Add(new Link
                //{
                //    Origin = nodeId - 1,
                //    Target = nodeId,
                //    LinkText = "[" + linkSeqNumber + "] ",
                //    ProgramId = treeView.ProgramId
                //});
                linkSeqNumber++;

                #endregion
            }
            LogMessage.WriteLogMessage(stringBuilder);
            return treeViewData;
        }
    }
}