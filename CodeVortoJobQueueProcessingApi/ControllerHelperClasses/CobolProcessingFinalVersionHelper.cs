using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using BusinessLayer.DbEntities;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.VisualBasicVba;

namespace CodeVortoJobQueueProcessingApi.ControllerHelperClasses
{
    public class CobolProcessingFinalVersionHelper
    {
        public TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
        TreeViewData treeViewData, List<StatementReferenceMaster> allClassNameDeclaredAndCalssCalledList, Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            try
            {
                var stringBuilder = new StringBuilder();
                if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
                {
                    #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                    string condition;
                    string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                    if (ifPart.Contains("Then"))
                    {
                        condition = ifPart.Substring(0,
                            ifPart.IndexOf("Then",
                                StringComparison.InvariantCulture));
                    }
                    else
                        condition = ifPart;

                    nodeId++;

                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "Decision2",
                    //    Name = condition,
                    //    Color = "#ff6600",
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "Decision2", "#ff6600", condition, treeView);
                    treeViewData.Nodes.Add(node);

                    string linkText;
                    string m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                    {
                        m = m + "()";
                        linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                        //});

                    }
                    else
                    {
                        linkText = "[" + linkSeqNumber + "] ";
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] "
                        //});
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf (projectId): (" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList, node, ref nodeId,
                            ref linkSeqNumber);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 3)
                {
                    #region BaseCommandId == 3
                    nodeId++;
                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "Decision2",
                    //    Name = treeView.GraphName,
                    //    Color = "#ff6600",
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                    treeViewData.Nodes.Add(node);

                    string linkText = "[" + linkSeqNumber + "] ";
                    var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    Origin = lastNode.Id,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] "
                    //});
                    linkSeqNumber++;
                    var childItems =
                       (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsLoop (projectId): (" +
                                             projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList, node, ref nodeId,
                            ref linkSeqNumber);
                    }
                    #endregion
                }
                else if (treeView.BaseCommandId == 4)
                {
                    #region BaseCommandId == 4
                    nodeId++;
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "Decision2", "#ff6600", treeView.GraphName, treeView);
                    treeViewData.Nodes.Add(node);
                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "Decision2",
                    //    Name = treeView.GraphName,
                    //    Color = "#ff6600",
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    //treeViewData.Nodes.Add(node);

                    string linkText = "[" + linkSeqNumber + "] ";
                    var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    Origin = lastNode.Id,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] "
                    //});
                    linkSeqNumber++;
                    #endregion
                }
                else if (treeView.BaseCommandId == 5)
                {
                    #region BaseCommandId == 5

                    string nodeColor = "#c0c0c0";
                    var methodCalled =
                        allClassNameDeclaredAndCalssCalledList.Where(
                            x => x.BaseCommandId == 19 && x.FileId == treeView.StatementReferenceMaster.FileId).ToList();
                    if (methodCalled.Count != 0)
                        nodeColor = "#00ffff";
                    nodeId++;
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "RoundRect", nodeColor, methodCalled[0].ClassNameDeclared, treeView);
                    treeViewData.Nodes.Add(node);

                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "RoundRect",
                    //    Name = methodCalled[0].ClassNameDeclared,
                    //    Color = nodeColor,
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    string linkText;
                    string m = treeView.StatementReferenceMaster.MethodCalled;
                    if (m != null)
                    {
                        linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //m = m + "()";
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                        //});
                    }
                    else
                    {
                        linkText = "[" + linkSeqNumber + "] ";
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] "
                        //});
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine(
                        "Started process for called internal: ProcessChildItemsCallInternal (projectId): (" + projectId +
                        ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList, node,
                            ref nodeId, ref linkSeqNumber);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 6)
                {
                    #region PrimaryCommandId == 6

                    var item = treeView;
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
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "RoundRect", nodeColor, treeView.StatementReferenceMaster.ClassCalled, treeView);
                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "RoundRect",
                    //    Name = treeView.StatementReferenceMaster.ClassCalled,
                    //    Color = nodeColor,
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    treeViewData.Nodes.Add(node);
                    string m = treeView.StatementReferenceMaster.MethodCalled;

                    string linkText;
                    if (!string.IsNullOrEmpty(m))
                    {
                        linkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('));
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);

                        //m = m + "()";
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        //    // Origin = nodeId - 1,
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                        //});
                    }
                    else
                    {
                        //treeViewData.Links.Add(new Link
                        //{
                        //    Origin = lastNode.Id,
                        // 
                        //    Target = nodeId,
                        //    LinkText = "[" + linkSeqNumber + "] "
                        //});
                        linkText = "[" + linkSeqNumber + "] ";
                        var link = VisualBasicVba1.GetLinkDetails(lastNode.Id, nodeId, linkText, treeView);
                        treeViewData.Links.Add(link);

                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                            .ToList().Distinct();
                    stringBuilder.AppendLine(
                        "Started process for called external: ProcessChildItemsCallExt (projectId): (" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData,
                            allClassNameDeclaredAndCalssCalledList, node, ref nodeId,
                            ref linkSeqNumber);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 10)
                {
                    #region BaseCommandId == 10

                    var parentIf =
                        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    nodeId++;
                    string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "Decision2", "#ff6600", condition, treeView);
                    treeViewData.Nodes.Add(node);
                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "Decision2",
                    //    Name = condition,
                    //    Color = "#ff6600",
                    //    Width = width,
                    //    Height = height,
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    //treeViewData.Nodes.Add(node);
                    string linkText = "[" + linkSeqNumber + "] ";
                    var link = VisualBasicVba1.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] "
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
                                allClassNameDeclaredAndCalssCalledList, node,
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
                    #region BaseCommandId = 45
                    nodeId++;
                    //var node = new Node
                    //{
                    //    Id = nodeId,
                    //    ShapeId = "RoundRect",
                    //    Name = treeView.GraphName,
                    //    Color = "#c0c0c0",
                    //    Width = width,
                    //    Height = height,
                    //    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    //    GroupName = treeView.GroupName,
                    //    GroupId = treeView.GroupId
                    //};
                    var node = VisualBasicVba1.GetNodeDetails(nodeId, "RoundRect", "#c0c0c0", treeView.GraphName, treeView);
                    treeViewData.Nodes.Add(node);

                    string linkText = "[" + linkSeqNumber + "] ";
                    var link = VisualBasicVba1.GetLinkDetails(nodeId - 1, nodeId, linkText, treeView);
                    treeViewData.Links.Add(link);
                    //treeViewData.Links.Add(new Link
                    //{
                    //    Origin = nodeId - 1,
                    //    Target = nodeId,
                    //    LinkText = "[" + linkSeqNumber + "] "
                    //});
                    linkSeqNumber++;
                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }
    }
}