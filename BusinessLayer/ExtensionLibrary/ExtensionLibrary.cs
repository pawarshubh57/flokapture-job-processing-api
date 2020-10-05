using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;
using MySql.Data.MySqlClient;

namespace BusinessLayer.ExtensionLibrary
{
    public static partial class ExtensionLibrary
    {
        /// <summary>
        /// </summary>
        /// <para>Created by: Yogesh Sonawane</para>
        /// <param name="dbContext"></param>
        /// <param name="storedProcedureName"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        public static async Task<int> ExecuteStoredProcedure(this DbContext dbContext, string storedProcedureName,
            params object[] parameters)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("Call " + storedProcedureName);
            stringBuilder.AppendLine("( ");
            foreach (MySqlParameter paremeter in parameters)
            {
                if (!paremeter.ParameterName.Contains('@'))
                    paremeter.ParameterName = "@" + paremeter.ParameterName + "".Trim();
                stringBuilder.AppendLine(paremeter.ParameterName + ",");
            }
            var index = stringBuilder.ToString().LastIndexOf(",", StringComparison.InvariantCulture);
            stringBuilder = stringBuilder.Remove(index, 1);
            stringBuilder.AppendLine(" ); ");
            var sqlStatement = stringBuilder.ToString();
            var rowData = await dbContext.Database.SqlQuery(typeof(string), sqlStatement, parameters).ToListAsync();
            return rowData.Count;
        }

        /// <summary>
        /// </summary>
        /// <para>Created by: Yogesh Sonawane</para>
        /// <typeparam name="T"></typeparam>
        /// <param name="dbContext"></param>
        /// <param name="storedProcedureName"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        public static async Task<List<T>> ExecuteStoredProcedure<T>(this DbContext dbContext, string storedProcedureName,
            params object[] parameters)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("Call " + storedProcedureName);
            stringBuilder.AppendLine("( ");
            foreach (MySqlParameter paremeter in parameters)
            {
                if (!paremeter.ParameterName.Contains('@'))
                    paremeter.ParameterName = "@" + paremeter.ParameterName + "".Trim();
                stringBuilder.AppendLine(paremeter.ParameterName + ",");
            }
            var index = stringBuilder.ToString().LastIndexOf(",", StringComparison.InvariantCulture);
            stringBuilder = stringBuilder.Remove(index, 1);
            stringBuilder.AppendLine(" ); ");
            var lstData = await dbContext.Database.SqlQuery<T>(stringBuilder.ToString(), parameters).ToListAsync()
                .ContinueWith(t => t.Result);
            return lstData;
        }

        /// <summary>
        /// </summary>
        /// <param name="str"></param>
        /// <param name="afterWhich"></param>
        /// <param name="beforeWhich"></param>
        /// <param name="character"></param>
        /// <returns></returns>
        public static string InsertExtraChars(this string str, char afterWhich, char beforeWhich, string character)
        {
            if (str.Length <= 0) return str;
            var tempStringBuilder = new StringBuilder();
            foreach (var t in str)
            {
                if (t == afterWhich)
                {
                    tempStringBuilder.Append(character + t + character);
                    continue;
                }
                if (t == beforeWhich)
                {
                    tempStringBuilder.Append(character + t + character);
                    continue;
                }
                tempStringBuilder.Append(t);
            }
            return tempStringBuilder.ToString();
        }

        /// <summary>
        /// </summary>
        /// <param name="inputStringArray"></param>
        /// <param name="blockStartExpression"></param>
        /// <param name="blockEndExpression"></param>
        /// <returns></returns>
        public static string[] RemoveAllCommentedBlocks(this string[] inputStringArray, string blockStartExpression,
            string blockEndExpression)
        {
            var lstBlock = new List<string>();
            if (inputStringArray.Length == 0) return lstBlock.ToArray();
            var linesCopy = new string[inputStringArray.Length];
            inputStringArray.CopyTo(linesCopy, 0);

            for (var i = 0; i < inputStringArray.Length; i++)
                if (inputStringArray[i].StartsWith(blockStartExpression))
                {
                    var commentedBlock = GetBlockOfLinesAsArray(linesCopy, ref i, blockEndExpression);
                    inputStringArray = commentedBlock;
                    --i;
                }
                else
                    lstBlock.Add(inputStringArray[i]);
            return lstBlock.ToArray();
        }

        /// <summary>
        ///     <para>This is spacial case for JCL files to get block of EXEC statements.</para>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputStringArray"></param>
        /// <param name="startIndex"></param>
        /// <param name="upToStringExpression"></param>
        /// <returns></returns>
        private static string[] GetBlockOfLinesAsArray(this string[] inputStringArray, ref int startIndex,
            string upToStringExpression)
        {
            var list = new List<string>(inputStringArray);
            var cnt = startIndex;
            if (inputStringArray.Length == 0) return list.ToArray();

            for (var k = cnt; k < inputStringArray.Length;)
            {
                if (list[k].Contains(upToStringExpression))
                {
                    list.RemoveAt(k);
                    break;
                }
                list.RemoveAt(k);
                k = cnt;
            }
            return list.ToArray();
        }

        /// <summary>
        ///     <para>This is spacial case for JCL files to get block of EXEC statements.</para>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputStringArray"></param>
        /// <param name="startIndex"></param>
        /// <param name="upToStringExpression"></param>
        /// <returns></returns>
        public static List<string> GetBlockOfLinesAsList(this List<string> inputStringArray, int startIndex,
            string upToStringExpression)
        {
            var lstBlock = new List<string>();
            var cnt = startIndex;
            if (inputStringArray.Count == 0) return lstBlock;
            do
            {
                lstBlock.Add(inputStringArray[cnt]);
                cnt++;
                if (cnt >= inputStringArray.Count)
                    break;
            } while (
                !inputStringArray[cnt].StartsWith(upToStringExpression, StringComparison.CurrentCultureIgnoreCase) &&
                (cnt < inputStringArray.Count));
            if (cnt >= inputStringArray.Count)
            {
            }
            else
                lstBlock.Add(inputStringArray[cnt]);

            return lstBlock;
        }

        /// <summary>
        ///     <para>This is spacial case for JCL files to get block of step notes before EXEC statements.</para>
        ///     <para></para>
        ///     <remarks>reverse: If true, then returns array in sequence </remarks>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputStringArray"></param>
        /// <param name="startIndex"></param>
        /// <param name="strContainsExpression"></param>
        /// <param name="reverse"></param>
        /// <returns></returns>
        public static string[] GetBlockOfLinesAsArray(this string[] inputStringArray, int startIndex,
            string strContainsExpression, bool reverse)
        {
            var lstBlock = new List<string>();
            var cnt = startIndex - 1;
            if (inputStringArray.Length == 0) return lstBlock.ToArray();
            do
            {
                lstBlock.Add(inputStringArray[cnt]);
                --cnt;
                if (cnt <= 0)
                    break;
            } while (
                (inputStringArray[cnt].IndexOf(strContainsExpression, StringComparison.CurrentCultureIgnoreCase) >= 0) &&
                (cnt >= 0));
            if (!reverse) return lstBlock.ToArray();
            if (lstBlock.Count == 0) return lstBlock.ToArray();
            var reverseArray = new string[lstBlock.Count];
            for (int i = lstBlock.Count - 1, j = 0; i >= 0; i--, j++)
                reverseArray[j] = lstBlock[i];
            return reverseArray;
        }

        /// <summary>
        ///     <para>This is spacial case for JCL files to get block of EXEC statements.</para>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputStringArray"></param>
        /// <param name="startIndex"></param>
        /// <param name="upToStringExpression"></param>
        /// <returns></returns>
        public static string[] GetBlockOfLinesAsArray(this string[] inputStringArray, int startIndex,
            string upToStringExpression)
        {
            var lstBlock = new List<string>();
            var cnt = startIndex;
            if (inputStringArray.Length == 0) return lstBlock.ToArray();
            do
            {
                lstBlock.Add(inputStringArray[cnt]);
                cnt++;
                if (cnt >= inputStringArray.Length)
                    break;
            } while (
                !(inputStringArray[cnt].IndexOf(upToStringExpression, StringComparison.CurrentCultureIgnoreCase) >= 0) &&
                (cnt < inputStringArray.Length));
            if (cnt >= inputStringArray.Length)
            {
            }
            else
                lstBlock.Add(inputStringArray[cnt]);

            return lstBlock.ToArray();
        }

        /// <summary>
        /// </summary>
        /// <param name="source"></param>
        /// <param name="toCheck"></param>
        /// <param name="comp"></param>
        /// <returns></returns>
        public static int Contains(this string[] source, string toCheck, StringComparison comp)
        {
            var index = -1;
            toCheck = toCheck.Trim();
            foreach (var s in source)
            {
                index++;
                if (s.IndexOf("Dim " + toCheck + " As New", comp) >= 0)
                    return index;
            }
            return -1;
        }

        /// <summary>
        /// </summary>
        /// <param name="fileManagerEnumerator"></param>
        /// <param name="instanceStringToSearch"></param>
        /// <param name="lineStartIndicator"></param>
        /// <param name="blockStartExpression"></param>
        /// <param name="blockEndExpression"></param>
        /// <returns></returns>
        public static List<string> AllFilesWhichContainsActualInstance(
            this List<FileMaster> fileManagerEnumerator, string instanceStringToSearch, string lineStartIndicator, string blockStartExpression,
            string blockEndExpression)
        {
            lineStartIndicator = lineStartIndicator.Trim();
            instanceStringToSearch = instanceStringToSearch.Trim();
            var allFilesPath = new List<string>();
            foreach (var file in fileManagerEnumerator)
            {
                var allLines = GetFileLines(file.FilePath, blockStartExpression, blockEndExpression);
                allFilesPath.AddRange(from line in allLines
                                      where line.Contains(lineStartIndicator + " " + instanceStringToSearch)
                                      select file.FilePath);
            }
            return allFilesPath;
        }

        /// <summary>
        /// </summary>
        /// <param name="filePath"></param>
        /// <param name="blockStartExpression"></param>
        /// <param name="blockEndExpression"></param>
        /// <returns></returns>
        private static List<string> GetFileLines(string filePath, string blockStartExpression, string blockEndExpression)
        {
            var loopLines = File.ReadAllLines(filePath);
            var strLines = new List<string>();
            for (var l = 0; l < loopLines.Select(s => s != "").Count(); l++)
            {
                if (loopLines[l].TrimStart().Trim(' ', '\t') == "") continue;
                strLines.Add(loopLines[l].TrimStart().Trim(' ', '\t'));
            }
            var allLines = strLines.ToArray().RemoveAllCommentedBlocks(blockStartExpression, blockEndExpression);
            return allLines.ToList();
        }

        /// <summary>
        /// </summary>
        /// <param name="str"></param>
        /// <param name="values"></param>
        /// <returns></returns>
        public static bool ContainsAll(this string str, params string[] values)
        {
            if (!string.IsNullOrEmpty(str) || (values.Length > 0))
                return values.All(value => (str != null) && str.Contains(value));

            return false;
        }

        /// <summary>
        /// </summary>
        /// <param name="str"></param>
        /// <param name="appendSpaces"></param>
        /// <param name="values"></param>
        /// <returns></returns>
        public static bool ContainsAll(this string str, bool appendSpaces, params string[] values)
        {
            var space = string.Empty;
            if (appendSpaces)
                space = " ";
            if (!string.IsNullOrEmpty(str) || (values.Length > 0))
                return values.All(value => (str != null) && str.Contains(space + value + space));
            return false;
        }

        /// <summary>
        /// </summary>
        /// <param name="inputArray"></param>
        /// <param name="lineBreakElement"></param>
        /// <returns></returns>
        public static string[] CombineAllBrokenLines(this string[] inputArray, char lineBreakElement)
        {
            if (inputArray.Length <= 0) return inputArray;
            var tempList = new List<string>();
            var indexPosition = -1;
            var tempString = string.Empty;
            for (var i = 0; i < inputArray.Length; i++)
            {
                indexPosition++;
                var regex = new Regex(@"[_\s]$");
                if (regex.IsMatch(inputArray[i].TrimEnd()))
                {
                    if (inputArray[i].TrimStart().TrimEnd().StartsWith("'")) continue;
                    for (var j = i; j < inputArray.Length; j++)
                    {
                        if (regex.IsMatch(inputArray[j].TrimEnd()))
                        {
                            tempString += inputArray[j].Remove(inputArray[j].Length - 1, 1) + " ";
                            indexPosition++;
                            continue;
                        }
                        tempString += inputArray[j];
                        tempList.Add(tempString);
                        tempString = string.Empty;
                        break;
                    }
                    i = indexPosition;
                }
                else
                    tempList.Add(inputArray[i]);
            }
            return tempList.ToArray();
        }

        /// <summary>
        /// </summary>
        /// <param name="inputArray"></param>
        /// <returns></returns>
        public static string[] DoIfLoopsAdjustment(this string[] inputArray)
        {
            var adjustedStringArray = new List<string>();
            foreach (var currentLine in inputArray)
            {
                if (currentLine.StartsWith("ElseIf"))
                {
                    adjustedStringArray.Add(currentLine);
                    continue;
                }
                if (currentLine.ContainsAll("If ", " Then"))
                {
                    adjustedStringArray.Add(currentLine);
                    if (!currentLine.After(" Then ")) continue;

                    adjustedStringArray.Add("End If");
                }
                else
                {
                    adjustedStringArray.Add(currentLine);
                }
            }
            return adjustedStringArray.ToArray();
        }

        public static string[] IfLoopsAdjustmentCobol(this string[] inputArray)
        {
            var adjustedStringArray = new List<string>();
            var stmtIf = false;
            foreach (var currentLine in inputArray)
            {
                if (stmtIf)
                    if (currentLine.Trim().EndsWith("."))
                    {
                        if (!currentLine.Trim().StartsWith("END"))
                        {
                            adjustedStringArray.Add(currentLine);
                            adjustedStringArray.Add("END-IF");
                            stmtIf = false;
                            continue;
                        }
                        adjustedStringArray.Add(currentLine);
                        stmtIf = false;
                        continue;
                    }

                if (currentLine.ContainsAll("IF "))
                {
                    adjustedStringArray.Add(currentLine);
                    stmtIf = true;
                    if (!currentLine.After(".")) continue;
                    adjustedStringArray.Add("END-IF");
                }
                else
                {
                    adjustedStringArray.Add(currentLine);
                }
            }
            return adjustedStringArray.ToArray();
        }

        /// <summary>
        /// </summary>
        /// <param name="value"></param>
        /// <param name="a"></param>
        /// <returns></returns>
        public static bool After(this string value, string a)
        {
            var posA = value.LastIndexOf(a, StringComparison.Ordinal);
            return posA != -1;
        }

        /// <summary>
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="source"></param>
        /// <returns></returns>
        public static IEnumerable<T> GetDuplicates<T>(this IEnumerable<T> source)
        {
            var itemsSeen = new HashSet<T>();
            var itemsYielded = new HashSet<T>();

            foreach (var item in source)
                if (!itemsSeen.Add(item))
                    if (itemsYielded.Add(item))
                        yield return item;
        }

        /// <summary>
        /// All links with origin 
        /// Get all child nodes with current links origin, map Id with node 
        /// </summary>
        /// <param name="links"></param>
        /// <param name="nodes"></param>
        /// <param name="origin"></param>
        /// <param name="allChildNodes"></param>
        /// <returns></returns>
        public static List<Node> GetAllChildNodesForLinkOrigin(this List<Link> links, List<Node> nodes,
            int origin, List<Node> allChildNodes)
        {
            var childNodes = (from lnk in links
                              where lnk.Origin == origin
                              select lnk.Target into p
                              select nodes.Find(n => n.Id == p)).Distinct().ToList();

            var presentChilds = (from c in childNodes where allChildNodes.Any(d => d.Id == c.Id) select c).ToList();
            foreach (var p in presentChilds)
            {
                childNodes.Remove(p);
            }

            allChildNodes.AddRange(childNodes);
            /*
            foreach (var cNode in childNodes)
            {
                FindChildItems(links, nodes, cNode, allChildNodes);
            }
            */
            return allChildNodes;
        }

        /// <summary>
        ///     <para>This method is to remove all nodes which has no parent as well as child items</para>
        ///     <para></para>
        ///     <para> Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="lstNodes"></param>
        /// <param name="lstLinks"></param>
        /// <param name="keepNodes"></param>
        /// <returns></returns>
        public static List<Node> RemoveHangingNodes(this List<Node> lstNodes, List<Link> lstLinks,
            params Node[] keepNodes)
        {
            var tempListNodes = lstNodes.FindAll(s => keepNodes.ToList().All(d => d.Name == s.Name));
            var missingNodes = lstNodes.Select(x => x.Id).ToList().Except(lstLinks.Select(y => y.Origin)).ToList();
            if (missingNodes.Count <= 0) return lstNodes;

            foreach (var mNode in missingNodes)
            {
                var iCnt = (from dd in lstLinks
                            where dd.Target == mNode
                            select dd.Target).ToList();
                if (iCnt.Count == 0)
                    lstNodes.RemoveAll(x => x.Id == mNode);
            }
            foreach (var node in tempListNodes)
            {
                var node1 = node;
                var n = from s in lstNodes where s.Id == node1.Id select s;
                if (n.Any()) continue;

                lstNodes.Add(node);
            }
            return lstNodes;
        }

        /// <summary>
        ///     <para>This method is to remove commented portion within line</para>
        ///     <para></para>
        ///     <para> Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="tempString"></param>
        /// <returns></returns>
        public static string RemoveCommentedPartFromLine(this string tempString)
        {
            var count = tempString.Count(x => x == '\'');
            var pattern = ".*\\((.*?)\"(.*?)\"(.*?)\\)";
            var startPattern = ".*\"(.*?)\"";
            if (count % 2 == 0) return tempString.Trim();
            if (count % 2 != 0)
            {
                var match = Regex.Match(tempString, pattern);
                if (match.Success)
                {
                    var matchedString = match.Groups[0].Value; // regex.Match(tempString).Value;
                    return matchedString;
                }
                var subStr = tempString.Substring(0, tempString.LastIndexOf('\''));
                return subStr;
            }
            if (count % 2 != 0) return tempString;
            var openingBracketPosition = tempString.IndexOf('(');
            var doubleQuoteCharPosition = tempString.IndexOf('\"');
            if (doubleQuoteCharPosition < openingBracketPosition)
            {
                var match2 = Regex.Match(tempString, startPattern);
                if (match2.Success)
                {
                    var matchedString = match2.Groups[0].Value; // regex.Match(tempString).Value;
                    return matchedString;
                }
            }
            else
            {
                var match2 = Regex.Match(tempString, pattern);
                if (match2.Success)
                {
                    var matchedString = match2.Groups[0].Value; // regex.Match(tempString).Value;
                    return matchedString;
                }
            }

            var otherMatch = Regex.Match(tempString, "(.*?)\"(.*?)\"");
            if (otherMatch.Success)
            {
                var matchedString = otherMatch.Groups[1].Value; // regex.Match(tempString).Value;
                return matchedString;
            }
            var subStr2 = tempString.Substring(0, tempString.LastIndexOf('\''));
            return subStr2;
        }

        public static List<TreeView> DistinctBy(this IEnumerable<TreeView> collection)
        {
            var distinctList = new List<TreeView>();

            var values = collection as IList<TreeView> ?? collection.ToList();
            foreach (var value in values)
            {
                if (!distinctList.Any(v => value.NodeId == v.NodeId || v.GraphId == value.GraphId))
                    distinctList.Add(value);
            }
            return distinctList;
        }

        public static List<Node> DistinctByNodeId(this IEnumerable<Node> collection)
        {
            var distinctList = new List<Node>();

            var values = collection as IList<Node> ?? collection.ToList();
            foreach (var value in values)
            {
                if (!distinctList.Exists(v => value.StatementId == v.StatementId))
                    distinctList.Add(value);
            }
            return distinctList;
        }

        public static List<TreeView> GetOnlyIfBlockContainingElse(this List<TreeView> inputList)
        {
            var finalList = new List<TreeView>();
            var treeViewList = inputList.ToList();

            if (inputList.Count(s => s.BaseCommandId == 10) == 1) return treeViewList;
            int indexPos = -1;
            int endIfCounter = -1;
            foreach (var list in inputList)
            {
                indexPos++;
                if (list.BaseCommandId != 10) continue;
                for (int i = indexPos; i < inputList.Count; i++)
                {
                    finalList.Add(inputList[i]);
                    if (inputList[i].BaseCommandId == 1)
                        endIfCounter--;
                    if (inputList[i].BaseCommandId == 2)
                        endIfCounter++;
                    if (endIfCounter == 0)
                        break;
                }
                return finalList;
            }
            return finalList;
        }

        public static List<TreeView> AssignColorsToMethodBlocks(this List<TreeView> copyOfLstTreeView)
        {
            // string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
            var tempId = 0; var groupId = 1;
            foreach (var treeItem in copyOfLstTreeView.FindAll(t => t.BaseCommandId == 8).ToList())
            {
                var childItems = (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
                foreach (var child in childItems)
                {
                    if (tempId > 3) tempId = 0;
                    if (child.BaseCommandId != 5 && child.BaseCommandId != 8) continue;
                    var groupName = string.Empty;
                    if (child.StatementReferenceMaster == null) continue;
                    if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                    {
                        var iOreCall = child.BaseCommandId == 6 ? "External Call" : "Internal Call";
                        var methodCalled = child.StatementReferenceMaster.MethodCalled.Substring(0,
                            child.StatementReferenceMaster.MethodCalled.IndexOf('('));
                        var bName = child.StatementReferenceMaster.BusinessName;
                        if (string.IsNullOrEmpty(bName)) bName = methodCalled;
                        groupName = "(" + bName.Trim() + ")(" + methodCalled + ")(" + iOreCall + ")";
                        groupId = groupId + 1;
                    }
                    AssignColorsToChildNodes(child, ref copyOfLstTreeView, groupName, groupId);
                    tempId++;
                }
            }
            return copyOfLstTreeView;
        }

        private static void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string groupName, int groupId)
        {
            var childItems = (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
            var methodChildItems = new List<TreeView>();
            methodChildItems.AddRange(childItems);

            foreach (var child in childItems)
            {
                var allChilds = (from s in lstTreeView where s.ParentId == child.GraphId select s).ToList();
                methodChildItems.AddRange(allChilds);
            }

            foreach (var child in methodChildItems)
            {
                child.GroupName = groupName;
                child.GroupId = groupId;
                child.IndentLevel = child.IndentLevel + 2;
            }
        }

        public static List<TreeView> IfBlockStatement(this List<TreeView> allSeqListItems, List<TreeView> lstTreeView)
        {
            int indexPosition = -1;
            int ifCounter = 0;
            foreach (var treeItem in allSeqListItems)
            {
                indexPosition++;
                if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                if (treeItem.BaseCommandId != 1 && treeItem.PrimaryCommandId != 1) continue;

                var treeViewList = new List<TreeView>();
                for (int i = indexPosition; i < lstTreeView.Count; i++)
                {
                    treeViewList.Add(lstTreeView[i]);
                    if (lstTreeView[i].BaseCommandId == 1 || lstTreeView[i].PrimaryCommandId == 1)
                        ifCounter++;
                    if (lstTreeView[i].BaseCommandId == 2 || lstTreeView[i].PrimaryCommandId == 2)
                        ifCounter--;
                    if (ifCounter == 0)
                        break;
                }
                var prevParentId = treeViewList.First().ParentId;
                var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
                treeViewList.First().GraphId = graphId;
                for (int j = 1; j < treeViewList.Count; j++)
                {
                    if (treeViewList[j].ParentId != prevParentId) continue;
                    treeViewList[j].ParentId = graphId;

                    treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    if (treeViewList[j].BaseCommandId == 2)
                        treeViewList[j].IndentLevel = treeViewList.First().IndentLevel;
                }
            }
            return lstTreeView;
        }

        public static List<TreeView> LoopBlockStatement(this List<TreeView> allSeqListItems, List<TreeView> lstTreeView)
        {
            int indexPosition = -1;
            int loopCounter = 0;

            foreach (var treeItem in allSeqListItems)
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
                    var childItems = (from s in allSeqListItems where s.ParentId == treeViewList[j].GraphId select s).ToList();
                    childItems.ForEach(c => { c.IndentLevel = c.IndentLevel + 2; });
                    treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    if (treeViewList[j].BaseCommandId == 4)
                        treeViewList[j].IndentLevel = curIndentLevel + 1;
                }
            }
            return allSeqListItems;
        }

        public static List<TreeView> ElseBlockStatement(this List<TreeView> allSeqListItems, List<TreeView> lstTreeView)
        {
            int indexPosition = -1;
            foreach (var treeItem in allSeqListItems)
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
                var parentIf = allSeqListItems.First(f => f.GraphId == treeViewList.First().ParentId);
                treeViewList.First().IndentLevel = parentIf.IndentLevel;
                for (var j = 1; j < treeViewList.Count; j++)
                {
                    if (treeViewList[j].BaseCommandId == 2 || treeViewList[j].BaseCommandId == 9) continue;
                    if (treeViewList[j].ParentId != prevParentId) continue;

                    treeViewList[j].ParentId = graphId;
                    treeViewList[j].IndentLevel = curIndentLevel + 1;
                }
            }
            return allSeqListItems;
        }

        // TODO: We need method to extract file name from provided statement.
        /// <summary>
        /// This is not completed yet. Please try avoiding this method.
        /// </summary>
        /// <param name="statement"></param>
        /// <param name="startTokens"></param>
        /// <returns></returns>
        public static string GetFileName(this string statement, params string[] startTokens)
        {
            var preTokens = new List<string>(); // {"CALL @", "CALL "}};
            if (startTokens.Any()) preTokens.AddRange(startTokens);

            foreach (string token in preTokens)
            {
                if (statement.Contains(token))
                {

                }
            }

            return "Method not implemented";
        }
        public static string ExtractUniverseFileName(this string statement)
        {
            if (string.IsNullOrEmpty(statement)) return "File-Not-Found";
            string inputStatement = statement.CheckCommentInStatement();
            string newStatement = Regex.Replace(inputStatement, @"\s+", " ").Trim();
            var callRegEx = new Regex(@"^CALL\s+(.*?(?=\())", RegexOptions.IgnoreCase | RegexOptions.Singleline);
            var phExePhantomRegEx = new Regex(@"^(EXECUTE\s+|PH\s+|PHANTOM\s+)", RegexOptions.IgnoreCase);
            var includeInsertRegEx = new Regex(@"([a-zA-Z0-9\.\\_]+$)", RegexOptions.IgnoreCase);
            var runRegEx = new Regex(@"^(RUN\s+)");

            if (callRegEx.IsMatch(newStatement))
            {
                string tempObjName = callRegEx.Match(newStatement).Groups[1].Value;
                if (string.IsNullOrEmpty(tempObjName)) return "File-Not-Found";
                string objName = tempObjName.Trim().Trim('@').Trim();
                return objName;
            }

            if (includeInsertRegEx.IsMatch(newStatement) && !runRegEx.IsMatch(newStatement)
                    && !phExePhantomRegEx.IsMatch(newStatement))
            {
                string tempObjName = includeInsertRegEx.Match(newStatement).Groups[1].Value;
                if (string.IsNullOrEmpty(tempObjName)) return "File-Not-Found";
                string objName = tempObjName.Trim().Trim(' ');
                return objName;
            }

            if (phExePhantomRegEx.IsMatch(newStatement))
            {
                string tempObjName = includeInsertRegEx.Match(newStatement).Groups[1].Value;
                if (string.IsNullOrEmpty(tempObjName)) return "File-Not-Found";
                string objName = tempObjName.Trim().Trim(' ');
                return objName;
            }

            if (!runRegEx.IsMatch(newStatement)) return "File-Not-Found";

            var tempObjNames = newStatement.Split(' ');
            if (tempObjNames.Length <= 2) return "File-Not-Found";

            string tempObjectName = tempObjNames.Skip(2).First();
            if (string.IsNullOrEmpty(tempObjectName)) return "";
            string objectName = tempObjectName.Trim().Trim(' ');
            return objectName;
        }
        public static string ExtractIncludeFileName(this string statement)
        {
            if (string.IsNullOrWhiteSpace(statement) || string.IsNullOrEmpty(statement)) return "";
            string includeFileName = statement.StartsWith("$INSERT")
                ? statement.Split('>').Last()
                : statement.Split(' ').Last();
            if (!string.IsNullOrWhiteSpace(includeFileName) && !string.IsNullOrEmpty(includeFileName)) return includeFileName;

            string insertFileName = statement.Split(' ').Last();
            if (string.IsNullOrWhiteSpace(insertFileName) || string.IsNullOrEmpty(insertFileName)) return "";

            return insertFileName;
        }
    }
}