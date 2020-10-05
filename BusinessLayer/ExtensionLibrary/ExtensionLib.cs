using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using BusinessLayer.Models;

namespace BusinessLayer.ExtensionLibrary
{
    /// <summary>
    ///     This partial class intensionally used only for Universe Basic Language processing...
    ///     Add any extension method here in this file only for Universe Basic
    /// </summary>
    public static partial class ExtensionLibrary
    {
        private static readonly List<string> LstReadList =
            new List<string> { "MATREADU ", "READVU ", "READV ", "OPEN ", "WEOF ", "WRITESEQ " };
        /*
        private static readonly List<string> LstIfAndLoopStarts =
          new List<string> { "IF ", "MATREADU ", "MATREAD ", "WEOF ", "WRITESEQ ", "LOCATE " };
        private static readonly List<string> LstIfAndLoopEnds =
            new List<string> { "END", "END-IF", "END IF" };
        */

        /*
        private static readonly List<string> LstIfAndLoopStarts =
          new List<string> { "IF ", "FOR ", "LOOP", "WITH ", "WHILE " };
        private static readonly List<string> LstIfAndLoopEnds =
            new List<string> { "END", "NEXT ", "END-IF", "END IF", "REPEAT", "End While" };
        */

        /// <summary>Finds the STOP statement within program and pick up exact location</summary>
        /// <para></para>
        /// <para>Created by: Yogesh Sonawane</para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> GetListUpToLastStop(this List<string> allLines)
        {
            var copyOfAllLines = new List<string>();
            var onlyGoSubsList = allLines.GetOnlyGoSub("GOSUB");

            foreach (var line in allLines)
            {
                if (line.StartsWith("STOP"))
                {
                    string thisLine = line.CheckCommentInStatement();
                    copyOfAllLines.Add(thisLine);
                    continue;
                }
                copyOfAllLines.Add(line);
            }
            allLines = copyOfAllLines.ToList();

            var stopLinesCount = allLines.Count(l => l == "STOP");
            if (stopLinesCount <= 0) return allLines;

            int sIndex = -1;
            foreach (var line in allLines)
            {
                sIndex++;
                string thisLine = line.CheckCommentInStatement();
                if (!onlyGoSubsList.Select(goSub => goSub.CheckCommentInStatement())
                         .Select(methodName => methodName.Trim() + ":")
                         .Any(mName => thisLine == mName || thisLine == mName + "*" || thisLine == mName + " *"))
                    continue;
                var lnRange = allLines.GetRange(0, sIndex);
                return lnRange;

                /*
                foreach (var goSub in onlyGoSubsList)
                {
                    string methodName = goSub.CheckCommentInStatement();
                    string mName = methodName.Trim();
                    if (thisLine != mName && thisLine != mName + "*" && thisLine != mName + " *") continue;
                    var lnRange = allLines.GetRange(0, sIndex);
                    return lnRange;
                }
                */
            }

            int ifCounts = allLines.Count(s => s.StartsWith("IF "));
            int endIfCounts = allLines.Count(s => s == "END");
            Console.WriteLine("IF's are: " + ifCounts + " And END IF's are: " + endIfCounts);
            int stopCounts = allLines.Count(s => s == "STOP");
            Console.WriteLine("STOP's Counts: " + stopCounts);

            int sCounts = 0;
            for (int i = 0; i < allLines.Count; i++)
            {
                if (allLines[i] == "STOP") sCounts = i;
            }
            var setOfLines = allLines.GetRange(0, sCounts + 1);

            if (stopCounts == 1)
                return setOfLines;

            //int iPos = -1;
            /*
            int iCounts = 0;
            for (int k = 0; k < sCounts + 1; k++)
            {
                string currentLine = setOfLines[k];
                if (LstIfAndLoopStarts.Any(s => currentLine.StartsWith(s)) || currentLine == "BEGIN CASE")
                    iCounts++;
                if (LstIfAndLoopEnds.Any(s => s == currentLine) || currentLine == "END CASE")
                    iCounts--;

                if (currentLine != "STOP") continue;
                if (iCounts != 0) continue;

                setOfLines[k] = "STOP @#THIS IS LAST STOP";
                allLines[k] = "STOP @#THIS IS LAST STOP";
                var range = allLines.GetRange(0, k + 1);
                return range;
            }
            */

            var linesSubset = new List<string>();
            allLines.Reverse();
            var indexPos = -1;

            foreach (var line in allLines)
            {
                indexPos++;
                if (line != "STOP") continue;

                var range = allLines.GetRange(indexPos, allLines.Count - indexPos);
                linesSubset.AddRange(range);
                linesSubset.Reverse();
                allLines.Reverse();
                break;
            }
            return linesSubset;
        }

        public static List<string> GetAllIncludeStatements(this List<string> inputList, params string[] startIndicator)
        {
            var allIncludes =
            (from input in inputList
             where
             startIndicator.Any(s => input.StartsWith(s + " "))
             let inName = input.CheckCommentInStatement()
             select inName).ToList();
            return allIncludes;
        }

        public static string[] CaseAdjustmentUniverse(this string[] inputArray)
        {
            var adjustedStringArray = new List<string>();
            var iBasicCase = 0;
            var iCase = 0;
            foreach (var currentLine in inputArray)
            {
                if (currentLine.Trim().StartsWith("*")) continue;
                if (currentLine.Trim().StartsWith("BEGIN CASE") || currentLine.Trim().StartsWith("CASE "))
                {
                    if (currentLine.Trim().StartsWith("BEGIN CASE"))
                    {
                        iBasicCase++;
                    }
                    else if (currentLine.Trim().StartsWith("CASE "))
                    {
                        if (iCase > 0 && iBasicCase == 1)
                            adjustedStringArray.Add(" END");
                        if (iBasicCase > 1)
                            iBasicCase--;
                        iCase++;
                    }

                    if (iBasicCase > 0 && iCase > 0)
                        adjustedStringArray.Add(currentLine);
                    else if (iBasicCase > 0)
                        adjustedStringArray.Add(currentLine);
                }
                else if (currentLine.Trim().StartsWith("END CASE"))
                {
                    adjustedStringArray.Add(" END");
                    adjustedStringArray.Add(currentLine);
                }
                else
                {
                    adjustedStringArray.Add(currentLine);
                }
            }
            return adjustedStringArray.ToArray();
        }

        public static string[] IfAdjustmentUniverseJcl(this string[] inputArray)
        {
            var adjustedStringArray = new List<string>();
            var iIf = 0;
            foreach (var currentLine in inputArray)
            {
                if (currentLine.Trim().StartsWith("*")) continue;
                if (currentLine.Trim().StartsWith("IF ") && currentLine.Trim().Contains("THEN ")
                    && currentLine.Trim().Contains("GO "))
                {
                    if (!currentLine.Trim().StartsWith("IF ")) continue;
                    iIf = iIf + 1;
                    if (iIf > 0)
                    {
                        var i = currentLine.IndexOf("THEN", StringComparison.Ordinal);
                        var secondString = currentLine.Substring(i);
                        var firstString = currentLine.Replace(secondString, "").Trim();

                        if (firstString.Length > 0)
                            adjustedStringArray.Add(firstString);
                        iIf--;
                    }
                    else
                    {
                        adjustedStringArray.Add(currentLine);
                    }
                }
                else
                {
                    adjustedStringArray.Add(currentLine);
                }
            }
            return adjustedStringArray.ToArray();
        }

        public static List<string> ModifyEndWithThenOrElseOrLockedStatement(this string input, out bool lOrMOr)
        {
            lOrMOr = false;
            var searchResults = new List<string>();
            if (input.EndsWith("THEN") && !input.StartsWith("LOCATE "))
            {
                var modify = input.Replace("THEN", "");
                searchResults.Add(modify.Trim());
                searchResults.Add("IF SUCCESS THEN");
            }
            else if (input.EndsWith("ELSE") && !input.StartsWith("LOCATE "))
            {
                var modify = input.Replace("ELSE", "");
                searchResults.Add(modify.Trim());
                searchResults.Add("IF NOT SUCCESS THEN");
            }
            else if (input.StartsWith("LOCATE ") && input.EndsWith(" ELSE"))
            {
                // TODO: Need to discuss with AJ...
                var modify = input.Replace("LOCATE", "FIND").Replace("ELSE", "INTO FOUND");
                searchResults.Add(modify.Trim());
                searchResults.Add("IF NOT FOUND");
                lOrMOr = true;
            }
            else if (input.StartsWith("LOCATE ") && input.EndsWith("THEN"))
            {
                var modify = input.Replace("LOCATE", "FIND").Replace("THEN", "INTO FOUND");
                searchResults.Add(modify.Trim());
                searchResults.Add("IF FOUND");
                lOrMOr = true;
            }
            return searchResults;
        }

        public static string CheckCommentInStatement(this string input, out string commentedPart)
        {
            commentedPart = string.Empty;
            if (string.IsNullOrEmpty(input)) return input;

            var thisString = input.Trim();
            if (!thisString.IsValid())
                return thisString;

            int commentIndex = thisString.LastIndexOf(';');
            thisString = thisString.Substring(0, commentIndex);
            commentedPart = input.Substring(commentIndex);
            thisString = thisString.Trim();
            return thisString;
        }

        public static string CheckCommentInStatement(this string input)
        {
            if (string.IsNullOrEmpty(input)) return input;

            var thisString = input.Trim();
            if (!thisString.IsValid())
                return thisString;

            thisString = thisString.Substring(0, thisString.LastIndexOf(';'));
            thisString = thisString.Trim();
            return thisString;
        }

        public static List<string> ExtractCaseStatementBlock(this List<string> statementsList)
        {
            // Extract case block only
            int indexPosition = -1;
            var copyOfStatementsList = statementsList.ToList();
            foreach (var line in statementsList)
            {
                indexPosition++;
                if (line != "BEGIN CASE") continue;

                var caseBlock = copyOfStatementsList.GetBlockOfLinesAsList(indexPosition, "END CASE");
                var caseBlockList = caseBlock.ConvertCaseBlockWithIf();
                copyOfStatementsList.RemoveRange(indexPosition, caseBlock.Count);
                copyOfStatementsList.InsertRange(indexPosition, caseBlockList);
            }
            return copyOfStatementsList;
        }

        private static List<string> ConvertCaseBlockWithIf(this List<string> caseBlockList)
        {
            caseBlockList = caseBlockList.Select(s => s.Trim()).ToList();
            var copyOfCaseBlock = caseBlockList.ToList();
            int indexPos = -1;
            foreach (var line in caseBlockList)
            {
                indexPos++;
                if (line == "BEGIN CASE")
                    copyOfCaseBlock[indexPos] = "";
                if (line == "END CASE")
                    copyOfCaseBlock[indexPos] = "END IF";
            }
            copyOfCaseBlock = copyOfCaseBlock.FindAll(s => !string.IsNullOrEmpty(s));
            caseBlockList = copyOfCaseBlock.ToList();
            copyOfCaseBlock = new List<string>();
            indexPos = -1;
            List<string> caseConditions = new List<string>();
            int caseCounter = 0;
            foreach (var line in caseBlockList)
            {
                indexPos++;
                var sentense = line.CheckCommentInStatement();
                if (sentense == "CASE OTHERWISE")
                {
                    string caseOtherwise = caseConditions
                        .Aggregate(string.Empty, (current, condition) => current + condition + " OR ");
                    if (string.IsNullOrEmpty(caseOtherwise))
                        caseOtherwise = " OR ";
                    caseOtherwise = caseOtherwise.Substring(0,
                        caseOtherwise.LastIndexOf("OR", StringComparison.CurrentCulture));
                    caseOtherwise = "IF NOT (" + caseOtherwise + ") THEN";
                    copyOfCaseBlock.Add("END IF");
                    copyOfCaseBlock.Add(caseOtherwise);
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter == 0)
                {
                    caseConditions.Add(sentense.Replace("CASE", ""));
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter >= 1)
                {
                    caseConditions.Add(sentense.Replace("CASE", ""));
                    copyOfCaseBlock.Add("END IF");
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                copyOfCaseBlock.Add(sentense);
            }
            return copyOfCaseBlock;
        }

        public static bool IsValid(this string str)
        {
            var valid = Regex.IsMatch(str, @"(?:[\;\*][\;\s\*]+)");
            if (valid && !str.Contains(';'))
                return false;

            return valid;
        }

        public static List<string> GetOnlyGoSub(this List<string> lstFileStatements, string expression)
        {
            expression = expression + " ";
            var allGoSubs = (from g in lstFileStatements
                             where g.StartsWith(expression)
                             let m = g.Replace(expression, "")
                             select m.Trim() + ":").ToList();
            var newList = allGoSubs.Distinct().ToList();
            return newList;
        }

        public static List<string> GetAllGoSub(this List<string> lstFileStatements, string expression)
        {
            expression = expression + " ";
            var allGoSubs = (from g in lstFileStatements
                             where g.StartsWith(expression)
                             let m = g.Replace(expression, "")
                             select m.Trim() + ":").ToList();

            allGoSubs.AddRange(from thisStmt in lstFileStatements
                               where Regex.IsMatch(thisStmt, @"^\d") || Regex.IsMatch(thisStmt, @"^(.*\:\*)")
                               select Regex.Replace(thisStmt, @"[^0-9A-Za-z\.]?", string.Empty)
                                   into thisStmt
                               select thisStmt + ":");

            //if (Regex.IsMatch(currentSentance, @"^\d"))
            //currentSentance = Regex.Replace(currentSentance, @"[^0-9\.]?", string.Empty);
            var newList = allGoSubs.Distinct().ToList();
            return newList;
        }

        public static bool VerifyMethodBlockForIfWithMatchingEndIf(this List<string> lstMethodBlock)
        {
            var afterRemovingComments = new List<string>();
            lstMethodBlock.ForEach(line =>
            {
                string withoutComments = line.CheckCommentInStatement();
                afterRemovingComments.Add(withoutComments);
            });
            var ifCount = afterRemovingComments.Count(s => !string.IsNullOrEmpty(s) && s.StartsWith("IF "));
            var endIfCount = afterRemovingComments.Count(s => !string.IsNullOrEmpty(s) && (s == "END" || s == "END IF"));
            /*
            int ifCounter = 0;
            foreach (var statement in lstMethodBlock)
            {
                if (statement.StartsWith("IF "))
                    ifCounter++;
                if (statement == "END")
                    ifCounter--;
            }
            */
            var check = ifCount == endIfCount;
            return check;
        }

        public static List<string> ReModifyMethodBlockForIfWithMatchingEndIf(this List<string> lstMethodBlock)
        {
            var newList = new List<string>();
            foreach (var statement in lstMethodBlock)
            {
                if (string.IsNullOrEmpty(statement))
                {
                    newList.Add(statement);
                    continue;
                }
                if (statement == "END" || statement == "END IF" || statement == "END-IF")
                {
                    /*
                    if (lOrM)
                    {
                        newList.Add(statement);
                        lOrM = false;
                    }
                    */
                    bool check = newList.VerifyMethodBlockForIfWithMatchingEndIf();
                    if (!check)
                        newList.Add(statement);
                    continue;
                }

                if (!statement.EndsWith("ELSE") && !statement.EndsWith("THEN"))
                {
                    newList.Add(statement);
                    continue;
                }
                if (statement.StartsWith("IF "))
                {
                    newList.Add(statement);
                    continue;
                }
                if (statement.StartsWith("END ELSE") || statement.StartsWith("ELSE"))
                {
                    newList.Add(statement);
                    continue;
                }

                // bool lOrM;
                // var list = statement.ModifyEndWithThenOrElseOrLockedStatement(out lOrM);
                bool lOrM;
                var list = statement.ModifyEndWithThenOrElseOrLockedStatement(out lOrM); // lOrM replaced by _
                newList.AddRange(list);
            }
            return newList;
        }

        public static List<string> AdjustMatReadLockedLikeStatements(this List<string> inputStatementsList)
        {
            var newList = new List<string>();
            foreach (var input in inputStatementsList)
            {
                if (LstReadList.Any(w => input.StartsWith(w)))
                {
                    var list = input.SplitStatement();
                    newList.AddRange(list);
                    continue;
                }
                if (input.StartsWith("READ ") || input.StartsWith("READLIST "))
                {
                    var list = input.SplitReadOpenStatement();
                    newList.AddRange(list);
                    continue;
                }
                newList.Add(input);
            }
            return newList;
        }

        /// <summary>
        ///     <para>This is spacial case for JCL files to get block of EXEC statements.</para>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputStringList"></param>
        /// <param name="startIndex"></param>
        /// <param name="startExpression"></param>
        /// <param name="upToStringExpression"></param>
        /// <returns></returns>
        public static List<string> GetBlockOfLinesAsList(this List<string> inputStringList,
            int startIndex, string startExpression, string upToStringExpression)
        {
            var lstBlock = new List<string>();
            if (inputStringList.Count <= 0) return inputStringList;
            var startExpCount = 0;
            var endExpCount = 0;
            for (var index = startIndex; index < inputStringList.Count; index++)
            {
                var input = inputStringList[index];
                if (input == startExpression)
                {
                    lstBlock.Add(input);
                    startExpCount++;
                    endExpCount--;
                    continue;
                }
                if (input == upToStringExpression)
                {
                    lstBlock.Add(input);
                    endExpCount++;
                    startExpCount--;
                    continue;
                }
                if (startExpCount == 0 && endExpCount == 0)
                    break;

                lstBlock.Add(input);
            }
            return lstBlock;
        }

        /// <summary>
        ///     <para> Returns true is BEGIN CASE statements are equal to END CASE count</para>
        ///     <para></para>
        ///     <para>Created by: Yogesh Sonawane</para>
        /// </summary>
        /// <param name="inputList"></param>
        /// <returns></returns>
        public static bool CheckBeginAndEndCaseCounts(this List<string> inputList)
        {
            var beginCaseCount = inputList.Count(s => s == "BEGIN CASE");
            var endCaseCount = inputList.Count(s => s == "END CASE");
            var check = beginCaseCount == endCaseCount;
            return check;
        }

        private static IEnumerable<string> SplitReadOpenStatement(this string statement)
        {
            var methodBlockListMain = new List<string>();
            if (statement.EndsWith("THEN"))
            {
                statement = statement.Replace("THEN", "");
                methodBlockListMain.Add(statement);
                methodBlockListMain.Add("IF FOUND THEN");
            }
            else if (statement.EndsWith("ELSE"))
            {
                statement = statement.Replace("ELSE", "");
                methodBlockListMain.Add(statement);
                methodBlockListMain.Add("IF NOT-FOUND THEN");
            }
            return methodBlockListMain;
        }

        private static IEnumerable<string> SplitStatement(this string statement)
        {
            statement = statement.CheckCommentInStatement();
            var methodBlockListMain = new List<string>();
            if (statement.StartsWith("WEOF ") && statement.EndsWith("ELSE"))
            {
                string stmt = statement.Replace("ELSE", "");
                methodBlockListMain.Add(stmt);
                methodBlockListMain.Add("IF NOT-SUCCESS THEN");
                return methodBlockListMain;
            }
            if (statement.EndsWith("LOCKED"))
            {
                methodBlockListMain.Add(statement);
                methodBlockListMain.Add("IF RECORD-ID-EXISTS");
            }
            else if (statement.StartsWith("OPEN "))
            {
                if (statement.EndsWith("ELSE"))
                {
                    string stmt = statement.Replace("ELSE", "");
                    methodBlockListMain.Add(stmt);
                    methodBlockListMain.Add("IF NOT-SUCCESS THEN");
                }
                else if (statement.EndsWith("THEN"))
                {
                    string stmt = statement.Replace("THEN", "");
                    methodBlockListMain.Add(stmt);
                    methodBlockListMain.Add("IF SUCCESS THEN");
                }
                else if (statement.Contains("ELSE"))
                {
                    List<string> lstElse = statement.Split(new[] { "ELSE" }, StringSplitOptions.None).ToList();
                    methodBlockListMain.Add(lstElse[0]);
                    methodBlockListMain.Add("IF NOT-SUCCESS THEN");
                    methodBlockListMain.Add(lstElse[1]);
                    methodBlockListMain.Add("END");
                }
            }
            else if (statement.EndsWith("ELSE"))
            {
                statement = statement.Replace("ELSE", "");
                methodBlockListMain.Add(statement);
                methodBlockListMain.Add("IF NOT-SUCCESS THEN");
            }
            else if (statement.EndsWith("THEN"))
            {
                statement = statement.Replace("THEN", "");
                methodBlockListMain.Add(statement);
                methodBlockListMain.Add("IF SUCCESS THEN");
            }
            return methodBlockListMain;
        }

        public static
            List<string> SimplifyCaseAndNestedCaseStatementsBlock(this List<string> caseStatementBlock)
        {
            var copyOfCaseStatements = caseStatementBlock.ToList();
            var baginCaseCnt = copyOfCaseStatements.Count(s => s == "BEGIN CASE");
            if (baginCaseCnt > 1)
            {
                var indexPosition = -1;
                foreach (var caseLine in caseStatementBlock)
                {
                    indexPosition = indexPosition + 1;
                    if (indexPosition == 0) continue;
                    if (caseLine != "BEGIN CASE") continue;

                    baginCaseCnt--;
                    if (baginCaseCnt != 0) continue;

                    var caseBlock = caseStatementBlock.
                        GetBlockOfLinesAsList(indexPosition, "BEGIN CASE", "END CASE");

                    var list = caseBlock.ConvertCaseBlockWithIf();
                    bool check = list.VerifyMethodBlockForIfWithMatchingEndIf();
                    if (check)
                    {
                        copyOfCaseStatements.RemoveRange(indexPosition, caseBlock.Count);
                        copyOfCaseStatements.InsertRange(indexPosition, list);
                    }
                    else
                    {
                        var thisList = list.ReModifyMethodBlockForIfWithMatchingEndIf();
                        copyOfCaseStatements.RemoveRange(indexPosition, caseBlock.Count);
                        copyOfCaseStatements.InsertRange(indexPosition, thisList);
                    }
                }
            }

            var listNew = copyOfCaseStatements.ExtractCaseStatementBlock();
            bool checkNew = listNew.VerifyMethodBlockForIfWithMatchingEndIf();
            if (checkNew)
            {
                copyOfCaseStatements = listNew;
                return copyOfCaseStatements;
            }

            var newList = listNew.ReModifyMethodBlockForIfWithMatchingEndIf();
            bool recheck = newList.VerifyMethodBlockForIfWithMatchingEndIf();
            if (!recheck) return newList;

            copyOfCaseStatements = newList;
            return copyOfCaseStatements;
        }

        public static List<string> PickUpAllMethodBlocks(this List<string> inputList, string startingPoint,
            List<string> allGoSub, params string[] otherEndPoints)
        {
            var outputList = new List<string>();
            var index = inputList.FindIndex(a => a.StartsWith(startingPoint));
            if (index == -1)
            {
                outputList.Add(null);
                return outputList;
            }

            index = index + 1;

            var goupTo4Lines = index - 4;
            var methodComments = string.Empty;
            for (var length = index; length >= 0; length--)
            {
                var line = inputList[length];
                if (length == goupTo4Lines)
                {
                    if (methodComments == string.Empty) methodComments = null;
                    break;
                }

                if (!line.StartsWith("* ")) continue;
                methodComments = line.Split('*').LastOrDefault();
                break;
            }
            outputList.Add(methodComments);
            /*
            string regExString = String.Empty;
            int start = 0;
            foreach (var endPoint in otherEndPoints)
            {
                if (start == 0)
                    regExString += "(" + endPoint + "\\s\\S+?[\\s])";
                else
                    regExString += "|(" + endPoint + "\\s\\S+?[\\s])";

                start++;
            }
            Regex regExPattern = new Regex(regExString, RegexOptions.IgnoreCase);
            */

            for (var length = index; length < inputList.Count; length++)
            {
                var line = inputList[length];
                if (line.StartsWith("*")) continue;

                outputList.Add(line);
                if (line.StartsWith("*")) break;
                if (otherEndPoints.Any(l => l == line)) break;
                if (!allGoSub.Any(l => line.StartsWith(l))) continue;

                outputList.Remove(line);
                break;
            }
            /*
            foreach (string line in inputList)
            {
                outputList.Add(line);
                if (otherEndPoints.Any(l => line.StartsWith(l))) break;
                if (allGoSub.Any(l => line.StartsWith(l))) break;
            }
            */
            return outputList;
        }

        public static string StripHtmlTags(this string source)
        {
            string plainText = Regex.Replace(source, "<.*?>", string.Empty);
            plainText = Regex.Replace(plainText, "&nbsp;", string.Empty);
            return plainText;
        }

        /// <summary>This is to replace all symbolic string to normal string. </summary>
        /// <para>Ex. Ne will be replaced as is not equal to,  </para>
        /// <para>Ex. >= will br replaced as is equal to or greater than</para>
        /// <para>Created by: Shubhangi Pawar</para>
        /// <param name="inputStr"></param>
        /// <returns></returns>
        public static string FormatSymbolicString(this string inputStr)
        {
            inputStr = inputStr.Replace(" >= ", " is greater than or equal to ");
            inputStr = inputStr.Replace(" <= ", " is less than or equal to ");
            inputStr = inputStr.Replace(" => ", " is equal to or greater than ");
            inputStr = inputStr.Replace(" =< ", " is equal to or less than ");
            inputStr = inputStr.Replace(">= ", " is greater than or equal to ");
            inputStr = inputStr.Replace("<= ", " is less than or equal to ");
            inputStr = inputStr.Replace("=> ", " is equal to or greater than ");
            inputStr = inputStr.Replace("=< ", " is equal to or less than ");
            inputStr = inputStr.Replace(" != ", " is not equal to ");
            inputStr = inputStr.Replace(" Ne ", " is not equal to ");
            inputStr = inputStr.Replace(" <> ", " is not equal to ");
            inputStr = inputStr.Replace(" Gt ", " is greater than ");
            inputStr = inputStr.Replace(" Lt ", " is less than ");
            inputStr = inputStr.Replace(" > ", " is greater than ");
            inputStr = inputStr.Replace(" < ", " is less than ");
            inputStr = inputStr.Replace(" = ", " is equal to ");
            inputStr = inputStr.Replace(" Not= ", " is not equal to ");
            inputStr = inputStr.Replace(" NE ", " is not equal to ");
            inputStr = inputStr.Replace(")NE ", ") is not equal to ");
            inputStr = inputStr.Replace(" THEN", " ").Replace(" Then", " ").Replace("then ", " ");
            return inputStr;
        }

        public static string ReplaceString(this string input)
        {
            if (string.IsNullOrEmpty(input)) return input;

            input = input.Replace(" >= ", " greater than or equal to ");
            input = input.Replace(" <= ", " less than or equal to ");
            input = input.Replace(" => ", " equal to or greater than ");
            input = input.Replace(" =< ", " equal to or less than ");
            input = input.Replace(">= ", " greater than or equal to ");
            input = input.Replace("<= ", " less than or equal to ");
            input = input.Replace("=> ", " equal to or greater than ");
            input = input.Replace("=< ", " equal to or less than ");
            input = input.Replace(" != ", " not equal to ");
            input = input.Replace(" Ne ", " not equal to ");
            input = input.Replace(" <> ", " not equal to ");
            input = input.Replace(" Gt ", " greater than ");
            input = input.Replace(" GE ", " greater or equal to ");
            input = input.Replace(" LE ", " less than or equal to ");
            input = input.Replace(" Lt ", " less than ");
            input = input.Replace(" > ", " greater than ");
            input = input.Replace(" < ", " less than ");
            input = input.Replace(" = ", " equal to ");
            input = input.Replace(" Not= ", " not equal to ");
            input = input.Replace(" NE ", " not equal to ");
            input = input.Replace(")NE ", ") not equal to ");
            return input;
        }

        public static List<TreeView> AttachChildItems(this List<TreeView> allSeqListItems, List<TreeView> secondTab,
            TreeView curItem)
        {
            allSeqListItems.Add(curItem);
            var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
            foreach (var cItem in childItems)
            {
                allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
            }
            return allSeqListItems;
        }

        public static string GetLineComment(this string input)
        {
            var regEx = new Regex(@"(?:[\;\*][\;\s\*]+)(.*)", RegexOptions.IgnoreCase);
            if (!regEx.IsMatch(input)) return "";

            var groupCollection = regEx.Match(input).Groups;
            var comment = groupCollection.Count >= 2 ? groupCollection[1].Value : "";
            return comment;
        }
    }
}