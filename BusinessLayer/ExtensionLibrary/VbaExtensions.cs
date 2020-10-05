using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace BusinessLayer.ExtensionLibrary
{
    public static class VbaExtensions
    {
        /// <summary>
        ///     <para>This method is to remove commented portion within line</para>
        ///     <para></para>
        ///     <para> Created by: Shubhangi Pawar</para>
        /// </summary>
        /// <param name="tempString"></param>
        /// <returns></returns>
        public static string VbaRemoveCommentedPartFromLineNew(this string tempString)
        {
            var count = tempString.Count(x => x == '\'');
            var pattern = ".*\\((.*?)\"(.*?)\"(.*?)\\)";
            var startPattern = ".*\"(.*?)\"";
            if (count%2 == 0) return tempString.Trim();
            if (count%2 != 0)
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
            if (count%2 != 0) return tempString;
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


            /*
            string subStr2 = string.Empty;
            if (!tempString.Contains("'")) return subStr2;

            var count = tempString.Count(x => x == '\'');
            if (count <= 0) return subStr2.Trim();
            if (count%2 == 0) return tempString.Trim();
            int index = tempString.LastIndexOf('\'');
            string substring = tempString.Substring(0, index).Trim();
            return substring;
              */

            /*
            string subStr2 = string.Empty;
            if (!tempString.Contains("'")) return subStr2;

            var count = tempString.Count(x => x == '\'');
            if (count <= 0) return subStr2;

            string[] value = tempString.Split('\'');
            string lastOrDefault = value[0].Split(' ').LastOrDefault();
            if (lastOrDefault != null && lastOrDefault.Equals(""))
                subStr2 = value[0].Trim();
            else
                subStr2 = tempString;

            return subStr2;
             */
        }

        public static string VbaRemoveCommentedPartFromLine(this string tempString)
        {
            var regexFir = new Regex("(.*)(\"(.*)\'(.*)\")");
            var regexSec = new Regex("\'(.*)");
            if (regexFir.IsMatch(tempString))
            {
                return tempString.Trim();
            }
            if (regexSec.IsMatch(tempString))
            {
                int index = tempString.LastIndexOf('\'');
                string substring = tempString.Substring(0, index).Trim();
                return substring;
            }
            return tempString;
        }

        public static List<string> VbaAlterWithStatement(this List<string> allLines)
        {
            var methodBlockListMain = new List<string>();
            string withoutIf = string.Empty;
            int flagWith = 0;
            foreach (var method in allLines)
            {
                if (flagWith == 0)
                {
                    if (!method.StartsWith("With "))
                        methodBlockListMain.Add(method);
                    else
                    {
                        flagWith = 1;
                        withoutIf = method.Replace("With", "");
                    }
                }
                else
                {
                    if (flagWith != 1) continue;

                    string stratingDot = method;
                    if ((!stratingDot.StartsWith('.'.ToString()) && stratingDot.Contains('.')) ||
                        stratingDot.StartsWith('.'.ToString()))
                    {
                        string d = string.Empty;
                        string[] strval = stratingDot.Split('=');
                        for (int i = 0; i < strval.Length; i++)
                        {
                            if (i > 0)
                            {
                                if (strval[i].Trim().StartsWith("."))
                                    d = d + "=" + withoutIf + strval[i];
                                else
                                    d = d + "=" + strval[i];
                            }
                            else
                            {
                                if (strval[i].Trim().StartsWith("."))
                                    d = d + " " + withoutIf + strval[i];
                                else
                                    d = d + " " + strval[i];
                            }
                        }
                        methodBlockListMain.Add(d.TrimStart().Trim());
                    }
                    else if (stratingDot.StartsWith("End With"))
                        flagWith = 0;
                    else
                        methodBlockListMain.Add(method);
                }
            }
            return methodBlockListMain;
        }

        public static List<string> VbaRemoveBeginFormStatement(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodMain = allLines.ToArray();
            methodMain = methodMain.CombineAllSelectStatmentForRecordSource('\"');
            int flag = 0;
            foreach (var method in methodMain)
            {
                if (method.StartsWith("Begin Form") || method.StartsWith("Begin Report"))
                    flag = 1;
                if (method.StartsWith("CodeBehindForm"))
                    flag = 0;
                if (flag == 0 || method.StartsWith("RecordSource"))
                    selectBlock.Add(method);
            }
            return selectBlock;
        }

        public static List<int> FindSubFormIndexes(this List<string> tSource, Func<string, bool> predicate)
        {
            int num = -1;
            var indexList = new List<int>();
            foreach (string source in tSource)
            {
                num++;
                if (predicate(source))
                    indexList.Add(num);
            }
            return indexList;
        }

        /// <summary>
        /// Split statements which has IF and THEN and END IF in same statement
        /// </summary>
        /// <para><!----></para>
        /// <para>Created By: Shubhangi Pawar</para>
        /// <param name="allLines"></param>
        /// <param name="universe"></param>
        /// <returns></returns>
        public static List<string> SplitIfElseThenStatement(this List<string> allLines, bool universe)
        {
            var programLineListMainList = new List<string>();
            foreach (var lineList in allLines)
            {
                if (string.IsNullOrEmpty(lineList)) continue;
                if (lineList.StartsWith("IF ") || lineList.StartsWith("If ") || lineList.StartsWith("if "))
                {
                    if (lineList.ToUpper().Contains(" THEN"))
                    {
                        int index = lineList.ToUpper().IndexOf(" THEN", StringComparison.Ordinal);
                        string result = lineList.Substring(0, index + 5);
                        programLineListMainList.Add(result);
                        string result1 = lineList.Substring(index + 5);
                        if (!string.IsNullOrEmpty(result1))
                        {
                            programLineListMainList.Add(result1.Trim());
                            if (universe)
                                programLineListMainList.Add("END IF");
                        }
                        continue;
                    }
                }
                programLineListMainList.Add(lineList);
            }
            return programLineListMainList;
        }

        /// <summary>
        /// Split statements which has ELSEIF in same statement
        /// </summary>
        /// <para>
        /// Created By: Shubhangi Pawar
        /// </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> SplitElseIfStatements(this IEnumerable<string> allLines)
        {
            var mainBlock = new List<string>();
            bool lineCheck = false;

            foreach (var line in allLines)
            {
                if (line.StartsWith("ElseIf"))
                {
                    lineCheck = true;
                    string[] block = line.Split(new[] {"If"}, StringSplitOptions.None);
                    if (block.Length < 2) continue;
                    mainBlock.Add(block[0]);
                    mainBlock.Add("If" + block[1]);
                    continue;
                }
                if (line.StartsWith("Else") || line.StartsWith("End If"))
                {
                    if (lineCheck)
                        mainBlock.Add("End If");

                    mainBlock.Add(line);
                    lineCheck = false;
                    continue;
                }
                mainBlock.Add(line);
            }
            return mainBlock;
        }
    }
}