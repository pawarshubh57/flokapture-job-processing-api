using BusinessLayer.DbEntities;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace BusinessLayer.ExtensionLibrary
{
    public static class CobolVersionBatchExtenions
    {
        /// <summary> Prepare  same  length for alllines </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> PrepareSameLength(this List<string> allLines)
        {
            var mainLines = new List<string>();
            var maxLength = allLines.OrderByDescending(s => s.Length).First().Length;
            if (maxLength <= 72)
                maxLength = 72;
            foreach (var line in allLines)
            {
                var currentLine = line;
                var newLine = currentLine.PadRight(maxLength);
                mainLines.Add(newLine);
            }

            return mainLines;
        }

        ///<summary> Remove Start and end character form string </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"> </param>
        /// <param name="startPosition"> </param>
        /// <param name="length"> </param>
        /// <returns></returns>
        public static List<string> RemoveCharacter(this List<string> allLines, int startPosition, int length)
        {
            var mainLines = new List<string>();
            foreach (var line in allLines)
            {
                var newLine = line.Substring(startPosition, length);
                newLine = newLine.TrimEnd();
                // if (string.IsNullOrWhiteSpace(newLine)) continue;
                mainLines.Add(newLine);
            }

            return mainLines;
        }

        /// <summary> Remove all commented lines </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="comments"></param>
        /// <returns></returns>
        public static List<string> RemoveAllCommentedLines(this List<string> allLines, string[] comments)
        {
            var mainBlock = new List<string>();
            foreach (var line in allLines)
            {
                var newLine = line.Trim();
                if (string.IsNullOrWhiteSpace(newLine)) continue;
                var isPresent = comments.Any(x => newLine.StartsWith(x));
                if (isPresent) continue;
                // if (string.IsNullOrWhiteSpace(newLine)) continue;
                mainBlock.Add(line);
            }

            return mainBlock;
        }

        /// <summary> GET PROCEDURE DIVISION SECTION </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> GetProcedureDivisionSection(this List<string> allLines)
        {
            var mainBlock = new List<string>();
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;
                var regexPattern = "(.*PROCEDURE DIVISION.*)";
                if (!Regex.IsMatch(line, regexPattern)) continue;
                var remainingRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                mainBlock.AddRange(remainingRange);
                break;
                /*
                for (int i = indexPosition; i < allLines.Count; i++)
                {
                    var newLine = allLines[i];
                    mainBlock.Add(newLine);
                }
                break;
                */
            }

            return mainBlock;
        }

        /// <summary> Remove all unwanted lines </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="paramaters"></param>
        /// <returns></returns>
        public static List<string> RemoveAll(this List<string> allLines, params string[] paramaters)
        {
            var regexes = paramaters.Select(parameter => parameter + "(.*)").ToList();
            allLines.RemoveAll(x => regexes.Any(r => Regex.IsMatch(x, r, RegexOptions.IgnoreCase)));
            return allLines;
        }

        /// <summary> Remove all null or empty lines form list </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> RemoveEmptyLines(this List<string> allLines)
        {
            var mainBlockList = new List<string>();
            foreach (var line in allLines)
            {
                if (String.IsNullOrWhiteSpace(line)) continue;
                mainBlockList.Add(line);
            }

            return mainBlockList;
        }

        /// <summary> Get All Cobol file MathodsName Into PROCEDURE DIVISION </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        ///  <param name="expression"></param>
        /// <returns></returns>
        public static List<string> GetAllCobolMethodsIntoProcedureDivision(this List<string> allLines, string expression)
        {
            var allMethods = new List<string>();

            foreach (var line in allLines)
            {
                if (line.Length <= 6) continue;
                char firstChar = line[1];
                if (char.IsSymbol(firstChar)) continue;
                string methodString = line.Substring(0, 4);
                if (methodString.Is8To11NonSpaceChar()) continue;
                // if (char.IsWhiteSpace(firstChar) || firstChar == '*') continue;
                allMethods.Add(line);
                // string newLine = line.Replace(expression, ".");
                // allMethods.Add(newLine);
            }

            allMethods = allMethods.Distinct().ToList();
            return allMethods;
        }

        public static bool Is8To11NonSpaceChar(this string input)
        {
            return input.Length >= 4 && input.All(char.IsWhiteSpace);
        }

        /// <summary> Modify statement method Name After Dot </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        ///  <param name="methodNameList"></param>
        /// <returns></returns>
        /// <example> B-MAIN-EXIT. EXIT.</example>
        public static List<string> ModifyMethodsNameAfterDot(this List<string> allLines, List<string> methodNameList)
        {
            var mainBlockList = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line;
                if (methodNameList.Any(x => currentLine == x))
                {
                    if (!currentLine.Contains("."))
                    {
                        mainBlockList.Add(currentLine);
                    }
                    else
                    {
                        string[] arrLine = currentLine.Split('.');
                        {
                            foreach (var arr in arrLine)
                            {
                                if (string.IsNullOrWhiteSpace(arr)) continue;
                                mainBlockList.Add(arr);
                            }

                            continue;
                        }
                    }
                }

                {
                    mainBlockList.Add(currentLine);
                }
            }

            return mainBlockList;
        }

        /// <summary>
        /// Collected all method lines into data dictionary 
        /// </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="methodNameList"></param>
        /// <param name="dictionary"></param>
        /// <returns></returns>
        public static Dictionary<string, List<string>> CollectAllMethodsStatements(this List<string> allLines,
            List<string> methodNameList, Dictionary<string, List<string>> dictionary)
        {

            int indexPosition = 0;
            foreach (var line in allLines)
            {
                //  string[] arrLine = line.Split('.');
                var methodName = line.Trim();
                var methodsLines = new List<string>();
                indexPosition++;
                if (methodNameList.All(x => methodName != x)) continue;
                for (int i = indexPosition; i < allLines.Count; i++)
                {
                    string thisLine = allLines[i].Trim();
                    if (string.IsNullOrEmpty(thisLine)) continue;
                    if (i == allLines.Count - 1)
                    {
                        // methodsLines = methodsLines.CombineAllExecSqlStatements();
                        dictionary.Add(methodName, methodsLines);
                    }

                    if (methodNameList.Any(x => thisLine == x)) //|| i == allLines.Count - 1
                    {
                        if (i == allLines.Count - 1) methodsLines.Add(thisLine);
                        if (dictionary.Keys.Any(k => k == methodName))
                            methodName = methodName + " DefineMethod_" + i;
                        // methodsLines = methodsLines.CombineAllExecSqlStatements();
                        dictionary.Add(methodName, methodsLines);
                        break;
                    }

                    methodsLines.Add(thisLine);
                }
            }

            return dictionary;
        }

        /// <summary> Remove dot from method name </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="methodList"></param>
        /// <returns></returns>
        public static List<string> RemoveDot(this List<string> methodList)
        {
            var methodNamesList = new List<string>();
            foreach (var method in methodList)
            {
                var line = method.Trim();
                if (!line.EndsWith(".")) continue;
                string[] newLine = line.Split('.');
                var nLine = newLine[0].Trim();
                methodNamesList.Add(nLine);
            }

            return methodNamesList;
        }

        /// <summary> Get WORKING-STORAGE SECTION </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="cobolSection"></param>
        /// <param name="sectionName"></param>
        /// <returns></returns>
        public static List<string> GetWorkingStorageSection(this List<string> allLines, List<string> cobolSection,
            string sectionName)
        {
            var mainMethodBlock = new List<string>();
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;
                const string regexPattern = @"(.*WORKING-STORAGE SECTION.*)";
                if (!Regex.IsMatch(line, regexPattern)) continue;
                var exist = false;
                for (int i = indexPosition; i < allLines.Count; i++)
                {
                    var currentLine = allLines[i];
                    if (currentLine.StartsWith("*")) continue;
                    foreach (var section in cobolSection)
                    {
                        var regexLine = @"(.*" + section + ".*)";
                        if (!Regex.IsMatch(currentLine, regexLine) ||
                            Regex.IsMatch(currentLine, regexPattern)) continue;
                        exist = true;
                        break;
                    }
                    if (exist) break;
                    /*
                    var regexPatternProcedure = "(.*PROCEDURE DIVISION.*)";
                    if (Regex.IsMatch(currentLine, regexPatternProcedure)) break;
                    */
                    mainMethodBlock.Add(currentLine);
                }
            }
            return mainMethodBlock;
        }

        /// <summary>Get All Data division section </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="cobolSection"></param>
        /// <param name="sectionName"></param>
        /// <returns></returns>
        public static List<string> GetStatementBetweenSection(this List<string> allLines, List<string> cobolSection,
            string sectionName)
        {
            var mainMethodBlock = new List<string>();
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;
                var newLine = line.Trim();
                var regexPattern = @"(.*" + sectionName + "*)";
                if (!Regex.IsMatch(newLine, regexPattern)) continue;
                for (int i = indexPosition + 1; i < allLines.Count; i++)
                {
                    var currentLine = allLines[i];
                    currentLine = currentLine.Trim();
                    if (currentLine.StartsWith("*")) continue;
                    var exist = cobolSection.Any(x => x.StartsWith(currentLine));
                    if (exist) break;
                    mainMethodBlock.Add(currentLine);
                }
            }
            return mainMethodBlock;
        }

        /// <summary>  Combine all lines for working storage sections. </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> CombineAllLinesInWorkingStorageSection(this List<string> allLines)
        {
            if (allLines.Count <= 0) return allLines;
            var mainMethodBlock = new List<string>();
            var tempString = string.Empty;
            foreach (var line in allLines)
            {
                var currentLine = line.TrimEnd();
                if (currentLine.EndsWith("."))
                {
                    if (!string.IsNullOrEmpty(tempString))
                    {
                        tempString += currentLine;
                        mainMethodBlock.Add(tempString);
                        tempString = string.Empty;
                        continue;
                    }
                    mainMethodBlock.Add(currentLine);
                    continue;
                }
                if (string.IsNullOrEmpty(tempString))
                    tempString += currentLine;
                else
                    tempString += " " + currentLine;
            }
            return mainMethodBlock;
        }

        /// <summary> Get all Copy calls into working storage section
        /// <para>Created by: Shubhani Pawar </para>
        /// </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> GetCopyStatementInWorkingStorageSection(this List<string> allLines)
        {
            if (allLines.Count <= 0) return allLines;
            var finalList = new List<string>();
            foreach (var line in allLines)
            {
                var newLine = line.Trim();
                var regex = @"^\s|COPY\s";
                if (Regex.IsMatch(newLine, regex))
                    finalList.Add(newLine);
            }
            return finalList;
        }

        /// <summary>Replace particular statement </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="replaceWith"></param>
        /// <param name="replaceTo"></param>
        /// <returns></returns>
        public static List<string> ReplaceCurrentStatement(this List<string> allLines, string replaceWith,
            string replaceTo)
        {
            var mainListBlock = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line;
                if (currentLine.StartsWith(replaceWith))
                {
                    currentLine = currentLine.Replace(replaceWith, replaceTo);
                    mainListBlock.Add(currentLine);
                    continue;
                }
                mainListBlock.Add(currentLine);
            }
            return mainListBlock;
        }

        /// <summary> Combine Move To statement single move statement line </summary>
        /// <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <param name="cobolKeyWords"></param>
        /// <returns></returns>
        public static List<string> CombineLineForMoveToStatement(this List<string> allLines, List<string> cobolKeyWords)
        {
            if (allLines.Count < 0) return allLines;
            var mainMethodBlock = new List<string>();
            string moveLineStatement = string.Empty;
            foreach (var line in allLines)
            {
                if (cobolKeyWords.Any(x => line.StartsWith(x)) && !line.StartsWith("MOVE "))
                {
                    mainMethodBlock.Add(line);
                    continue;
                }
                if (line.StartsWith("MOVE "))
                {
                    if (line.EndsWith("TO ") || !line.Contains(" TO "))
                    {
                        moveLineStatement = line;
                        continue;
                    }
                    mainMethodBlock.Add(line);
                    continue;
                }
                if (!string.IsNullOrEmpty(moveLineStatement))
                {
                    if (cobolKeyWords.Any(x => line.StartsWith(x)) || line == ".")
                    {
                        mainMethodBlock.Add(line);
                        moveLineStatement = string.Empty;
                    }
                    else
                    {
                        var newLine = moveLineStatement + " " + line;
                        mainMethodBlock.Add(newLine);
                    }
                }
                else
                {
                    mainMethodBlock.Add(line);
                    moveLineStatement = string.Empty;
                }
            }
            return mainMethodBlock;
        }

        /// <summary>
        /// Join all move statement with To lines 
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="cobolKeyWords"></param>
        /// <returns></returns>
        public static List<string> ConvertAllMoveStatement(this List<string> allLines, List<string> cobolKeyWords)
        {
            if (allLines.Count < 0) return allLines;
            var mainMethodBlock = new List<string>();
            string[] toStr = { "TO" };
            string moveLineStatement = string.Empty;
            foreach (var line in allLines)
            {
                if (line == ".")
                {
                    mainMethodBlock.Add(line);
                    moveLineStatement = string.Empty;
                    continue;
                }

                if (line.StartsWith("MOVE ") && line.Contains(" TO "))
                {
                    string[] moveStatement = line.Split(toStr, StringSplitOptions.None);
                    moveLineStatement = moveStatement[0];
                    mainMethodBlock.Add(line);
                }
                else
                {
                    if (!string.IsNullOrEmpty(moveLineStatement))
                    {
                        if (cobolKeyWords.Any(x => line.StartsWith(x)) || line == ".")
                        {
                            mainMethodBlock.Add(line);
                            moveLineStatement = string.Empty;
                        }
                        else
                        {
                            var newLine = moveLineStatement + " TO " + line;
                            mainMethodBlock.Add(newLine);
                        }
                    }
                    else
                    {
                        mainMethodBlock.Add(line);
                        moveLineStatement = string.Empty;
                    }
                }
            }

            return mainMethodBlock;
        }

        /// <summary>
        /// Add EndIf for If statements
        /// </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> AddEndIfStatement(this List<string> allLines)
        {
            var mainMethodBlock = new List<string>();
            int ifCounter = 0;
            foreach (var line in allLines)
            {
                if (line.StartsWith("IF "))
                {
                    ifCounter++;
                    mainMethodBlock.Add(line);
                }
                else if (line == "ELSE")
                {
                    mainMethodBlock.Add(line);
                }
                else if (line == "END-IF")
                {
                    var cnt = CountStringOccurrences(line, "END-IF");
                    if (cnt != 1)
                    {
                        for (int i = 0; i < cnt; i++)
                        {
                            mainMethodBlock.Add("END-IF ");
                            ifCounter--;
                        }
                    }
                    else
                    {
                        ifCounter--;
                        mainMethodBlock.Add(line);
                    }
                }
                else if (line == "END-IF.")
                {
                    for (int i = 0; i < ifCounter; i++)
                    {
                        mainMethodBlock.Add("END-IF ");
                    }

                    ifCounter = 0;
                }
                else
                {
                    if (ifCounter != 0)
                    {
                        if (line.EndsWith("."))
                        {
                            mainMethodBlock.Add(line);
                            for (int i = 0; i < ifCounter; i++)
                            {
                                mainMethodBlock.Add("END-IF ");
                            }

                            ifCounter = 0;
                            continue;
                        }
                    }
                    mainMethodBlock.Add(line);
                }
            }
            return mainMethodBlock;
        }

        public static int CountStringOccurrences(string text, string pattern)
        {
            // Loop through all instances of the string 'text'.
            int count = 0;
            int i = 0;
            while ((i = text.IndexOf(pattern, i, StringComparison.Ordinal)) != -1)
            {
                i += pattern.Length;
                count++;
            }

            return count;
        }

        /// <summary>
        /// Split line for multiple kewords 
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="cobolKeyWords"></param>
        /// <returns></returns>

        public static List<string> AddNewLineForMultipleKeyoword(this List<string> allLines, List<string> cobolKeyWords)
        {
            var mainMethodBlock = new List<string>();
            foreach (var line in allLines)
            {
                if (line.StartsWith("EXEC CICS") || line.StartsWith("EXEC CICS "))
                {
                    mainMethodBlock.Add(line);
                    continue;
                }
                var cnt = line.Split(' ').Count(word => cobolKeyWords.Any(s => s == word));
                if (cnt <= 1) { mainMethodBlock.Add(line); continue; }

                int firstIndex = line.IndexOf('\'');
                int lastIndex = line.LastIndexOf('\'');

                string newLine = string.Empty;
                var lineWords = line.Split(' ').ToList();
                int indexPosition = 0;
                foreach (var key in lineWords)
                {
                    indexPosition++;
                    if (string.IsNullOrWhiteSpace(key)) continue;
                    if (cobolKeyWords.Any(x => key == x)
                        && line.IndexOf(key, StringComparison.InvariantCulture) < firstIndex
                        && line.IndexOf(key, StringComparison.InvariantCulture) > lastIndex)
                    {
                        if (!string.IsNullOrEmpty(newLine)) mainMethodBlock.Add(newLine);
                        newLine = string.Empty;
                        newLine += key;
                        continue;
                    }

                    newLine += key + " ";
                    if (indexPosition == lineWords.Count) mainMethodBlock.Add(newLine);
                }
            }

            return mainMethodBlock;
        }

        /// <summary>
        /// Split line for Dot.
        /// </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>

        public static List<string> SplitAllLinesAfterDot(this List<string> allLines)
        {
            var mainBlockList = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line.Split('.');
                foreach (var cLine in currentLine)
                {
                    if (string.IsNullOrEmpty(cLine)) continue;
                    mainBlockList.Add(cLine);
                }
            }
            return mainBlockList;
        }

        /// <summary> Combine all lines whos not start with Cobol keyword </summary>
        /// <param name="allLines"></param>
        /// <param name="methodNameList"></param>
        /// <param name="cobolKeyWords"></param>
        /// <returns></returns>

        public static List<string> CombineAllLines(this List<string> allLines, List<string> methodNameList, List<string> cobolKeyWords)
        {
            if (allLines.Count <= 1) return allLines;
            var combinedLines = new List<string>();
            for (int cnt = 0; cnt < allLines.Count; cnt++)
            {
                string currentLine = allLines[cnt];

                if (currentLine == "EXEC SQL" || currentLine.StartsWith("EXEC CICS"))
                {
                    var mainLine = currentLine.Trim();
                    for (int j = cnt + 1; j < allLines.Count; j++)
                    {
                        var nLine = allLines[j].Trim();
                        mainLine += " " + nLine;
                        if (nLine != "END-EXEC" && nLine != "END-EXEC.") continue;
                        combinedLines.Add(mainLine);
                        cnt = j;
                        break;
                    }
                }
                else if (!cobolKeyWords.Any(k => currentLine.StartsWith(k)))
                {
                    string tempLine = string.Empty;
                    for (int i = cnt; i < allLines.Count; i++)
                    {
                        string nextLine = allLines[i].Trim();
                        if (!cobolKeyWords.Any(k => nextLine.StartsWith(k)))
                        {
                            tempLine += " " + nextLine;
                            cnt = i;
                        }
                        if (!cobolKeyWords.Any(k => nextLine.StartsWith(k) || i + 1 >= allLines.Count)) continue;
                        var lastItem = combinedLines.LastOrDefault();
                        if (string.IsNullOrEmpty(lastItem))
                        {
                            combinedLines.Add(tempLine);
                        }
                        else
                        {
                            lastItem += tempLine;
                            combinedLines[combinedLines.Count - 1] = lastItem;
                        }
                        break;
                    }
                }
                else
                {
                    combinedLines.Add(currentLine);
                }
            }
            return combinedLines;
        }

        public static List<string> GetAllExecStatementUptoEndExec(this List<string> allLines)
        {
            if (allLines.Count <= 1) return allLines;
            var combinedLines = new List<string>();
            for (int cnt = 0; cnt < allLines.Count; cnt++)
            {
                string currentLine = allLines[cnt];
                if (currentLine == "EXEC SQL" || currentLine.StartsWith("EXEC CICS"))
                {
                    var mainLine = currentLine.Trim();
                    for (int j = cnt + 1; j < allLines.Count; j++)
                    {
                        var nLine = allLines[j].Trim();
                        mainLine += " " + nLine;
                        if (nLine != "END-EXEC" && nLine != "END-EXEC.") continue;
                        combinedLines.Add(mainLine);
                        cnt = j;
                        break;
                    }
                }
                else
                {
                    combinedLines.Add(currentLine);
                }
            }
            return combinedLines;
        }

        /// <summary> Conversion of EVALUATE Statement block With IF ELSE Block </summary>
        /// <para>Created by:Shubhangi Pawar</para>
        /// <param name="allLines"></param>
        /// <returns></returns>

        public static List<string> ConversionOfEvaluateStatement(this List<string> allLines)
        {
            var mainBlock = new List<string>();
            for (var cnt = 0; cnt < allLines.Count; cnt++)
            {
                var tempBlock = new List<string>();
                var currentLine = allLines[cnt];
                if (!currentLine.StartsWith("EVALUATE "))
                {
                    mainBlock.Add(currentLine);
                    continue;
                }
                for (int i = cnt; i < allLines.Count; i++)
                {
                    var newLine = allLines[i];
                    if (newLine == "END-EVALUATE")
                    {
                        tempBlock.Add(newLine);
                        tempBlock = tempBlock.PickUpOnlyEvaluteBlock();
                        mainBlock.AddRange(tempBlock);
                        cnt = i;
                        break;
                    }
                    tempBlock.Add(newLine);
                }
            }
            return mainBlock;
        }

        /// <summary>Pick up EVALUATE Block </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> PickUpOnlyEvaluteBlock(this List<string> allLines)
        {
            var copyOfBlock = allLines.ToList();
            int indexPostiosion = -1;
            foreach (var line in allLines)
            {
                indexPostiosion++;
                if (line.StartsWith("EVALUATE"))
                    copyOfBlock[indexPostiosion] = "";
                if (line == "END-EVALUATE")
                    copyOfBlock[indexPostiosion] = "END-IF";
            }
            int counter = 0;
            allLines = new List<string>();
            allLines.AddRange(copyOfBlock.Where(line => !string.IsNullOrEmpty(line)));
            copyOfBlock = new List<string>();
            List<string> whenConditions = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line;
                if (line == "WHEN OTHER")
                {
                    string caseOther = whenConditions.Aggregate(string.Empty,
                        (current, condition) => current + condition + " OR ");
                    if (string.IsNullOrEmpty(caseOther))
                        caseOther = " OR ";
                    caseOther = caseOther.Substring(0, caseOther.LastIndexOf("OR", StringComparison.CurrentCulture));
                    caseOther = "IF NOT (" + caseOther + ") THEN";
                    copyOfBlock.Add("END-IF");
                    copyOfBlock.Add(caseOther);
                    continue;
                }
                if (line.StartsWith("WHEN ") && counter == 0)
                {
                    whenConditions.Add(currentLine.Replace("WHEN", ""));
                    currentLine = currentLine.Replace("WHEN ", "IF ") + " THEN ";
                    copyOfBlock.Add(currentLine);
                    counter++;
                    continue;
                }
                if (line.StartsWith("WHEN ") && counter >= 1)
                {
                    whenConditions.Add(currentLine.Replace("WHEN", ""));
                    copyOfBlock.Add("END-IF");
                    currentLine = currentLine.Replace("WHEN ", "IF ") + " THEN";
                    copyOfBlock.Add(currentLine);
                    counter++;
                    continue;
                }
                copyOfBlock.Add(currentLine);
            }
            return copyOfBlock;
        }

        public static List<string> PerfromVarying(this List<string> allLines, List<string> methodNameList, List<CobolVariable> cobolVariableList)
        {
            var mainMethodList = new List<string>();
            if (allLines.Count == 0) return mainMethodList;
            foreach (var line in allLines)
            {
                if (!line.StartsWith("PERFORM "))
                {
                    mainMethodList.Add(line);
                    continue;
                }
                var mainBlock = line.ConversionDoWhile(methodNameList, cobolVariableList);
                mainMethodList.AddRange(mainBlock);
            }
            return mainMethodList;
        }

        public static List<string> ConversionDoWhile(this string text, List<string> methodNameList, List<CobolVariable> cobolVariableList)
        {
            var mainBlockList = new List<string>();
            var perFormStmtList = new List<string>();
            var methodList = methodNameList.Select(list => list.Replace(".", "")).ToList();
            var currentStatement = text;
            string strDeclare = string.Empty;
            string strWhile = string.Empty;
            string variable = string.Empty;

            if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL") &&
                currentStatement.Contains("VARYING"))
            {
                #region  logic PERFORM UNTIL VARYING
                string[] arrlast = currentStatement.Split(new[] { "UNTIL" }, StringSplitOptions.None);
                if (arrlast.Length > 1)
                {
                    strWhile = arrlast[1].WhileConversion();
                    strWhile = "WHILE (" + strWhile + ")";
                    strWhile = strWhile.Replace(".", "");
                }
                var newLine = arrlast[0];
                var regexSet = @"VARYING(\s+[A-z0-9\-]+\s+)FROM ";
                if (Regex.IsMatch(newLine, regexSet))
                {
                    var match = Regex.Match(newLine, regexSet);
                    strDeclare = match.Groups[1].Value;
                    variable = "SET " + strDeclare;
                }
                var regexPerfrom = @"(PERFORM.*)VARYING";
                if (Regex.IsMatch(newLine, regexPerfrom))
                {
                    var match = Regex.Match(newLine, regexPerfrom);
                    string strPerform = match.Groups[1].Value;
                    perFormStmtList = strPerform.LogicOfPerfromThru(methodList, cobolVariableList);
                    if (perFormStmtList.Count == 0)
                        perFormStmtList.Add(strPerform);

                }
                var regexFrom = @"(FROM.*)UNTIL";
                if (Regex.IsMatch(currentStatement, regexFrom))
                {
                    var match = Regex.Match(currentStatement, regexFrom);
                    string strFrom = match.Groups[1].Value;
                    var regexFm = @"FROM\s+(.*)\s+BY\s+(.*)";
                    if (Regex.IsMatch(strFrom, regexFm))
                    {
                        var matchFrom = Regex.Match(strFrom, regexFm);
                        variable += "= " + matchFrom.Groups[1].Value;
                        strDeclare = strDeclare + "=" + strDeclare + " " + matchFrom.Groups[2].Value;
                    }
                }
                mainBlockList.Add(variable);
                mainBlockList.Add("DO");
                mainBlockList.AddRange(perFormStmtList);
                mainBlockList.Add(strDeclare);
                mainBlockList.Add(strWhile);
                #endregion
            }
            else if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL")
                && currentStatement.Contains("THRU"))
            {
                #region Logic PERFORM UNTIL THRU
                string[] arrlastPerfrom = currentStatement.Split(new[] { "UNTIL" }, StringSplitOptions.None);
                if (arrlastPerfrom.Length > 1)
                {
                    strWhile = arrlastPerfrom[1].WhileConversion();
                    strWhile = "WHILE (" + strWhile + ")";
                    strWhile = strWhile.Replace(".", "");
                }
                var regexDeclare = @"(.*\s)";
                if (Regex.IsMatch(arrlastPerfrom[1], regexDeclare))
                {
                    var match = Regex.Match(arrlastPerfrom[1], regexDeclare);
                    strDeclare = match.Groups[1].Value;
                }
                string[] arrDecalare = strDeclare.Split(' ');
                var newArr = new List<string>();
                foreach (var arr in arrDecalare)
                {
                    if (string.IsNullOrEmpty(arr)) continue;
                    newArr.Add(arr);
                }
                var variableValue = cobolVariableList.ToList().Where(f => f.VariableName == newArr[0]).ToList();
                var firstOrDefault = variableValue.FirstOrDefault();
                if (firstOrDefault != null)
                {
                    var variableDefaultValue = firstOrDefault.DefaultValue;
                    variable = "SET " + strDeclare + " " + variableDefaultValue;
                }

                var perFromList = arrlastPerfrom[0].LogicOfPerfromThru(methodList, cobolVariableList);
                mainBlockList.Add(variable);
                mainBlockList.Add("DO");
                mainBlockList.AddRange(perFromList);
                mainBlockList.Add(strWhile);

                #endregion
            }
            else if (currentStatement.StartsWith("PERFORM ") && currentStatement.Contains("THRU "))
            {
                #region  Logic PERFORM THRU
                var perFromList = currentStatement.LogicOfPerfromThru(methodList, cobolVariableList);
                mainBlockList.AddRange(perFromList);
                #endregion
            }

            else if (currentStatement.StartsWith("PERFORM UNTIL"))
            {
                #region Logic PERFORM UNTIL
                /*
                int maxid = 0;
                string[] dwstr4 = currentStatement.Split(' ');
                string remainingItem = string.Empty;
                if (dwstr4.Length > 3)
                {
                    for (int i = 3; i < dwstr4.Length; i++)
                    {
                        remainingItem = remainingItem + dwstr4[i].Trim() + " ";
                    }
                    stmt1 = "Do While " + dwstr4[2] + " " + remainingItem + " is not equal to TRUE ";
                    stmt3 = "End do";
                    Console.WriteLine(stmt1);
                    Console.WriteLine(stmt3);
                }
                else
                {
                    stmt1 = "Do While " + dwstr4[2] + " is not equal to TRUE ";
                    stmt3 = "End do";
                    Console.WriteLine(stmt1);
                    Console.WriteLine(stmt3);
                }
                */
                #endregion
            }
            else if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL") &&
                     !currentStatement.Contains("VARYING") && !currentStatement.Contains("THRU") &&
                     !currentStatement.StartsWith("PERFORM UNTIL"))
            {
                #region Logic PERFORM AND UNTIL
                string[] arrlast = currentStatement.Split(new[] { "UNTIL" }, StringSplitOptions.None);
                string strPerform = arrlast[0].Trim();
                // strWhile = arrlast[1].Trim();
                var regexDeclare = @"(.*\s)";
                if (Regex.IsMatch(arrlast[1], regexDeclare))
                {
                    var match = Regex.Match(arrlast[1], regexDeclare);
                    strDeclare = match.Groups[1].Value;
                }
                string[] arrDecalare = strDeclare.Split(' ');
                var newArr = new List<string>();
                foreach (var arr in arrDecalare)
                {
                    if (string.IsNullOrEmpty(arr)) continue;
                    newArr.Add(arr);
                }
                string varName = string.IsNullOrEmpty((newArr.Count > 0).ToString()) ? newArr[0] : arrlast[1];
                var variableValue = cobolVariableList.ToList().Where(f => f.VariableName == varName).ToList();
                var firstOrDefault = variableValue.FirstOrDefault();
                if (firstOrDefault != null)
                {
                    var variableDefaultValue = firstOrDefault.DefaultValue;
                    var variableName = newArr[0] + " " + newArr[1];
                    strWhile = "WHILE (" + variableName + " " + variableDefaultValue + ")";
                }
                else
                {
                    strWhile = "WHILE (" + varName + ")";
                }
                mainBlockList.Add("DO");
                mainBlockList.Add(strPerform);
                mainBlockList.Add(strWhile);

                #endregion
            }
            else
            {
                mainBlockList.Add(currentStatement);
            }
            return mainBlockList;
        }

        public static List<string> LogicOfPerfromThru(this string text, List<string> methodList, List<CobolVariable> cobolVariableList)
        {
            var perFormStmtList = new List<string>();
            string[] arrlist = text.Split(new[] { "THRU" }, StringSplitOptions.None);
            if (!text.Contains("THRU ")) return perFormStmtList; // || !text.Contains("Thru ")
            string sLine = arrlist[0].Replace("PERFORM", "").Trim();
            string eLine = arrlist[1].Replace(".", "").Trim();
            int indexPosition = -1;
            foreach (var list in methodList)
            {
                indexPosition++;
                if (list != sLine) continue;
                for (int i = indexPosition; i < methodList.Count; i++)
                {
                    if (i >= methodList.Count) break;
                    string nLine = methodList[i].Trim();
                    nLine = nLine.Replace("PERFORM", "");
                    if (nLine == eLine)
                    {
                        perFormStmtList.Add("PERFORM " + nLine);
                        break;
                    }
                    perFormStmtList.Add("PERFORM " + nLine);
                }
            }
            return perFormStmtList;
        }

        /// <summary> Replace of operater for while conversion </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        public static string WhileConversion(this string text)
        {
            var currentLine = text;
            if (currentLine.Contains(">="))
            {
                currentLine = currentLine.Replace(">=", "<=");
            }
            else if (currentLine.Contains("<="))
            {
                currentLine = currentLine.Replace("<=", ">=");
            }
            else if (currentLine.Contains("<"))
            {
                currentLine = currentLine.Replace("<", ">");
            }
            else if (currentLine.Contains(">"))
            {
                currentLine = currentLine.Replace(">", "<");
            }
            else if (currentLine.Contains("="))
            {
                currentLine = currentLine.Replace("=", "<>");
            }
            else
            {
                currentLine = currentLine + " is equal to TRUE";
            }
            return currentLine;
        }

        /* --------------------- JCl ------------------------------*/

        ///<summary>Remove start character from string</summary>
        /// <para>Created by: Shubhangi Pawar</para>
        /// <returns></returns>

        public static List<string> RemoveStartCharacter(this List<string> allLines, params string[] characters)
        {
            var mainLines = new List<string>();
            foreach (var line in allLines)
            {
                var isPresent = characters.Any(x => line.StartsWith(x));
                if (isPresent)
                {
                    var currentLine = characters.Select(x => line.Replace(x, " "));
                    foreach (var s in currentLine) mainLines.Add(s);
                    continue;
                }
                mainLines.Add(line);
            }

            return mainLines;
        }

        /// <summary>  Combine all broken lines </summary>
        /// <param name="inputArray"></param>
        /// <param name="lineBreakElement"></param>
        /// <param name="keepOrRemove"></param>
        /// <returns></returns>
        public static List<string> CombineAllBrokenLines(this List<string> inputArray, char lineBreakElement,
            bool keepOrRemove = true)
        {
            if (inputArray.Count <= 0) return inputArray;
            var tempList = new List<string>();
            var indexPosition = -1;
            var tempString = string.Empty;
            for (var i = 0; i < inputArray.Count; i++)
            {
                indexPosition++;
                var regex = new Regex("[" + lineBreakElement + @"\s]$");
                if (regex.IsMatch(inputArray[i].TrimEnd()))
                {
                    if (inputArray[i].TrimStart().TrimEnd().StartsWith("'")) continue;
                    for (var j = i; j < inputArray.Count; j++)
                    {
                        if (regex.IsMatch(inputArray[j].TrimEnd()))
                        {
                            if (keepOrRemove)
                                tempString += inputArray[j].Remove(inputArray[j].Length - 1, 1).Trim() + " ";
                            else
                                tempString += inputArray[j].Trim() + " ";

                            indexPosition++;
                            continue;
                        }

                        tempString += inputArray[j].Trim();
                        tempList.Add(tempString);
                        tempString = string.Empty;
                        break;
                    }

                    i = indexPosition;
                }
                else
                    tempList.Add(inputArray[i]);
            }

            return tempList;
        }

        /// <summary> Statement Trim  </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> StatementTrim(this List<string> allLines)
        {
            return allLines.Select(line => line.Trim()).ToList();
        }

        /// <summary>Remove last commented part </summary>
        /// <param name="allLines"> </param>
        /// <return></return>
#pragma warning disable 1570
        /// <example><=== ABSTRACT TOBE CONVERTED"</example>
#pragma warning restore 1570
        public static List<string> RemoveLastCommentedPartForJcl(this List<string> allLines)
        {
            var mainLines = new List<string>();
            var regexCheck = new Regex(@"(<=)|(<==)|(<===)", RegexOptions.IgnoreCase);
            var regEx = new Regex(@"(^.*?(?=<=))|(^.*?(?=<==))|(^.*?(?=<===))", RegexOptions.IgnoreCase);
            foreach (var line in allLines)
            {
                if (!regexCheck.IsMatch(line))
                {
                    mainLines.Add(line);
                    continue;
                }
                Match match = regEx.Match(line);
                string newLine = match.Groups[1].Value;
                if (string.IsNullOrWhiteSpace(newLine)) continue;
                mainLines.Add(newLine);
            }
            return mainLines;
        }

        /// <summary>Remove last inline commented part </summary>
        /// <param name="allLines"> </param>
        /// <param name="patterns"> </param>
        /// <return></return>
#pragma warning disable 1570
        /// <example><=== ABSTRACT TOBE CONVERTED</example>
#pragma warning restore 1570
        /// <example>"//TAXSTMTS DD DSN=PRUCAT.ANNUAL.SUPPTS??,  ===> TAX STMT TOBE CONVERTED"  </example>
        /// <example>"//JESDS=ALL,DEST=ESF ** WRITER "  </example>

        public static List<string> RemoveInlineComment(this List<string> allLines, params string[] patterns)
        {
            if (allLines.Count <= 0) return allLines;
            var mainLines = new List<string>();
            foreach (var line in allLines)
            {
                var regex = patterns.Any(x => Regex.IsMatch(line, x, RegexOptions.IgnoreCase));
                if (!regex)
                {
                    mainLines.Add(line);
                    continue;
                }

                /*
                var matchRegex = patterns.FirstOrDefault(x => Regex.IsMatch(line, x, RegexOptions.IgnoreCase));
                if (matchRegex == null) continue;
                foreach (Match regx in matchRegex)
                {
                    var regEx = new Regex(regx.ToString(), RegexOptions.IgnoreCase);
                    if (regEx.IsMatch(line))
                    {
                        Match match = regEx.Match(line);
                        string newLine = match.Groups[1].Value;
                        if (string.IsNullOrWhiteSpace(newLine))
                            continue;
                        mainLines.Add(newLine);
                    }
                }
                */

                var matchRegex = patterns.FirstOrDefault(x => Regex.IsMatch(line, x, RegexOptions.IgnoreCase));
                if (matchRegex == null) continue;
                var regEx = new Regex(matchRegex, RegexOptions.IgnoreCase);
                if (!regEx.IsMatch(line)) continue;
                var match = regEx.Match(line);
                string newLine = match.Groups[1].Value;
                if (string.IsNullOrWhiteSpace(newLine)) continue;
                mainLines.Add(newLine);
            }
            return mainLines;
        }

        ///<summary></summary>
        /// <param name="allLines"> </param>
        /// <param name="fileName"> </param>
        /// <return></return>

        public static Dictionary<string, List<string>> CreateMethodForExec(this List<string> allLines, string fileName)
        {
            var dictionary = new Dictionary<string, List<string>>();
            int indexPosition = -1;
            var valueStatement = new List<string>();
            foreach (var line in allLines)
            {
                indexPosition++;
                if (line.Contains("EXEC") || line.Contains("EXEC "))
                {
                    var methodStatement = line.TempExec();
                    var newMethodNm = methodStatement[0];
                    var execStatemt = "EXEC " + methodStatement[1];
                    if (string.IsNullOrWhiteSpace(newMethodNm))
                    {
                        valueStatement.Add(execStatemt);
                        continue;
                    }
                    if (dictionary.Keys.Any(k => k == fileName))
                        fileName = fileName + " DefineMethod_" + indexPosition;
                    dictionary.Add(fileName, valueStatement);
                    fileName = newMethodNm;
                    valueStatement = new List<string> { execStatemt };
                    continue;
                }
                valueStatement.Add(line);
            }
            if (dictionary.Keys.Any(k => k == fileName)) fileName = fileName + " DefineMethod_" + indexPosition;

            dictionary.Add(fileName, valueStatement);
            return dictionary;
        }


        /// <summary> GET PROCEDURE DIVISION SECTION </summary>
        ///  <para> Created by: Shubhangi Pawar </para>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<CobolVariable> GetWorkingStorageParentItem(this List<CobolVariable> workingStorageSection)
        {
            workingStorageSection.ForEach(w => { w.GraphId = "GraphId-" + w.VariableId; });
            int indexPosition = -1;
            foreach (var cobolVariable in workingStorageSection)
            {
                indexPosition++;
                if (int.Parse(cobolVariable.VariableLevel) == 1)
                {
                    cobolVariable.ParentId = "-1";
                    continue;
                }
                var itsLevel = int.Parse(cobolVariable.VariableLevel);
                for (int idx = indexPosition; idx >= 0; idx--)
                {
                    var element = workingStorageSection[idx];
                    if (itsLevel == int.Parse(element.VariableLevel)) continue;

                    if (int.Parse(element.VariableLevel) >= itsLevel) continue;

                    cobolVariable.ParentId = element.GraphId;
                    break;
                }
            }

            return workingStorageSection;
        }
        private static string[] TempExec(this string execStatement)
        {
            string[] toStr = { "EXEC", "EXEC " };
            var execSplit = execStatement.Split(toStr, StringSplitOptions.None);
            return execSplit;
        }
    }
}
