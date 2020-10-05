using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
namespace BusinessLayer.CobolVersion
{
    public static class CobolProcessing
    {
        public static List<string> GetAllDataBetweenProcedureDivision(this List<string> allLines) //  
        {
            var mainMethodBlock = new List<string>();
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;
                var regexPattern = "(.*PROCEDURE DIVISION.*)";
                if (!Regex.IsMatch(line, regexPattern)) continue;
                for (int i = indexPosition; i < allLines.Count; i++)
                {
                    var newLine = allLines[i];
                    mainMethodBlock.Add(newLine);
                }
                break;
            }
            return mainMethodBlock;
        }

        public static List<string> GetAllMethods(this List<string> allLines, string expression)
        {
            var allMethods = new List<string>();
            #region commented region
            /*
            expression = expression + " ";
            var regexWithDot = expression + "(.*\\.)";
            var regexWithoutDot = expression + "(\\S+)"; // PERFORM (\S+)
            foreach (var line in allLines)
            {
                if (!line.Contains(expression)) continue;
                string usePattern = string.Empty;
                if (Regex.IsMatch(line, regexWithDot))
                    usePattern = regexWithDot;
                if (Regex.IsMatch(line, regexWithoutDot) && string.IsNullOrEmpty(usePattern))
                    usePattern = regexWithoutDot;
                var match = Regex.Match(line, usePattern);
                if (match.Groups.Count <= 1) continue;
                var methodName = match.Groups[1].Value;
                allMethods.Add(methodName);
            }
             */
            /*
            allMethods.AddRange(from line in allLines
                                where !string.IsNullOrEmpty(line)
                                where line.Length > 7
                                let eighthChar = line[7]
                                where !char.IsWhiteSpace(eighthChar) && eighthChar != '*'
                                select line.Substring(7)
                                    into newLine
                                    select Regex.Match(newLine, "^(\\S+)")
                                        into match
                                        where match.Groups.Count > 1
                                        select match.Groups[1].Value
                                            into methodName
                                            where methodName.EndsWith(".")
                                            select methodName);
             */
            #endregion

            foreach (var line in allLines)
            {
                if (line.Length <= 6) continue;
                char firstChar = line[1];
                if (char.IsWhiteSpace(firstChar) || firstChar == '*') continue;
                // if (line.Contains("++INCLUDE ")) continue;
                //string[] newLine = line.Split('.');
                //var nLine = newLine[0].Trim();
                allMethods.Add(line);
            }
            allMethods = allMethods.Distinct().ToList();
            return allMethods;
            // string newLine = line.Substring(7);
            // var match = Regex.Match(newLine, "^(\\S+)");
            // if (match.Groups.Count <= 1) continue;
            // var methodName = match.Groups[1].Value;
            // if (!methodName.EndsWith(".")) continue;
        }

        /// <summary>
        /// Collected all method lines into dictionary 
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="methodNameList"></param>
        /// <returns></returns>
        public static Dictionary<string, List<string>> CollectAllMethodsData(this List<string> allLines, List<string> methodNameList)
        {
            var dictionary = new Dictionary<string, List<string>>();
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
                        dictionary.Add(methodName, methodsLines);
                    }
                    if (methodNameList.Any(x => thisLine == x)) //|| i == allLines.Count - 1
                    {
                        if (i == allLines.Count - 1) methodsLines.Add(thisLine);
                        if (dictionary.Keys.Any(k => k == methodName))
                            methodName = methodName + " DefineMethod_" + i;
                        dictionary.Add(methodName, methodsLines);
                        break;
                    }

                    methodsLines.Add(thisLine);
                }
            }
            return dictionary;
        }

        public static List<string> ModifyAllLinesMethodsName(this List<string> allLines, List<string> methodNameList)
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

        public static List<string> RemoveDotFromMethodName(this List<string> methodList)
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

        //public static List<string> PerfromVarying(this List<string> allLines, List<string> methodNameList, List<CobolVariable> cobolVariableList)
        //{
        //    var mainMethodList = new List<string>();
        //    if (allLines.Count == 0) return mainMethodList;
        //    foreach (var line in allLines)
        //    {
        //        if (!line.StartsWith("PERFORM "))
        //        {
        //            mainMethodList.Add(line);
        //            continue;
        //        }
        //        var mainBlock = line.ConversionDoWhile(methodNameList, cobolVariableList);
        //        mainMethodList.AddRange(mainBlock);
        //    }
        //    return mainMethodList;
        //}

        //public static List<string> ConversionDoWhile(this string text, List<string> methodNameList, List<CobolVariable> cobolVariableList)
        //{
        //    var mainBlockList = new List<string>();
        //    var perFormStmtList = new List<string>();
        //    var methodList = methodNameList.Select(list => list.Replace(".", "")).ToList();
        //    // var currentStatement = "PERFORM S2500-LOWV-DET-FIELDS VARYING EDIT-SEQ  FROM 1 BY 1  UNTIL EDIT-SEQ > 14.";
        //    // var currentStatement = "PERFORM  A-START-PROCESSING THRU B-MAIN-PROCESSING.";
        //    // var currentStatement = "PERFORM  B-MAIN-PROCESSING THRU B-MAIN-EXIT UNTIL EOF-FLAG  =  'Y'.";
        //    // var currentStatement = "PERFORM  B-MAIN-PROCESSING THRU B-MAIN-EXIT UNTIL EOF-FLAG  =  'Y'.";
        //    var currentStatement = text;
        //    string strPerform = string.Empty;
        //    string strDeclare = string.Empty;
        //    string strWhile = string.Empty;
        //    string variable = string.Empty;

        //    #region  logic PERFORM UNTIL VARYING

        //    if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL") &&
        //        currentStatement.Contains("VARYING"))
        //    {
        //        string[] arrlast = currentStatement.Split(new string[] { "UNTIL" }, StringSplitOptions.None);
        //        if (arrlast.Length > 1)
        //        {
        //            strWhile = arrlast[1].WhileConversion();
        //            strWhile = "WHILE (" + strWhile + ")";
        //            strWhile = strWhile.Replace(".", "");
        //        }
        //        var newLine = arrlast[0];
        //        var regexSet = @"VARYING(\s+[A-z0-9\-]+\s+)FROM ";
        //        if (Regex.IsMatch(newLine, regexSet))
        //        {
        //            var match = Regex.Match(newLine, regexSet);
        //            strDeclare = match.Groups[1].Value;
        //            variable = "SET " + strDeclare;
        //        }
        //        var regexPerfrom = @"(PERFORM.*)VARYING";
        //        if (Regex.IsMatch(newLine, regexPerfrom))
        //        {
        //            var match = Regex.Match(newLine, regexPerfrom);
        //            strPerform = match.Groups[1].Value;
        //            perFormStmtList = strPerform.LogicOfPerfromThru(methodList, cobolVariableList);
        //            if (perFormStmtList.Count == 0)
        //                perFormStmtList.Add(strPerform);

        //        }
        //        var regexFrom = @"(FROM.*)UNTIL";
        //        if (Regex.IsMatch(currentStatement, regexFrom))
        //        {
        //            var match = Regex.Match(currentStatement, regexFrom);
        //            string strFrom = match.Groups[1].Value;
        //            var regexFm = @"FROM\s+(.*)\s+BY\s+(.*)";
        //            if (Regex.IsMatch(strFrom, regexFm))
        //            {
        //                var matchFrom = Regex.Match(strFrom, regexFm);
        //                variable += "= " + matchFrom.Groups[1].Value;
        //                strDeclare = strDeclare + "=" + strDeclare + " " + matchFrom.Groups[2].Value;
        //            }
        //        }
        //        mainBlockList.Add(variable);
        //        mainBlockList.Add("DO");
        //        mainBlockList.AddRange(perFormStmtList);
        //        mainBlockList.Add(strDeclare);
        //        mainBlockList.Add(strWhile);
        //    }
        //    #endregion

        //    #region Logic PERFORM UNTIL THRU
        //    else if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL")
        //        && currentStatement.Contains("THRU"))
        //    {
        //        string[] arrlastPerfrom = currentStatement.Split(new string[] { "UNTIL" }, StringSplitOptions.None);
        //        if (arrlastPerfrom.Length > 1)
        //        {
        //            strWhile = arrlastPerfrom[1].WhileConversion();
        //            strWhile = "WHILE (" + strWhile + ")";
        //            strWhile = strWhile.Replace(".", "");
        //        }
        //        var regexDeclare = @"(.*\s)";
        //        if (Regex.IsMatch(arrlastPerfrom[1], regexDeclare))
        //        {
        //            var match = Regex.Match(arrlastPerfrom[1], regexDeclare);
        //            strDeclare = match.Groups[1].Value;
        //        }
        //        string[] arrDecalare = strDeclare.Split(' ');
        //        var newArr = new List<string>();
        //        foreach (var arr in arrDecalare)
        //        {
        //            if (string.IsNullOrEmpty(arr)) continue;
        //            newArr.Add(arr);
        //        }
        //        var variableValue = cobolVariableList.ToList().Where(f => f.VariableName == newArr[0]).ToList();
        //        var firstOrDefault = variableValue.FirstOrDefault();
        //        if (firstOrDefault != null)
        //        {
        //            var variableDefaultValue = firstOrDefault.DefaultValue;
        //            variable = "SET " + strDeclare + " " + variableDefaultValue;
        //        }

        //        var perFromList = arrlastPerfrom[0].LogicOfPerfromThru(methodList, cobolVariableList);
        //        mainBlockList.Add(variable);
        //        mainBlockList.Add("DO");
        //        mainBlockList.AddRange(perFromList);
        //        mainBlockList.Add(strWhile);
        //    }

        //    #endregion

        //    #region  Logic PERFORM THRU
        //    else if (currentStatement.StartsWith("PERFORM ") && currentStatement.Contains("THRU "))
        //    {
        //        var perFromList = currentStatement.LogicOfPerfromThru(methodList, cobolVariableList);
        //        mainBlockList.AddRange(perFromList);
        //    }
        //    #endregion

        //    #region Logic PERFORM UNTIL
        //    /*
        //    else if (currentStatement.StartsWith("PERFORM UNTIL"))
        //    {
        //        try
        //        {

        //            int maxid = 0;
        //            string[] dwstr4 = currentStatement.Split(' ');
        //            string RemainingItem = string.Empty;
        //            if (dwstr4.Length > 3)
        //            {
        //                for (int i = 3; i < dwstr4.Length; i++)
        //                {
        //                    RemainingItem = RemainingItem + dwstr4[i].Trim() + " ";
        //                }
        //                stmt1 = "Do While " + dwstr4[2] + " " + RemainingItem + " is not equal to TRUE ";
        //                stmt3 = "End do";
        //                Console.WriteLine(stmt1);
        //                Console.WriteLine(stmt3);
        //            }
        //            else
        //            {
        //                stmt1 = "Do While " + dwstr4[2] + " is not equal to TRUE ";
        //                stmt3 = "End do";
        //                Console.WriteLine(stmt1);
        //                Console.WriteLine(stmt3);
        //            }
        //        }
        //        catch (Exception)
        //        {
        //        }

        //    }
        //         */
        //    #endregion

        //    #region Logic PERFORM AND UNTIL

        //    else if (currentStatement.StartsWith("PERFORM") && currentStatement.Contains("UNTIL") &&
        //             !currentStatement.Contains("VARYING") && !currentStatement.Contains("THRU") &&
        //             !currentStatement.StartsWith("PERFORM UNTIL"))
        //    {
        //        string[] arrlast = currentStatement.Split(new string[] { "UNTIL" }, StringSplitOptions.None);
        //        strPerform = arrlast[0].Trim();
        //        strWhile = arrlast[1].Trim();
        //        var regexDeclare = @"(.*\s)";
        //        if (Regex.IsMatch(arrlast[1], regexDeclare))
        //        {
        //            var match = Regex.Match(arrlast[1], regexDeclare);
        //            strDeclare = match.Groups[1].Value;
        //        }
        //        string[] arrDecalare = strDeclare.Split(' ');
        //        var newArr = new List<string>();
        //        foreach (var arr in arrDecalare)
        //        {
        //            if (string.IsNullOrEmpty(arr)) continue;
        //            newArr.Add(arr);
        //        }
        //        string varName;
        //        varName = string.IsNullOrEmpty((newArr.Count > 0).ToString()) ? newArr[0] : arrlast[1];

        //        var variableValue = cobolVariableList.ToList().Where(f => f.VariableName == varName).ToList();
        //        var firstOrDefault = variableValue.FirstOrDefault();
        //        if (firstOrDefault != null)
        //        {
        //            var variableDefaultValue = firstOrDefault.DefaultValue;
        //            var variableName = newArr[0] + " " + newArr[1];
        //            strWhile = "WHILE (" + variableName + " " + variableDefaultValue + ")";
        //        }
        //        else
        //        {
        //            strWhile = "WHILE (" + varName + ")";
        //        }
        //        mainBlockList.Add("DO");
        //        mainBlockList.Add(strPerform);
        //        mainBlockList.Add(strWhile);
        //    }

        //    #endregion

        //    #region  Only for PERFORM

        //    else
        //    {
        //        mainBlockList.Add(currentStatement);
        //    }

        //    #endregion
        //    return mainBlockList;
        //}

        //public static string WhileConversion(this string text)
        //{
        //    var currentLine = text;
        //    if (currentLine.Contains(">="))
        //    {
        //        currentLine = currentLine.Replace(">=", "<=");
        //    }
        //    else if (currentLine.Contains("<="))
        //    {
        //        currentLine = currentLine.Replace("<=", ">=");
        //    }
        //    else if (currentLine.Contains("<"))
        //    {
        //        currentLine = currentLine.Replace("<", ">");
        //    }
        //    else if (currentLine.Contains(">"))
        //    {
        //        currentLine = currentLine.Replace(">", "<");
        //    }
        //    else if (currentLine.Contains("="))
        //    {
        //        currentLine = currentLine.Replace("=", "<>");
        //    }
        //    else
        //    {
        //        currentLine = currentLine + " is equal to TRUE";
        //    }
        //    return currentLine;
        //}

        //public static List<string> LogicOfPerfromThru(this string text, List<string> methodList, List<CobolVariable> cobolVariableList)
        //{
        //    var perFormStmtList = new List<string>();
        //    string[] arrlist = text.Split(new string[] { "THRU" }, StringSplitOptions.None);
        //    if (!text.Contains("THRU ")) return perFormStmtList; // || !text.Contains("Thru ")
        //    string sLine = arrlist[0].Replace("PERFORM", "").Trim();
        //    string eLine = arrlist[1].Replace(".", "").Trim();
        //    int indexPosition = -1;
        //    foreach (var list in methodList)
        //    {
        //        indexPosition++;
        //        if (list != sLine) continue;
        //        // perFormStmtList.Add("PERFORM " + list);
        //        // for (int i = indexPosition - 1; i < methodList.Count; i++)
        //        for (int i = indexPosition; i < methodList.Count; i++)
        //        {
        //            if (i >= methodList.Count) break;
        //            string nLine = methodList[i].Trim();
        //            nLine = nLine.Replace("PERFORM", "");
        //            if (nLine == eLine)
        //            {
        //                perFormStmtList.Add("PERFORM " + nLine);
        //                break;
        //            }
        //            perFormStmtList.Add("PERFORM " + nLine);
        //        }
        //    }
        //    return perFormStmtList;
        //}

        public static List<string> GetAllDataBetweenWorkingStorageSection(this List<string> allLines)
        {
            var mainMethodBlock = new List<string>();
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;
                var newLine = line;
                var regexPattern = @"(.*WORKING-STORAGE SECTION.*)";
                if (!Regex.IsMatch(newLine, regexPattern)) continue;
                for (int i = indexPosition; i < allLines.Count; i++)
                {
                    var currentLine = allLines[i];
                    if (currentLine.StartsWith("*")) continue;
                    var regexPatternProcedure = "(.*PROCEDURE DIVISION.*)";
                    if (Regex.IsMatch(line, regexPatternProcedure)) break;
                    mainMethodBlock.Add(currentLine);
                }
            }
            return mainMethodBlock;
        }

        /// <summary>
        /// Combine all lines for working storage sections.
        /// </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> CombineAllLinesOfWorkingStorageSection(this List<string> allLines)
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
                tempString += currentLine;
            }
            return mainMethodBlock;
        }

        public static List<string> CombineAllExecSqlStatements(this List<string> allLines)
        {
            var mainBlockList = new List<string>();
            if(allLines.Count <= 0) return mainBlockList;
            for (int i = 0; i < allLines.Count; i++)
            {
                string tempString = string.Empty;
                var currentLine = allLines[i].Trim();
                if (currentLine != "EXEC SQL" && !currentLine.StartsWith("EXEC CICS"))
                {
                    mainBlockList.Add(currentLine);
                    continue;
                }
                for (int j = i; j < allLines.Count; j++, ++i)
                {
                    var cLine = allLines[j].Trim();
                    if (cLine.Contains("END-EXEC"))
                    {
                        tempString += " " + cLine.TrimStart();
                        mainBlockList.Add(tempString.TrimStart());
                        break;
                    }
                    tempString += " " + cLine.TrimStart();
                }

                /*
                for (int i = cnt + 1; i < allLines.Count; i++)
                {
                    if (currentLine.StartsWith("EXEC CICS"))
                    {
                        var regexExec = @"(EXEC CICS)\s+(.*)";
                        if (Regex.IsMatch(currentLine, regexExec))
                        {
                            var matches = Regex.Match(currentLine, regexExec);
                            // currentLine = matches.Groups[1].Value;
                            tempString = currentLine; //matches.Groups[2].Value;
                        }
                    }
                    // var newLine = allLines[i];
                    if (currentLine.Contains("END-EXEC"))
                    {
                        tempString += " " + currentLine.TrimStart();
                        mainBlockList.Add(tempString);
                        // mainBlockList.Add(newLine);
                        cnt = i;
                        break;
                        // continue;
                    }
                    // mainBlockList.Add(currentLine);
                  
                }*/
            }
            return mainBlockList;
        }
        
        ///<summary>split fd section </summary>
        public static string SplitFdLevel(this string line)
        {
            var currentLine = line;
            const string regexPatternLevel = @"^FD\s+([A-z0-9\-]+)";
            var level = string.Empty;
            if (!Regex.IsMatch(currentLine, regexPatternLevel)) return level;
            var match = Regex.Match(currentLine, regexPatternLevel);
            level = match.Groups[1].Value;
            return level;
        }

        /// <summary>
        /// split level of variable 
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        public static string SplitLevel(this string line)
        {
            var currentLine = line;
            var regexPatternLevel = @"^([0-9]{2})";
            string level = string.Empty;
            if (Regex.IsMatch(currentLine, regexPatternLevel))
            {
                var match = Regex.Match(currentLine, regexPatternLevel);
                level = match.Groups[1].Value;
                return level;
            }
            return level;
        }

        public static string SplitVaiableName(this string line)
        {
            var currentLine = line.TrimStart();
            var regexPatternVariable = @"(^[^\s]*)";
            string varibleName = string.Empty;
            if (Regex.IsMatch(currentLine, regexPatternVariable))
            {
                var match = Regex.Match(currentLine, regexPatternVariable);
                varibleName = match.Groups[1].Value;
                return varibleName;
            }
            return varibleName;
        }

        public static string SplitDataTypeField(this string line)
        {
            var currentLine = line;
            string strDataType = string.Empty;
            var regexPatternDType = @"PIC\s([A-z0-9\-()]+)";
            if (!Regex.IsMatch(currentLine, regexPatternDType)) return strDataType;
            var match = Regex.Match(currentLine, regexPatternDType);
            strDataType = match.Groups[1].Value;
            return strDataType;
        }
        public static string SplitDataType(this string line)
        {
            var currentLine = line;
            string dataType = string.Empty;
            var regexPatternLenght = @"(^[^(]+|(?<=().+?(?=))|[^)]+$)";
            if (!Regex.IsMatch(currentLine, regexPatternLenght)) return dataType;
            var matchLenght = Regex.Match(currentLine, regexPatternLenght);
            dataType = matchLenght.Groups[1].Value;
            return dataType;
        }


        public static string SplitLenght(this string line)
        {
            var currentLine = line;
            string lenght = string.Empty;
            var regexPatternLenght = @"(\(\d+\))";
            if (!Regex.IsMatch(currentLine, regexPatternLenght)) return lenght;
            var matchLenght = Regex.Match(currentLine, regexPatternLenght);
            lenght = matchLenght.Groups[1].Value;
            return lenght;
        }

        public static string SplitDefaultValue(this string line)
        {
            var currentLine = line;
            var defaultValue = string.Empty;
            var regexPatternDefaultValues = "(VALUES(.*))";
            var regexPatternDefaultValue = "(VALUE(.*))";
            if (Regex.IsMatch(currentLine, regexPatternDefaultValues))
            {
                var match = Regex.Match(currentLine, regexPatternDefaultValues);
                defaultValue = match.Groups[2].Value;
                return defaultValue;
            }
            if (Regex.IsMatch(currentLine, regexPatternDefaultValue))
            {
                var match = Regex.Match(currentLine, regexPatternDefaultValue);
                defaultValue = match.Groups[2].Value;
                return defaultValue;
            }
            return defaultValue;
        }

        public static string SplitPictureClause(this string line)
        {
            var currentLine = line;
            var computationOrBinary = string.Empty;
            var regexPatternPictureClause = @"(COMP\s+PIC)";
            var regexpatternComputation = @"(PIC)";
            if (Regex.IsMatch(currentLine, regexPatternPictureClause))
            {
                var match = Regex.Match(currentLine, regexPatternPictureClause);
                computationOrBinary = match.Groups[0].Value;
                return computationOrBinary;
            }
            if (Regex.IsMatch(currentLine, regexpatternComputation))
            {
                var match = Regex.Match(currentLine, regexpatternComputation);
                computationOrBinary = match.Groups[0].Value;
                return computationOrBinary;
            }
            return computationOrBinary;
        }

        public static string SplitComputationBinary(this string line)
        {
            var currentLine = line;
            var computationOrBinary = string.Empty;
            var regexPatternComputationOrBinary = @"(comp\-[0-9\s])";
            if (!Regex.IsMatch(currentLine, regexPatternComputationOrBinary)) return computationOrBinary;
            var match = Regex.Match(currentLine, regexPatternComputationOrBinary);
            computationOrBinary = match.Groups[0].Value;
            return computationOrBinary;
        }

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
        /*
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
        */
        public static List<string> ConversionOfExecStatements(this List<string> allLines)
        {
            var mainBlock = new List<string>();
            if (allLines.Count <= 0) return mainBlock;
            // var regex = @"^EXEC CICS RETURN|^EXEC CICS XCTL";
            foreach (string currentLine in allLines)
            {

                if (currentLine.StartsWith("EXEC CICS HANDLE AID"))
                // if(Regex.IsMatch(currentLine, regex))
                {
                    string[] toStr = { "AID" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1];
                    var finalStatement = moveState.Split(' ');
                    foreach (var statement in finalStatement)
                    {
                        if (statement == "END-EXEC." || statement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(statement)) continue;
                        string[] state = statement.Split('(');
                        var mainLine = "IF " + state[0] + " IS PRESSED";
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall);
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }
               
                if (currentLine.StartsWith("EXEC CICS HANDLE CONDITION"))
                {
                    string[] toStr = { "CONDITION" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1].Trim();
                    var finalStatement = moveState.Split(')');
                    int finalStatCount = finalStatement.Length - 1;
                    int index = 0;
                    foreach (var statement in finalStatement)
                    {
                        var newStatement = statement.Trim();
                        index++;
                        if (newStatement == "END-EXEC." || newStatement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(newStatement)) continue;
                        var ifState = index == 1 ? "IF " : "ELSE-IF ";
                        if (finalStatCount > 1)
                        {
                            if (index == finalStatCount)
                            {
                                ifState = "ELSE ";
                            }
                        }
                        string[] state = newStatement.Split('(');
                        var mainLine = ifState + "CONDITION IS " + state[0];
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall.Trim());
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }

                //if (Regex.IsMatch(currentLine, regex))
                //{
                //    var regexPattern = @"EXEC CICS\\s+[A-z0-9\\-]\\S+(\\s+[A-z0-9\\-]\\S+)";
                //    var matches = Regex.Match(currentLine, regexPattern, RegexOptions.IgnoreCase);
                //    var statementMatch = matches.Groups[1].Value;
                //    Console.WriteLine(statementMatch);

                //}
                mainBlock.Add(currentLine);

            }
            return mainBlock;
        }

        public static List<string> ConvertionOfCrudActivity(this List<string> allLines)
        {
            var mainBlock = new List<string>();
            if (allLines.Count <= 0) return mainBlock;
            foreach (var line in allLines)
            {
                var regexSqlselect = @"EXEC SQL(\s+[A-z0-9\-]+\s+)";
                var regexCicsStatement = @"EXEC CICS(\s+[A-z0-9\-]+\s+)";
                var currentLine = line.Trim();
                if (Regex.IsMatch(line, regexSqlselect))
                {
                    var matches = Regex.Match(currentLine, regexSqlselect);
                    var pgmName = matches.Groups[1].Value.Trim();
                    if (pgmName == "SELECT" || pgmName == "UPDATE")
                    {
                        var newLine = currentLine.Replace("EXEC SQL", "").Replace("END-EXEC.", "").Replace("END-EXEC", "").Trim();
                        mainBlock.Add(newLine);
                        continue;
                    }
                }
                if (Regex.IsMatch(currentLine, regexCicsStatement))
                {
                    var matches = Regex.Match(currentLine, regexCicsStatement);
                    var actionStatement = matches.Groups[1].Value.Trim();
                    // string nLine = string.Empty;
                    if (actionStatement == "READ")
                    {
                        var newReadLine = ConversionReadStatement(line);
                        mainBlock.Add(newReadLine);
                        continue;
                    }
                    if (actionStatement == "SEND")
                    {
                        var newSendLine = ConversionSendStatement(line);
                        mainBlock.Add(newSendLine);
                        continue;
                    }
                    if (actionStatement == "RECEIVE")
                    {
                        var newReceiveLine = ConversionReceiveStatement(line);
                        mainBlock.Add(newReceiveLine);
                        continue;
                    }
                    if (actionStatement == "HANDLE")
                    {
                        var newList = ConverstionOfHandleStatement(line);
                        mainBlock.AddRange(newList);
                        continue;
                    }
                    if (actionStatement == "XCTL" || actionStatement == "LINK")
                    {
                        var newProgramLine = ConversionProgramStatement(line);
                        mainBlock.Add(newProgramLine);
                        continue;
                    }
                    var cLine =
                        line.Replace("EXEC CICS", "")
                            .Replace("EXEC CICS ", "")
                            .Replace("END-EXEC.", "")
                            .Replace("END-EXEC", "");
                    mainBlock.Add(cLine);
                    continue;
                }
                mainBlock.Add(line);
            }
            return mainBlock;
        }

        public static List<string> ConverstionOfHandleStatement(string currentStatement)
        {
            var mainBlock = new List<string>();
            if (currentStatement.StartsWith("EXEC CICS HANDLE AID"))
            {
                string[] toStr = { "AID" };
                string[] moveStatement = currentStatement.Split(toStr, StringSplitOptions.None);
                var moveState = moveStatement[1];
                var finalStatement = moveState.Split(' ');
                foreach (var statement in finalStatement)
                {
                    if (statement == "END-EXEC." || statement == "END-EXEC") continue;
                    if (string.IsNullOrWhiteSpace(statement)) continue;
                    string[] state = statement.Split('(');
                    var mainLine = "IF " + state[0] + " IS PRESSED";
                    mainBlock.Add(mainLine);
                    var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                    mainBlock.Add(internalcall);
                    mainBlock.Add("END-IF");
                }
            }
            if (currentStatement.StartsWith("EXEC CICS HANDLE CONDITION"))
            {
                string[] toStr = { "CONDITION" };
                string[] moveStatement = currentStatement.Split(toStr, StringSplitOptions.None);
                var moveState = moveStatement[1].Trim();
                var finalStatement = moveState.Split(')');
                int finalStatCount = finalStatement.Length - 1;
                int index = 0;
                foreach (var statement in finalStatement)
                {
                    var newStatement = statement.Trim();
                    index++;
                    if (newStatement == "END-EXEC." || newStatement == "END-EXEC") continue;
                    if (string.IsNullOrWhiteSpace(newStatement)) continue;
                    var ifState = index == 1 ? "IF " : "ELSE-IF ";
                    if (finalStatCount > 1)
                    {
                        if (index == finalStatCount)
                        {
                            ifState = "ELSE ";
                        }
                    }
                    string[] state = newStatement.Split('(');
                    var mainLine = ifState + "CONDITION IS " + state[0];
                    mainBlock.Add(mainLine);
                    var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                    mainBlock.Add(internalcall.Trim());
                    mainBlock.Add("END-IF");
                }
            }
            return mainBlock;
        }

        public static string ConversionReceiveStatement(string currentStatement)
        {
            string nLine = "RECEIVE USER SCREEN ";
            string[] toStr = { "MAP(" };
            string[] mapStatement = currentStatement.Split(toStr, StringSplitOptions.None);
            var mapState = mapStatement[1];
            var finalMapStatement = mapState.Split(' ');
            int index = 0;
            foreach (var mStatement in finalMapStatement)
            {
                index++;
                if (mStatement == "END-EXEC." || mStatement == "END-EXEC") continue;
                if (index == 1)
                {
                    nLine += mStatement.Replace("(", "").Replace(")", "").Trim();
                    nLine += " USING MAP DATASET ";
                }
                else
                {
                    if (!mStatement.StartsWith("MAPSET(") && !mStatement.StartsWith("INTO(")) continue;
                    if (mStatement.StartsWith("MAPSET("))
                    {
                        string[] tostr = { "MAPSET(" };
                        string[] mapSetStatement = mStatement.Split(tostr, StringSplitOptions.None);
                        nLine += mapSetStatement[1].Replace("(", "").Replace(")", "").Trim();
                    }
                    if (!mStatement.StartsWith("INTO(")) continue;
                    {
                        nLine += " INTO ";
                        string[] tostr = { "INTO(" };
                        string[] mapSetStatement = mStatement.Split(tostr, StringSplitOptions.None);
                        nLine += mapSetStatement[1].Replace("(", " ").Replace(")", " ").Trim();
                    }
                }
            }
            return nLine;
        }

        public static string ConversionSendStatement(string currentStatement)
        {
            string nLine = "SEND TO USER SCREEN ";
            string[] toStr = { "MAP(" };
            string[] mapStatement = currentStatement.Split(toStr, StringSplitOptions.None);
            var mapState = mapStatement[1];
            var finalMapStatement = mapState.Split(' ');
            int index = 0;
            foreach (var mStatement in finalMapStatement)
            {
                index++;
                if (mStatement == "END-EXEC." || mStatement == "END-EXEC") continue;
                if (index == 1)
                {
                    nLine += mStatement.Replace("(", "").Replace(")", "").Trim();
                    nLine += " USING MAP DATASET ";
                }
                else
                {
                    if (!mStatement.StartsWith("MAPSET(") && !mStatement.StartsWith("FROM(")) continue;
                    if (mStatement.StartsWith("MAPSET("))
                    {
                        string[] tostr = { "MAPSET(" };
                        var mapSetStatement = mStatement.Split(tostr, StringSplitOptions.None);
                        if (mapSetStatement.Length <= 1) return nLine;
                        nLine += mapSetStatement[1].Replace("(", "").Replace(")", "").Trim();
                    }
                    if (!mStatement.StartsWith("FROM(")) continue;
                    {

                        string[] tostr = { "FROM(" };
                        var mapSetStatement = mStatement.Split(tostr, StringSplitOptions.None);
                        if (mapSetStatement.Length <= 1) return nLine;
                        nLine += " FROM ";
                        nLine += mapSetStatement[1].Replace("(", "").Replace(")", "").Trim();
                    }
                }
            }
            return nLine;
        }

        public static string ConversionProgramStatement(string currentStatement)
        {
            string nLine = "CALL ";
            string[] toStr = { "PROGRAM(" };
            string[] mapStatement = currentStatement.Split(toStr, StringSplitOptions.None);
            var mapState = mapStatement[1];
            var finalMapStatement = mapState.Split(' ');
            int index = 0;
            foreach (var mStatement in finalMapStatement)
            {
                index++;
                if (mStatement == "END-EXEC." || mStatement == "END-EXEC") continue;
                if (index == 1)
                {
                    nLine += mStatement.Replace("(", "").Replace(")", "").Trim();
                    nLine += " PASSING ";
                }
                else
                {
                    if (!mStatement.StartsWith("COMMAREA(")) continue;
                    string[] tostr = { "COMMAREA(" };
                    string[] mapSetStatement = mStatement.Split(tostr, StringSplitOptions.None);
                    nLine += mapSetStatement[1].Replace("(", "").Replace(")", "").Trim();
                }
            }
            return nLine;
        }

        public static string ConversionReadStatement(string currentStatement)
        {
            string nLine = "READ FROM ";
            string[] toStr = { "EXEC CICS READ" };
            string[] mapStatement = currentStatement.Split(toStr, StringSplitOptions.None);
            var mapState = mapStatement[1].Split(')');
            var finalMapStatement = mapState; // .Split(' ');
            string intoState = string.Empty;
            string fieldState = string.Empty;
            string dataSet = string.Empty;
            foreach (var mStatement in finalMapStatement)
            {
                var cLine = mStatement.Trim();
                if (cLine == "END-EXEC." || cLine == "END-EXEC") continue;
                if (cLine.StartsWith("DATASET"))
                {
                    dataSet = cLine.Replace("(", " ").Replace(")", " ").Replace("'", "").Trim();
                }
                if (cLine.StartsWith("RIDFLD"))
                {
                    string[] toStrField = { "RIDFLD" };
                    string[] fieldStatement = cLine.Split(toStrField, StringSplitOptions.None);
                    fieldState = fieldStatement[1].Replace("(", "").Replace(")", "").Replace("'", "").Trim();
                }
                if (cLine.StartsWith("INTO"))
                {
                    var intoStatement = cLine.Split(' ');
                    foreach (var iStatement in intoStatement)
                    {
                        if (string.IsNullOrWhiteSpace(iStatement)) continue;
                        intoState += iStatement.Replace("(", "").Replace(")", "").Replace("'", "").Trim() + " ";
                    }
                }
            }
            nLine += dataSet + " " + intoState + "USING KEY " + fieldState;
            return nLine;
        }

        public static string ConversionPesudoCode(this string currentLine)
        {
            var newLine = string.Empty;
            if (currentLine.StartsWith("ADD "))
                return currentLine.PseudoCodeForAddStatement();
            if (currentLine.StartsWith("MOVE "))
                return currentLine.PseudoCodeForMoveStatement();
            if (currentLine.StartsWith("IF "))
                // TODO: This is temporary solution for IF Statements conversion. Need to do with Regex pattern
                return currentLine.Replace("IF ", "IS ") + " ?";
            if (!currentLine.StartsWith("COMPUTE ")) return newLine;

            return currentLine.PseudoCodeforComputeStatement();
        }

        private static string PseudoCodeforComputeStatement(this string cLine)
        {
            string mainLine = string.Empty;
            var regexPattern = new Regex(@"COMPUTE ([a-zA-Z0-9\-\s\.\$\""\,\/\:\=\&\+\(\)\@\*\|\\_\#\[\]\{\}\;\?\\!\']+) = ([a-zA-Z0-9\-\s\.\$\""\,\/\:\=\&\+\(\)\@\*\|\\_\#\[\]\{\}\;\?\\!\']+)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
            if (!regexPattern.IsMatch(cLine)) return mainLine;
            var allGroups = regexPattern.Match(cLine).Groups;
            var value = allGroups[1].Value;
            var groupSecond = allGroups[2].Value;
            mainLine = "SET " + value + " TO " + groupSecond;
            return mainLine;
        }

        private static string PseudoCodeForMoveStatement(this string cLine)
        {
            string mainLine = string.Empty;
            var regexPattern = new Regex(@"MOVE ([A-Za-z0-9\w\(\)\<\>\-\""\'\:\+\,\*\s\""\.\=\/\#\\_]*) TO ([A-Za-z0-9\w\(\)\<\+\,\>\-\""\'\:\*\s\.\""\=\/\#\\_]*)", RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);
            if (!regexPattern.IsMatch(cLine)) return mainLine;
            var allGroups = regexPattern.Match(cLine).Groups;
            var value = allGroups[1].Value;
            var groupSecond = allGroups[2].Value;
            mainLine = "SET " + groupSecond + " TO " + value;
            return mainLine;
        }

        private static string PseudoCodeForAddStatement(this string cLine)
        {
            string mainLine = string.Empty;
            var regexPattern = new Regex(@"ADD (([a-zA-Z0-9\-\s\\_\.\""\,\;\/\'\(\)]+)) TO (([a-zA-Z0-9\-\s\\_\.\""\,\;\/\'\(\)\#\-]+))", RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);
            if (!regexPattern.IsMatch(cLine)) return mainLine;
            var allGroups = regexPattern.Match(cLine).Groups;
            var value = allGroups[1].Value;
            var groupThird = allGroups[3].Value;
            mainLine = "SET " + groupThird + " TO " + groupThird + " + " + value;
            return mainLine;
        }
    }
}

/*
 *    public static List<string> CombineAllLines(this List<string> allLines, List<string> methodNameList,List<string> cobolKeyWords)
        {
            var mainBlockList = new List<string>();
            if (allLines.Count <= 0) return allLines;
            var tempList = new List<string>();
            var indexPosition = -1;
            var tempString = string.Empty;
            for (var i = 0; i < allLines.Count; i++)
            {
                indexPosition++;
                var currentLine = allLines[i].Trim();
                if (methodNameList.Any(x => currentLine == x))
                {
                    mainBlockList.Add(currentLine);
                    continue;
                }
                if (cobolKeyWords.Any(x => currentLine.StartsWith(x)))
                {
                    
                }
            }
        }*/
