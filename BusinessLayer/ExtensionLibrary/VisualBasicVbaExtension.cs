using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using BusinessLayer.DbEntities;

namespace BusinessLayer.ExtensionLibrary
{
    public static class VisualBasicVbaExtension
    {
        /// <summary>
        /// <para>Created by: Shubhangi</para>
        /// </summary>
        /// <param name="caseBlockList"></param>
        /// <returns></returns>
        public static List<string> ConvertSelectCaseBlockWithIf(this List<string> caseBlockList)
        {
            var methodBlockList =
                caseBlockList.Where(cLine => !string.IsNullOrEmpty(cLine) && !cLine.StartsWith("Select Case"))
                    .Where(cLine => !string.IsNullOrEmpty(cLine) && !cLine.StartsWith("End Select"))
                    .ToList();

            var firstOrDefault = methodBlockList.FirstOrDefault();
            string expression = string.Empty;
            if (firstOrDefault != null)
            {
                expression = firstOrDefault.Replace("Select Case", "").Trim();
            }
            var finalList = new List<string>();
            var caseOtherwise = methodBlockList.Exists(s => s == "Case Else");
            // Added for check select case avaiable or not 
            var caseSelect = methodBlockList.Exists(s => s == "Select Case");
            if (!caseSelect)
            {
                return methodBlockList;
            }
            //
            var caseCounter = 0;
            var length = methodBlockList.Count;
            foreach (var t in methodBlockList)
            {
                length--;
                var sentense = t;
                if (sentense.TrimStart().StartsWith("Select Case")) continue;

                if ((caseCounter == 0) && sentense.TrimStart().StartsWith("Case "))
                {
                    sentense = t.Replace("Case", "If " + expression);
                    finalList.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.TrimStart().StartsWith("Case Else"))
                {
                    finalList.Add("End If");
                    finalList.Add("else");
                    caseCounter++;
                    continue;
                }
                if (sentense.TrimStart().StartsWith("Case ") && (caseCounter >= 1))
                {
                    finalList.Add("End If");
                    finalList.Add("Else");
                    sentense = t.Replace("Case", "If " + expression);
                    finalList.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (!caseOtherwise && (length == 0))
                    finalList.Add("End If");
                else
                {
                    finalList.Add(sentense);
                }
            }
            return finalList;
        }

        public static List<string> ConvertSelectCaseBlockWithIfStatement(this List<string> caseBlockList)
        {
            var finalList = new List<string>();
            for (int i = 0; i < caseBlockList.Count; i++)
            {
                if (!caseBlockList[i].StartsWith("Select Case"))
                {
                    var statement = caseBlockList[i];
                    finalList.Add(statement);
                }
                else
                {
                    string expression = string.Empty;
                    var selectBlock = caseBlockList.GetBlockOfLinesAsList(i, "End Select");
                    int counter = selectBlock.Count - 1;
                    i = i + counter;
                    var firstOrDefalult = selectBlock.FirstOrDefault();
                    if (firstOrDefalult != null)
                    {
                        expression = firstOrDefalult.Replace("Select Case", "");
                    }
                    var caseCounter = 0;
                    foreach (var t in selectBlock.Skip(1))
                    {
                        var sentense = t;
                        if (sentense.TrimStart().StartsWith("Select Case")) continue;

                        if ((caseCounter == 0) && sentense.TrimStart().StartsWith("Case "))
                        {
                            sentense = t.Replace("Case", "If " + expression);
                            finalList.Add(sentense);
                            caseCounter++;
                            continue;
                        }
                        if (sentense.TrimStart().StartsWith("Case Else"))
                        {
                            // finalList.Add("End If");
                            finalList.Add("else");
                            caseCounter++;
                            continue;
                        }
                        if (sentense.TrimStart().StartsWith("Case ") && (caseCounter >= 1))
                        {

                            //finalList.Add("Else");
                            finalList.Add("End If");
                            sentense = t.Replace("Case", "If " + expression);
                            finalList.Add(sentense);
                            caseCounter++;
                            continue;
                        }

                        finalList.Add(sentense.TrimStart().StartsWith("End Select") ? "End If" : sentense);
                    }

                }

            }
            return finalList;
        }

        /// <summary>
        ///     <para>This method is to remove commented portion within line</para>
        ///     <para></para>
        ///     <para> Created by: Shubhangi Pawar</para>
        /// </summary>
        /// <param name="tempString"></param>
        /// <returns></returns>
        public static string RemoveVbaCommentedPartFromLine(this string tempString)
        {
            string subStr2 = string.Empty;
            if (!tempString.Contains("'")) return subStr2;

            var count = tempString.Count(x => x == '\'');
            if (count <= 0) return subStr2.Trim();
            if (count % 2 == 0) return tempString.Trim();
            int index = tempString.LastIndexOf('\'');
            string substring = tempString.Substring(0, index).Trim();
            return substring;
            /*
            string[] value = tempString.Split('\'');
            var lastOrDefault = value[0].Split(' ').LastOrDefault();
            string secondLine = value[1];
            var aaaa = secondLine.Count(x => x == '\'');
            if (aaaa%2 == 0)
            {
                if (secondLine != null && lastOrDefault.Equals(""))
                {
                    if (secondLine != null && secondLine.Equals(""))
                    {
                        subStr2 = value[0].Trim() + value[1].Trim();
                        return subStr2;
                    }
                }
            }

            if (secondLine != null && lastOrDefault.Equals(""))
                subStr2 = value[0].Trim();

            else
                subStr2 = tempString;
            return subStr2;
             * */
        }

        /// <summary>
        /// </summary>
        /// <param name="inputArray"></param>
        /// <returns></returns>
        public static string[] DoIfLoopsVbaAdjustment(this string[] inputArray)
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

        public static List<string> AlterWithStatement(this List<string> allLines)
        {
            var loopLines = allLines;
            var fileLines = loopLines.ToArray();
            var methodBlockListMain = new List<string>();
            var methodBlock = fileLines.ToList();
            string aaaa = string.Empty;
            int flagWith = 0;
            foreach (var method in methodBlock)
            {
                if (flagWith == 0)
                {
                    if (!method.StartsWith("With "))
                    {
                        methodBlockListMain.Add(method);
                    }
                    else
                    {
                        flagWith = 1;
                        aaaa = method.Replace("With", "");
                    }
                }
                else
                {
                    if (flagWith == 1)
                    {
                        string stratingDot = method;
                        if ((!stratingDot.StartsWith('.'.ToString()) && stratingDot.Contains('.')) ||
                            (stratingDot.StartsWith('.'.ToString())))
                        {
                            string d = string.Empty;
                            string[] strval = stratingDot.Split('=');
                            for (int i = 0; i < strval.Length; i++)
                            {
                                if (i > 0)
                                {
                                    if (strval[i].Trim().StartsWith("."))
                                    {
                                        d = d + "=" + aaaa + strval[i];
                                    }
                                    else
                                    {
                                        d = d + "=" + strval[i];
                                    }
                                }
                                else
                                {
                                    if (strval[i].Trim().StartsWith("."))
                                    {
                                        d = d + " " + aaaa + strval[i];
                                    }
                                    else
                                    {
                                        d = d + " " + strval[i];
                                    }
                                }
                            }
                            methodBlockListMain.Add(d.TrimStart().Trim());
                        }
                        else if (stratingDot.StartsWith("End With"))
                        {
                            flagWith = 0;
                        }
                        else
                        {
                            methodBlockListMain.Add(method);
                        }
                    }
                }

                //var withBlock = methodBlock.GetBlockOfLinesAsList(indexPositionPrgm, "End With");
                //foreach (var line in withBlock)
                //{
                //    iIndex++;
                //    var newline = withBlock[0];
                //    var aaaa = newline.Replace("With", "");
                //    if (iIndex > 0)
                //    {
                //        string stratingDot = line;
                //        if ((!stratingDot.StartsWith('.'.ToString()) && stratingDot.Contains('.')) ||
                //            (stratingDot.StartsWith('.'.ToString())))
                //        {
                //            string d = string.Empty;
                //            string[] strval = stratingDot.Split(' ');
                //            for (int i = 0; i < strval.Length; i++)
                //            {
                //                if (strval[i].StartsWith("."))
                //                {
                //                    d = d + " " + aaaa + strval[i];
                //                }
                //                else
                //                {
                //                    d = d + " " + strval[i];
                //                }
                //            }
                //            methodBlockListMain.Add(d.TrimStart().Trim());
                //        }
                //        else if (!stratingDot.StartsWith("End With"))
                //        {
                //            methodBlockListMain.Add(stratingDot);
                //        }
                //    }
                //}
            }
            return methodBlockListMain;
        }

        public static List<string> ConvertCaseStatement(this List<string> allLines)
        {

            //var selectBlock = allLines.ConvertSelectCaseBlockWittIfStatement();
            var selectBlock = allLines.ConvertSelectCaseBlockWithIfStatement();
            return selectBlock;
        }

        public static List<string> RemoveBeginFormStatement(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var indexPositionPrgm = -1;
            foreach (var method in methodBlock)
            {
                indexPositionPrgm++;
                if (!method.StartsWith("Begin Form"))
                    continue;
                selectBlock = methodBlock.GetBlockOfLinesAsList(indexPositionPrgm, "CodeBehindForm");
                //selectBlock = withBlock;
                return selectBlock;
            }
            return selectBlock;
        }

        public static List<string> RemoveBeginFormStatementNew(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var methodMain = methodBlock.ToArray();
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

        public static List<string> GetAllRecordSource(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            foreach (var method in methodBlock)
            {
                if (method.StartsWith("RecordSource"))
                    selectBlock.Add(method);
            }
            return selectBlock;
        }

        public static string[] CombineAllBrokenLinesQueryWhere(this string[] inputArray, char lineBreakElement)
        {
            if (inputArray.Length <= 0) return inputArray;
            var tempList = new List<string>();
            var indexPosition = -1;
            var tempString = string.Empty;

            for (var i = 0; i < inputArray.Length; i++)
            {
                indexPosition++;
                var regex = new Regex(@"[""\s]$");
                var regexSql = new Regex(@"[\""]SQL[""][\w\s][\=]");

                if (regexSql.IsMatch(inputArray[i].TrimEnd()))
                {
                    if (inputArray[i].Contains("SQL"))
                    {
                        for (var j = i; j < inputArray.Length; j++)
                        {
                            if (inputArray[j].Contains("SQL") ||
                                inputArray[j].TrimStart().TrimEnd().StartsWith("\""))
                            {
                                if (j == i)
                                {
                                    var reomveComma = inputArray[j].Remove(inputArray[j].Length - 1, 1);
                                    if (reomveComma.EndsWith("\""))
                                    {
                                        tempString = reomveComma.Remove(reomveComma.Length - 1, 1) + " ";
                                    }
                                    else
                                    {
                                        tempString = reomveComma + " ";
                                    }
                                }
                                else
                                {
                                    var reomveComma1 = inputArray[j].Remove(inputArray[j].Length - 1, 1);

                                    reomveComma1 = reomveComma1.TrimStart().Remove(0, 1);
                                    tempString += reomveComma1.TrimStart();
                                    //tempString +=
                                    //    inputArray[j].Remove(inputArray[j].Length - 1, 1) + " ";
                                }
                                indexPosition++;
                                continue;
                            }
                            tempList.Add(tempString + "\"");
                            //tempList.Add(inputArray[j - 1]);
                            tempString = string.Empty;
                            break;
                        }
                        i = indexPosition;
                    }
                }
                else if (regex.IsMatch(inputArray[i].TrimEnd()))
                {
                    if (inputArray[i].StartsWith("Where") || inputArray[i].StartsWith("Having"))
                    {
                        var flagWhere = 1;
                        if (flagWhere != 1) continue;
                        for (var j = i; j < inputArray.Length; j++)
                        {
                            if (!regex.IsMatch(inputArray[j].TrimStart().TrimEnd())) continue;
                            if (inputArray[j].StartsWith("Where") || inputArray[j].StartsWith("Having") ||
                                inputArray[j].TrimStart().TrimEnd().StartsWith("\""))
                            {
                                if (j == i)
                                {
                                    var reomveComma = inputArray[j].Remove(inputArray[j].Length - 1, 1);
                                    tempString = reomveComma.EndsWith("\"") 
                                        ? reomveComma.Remove(reomveComma.Length - 1, 1) 
                                        : reomveComma;
                                    //tempString += inputArray[j].Remove(inputArray[j].Length - 1, 1)
                                    //    .TrimEnd('\"');
                                }
                                else
                                {
                                    var reomveComma1 = inputArray[j].Remove(inputArray[j].Length - 1, 1);

                                    reomveComma1 = reomveComma1.TrimStart().Remove(0, 1);
                                    tempString += reomveComma1.TrimStart();
                                    //tempString +=
                                    //    inputArray[j].Remove(inputArray[j].Length - 1, 1) + " ";
                                }
                                indexPosition++;
                                continue;
                            }
                            tempList.Add(tempString + "\"");
                            tempList.Add(inputArray[j - 1]);
                            tempString = string.Empty;
                            break;
                        }
                        i = indexPosition;
                    }
                    else
                    {
                        tempList.Add(inputArray[i]);
                    }
                }
                else
                    tempList.Add(inputArray[i]);
            }
            return tempList.ToArray();
        }

        public static List<string> RemoveBeginStatement(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var indexPositionPrgm = -1;
            var regexSql = new Regex(@"[\""]SQL[""][\w\s][\=]");

            var methodMain = methodBlock.ToArray();
            methodMain = methodMain.CombineAllBrokenLinesQueryWhere('\"');
            foreach (var method in methodMain)
            {
                indexPositionPrgm++;
                if (method.StartsWith("Where") || method.StartsWith("Having"))
                {
                    selectBlock.Insert(0, method);
                }
                else if (method.StartsWith("Begin InputTables") || method.StartsWith("Begin Joins") ||
                         method.StartsWith("Begin OrderBy") || method.StartsWith("Begin Groups"))
                {
                    var selectBlock1 = methodMain.ToList().GetBlockOfLinesAsList(indexPositionPrgm, "End");
                    selectBlock.InsertRange(selectBlock.Count, selectBlock1);
                }
                else if (regexSql.IsMatch(method.TrimEnd()))
                {
                    selectBlock.Insert(0, method);
                }
                // else if(method.Contains("Begin Joins"))
            }
            return selectBlock;
        }

        public static string Between(this string value, string a, string b)
        {
            int posA = value.IndexOf(a, StringComparison.Ordinal);
            int posB = value.LastIndexOf(b, StringComparison.Ordinal);
            if (posA == -1)
                return "";
            if (posB == -1)
                return "";
            int adjustedPosA = posA + a.Length;
            return adjustedPosA >= posB
                ? ""
                : value.Substring(adjustedPosA, posB - adjustedPosA);
        }

        public static string[] CombineAllSelectStatmentForRecordSource(this string[] inputArray, char lineBreakElement)
        {
            if (inputArray.Length <= 0) return inputArray;
            var tempList = new List<string>();
            var indexPosition = -1;
            var tempString = string.Empty;

            for (var i = 0; i < inputArray.Length; i++)
            {
                indexPosition++;

                if (inputArray[i].TrimStart().StartsWith("RecordSource"))
                {
                    for (var j = i; j < inputArray.Length; j++)
                    {
                        if (i == j)
                        {
                            var reomveComma = inputArray[j].Remove(inputArray[j].Length - 1, 1);
                            tempString = reomveComma.EndsWith("\"")
                                ? reomveComma.Remove(reomveComma.Length - 1, 1)
                                : reomveComma;
                            continue;
                        }
                        if (inputArray[j].TrimStart().TrimEnd().StartsWith("\""))
                        {
                            var reomveComma1 = inputArray[j].Remove(inputArray[j].Length - 1, 1);

                            reomveComma1 = reomveComma1.TrimStart().Remove(0, 1);
                            tempString += reomveComma1.TrimStart();
                            indexPosition++;
                            continue;
                        }
                        break;
                    }
                    tempList.Add(tempString + "\"");
                    tempString = string.Empty;
                    i = indexPosition;
                }


                else
                    tempList.Add(inputArray[i]);
            }
            return tempList.ToArray();
        }

        public static List<string> GetRecordSourceStatement(this List<string> allLines)
        {
            var recordSourceList =
                allLines.Where(x => !string.IsNullOrEmpty(x) && x.StartsWith("RecordSource")).ToList();
            return recordSourceList;
        }

        public static List<string> CheckExitSubInIf(this List<string> allLines)
        {
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var methodMain = methodBlock.ToArray();

            foreach (var method in methodMain)
            {
                if (method.ContainsAll("If ", " Exit Sub"))
                {
                    int val = method.IndexOf("Exit", StringComparison.Ordinal);
                    string firstVal = method.Substring(0, val);
                    string secondVal = method.Substring(val);
                    if (!string.IsNullOrEmpty(firstVal))
                        selectBlock.Add(firstVal.Trim());
                    if (!string.IsNullOrEmpty(secondVal))
                        selectBlock.Add(secondVal.Trim());
                }
                else
                    selectBlock.Add(method);
            }
            return selectBlock;
        }


        public static List<string> RemoveBeginFormStatementNew(this List<string> allLines,
            out Dictionary<string, NameCaption> dictionary)
        {
            var selectBlock = new List<string>();
            var captionBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var methodMain = methodBlock.ToArray();
            methodMain = methodMain.CombineAllSelectStatmentForRecordSource('\"');

            int flag = 0;

            foreach (var method in methodMain)
            {
                if (method.StartsWith("Begin Form") || method.StartsWith("Begin Report"))
                {
                    flag = 1;
                    captionBlock.Add(method);
                    continue;
                }
                if (method.StartsWith("CodeBehindForm"))
                {
                    flag = 0;
                    continue;
                }
                if (flag == 0 || method.StartsWith("RecordSource"))
                {
                    selectBlock.Add(method);
                    continue;
                }
                captionBlock.Add(method);
            }
            // return captionBlock;
            dictionary = new Dictionary<string, NameCaption>();
            foreach (var unused in captionBlock)
            {
                var list = new List<string>();
                captionBlock = captionBlock.Select(line => line.Trim()).ToList();
                int indexPosition = -1;
                int bStart = 0;
                // int bEnd = 0;
                foreach (var l in captionBlock)
                {
                    // indexPosition++;
                    list.Add(l);
                    if (l.StartsWith("Begin") || l.Contains("= Begin"))
                        bStart++;
                    if (l == "End")
                        bStart--;
                    if (bStart == 0)
                        break;
                }

                // loop for forms caption and name...
                //bool begin = false;
                dictionary = new Dictionary<string, NameCaption>();
                //List<NameCaption> naemCaption = new List<NameCaption>();
                NameCaption nameCaption = new NameCaption();
                foreach (var l in list)
                {
                    indexPosition++;
                    if (l.StartsWith("Begin Form") || l.StartsWith("Begin Report")) continue;
                    if (l.StartsWith("Name =") || l.StartsWith("Name="))
                        nameCaption.Name = l.Split('=')[1];
                    if (l.StartsWith("Caption =") || l.StartsWith("Caption="))
                        if (list[indexPosition + 1].StartsWith("\""))
                        {
                            nameCaption.Caption = l + list[indexPosition + 1].Replace("\"", "");
                            nameCaption.Caption = nameCaption.Caption.Split('=').LastOrDefault();
                        }
                        else
                        {
                            nameCaption.Caption = l.Split('=').LastOrDefault();
                        }

                    //begin = true;

                    if (!l.StartsWith("Begin")) continue;
                    //naemCaption.Add(nameCaption);
                    break;
                }

                dictionary.Add("frmName", nameCaption);
                //list = new List<string>();
                list = list.Skip(indexPosition).ToList();
                indexPosition = -1;

                foreach (var item in list)
                {
                    bool check = false;
                    indexPosition++;
                    if (!item.StartsWith("Begin") || item == "Begin") continue;
                    var listFinal = new List<string>();
                    for (int i = indexPosition; i < list.Count; i++)
                    {
                        listFinal.Add(list[i]);
                        if (list[i].StartsWith("Begin") || list[i].Contains("= Begin"))
                            bStart++;
                        if (list[i] == "End")
                            bStart--;
                        if (bStart == 0)
                            break;
                        if (list[i] == "Begin")
                            check = true;
                    }
                    if (check) continue;
                    nameCaption = new NameCaption();
                    foreach (var l in listFinal)
                    {
                        if (l.StartsWith("Name =") || l.StartsWith("Name="))
                            nameCaption.Name = l.Split('=')[1];
                        if (l.StartsWith("Caption =") || l.StartsWith("Caption="))
                            nameCaption.Caption = l.Replace("\"", "");
                        if (nameCaption.Caption == null) continue;
                        nameCaption.Caption = nameCaption.Caption.Split('=').LastOrDefault();
                        if (nameCaption.Caption == null || nameCaption.Name == null)
                        {
                        }
                    }
                    if (!string.IsNullOrEmpty(nameCaption.Name) &&
                        !dictionary.Keys.Any(d => d.StartsWith(nameCaption.Name)))
                    {
                        var nm = nameCaption.Name.Split('=').LastOrDefault();
                        if (nm != null)
                            dictionary.Add(nm, new NameCaption
                            {
                                Caption =
                                    string.IsNullOrWhiteSpace(nameCaption.Caption)
                                        ? ""
                                        : nameCaption.Caption.Replace("\"", "").Split('=').LastOrDefault(),
                                Name = nameCaption.Name.Replace("\"", "").Split('=').LastOrDefault()
                            });
                    }
                    // dictionary.Add("", nameCaption);
                }

                //dictionary.Add("", nameCaption);
                //return dictionary;
            }


            return selectBlock;
        }

        public static List<DataDependency> DistinctByData(this List<DataDependency> collection)
        {
            var distinctList = new List<DataDependency>();

            var values = collection as IList<DataDependency> ?? collection.ToList();
            foreach (var value in values)
            {
                if (distinctList.All(v => !Equals(value.Entity.ToLower(), v.Entity.ToLower())))
                    distinctList.Add(value);
            }
            return distinctList;
        }
        public class NameCaption
        {
            public string Name { get; set; }
            public string Caption { get; set; }
        }
    }
}