using DataAccessLayer;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace BusinessLayer.ExtensionLibrary
{
    /// <summary>
    /// This partial class intensionally used only for COBOL Language processing...
    /// Add any extension method here in this file only for COBOL Language
    /// </summary>
    public static class CobolExtensions
    {
        static readonly List<string> _finalStringWorkStor = new List<string>();
        static int _flag;
        static string _tempStrNoPeriod = string.Empty;
        static List<string> lstLanguageKeywords = new List<string>();

        /// <summary>
        /// <para>Collect comment section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectAllComments(this List<string> lstAllLines)
        {
            List<string> textComment = new List<string>();
            List<int> lstFirstComment =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i }).Select(x => x.Index).ToList<int>();
            List<int> lstlastComment = null;
            lstlastComment =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("DATA DIVISION".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();

            if (lstFirstComment.Count != 0 && lstlastComment.Count != 0)
            {
                lstlastComment[0] = lstlastComment[0] - 1;
                lstFirstComment[0] = lstFirstComment[0];
                textComment =
                    lstAllLines.Skip(lstFirstComment[0])
                        .Take(lstlastComment[0] - lstFirstComment[0])
                        .ToList<string>();
            }

            return textComment;
        }

        /// <summary>
        /// <para>Collect working storage section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectWorkingStorageSection(this List<string> lstAllLines)
        {
            List<string> textLineWs = new List<string>();

            List<int> lstFirstIndexWs =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                            .Where(x => x.Str.Contains("working-storage section.".ToUpper())
                                                && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                            .Select(x => x.Index)
                                            .ToList<int>();
            List<int> lstlastIndexWs = null;
            lstlastIndexWs =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where((x => x.Str.Contains("LINKAGE SECTION.".ToUpper())
                           && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false))))
                    .Select(x => x.Index)
                    .ToList<int>();
            if (lstlastIndexWs.Count == 0)
            {
                lstlastIndexWs =
                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                        .Where(x => x.Str.Contains("PROCEDURE DIVISION".ToUpper())
                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                        .Select(x => x.Index)
                        .ToList<int>();
            }
            if (lstFirstIndexWs.Count != 0 && lstlastIndexWs.Count != 0)
            {
                lstlastIndexWs[0] = lstlastIndexWs[0] - 1;
                lstFirstIndexWs[0] = lstFirstIndexWs[0] + 1;
                textLineWs =
                    lstAllLines.Skip(lstFirstIndexWs[0])
                        .Take(lstlastIndexWs[0] - lstFirstIndexWs[0])
                        .ToList<string>();
            }

            textLineWs.RemoveAll(stringToCheck => stringToCheck.Contains(" INCLUDE "));
            textLineWs.RemoveAll(stringToCheck => stringToCheck.Contains("EXEC SQL INCLUDE "));

            for (int iCnt = 0; iCnt < textLineWs.Count; iCnt++)
            {
                string strListStr = textLineWs[iCnt].ToString();
                if (strListStr.Length > 5)
                {
                    strListStr = strListStr.Remove(0, 6);
                    textLineWs[iCnt] = textLineWs[iCnt].Replace(textLineWs[iCnt].ToString(), strListStr);
                    if (textLineWs[iCnt].Length >= 67)
                    {
                        textLineWs[iCnt] = textLineWs[iCnt].Remove(66);
                    }
                }
            }
            List<string> lstringWorkingStorage = new List<string>();
            foreach (string str in textLineWs)
            {
                lstringWorkingStorage = FunctionReturnWholeStatementWs(str);
            }

            if (lstringWorkingStorage != null && lstringWorkingStorage.Count > 0)
            {
                textLineWs = new List<string>();
                textLineWs = lstringWorkingStorage;
            }
            return textLineWs;
        }

        /// <summary>
        /// <para>Collect working storage section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectFileControlSection(this List<string> lstAllLines)
        {
            List<string> textLineFc = new List<string>();
            List<int> lstFirstIndexFc =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("FILE-CONTROL".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            List<int> lstlastIndexFc =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("DATA DIVISION".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            if (lstFirstIndexFc.Count != 0 && lstlastIndexFc.Count != 0)
            {
                lstlastIndexFc[0] = lstlastIndexFc[0] - 1;
                lstFirstIndexFc[0] = lstFirstIndexFc[0] + 1;
                textLineFc =
                    lstAllLines.Skip(lstFirstIndexFc[0] + 1)
                        .Take(lstlastIndexFc[0] - lstFirstIndexFc[0])
                        .ToList<string>();
            }
            return textLineFc;
        }

        /// <summary>
        /// <para>Collect data defination section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectDataDefinationSection(this List<string> lstAllLines)
        {
            List<string> textLineDd = new List<string>();
            List<int> lstFirstIndexDd =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("DATA DIVISION".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            List<int> lstlastIndexDd =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("WORKING-STORAGE SECTION.".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            if (lstFirstIndexDd.Count != 0 && lstlastIndexDd.Count != 0)
            {
                lstlastIndexDd[0] = lstlastIndexDd[0] - 1;
                lstFirstIndexDd[0] = lstFirstIndexDd[0] + 1;
                textLineDd =
                    lstAllLines.Skip(lstFirstIndexDd[0] + 1)
                        .Take(lstlastIndexDd[0] - lstFirstIndexDd[0])
                        .ToList<string>();
            }
            return textLineDd;
        }

        /// <summary>
        /// <para>Collect linkage section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectLinkageSection(this List<string> lstAllLines)
        {
            List<string> textLineLs = new List<string>();
            List<int> lstFirstIndexLs =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where((x => x.Str.Contains("LINKAGE SECTION.".ToUpper())
                           && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false))))
                    .Select(x => x.Index)
                    .ToList<int>();
            List<int> lstlastIndexLs =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("PROCEDURE DIVISION".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            if (lstFirstIndexLs.Count != 0 && lstlastIndexLs.Count != 0)
            {
                lstlastIndexLs[0] = lstlastIndexLs[0] - 1;
                lstFirstIndexLs[0] = lstFirstIndexLs[0] + 1;
                textLineLs =
                    lstAllLines.Skip(lstFirstIndexLs[0])
                        .Take(lstlastIndexLs[0] - lstFirstIndexLs[0])
                        .ToList<string>();
            }
            return textLineLs;
        }

        /// <summary>
        /// <para>Collect procedure division section in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> CollectProcedureDivisionSection(this List<string> lstAllLines)
        {
            List<string> textLinePd = new List<string>();
            List<int> lstFirstIndexPd =
                lstAllLines.Select((s, i) => new { Str = s, Index = i })
                    .Where(x => x.Str.Contains("PROCEDURE DIVISION".ToUpper())
                        && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                    .Select(x => x.Index)
                    .ToList<int>();
            // instead of above -> try to read the file till end.
            int lstlastIndexPd = lstAllLines.Count;
            if (lstFirstIndexPd.Count != 0 && lstlastIndexPd != 0)
            {
                textLinePd =
                    lstAllLines.Skip(lstFirstIndexPd[lstFirstIndexPd.Count - 1])
                        .Take(lstlastIndexPd - lstFirstIndexPd[lstFirstIndexPd.Count - 1])
                        .ToList<string>();
            }
            return textLinePd;
        }

        /// <summary>
        /// <para>Remove all commented line in cobol</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> RemoveAllCommentedLines(this List<string> lstAllLines)
        {
            List<string> exceptCommentsLines = new List<string>();
            lstAllLines.RemoveAll(stringToCheck => stringToCheck.Trim().StartsWith("*"));
            foreach (string cLine in lstAllLines)
            {
                string tempString = cLine;
                exceptCommentsLines.Add(tempString);
            }

            return exceptCommentsLines;
        }

        public static void ModifyOnErrorOrContinueStatements(this string input)
        {

        }

        public static List<string> FunctionReturnWholeStatementWs(string str)
        {
            var formattedString = str;
            // if that line contains PERIOD / FULLSTOP
            if ((formattedString.Contains(".")) && (_flag == 0) && (formattedString.Contains("*") == false) && (formattedString.Trim().StartsWith("/") == false))
            {
                // if that is NOT the LAST FULLSTOP
                if ((!formattedString.Trim().EndsWith(".")))
                {

                    List<string> lstValuesToAdd = formattedString.Split('.').ToList();
                    for (int counter = 0; counter < lstValuesToAdd.Count - 1; counter++)
                    {
                        _finalStringWorkStor.Add(lstValuesToAdd[counter] + ".");
                    }
                    _tempStrNoPeriod += lstValuesToAdd[lstValuesToAdd.Count - 1];
                    _flag = 1;
                }
                //If that FULLSTOP occures at the END - Insert that line directly in DB
                else
                {
                    _finalStringWorkStor.Add(formattedString);
                    _flag = 0;
                }
            }

            else if (((formattedString.TrimStart().StartsWith("*") == false)) && ((formattedString.Trim() != "")))
            {
                // Try to find period in next lines. Traverse upto Period.
                // This will be controlled by flag [flag=1 means it will traverse again for next line]
                if (formattedString.Contains("."))
                {
                    _finalStringWorkStor.Add(_tempStrNoPeriod + " " + formattedString);
                    // Empty tempStrNoPeriod, else it will append again next time
                    _tempStrNoPeriod = string.Empty;
                    _flag = 0;
                }
                else
                {
                    _tempStrNoPeriod += " " + formattedString;
                    _flag = 1;
                }
            }
            return _finalStringWorkStor;
        }

        /// <summary>
        /// <para>Remove unnecessary first 6 & last 8 digits and remove unwanted statements</para>
        /// </summary>
        /// <param name="lstAllLines"></param>
        /// <returns></returns>
        public static List<string> RemoveUnnecessaryDigit(this List<string> textLinePd)
        {
            string strListStr = "";
            try
            {
                for (int iCnt = 0; iCnt < textLinePd.Count; iCnt++)
                {
                    strListStr = textLinePd[iCnt].ToString();
                    if (strListStr != "" && strListStr.Length > 5)
                    {
                        strListStr = strListStr.Remove(0, 6);
                        textLinePd[iCnt] = textLinePd[iCnt].Replace(textLinePd[iCnt].ToString(), strListStr);
                        if (textLinePd[iCnt].Length >= 67)
                        {
                            textLinePd[iCnt] = textLinePd[iCnt].Remove(66);
                            string strTemp = textLinePd[iCnt];
                        }
                    }
                    else if (strListStr != "")
                    {
                        if (strListStr.StartsWith(" "))
                        {
                        }
                        else if (textLinePd[iCnt].Length != 0)
                        {
                            strListStr = strListStr.Remove(0, ((textLinePd[iCnt].Length)));
                            textLinePd[iCnt] = textLinePd[iCnt].Remove(0, ((textLinePd[iCnt].Length)));
                        }
                    }
                }

                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("EJECT "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP1 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP2 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP3 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP4 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP5 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP6 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP7 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP8 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP9 "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("SKIP"));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("COPY "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("/ "));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("/"));
                textLinePd.RemoveAll(x => x.TrimStart().StartsWith("**"));
                textLinePd.RemoveAll(x => x.Trim() == "");
            }
            catch (Exception e)
            {
                Console.WriteLine(e.InnerException);

            }
            return textLinePd;
        }

        public static List<string> LanguageKeywordStatementSplit(this List<string> textLinePd)
        {
            char[] TRIM_CHARACTER = { ' ', '0', '+' };
            const StringComparison NoCase = StringComparison.OrdinalIgnoreCase;
            List<string> lstFinalBlock = new List<string>();
            string currentSentence = "";
            string tempInsertToStr = "";
            string nextSentence = "";
            List<string> textLineProcedureDivisionTemp = new List<string>();

            lstLanguageKeywords.AddRange(GetLanguageKeyword());
            #region  -- Setting language keywords logic --

            for (int ilstCNt = 0; ilstCNt < textLinePd.Count; ilstCNt++)
            {
                currentSentence = textLinePd[ilstCNt];
                #region ------------ KEYWORD LOGIC -------------------

                if (currentSentence.Trim() != "" && currentSentence.Trim() != "+" && currentSentence.Trim() != "0" &&
                    currentSentence.Trim() != "0+" && !currentSentence.StartsWith("*"))
                {
                    if (currentSentence.Trim().EndsWith(".") || tempInsertToStr.Trim().EndsWith(".") ||
                        currentSentence.Contains(" := ") || (currentSentence.Contains("END-SUBROUTINE")))
                    {
                        if (currentSentence.Trim().EndsWith(".") && tempInsertToStr.Trim().EndsWith("."))
                        {
                            textLineProcedureDivisionTemp.Add(currentSentence);
                            textLineProcedureDivisionTemp.Add(tempInsertToStr);
                        }
                        else if (currentSentence.Trim().EndsWith(".") && tempInsertToStr != "")
                        {
                            textLineProcedureDivisionTemp.Add(tempInsertToStr);
                            textLineProcedureDivisionTemp.Add(currentSentence);
                        }
                        else
                        {
                            textLineProcedureDivisionTemp.Add(currentSentence);
                        }
                        tempInsertToStr = string.Empty;
                    }

                    else
                    {
                        for (int jlstCNt = ilstCNt; jlstCNt < textLinePd.Count; jlstCNt++)
                        {
                            if (jlstCNt + 1 < textLinePd.Count)
                            {
                                nextSentence = textLinePd[jlstCNt + 1];
                                if (nextSentence.Trim() != "")
                                {
                                    string tmpStrMatchNS = string.Empty;
                                    string tmpStrMatchNS1 = string.Empty;
                                    string tmpStrMatchCS = string.Empty;
                                    string tmpStrMatchCS1 = string.Empty;
                                    string tmpStrMatchTemp = string.Empty;
                                    List<string> matchingCS = lstLanguageKeywords.Intersect(currentSentence);
                                    List<string> matchingNS = lstLanguageKeywords.Intersect(nextSentence.Replace(".", ""));

                                    if (nextSentence.IndexOf("ON ERROR", NoCase) != -1)
                                    {
                                        matchingNS.Add("ON ERROR");
                                        tmpStrMatchNS = "ON ERROR";
                                    }
                                    else if (nextSentence.IndexOf("CONTINUE", NoCase) != -1)
                                    {
                                        matchingNS.Add("CONTINUE");
                                        tmpStrMatchNS = "CONTINUE";
                                    }

                                    List<string> matchingTmp = lstLanguageKeywords.Intersect(tempInsertToStr);

                                    if ((matchingNS.Count > 0) || (matchingCS.Count > 0))
                                    {
                                        if ((matchingNS.Count > 0))
                                        {
                                            tmpStrMatchNS = matchingNS[0];
                                            if (matchingNS.Count > 1)
                                            {
                                                tmpStrMatchNS1 = matchingNS[1];
                                            }
                                        }
                                        if ((matchingCS.Count > 0))
                                        {
                                            tmpStrMatchCS = matchingCS[0];
                                            if (matchingCS.Count > 1)
                                            {
                                                tmpStrMatchCS1 = matchingCS[1];
                                            }
                                        }
                                        if ((matchingTmp.Count > 0))
                                        {
                                            tmpStrMatchTemp = matchingTmp[0];
                                        }
                                    }

                                    if ((nextSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchNS) && matchingNS.Count > 0) ||
                                        (nextSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchNS1) && matchingNS.Count > 0 && tmpStrMatchNS1 != ""))
                                    {
                                        if (
                                            ((currentSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchCS)) && matchingCS.Count > 0) ||
                                            ((tmpStrMatchCS1 != "") && (((currentSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchCS1)) && matchingCS.Count > 0)) ||
                                             currentSentence.StartsWith("REINPUT ")))
                                        {

                                            textLineProcedureDivisionTemp.Add(Convert.ToString(currentSentence));
                                            currentSentence = string.Empty;
                                            currentSentence = nextSentence;

                                            break;
                                        }
                                        else
                                        {
                                            if ((nextSentence.Trim().EndsWith(".")) ||
                                                (nextSentence.Trim().ToUpper().Contains("ON ERROR")))
                                            {
                                                if (currentSentence.Trim().StartsWith("+++++++++++++++++"))
                                                {
                                                    textLineProcedureDivisionTemp.Add(currentSentence);
                                                    textLineProcedureDivisionTemp.Add(nextSentence);
                                                    currentSentence = string.Empty;
                                                    nextSentence = string.Empty;
                                                    ilstCNt = jlstCNt + 1;
                                                    break;
                                                }
                                                else
                                                {
                                                    if (matchingCS.Count == 0)
                                                    {
                                                        textLineProcedureDivisionTemp.Add(currentSentence);
                                                        textLineProcedureDivisionTemp.Add(nextSentence);
                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt + 1;
                                                        break;
                                                    }
                                                    else
                                                    {
                                                        if (nextSentence.Trim().StartsWith(Convert.ToString(tmpStrMatchNS)))
                                                        {
                                                            textLineProcedureDivisionTemp.Add(Convert.ToString(currentSentence));
                                                            textLineProcedureDivisionTemp.Add(Convert.ToString(nextSentence));
                                                        }
                                                        else
                                                        {
                                                            textLineProcedureDivisionTemp.Add(
                                                            Convert.ToString(currentSentence + nextSentence));
                                                        }

                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt + 1;
                                                        break;
                                                    }
                                                }
                                            }
                                            else
                                            {
                                                currentSentence = currentSentence + nextSentence;
                                                continue;
                                            }
                                        }

                                    }
                                    else
                                    {
                                        if (
                                            ((currentSentence.TrimStart(TRIM_CHARACTER)
                                                .StartsWith(tmpStrMatchCS)) && matchingCS.Count > 0))
                                        {
                                            if (nextSentence.Trim().EndsWith(".") ||
                                                (nextSentence.Trim().Contains(" := ") ||
                                                (nextSentence.Trim().StartsWith("REINPUT ")) ||
                                                (nextSentence.Trim().Contains("AT TOP OF PAGE")) ||
                                                (nextSentence.Trim().Contains("AT BREAK OF ARTIST")) ||
                                                (nextSentence.Trim().Contains("AT BREAK OF")) ||
                                                (nextSentence.Trim().Contains("NEWPAGE")) ||
                                                (nextSentence.Trim().StartsWith("IGNORE"))))
                                            {
                                                if (currentSentence.Trim().EndsWith(".") ||
                                                    (nextSentence.Trim().Contains(" := ")) ||
                                                    (nextSentence.Trim().StartsWith("REINPUT ")) ||
                                                    (nextSentence.Trim().Contains("AT TOP OF PAGE")) ||
                                                    (nextSentence.Trim().Contains("AT BREAK OF ARTIST")) ||
                                                    (nextSentence.Trim().Contains("AT BREAK OF")) ||
                                                    (nextSentence.Trim().Contains("NEWPAGE")) ||
                                                    (nextSentence.Trim().StartsWith("IGNORE")) ||
                                                    (nextSentence.Trim().StartsWith("CONTINUE"))
                                                    )
                                                {
                                                    if (nextSentence.Trim().StartsWith("REINPUT "))
                                                    {
                                                        textLineProcedureDivisionTemp.Add(Convert.ToString(currentSentence));
                                                        currentSentence = string.Empty;
                                                        break;
                                                    }
                                                    else if (nextSentence.Trim().StartsWith("IGNORE"))
                                                    {
                                                        textLineProcedureDivisionTemp.Add(Convert.ToString(currentSentence));
                                                        textLineProcedureDivisionTemp.Add(Convert.ToString(nextSentence));
                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt + 1;
                                                        break;
                                                    }
                                                    else
                                                    {
                                                        textLineProcedureDivisionTemp.Add(Convert.ToString(currentSentence));
                                                        textLineProcedureDivisionTemp.Add(Convert.ToString(nextSentence));
                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt + 1;
                                                        break;
                                                    }
                                                }
                                                else
                                                {
                                                    if (nextSentence.Trim().StartsWith("END-IF"))
                                                    {
                                                        textLineProcedureDivisionTemp.Add(
                                                           Convert.ToString(currentSentence));
                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt;
                                                        break;
                                                    }
                                                    else
                                                    {
                                                        textLineProcedureDivisionTemp.Add(
                                                            Convert.ToString(currentSentence + nextSentence));
                                                        currentSentence = string.Empty;
                                                        nextSentence = string.Empty;
                                                        ilstCNt = jlstCNt + 1;
                                                        break;
                                                    }
                                                }
                                            }
                                            else
                                            {
                                                currentSentence = currentSentence + " " + nextSentence;
                                                ilstCNt = jlstCNt + 1;
                                                continue;
                                            }
                                        }
                                        else
                                        {
                                            if (nextSentence.Trim().EndsWith(".") ||
                                                (nextSentence.Trim().Contains(" := ")))
                                            {
                                                if (nextSentence.Contains(" := ") ||
                                                    nextSentence.TrimStart(TRIM_CHARACTER).ToUpper().StartsWith("INCLUDE") ||
                                                    nextSentence.TrimStart(TRIM_CHARACTER).ToUpper().StartsWith("INCLUDE"))
                                                {
                                                    textLineProcedureDivisionTemp.Add(currentSentence + tempInsertToStr);
                                                    textLineProcedureDivisionTemp.Add(nextSentence);
                                                    currentSentence = string.Empty;
                                                    tempInsertToStr = string.Empty;
                                                    nextSentence = string.Empty;
                                                    ilstCNt = jlstCNt + 1;
                                                    break;
                                                }
                                                else
                                                {
                                                    textLineProcedureDivisionTemp.Add(currentSentence + tempInsertToStr + nextSentence);
                                                    currentSentence = string.Empty;
                                                    tempInsertToStr = string.Empty;
                                                    nextSentence = string.Empty;
                                                    ilstCNt = jlstCNt + 1;
                                                    break;
                                                }
                                            }
                                            else
                                            {
                                                currentSentence += " " + nextSentence;
                                                ilstCNt = jlstCNt + 1;
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                #endregion
            }
            textLinePd = textLineProcedureDivisionTemp;
            #endregion

            return textLinePd;
        }

        public static List<string> GetLanguageKeyword()
        {
            DataSet dsLanguageKeyword = new DataSet();
            List<string> lstLanguageKeywordsNew = new List<string>();
            string strQuery = "Select KeywordName from languagekeywords";
            MySqlDbConnectionBaseClass _appBlock = new MySqlDbConnectionBaseClass();
            dsLanguageKeyword = _appBlock.ExecuteNonQuery(strQuery, "");

            if (dsLanguageKeyword != null && dsLanguageKeyword.Tables.Count > 0)
            {
                foreach (var row in dsLanguageKeyword.Tables[0].AsEnumerable())
                {
                    lstLanguageKeywordsNew.Add(Convert.ToString(row[0]));
                }
            }
            return lstLanguageKeywordsNew;
        }

        public static string[] IfAdjustmentCobol(this string[] inputArray)
        {
            List<string> adjustedStringArray = new List<string>();
            int ifIndex = 0;
            foreach (string currentLine in inputArray)
            {
                if (currentLine.Trim().StartsWith("IF"))
                {
                    adjustedStringArray.Add(currentLine);
                    ifIndex = ifIndex + 1;
                }
                else if (currentLine.Trim().EndsWith(".") || currentLine.Trim().EndsWith("END-IF"))
                {
                    if (currentLine.Trim().StartsWith("END-IF"))
                    {
                        adjustedStringArray.Add(currentLine);
                        ifIndex--;
                    }
                    else
                    {
                        if (ifIndex > 0)
                        {
                            int icCount = ifIndex;
                            for (int i = 0; i < icCount; i++)
                            {
                                if (i == 0)
                                {
                                    adjustedStringArray.Add(currentLine);
                                }
                                adjustedStringArray.Add("END-IF");
                                ifIndex--;
                            }
                        }
                        else
                        {
                            adjustedStringArray.Add(currentLine);
                        }
                    }
                }
                else if (currentLine.Trim().StartsWith("ELSE"))
                {
                    string chkPreviousStatement = string.Empty;
                    for (int i = adjustedStringArray.Count - 1; i >= 0; --i)
                    {
                        chkPreviousStatement = adjustedStringArray[i].ToString();
                        if (chkPreviousStatement.Trim().StartsWith("IF") || chkPreviousStatement.Trim().StartsWith("END-IF"))
                        {
                            adjustedStringArray.Add(currentLine);
                            break;
                        }
                        else if (chkPreviousStatement.Trim().StartsWith("ELSE"))
                        {
                            adjustedStringArray.Add("END-IF");
                            adjustedStringArray.Add(currentLine);
                            ifIndex--;
                            break;
                        }
                    }
                }
                else
                {
                    adjustedStringArray.Add(currentLine);
                }
            }

            return adjustedStringArray.ToArray();
        }

        public static List<string> GetAllMethods(this List<string> lstFileStatements)
        {
            int cntSpaces = 0;
            List<string> lstAllMethods = new List<string>();
            for (int iFileStatements = 0; iFileStatements < lstFileStatements.Count; iFileStatements++)
            {
                cntSpaces = GetLeadingWhitespaceLength(lstFileStatements[iFileStatements].ToString());
                if (cntSpaces == 1)
                {
                    //lstAllMethods.Add(lstFileStatements[iFileStatements].ToString().TrimStart().TrimEnd().Replace(" EXIT.", "").Replace(" SECTION.", "").Replace(".", "").Trim());
                    if (lstFileStatements[iFileStatements].ToString().Trim().Contains(" EXIT.") || lstFileStatements[iFileStatements].ToString().Trim().Contains(" SECTION."))
                    {
                        lstAllMethods.Add(lstFileStatements[iFileStatements].ToString().TrimStart().TrimEnd().Trim().Split(new[] { " " }, StringSplitOptions.None).FirstOrDefault().Trim().Replace(".", ""));
                    }
                    else
                    {
                        lstAllMethods.Add(lstFileStatements[iFileStatements].ToString().TrimStart().TrimEnd().Trim().Replace(".", ""));
                    }
                }
            }
            return lstAllMethods;
        }

        public static int GetLeadingWhitespaceLength(string s)
        {
            int whiteSpaceCount = 0;
            while (Char.IsWhiteSpace(s[whiteSpaceCount]))
                whiteSpaceCount++;

            return whiteSpaceCount;
        }

        public static List<string> ReplaceGoToPerformSpace(this List<string> lstFileStatements)
        {
            string strCurrentLine = string.Empty;
            List<string> performList = new List<string>();
            //var allList = (from g in lstFileStatements
            //               let m = g
            //               select m.Trim().TrimStart().TrimEnd()).ToList();

            foreach (string currentLine in lstFileStatements)
            {
                if (currentLine.Trim().StartsWith("GO TO"))
                {
                    strCurrentLine = currentLine.Trim().Replace("GO TO", "PERFORM").Replace(".", "");
                }
                else if (currentLine.Trim().StartsWith("PERFORM") && currentLine.EndsWith("."))
                {
                    strCurrentLine = currentLine.Trim().Replace(".", "");
                }
                else
                {
                    int cntSpaces = GetLeadingWhitespaceLength(currentLine.ToString());
                    if (cntSpaces == 1)
                    {
                        //strCurrentLine = currentLine.Trim().Replace(" EXIT.", "").Replace(" SECTION.", "").Replace(".", "").Trim();
                        if (currentLine.Trim().Contains(" EXIT.") || currentLine.Trim().Contains(" SECTION."))
                        {
                            strCurrentLine = currentLine.Trim().Split(new[] { " " }, StringSplitOptions.None).FirstOrDefault().Trim().Replace(".", "");
                        }
                        else
                        {
                            strCurrentLine = currentLine.Trim().Replace(".", "");
                        }
                    }
                    else
                    {
                        strCurrentLine = currentLine.Trim();
                    }
                }
                performList.Add(strCurrentLine);
            }
            var newList = performList;
            return newList;
        }

        public static string[] MethodEndLogicCobol(this string[] inputArray, List<string> lstMethods)
        {
            List<string> adjustedStringArray = new List<string>();
            int iIndex = 0;
            foreach (string currentLine in inputArray)
            {
                iIndex = iIndex + 1;
                if (lstMethods.Any(a => currentLine.StartsWith(a)))
                {
                    if (iIndex > 1)
                    {
                        adjustedStringArray.Add("END");
                        adjustedStringArray.Add(currentLine);
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
            adjustedStringArray.Add("END");
            return adjustedStringArray.ToArray();
        }

        public static List<string> SplitStatementCobolKeywords(this List<string> textLinePd)
        {

            string strCurrentString = string.Empty;
            lstLanguageKeywords.AddRange(GetLanguageKeyword());
            lstLanguageKeywords = lstLanguageKeywords.Distinct().ToList();
            Dictionary<string, string> stringArrayForSingQuoteValues = new Dictionary<string, string>();

            for (int itextLinePd = 0; itextLinePd < textLinePd.Count; itextLinePd++)
            {
                bool isApostrophePresent = false; string Constants = "";
                strCurrentString = textLinePd[itextLinePd].ToString();
                List<string> strAllConstantsFromCurrentStatement = null;

                //var results = lstLanguageKeywords.Where(i => strCurrentString.Contains(" " + i + " ")).ToList();

                var results = lstLanguageKeywords.Where(i => strCurrentString.Contains(" " + i + " ")).ToList();

                if (results.Count > 0 && (strCurrentString.ToUpper().TrimStart().StartsWith("EXEC CICS") == false)
       && (strCurrentString.Trim().ToUpper().StartsWith("EXEC SQL") == false))
                {
                    string[] delimiters = new string[results.Count];
                    for (int jRes = 0; jRes < results.Count; jRes++)
                    {
                        delimiters[jRes] = results[jRes];
                    }
                    if (delimiters.Length > 0)
                    {
                        string[] partsSplittedString = new string[delimiters.Length + 1];
                        for (int l = 0; l < delimiters.Length; l++)
                        {
                            try
                            {
                                if (l > 0)
                                {
                                    strCurrentString = partsSplittedString[l];
                                    if (strCurrentString == null)
                                    {
                                        continue;
                                    }
                                }
                                int iIndex = strCurrentString.IndexOf(delimiters[l]);
                                if (iIndex != -1)
                                {
                                    var strTemp2 = strCurrentString.Substring(iIndex);
                                    var strTemp1 = strCurrentString.Replace(strTemp2, "");
                                    partsSplittedString[l] = strTemp1;
                                    partsSplittedString[l + 1] = strTemp2;

                                }
                            }
                            catch
                            {
                            }
                        }
                        if (partsSplittedString != null && partsSplittedString.Length > 0)
                        {
                            try
                            {
                                partsSplittedString = partsSplittedString.Where(x => !string.IsNullOrEmpty(x)).ToArray();
                                partsSplittedString = partsSplittedString.Where(x => !string.IsNullOrEmpty(x.Trim())).ToArray();
                            }
                            catch { }
                            if (isApostrophePresent == true)
                            {
                                for (int iDo = 0; iDo < partsSplittedString.Length; iDo++)
                                {
                                    if (strAllConstantsFromCurrentStatement != null && strAllConstantsFromCurrentStatement.Count > 0)
                                    {
                                        for (int jCnt = 0; jCnt < strAllConstantsFromCurrentStatement.Count; jCnt++)
                                        {
                                            var value = strAllConstantsFromCurrentStatement[jCnt];
                                            if (value.Length > 0)
                                            {
                                                partsSplittedString[iDo] = partsSplittedString[iDo].Replace("XXXXTESTSXXXX" + jCnt, value);
                                            }
                                        }
                                    }
                                    else if (Constants != "")
                                    {
                                        partsSplittedString[iDo] = partsSplittedString[iDo].Replace("XXXXTESTSXXXX" + 1, Constants);
                                    }
                                }
                                Constants = "";
                                strAllConstantsFromCurrentStatement = null;
                            }
                            int leadingWhiteSpacesOfIF = 0;
                            for (int kRec = 0; kRec < partsSplittedString.Length; kRec++)
                            {
                                var strTempString = partsSplittedString[kRec];
                                if ((strTempString == null) || strTempString.Trim() == "")
                                {
                                    continue;
                                }
                                if (strTempString.Contains("DictKey") && stringArrayForSingQuoteValues.Count > 0)
                                {
                                    string output = "";
                                    for (int mRec = 0; mRec < stringArrayForSingQuoteValues.Count; mRec++)
                                    {
                                        var item = stringArrayForSingQuoteValues.ElementAt(mRec);
                                        var itemKey = item.Key;
                                        var itemValue = item.Value;

                                        if (strTempString.Contains(itemKey))
                                        {
                                            output = strTempString.Replace(itemKey, itemValue);
                                        }
                                    }
                                    if (kRec == 0)
                                    {
                                        leadingWhiteSpacesOfIF = GetLeadingWhitespaceLength(strTempString);
                                        textLinePd[itextLinePd + kRec] = output;
                                    }
                                    else
                                    {
                                        string strToInsert = "";
                                        for (int iLeadSpace = 0; iLeadSpace < leadingWhiteSpacesOfIF; iLeadSpace++)
                                        {
                                            strToInsert += " ";
                                        }
                                        strToInsert += " ";
                                        strToInsert += output;
                                        textLinePd.Insert(itextLinePd + kRec, strToInsert);
                                    }
                                }
                                else
                                {
                                    if (kRec == 0)
                                    {
                                        leadingWhiteSpacesOfIF = GetLeadingWhitespaceLength(strTempString);
                                        textLinePd[itextLinePd + kRec] = strTempString;
                                    }
                                    else
                                    {
                                        string strToInsert = "";
                                        for (int iLeadSpace = 0; iLeadSpace < leadingWhiteSpacesOfIF; iLeadSpace++)
                                        {
                                            strToInsert += " ";
                                        }
                                        strToInsert += " ";
                                        strToInsert += strTempString;
                                        textLinePd.Insert(itextLinePd + kRec, strToInsert);
                                    }
                                }
                            }
                            stringArrayForSingQuoteValues = new Dictionary<string, string>();
                        }
                    }
                }
            }

            return textLinePd;
        }

        private static readonly char[] TRIM_CHARS = { ' ', '0', '+', '\t' };

        public static List<string> Intersect(this List<string> first, string second)
        {
            if (first == null)
            {
                throw new ArgumentNullException("first");
            }
            if (second == null)
            {
                throw new ArgumentNullException("second");
            }

            var list = new List<string>();
            var set = new HashSet<string>(second.Split(TRIM_CHARS, StringSplitOptions.RemoveEmptyEntries));

            foreach (var key in first)
            {
                if (set.Remove(key))
                {
                    list.Add(key);
                }
            }
            return list;
        }
    }
}
