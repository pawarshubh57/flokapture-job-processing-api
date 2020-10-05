using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using BusinessLayer.DbEntities;

namespace BusinessLayer.ExtensionLibrary
{
    public static class UniverseStatementExtensions
    {
        public static List<string> LstStatements = new List<string> { "LOCATE " };
        public static RegexOptions RegexOptions = RegexOptions.IgnoreCase;

        // This list is having THEN or ELSE at end or in between statements...
        public static List<Regex> LstRegExSimple = new List<Regex>
        {
            new Regex(@"^Open\s", RegexOptions),
            new Regex(@"^RollBack\s", RegexOptions),
            new Regex(@"^Write\s|^Read\s|^WriteVu\s|^WriteU\s|^WriteV\s|^WriteBlk\s|^WriteSeqF\s|^WriteT\s", RegexOptions),
            new Regex(@"^DeleteU\s", RegexOptions),
            new Regex(@"^Weof\s", RegexOptions),
            new Regex(@"^FindStr\s", RegexOptions),
            new Regex(@"^OpenDev\s", RegexOptions),
            new Regex(@"^Rewind\s", RegexOptions),
            new Regex(@"^Locate\s", RegexOptions),
            new Regex(@"^Lock\s", RegexOptions),
            new Regex(@"^MatRead\s|^MatReadL\s|^MatReadU\s|^MatWrite\s^MatWriteU\s|^MatWriteL\s", RegexOptions),
            new Regex(@"^NoBuf", RegexOptions),
            new Regex(@"^OpenCheck\s", RegexOptions),
            new Regex(@"^OpenDev\s", RegexOptions),
            new Regex(@"^OpenPath\s", RegexOptions),
            new Regex(@"^ProcRead\s|^ReadL\s|^ReadU\s|^ReadV\s|^ReadVL\s|^ReadVU\s|^ReadBlk\s|^ReadList\s|^ReadNext\s|^ReadT\s", RegexOptions),
            new Regex(@"^Seek\s|^Send\s|^Status\s", RegexOptions),
            new Regex(@"^Transaction Start\s|^Transaction End\s|^Transaction Commit\s", RegexOptions),
            new Regex(@"^TtyCtl\s|^TtyGet\s|^TtySet\s", RegexOptions),
            new Regex(@"^OpenSeq\s", RegexOptions),
            new Regex(@"^ReadSeq\s", RegexOptions),
            new Regex(@"^WriteSeq\s", RegexOptions),
            new Regex(@"^GetList\s", RegexOptions)
        };

        /*
        // This list is having LOCKED, ELSE, THEN, END THEN, END ELSE like statement in block...
        public static List<Regex> LstRegExComplex = new List<Regex>
        {
            new Regex(@"^OpenSeq", RegexOptions),
            new Regex(@"^ReadSeq", RegexOptions),
            new Regex(@"^WriteSeq", RegexOptions)
        };
        */

        public static List<string> ProcessForStatementsConversion(this IEnumerable<string> lstFileLines, FileMaster fileMaster)
        {
            var copyOfLines = new List<string>();
            var lockedRegEx = new Regex(@"(.*\sLOCKED\s)(.*)", RegexOptions.IgnoreCase);
            var endWithLockedRegEx = new Regex(@"\s+LOCKED$", RegexOptions.IgnoreCase);
            var thenRegEx = new Regex(@"(.*\sTHEN)(.*)", RegexOptions.IgnoreCase);
            var allFileLines = lstFileLines.ToList();  // File.ReadAllLines(filePath).ToList();
            allFileLines.RemoveAll(s => s.Length <= 0);
            allFileLines = allFileLines.Select(s => s.Trim()).ToList();
            allFileLines = allFileLines.CombineAllBrokenLines('_');
            var lstConvertedLines = new List<string>();
            int loopCounter = -1;
            foreach (var line in allFileLines)
            {
                loopCounter++;
                string thisLine = line.Trim();
                if (loopCounter == 0) { copyOfLines.Add(thisLine); continue; }
                if (thisLine.StartsWith("*") || string.IsNullOrEmpty(thisLine)) continue;

                if (endWithLockedRegEx.IsMatch(thisLine))
                {
                    string witoutComments = thisLine.CheckCommentInStatement();
                    if (!endWithLockedRegEx.IsMatch(witoutComments))
                    {
                        copyOfLines.Add(thisLine);
                        continue;
                    }
                    copyOfLines.Add(thisLine);
                    copyOfLines.Add("IF NOT SUCCESS THEN");
                    continue;
                }
                if (thisLine.StartsWith("IF ", true, CultureInfo.CurrentCulture) &&
                    !thisLine.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                {
                    string withoutComments = thisLine.CheckCommentInStatement();
                    if (withoutComments.StartsWith("IF ", true, CultureInfo.CurrentCulture) &&
                        withoutComments.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                    {
                        copyOfLines.Add(thisLine);
                        continue;
                    }
                }
                if (thisLine.StartsWith("IF ", true, CultureInfo.CurrentCulture) &&
                    !thisLine.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                {
                    thisLine = thisLine.CheckCommentInStatement();
                    if (thisLine.StartsWith("IF ", true, CultureInfo.CurrentCulture)
                        && thisLine.EndsWith(" ELSE", true, CultureInfo.CurrentCulture))
                    {
                        string ifPart = thisLine.Replace(" ELSE", " THEN");
                        copyOfLines.Add(ifPart);
                        copyOfLines.Add("END ELSE");
                        continue;
                    }

                    if (thenRegEx.IsMatch(thisLine))
                    {
                        var matches = thenRegEx.Match(thisLine);

                        int loopCnt = -1;
                        foreach (Group group in matches.Groups)
                        {
                            loopCnt++;
                            if (loopCnt <= 0) continue;
                            if (string.IsNullOrEmpty(group.Value)) continue;
                            string groupValue = group.Value.Trim();
                            copyOfLines.Add(groupValue);
                        }

                        copyOfLines.Add("END");
                        continue;
                    }
                    copyOfLines.Add(thisLine);
                    continue;
                }

                if (!lockedRegEx.IsMatch(thisLine))
                {
                    copyOfLines.Add(thisLine);
                    continue;
                }
                string newLine = thisLine.CheckCommentInStatement();
                var newLines = newLine.CheckForLocked();
                copyOfLines.AddRange(newLines);
            }

            loopCounter = -1;
            foreach (var line in copyOfLines)
            {
                loopCounter++;
                string thisLine = line.Trim();
                if (loopCounter == 0) { lstConvertedLines.Add(thisLine); continue; }
                if (thisLine.StartsWith("*")) continue;
                var lstLines = thisLine.CheckForConversion();
                lstConvertedLines.AddRange(lstLines);
            }
            return lstConvertedLines;
        }

        /// <summary>
        /// <para>This method is to check various patterns of statement starting with Read, MatRead, Open.</para>
        /// <para>If found such statement, then it processes it and returns list of converted statements</para>
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
        public static List<string> CheckForConversion(this string input)
        {
            string tempString = input;
            if (string.IsNullOrEmpty(tempString)) return new List<string>();
            // int inputLen = tempString.Length;
            // Check here if statement == "END THEN"
            if (Regex.IsMatch(input, @"^END\s+THEN$", RegexOptions))
            {
                var newStatements = new List<string> { "END-IF", "IF SUCCESS THEN" };
                return newStatements;
            }
            if (input.StartsWith("END ELSE") && !input.EndsWith("END ELSE"))
            {
                string withoutComments = input.CheckCommentInStatement();
                if (withoutComments.StartsWith("END ELSE") && withoutComments.EndsWith("END ELSE"))
                {
                    var elseStatement = new List<string> { withoutComments };
                    return elseStatement;
                }

                var newStatements = new List<string> { "END ELSE" };
                string restStatement = input.Replace("END ELSE", "").Trim();
                newStatements.Add(restStatement);
                newStatements.Add("END");
                return newStatements;
            }
            // Here are 3 cases:
            // 1. Input statement can contain THEN at end or in between...
            // 2. Input statement can contain ELSE at end or in between...
            // 3. Input statement can contain LOCKED at end or in between...
            // Case 1.

            if (!LstRegExSimple.Any(reg => reg.IsMatch(tempString))) return new List<string> { input };
            var statements = ProcessStatement(tempString);
            return statements;
        }

        private static List<string> ProcessStatement(string statement)
        {
            var statementBlock = new List<string>();
            if (Regex.IsMatch(statement, @"\sELSE$", RegexOptions))
            {
                string stmt = Regex.Replace(statement, @"\sELSE$", "", RegexOptions);
                statementBlock.Add(stmt);
                statementBlock.Add("IF NOT-SUCCESS THEN");
                return statementBlock;
            }
            if (Regex.IsMatch(statement, @"\sTHEN$", RegexOptions))
            {
                string stmt = Regex.Replace(statement, @"\sTHEN$", "", RegexOptions);
                statementBlock.Add(stmt);
                statementBlock.Add("IF SUCCESS THEN");
                return statementBlock;
            }

            var endWithLockedRegEx = new Regex(@"\s+LOCKED$", RegexOptions);
            if (endWithLockedRegEx.IsMatch(statement))
                return new List<string> { statement };

            var elseRegEx = new Regex(@"(.*)(\sELSE\s)(.*)", RegexOptions);
            if (!elseRegEx.IsMatch(statement)) return new List<string>() { statement };

            var match = elseRegEx.Match(statement);
            int loopCnt = -1;
            foreach (Group group in match.Groups)
            {
                loopCnt++;
                if (loopCnt == 0) continue;
                if (string.IsNullOrEmpty(group.Value)) continue;
                string groupValue = group.Value.Trim();
                statementBlock.Add(groupValue);
                if (loopCnt > 1) continue;
                statementBlock.Add("IF SUCCESS THEN");
            }
            statementBlock.Add("END");
            return statementBlock;
        }

        public static List<string> CheckForLocked(this string input)
        {
            if (!LstRegExSimple.Any(reg => reg.IsMatch(input))) return new List<string> { input };
            var lockedRegEx = new Regex(@"(.*\sLOCKED\s)(.*)", RegexOptions.IgnoreCase);
            if (!lockedRegEx.IsMatch(input)
                || input.EndsWith(" LOCKED", true, CultureInfo.InvariantCulture)) return new List<string>();

            var newLines = new List<string>();
            var match = lockedRegEx.Match(input);
            int loopCnt = -1;
            foreach (Group group in match.Groups)
            {
                loopCnt++;
                if (loopCnt <= 0) continue;
                if (string.IsNullOrEmpty(group.Value)) continue;
                string groupValue = group.Value.Trim();
                if (groupValue.EndsWith(" LOCKED", true, CultureInfo.CurrentCulture))
                {
                    newLines.Add("READ RECORD USING " + groupValue);
                }
                else if (groupValue.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                {
                    newLines.Add("IF " + groupValue);
                }
                else if (groupValue.EndsWith(" ELSE", true, CultureInfo.CurrentCulture))
                {
                    newLines.Add("IF SUCCESS THEN");
                    string ifPart = Regex.Replace(groupValue, @"\sELSE", "", RegexOptions.IgnoreCase);
                    newLines.Add(ifPart);
                    newLines.Add("ELSE");
                }
            }
            return newLines;
        }
    }
}
