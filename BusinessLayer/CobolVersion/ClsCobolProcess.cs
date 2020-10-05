using BusinessLayer.DbEntities;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.Models;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace BusinessLayer.CobolVersion
{
    public class ClsCobolProcess
    {
        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator)
        {
            var lstStatementReference = new List<StatementReferenceMaster>();
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var projectName = fileMaster.ProjectMaster.ProjectName;
            var physicalPath = fileMaster.ProjectMaster.PhysicalPath;
            var classNameFile =
                fileMaster.FilePath.Replace(physicalPath + "\\", "")
                    .Replace(fileMaster.FileName, "")
                    .Replace("\\", ".");
            var classNameDeclared = projectName + "." + classNameFile + fileName;

            lstStatementReference.Add(new StatementReferenceMaster
            {

                FileId = fileMaster.FileId,
                ResolvedStatement = fileName,
                OriginalStatement = fileName,
                ClassCalled = null,
                MethodName = null,
                DataOrObjectType = null,
                MethodCalled = null,
                VariableNameDeclared = null,
                ClassNameDeclared = classNameDeclared,
                PrimaryCommandId = pClassStartIndicator,
                BaseCommandId = 19,
                ProjectId = fileMaster.ProjectId,
                SolutionId = fileMaster.SolutionId ?? 0
            });
            /*
            lstStatementReference.Add(new StatementReferenceMaster
            {
                FileId = fileMaster.FileId,
                ResolvedStatement = fileName,
                OriginalStatement = fileName,
                ClassCalled = fileName + "()",
                MethodName = fileName + "()",
                DataOrObjectType = null,
                MethodCalled = null,
                VariableNameDeclared = null,
                ClassNameDeclared = null,
                PrimaryCommandId = 23,
                BaseCommandId = 8,
                ProjectId = fileMaster.ProjectId,
                SolutionId = fileMaster.SolutionId ?? 0
            });
            */
            return lstStatementReference;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterEnd(FileMaster fileMaster,
            int pClassEndIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReference = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = null,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pClassEndIndicator,
                    BaseCommandId = 20,
                    ProjectId = fileMaster.ProjectId,
                    SolutionId = fileMaster.SolutionId ?? 0
                }
            };

            /*
            lstStatementReference.Add(new StatementReferenceMaster
            {
                FileId = fileMaster.FileId,
                ResolvedStatement = "END",
                OriginalStatement = "END",
                ClassCalled = null,
                MethodName = null,
                DataOrObjectType = null,
                MethodCalled = null,
                VariableNameDeclared = null,
                ClassNameDeclared = null,
                PrimaryCommandId = 30,
                BaseCommandId = 9,
                ProjectId = fileMaster.ProjectId,
                SolutionId = fileMaster.SolutionId ?? 0
            });
            */
            return lstStatementReference;
        }

        public StatementReferenceMaster PrepareStatementReferenceMasterMethodStart(FileMaster fileMaster, string methodStatement)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);

            var lstStatementReference = new StatementReferenceMaster
            {
                FileId = fileMaster.FileId,
                ResolvedStatement = methodStatement,
                OriginalStatement = methodStatement,
                ClassCalled = fileName + "()",
                MethodName = methodStatement + "()",
                DataOrObjectType = null,
                MethodCalled = null,
                VariableNameDeclared = null,
                ClassNameDeclared = null,
                PrimaryCommandId = 23,
                BaseCommandId = 8,
                ProjectId = fileMaster.ProjectId,
                SolutionId = fileMaster.SolutionId ?? 0

            };
            return lstStatementReference;
        }

        public StatementReferenceMaster PrepareStatementReferenceMasterMethodEnd(FileMaster fileMaster)
        {
            var lstStatementReference = new StatementReferenceMaster
            {
                FileId = fileMaster.FileId,
                ResolvedStatement = "END",
                OriginalStatement = "END",
                ClassCalled = null,
                MethodName = null,
                DataOrObjectType = null,
                MethodCalled = null,
                VariableNameDeclared = null,
                ClassNameDeclared = null,
                PrimaryCommandId = 30,
                BaseCommandId = 9,
                ProjectId = fileMaster.ProjectId,
                SolutionId = fileMaster.SolutionId ?? 0
            };
            return lstStatementReference;
        }
        public Dictionary<string, List<string>> GetListFromCertainPoint(List<string> allLines, List<string> methodList)
        {
            var copyOfAllLines = new List<string>();
            foreach (var line in allLines)
            {
                if (line.Contains("STOP RUN") || line.Contains("STOP RUN."))
                {
                    string thisLine = line.CheckCommentInStatement();
                    copyOfAllLines.Add(thisLine);
                    continue;
                }
                copyOfAllLines.Add(line);
            }

            List<string> extractedLines;
            var methodBlockDictionary = new Dictionary<string, List<string>>();
            var stopLinesCount = copyOfAllLines.Count(l => l == "STOP RUN" || l == "STOP RUN.");
            if (stopLinesCount > 0)
            {
                var upToStopBlock = GetListUpToLastStop(copyOfAllLines);
                extractedLines = copyOfAllLines.GetRange(upToStopBlock.Count, copyOfAllLines.Count - upToStopBlock.Count);
                methodBlockDictionary.Add("STOP", upToStopBlock);
            }
            else
            {
                extractedLines = copyOfAllLines.ToList();
            }

            foreach (var goSub in methodList.ToList())
            {
                if (goSub.ContainsAll(";", "*")) continue;
                var methodBlock = PickUpAllMethodBlocks(extractedLines, goSub, methodList, "END");
                methodBlock.Insert(1, goSub);
                var caseStmtCount = methodBlock.Count(s => s == "BEGIN CASE");
                if (caseStmtCount >= 1)
                {
                    var simplifiedWithCaseBlocks = methodBlock.SimplifyCaseAndNestedCaseStatementsBlock();
                    methodBlockDictionary.Add(goSub, simplifiedWithCaseBlocks);
                    continue;
                }
                var listBlock = methodBlock.ReModifyMethodBlockForIfWithMatchingEndIf();
                methodBlockDictionary.Add(goSub, listBlock);

                //continue;
                //methodBlockDictionary.Add(goSub, methodBlock);
            }
            return methodBlockDictionary;
        }

        public List<string> PickUpAllMethodBlocks(List<string> inputList, string startingPoint,
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

            var goUpBy4Lines = index - 4;
            var methodComments = string.Empty;
            for (var length = index; length >= 0; length--)
            {
                var line = inputList[length];
                if (length == goUpBy4Lines)
                {
                    if (methodComments == string.Empty) methodComments = null;
                    break;
                }

                if (!line.StartsWith("* ")) continue;
                methodComments = line.Split('*').LastOrDefault();
                break;
            }
            outputList.Add(methodComments);

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
            return outputList;
        }

        public static List<string> GetListUpToLastStop(List<string> allLines)
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

            var stopLinesCount = allLines.Count(l => l == "STOP RUN" || l == "STOP RUN.");
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
            int stopCounts = allLines.Count(s => s == "STOP RUN" || s == "STOP RUN.");
            Console.WriteLine("STOP's Counts: " + stopCounts);

            int sCounts = 0;
            for (int i = 0; i < allLines.Count; i++)
            {
                if (allLines[i] == "STOP RUN" || allLines[i] == "STOP RUN.") sCounts = i;
            }
            var setOfLines = allLines.GetRange(0, sCounts + 1);

            if (stopCounts == 1)
                return setOfLines;


            var linesSubset = new List<string>();
            allLines.Reverse();
            var indexPos = -1;

            foreach (var line in allLines)
            {
                indexPos++;
                if (line != "STOP RUN" || line != "STOP RUN.") continue;

                var range = allLines.GetRange(indexPos, allLines.Count - indexPos);
                linesSubset.AddRange(range);
                linesSubset.Reverse();
                allLines.Reverse();
                break;
            }
            return linesSubset;
        }

        public FileMaster GetExternalCallFiles(string oStatement, int fileExtensionTypeId, List<FileMaster> copyOfFileMaster)
        {
            try
            {
                const string regexProc = @"(EXEC\s*PROC\s*=)";
                const string regexPgm = @"(EXEC\s*PGM\s*=)";
                const string regexPgmFile = @"(EXEC\s*PGM\s*=(\s*[A-z0-9]+))";
                const string regexPattern = @"EXEC(\s*[A-z0-9]+)";
                if (Regex.IsMatch(oStatement, regexProc))
                {
                    const string regexProcFile = @"(EXEC\s*PROC\s*=(\s*[A-z0-9]+))";
                    var matches = Regex.Match(oStatement, regexProcFile);
                    string pgmName = matches.Groups[2].Value;
                    if (string.IsNullOrEmpty(pgmName)) return null;
                    string objectName = pgmName.Trim();
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => f.FileName.ToUpper().StartsWith(objectName)
                                  && f.FileTypeExtensionId == 8);
                    if (!anyFile) return null;
                    var programFilePath = copyOfFileMaster.ToList()
                        .Find(f => f.FileName.ToUpper().StartsWith(objectName)
                                   && f.FileTypeExtensionId == 8);
                    return programFilePath;
                }

                if (Regex.IsMatch(oStatement, regexPgm))
                {
                    var matchesP = Regex.Match(oStatement, regexPgmFile);
                    string pgmNameP = matchesP.Groups[2].Value;

                    if (string.IsNullOrEmpty(pgmNameP)) return null;
                    string objName = pgmNameP.Trim();
                    var anyFileP = copyOfFileMaster.ToList()
                        .Any(f => f.FileName.ToUpper().StartsWith(objName)
                                  && f.FileTypeExtensionId == 6);
                    if (!anyFileP) return null;
                    var programFilePathP = copyOfFileMaster.ToList()
                        .Find(f => f.FileName.ToUpper().StartsWith(objName)
                                   && f.FileTypeExtensionId == 6);
                    return programFilePathP;
                }

                if (!Regex.IsMatch(oStatement, regexPattern)) return null;

                var match = Regex.Match(oStatement, regexPattern);
                var pName = match.Groups[1].Value;
                if (string.IsNullOrEmpty(pName)) return null;
                string pObjName = pName.Trim();
                if (fileExtensionTypeId == 7)
                {
                    if (string.IsNullOrEmpty(pObjName)) return null;
                    var anyFile = copyOfFileMaster
                        .Any(f => f.FileName.ToUpper().StartsWith(pObjName)
                                  && f.FileTypeExtensionId == 8);
                    if (!anyFile) return null;
                    var programFilePath = copyOfFileMaster
                        .Find(f => f.FileName.ToUpper().StartsWith(pObjName)
                                   && f.FileTypeExtensionId == 8);
                    return programFilePath;
                }

                if (fileExtensionTypeId != 8) return null;

                if (string.IsNullOrEmpty(pObjName)) return null;
                var pFile = copyOfFileMaster
                    .Any(f => f.FileName.ToUpper().StartsWith(pObjName)
                              && f.FileTypeExtensionId == 6);
                if (!pFile) return null;
                var pFilePath = copyOfFileMaster
                    .Find(f => f.FileName.ToUpper().StartsWith(pObjName)
                               && f.FileTypeExtensionId == 6);
                return pFilePath;
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return null;
            }
        }

        public FileMaster GetCurrentFile(string objectName, int fileExtensionId, List<FileMaster> copyOfFileMaster)
        {
            if (string.IsNullOrEmpty(objectName)) return null;
            var anyFileP = copyOfFileMaster
                .Any(f => f.FileName.ToUpper().StartsWith(objectName)
                          && f.FileTypeExtensionId == fileExtensionId);
            if (!anyFileP) return null;
            var programFilePathP = copyOfFileMaster
                .Find(f => f.FileName.ToUpper().StartsWith(objectName)
                           && f.FileTypeExtensionId == fileExtensionId);
            return programFilePathP;
        }

        public FileMaster GetExternalCallFiles(string oStatement, int fileExtensionTypeId, 
            List<FileMaster> copyOfFileMaster, out NameValue nameValue)
        {
            nameValue = new NameValue();
            string calledObjectName;
            const string regexProc = @"(EXEC\s*PROC\s*=)";
            const string regexPgm = @"(EXEC\s*PGM\s*=)";
            const string regexPgmFile = @"(EXEC\s*PGM\s*=(\s*[A-z0-9\&]+))";
            const string regexPattern = @"EXEC(\s*[A-z0-9]+)";
            var regexCall = new Regex(@"^CALL\s+(.+?\s+)", RegexOptions.IgnoreCase);

            var regexCopy = new Regex(@"^COPY\s+(.+?\s+)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
            var regexInputPatteren = new Regex(@"INPUTLIB\(([^)]*)\)",
                RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
            var callExtRegExScreen = new Regex(@"SEND\s+MAP.*\(\'(?<ObjName>[A-z0-9\s]+)\'\)|RECEIVE\s+MAP.*\(\'(?<ObjName>[A-z0-9\s]+)\'\)", RegexOptions.IgnoreCase);
            var callExtRegExProgram = new Regex(@"XCTL\s+PROGRAM.*\(\'(?<ObjName>[A-z0-9\s]+)\'\)", RegexOptions.IgnoreCase);
            // var regexRunProgram = new Regex(@"RUN\s*PROGRAM\s*\((.*?)\)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

            var regexInput = new Regex(@"INPUTLIB\(([^)]*)\)|RUN\s*PROGRAM\s*\((.*?)\)",
                RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

            var regexIncludeMember = new Regex(@"^INCLUDE\s+MEMBER\s*=(\s*[A-z0-9]+)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

            if (regexIncludeMember.IsMatch(oStatement))
            {
                var includeMemberMatch = regexIncludeMember.Match(oStatement);
                var pgmName = includeMemberMatch.Groups[1].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                var objectName = pgmName.Trim().ToUpper();
                int fileTypeExtensionId = 8;
                calledObjectName = objectName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Proc";
                var fileMaster = GetCurrentFile(objectName, fileTypeExtensionId, copyOfFileMaster);
                return fileMaster;
            }

            if (regexInput.IsMatch(oStatement))
            {
                int fileTypeExtensionId = regexInputPatteren.IsMatch(oStatement) ? 19 : 6;
                var matchInputRunPrg = regexInput.Match(oStatement);
                string inputLib = matchInputRunPrg.Groups[1].Value;
                string runProgram = matchInputRunPrg.Groups[2].Value;
                var pgmName = !string.IsNullOrEmpty(inputLib) ? inputLib : runProgram;
                if (string.IsNullOrEmpty(pgmName)) return null;
                var objectName = pgmName.Trim();
                calledObjectName = objectName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = !string.IsNullOrEmpty(inputLib) ? "InputLib" : "Cobol";
                var fileMaster = GetCurrentFile(objectName, fileTypeExtensionId, copyOfFileMaster);
                return fileMaster;
            }
            if (regexCall.IsMatch(oStatement))
            {
                var callMatch = regexCall.Match(oStatement);
                var pgmName = callMatch.Groups[1].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                var objectName = pgmName.Trim().TrimStart('\'', '\"').TrimEnd('\'', '\"').Trim();
                calledObjectName = objectName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Cobol";
                var anyFile = copyOfFileMaster.Any(f => Path.GetFileNameWithoutExtension(f.FilePath) == objectName.ToUpper()
                                                        || Path.GetFileNameWithoutExtension(f.FilePath) == objectName.ToLower());
                if (!anyFile) return null;
                var fileMaster = copyOfFileMaster.Find(f => f.FileName.StartsWith(objectName, true, CultureInfo.CurrentCulture)
                                                            && f.FileTypeExtensionId == 6);
                return fileMaster;
            }
            if (regexCopy.IsMatch(oStatement) && !Regex.IsMatch(oStatement, regexPgm))
            {
                var copyMatch = regexCopy.Match(oStatement);
                var pgmName = copyMatch.Groups[1].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                var objName = pgmName.Trim();
                calledObjectName = objName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Cobol";
                var anyFile = copyOfFileMaster.Any(f => Path.GetFileNameWithoutExtension(f.FilePath) == objName.ToUpper()
                                                        || Path.GetFileNameWithoutExtension(f.FilePath) == objName.ToLower());
                if (!anyFile) return null;
                var fileMaster = copyOfFileMaster.Find(f => f.FileName.StartsWith(objName, true, CultureInfo.CurrentCulture)
                                                            && f.FileTypeExtensionId == 4);
                return fileMaster;
            }
            if (callExtRegExScreen.IsMatch(oStatement) || callExtRegExProgram.IsMatch(oStatement))
            {
                var copyMatch = callExtRegExScreen.IsMatch(oStatement) ? callExtRegExScreen.Match(oStatement) : callExtRegExProgram.Match(oStatement);
                int fteId = callExtRegExScreen.IsMatch(oStatement) ? 20 : 6;
                var pgmName = copyMatch.Groups["ObjName"].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                var objName = pgmName.Trim();
                calledObjectName = objName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Cobol";
                var anyFile = copyOfFileMaster.Any(f => Path.GetFileNameWithoutExtension(f.FilePath) == objName.ToUpper()
                    || Path.GetFileNameWithoutExtension(f.FilePath) == objName.ToLower());
                if (!anyFile) return null;
                var fileMaster = copyOfFileMaster.Find(f => f.FileName.StartsWith(objName, true, CultureInfo.CurrentCulture)
                                                            && f.FileTypeExtensionId == fteId);
                return fileMaster;
            }

            if (Regex.IsMatch(oStatement, regexProc))
            {
                const string regexProcFile = @"(EXEC\s*PROC\s*=(\s*[A-z0-9]+))";
                var matches = Regex.Match(oStatement, regexProcFile);
                string pgmName = matches.Groups[2].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                string objectName = pgmName.Trim().ToUpper();
                calledObjectName = pgmName;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Proc";
                var anyFile = copyOfFileMaster.ToList()
                    .Any(f => f.FileName.ToUpper().StartsWith(objectName)
                              && f.FileTypeExtensionId == 8);
                if (!anyFile) return null;
                var programFilePath = copyOfFileMaster.ToList()
                    .Find(f => f.FileName.ToUpper().StartsWith(objectName)
                               && f.FileTypeExtensionId == 8);
                return programFilePath;
            }

            if (Regex.IsMatch(oStatement, regexPgm))
            {
                var matchesP = Regex.Match(oStatement, regexPgmFile);
                string pgmNameP = matchesP.Groups[2].Value;
                if (string.IsNullOrEmpty(pgmNameP)) return null;
                string objName = pgmNameP.Trim().ToUpper().TrimStart('&').TrimEnd('&');
                calledObjectName = pgmNameP;
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Cobol";
                var anyFileP = copyOfFileMaster.ToList()
                    .Any(f => f.FileName.ToUpper().StartsWith(objName)
                              && f.FileTypeExtensionId == 6);
                if (!anyFileP) return null;
                var programFilePathP = copyOfFileMaster.ToList()
                    .Find(f => f.FileName.ToUpper().StartsWith(objName)
                               && f.FileTypeExtensionId == 6);
                return programFilePathP;
            }

            if (!Regex.IsMatch(oStatement, regexPattern)) return null;

            var match = Regex.Match(oStatement, regexPattern);
            var pName = match.Groups[1].Value;
            if (string.IsNullOrEmpty(pName)) return null;
            string pObjName = pName.Trim().ToUpper();
            calledObjectName = pObjName;
            if (fileExtensionTypeId == 7)
            {
                nameValue.CalledObjectName = calledObjectName;
                nameValue.CalledObjectType = "Proc";
                var anyFile = copyOfFileMaster
                      .Any(f => f.FileName.ToUpper().StartsWith(pObjName)
                                && f.FileTypeExtensionId == 8);
                if (!anyFile) return null;
                var programFilePath = copyOfFileMaster
                    .Find(f => f.FileName.ToUpper().StartsWith(pObjName)
                               && f.FileTypeExtensionId == 8);
                return programFilePath;
            }

            if (fileExtensionTypeId != 8) return null;

            nameValue.CalledObjectName = calledObjectName;
            nameValue.CalledObjectType = "Cobol";
            var pFile = copyOfFileMaster
                .Any(f => f.FileName.ToUpper().StartsWith(pObjName)
                          && f.FileTypeExtensionId == 6);
            if (!pFile) return null;
            var pFilePath = copyOfFileMaster
                .Find(f => f.FileName.ToUpper().StartsWith(pObjName)
                           && f.FileTypeExtensionId == 6);
            return pFilePath;
        }

        public FileMaster GetExternalCallFilesWithoutFileType(string oStatement, int fileExtensionTypeId, List<FileMaster> copyOfFileMaster)
        {
            try
            {
                var regexPattern = @"EXEC(\s*[A-z0-9]+)";  // @"EXEC\s+(.*)";
                var match = Regex.Match(oStatement, regexPattern);
                var pgmName = match.Groups[1].Value;
                if (string.IsNullOrEmpty(pgmName)) return null;
                string objectName = pgmName.Trim();
                if (fileExtensionTypeId == 7)
                {
                    if (string.IsNullOrEmpty(objectName)) return null;
                    var anyFile = copyOfFileMaster
                        .Any(f => f.FileName.ToUpper().StartsWith(objectName)
                                  && f.FileTypeExtensionId == 8);
                    if (!anyFile) return null;
                    var programFilePath = copyOfFileMaster
                        .Find(f => f.FileName.ToUpper().StartsWith(objectName)
                                   && f.FileTypeExtensionId == 8);
                    return programFilePath;
                }

                if (fileExtensionTypeId != 8) return null;

                if (string.IsNullOrEmpty(objectName)) return null;
                var anyFileP = copyOfFileMaster
                    .Any(f => f.FileName.ToUpper().StartsWith(objectName)
                              && f.FileTypeExtensionId == 6);
                if (!anyFileP) return null;
                var programFilePathP = copyOfFileMaster
                    .Find(f => f.FileName.ToUpper().StartsWith(objectName)
                               && f.FileTypeExtensionId == 6);
                return programFilePathP;
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return null;
            }
        }
    }
}
