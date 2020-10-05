using System.Collections.Generic;
using System.IO;
using System.Linq;
using BusinessLayer.DbEntities;
using BusinessLayer.ExtensionLibrary;

namespace BusinessLayer.UniverseBasic
{
    public class UniverseBasicVersion2
    {
        public List<StatementReferenceMaster> PrepareJclFile(int fileId, List<FileMaster> fileMaster,
            List<string> inputList,
            int callExtRefId, string projectName, string pPath, params string[] lineStartString)
        {
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>();
            var classNameDeclared = "";
            foreach (var input in inputList)
            {
                if (input.StartsWith("RUN"))
                {
                    var pgName = input.Split(' ').LastOrDefault();

                    var pName = fileMaster.ToList().Where(f => !string.IsNullOrEmpty(pgName)
                                                               && f.FileName.StartsWith(pgName) &&
                                                               (f.FileTypeExtensionId == 9)).ToList();
                    if (!pName.Any()) continue;

                    var className = pName[0].FilePath.Replace(pPath + "\\", "")
                        .Replace(pName[0].FileName, "").Replace("\\", ".");
                    var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                    // Use it later...
                    classNameDeclared = projectName + "." + className + fileName;
                }

                if (lineStartString.Any(l => input.StartsWith(l + " ")))
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = classNameDeclared,
                        MethodName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        ClassNameDeclared = null,
                        PrimaryCommandId = callExtRefId,
                        BaseCommandId = 6
                    });
                else
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        ClassNameDeclared = null,
                        PrimaryCommandId = 0,
                        BaseCommandId = 0
                    });
            }

            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
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
                    PrimaryCommandId = pClassStartIndicator,
                    BaseCommandId = 19,
                    ProjectId = fileMaster.ProjectId
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterEnd(FileMaster fileMaster,
            int pClassEndIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
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
                    ProjectId = fileMaster.ProjectId
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator, int pMethodStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pClassStartIndicator,
                    BaseCommandId = 19
                },
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pMethodStartIndicator,
                    BaseCommandId = 8
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterEnd(FileMaster fileMaster,
            int pClassEndIndicator, int pMethodEndIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pMethodEndIndicator,
                    BaseCommandId = 9
                },
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pClassEndIndicator,
                    BaseCommandId = 20
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<string> CheckIfSatetment(string input)
        {
            bool tOrF;
            return input.ModifyEndWithThenOrElseOrLockedStatement(out tOrF);
        }

        public List<string> SimplifyCaseAndNestedCaseStatementsBlock(List<string> caseStatementBlock)
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
                    var caseBlock = caseStatementBlock.
                        GetBlockOfLinesAsList(indexPosition, "BEGIN CASE", "END CASE");

                    var list = caseBlock.ExtractCaseStatementBlock();
                    var check = list.VerifyMethodBlockForIfWithMatchingEndIf();
                    if (check)
                    {
                        copyOfCaseStatements.RemoveRange(indexPosition, caseBlock.Count);
                        copyOfCaseStatements.InsertRange(indexPosition, list);
                    }
                    else
                    {
                        var thisList = list.ReModifyMethodBlockForIfWithMatchingEndIf();
                        //bool recheckThis = thisList.VerifyMethodBlockForIfWithMatchingEndIf();
                        //if (!recheckThis) continue;
                        copyOfCaseStatements.RemoveRange(indexPosition, caseBlock.Count);
                        copyOfCaseStatements.InsertRange(indexPosition, thisList);
                    }
                }
            }

            var listNew = copyOfCaseStatements.ExtractCaseStatementBlock();
            var checkNew = listNew.VerifyMethodBlockForIfWithMatchingEndIf();
            if (checkNew)
            {
                copyOfCaseStatements = listNew;
                //copyOfCaseStatements.Add("==============================");
                //copyOfCaseStatements.Add("IF Counts: " + copyOfCaseStatements.Count(s => s.StartsWith("IF ")));
                //copyOfCaseStatements.Add("END IF Counts: " + copyOfCaseStatements.Count(s => s == "END-IF" || s == "END" || s == "END IF"));

                return copyOfCaseStatements;
            }

            var newList = listNew.ReModifyMethodBlockForIfWithMatchingEndIf();
            var recheck = newList.VerifyMethodBlockForIfWithMatchingEndIf();
            if (!recheck) return newList;

            copyOfCaseStatements = newList;

            //copyOfCaseStatements.Add("==============================");
            //copyOfCaseStatements.Add("IF Counts: " + copyOfCaseStatements.Count(s => s.StartsWith("IF ")));
            //copyOfCaseStatements.Add("END IF Counts: " + copyOfCaseStatements.Count(s => s == "END-IF" || s == "END" || s == "END IF"));

            return copyOfCaseStatements;
        }

        public Dictionary<string, List<string>> GetListFromCertainPoint(List<string> allLines, List<string> lstAllGoSubs)
        {
            /*
            int indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;

                if (line.StartsWith("STOP"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }
                if (line.StartsWith("* BEGIN MAINLINE"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }

                if (line.StartsWith("* MAINLINE"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }
                if (!lstAllGoSubs.Any(l => line.StartsWith(l))) continue;

                var getRangeNew = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                extractedLines.AddRange(getRangeNew);
                //return Ok(extractedLines);
                break;
            }
            */
            List<string> extractedLines;
            var stopLinesCount = allLines.Count(l => l == "STOP");
            if (stopLinesCount > 0)
            {
                var upToStopBlock = allLines.GetListUpToLastStop();
                extractedLines = allLines.GetRange(upToStopBlock.Count, allLines.Count - upToStopBlock.Count);
            }
            else
            {
                extractedLines = allLines.ToList();
            }

            var methodBlockDictionary = new Dictionary<string, List<string>>();

            foreach (var goSub in lstAllGoSubs.Distinct().ToList())
            {
                var methodBlock = PickUpAllMethodBlocks(extractedLines, goSub, lstAllGoSubs, "RETURN", "STOP");
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

        public Dictionary<string, List<string>> GetListFromCertainPointInclude(List<string> allLines,
            List<string> lstAllGoSubs)
        {
            var methodBlockDictionary = new Dictionary<string, List<string>>();

            foreach (var goSub in lstAllGoSubs.Distinct().ToList())
            {
                var methodBlock = PickUpAllMethodBlocks(allLines, goSub, lstAllGoSubs, "RETURN");
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
    }
}