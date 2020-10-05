using System.Collections.Generic;
using System.Data.Entity;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using Newtonsoft.Json;

namespace BusinessLayer.UniverseBasic
{
    public class UniverseBasicVersion1
    {
        private readonly AppDbContext _appDbContext = new AppDbContext();

        public List<StatementReferenceMaster> PrepareJclFile(int fileId, List<FileMaster> fileMaster,
            List<string> inputList, int callExtRefId, params string[] lineStartString)
        {
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>();

            foreach (var input in inputList)
            {
                var classNameDeclared = "";
                if (input.StartsWith("*")) continue;
                if (lineStartString.Any(l => input.StartsWith(l + " ")))
                {
                    var pgName = input.Split(' ').LastOrDefault();
                    pgName = !string.IsNullOrEmpty(pgName) ? pgName.Trim() : "";
                    var pName = fileMaster.ToList().Where(f => !string.IsNullOrEmpty(pgName)
                                                               && Path.GetFileNameWithoutExtension(f.FilePath) ==
                                                               pgName && (f.FileTypeExtensionId == 9)).ToList();
                    if (pName.Any())
                    {
                        int projectId = pName[0].ProjectId;
                        var projectDetatils = _appDbContext.ProjectMaster
                            .Where(p => p.ProjectId == projectId).ToList();
                        if (projectDetatils.Any())
                        {
                            var pNameNew = projectDetatils[0].ProjectName;
                            var pPathNew = projectDetatils[0].PhysicalPath;

                            var className = pName[0].FilePath.Replace(pPathNew + "\\", "")
                                .Replace(pName[0].FileName, "").Replace("\\", ".");
                            var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                            // Use it later...
                            classNameDeclared = pNameNew + "." + className + fileName;
                        }
                    }
                }

                if (lineStartString.Any(l => input.StartsWith(l + " ")))
                {
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
                    continue;
                }
                if (lineStartString.Any(l => input.StartsWith("If ") || input.StartsWith("IF ")))
                {
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
                        PrimaryCommandId = 33,
                        BaseCommandId = 1
                    });
                    continue;
                }
                if (lineStartString.Any(l => input == "END" || input == "END-IF" || input == "END IF"))
                {
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
                        PrimaryCommandId = 34,
                        BaseCommandId = 2
                    });
                    continue;
                }
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

        public List<StatementReferenceMaster> PrepareParseJclFile(int fileId, int languageId, List<FileMaster> fileMaster, List<BaseCommandReference> baseCommandReference, List<string> inputList, int callExtRefId, params string[] lineStartString)
        {
            if (baseCommandReference == null)
            {
                using (var appDbContext = new AppDbContext())
                {
                    var lstItems = appDbContext.Set<BaseCommandReference>().Include("PrimaryLanguageReference").ToList();
                    var settings = new JsonSerializerSettings
                    {
                        ContractResolver = new ReferenceLoopResolver<BaseCommandReference>(),
                        PreserveReferencesHandling = PreserveReferencesHandling.None,
                        ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                        Formatting = Formatting.Indented
                    };
                    var json = JsonConvert.SerializeObject(lstItems, settings);
                    var listData = JsonConvert.DeserializeObject<List<BaseCommandReference>>(json);
                    baseCommandReference = listData;
                }
            }

            if (fileMaster == null)
            {
                using (var appDbContext = new AppDbContext())
                {
                    var lstItems = appDbContext.Set<FileMaster>().AsQueryable().Include("ProjectMaster").
                        Include("FileTypeExtensionReference").ToList();
                    var settings = new JsonSerializerSettings
                    {
                        ContractResolver = new ReferenceLoopResolver<FileMaster>(),
                        PreserveReferencesHandling = PreserveReferencesHandling.None,
                        ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                        Formatting = Formatting.Indented
                    };
                    var json = JsonConvert.SerializeObject(lstItems, settings);
                    var listData = JsonConvert.DeserializeObject<List<FileMaster>>(json);
                    fileMaster = listData;
                }
            }

            var lstStatementReferenceMaster = new List<StatementReferenceMaster>();
            var lineCommentedIndicators = baseCommandReference.Find(s => s.BaseCommand == "Line Comment")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                   .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                   .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

            var ifStart = ifConditionStart.Find(s => true);
            var ifEnd = ifConditionEnd.FindAll(s => true);
            var loopStart = loopIndicatorStart.FindAll(l => true);
            var loopEnd = loopIndicatorEnd.FindAll(l => true);
            var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s", RegexOptions.IgnoreCase);
            var programLineListMainList = inputList.SplitIfElseThenStatement(true);

            foreach (var input in programLineListMainList)
            {
                var classNameDeclared = "";
                FileMaster referenceFileMaster = null;
                if (input.StartsWith("*")) continue;
                if (lineStartString.Any(l => input.StartsWith(l + " ")))
                {
                    var oStatement = input.Trim();
                    if (regEx.IsMatch(oStatement))
                    {
                        oStatement = Regex.Replace(oStatement, @"\s+", " ").Trim();
                        int splitLength = oStatement.Split(' ').Length;
                        if (splitLength <= 1) goto ContinueFurther;

                        string programName = regEx.IsMatch(oStatement) ? oStatement.Split(' ')[1] : oStatement.Split(' ')[2];
                        var anyFile = fileMaster.ToList()
                            .Any(f => programName != null
                                      && Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                      && f.FileTypeExtensionId == 10);
                        if (!anyFile) goto ContinueFurther;

                        var pName = fileMaster.ToList()
                            .Find(f => programName != null
                                       && Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                       && f.FileTypeExtensionId == 10);

                        if (pName == null) goto ContinueFurther;
                        var projectMaster = _appDbContext.ProjectMaster.Find(pName.ProjectId);
                        if (pName.ProjectMaster == null) pName.ProjectMaster = projectMaster;
                        if (pName.ProjectMaster != null)
                        {
                            var pNameNew = pName.ProjectMaster.ProjectName;
                            var pPathNew = pName.ProjectMaster.PhysicalPath;
                            var className = pName.FilePath.Replace(pPathNew + "\\", "")
                                .Replace(pName.FileName, "").Replace("\\", ".");
                            var fileName = Path.GetFileNameWithoutExtension(pName.FilePath);
                            classNameDeclared = pNameNew + "." + className + fileName;
                        }

                        referenceFileMaster = pName;
                    }

                    else
                    {
                        string programName = regEx.IsMatch(oStatement)
                            ? oStatement.Split(' ')[1]
                            : oStatement.Split(' ')[2];
                        var anyFile = fileMaster.ToList()
                            .Any(f => programName != null
                                      && Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                      && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));
                        if (!anyFile) goto ContinueFurther;

                        var pName = fileMaster.ToList()
                            .Find(f => programName != null
                                       && Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                       && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17));

                        if (pName == null) goto ContinueFurther;
                        var pNameNew = pName.ProjectMaster.ProjectName;
                        var pPathNew = pName.ProjectMaster.PhysicalPath;
                        var className = pName.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName.FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName.FilePath);
                        classNameDeclared = pNameNew + "." + className + fileName;
                        referenceFileMaster = pName;
                    }
                }

                ContinueFurther:

                if (input.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                    continue;

                if (lineStartString.Any(l => input.StartsWith(l + " ")) || regEx.IsMatch(input))
                {
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
                        BaseCommandId = 6,
                        ReferenceFileId = referenceFileMaster?.FileId ?? 0,
                        StatementComment = ""
                    });
                    continue;
                }
                if (input.StartsWith(ifStart.StartIndicator))
                {
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        BusinessName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId = ifStart.BaseCommandId,
                        PrimaryCommandId = ifStart.PrimaryReferenceId
                    });
                    continue;
                }
                if (ifEnd.Any(l => input == l.StartIndicator))
                {
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        BusinessName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId = 2,
                        PrimaryCommandId =
                            ifEnd.Find(s => input.StartsWith(s.StartIndicator)).PrimaryReferenceId
                    });
                    continue;
                }
                if (loopStart.Any(l => input.StartsWith(l.StartIndicator)) || input == "LOOP")
                {
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        BusinessName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId =
                            input == "LOOP" ? 3 : loopStart.Find(s => input.StartsWith(s.StartIndicator)).BaseCommandId,
                        PrimaryCommandId = input == "LOOP"
                            ? 62
                            : loopStart.Find(s => input.StartsWith(s.StartIndicator)).PrimaryReferenceId
                    });

                    continue;
                }
                if (loopEnd.Any(l => input.StartsWith(l.StartIndicator)))
                {
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        BusinessName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId = loopEnd.Find(s => input.StartsWith(s.StartIndicator)).BaseCommandId,
                        PrimaryCommandId = loopEnd.Find(s => input.StartsWith(s.StartIndicator)).PrimaryReferenceId
                    });
                    continue;
                }
                if ((input == "END ELSE") || (input == "ELSE"))
                {
                    lstStatementReferenceMaster.Add(new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = input,
                        OriginalStatement = input,
                        ClassCalled = null,
                        MethodName = null,
                        BusinessName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId = 10,
                        PrimaryCommandId = 0
                    });
                    continue;
                }

                lstStatementReferenceMaster.Add(new StatementReferenceMaster
                {
                    FileId = fileId,
                    ResolvedStatement = input,
                    OriginalStatement = input,
                    ClassCalled = null,
                    MethodName = null,
                    BusinessName = null,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    BaseCommandId = 0,
                    PrimaryCommandId = 0
                });
            }
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var pNameNew = fileMaster.ProjectMaster.ProjectName;
            var pPathNew = fileMaster.ProjectMaster.PhysicalPath;
            var classNameProgram = fileMaster.FilePath.Replace(pPathNew + "\\", "")
                .Replace(fileMaster.FileName, "").Replace("\\", ".");
            var fileNameProgram = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var classNameDeclaredProgram = pNameNew + "." + classNameProgram + fileNameProgram;

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
                    ClassNameDeclared = classNameDeclaredProgram,
                    PrimaryCommandId = pClassStartIndicator,
                    BaseCommandId = 19,
                    ProjectId = fileMaster.ProjectId,
                    SolutionId = fileMaster.SolutionId ?? 0
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
                    ProjectId = fileMaster.ProjectId,
                    SolutionId = fileMaster.SolutionId ?? 0
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator, int pMethodStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var pNameNew = fileMaster.ProjectMaster.ProjectName;
            var pPathNew = fileMaster.ProjectMaster.PhysicalPath;
            var classNameProgram = fileMaster.FilePath.Replace(pPathNew + "\\", "")
                .Replace(fileMaster.FileName, "").Replace("\\", ".");
            var fileNameProgram = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var classNameDeclaredProgram = pNameNew + "." + classNameProgram + fileNameProgram;

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
                    ClassNameDeclared = classNameDeclaredProgram,
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
            var copyOfAllLines = new List<string>();
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

            List<string> extractedLines;
            var methodBlockDictionary = new Dictionary<string, List<string>>();
            var stopLinesCount = copyOfAllLines.Count(l => l == "STOP");
            if (stopLinesCount > 0)
            {
                var upToStopBlock = copyOfAllLines.GetListUpToLastStop();
                extractedLines = copyOfAllLines.GetRange(upToStopBlock.Count, copyOfAllLines.Count - upToStopBlock.Count);
                methodBlockDictionary.Add("STOP", upToStopBlock);
            }
            else
            {
                extractedLines = copyOfAllLines.ToList();
            }

            foreach (var goSub in lstAllGoSubs.ToList())
            {
                if (goSub.ContainsAll(";", "*")) continue;
                var methodBlock = PickUpAllMethodBlocks(extractedLines, goSub, lstAllGoSubs, "RETURN");
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

        public Dictionary<string, List<string>> GetListFromCertainPoint(List<string> allLines, List<string> lstAllGoSubs, bool incOrSub)
        {
            var copyOfAllLines = new List<string>();

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
            /*
            List<string> extractedLines;
            var methodBlockDictionary = new Dictionary<string, List<string>>();
            var stopLinesCount = copyOfAllLines.Count(l => l == "STOP");
            if (stopLinesCount > 0)
            {
                var upToStopBlock = copyOfAllLines.GetListUpToLastStop();
                extractedLines = copyOfAllLines.GetRange(upToStopBlock.Count, copyOfAllLines.Count - upToStopBlock.Count);
                methodBlockDictionary.Add("STOP", upToStopBlock);
            }
            else
            {
                extractedLines = copyOfAllLines.ToList();
            }
            */
            List<string> extractedLines = copyOfAllLines.ToList();
            var methodBlockDictionary = new Dictionary<string, List<string>>();
            foreach (var goSub in lstAllGoSubs.ToList())
            {
                if (goSub.ContainsAll(";", "*")) continue;
                var methodBlock = PickUpAllMethodBlocks(extractedLines, goSub, lstAllGoSubs, "RETURN");
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
            int ifCounter = 0;
            int caseCounter = 0;
            for (var length = index; length < inputList.Count; length++)
            {
                var line = inputList[length];
                if (line.StartsWith("*")) continue;

                outputList.Add(line);
                if (line.StartsWith("*")) break;
                if (line.StartsWith("IF ", true, CultureInfo.CurrentCulture)) ifCounter++;
                if (line == "END" || line == "END-IF" || line == "END IF") ifCounter--;
                if (line == "BEGIN CASE") caseCounter++;
                if (line == "END CASE") caseCounter--;
                if (otherEndPoints.Any(l => l == line) && ifCounter == 0 && caseCounter == 0) break;
                if (!allGoSub.Any(l => line.StartsWith(l))) continue;

                outputList.Remove(line);
                break;
            }
            return outputList;
        }

        public List<string> PickUpAllMethodBlocks(List<string> inputList, string startingPoint)
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
            return outputList;
            /*
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
            */
        }
    }
}