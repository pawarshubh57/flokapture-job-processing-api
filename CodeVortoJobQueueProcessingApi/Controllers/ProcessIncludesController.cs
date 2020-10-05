
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class ProcessIncludesController :ApiController
    {
     
        private ICodeVortoService _codeVortoService;

        public ProcessIncludesController()
        {
            
        }

        public ProcessIncludesController(ICodeVortoService codeVortoService)
        {
            
        }

        [HttpGet]
        public  async  Task<IHttpActionResult> StartProcessIncludesFile(int projectId)  // projectId =2
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(
                        x => x.ProjectId == projectId && x.FileTypeExtensionId == 12 && x.Processed == 0);
                foreach (var fMaster in fileMaster)
                {
                    var lstFileLines = File.ReadAllLines(fMaster.FilePath).ToList();
                    if (lstFileLines.Count <= 0) return Ok("No contents in file: " + fMaster.FilePath);
                  await ParseCallAndIncludesFilesUniverse(fMaster,5,fileMaster);

                  #region Update field for basecommandId = 45

                  stringBuilder.AppendLine(
      "========================================================================================");
                  stringBuilder.AppendLine("Started update field for basecommandId = 45 for Project: " + projectId + "");

                  var sqlQuery = " SELECT  * FROM UniverseBasicDataDictionary WHERE ProjectId=" +projectId+" GROUP BY FileName;";
                  var universeBasicDataDicyionary =
                      await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

                  foreach (var dataDictionary in universeBasicDataDicyionary)
                  {
                      string tableName = dataDictionary.Description;

                      string fileName = dataDictionary.FileName;
                      string sqlQry = "SELECT * from statementreferencemaster WHERE fileId=" + fMaster.FileId +
                                      " AND OriginalStatement like '%" + fileName + "%';";
                      var statementRefMaster =
                     await
                         _codeVortoService.StatementReferenceMasterRepository
                             .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                      foreach (var statementRef in statementRefMaster)
                      {
                          if (fileName != null)
                          {
                              var newStatement = Regex.Replace(!string.IsNullOrEmpty(statementRef.AlternateName) ? statementRef.AlternateName : statementRef.OriginalStatement, fileName, tableName, RegexOptions.IgnoreCase);
                              if (statementRef.BaseCommandId != 0)
                              {
                                  statementRef.AlternateName = newStatement;
                                  statementRef.BusinessName = newStatement;
                                  statementRef.ResolvedStatement = newStatement;
                              }
                              else
                              {
                                  statementRef.BaseCommandId = 45;
                                  statementRef.AlternateName = newStatement;
                                  statementRef.BusinessName = newStatement;
                                  statementRef.ResolvedStatement = newStatement;
                              }
                          }
                          await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef);
                      }
                  }
                  stringBuilder.AppendLine(
      "========================================================================================");
                  stringBuilder.AppendLine("Ended update field for basecommandId = 45 for ProjectId: " + projectId + "");

                  #endregion
                }
               
            }
            return StatusCode(HttpStatusCode.NoContent);
        }

        public async Task<IHttpActionResult> ParseCallAndIncludesFilesUniverse(FileMaster icdFile, int languageId, List<FileMaster> copyOfFileMaster)
        {
            var stringBuilder = new StringBuilder();
            int commanClassProjId = 9;
            using (_codeVortoService = new CodeVortoService())
            {
                #region Load Project and Base Command Reference Details...

                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllItems()
                    .ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var universeBasicV1 = new UniverseBasicVersion1();
                 var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId ==languageId);
                var methodEnd = methodIndicationEnd.Find(x => true);
                  var classStart = callClassIndicatorStart.Find(x => true);
                var classEnd = callClassIndicatorEnd.Find(x => true);
                var methodStart = methodIndicationStart.Find(x => true);
                #endregion

                var programFileLines = File.ReadAllLines(icdFile.FilePath).ToList();
                int projectId = icdFile.ProjectId;
                programFileLines.RemoveAll(s => s.Length <= 0);
                programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                programFileLines = programFileLines.AdjustMatReadLockedLikeStatements();
                // 
                var copyOfFileLines = programFileLines.ToList();
                programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                               && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                               !s.StartsWith("; *")).ToList();

                #region Correct all method blocks and statements...

                // Program file processing started...
                var programLineList = new List<string>();
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started collecting all GOSUB: GetAllGoSub for project:" + projectId);
                var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                Dictionary<string, List<string>> methodBlockDictionary;
             
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started collecting all certainpointInclude: GetListFromCertainPointInclude for project: " + projectId);
                    methodBlockDictionary = universeBasicV1.GetListFromCertainPointInclude(copyOfFileLines,
                        lstAllGoSubs);
              
                var startedBlock = false;
                for (var length = 0; length < programFileLines.Count; length++)
                {
                    var currentLine = programFileLines[length];
                    if (lstAllGoSubs.Any(l => currentLine.StartsWith(l)))
                    {
                        startedBlock = true;
                        var firstOrDefault = currentLine.Split('*').FirstOrDefault();
                        if (firstOrDefault != null)
                            currentLine = firstOrDefault.Trim();
                        stringBuilder.AppendLine("========================================================================================");
                        stringBuilder.AppendLine("Started collecting all methodblocks: PickUpAllMethodBlocks for project:" + projectId);
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines,
                            programFileLines[length], lstAllGoSubs, "RETURN", "STOP");
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        programLineList.AddRange(methodBlock.Value);
                        length = length + methodBlockOriginal.Count - 1;
                        continue;
                    }
                    if (startedBlock) continue;

                    programLineList.Add(currentLine);
                }

                #endregion

                var statmentRefStart = universeBasicV1.PrepareStatementReferenceMasterStart(icdFile, 42);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                #region Insert into StatementReferenceMaster...

                var ifStart = ifConditionStart.Find(s => true);
                var ifEnd = ifConditionEnd.FindAll(s => true);
                var loopStart = loopIndicatorStart.FindAll(l => true);
                var loopEnd = loopIndicatorEnd.FindAll(l => true);
                var callInternal = callInternalIndicationStart.Find(c => true);
                var callExternal = callExternalIndicationStart.Find(c => true);
                var indexPosition = -1;
                var methodBusinessName = string.Empty;
                string businessName = null;
                var linesWithCommentedPart = programLineList.ToList();
                programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();
                stringBuilder.AppendLine("Started dump statement into database of file: " + icdFile.FilePath);
                foreach (var line in programLineList)
                {
                    indexPosition = indexPosition + 1;

                    if (string.IsNullOrEmpty(line)) continue;

                    if (indexPosition + 1 < programLineList.Count)
                    {
                        var nextLine = programLineList[indexPosition + 1];
                        if (!string.IsNullOrEmpty(nextLine) && lstAllGoSubs.Any(a => a.StartsWith(nextLine)))
                        {
                            methodBusinessName = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(line.ToLower());
                            continue;
                        }
                        if (linesWithCommentedPart[indexPosition].Contains("; *") ||
                            linesWithCommentedPart[indexPosition].Contains(";*"))
                        {
                            var lastOrDefault = linesWithCommentedPart[indexPosition].Split(';').LastOrDefault();
                            if (lastOrDefault != null)
                                businessName = lastOrDefault.Replace("*", "");
                            if (businessName != null)
                            {
                                businessName = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(businessName.ToLower());
                                businessName = businessName.Replace("&", "AND").Replace("&&", "AND").Replace("||", "OR");
                            }
                        }
                    }

                    if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                        continue;

                    if (line.StartsWith("RETURN"))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = methodEnd.BaseCommandId,
                            PrimaryCommandId = methodEnd.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (lstAllGoSubs.Any(a => line.StartsWith(a)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = line.Split(':').FirstOrDefault() + "()",
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 8,
                            PrimaryCommandId = 36,
                            ParsedOrNot = "Y",
                            ProjectId = projectId,
                            BusinessName = methodBusinessName
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        methodBusinessName = string.Empty;
                        continue;
                    }

                    if (line.StartsWith(ifStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = ifStart.BaseCommandId,
                            PrimaryCommandId = ifStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }

                    if ((line == "END ELSE") || (line == "ELSE"))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 10,
                            PrimaryCommandId = 0,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }

                    if (ifEnd.Any(l => line == l.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 2,
                            PrimaryCommandId = ifEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }

                    if (loopStart.Any(l => line.StartsWith(l.StartIndicator)) || line == "LOOP")
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = line == "LOOP" ? 3 : loopStart.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                            PrimaryCommandId = line == "LOOP" ? 62 :
                                    loopStart.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (loopEnd.Any(l => line.StartsWith(l.StartIndicator)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                            PrimaryCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        var methodName = line.CheckCommentInStatement();
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = methodName,
                            OriginalStatement = methodName,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = methodName.Split(' ').LastOrDefault() + "()",
                            VariableNameDeclared = null,
                            BaseCommandId = callInternal.BaseCommandId,
                            PrimaryCommandId = callInternal.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (line.StartsWith(callExternal.StartIndicator + " "))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = callExternal.BaseCommandId,
                            PrimaryCommandId = callExternal.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                    }
                    else
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = icdFile.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 0,
                            PrimaryCommandId = 0,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                    }
                }

                #endregion

                var statmentRefEnd = universeBasicV1.PrepareStatementReferenceMasterEnd(icdFile, 43);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);

                #region Update ClassCalled field for StatementReferenceMaster...
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started update classcalled and methodcalled for basecommandId = 6 & 19 for project: " + projectId);
                /* Added shubhangi */
                var execOrCallSql = " Select sm.* from StatementReferenceMaster as sm " +
                                    " Inner Join FileMaster as fm ON sm.FileId = fm.FileId Where sm.ProjectId IN (" +
                                    projectId + "," + commanClassProjId +
                                    " ) AND fm.Processed = 0 AND sm.BaseCommandId IN (6, 19); ";
                var callExternals = await baseCommandReferenceRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(execOrCallSql).ConfigureAwait(false);
                
                foreach (var cExternal in callExternals)
                {
                    if (cExternal.BaseCommandId == 6)
                    {
                        string pgName;
                        if (cExternal.OriginalStatement.ContainsAll("@", "(", ")"))
                        {
                            pgName = cExternal.OriginalStatement.Split('@').LastOrDefault();
                            if (pgName != null) pgName = pgName.Split('(').FirstOrDefault();
                        }
                        else
                            pgName = cExternal.OriginalStatement.Split('@').LastOrDefault();

                        var pName =
                            copyOfFileMaster.ToList().Where(f => !string.IsNullOrEmpty(pgName)
                                                                 && f.FileName.StartsWith(pgName) &&
                                                                 (f.FileTypeExtensionId == 9)).ToList();
                        if (!pName.Any()) continue;
                        var projectDetatils = _codeVortoService.ProjectMasterRepository.GetItem(pName[0].ProjectId);
                        var pNameNew = projectDetatils.ProjectName;
                        var pPathNew = projectDetatils.PhysicalPath;

                        var className = pName[0].FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclared = pNameNew + "." + className + fileName;
                        cExternal.ClassCalled = classNameDeclared;

                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(cExternal).ConfigureAwait(false);
                        continue;
                    }
                    if (cExternal.BaseCommandId == 19)
                    {
                        var fileId = cExternal.FileId;


                        var pName =
                            copyOfFileMaster.ToList().Where(f => f.FileId == fileId).ToList();
                        if (pName.Count == 0) // Added shubhangi
                        {
                            var fileMasterNew = await _codeVortoService.FileMasterRepository
                                .GetAllItems(p => p.ProjectId == projectId || p.ProjectId == commanClassProjId);

                            copyOfFileMaster =
                                new List<FileMaster>(fileMasterNew as FileMaster[] ?? fileMasterNew.ToArray());
                            pName =
                                copyOfFileMaster.ToList().Where(f => f.FileId == fileId).ToList();
                        }
                        var projectDetatils = _codeVortoService.ProjectMasterRepository.GetItem(pName[0].ProjectId);
                        var pNameNew = projectDetatils.ProjectName;
                        var pPathNew = projectDetatils.PhysicalPath;
                        var classNameProgram = pName[0].FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclaredProgram = pNameNew + "." + classNameProgram + fileNameProgram;
                        cExternal.ClassNameDeclared = classNameDeclaredProgram;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                    }
                }
                
                #endregion

                #region Update Method called for base command id = 6 in Jcl and program...
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started update methodcalled for Jcl and program basecommandId = 6 for project: " + projectId);
               
                var execSql =
                    " Select sm.* from StatementReferenceMaster as sm " +
                    " Inner Join FileMaster as fm ON fm.FileId = sm.FileId Where sm.ProjectId = " + projectId +
                    " AND sm.BaseCommandId = 6 " +
                    " AND fm.Processed = 0 AND sm.ClassCalled is not null;";

                var execName = await baseCommandReferenceRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(execSql).ConfigureAwait(false);

                foreach (var constructor in execName)
                {
                    // ReSharper disable once RedundantAssignment
                    var fName = constructor.ClassCalled.Split('.').LastOrDefault().Trim() + ".pgm";
                    var allCheckFiles = await _codeVortoService.FileMasterRepository
                        .GetAllItems(f => (f.ProjectId == projectId) && (f.FileName == fName)).ConfigureAwait(false);

                    foreach (var files in allCheckFiles)
                    {
                        var methodSql = " SELECT DISTINCT sm.* from statementreferencemaster as sm " +
                                        " INNER JOIN FileMaster as fm where sm.FileId = " + files.FileId +
                                        " AND fm.SolutionId = " + files.SolutionId + " AND sm.BaseCommandId = 8;";

                        var methodName = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(methodSql).ConfigureAwait(false);
                        
                        foreach (var statementReference in methodName)
                        {
                            if (string.IsNullOrEmpty(statementReference.MethodName)) continue;
                            
                            var pName =
                                copyOfFileMaster.ToList().Where(f => f.FileId == statementReference.FileId).ToList();
                            var projectDetails1 =
                                _codeVortoService.ProjectMasterRepository.GetItem(pName[0].ProjectId);
                            var pNameNew = projectDetails1.ProjectName;
                            var pPathNew = projectDetails1.PhysicalPath;
                            var classNameProgram = pName[0].FilePath.Replace(pPathNew + "\\", "")
                                .Replace(pName[0].FileName, "").Replace("\\", ".");
                            var fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                            // Use it later...
                            var classCalled = pNameNew + "." + classNameProgram + fileNameProgram;

                            constructor.ClassCalled = classCalled;
                            constructor.MethodCalled = statementReference.MethodName;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor).ConfigureAwait(false);
                            break;
                        }
                        
                    }
                }

                #endregion

                #region Update field for basecommandId = 30
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started update field for basecommandId = 30 for project: " + projectId);
                if (!string.IsNullOrEmpty(icdFile.FilePath))
                {
                    var fileId = icdFile.FileId;
                    var execSqlPrgm = " Select * from StatementReferenceMaster where ProjectId =" + projectId +
                                      " AND FileId = " + fileId + " AND BaseCommandId IN (0, 1);";

                    var execNamePrgm =
                        await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlPrgm);
                    var indexPositionPrgm = -1;
                    foreach (var exNmPg in execNamePrgm)
                    {
                        indexPositionPrgm = indexPositionPrgm + 1;
                        if (exNmPg.BaseCommandId != 1) continue;
                        if ((exNmPg.OriginalStatement != "IF FOUND THEN")
                            && (exNmPg.OriginalStatement != "IF FOUND")
                            && (exNmPg.OriginalStatement != "IF NOT SUCCESS THEN")
                            && (exNmPg.OriginalStatement != "IF NOT-SUCCESS THEN")
                            && (exNmPg.OriginalStatement != "IF SUCCESS THEN")
                            && (exNmPg.OriginalStatement != "IF NOT FOUND THEN")
                            && (exNmPg.OriginalStatement != "IF NOT-FOUND THEN")
                            && (exNmPg.OriginalStatement != "IF NOT FOUND")
                            && (exNmPg.OriginalStatement != "IF NOT-FOUND"))
                            continue;

                        var aboveLine = execNamePrgm[indexPositionPrgm - 1];
                        aboveLine.BaseCommandId = 30;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(aboveLine);
                    }
                }

                #endregion

                #region Update Include program File Processed
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started update Include program File Processed for project: " + projectId + ", and file is:" + icdFile.FileName + "");

                icdFile.DoneParsing = 1;
                icdFile.ProjectMaster = null;
                await _codeVortoService.FileMasterRepository.UpdateItem(icdFile).ConfigureAwait(false);

                if (icdFile.FileTypeExtensionId != 12) return Ok("Done");
                icdFile.Processed = 1;
                icdFile.ProjectMaster = null;
                await _codeVortoService.FileMasterRepository.UpdateItem(icdFile).ConfigureAwait(false);

                #endregion
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Done");
            }
        }


     
    }
}