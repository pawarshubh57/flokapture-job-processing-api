using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Entity;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using DataAccessLayer;
using MySql.Data.MySqlClient;

// ReSharper disable RedundantAssignment

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class UniverseBasicVersion1Controller : ApiController
    {
        private AppDbContext _appDbContext;
        private ICodeVortoService _codeVortoService;
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();

        public UniverseBasicVersion1Controller()
        {
        }

        public UniverseBasicVersion1Controller(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessUniverseProject(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectConfigFilesGeneralRepository = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));

                var projectPath = projectDetails.PhysicalPath;
                var directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                var projectImpFiles = await projectConfigFilesGeneralRepository.GetAllItems(p => p.ConfigFileId != 2);

                strExtensions.AddRange(projectImpFiles.Distinct().Select(e => e.ToString()));
                foreach (var directory in directoryList)
                {
                    var allFiles = Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                        .Where(s => strExtensions.Any(s.EndsWith));

                    var enumerator = allFiles.GetEnumerator();
                    var lstFileMasters = new List<FileMaster>();
                    while (enumerator.MoveNext())
                    {
                        var currentFile = enumerator.Current;
                        if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                        var fileName = Path.GetFileName(currentFile);
                        if (string.IsNullOrEmpty(fileName)) continue;
                        if (fileName.Contains(".dll.config")) continue;
                        var extension = Path.GetExtension(currentFile);
                        var extensionId =
                            fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                        var fileMaster = new FileMaster
                        {
                            FileId = 0,
                            FileName = fileName,
                            FilePath = currentFile,
                            FileTypeExtensionId = extensionId,
                            ProjectId = projectId
                        };
                        lstFileMasters.Add(fileMaster);
                    }
                    enumerator.Dispose();
                    await _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters);
                }
                //return Ok(projectId);
                IHttpActionResult processResult = await StartParsingProcessUniverseBasic(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartParsingProcessUniverseBasic(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId);
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                var copyOfFileMasterNew = copyOfFileMaster
                    .Where(f => (f.FileTypeExtensionId == 10)
                                && (f.ProjectId == projectId)
                                && (f.Processed == 0)).ToList();
                foreach (var copyFile in copyOfFileMasterNew)
                {
                    var processResult = await ParseProjectFilesUniverse(copyFile.ProjectId);
                    await processResult.ExecuteAsync(CancellationToken.None);
                }

                var actionWorkflowResult = await ProcessAllActionWorkflows(projectId);
                await actionWorkflowResult.ExecuteAsync(CancellationToken.None);

                var processDataDictionaryResult = await ParseDataDictionaryReferenceCode(projectId);
                var dataContent = await processDataDictionaryResult.ExecuteAsync(CancellationToken.None);
                var result = await dataContent.Content.ReadAsStringAsync();

                IHttpActionResult processStartingPointResult = await GetAllStartingPoints(projectId);
                await processStartingPointResult.ExecuteAsync(CancellationToken.None);


                return Ok();
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActionWorkflows(int projectId)
        {
            #region Added the Action Workflow Table...

            // Action Workflows data...
            var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
            Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
            var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
            var projectType = projectMasterData.ProjectConfigType;
            if (projectType == 5) // For COBOL file
            {
                var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectConfigData = projectConfig.GetItem(projectType);
                if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                // Means the project type is windows application and we need to read starting point from that 
                // files respective code behind file...
                var configFileName = projectConfigData.ToString();
                if (string.IsNullOrEmpty(configFileName)) return Ok("Process completed successfully");

                try
                {
                    var fileMasterRepository = new FileMasterRepository(new AppDbContext());
                    var actionWorkflowsRepository = new ActionWorkflowsRepository(new AppDbContext());
                    var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());

                    var allConfigFiles = await fileMasterRepository.GetAllItems(
                        f => f.ProjectId == projectId);

                    foreach (var cFile in allConfigFiles)
                    {
                        var fileExtension = cFile.FileTypeExtensionId;
                        if (fileExtension != 10) continue;

                        object[] param =
                        {
                            new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                            new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = cFile.FileId}
                        };

                        var genericBlocks = await
                            statementReferenceRepository.ExecuteStoreProcedure<StatementReferenceMaster>(
                                "SpGetMethodData", param);

                        foreach (var statement in genericBlocks)
                        {
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Batch",
                                MethodStatementId = statement.StatementId,
                                OriginFileName = Path.GetFileName(cFile.FilePath),
                                OriginFilePath = cFile.FilePath,
                                ProjectId = projectId,
                                OriginEventMethod = cFile.FileName,
                                OriginObject = projectMasterData.ProjectName +
                                               cFile.FilePath
                                                   .Replace(".jcl", "")
                                                   .Replace(projectMasterData.PhysicalPath, "")
                                                   .Trim()
                                                   .Replace("\\", "."),
                                WorkflowName = cFile.FileName,
                                ServiceBaseAddress = null,
                                ServiceContract = null,
                                WorkflowBusinessName = statement.BusinessName,
                                Processed = 0
                            };
                            await actionWorkflowsRepository.AddNewItem(actionWorkflow);
                        }
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }
            }
            return Ok("Action Workflows processed successfully");

            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseDataDictionaryReferenceCode(int projectId)
        {
            #region region 2...Data Dictionary Reference Code & Include (Subroutine) file logic

            try
            {
                using (_appDbContext = new AppDbContext())
                {
                    var dictionaryData = await _appDbContext.DataDictionary.ToListAsync();

                    var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var dataDictionarySql =
                        " SELECT state.* from fileMaster file, statementreferencemaster state where file.FileId = state.FileId AND file.FileTypeExtensionId = 9 AND file.ProjectId='" +
                        projectId +
                        "' AND  state.BaseCommandId Not In (8, 9, 2, 5, 6, 19, 20) AND  OriginalStatement like '%)%'; ";
                    var dataDictionaryNew = await
                        baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql);
                    var regExMatch = new Regex(@"([A-Z0-9]\.)\w+([A-Z0-9]\.)\w+(\([\[0-9]*\))");

                    var allDataDictStatements = dataDictionaryNew
                        .Where(s => regExMatch.IsMatch(s.OriginalStatement));

                    foreach (var dataDictionaryStmt in allDataDictStatements)
                    {
                        var currentSentance = dataDictionaryStmt.OriginalStatement;
                        //var word = new Regex(@"([A-Z0-9]\.)\w+([A-Z0-9]\.)\w+(\([\[0-9]*\))");
                        foreach (Match match in regExMatch.Matches(currentSentance))
                        {
                            var currentstatement = match.Value;
                            var fileName = currentstatement.Split('(')[0];
                            fileName = fileName.Substring(fileName.IndexOf(".", StringComparison.Ordinal) + 1).Trim();

                            var result = dictionaryData
                                .Where(x => x.ReplacementName == currentstatement).ToList();
                            if (result.Count < 1) continue;

                            var firstOrDefault = dictionaryData
                                .FirstOrDefault(x => x.ReplacementName == currentstatement);

                            if (firstOrDefault == null) continue;
                            var replacementName = firstOrDefault.Description;

                            if (replacementName == null) continue;

                            var replaceStatement = replacementName + " field of " + fileName;
                            var orignalStatement = currentSentance.Replace(currentstatement,
                                replaceStatement);
                            currentSentance = orignalStatement;

                            dataDictionaryStmt.OriginalStatement = currentSentance;
                            dataDictionaryStmt.ResolvedStatement = currentSentance;
                            var x1 = await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(dataDictionaryStmt);

                            if (x1 == null) continue;

                            var statWithDataDictionary = new StatementWithDataDictionary
                            {
                                StatementId = dataDictionaryStmt.StatementId,
                                OrignalStatement = currentstatement,
                                ReplacedStatement = replacementName
                            };
                            _appDbContext.StatementWithDataDictionary.Add(statWithDataDictionary);
                            await _appDbContext.SaveChangesAsync();
                        }
                    }
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }
            return Ok("Project Processed Successfully");

            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProjectFilesUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);

                // FileTypeExtensionId == 10 is for jcl files which is starting point of Universe Basics...
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId);
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                if (copyOfFileMaster.All(f => f.FileTypeExtensionId != 10))
                    return Ok("There is no Jcl file found which is remaining or pending for processing");

                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllItems()
                    .ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                //var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External")
                //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                //var findIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Find Start")
                //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);

                var universeBasicV1 = new UniverseBasicVersion1();
                var projectName = projectDetails.ProjectName;
                var classStart = callClassIndicatorStart.Find(x => true);
                var classEnd = callClassIndicatorEnd.Find(x => true);
                var methodStart = methodIndicationStart.Find(x => true);
                var methodEnd = methodIndicationEnd.Find(x => true);
                var callExternalStart = callExternalIndicationStart.Find(x => true);

                // This is common for all statement that is ClassNameDeclared
                var pPath = projectDetails.PhysicalPath;

                #region Jcl file processing...

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();

                foreach (var fMaster in copyOfFileMaster.Where(f => (f.FileTypeExtensionId == 10) && (f.Processed == 0))
                    )
                {
                    //string className = fMaster.FilePath.Replace(pPath + "\\", "")
                    //    .Replace(fMaster.FileName, "").Replace("\\", ".");
                    //string fileName = Path.GetFileNameWithoutExtension(fMaster.FilePath);
                    // Use it later...
                    //string classNameDeclared = projectName + "." + className + fileName;

                    var lstFileLines = File.ReadAllLines(fMaster.FilePath).ToList();
                    if (lstFileLines.Count <= 0) return Ok("No contents in file: " + fMaster.FilePath);
                    // Prepare list to insert into statement reference master...
                    // As Jcl file will not hace class start and end indicator, so we need to add it to make generic class block...

                    lstStatementReferenceMaster =
                        universeBasicV1.PrepareStatementReferenceMasterStart(fMaster, classStart.PrimaryReferenceId,
                            methodStart.PrimaryReferenceId);
                    lstStatementReferenceMaster.ForEach(
                        s =>
                        {
                            var firstOrDefault = lstFileLines.FirstOrDefault();
                            if (firstOrDefault != null)
                                s.BusinessName = firstOrDefault.Replace("*", "");
                        });
                    // List of statement reference master for all statements in Jcl file with association for base and primary command id's
                    var fileStatements = universeBasicV1.PrepareJclFile(fMaster.FileId, copyOfFileMaster.ToList(),
                        lstFileLines, callExternalStart.PrimaryReferenceId, "RUN", "CALL");

                    lstStatementReferenceMaster.AddRange(fileStatements);
                    // End of statement reference...
                    var closeList = universeBasicV1.PrepareStatementReferenceMasterEnd(fMaster,
                        classEnd.PrimaryReferenceId, methodEnd.PrimaryReferenceId);
                    lstStatementReferenceMaster.AddRange(closeList);
                    lstStatementReferenceMaster.ForEach(p => { p.ProjectId = projectId; });
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    // Update file status to processed = 1...
                    fMaster.Processed = 1;
                    fMaster.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(fMaster);
                    //file = fMaster;
                    // As we are intended to process one Jcl file (Starting point) at a time, so break the loop and exit...
                    break;
                }

                // Pickup statement from above list which has a base command id = 6.
                var statementReferenceMaster =
                    lstStatementReferenceMaster.FirstOrDefault(s => s.BaseCommandId == 6);
                if (statementReferenceMaster == null)
                    return Ok("This Jcl file has no call External. File Name: "
                              + lstStatementReferenceMaster.First().OriginalStatement);

                var oStatement = statementReferenceMaster.OriginalStatement;
                var programName = oStatement.Split(' ').LastOrDefault();


                var anyFile = copyOfFileMaster
                    .Any(f => (programName != null)
                              && f.FileName.StartsWith(programName)
                              && (f.FileTypeExtensionId == 9) && (f.Processed == 0));

                if (!anyFile) return Ok("File already processed");

                var programFilePath = copyOfFileMaster
                    .Single(f => (programName != null)
                                 && f.FileName.StartsWith(programName)
                                 && (f.FileTypeExtensionId == 9) && (f.Processed == 0));
                if (string.IsNullOrEmpty(programFilePath.FilePath)) return Ok("File Not Found");

                var programFileLines = File.ReadAllLines(programFilePath.FilePath).ToList();

                programFileLines.RemoveAll(s => s.Length <= 0);
                programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                // programFileLines = programFileLines.Select(s => s.CheckCommentInStatement()).ToList();
                programFileLines = programFileLines.AdjustMatReadLockedLikeStatements();
                // 
                var copyOfFileLines = programFileLines.ToList();
                var allIncludesFiles = programFileLines.GetAllIncludeStatements("$INSERT");
                programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                               && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                               !s.StartsWith("; *")).ToList();
                programFileLines = programFileLines.Where(a => !a.StartsWith("$INSERT")).ToList();

                //var temp = programFileLines.ToList();
                //programFileLines.RemoveRange(0, temp.Count);
                //programFileLines.InsertRange(0, allIncludesFiles.ToList());
                //programFileLines.AddRange(temp);

                #endregion

                #region Process Include file statements...

                foreach (var includeFile in allIncludesFiles)
                {
                    var iName = includeFile.Split('>').LastOrDefault();
                    await
                        ParseCallAndIncludesFilesUniverse(projectId, iName, copyOfFileMaster.ToList(), projectName,
                            pPath,
                            projectDetails.LanguageId);

                    #region Include files statements..

                    //iName = iName.CheckCommentInStatement();
                    //var icdFile = copyOfFileMaster
                    //    .Single(f => iName != null && f.FileTypeExtensionId == 12 && f.FileName.StartsWith(iName));
                    //if (icdFile == null) continue;
                    //var inclideFileLines = File.ReadAllLines(icdFile.FilePath).ToList();

                    //inclideFileLines.RemoveAll(s => s.Length <= 0);
                    //inclideFileLines = inclideFileLines.Select(s => s.Trim()).ToList();
                    //inclideFileLines = inclideFileLines.Select(s => s.CheckCommentInStatement()).ToList();
                    //inclideFileLines = inclideFileLines.AdjustMatReadLockedLikeStatements();
                    //// 
                    //var includeOfFileLines = inclideFileLines.ToList();
                    //inclideFileLines = inclideFileLines.Where(s => !s.StartsWith("*")
                    //    && !s.StartsWith("!!") && !s.StartsWith(";*") && !s.StartsWith("; *")).ToList();

                    ////inclideFileLines = inclideFileLines.Where(l => !l.StartsWith("*")).ToList();
                    ////inclideFileLines = inclideFileLines.Select(l => l.Trim()).ToList();

                    //    var lstStatementMaster = universeBasicV1.PrepareStatementReferenceMasterStart(icdFile, 19);
                    //    foreach (var fLine in inclideFileLines)
                    //    {
                    //        if (fLine.EndsWith(":") || fLine.EndsWith(":*") || fLine.EndsWith(": *"))
                    //        {
                    //            var statementMaster = new StatementReferenceMaster
                    //            {
                    //                FileId = icdFile.FileId,
                    //                OriginalStatement = fLine,
                    //                BaseCommandId = 8,
                    //                MethodName = fLine.Substring(0, fLine.LastIndexOf(":", StringComparison.Ordinal)),
                    //                StatementId = 0,
                    //                ProjectId = projectId,
                    //                ResolvedStatement = fLine
                    //            };
                    //            lstStatementMaster.Add(statementMaster);
                    //        }
                    //        else
                    //        {
                    //            string statement = fLine.CheckCommentInStatement();
                    //            var statementMaster = new StatementReferenceMaster
                    //            {
                    //                FileId = icdFile.FileId,
                    //                OriginalStatement = statement,
                    //                ResolvedStatement = statement,
                    //                BaseCommandId = 0,
                    //                MethodName = null,
                    //                StatementId = 0,
                    //                ProjectId = projectId,
                    //                PrimaryCommandId = 0,
                    //                OtherBaseCommandId = 0
                    //            };
                    //            lstStatementMaster.Add(statementMaster);
                    //        }
                    //    }
                    //    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementMaster); 

                    #endregion
                }

                #endregion

                #region Process Call file statements... (BaseCommand Id= 6)

                var allCallFiles = programFileLines.GetAllIncludeStatements("CALL");
                foreach (var callFile in allCallFiles)
                {
                    var iName = callFile.Split('@').LastOrDefault();
                    var iExactFileName = iName.Split('(').FirstOrDefault();
                    await
                        ParseCallAndIncludesFilesUniverse(projectId, iExactFileName, copyOfFileMaster.ToList(),
                            projectName,
                            pPath,
                            projectDetails.LanguageId);
                }

                #endregion

                #region Correct all method blocks and statements...

                // Program file processing started...

                var programLineList = new List<string>();
                var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");

                var methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(copyOfFileLines, lstAllGoSubs);
                var tempBlock = programFileLines.GetListUpToLastStop();
                programFileLines.RemoveRange(0, tempBlock.Count);

                tempBlock.Insert(0, copyOfFileLines.FirstOrDefault().Replace("*", ""));
                tempBlock.Insert(1, programName + ":");
                methodBlockDictionary.Add(programName + ":", tempBlock);
                lstAllGoSubs.Add(programName + ":");

                programLineList.InsertRange(0, tempBlock);
                programLineList.InsertRange(2, allIncludesFiles.ToList());

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
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines,
                            programFileLines[length], lstAllGoSubs, "RETURN", "STOP");
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        //methodBlock.Value.Insert(1, currentLine);
                        programLineList.AddRange(methodBlock.Value);
                        length = length + methodBlockOriginal.Count - 1;
                        continue;
                    }
                    if (startedBlock) continue;

                    programLineList.Add(currentLine);
                }
                //programLineList.InsertRange(0, tempBlock);

                #endregion

                var statmentRefStart = universeBasicV1.PrepareStatementReferenceMasterStart(programFilePath, 42);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                #region Insert into StatementReferenceMaster...

                var ifStart = ifConditionStart.Find(s => true);
                var ifEnd = ifConditionEnd.FindAll(s => true);
                var loopStart = loopIndicatorStart.Find(l => true);
                var loopEnd = loopIndicatorEnd.Find(l => true);
                var callInternal = callInternalIndicationStart.Find(c => true);
                var callExternal = callExternalIndicationStart.Find(c => true);
                var indexPosition = -1;
                var methodBusinessName = string.Empty;
                string businessName = null;
                var linesWithCommentedPart = programLineList.ToList();
                programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();
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
                            businessName = linesWithCommentedPart[indexPosition].Split(';')
                                .LastOrDefault()
                                .Replace("*", "");
                            businessName = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(businessName.ToLower());
                            businessName = businessName.Replace("&", "AND")
                                .Replace("&&", "AND")
                                .Replace("||", "OR");
                        }
                    }

                    if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                        continue;

                    if (line.StartsWith("RETURN") || (line == "STOP"))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
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
                            FileId = programFilePath.FileId,
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
                        methodBusinessName = string.Empty;
                        businessName = string.Empty;
                        continue;
                    }

                    if (line.StartsWith(ifStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
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
                            FileId = programFilePath.FileId,
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
                            FileId = programFilePath.FileId,
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

                    /*
                    if (line.StartsWith(methodStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = line.Split(' ').FirstOrDefault(),
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = methodStart.BaseCommandId,
                            PrimaryCommandId = methodStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId,
                            BusinessName =  methodBusinessName
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    */

                    if (line.StartsWith(loopStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = loopStart.BaseCommandId,
                            PrimaryCommandId = loopStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (line.StartsWith(loopEnd.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = loopEnd.BaseCommandId,
                            PrimaryCommandId = loopEnd.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            BusinessName = businessName,
                            DataOrObjectType = null,
                            MethodCalled = line.Split(' ').LastOrDefault() + "()",
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
                        /*
                        string methodCalled;
                        if (line.ContainsAll("@", "(", ")"))
                        {
                            methodCalled = line.Split('@').LastOrDefault();
                            if (methodCalled != null) methodCalled = methodCalled.Split('(').FirstOrDefault();
                        }
                        else
                            methodCalled = line.Split('@').LastOrDefault();
                        */
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
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
                            FileId = programFilePath.FileId,
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

                var statmentRefEnd = universeBasicV1.PrepareStatementReferenceMasterEnd(programFilePath, 43);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);

                #region Update ClassCalled field for StatementReferenceMaster...

                var execOrCallSql = " Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                                    " AND BaseCommandId IN (6, 19); ";
                var callExternals =
                    await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execOrCallSql);

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

                        var className = pName[0].FilePath.Replace(pPath + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclared = projectName + "." + className + fileName;
                        cExternal.ClassCalled = classNameDeclared;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                        continue;
                    }
                    if (cExternal.BaseCommandId == 19)
                    {
                        var pName =
                            copyOfFileMaster.ToList().Where(f => f.FileId == cExternal.FileId).ToList();
                        var classNameProgram = pName[0].FilePath.Replace(pPath + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclaredProgram = projectName + "." + classNameProgram + fileNameProgram;
                        cExternal.ClassNameDeclared = classNameDeclaredProgram;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                    }
                }

                #endregion

                #region Update Method called for base command id = 6 in Jcl and program...

                var execSql = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                              " AND BaseCommandId = 6 AND ClassCalled is not null; ";
                var execName = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSql);
                foreach (var constructor in execName)
                {
                    // ReSharper disable once RedundantAssignment
                    var fName = constructor.ClassCalled.Split('.').LastOrDefault().Trim() + ".pgm";
                    var allCheckFiles = await _codeVortoService.FileMasterRepository
                        // ReSharper disable once AccessToModifiedClosure
                        .GetAllItems(f => (f.ProjectId == projectId) && (f.FileName == fName));

                    foreach (var files in allCheckFiles)
                    {
                        var methodSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                        " AND ProjectId = " + projectId + " AND BaseCommandId=8;";
                        var methodName = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(methodSql);

                        for (var iMethodName = 0; iMethodName < methodName.Count; iMethodName++)
                        {
                            string checkSql;
                            if (methodName.Count > 1)
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                           " AND ProjectId = " + projectId + " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and StatementId < " +
                                           methodName[iMethodName + 1].StatementId + " and BaseCommandId!=9;";
                            else
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                           " AND ProjectId = " + projectId + " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and BaseCommandId!=9;";

                            var checkCnt = await
                                baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(checkSql);
                            if (checkCnt.Count > 0)
                            {
                                constructor.MethodCalled = methodName[iMethodName].MethodName;
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }
                        }
                    }
                }

                #endregion

                #region Update ClassCalled field for BaseCommandId = 5

                try
                {
                    var execSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId +
                                     " AND BaseCommandId = 5 AND ClassCalled is null;";
                    var execNameNew =
                        await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNew);
                    var includeFiles = allIncludesFiles.Select(s => s.CheckCommentInStatement()).ToList();
                    var fileMasterInclude = new List<FileMaster>();
                    foreach (var allIncludeFile in includeFiles)
                    {
                        // ReSharper disable once RedundantAssignment
                        var iName = allIncludeFile.Split('>').LastOrDefault();
                        var icdFile = copyOfFileMaster.Single(
                            f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                 f.FileName.StartsWith(iName));
                        fileMasterInclude.Add(icdFile);
                    }
                    var lstFIleIds = (from f in fileMasterInclude select f.FileId).ToList();
                    var fIds = string.Join(",", lstFIleIds);
                    var mysqlQuery = "Select * from  statementReferenceMaster where FileId in ( " + fIds +
                                     ") and BaseCommandId In (8, 19); ";
                    var allStatements = await
                        _codeVortoService.StatementReferenceMasterRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(mysqlQuery);
                    foreach (var constrctor in execNameNew)
                        if (allStatements.ToList().Any(s => s.MethodName == constrctor.MethodCalled))
                        {
                            // ReSharper disable once RedundantAssignment
                            var fileId = allStatements.Find(s => s.MethodName == constrctor.MethodCalled).FileId;
                            var classCalled =
                                allStatements.Single(d => (d.FileId == fileId) && (d.BaseCommandId == 19))
                                    .ClassNameDeclared;
                            constrctor.ClassCalled = classCalled;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constrctor);
                        }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }

                #endregion

                #region Update $INSERT and CALL statement (To apply the link)

                try
                {
                    var execCallInsertSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId +
                                               "  and OriginalStatement like 'CALL%' and FileId=" +
                                               programFilePath.FileId + ";";
                    var execInsertAndCallNew =
                        await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                execCallInsertSqlNew);

                    foreach (var stat in execInsertAndCallNew)
                    {
                        string fName = string.Empty;
                        if (stat.OriginalStatement.StartsWith("$INSERT"))
                            fName = stat.OriginalStatement.Split('>').LastOrDefault();
                        else
                        {
                            var iName = stat.OriginalStatement.Split('@').LastOrDefault();
                            fName = iName.Split('(').FirstOrDefault();
                        }

                        var anyNewFile = copyOfFileMaster.Any(
                            f =>
                                !string.IsNullOrEmpty(fName) &&
                                (f.FileTypeExtensionId == 9) &&
                                f.FileName.StartsWith(fName));


                        if (!anyNewFile)
                            continue;
                        var icdFile = copyOfFileMaster.Single(
                            f =>
                                !string.IsNullOrEmpty(fName) &&
                                (f.FileTypeExtensionId == 9) &&
                                f.FileName.StartsWith(fName));

                        var neLink = stat.OriginalStatement +
                                     "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                     icdFile.FileId + ");'>[ " + fName + " ]</a>";
                        stat.OriginalStatement = neLink;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stat);
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }

                #endregion

                #region Update field for basecommandId = 30

                if (!string.IsNullOrEmpty(programFilePath.FilePath))
                {
                    var fileId = programFilePath.FileId;
                    var execSqlPrgm = "select * from StatementReferenceMaster where ProjectId =" + projectId +
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

                #region update Includes file Processed

                foreach (var includeFile in allIncludesFiles)
                {
                    // ReSharper disable once RedundantAssignment
                    var iName = includeFile.Split('>').LastOrDefault();
                    var icdFile = copyOfFileMaster.Single(
                        f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                             f.FileName.StartsWith(iName));

                    icdFile.Processed = 1;
                    icdFile.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(icdFile);
                }

                #endregion

                #region Update program File Processed

                programFilePath.Processed = 1;
                programFilePath.ProjectMaster = null;
                await _codeVortoService.FileMasterRepository.UpdateItem(programFilePath);

                #endregion

                var ifCount = programLineList.Count(s => !string.IsNullOrEmpty(s) && s.StartsWith("IF"));
                var endIfCount =
                    programLineList.Count(s => !string.IsNullOrEmpty(s) && (s.EndsWith("END") || s.EndsWith("END IF")));

                programLineList.Add("==========================");
                programLineList.Add("IF Counts: " + ifCount);
                programLineList.Add("END Count: " + endIfCount);

                return Ok(programLineList);
            }
        }

        public async Task<IHttpActionResult> ParseCallAndIncludesFilesUniverse(int projectId, string iName,
            List<FileMaster> copyOfFileMaster, string projectName, string pPath, int languageId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
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
                var methodEnd = methodIndicationEnd.Find(x => true);
                var anyFile = copyOfFileMaster.Any(
                    f =>
                        !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12 || f.FileTypeExtensionId == 9) &&
                        f.FileName.StartsWith(iName) &&
                        (f.Processed == 0));

                if (!anyFile) return Ok("File not found. " + iName);

                var icdFile = copyOfFileMaster.Find(
                    f =>
                        !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12 || f.FileTypeExtensionId == 9) &&
                        f.FileName.StartsWith(iName) &&
                        (f.Processed == 0));

                if (icdFile == null) return Ok("File not found. " + iName);
                var programFileLines = File.ReadAllLines(icdFile.FilePath).ToList();

                programFileLines.RemoveAll(s => s.Length <= 0);
                programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                // programFileLines = programFileLines.Select(s => s.CheckCommentInStatement()).ToList();
                programFileLines = programFileLines.AdjustMatReadLockedLikeStatements();
                // 
                var copyOfFileLines = programFileLines.ToList();
                programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                               && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                               !s.StartsWith("; *")).ToList();

                #region Correct all method blocks and statements...

                // Program file processing started...
                var programLineList = new List<string>();
                var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");

                var methodBlockDictionary = universeBasicV1.GetListFromCertainPointInclude(copyOfFileLines, lstAllGoSubs);

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
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines,
                            programFileLines[length], lstAllGoSubs, "RETURN", "STOP");
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        //methodBlock.Value.Insert(1, currentLine);
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
                var loopStart = loopIndicatorStart.Find(l => true);
                var loopEnd = loopIndicatorEnd.Find(l => true);
                var callInternal = callInternalIndicationStart.Find(c => true);
                var callExternal = callExternalIndicationStart.Find(c => true);
                var indexPosition = -1;
                var methodBusinessName = string.Empty;
                string businessName = null;
                var linesWithCommentedPart = programLineList.ToList();
                programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();
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
                            //BusinessName = businessName,
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

                    /*
                    if (line.StartsWith(methodStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = programFilePath.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = line.Split(' ').FirstOrDefault(),
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = methodStart.BaseCommandId,
                            PrimaryCommandId = methodStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId,
                            BusinessName =  methodBusinessName
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    */

                    if (line.StartsWith(loopStart.StartIndicator))
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
                            BaseCommandId = loopStart.BaseCommandId,
                            PrimaryCommandId = loopStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        businessName = string.Empty;
                        continue;
                    }
                    if (line.StartsWith(loopEnd.StartIndicator))
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
                            BaseCommandId = loopEnd.BaseCommandId,
                            PrimaryCommandId = loopEnd.PrimaryReferenceId,
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
                        /*
                        string methodCalled;
                        if (line.ContainsAll("@", "(", ")"))
                        {
                            methodCalled = line.Split('@').LastOrDefault();
                            if (methodCalled != null) methodCalled = methodCalled.Split('(').FirstOrDefault();
                        }
                        else
                            methodCalled = line.Split('@').LastOrDefault();
                        */
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

                var execOrCallSql = " Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                                    " AND BaseCommandId IN (6, 19); ";
                var callExternals =
                    await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execOrCallSql);

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

                        var className = pName[0].FilePath.Replace(pPath + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclared = projectName + "." + className + fileName;
                        cExternal.ClassCalled = classNameDeclared;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                        continue;
                    }
                    if (cExternal.BaseCommandId == 19)
                    {
                        var fileId = cExternal.FileId;
                        var pName =
                            copyOfFileMaster.ToList().Where(f => f.FileId == fileId).ToList();
                        var classNameProgram = pName[0].FilePath.Replace(pPath + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclaredProgram = projectName + "." + classNameProgram + fileNameProgram;
                        cExternal.ClassNameDeclared = classNameDeclaredProgram;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                    }
                }

                #endregion

                #region Update Method called for base command id = 6 in Jcl and program...

                var execSql = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                              " AND BaseCommandId = 6 AND ClassCalled is not null; ";
                var execName = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSql);
                foreach (var constructor in execName)
                {
                    // ReSharper disable once RedundantAssignment
                    var fName = constructor.ClassCalled.Split('.').LastOrDefault().Trim() + ".pgm";
                    var allCheckFiles = await _codeVortoService.FileMasterRepository
                        // ReSharper disable once AccessToModifiedClosure
                        .GetAllItems(f => (f.ProjectId == projectId) && (f.FileName == fName));

                    foreach (var files in allCheckFiles)
                    {
                        var methodSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                        " AND ProjectId = " + projectId + " AND BaseCommandId=8;";
                        var methodName = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(methodSql);

                        for (var iMethodName = 0; iMethodName < methodName.Count; iMethodName++)
                        {
                            string checkSql;
                            if (methodName.Count > 1)
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                           " AND ProjectId = " + projectId + " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and StatementId < " +
                                           methodName[iMethodName + 1].StatementId + " and BaseCommandId!=9;";
                            else
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                           " AND ProjectId = " + projectId + " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and BaseCommandId!=9;";

                            var checkCnt = await
                                baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(checkSql);
                            if (checkCnt.Count > 0)
                            {
                                constructor.MethodCalled = methodName[iMethodName].MethodName;
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }
                        }
                    }
                }

                #endregion

                #region Commentted region

                //#region Update ClassCalled field for BaseCommandId = 5

                //try
                //{
                //    var execSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId +
                //                     " AND BaseCommandId = 5 AND ClassCalled is null;";
                //    var execNameNew =
                //        await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNew);
                //    var includeFiles = allIncludesFiles.Select(s => s.CheckCommentInStatement()).ToList();
                //    var fileMasterInclude = new List<FileMaster>();
                //    foreach (var allIncludeFile in includeFiles)
                //    {
                //        // ReSharper disable once RedundantAssignment
                //        var iNewName = allIncludeFile.Split('>').LastOrDefault();
                //        var icdNewFile = copyOfFileMaster.Single(
                //            f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                //                 f.FileName.StartsWith(iName));
                //        fileMasterInclude.Add(icdFile);
                //    }
                //    var lstFIleIds = (from f in fileMasterInclude select f.FileId).ToList();
                //    var fIds = string.Join(",", lstFIleIds);
                //    var mysqlQuery = "Select * from  statementReferenceMaster where FileId in ( " + fIds +
                //                     ") and BaseCommandId In (8, 19); ";
                //    var allStatements = await
                //        _codeVortoService.StatementReferenceMasterRepository
                //            .GetDataFromSqlQuery<StatementReferenceMaster>(mysqlQuery);
                //    foreach (var constrctor in execNameNew)
                //        if (allStatements.ToList().Any(s => s.MethodName == constrctor.MethodCalled))
                //        {
                //            // ReSharper disable once RedundantAssignment
                //            var fileId = allStatements.Find(s => s.MethodName == constrctor.MethodCalled).FileId;
                //            var classCalled =
                //                allStatements.Single(d => (d.FileId == fileId) && (d.BaseCommandId == 19))
                //                    .ClassNameDeclared;
                //            constrctor.ClassCalled = classCalled;
                //            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constrctor);
                //        }
                //}
                //catch (Exception exception)
                //{
                //    Console.WriteLine(exception.Message);
                //}

                //#endregion

                #endregion

                #region Update field for basecommandId = 30

                if (!string.IsNullOrEmpty(icdFile.FilePath))
                {
                    var fileId = icdFile.FileId;
                    var execSqlPrgm = "select * from StatementReferenceMaster where ProjectId =" + projectId +
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

                icdFile.Processed = 1;
                icdFile.ProjectMaster = null;
                await _codeVortoService.FileMasterRepository.UpdateItem(icdFile);

                #endregion

                var ifCount = programLineList.Count(s => !string.IsNullOrEmpty(s) && s.StartsWith("IF"));
                var endIfCount =
                    programLineList.Count(s => !string.IsNullOrEmpty(s) && (s.EndsWith("END") || s.EndsWith("END IF")));

                programLineList.Add("==========================");
                programLineList.Add("IF Counts: " + ifCount);
                programLineList.Add("END Count: " + endIfCount);

                return Ok(programLineList);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') AND CreatedBy=1 AND Processed = 0 ; ");
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    //return Ok(projectId);
                    if (workflow.ProjectId == null) return NotFound();
                    int workFlowProjectId = workflow.ProjectId.Value;
                    IHttpActionResult processResult =
                        await GetWorkFlowWorkSpace(workFlowProjectId, workflow.MethodStatementId);
                    await processResult.ExecuteAsync(CancellationToken.None);

                    workflow.Processed = 1;
                    workflow.ProjectMaster = null;
                    await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow);
                    //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                    //string workflowData = await dataContent.Content.ReadAsStringAsync();
                }
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                //var lstTreeViewData = new List<TreeViewData>();
                List<TreeView> lstTreeView = new List<TreeView>();
                List<TreeView> secondTab = new List<TreeView>();

                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                        .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId && p.Processed == 0);
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = clsName},
                    new MySqlParameter("@classSplitName", MySqlDbType.VarChar)
                    {
                        Value = clsName.Split('.').LastOrDefault()
                    }
                };
                //Expression<Func<StatementReferenceMaster, bool>> expression =
                //    master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName || master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.ExecuteStoreProcedure<StatementReferenceMaster>
                        ("SpBaseStatementMaster", param);

                //lstStatementMaster.Add(baseStatementMaster);
                //lstNodes.Add(new Node { Name = clsName, Id = 1111, ParentId = 0, ShapeId = "RoundRect", Color = "#28c965", Height = "15", Width = clsName.Length.ToString() });

                var workflowRef = GetMethodBlock(stmtId);

                int bId = workflowRef[0].BaseCommandId;
                int statementId = workflowRef[0].StatementId;
                int treeNodeId = 1;
                // This is class name where that method is defined...
                lstTreeView.Add(new TreeView
                {
                    GraphId = "StartNode_1",
                    GraphName = "<span class='nodeToBold'>" + baseStatementMaster[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster[0].BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = clsName,
                    ActualStatementId = "Actual_" + baseStatementMaster[0].StatementId,
                    NodeId = treeNodeId,
                    CanBeParent = true,
                    IndentLevel = 0
                });
                // This is Method start statement...
                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + statementId,
                    GraphName = "<span class='nodeToBold'>" + workflowRef[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = bId,
                    StatementReferenceMaster = workflowRef[0],
                    ActualStatementId = "Actual_" + statementId,
                    NodeId = ++treeNodeId,
                    CanBeParent = true,
                    IndentLevel = 1
                });
                workflowRef.RemoveAt(0);

                lstTreeView.AddRange(workflowRef.Select(statementMaster => new TreeView
                {
                    ActualStatementId = "Actual_" + statementMaster.StatementId,
                    GraphId = "Node_" + statementMaster.StatementId,
                    GraphName = statementMaster.OriginalStatement,
                    HasChild = false,
                    SpriteCssClass = "",
                    ParentId = "MethodNode_" + statementId,
                    BaseCommandId = statementMaster.BaseCommandId,
                    PrimaryCommandId = statementMaster.PrimaryCommandId,
                    ClassCalled = statementMaster.ClassCalled,
                    MethodCalled = statementMaster.MethodCalled,
                    StatementReferenceMaster = statementMaster,
                    NodeId = ++treeNodeId,
                    CanBeParent = true,
                    IndentLevel = 2
                }));
                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);
                int auto = 0;
                int indentLevel = 3;
                var callingAndCalled = new Dictionary<string, List<string>>();
                foreach (var treeItem in lstTreeView)
                {
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;

                    auto++;
                    //copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                    //    ref auto, ref treeNodeId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                        treeItem.StatementReferenceMaster.FileId, indentLevel,
                        ref auto, ref treeNodeId, ref callingAndCalled);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                var newList = copyOfLstTreeView.ToList();

                foreach (var treeItem in newList)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                    auto++;
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                        indentLevel,
                        ref auto, ref treeNodeId, ref callingAndCalled);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                    /*
                    copyOfLstTreeView.Add(new TreeView
                    {
                        ParentId = treeItem.GraphId,
                        GraphId = "CallExt_" + treeItem.StatementReferenceMaster.StatementId,
                        HasChild = true,
                        GraphName = "<span class='nodemissingelement'>&nbsp;Loading...&nbsp;</span>",
                        BaseCommandId = 500,
                        SpriteCssClass = treeItem.StatementReferenceMaster.StatementId.ToString()
                    });
                    treeItem.HasChild = true;
                    */
                }

                #endregion

                #region Extra blocks...

                var indexPosition = -1;
                lstTreeView = copyOfLstTreeView;
                int ifCounter = 0;
                string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                int tempId = 0;
                int groupId = 1;
                foreach (
                    var treeItem in
                        copyOfLstTreeView.FindAll(t => t.BaseCommandId == 6 || t.BaseCommandId == 5).ToList())
                {
                    //string color = "#2d5b7" + indexPosition;                    
                    treeItem.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeItem.GraphName +
                                         "</span>";
                    var childItems =
                        (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
                    foreach (var child in childItems)
                    {
                        if (tempId > 3)
                            tempId = 0;
                        if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
                        {
                            string groupName = string.Empty;
                            if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                            {
                                string iOreCall = child.BaseCommandId == 6 ? "External Call" : "Internal Call";
                                string methodCalled = child.StatementReferenceMaster.MethodCalled.Substring(0,
                                    child.StatementReferenceMaster.MethodCalled.IndexOf('('));
                                string bName = child.StatementReferenceMaster.BusinessName;
                                if (string.IsNullOrEmpty(bName))
                                    bName = methodCalled;

                                groupName = "(" + bName.Trim() + ")(" +
                                            methodCalled + ")(" + iOreCall + ")";
                                groupId = groupId + 1;
                            }
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" +
                                                 treeItem.GraphName +
                                                 "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView, colorArray[tempId], groupName,
                                groupId);
                            tempId++;
                        }
                        else
                            child.GraphName = "<span style='color: green;'>" + child.GraphName +
                                              "</span>";
                    }
                    //tempId++;
                }
                /*
                var allOtherCommand = copyOfLstTreeView.FindAll(s => s.StatementReferenceMaster.OtherBaseCommandId == 5);
                var oChildItems =
                       (from s in copyOfLstTreeView where s.ParentId == allOtherCommand[0].GraphId select s).ToList();
                foreach (var cItem in oChildItems)
                {
                    var m = cItem.GraphName;
                }
                */

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                    if (treeItem.BaseCommandId != 1 && treeItem.PrimaryCommandId != 1) continue;

                    var treeViewList = new List<TreeView>();
                    for (int i = indexPosition; i < lstTreeView.Count; i++)
                    {
                        treeViewList.Add(lstTreeView[i]);
                        if (lstTreeView[i].BaseCommandId == 1 || lstTreeView[i].PrimaryCommandId == 1)
                            ifCounter++;
                        if (lstTreeView[i].BaseCommandId == 2 || lstTreeView[i].PrimaryCommandId == 2)
                            ifCounter--;
                        if (ifCounter == 0)
                            break;
                    }
                    var prevParentId = treeViewList.First().ParentId;
                    var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
                    treeViewList.First().GraphId = graphId;
                    for (int j = 1; j < treeViewList.Count; j++)
                    {
                        //if (treeViewList[j].ParentId == prevParentId)
                        //    treeViewList[j].ParentId = graphId;

                        if (treeViewList[j].ParentId != prevParentId) continue;
                        treeViewList[j].ParentId = graphId;

                        //if (treeViewList[j].BaseCommandId == 2) continue;
                        treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }


                indexPosition = -1;

                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.BaseCommandId != 10) continue;
                    int endIfCounter = -1;

                    var treeViewList = new List<TreeView>();
                    for (var i = indexPosition; i < lstTreeView.Count; i++)
                    {
                        treeViewList.Add(lstTreeView[i]);
                        if (lstTreeView[i].BaseCommandId == 1)
                            endIfCounter--;
                        if (lstTreeView[i].BaseCommandId == 2)
                            endIfCounter++;
                        if (endIfCounter == 0)
                            break;
                    }
                    int curIndentLevel = treeViewList.First().IndentLevel;
                    var prevParentId = treeViewList.First().ParentId;
                    var graphId = "ElseBlock" + indexPosition + treeItem.ActualStatementId;
                    treeViewList.First().GraphId = graphId;
                    treeViewList.First().IndentLevel = curIndentLevel + 1;
                    for (var j = 1; j < treeViewList.Count; j++)
                    {
                        if (treeViewList[j].BaseCommandId == 2 || treeViewList[j].BaseCommandId == 9) continue;
                        if (treeViewList[j].ParentId != prevParentId) continue;

                        treeViewList[j].ParentId = graphId;
                        //treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }

                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                secondTab.AddRange(copyOfLstTreeView
                    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 || item.BaseCommandId == 8 ||
                                   item.BaseCommandId == 1 || item.BaseCommandId == 25
                                   || item.PrimaryCommandId == 1 || item.BaseCommandId == 5 || item.BaseCommandId == 30
                                   || item.BaseCommandId == 10 || item.PrimaryCommandId == 2));
                var tempList =
                    (from d in secondTab
                     where d.BaseCommandId == 1
                           || d.BaseCommandId == 2
                           || d.BaseCommandId == 25
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
                    secondTab.AddRange(childItems);
                    /*
                    if (sTab.BaseCommandId == 2 || childItems.Count == 1)
                        secondTab.Remove(sTab);
                    if (childItems.Any(k => k.BaseCommandId == 25)) continue;
                    if (!childItems.Any() && sTab.StatementReferenceMaster == null)
                        secondTab.Remove(sTab);
                    */
                }
                secondTab = secondTab.Distinct().OrderBy(k => k.NodeId).ToList();
                /*
                var tempListNew = secondTab.ToList().FindAll(k => k.BaseCommandId == 1);
                foreach (var sTab in tempListNew)
                {
                    var allChilds = new List<TreeView> { sTab };
                    var childItems = (from s in secondTab where s.ParentId == sTab.GraphId select s).ToList();
                    if (!childItems.Any())
                        secondTab.Remove(sTab);
                    else
                    {
                        foreach (var c in childItems)
                        {
                            allChilds = AttachChildItems(allChilds, secondTab, c);
                        }
                        var s = (from child in allChilds where child.BaseCommandId == 6 || child.BaseCommandId == 5 select child).ToList();
                        if (s.Any()) continue;
                        foreach (var c in allChilds)
                        {
                            secondTab.Remove(c);
                        }
                    }
                }
                */

                #endregion

                #region

                var allSeqListItems = new List<TreeView>();
                foreach (var curItem in secondTab)
                {
                    allSeqListItems.Add(curItem);
                    var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                    foreach (var cItem in childItems)
                    {
                        allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                    }
                    break;
                }

                var generalRepositoryNodeDetails =
                    new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;");
                int nodeId = 1;
                if (workflowMaxNode.Any())
                {
                    nodeId = workflowMaxNode[0].MaxNodeId;
                    nodeId = nodeId + 1;
                }

                List<Node> listNodes = new List<Node>();
                List<Link> listLinks = new List<Link>();
                TreeViewData treeView = new TreeViewData(lstTreeView)
                {
                    Nodes = listNodes,
                    Links = listLinks
                };
                int widthCnt = Convert.ToInt32(secondTab.First().SpriteCssClass.Length.ToString()) * 3;
                if (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23 ||
                    secondTab.First().StatementReferenceMaster.PrimaryCommandId == 36)
                {
                    string[] strSplit = secondTab.First().SpriteCssClass.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    treeView.Nodes.Add(new Node
                    {
                        Id = nodeId,
                        Name = strName.ToUpper(),
                        ShapeId = "Circle",
                        Color = "#ffcc00",
                        Width = widthCnt.ToString(),
                        StatementId = Int32.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                        GroupName = secondTab.First().GroupName,
                        GroupId = secondTab.First().GroupId
                    });
                }
                else
                {
                    treeView.Nodes.Add(new Node
                    {
                        Id = nodeId,
                        Name = secondTab.First().SpriteCssClass,
                        ShapeId = "Circle",
                        Color = "#ffcc00",
                        Width = widthCnt.ToString(),
                        StatementId = Int32.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                        GroupName = secondTab.First().GroupName,
                        GroupId = secondTab.First().GroupId
                    });
                }

                allSeqListItems = allSeqListItems.Skip(1).ToList();
                var firstItem = allSeqListItems.First();
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList()
                        .Distinct();
                int linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    string nm = curItem.GraphName.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                    if (!string.IsNullOrEmpty(nm))
                    {
                        if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
                        {
                            string width = "0";
                            string height = "0";

                            #region PrimaryCommandId == 1 || BaseCommandId == 1

                            string condition;
                            string ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                            if (ifPart.Contains("THEN"))
                            {
                                condition = ifPart.Substring(0,
                                    ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                            }
                            else
                                condition = ifPart;

                            nodeId++;

                            int charCountOfText = condition.Length;
                            if (charCountOfText > 0)
                            {
                                int widthCalculation = (charCountOfText * 2) + 20;
                                if (widthCalculation > 200 || charCountOfText > 100)
                                {
                                }
                                else
                                {
                                    widthCalculation = widthCalculation * 1;
                                }

                                width = widthCalculation.ToString();
                                int heightCalculation;
                                if (charCountOfText > 100)
                                {
                                    heightCalculation = charCountOfText - 40;
                                }
                                else
                                {
                                    heightCalculation = charCountOfText;
                                }

                                if (heightCalculation < 30)
                                {
                                    heightCalculation = 35;
                                }
                                height = heightCalculation.ToString();
                            }

                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "Decision2",
                                Name = condition,
                                Color = "#ff6600",
                                Width = width,
                                Height = height,
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                //Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                            linkSeqNumber++;
                            var childItems =
                                (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, node,
                                    ref nodeId, ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 6)
                        {
                            #region BaseCommandId == 6

                            var item = curItem;
                            string nodeColor = "#c0c0c0";
                            var classNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                                .GetAllItems(s => s.BaseCommandId == 19
                                                  && s.ClassNameDeclared == item.StatementReferenceMaster
                                                      .ClassCalled.Split('.').LastOrDefault() ||
                                                  s.ClassNameDeclared == item.StatementReferenceMaster
                                                      .ClassCalled);
                            if (classNameDeclared.Count() != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if (curItem.PrimaryCommandId == 25 || curItem.PrimaryCommandId == 38)
                            {
                                string[] strSplit = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                                string strName = "";
                                if (strSplit.Length > 2)
                                {
                                    strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                }
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = strName.ToUpper(),
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            else
                            {
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = curItem.StatementReferenceMaster.ClassCalled,
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            treeView.Nodes.Add(node);
                            string m = curItem.StatementReferenceMaster.MethodCalled;
                            if (m != null)
                            {
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId =
                                        Int32.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1])
                                });
                            }
                            else
                            {
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                            }
                            linkSeqNumber++;
                            var childItems = (from s in secondTab
                                              where s.ParentId == curItem.GraphId
                                                    && s.BaseCommandId != 25
                                              select s).ToList().Distinct();
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView, node,
                                    ref nodeId, ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 5)
                        {
                            #region BaseCommandId == 5

                            var item = curItem;
                            string nodeColor = "#c0c0c0";
                            var methodCalled = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                                    curItem.StatementReferenceMaster.FileId + ";");
                            if (methodCalled.Count != 0)
                                nodeColor = "#00ffff";
                            nodeId++;
                            Node node;
                            if (curItem.PrimaryCommandId == 24 || curItem.PrimaryCommandId == 37)
                            {
                                string[] strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                                string strName = "";
                                if (strSplit.Length > 2)
                                {
                                    strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                }
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = strName.ToUpper(),
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            else
                            {
                                node = new Node
                                {
                                    Id = nodeId,
                                    ShapeId = "RoundRect",
                                    Name = methodCalled[0].ClassNameDeclared,
                                    Color = nodeColor,
                                    StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                    GroupName = curItem.GroupName,
                                    GroupId = curItem.GroupId
                                };
                            }
                            treeView.Nodes.Add(node);
                            string m = item.StatementReferenceMaster.MethodCalled;
                            if (m != null)
                            {
                                m = m + "()";
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId = item.StatementReferenceMaster.StatementId
                                });
                            }
                            else
                            {
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                            }
                            linkSeqNumber++;
                            var childItems =
                                (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView,
                                    node, ref nodeId, ref linkSeqNumber);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 30)
                        {
                            #region BaseCommandId == 30

                            nodeId++;
                            var width = _clsUniverseBasic.CalculateWidth(curItem.GraphName.Length);
                            var height = _clsUniverseBasic.CalculateHeight(curItem.GraphName.Length);
                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "Decision2",
                                Name = curItem.GraphName,
                                Color = "#ff6600",
                                Width = width,
                                Height = height,
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                            linkSeqNumber++;

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 10)
                        {
                            #region BaseCommandId == 10

                            nodeId++;
                            var parentIf =
                                (from p in secondTab where curItem.ParentId == p.GraphId select p).FirstOrDefault();
                            string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
                            var condition = ifPart.Contains("Then")
                                ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                                : ifPart;
                            condition = "If NOT " + condition + " Then ";
                            var width = _clsUniverseBasic.CalculateWidth(condition.Length);
                            var height = _clsUniverseBasic.CalculateHeight(condition.Length);
                            var node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "Decision2",
                                Name = condition,
                                Color = "#ff6600",
                                Width = width,
                                Height = height,
                                StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                                GroupName = curItem.GroupName,
                                GroupId = curItem.GroupId
                            };
                            treeView.Nodes.Add(node);
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                            linkSeqNumber++;

                            #endregion
                        }
                    }
                }

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>(s => s.MethodStatementId == statementId && s.ProjectId == projectId);

                #region Add to database table...

                var lstWorkflowNodeDetails = treeView.Nodes.Select(node => new WorkflowNodeDetails
                {
                    ProjectId = projectId,
                    BaseCommandId = node.BaseCommandId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    BusinessDescription = node.BusinessDescription,
                    BusinessName = node.BusinessName,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    ParentId = node.ParentId,
                    Id = node.Id,
                    StatementId = node.StatementId,
                    StatementTypeId = node.StatementTypeId,
                    ChildId = node.ChildId,
                    FileId = node.FileId,
                    Width = node.Width,
                    Name = node.Name.Replace("</span>", "").Trim(),
                    //Name = node.Name,
                    Height = node.Height,
                    ShapeId = node.ShapeId,
                    Color = node.Color,
                    MaxNodeId = nodeId,
                    GroupName = node.GroupName,
                    GroupId = node.GroupId
                }).ToList();
                generalRepositoryNodeDetails =
                    new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails);

                #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                var statementNodes =
                    await
                        generalRepositoryNodeDetails.GetAllItems(
                            p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                foreach (var stmt in statementNodes)
                {
                    if (stmt.ShapeId == "Decision" || stmt.ShapeId == "Decision2")
                    {
                        Regex word =
                            new Regex(@"\<span ([\w\(\)\\_\,\<\#\;\:\&\$\-\.\'\\+\=EQ\sEQUAL\sAND\sNOT\sOR\s\/]*)\>");
                        var removeHtmlTag = "";
                        foreach (Match match in word.Matches(stmt.Name))
                        {
                            removeHtmlTag = match.Value;
                            stmt.Name = stmt.Name.Replace(removeHtmlTag, "");
                        }

                        if (removeHtmlTag != "")
                        {
                            stmt.Name =
                                stmt.Name.Replace("</span>", "")
                                    .Replace(" LT", " is less than")
                                    .Replace(" GT", " is greater than")
                                    .Replace("GE", " is greater than or equal to")
                                    .Replace(" #", " not equal to") + "?";
                        }
                        else
                        {
                            stmt.Name =
                                stmt.Name.Replace(" LT", " is less than")
                                    .Replace(" GT", " is greater than")
                                    .Replace("GE", " is greater than or equal to")
                                    .Replace(" #", " not equal to") + "?";
                        }

                        await
                            generalRepositoryNodeDetails.UpdateItem(stmt);
                    }
                }

                #endregion

                var lstWorkflowLinkDetails = treeView.Links.Select(link => new WorkflowLinkDetails
                {
                    BaseCommandId = link.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    BusinessDescription = link.BusinessDescription,
                    BusinessName = link.BusinessName,
                    LinkText = link.LinkText,
                    Origin = link.Origin,
                    StatementId = link.StatementId,
                    Target = link.Target,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId
                }).ToList();

                var generalRepositoryLinkDetails = new GeneralRepository<WorkflowLinkDetails>(new AppDbContext());
                await generalRepositoryLinkDetails.BulkInsert(lstWorkflowLinkDetails);

                secondTab = secondTab.Distinct().ToList();
                var lstWorkflowTreeviewSecondTabDetails = secondTab.Select(sTab => new WorkflowTreeviewSecondTabDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    //GraphName = sTab.GraphName + "&nbsp;<img id='imgpseudo' src='../images/regex_icon.png' onclick='PseudoCodeDialog(" + sTab.ActualStatementId.Split('_')[1] + ")'/>",
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                var statementSecondTab =
                    await
                        generalRepositoryWorkflowTreeviewSecondTabDetails.GetAllItems(
                            p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                foreach (var stmt in statementSecondTab)
                {
                    if (stmt.BaseCommandId == 1 || stmt.BaseCommandId == 5 || stmt.BaseCommandId == 6)
                    {
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await
                            generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                    }
                }

                #endregion

                var lsTreeviewTabFirstDetails = copyOfLstTreeView.Select(fTab => new WorkflowTreeviewTabFirstDetails
                {
                    BaseCommandId = fTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild.ToString(),
                    MethodCalled = fTab.MethodCalled,
                    ParentId = fTab.ParentId,
                    PrimaryCommandId = fTab.PrimaryCommandId,
                    SpriteCssClass = fTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    IndentLevel = fTab.IndentLevel
                }).ToList();

                var generalRepositoryTreeviewTabFirstDetails =
                    new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());
                await generalRepositoryTreeviewTabFirstDetails.BulkInsert(lsTreeviewTabFirstDetails);

                #endregion

                /* Previous code to send workflow data for API call from UI.
                List<TreeViewData> lstData = new List<TreeViewData>();
                TreeViewData treeViewDataNew = new TreeViewData(secondTab)
                {
                    Nodes = treeView.Nodes,
                    Links = treeView.Links,
                    ActionWorkflows = actionWorkflow.First()
                };
                lstData.Add(treeViewDataNew);s
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */

                #region Decision Chart Code

                //IHttpActionResult decisionChartResult =
                //        await GetDecisionChart(projectId, stmtId);
                //await decisionChartResult.ExecuteAsync(CancellationToken.None);

                #endregion

                return Ok("Workflow data collected successfully");
            }
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
                    if (widthCalculation > 200 || charCountOfText > 100)
                    {
                    }
                    else
                    {
                        widthCalculation = widthCalculation * 1;
                    }

                    width = widthCalculation.ToString();
                    int heightCalculation;
                    if (charCountOfText > 100)
                    {
                        heightCalculation = charCountOfText - 40;
                    }
                    else
                    {
                        heightCalculation = charCountOfText;
                    }

                    if (heightCalculation < 30)
                    {
                        heightCalculation = 35;
                    }
                    height = heightCalculation.ToString();
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                string nodeColor = "#c0c0c0";
                string andCondition = " BaseCommandId = 19 AND " +
                                      " ( ClassNameDeclared = '" +
                                      item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                      " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                };
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                    .Result;
                if (classNameDeclared.Count() != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                //var node = new Node
                //{
                //    Id = nodeId,
                //    ShapeId = "RoundRect",
                //    Name = treeView.StatementReferenceMaster.ClassCalled,
                //    Color = nodeColor
                //};
                Node node;
                if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
                {
                    string[] strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = treeView.StatementReferenceMaster.ClassCalled,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems =
                    (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                        .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                        treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
                {
                    string[] strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node,
                        ref nodeId, ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("Then")
                    ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "If NOT " + condition + " Then ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;

                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();

                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;


                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
                    if (widthCalculation > 200 || charCountOfText > 100)
                    {
                    }
                    else
                    {
                        widthCalculation = widthCalculation * 1;
                    }

                    width = widthCalculation.ToString();
                    int heightCalculation;
                    if (charCountOfText > 100)
                    {
                        heightCalculation = charCountOfText - 40;
                    }
                    else
                    {
                        heightCalculation = charCountOfText;
                    }

                    if (heightCalculation < 30)
                    {
                        heightCalculation = 35;
                    }
                    height = heightCalculation.ToString();
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);

                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                if (string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                }
                else
                {
                    string nodeColor = "#c0c0c0";
                    string andCondition = " BaseCommandId = 19 AND " +
                                          " ( ClassNameDeclared = '" +
                                          item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                          " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;
                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#00ffff";
                    nodeId++;

                    Node node;
                    if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
                    {
                        string[] strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        string strName = "";
                        if (strSplit.Length > 2)
                        {
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        }
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId
                        };
                    }
                    else
                    {
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = treeView.StatementReferenceMaster.ClassCalled,
                            Color = nodeColor,
                            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    string m = treeView.StatementReferenceMaster.MethodCalled;
                    if (!string.IsNullOrEmpty(m))
                    {
                        m = m + "()";
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                            StatementId = treeView.StatementReferenceMaster.StatementId
                        });
                    }
                    else
                    {
                        treeViewData.Links.Add(new Link
                        {
                            //Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] "
                        });
                    }
                    linkSeqNumber++;
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                            .ToList().Distinct();
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node,
                            ref nodeId,
                            ref linkSeqNumber);
                    }
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                        treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
                {
                    string[] strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                try
                {
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node,
                            ref nodeId, ref linkSeqNumber);
                    }
                }
                catch (Exception)
                {
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("Then")
                    ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "If NOT " + condition + " Then ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                // treeView.GraphName.Split(new[] { "If" }, StringSplitOptions.None)[1];
                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
                    if (widthCalculation > 200 || charCountOfText > 100)
                    {
                    }
                    else
                    {
                        widthCalculation = widthCalculation * 1;
                    }

                    width = widthCalculation.ToString();
                    int heightCalculation;
                    if (charCountOfText > 100)
                    {
                        heightCalculation = charCountOfText - 40;
                    }
                    else
                    {
                        heightCalculation = charCountOfText;
                    }

                    if (heightCalculation < 30)
                    {
                        heightCalculation = 35;
                    }
                    height = heightCalculation.ToString();
                }

                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    //Origin = lastNode.Id,
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6

                var item = treeView;
                string nodeColor = "#c0c0c0";
                /*
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .GetAllItems(s => s.BaseCommandId == 19 && s.ProjectId == projectId
                                      && s.ClassNameDeclared == item.StatementReferenceMaster
                                          .ClassCalled.Split('.').LastOrDefault() ||
                                      s.ClassNameDeclared == item.StatementReferenceMaster
                                          .ClassCalled).Result;
                */
                string andCondition = " BaseCommandId = 19 AND " +
                                      " ( ClassNameDeclared = '" +
                                      item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                      " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "' ); ";
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                };
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                    .Result;

                if (classNameDeclared.Count() != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
                {
                    string[] strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = treeView.StatementReferenceMaster.ClassCalled,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems =
                    (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                        .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5

                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                        treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;

                Node node;
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
                {
                    string[] strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }

                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node,
                        ref nodeId, ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 8)
            {
                #region BaseCommandId == 8

                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                        treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;

                Node node;
                if (treeView.PrimaryCommandId == 23 || treeView.PrimaryCommandId == 36)
                {
                    string[] strSplit = methodCalled[0].ClassNameDeclared.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                    }
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = strName.ToUpper(),
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor,
                        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                }
                treeViewData.Nodes.Add(node);
                string m = string.Empty;
                if (treeView.PrimaryCommandId == 23 || treeView.PrimaryCommandId == 36)
                {
                    m = treeView.StatementReferenceMaster.MethodName;
                }
                else
                {
                    m = treeView.StatementReferenceMaster.MethodCalled;
                }
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                        StatementId = treeView.StatementReferenceMaster.StatementId
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] "
                    });
                }
                linkSeqNumber++;
                var childItems =
                    (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                        .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 30)
            {
                #region BaseCommandId == 30

                nodeId++;
                var width1 = _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var height1 =
                    _clsUniverseBasic.CalculateHeight(treeView.StatementReferenceMaster.OriginalStatement.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = treeView.GraphName,
                    Color = "#ff6600",
                    Width = width1,
                    Height = height1,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("Then")
                    ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "If NOT " + condition + " Then ";
                width = _clsUniverseBasic.CalculateWidth(condition.Length);
                height = _clsUniverseBasic.CalculateHeight(condition.Length);
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);

                treeViewData.Links.Add(new Link
                {
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;

                #endregion
            }
            return treeViewData;
        }

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView, string color,
            string groupName, int groupId)
        {
            treeView.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeView.GraphName +
                                 "</span>";
            var childItems =
                (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
            foreach (var child in childItems)
            {
                child.GraphName = "<span style='color: " + color + ";'>" + child.GraphName +
                                  "</span>";
                child.GroupName = groupName;
                child.GroupId = groupId;
                child.IndentLevel = child.IndentLevel + 2;
            }
        }

        private List<Link> RemoveMultipleLinks(List<Link> lstLinks, List<Node> lstNodes)
        {
            var copyOfLinks = lstLinks;
            var linksToRemove = new List<Link>();
            foreach (var node in lstNodes)
            {
                var childNodes = (from lnk in lstLinks
                                  where lnk.Origin == node.Id
                                  select lnk.Target
                                      into p
                                      select lstNodes.Find(n => n.Id == p)).ToList();
                if (childNodes.Count > 1)
                {
                    //List<Node> duplicates = childNodes.GroupBy(x => x)
                    //    .Where(g => g.Count() > 1).Select(g => g.Key).ToList();
                    var dupes =
                        childNodes.Where(a => childNodes.Except(new List<Node> { a }).Any(x => x.Name == a.Name)).ToList();
                    //List<Node> duplicates = childNodes.GetDuplicates().ToList();
                    bool hasChilds = false;
                    foreach (var dup in dupes)
                    {
                        var klm = (from f in lstLinks where f.Origin == dup.Id select f).ToList();
                        if (klm.Count > 0)
                            hasChilds = true;
                    }
                    if (!hasChilds)
                    {
                        var tempLinks = new List<Link>();
                        if (dupes.Count > 0 && dupes[0] != null)
                        {
                            foreach (var n in dupes)
                            {
                                var link = (from h in lstLinks
                                            where h.Origin == node.Id
                                                  && h.Target == n.Id
                                            select h).ToList();
                                tempLinks.AddRange(link);
                            }
                            // var links =(from h in lstLinks where h.Origin == node.Id select h).ToList();
                            if (!tempLinks.Any()) continue;

                            string linkText = tempLinks.Aggregate(String.Empty,
                                (current, jk) => current + jk.LinkText + ", ");
                            linkText = linkText.Substring(0, linkText.LastIndexOf(','));
                            tempLinks.First().LinkText = linkText;

                            foreach (var l in tempLinks)
                            {
                                if (l.LinkText == linkText) continue;
                                linksToRemove.Add(l);
                                //copyOfLinks.Remove(l);
                            }
                        }
                    }
                }
            }
            foreach (var l in linksToRemove)
            {
                copyOfLinks.Remove(l);
            }
            return copyOfLinks;
        }

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab,
            TreeView curItem)
        {
            allSeqListItems.Add(curItem);
            var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
            foreach (var cItem in childItems)
            {
                allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
            }
            return allSeqListItems;
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int indentLevel, ref int auto, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (treeView.ClassCalled == null) return lstTreeView;

                string className = treeView.ClassCalled;
                var callExtExpandedCode = GetGenericBlock(className);

                if (callExtExpandedCode.Count == 0)
                {
                    auto++;
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item == null)
                    {
                        lstTreeView.Add(new TreeView
                        {
                            ParentId = statememtId,
                            GraphId = "111" + auto + "_" + statememtId,
                            HasChild = true,
                            GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                            BaseCommandId = 25,
                            StatementReferenceMaster = treeView.StatementReferenceMaster,
                            NodeId = ++treeNodeId,
                            ActualStatementId = "Missing_99999999",
                            IndentLevel = indentLevel
                        });
                    }
                }
                else
                {
                    foreach (var statementMaster in callExtExpandedCode)
                    {
                        if (!string.IsNullOrEmpty(treeView.MethodCalled) &&
                            !string.IsNullOrEmpty(statementMaster.MethodName)
                            && treeView.MethodCalled != statementMaster.MethodName) continue;

                        //if (!string.IsNullOrEmpty(treeView.MethodCalled) &&
                        //    !string.IsNullOrEmpty(statementMaster.MethodName)
                        //    && treeView.MethodCalled == statementMaster.MethodName)

                        int blockStmtId = statementMaster.StatementId;

                        var stmtsBlock = GetMethodBlock(blockStmtId);
                        int chkparentIdMethod = 0;

                        var calledGoSubs =
                            (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        callingAndCalled.Add(treeView.MethodCalled + "_" + auto, calledGoSubs);

                        foreach (var block in stmtsBlock)
                        {
                            auto++;
                            if (block.BaseCommandId == 6)
                            {
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + auto + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel
                                });
                                //lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref auto, ref treeNodeId, ref callingAndCalled);
                            }
                            else if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
                            {
                                auto++;
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + auto + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel
                                });
                                indentLevel = indentLevel + 1;
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, block.FileId, indentLevel, ref auto, ref treeNodeId,
                                    ref callingAndCalled);
                                indentLevel = indentLevel - 1;
                            }

                            else if (block.BaseCommandId == 8 || block.OtherBaseCommandId == 8)
                            {
                                chkparentIdMethod = 1;
                                auto++;
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + auto + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel
                                });

                                statememtId = "Node" + auto + "_" + block.StatementId;
                                //lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref auto, ref treeNodeId);
                            }
                            else
                            {
                                if (chkparentIdMethod == 1)
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + auto + "_" + block.StatementId,
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                }
                                else
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + auto + "_" + block.StatementId,
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                }
                            }
                        }
                    }
                }
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int fileId, int indentLevel, ref int autoInt, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                string methodName = treeView.MethodCalled;
                object[] parameters =
                {
                    new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId},
                };
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters).Result;
                if (stmtMaster.Count == 0)
                {
                    autoInt++;
                    var item = lstTreeView.Find(p => p.ParentId == statememtId);
                    if (item == null)
                    {
                        #region For the only Universe Basic launguage

                        string graphName = string.Empty;
                        graphName = GetIncludeStatement(projectId, fileId, methodName);

                        #endregion

                        if (!string.IsNullOrEmpty(graphName))
                        {
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + autoInt + "_" + statememtId,
                                HasChild = true,
                                //GraphName = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>",
                                GraphName = graphName,
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                }
                else
                {
                    var callExtExpandedCode = GetGenericBlock(stmtMaster[0].StatementId, 8, 9);
                    if (callExtExpandedCode.Count == 0)
                    {
                        autoInt++;
                        var item = lstTreeView.Find(p => p.ParentId == statememtId);
                        if (item == null)
                        {
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + autoInt + "_" + statememtId,
                                HasChild = true,
                                GraphName = "<span class='nodemissingelement'>&nbsp;Definition Missing&nbsp;</span>",
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                    else
                    {
                        var calledGoSubs =
                            (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        string mName = methodName + "_" + autoInt;
                        callingAndCalled.Add(mName, calledGoSubs);
                        bool result = false;

                        foreach (var block in callExtExpandedCode)
                        {
                            autoInt++;
                            if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
                            {
                                foreach (var keyValPair in callingAndCalled.Where(s => s.Key == mName))
                                {
                                    var goSubs = keyValPair.Value;
                                    var block1 = block;
                                    foreach (var gosub in goSubs.Where(s => s.StartsWith(block1.MethodCalled)))
                                    {
                                        if (!callingAndCalled.Keys.Any(s => s.StartsWith(gosub))) continue;
                                        var gosub1 = gosub;
                                        var newList = (from d in callingAndCalled
                                                       where d.Key.StartsWith(gosub1)
                                                       select d.Value).FirstOrDefault();
                                        result = newList != null && newList.Any(s => s.StartsWith(methodName));
                                        if (result) break;
                                    }
                                }
                                if (result)
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + autoInt + "_" + block.StatementId,
                                        HasChild = true,
                                        GraphName =
                                            "<span class='nodeToBold'>" + block.OriginalStatement +
                                            " (Recursive call)</span>",
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                    result = false;
                                    continue;
                                }

                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + autoInt + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel
                                });
                                indentLevel = indentLevel + 1;
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, block.FileId, indentLevel, ref autoInt, ref treeNodeId,
                                    ref callingAndCalled);
                                indentLevel = indentLevel - 1;
                            }
                            else if (block.BaseCommandId == 6)
                            {
                                lstTreeView.Add(new TreeView
                                {
                                    ActualStatementId = "Actual_" + block.StatementId,
                                    ParentId = statememtId,
                                    GraphId = "Node" + autoInt + "_" + block.StatementId,
                                    HasChild = true,
                                    GraphName = "<span class='nodeToBold'>" + block.OriginalStatement + "</span>",
                                    BaseCommandId = block.BaseCommandId,
                                    PrimaryCommandId = block.PrimaryCommandId,
                                    MethodCalled = block.MethodCalled,
                                    ClassCalled = block.ClassCalled,
                                    StatementReferenceMaster = block,
                                    NodeId = ++treeNodeId,
                                    IndentLevel = indentLevel
                                });
                                //lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                //    lstTreeView, projectId, ref autoInt, ref treeNodeId, ref callingAndCalled);
                            }
                            else
                            {
                                if (stmtMaster[0].MethodName.Trim() != block.MethodName)
                                {
                                    lstTreeView.Add(new TreeView
                                    {
                                        ActualStatementId = "Actual_" + block.StatementId,
                                        ParentId = statememtId,
                                        GraphId = "Node" + autoInt + "_" + block.StatementId,
                                        HasChild = false,
                                        GraphName = block.OriginalStatement,
                                        BaseCommandId = block.BaseCommandId,
                                        PrimaryCommandId = block.PrimaryCommandId,
                                        MethodCalled = block.MethodCalled,
                                        ClassCalled = block.ClassCalled,
                                        StatementReferenceMaster = block,
                                        NodeId = ++treeNodeId,
                                        IndentLevel = indentLevel
                                    });
                                }
                            }
                        }
                    }
                }
                return lstTreeView;
            }
        }

        private List<StatementReferenceMaster> GetMethodBlock(int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                return workflowRef;
            }
        }

        private List<StatementReferenceMaster> GetGenericBlock(string className)
        {
            object[] parametersExp =
            {
                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
            };
            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
                .ContinueWith(t => t.Result).Result;
            return callExtExpandedCode;
        }

        private List<StatementReferenceMaster> GetGenericBlock(int stmtId, int startBaseCommandId, int endBaseCommandId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = startBaseCommandId},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = endBaseCommandId}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                return workflowRef;
            }
        }

        public static List<string> GetLanguageKeyword()
        {
            DataSet dsLanguageKeyword = new DataSet();
            List<string> lstLanguageKeywordsNew = new List<string>();
            string strQuery = "Select KeywordName from languagekeywords; ";
            var appBlock = new MySqlDbConnectionBaseClass();
            dsLanguageKeyword = appBlock.ExecuteNonQuery(strQuery, "");

            if (dsLanguageKeyword != null && dsLanguageKeyword.Tables.Count > 0)
            {
                lstLanguageKeywordsNew.AddRange(Enumerable.Select(dsLanguageKeyword.Tables[0].AsEnumerable(),
                    row => Convert.ToString(row[0])));
            }
            return lstLanguageKeywordsNew;
        }

        private List<StatementReferenceMaster> GetInsertStmtData(int projectId, int fileid, string insertStmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileid},
                    new MySqlParameter("@insertStmt", MySqlDbType.VarChar) {Value = insertStmt}
                };
                var dictionaryData = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetInsertStatementData", parameters)
                    .ContinueWith(t => t.Result).Result;
                return dictionaryData;
            }
        }

        public string GetIncludeStatement(int projectId, int fileId, string methodName)
        {
            string result = string.Empty;
            try
            {
                var languageType = _codeVortoService.ProjectMasterRepository
                    .GetDataFromSqlQuery<ProjectMaster>(
                        "select * from ProjectMaster where ProjectId=" + projectId + "");
                var languageId = languageType.Result[0].LanguageId;

                if (languageId == 5)
                {
                    var stmt = "$INSERT";
                    var insertStmtData = GetInsertStmtData(projectId, fileId, stmt);

                    if (insertStmtData.Count > 0)
                    {
                        for (int iInsertStmt = 0; iInsertStmt < insertStmtData.Count; iInsertStmt++)
                        {
                            var insertStmt = insertStmtData[iInsertStmt].OriginalStatement;
                            string[] splitInsert = insertStmt.Split('>');
                            if (splitInsert.Length > 1)
                            {
                                string fileName = splitInsert.Last();

                                #region Check the file is exit or not (12 is include file)

                                var checkFileExistOrNot = GetFileCount(fileName, projectId, 12, 0);
                                if (checkFileExistOrNot.Count > 0)
                                {
                                    for (int iCheckFileExistOrNot = 0;
                                        iCheckFileExistOrNot < checkFileExistOrNot.Count;
                                        iCheckFileExistOrNot++)
                                    {
                                        var checkMethodExitOrNot = GetMethodExistOrNot(projectId,
                                            checkFileExistOrNot[iCheckFileExistOrNot].FileId,
                                            methodName.Replace("(", "").Replace(")", "").Trim() + ":");
                                        if (checkMethodExitOrNot.Count <= 0) continue;

                                        result = insertStmt +
                                                 "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                                 checkFileExistOrNot[iCheckFileExistOrNot].FileId + ");'>[ " +
                                                 fileName + " ]</a>";
                                        break;
                                    }
                                }

                                #endregion
                            }

                            if (!string.IsNullOrEmpty(result))
                            {
                                break;
                            }
                        }

                        if (string.IsNullOrEmpty(result))
                        {
                            result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                        }
                    }
                }
                else
                {
                    result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                }
            }
            catch (Exception ex)
            {
                ex.ToString();
            }

            return result;
        }

        private List<StatementReferenceMaster> GetMethodExistOrNot(int projectId, int fileId, string stmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId},
                    new MySqlParameter("@statement", MySqlDbType.VarChar) {Value = stmt}
                };
                var fileData = _codeVortoService.FileMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetMethodExistOrNot", parameters)
                    .ContinueWith(t => t.Result).Result;
                return fileData;
            }
        }

        private List<FileMaster> GetFileCount(string fileName, int projectId, int fextension, int flag)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fName", MySqlDbType.VarChar) {Value = fileName},
                    new MySqlParameter("@fextension", MySqlDbType.VarChar) {Value = fextension},
                    new MySqlParameter("@flag", MySqlDbType.Int32) {Value = flag}
                };
                var fileData = _codeVortoService.FileMasterRepository
                    .ExecuteStoreProcedure<FileMaster>("SpGetFileData", parameters)
                    .ContinueWith(t => t.Result).Result;
                return fileData;
            }
        }

        [HttpGet]
        public object ReadUniverseBasicProgramNew()
        {
            var fileName = @"D:\Auctor\Programs\BP.BP\BP2500.pgm";
            var funcionName = Path.GetFileNameWithoutExtension(fileName);
            var programFileLines = File.ReadAllLines(fileName).ToList();
            programFileLines.RemoveAll(s => s.Length <= 0);
            programFileLines = programFileLines.Select(s => s.Trim()).ToList();
            programFileLines = programFileLines.Select(s => s.CheckCommentInStatement()).ToList();
            programFileLines = programFileLines.AdjustMatReadLockedLikeStatements();
            var copyOfFileLines = programFileLines.ToList();
            var firstOrDefault = programFileLines.Select(x => x).FirstOrDefault();
            var methodBlockDictionary = new Dictionary<string, List<string>>();
            if (firstOrDefault == null) return methodBlockDictionary;

            var businessName = firstOrDefault.Replace("*", "").Trim();
            programFileLines =
                programFileLines.Where(s => !s.StartsWith("*") && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                            !s.StartsWith("; *")).ToList();
            var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
            var universeBasicV1 = new UniverseBasicVersion1();
            methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(copyOfFileLines, lstAllGoSubs);
            var tempBlock = new List<string>();
            foreach (var l in programFileLines)
            {
                tempBlock.Add(l);
                if (l == "STOP")
                    break;
            }
            tempBlock.Insert(0, businessName);
            methodBlockDictionary.Add(funcionName, tempBlock);
            return methodBlockDictionary;
        }
    }
}