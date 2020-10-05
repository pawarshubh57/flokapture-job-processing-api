using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
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
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicVersion2Controller : ApiController
    {
        private AppDbContext _appDbContext;
        private ICodeVortoService _codeVortoService;

        public UniverseBasicVersion2Controller()
        {
        }

        public UniverseBasicVersion2Controller(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessUbProject(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return BadRequest("Project not found: " + projectId);
                stringBuilder.AppendLine(
          "========================================================================================");

                stringBuilder.AppendLine("\n" + "Started process for project: " + projectId + " project name: " +
                                       projectDetails.ProjectName + " and project physical path is: " +
                                       projectDetails.PhysicalPath);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId).ConfigureAwait(true);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                stringBuilder.AppendLine("\n" + "Extensions: " + string.Join(",", strExtensions));
                var projectPath = projectDetails.PhysicalPath;
                var solutionId = projectDetails.SolutionId;
                var directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);


                foreach (var directory in directoryList)
                {
                    try
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
                                ProjectId = projectId,
                                SolutionId = solutionId
                            };
                            lstFileMasters.Add(fileMaster);
                        }

                        enumerator.Dispose();
                        await _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
                        stringBuilder.AppendLine("\n" + "Total files scaned: " + lstFileMasters.Count);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                    if (projectDetails.ProjectConfigType == 7)
                        return Ok("Files extracted for Common Library type project for Universe Basic is completed.");
                }
                // return Ok(projectId);
                // var processResult = await StartParsingProcessUniverseBasic(projectId);
                // await processResult.ExecuteAsync(CancellationToken.None);
                //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                //var data = await dataContent.Content.ReadAsStringAsync();
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Project processed successfully. Project Id: " + projectId);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartParsingProcessUniverseBasic(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId).ConfigureAwait(false);

                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                var copyOfFileMasterNew = copyOfFileMaster
                    .Where(f => (f.FileTypeExtensionId == 10)
                                && (f.ProjectId == projectId)
                                && (f.Processed == 0)).ToList();
                stringBuilder.AppendLine("Started executing next process: ParseUniverseBasicFiles(" + projectId + ")");
                foreach (var copyFile in copyOfFileMasterNew)
                {
                    await ParseUniverseBasicFiles(copyFile.ProjectId).ConfigureAwait(false);
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("All files are processed successfully for Project Id: " + projectId);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseUniverseBasicFiles(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                int commanClassProjId = 9;

                #region Load Project and Base Command Reference Details...

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                // FileTypeExtensionId == 10 is for jcl files which is starting point of Universe Basics...
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == solutionId);
                var fileMasters = fileMaster as IList<FileMaster> ?? fileMaster.ToList();
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMasters.ToArray();
                stringBuilder.AppendLine("Total file for parsing: " + copyOfFileMaster.Count());
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
                // var projectName = projectDetails.ProjectName;
                var classStart = callClassIndicatorStart.Find(x => true);
                var classEnd = callClassIndicatorEnd.Find(x => true);
                var methodStart = methodIndicationStart.Find(x => true);
                var methodEnd = methodIndicationEnd.Find(x => true);
                var callExternalStart = callExternalIndicationStart.Find(x => true);

                #endregion

                // This is common for all statement that is ClassNameDeclared
                // var pPath = projectDetails.PhysicalPath;
                var currentJclFile = new FileMaster();

                #region Jcl file processing...

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();

                foreach (var fMaster in copyOfFileMaster
                    .Where(f => (f.FileTypeExtensionId == 10) && (f.Processed == 0) && (f.ProjectId == projectId)))
                {
                    stringBuilder.AppendLine("Started reading file lines file name: " + fMaster.FileName +
                                               " and file path is: " + fMaster.FilePath);
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
                    stringBuilder.AppendLine("Started executing next process: PrepareJclFile(" + fMaster.FileId + "," + copyOfFileMaster.ToList() + ")");
                    var fileStatements = universeBasicV1.PrepareJclFile(fMaster.FileId, copyOfFileMaster.ToList(),
                        lstFileLines, callExternalStart.PrimaryReferenceId, "RUN", "CALL");

                    lstStatementReferenceMaster.AddRange(fileStatements);
                    // End of statement reference...
                    stringBuilder.AppendLine("Started executing next process: PrepareStatementReferenceMasterEnd(" + fMaster + "," + classEnd.PrimaryReferenceId + "," + methodEnd.PrimaryReferenceId + ")");

                    var closeList = universeBasicV1.PrepareStatementReferenceMasterEnd(fMaster,
                        classEnd.PrimaryReferenceId, methodEnd.PrimaryReferenceId);
                    lstStatementReferenceMaster.AddRange(closeList);
                    lstStatementReferenceMaster.ForEach(p => { p.ProjectId = projectId; });
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    // Update file status to processed = 1...
                    fMaster.Processed = 1;
                    fMaster.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(fMaster);
                    currentJclFile = fMaster;
                    // file = fMaster;
                    // As we are intended to process one Jcl file (Starting point) at a time, so break the loop and exit...
                    break;
                }

                // Pickup statement from above list which has a base command id = 6.
                var statementReferenceMaster = lstStatementReferenceMaster.FindAll(s => s.BaseCommandId == 6).ToList();
                //var statementReferenceMaster =
                //    lstStatementReferenceMaster.FirstOrDefault(s => s.BaseCommandId == 6);
                if (statementReferenceMaster.Count == 0)
                {
                    return Ok("This Jcl file has no call External.");
                }

                #endregion

                foreach (var lstStateref in statementReferenceMaster)
                {
                    var oStatement = lstStateref.OriginalStatement.Trim();
                    var programName = oStatement.Split(' ')[2];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => (programName != null)
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9) && (f.Processed == 0));

                    if (!anyFile)
                        continue;
                    var programFilePath = copyOfFileMaster.ToList()
                        .Find(f => (programName != null)
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9) && (f.Processed == 0));

                    if (string.IsNullOrEmpty(programFilePath.FilePath)) return Ok("File Not Found");

                    var programFileLines = File.ReadAllLines(programFilePath.FilePath).ToList();

                    programFileLines.RemoveAll(s => s.Length <= 0);
                    programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                    // programFileLines = programFileLines.Select(s => s.CheckCommentInStatement()).ToList();
                    stringBuilder.AppendLine("Started executing next process: AdjustMatReadLockedLikeStatements(" + projectId +
                                               ")");
                    programFileLines = programFileLines.AdjustMatReadLockedLikeStatements();
                    // 
                    var copyOfFileLines = programFileLines.ToList();
                    stringBuilder.AppendLine("Started executing next process: GetAllIncludeStatements(" + projectId +
                                             ")");
                    var allIncludesFiles = programFileLines.GetAllIncludeStatements("$INSERT");
                    programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                                   && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                                   !s.StartsWith("; *")).ToList();
                    programFileLines = programFileLines.Where(a => !a.StartsWith("$INSERT")).ToList();

                    #region Process Include file statements...

                    foreach (var includeFile in allIncludesFiles)
                    {
                        var iName = includeFile.Split('>').LastOrDefault();
                        stringBuilder.AppendLine("Started executing next process: ParseCallAndIncludesFilesUniverse(" + iName +
                                           "," + projectDetails.LanguageId + "," + solutionId + ")");
                        await
                            ParseCallAndIncludesFilesUniverse(iName, copyOfFileMaster.ToList(),
                                projectDetails.LanguageId, solutionId);
                    }

                    #endregion

                    #region Process Call file statements... (BaseCommand Id= 6)
                    stringBuilder.AppendLine("Started executing next process: GetAllIncludeStatements(CALL)");
                    var allCallFiles = programFileLines.GetAllIncludeStatements("CALL").Distinct().ToList();
                    foreach (var callFile in allCallFiles)
                    {
                        try
                        {
                            var iName = callFile.Split('@').LastOrDefault();
                            var iExactFileName = iName.Split('(').FirstOrDefault();
                            stringBuilder.AppendLine("Started executing next process: ParseCallAndIncludesFilesUniverse(" + iExactFileName +
                                       "," + projectDetails.LanguageId + "," + solutionId + ")");
                            await
                                ParseCallAndIncludesFilesUniverse(iExactFileName,
                                    copyOfFileMaster.ToList(), projectDetails.LanguageId, solutionId);
                        }
                        catch (Exception exception)
                        {
                            Console.WriteLine(exception.Message);
                        }
                    }

                    #endregion

                    #region Correct all method blocks and statements...

                    // Program file processing started...

                    var programLineList = new List<string>();
                    stringBuilder.AppendLine("Started collecting all GOSUB: GetAllGoSub");
                    var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                    stringBuilder.AppendLine("Started collecting all certainpoint: GetListFromCertainPoint");
                    var methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(programFileLines, lstAllGoSubs);
                    var tempBlock = new List<string>();

                    foreach (var dict in methodBlockDictionary)
                    {
                        if (dict.Key != "STOP") continue;

                        tempBlock = dict.Value;
                        break;
                    }
                    if (tempBlock.Count <= 0)
                        tempBlock = programFileLines.GetListUpToLastStop();

                    programFileLines.RemoveRange(0, tempBlock.Count);

                    tempBlock.Insert(0, copyOfFileLines.FirstOrDefault().Replace("*", ""));
                    tempBlock.Insert(1, programName + ":");
                    methodBlockDictionary.Add(programName + ":", tempBlock);
                    lstAllGoSubs.Add(programName + ":");

                    programLineList.InsertRange(0, tempBlock);
                    programLineList.InsertRange(2, allIncludesFiles.ToList());

                    //var startedBlock = false;
                    int methodIndexPosition = -1;
                    foreach (string pLine in programFileLines)
                    {
                        var currentLine = pLine;
                        methodIndexPosition = methodIndexPosition + 1;
                        if (!lstAllGoSubs.Any(l => currentLine.StartsWith(l))) continue;

                        //startedBlock = true;
                        var firstOrDefault = currentLine.Split('*').FirstOrDefault();
                        if (firstOrDefault != null)
                            currentLine = firstOrDefault.Trim();
                        stringBuilder.AppendLine("Started collecting all methodblocks: PickUpAllMethodBlocks");
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines, currentLine);
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        //methodBlock.Value.Insert(1, currentLine);
                        programLineList.AddRange(methodBlockOriginal);
                        if (methodBlock.Value == null)
                        {
                            Console.WriteLine("Method Block is empty...");
                            // Console.ReadLine();
                        }
                        if (methodBlock.Value != null)
                            programLineList.AddRange(methodBlock.Value);
                        //length = length + methodBlockOriginal.Count - 1;
                        //continue;
                        //if (startedBlock) continue;

                        //programLineList.Add(currentLine);
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
                    int indexPosition;
                    var methodBusinessName = string.Empty;
                    string businessName = null;

                    programLineList = programLineList.Where(s => !string.IsNullOrEmpty(s)).ToList();
                    var linesWithCommentedPart = programLineList.ToList();
                    programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();

                    // Here check whether file has multiple STOP... 
                    // Find out index position of last stop...
                    // Till that time don't close first method by base command id = 9
                    var stopLinesCount = programLineList.Count(l => l == "STOP");
                    var lastIndexOfStop = 0;
                    if (stopLinesCount > 0)
                    {
                        var indPos = -1;
                        foreach (var line in programLineList)
                        {
                            indPos++;
                            if (line == "STOP") stopLinesCount--;
                            if (stopLinesCount != 0) continue;

                            lastIndexOfStop = indPos;
                            break;
                        }
                    }
                    stringBuilder.AppendLine("Started dump statement into database of file: " + programFilePath.FileId);
                    indexPosition = -1;
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
                                businessName =
                                    CultureInfo.CurrentCulture.TextInfo.ToTitleCase(businessName.ToLower());
                                businessName = businessName.Replace("&", "AND")
                                    .Replace("&&", "AND")
                                    .Replace("||", "OR");
                            }
                        }

                        if (
                            line.StartsWith(
                                lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                            continue;

                        if (line.StartsWith("RETURN"))
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
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (line == "STOP")
                        {
                            var bCommandId = 0;
                            var pCommandId = 0;
                            if (indexPosition < lastIndexOfStop)
                            {
                                bCommandId = 0;
                                pCommandId = 0;
                            }
                            else if (indexPosition >= lastIndexOfStop)
                            {
                                bCommandId = methodEnd.BaseCommandId;
                                pCommandId = methodEnd.PrimaryReferenceId;
                            }

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
                                BaseCommandId = bCommandId,
                                PrimaryCommandId = pCommandId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                                BaseCommandId = methodStart.BaseCommandId,
                                PrimaryCommandId = methodStart.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                                PrimaryCommandId =
                                    ifEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

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
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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

                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                        }
                    }

                    #endregion

                    var statmentRefEnd = universeBasicV1.PrepareStatementReferenceMasterEnd(programFilePath, 43);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);
                    stringBuilder.AppendLine("Started executing next process: ParseDataDictionaryReferenceCode(" + projectId + "," + allCallFiles + "," + programFilePath + ")");
                    var processDataDictionaryResult =
                        await ParseDataDictionaryReferenceCode(projectId, allCallFiles, programFilePath);
                    await processDataDictionaryResult.ExecuteAsync(CancellationToken.None);

                    #region Update Class starting points with Menu names

                    stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetAllClassNamesWithCondition(" +
                                             projectId + ",)");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var allClassStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNamesWithCondition",
                            parameters);

                    using (_appDbContext = new AppDbContext())
                    {
                        var jclMenuName = await _appDbContext.UniverseFileMenu
                            .ToListAsync().ContinueWith(t =>
                            {
                                var result = t.Result;
                                return result;
                            });
                        foreach (var statementRef in allClassStatements)
                        {
                            // ReSharper disable once RedundantAssignment
                            var sRef = statementRef.OriginalStatement;
                            var menuName = (from menu in jclMenuName
                                            where menu.ActionExecuted == sRef
                                            select menu.MenuTitle).ToList().FirstOrDefault();

                            if (string.IsNullOrEmpty(menuName)) continue;

                            statementRef.ResolvedStatement = menuName;
                            statementRef.OriginalStatement = menuName;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef);
                        }
                    }

                    #endregion

                    #region Update program File Processed
                    stringBuilder.AppendLine("Started update program file Processed: (" + projectId + ")");
                    programFilePath.Processed = 1;
                    programFilePath.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(programFilePath);

                    foreach (var callFile in allCallFiles)
                    {
                        var iName = callFile.Split('@').LastOrDefault();
                        // ReSharper disable once RedundantAssignment
                        var iExactFileName = iName.Split('(').FirstOrDefault();
                        var anyFileNew = copyOfFileMaster.ToList().Any(
                            f =>
                                !string.IsNullOrEmpty(iExactFileName) &&
                                ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                                f.FileName.StartsWith(iExactFileName) &&
                                (f.Processed == 0));

                        if (!anyFileNew)
                        {
                            anyFileNew = copyOfFileMaster.ToList().Any(
                                f =>
                                    !string.IsNullOrEmpty(iExactFileName) &&
                                    ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                                    f.FileName.StartsWith(iExactFileName) && f.SolutionId == solutionId &&
                                    (f.Processed == 0));
                            if (!anyFileNew)
                                continue;
                        }

                        var icdFileThis = copyOfFileMaster.ToList().Find(f =>
                            !string.IsNullOrEmpty(iExactFileName) && ((f.FileTypeExtensionId == 12)
                                                                      || (f.FileTypeExtensionId == 9)) &&
                            f.FileName.StartsWith(iExactFileName) &&
                            (f.Processed == 0))
                                          ?? copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) &&
                                              ((f.FileTypeExtensionId == 12)
                                               || (f.FileTypeExtensionId == 9)) &&
                                              f.FileName.StartsWith(iExactFileName) &&
                                              (f.ProjectId == projectId) &&
                                              (f.Processed == 0));
                        if (icdFileThis == null)
                            // continue;
                            icdFileThis = copyOfFileMaster.ToList().Find(f =>
                                !string.IsNullOrEmpty(iExactFileName) && ((f.FileTypeExtensionId == 12)
                                                                          || (f.FileTypeExtensionId == 9)) &&
                                f.FileName.StartsWith(iExactFileName) &&
                                (f.Processed == 0))
                                          ?? copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) &&
                                              ((f.FileTypeExtensionId == 12)
                                               || (f.FileTypeExtensionId == 9)) &&
                                              f.FileName.StartsWith(iExactFileName) && f.SolutionId == solutionId &&
                                              (f.Processed == 0));

                        icdFileThis.Processed = 1;
                        icdFileThis.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(icdFileThis);
                    }

                    #endregion

                    #region update Includes file Processed
                    stringBuilder.AppendLine("Started update Includes file Processed: (" + projectId + ")");
                    foreach (var includeFile in allIncludesFiles)
                    {
                        // ReSharper disable once RedundantAssignment
                        var iName = includeFile.Split('>').LastOrDefault();

                        //var icdFile = copyOfFileMaster.Single(
                        //    f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                        //         f.FileName.StartsWith(iName));

                        /*Added Shubhangi*/
                        //fileMaster = await _codeVortoService.FileMasterRepository
                        //    .GetAllItems(p => p.ProjectId == projectId || p.ProjectId == commanClassProjId);
                        //copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                        var icdFile = copyOfFileMaster.ToList().FindAll(
                            f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                 f.FileName.StartsWith(iName) && f.Processed == 0);
                        if (!icdFile.Any())
                            continue;

                        foreach (var iFile in icdFile)
                        {
                            iFile.Processed = 1;
                            iFile.ProjectMaster = null;
                            await _codeVortoService.FileMasterRepository.UpdateItem(iFile);
                        }
                    }

                    #endregion
                    stringBuilder.AppendLine(
                    "Started update classcalled a for base command Id = 6 where recordsource statement: (" +
                    projectId + ")");
                    #region Update ClassCalled field for BaseCommandId = 5

                    try
                    {
                        var execSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId +
                                         " AND BaseCommandId = 5 AND ClassCalled is null;";


                        /* Added Shubhangi */

                        //var execSqlNew = "Select * from StatementReferenceMaster where IN (" + projectId + "," +
                        //                 commanClassProjId + ") AND BaseCommandId = 5 AND ClassCalled is null;";

                        var execNameNew =
                            await
                                baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                    execSqlNew);
                        if (execNameNew.Count == 0)
                        {
                            var execSqlNew1 = "Select * from StatementReferenceMaster where ProjectId=" +
                                              commanClassProjId +
                                              " AND BaseCommandId = 5 AND ClassCalled is null;";
                            execNameNew =
                                await
                                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                        execSqlNew1);
                        }

                        var includeFiles = allIncludesFiles.Select(s => s.CheckCommentInStatement()).ToList();
                        var fileMasterInclude = new List<FileMaster>();
                        foreach (var allIncludeFile in includeFiles)
                        {
                            // ReSharper disable once RedundantAssignment
                            var iName = allIncludeFile.Split('>').LastOrDefault();
                            var anyFileNew = copyOfFileMaster.Any(
                                f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                     f.FileName.StartsWith(iName));

                            if (!anyFileNew)
                            {
                                anyFileNew = copyOfFileMaster.Any(
                                    f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                         f.FileName.StartsWith(iName) && f.SolutionId == solutionId);
                                // (f.ProjectId == projectId || f.ProjectId == commanClassProjId));
                                if (!anyFileNew)
                                    continue;
                            }

                            var icdFile = copyOfFileMaster.ToList().Find(
                                f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                     f.FileName.StartsWith(iName))
                                          ?? copyOfFileMaster.ToList().Find(
                                              f => !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12) &&
                                                   f.FileName.StartsWith(iName) && f.SolutionId == solutionId);
                            //(f.ProjectId == projectId || f.ProjectId == commanClassProjId));
                            if (icdFile == null)
                                continue;


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
                        Console.WriteLine("==================================");
                    }

                    #endregion
                    stringBuilder.AppendLine(
                 "Started update $INSERT abd CALL statement: (" + projectId + "," + programFilePath.FileId + ")");
                    #region Update $INSERT and CALL statement (To apply the link)

                    try
                    {
                        var execCallInsertSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId + "" +
                                                   " and OriginalStatement like 'CALL %' and FileId=" + programFilePath.FileId + ";";

                        var execInsertAndCallNew = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(execCallInsertSqlNew);

                        foreach (var stat in execInsertAndCallNew)
                        {
                            string fName;
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
                            {
                            }
                            else
                            {
                                var icdFile = copyOfFileMaster.Single(
                                    f =>
                                        !string.IsNullOrEmpty(fName) &&
                                        (f.FileTypeExtensionId == 9) &&
                                        f.FileName.StartsWith(fName));

                                var neLink = stat.OriginalStatement +
                                             "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                             icdFile.FileId + ");'>[ " + fName + " ]</a>";
                                stat.OriginalStatement = neLink;
                                await _codeVortoService.StatementReferenceMasterRepository
                                    .UpdateItem(stat).ConfigureAwait(false);
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(ex.Message);
                    }

                    #endregion
                    stringBuilder.AppendLine(
                    "Started update classcalled and methodcalled for basecommandId = 30: (" +
                    projectId + ")");
                    #region Update field for basecommandId = 30

                    if (!string.IsNullOrEmpty(programFilePath.FilePath))
                    {
                        var fileId = programFilePath.FileId;
                        var execSqlPrgm = "select * from StatementReferenceMaster where ProjectId =" + projectId +
                                          " AND FileId = " + fileId + " AND BaseCommandId IN (0, 1);";

                        var execNamePrgm = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(execSqlPrgm).ConfigureAwait(false);

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
                            await _codeVortoService.StatementReferenceMasterRepository
                                .UpdateItem(aboveLine).ConfigureAwait(false);
                        }
                    }

                    #endregion
                }

                #region Update ClassCalled field for StatementReferenceMaster...
                stringBuilder.AppendLine("Started update classcalled for statement: (" + projectId + ")");
                var execOrCallSql = " Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                                    " AND BaseCommandId IN (6, 19) AND (ClassCalled is null AND ClassNameDeclared IS NULL) " +
                                    " AND OriginalStatement NOT LIKE '%CALL.%'";


                var callExternals = await baseCommandReferenceRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(execOrCallSql).ConfigureAwait(false);

                foreach (var cExternal in callExternals)
                {
                    if (cExternal.BaseCommandId == 6)
                    {
                        // ReSharper disable once RedundantAssignment
                        string thisProgramName = null;
                        if (cExternal.OriginalStatement.ContainsAll("@", "("))
                        {
                            var pgName = cExternal.OriginalStatement.Split('@').LastOrDefault();
                            if (pgName != null) thisProgramName = pgName.Split('(').FirstOrDefault();
                        }
                        else
                        {
                            var pgmName = cExternal.OriginalStatement.Split(' ');
                            if (pgmName.Length > 2) thisProgramName = pgmName[2];
                        }

                        if (!string.IsNullOrEmpty(thisProgramName))
                        {
                            var pName =
                                copyOfFileMaster.ToList().Where(f => !string.IsNullOrEmpty(thisProgramName)
                                                                     && f.FileName.StartsWith(thisProgramName) &&
                                                                     (f.FileTypeExtensionId == 9)).ToList();
                            if (!pName.Any()) continue;
                            var projectDetails1 =
                                _codeVortoService.ProjectMasterRepository.GetItem(pName[0].ProjectId);
                            var pNameNew = projectDetails1.ProjectName;
                            var pPathNew = projectDetails1.PhysicalPath;
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
                    }
                    if (cExternal.BaseCommandId == 19)
                    {
                        var pName =
                            copyOfFileMaster.ToList().Where(f => f.FileId == cExternal.FileId).ToList();
                        var projectDetails1 =
                            _codeVortoService.ProjectMasterRepository.GetItem(pName[0].ProjectId);
                        var pNameNew = projectDetails1.ProjectName;
                        var pPathNew = projectDetails1.PhysicalPath;
                        var classNameProgram = pName[0].FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        var fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        var classNameDeclaredProgram = pNameNew + "." + classNameProgram + fileNameProgram;
                        cExternal.ClassNameDeclared = classNameDeclaredProgram;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal).ConfigureAwait(false);
                    }
                }

                #endregion

                #region Update Method called for base command id = 6 in Jcl and program...
                stringBuilder.AppendLine("Started update methodcalled for Jcl and program basecommandId = 6: (" + projectId + ")");
                string sqlCallExt = " Select * from StatementReferenceMaster Where ProjectId = " + projectId + "" +
                                    " AND BaseCommandId = 6 AND IFNULL(ClassCalled = '', TRUE) AND MethodCalled IS NULL " +
                                    " AND (OriginalStatement NOT LIKE '%CALL %' AND OriginalStatement NOT LIKE '%CALL.%'); ";

                var execName = await baseCommandReferenceRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlCallExt).ConfigureAwait(false);
                foreach (var constructor in execName)
                {
                    // ReSharper disable once RedundantAssignment
                    if (string.IsNullOrEmpty(constructor.OriginalStatement)) continue;

                    var oStatement = constructor.OriginalStatement.Trim();
                    if (oStatement.Split(' ').Length <= 2) continue;

                    // ReSharper disable once RedundantAssignment
                    var programName = oStatement.Split(' ')[2];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => (programName != null)
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9) && f.SolutionId == 5);

                    if (!anyFile) continue;
                    var allCheckFiles = copyOfFileMaster.ToList()
                        .FindAll(f => (programName != null)
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9) && f.SolutionId == 5);

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
                            /*
                            string checkSql;
                            if (methodName.Count > 1)
                                checkSql = " Select * from statementreferencemaster where FileId = " +
                                           files.FileId +
                                           " AND ProjectId = " + methodName[iMethodName].ProjectId +
                                           " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and StatementId < " +
                                           methodName[iMethodName + 1].StatementId + " and BaseCommandId != 9;";
                            else
                                checkSql = " Select * from statementreferencemaster where FileId = " +
                                           files.FileId +
                                           " AND ProjectId = " + methodName[iMethodName].ProjectId +
                                           " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and BaseCommandId != 9;";

                            var checkCnt = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(checkSql).ConfigureAwait(false);
                            if (checkCnt.Count <= 0) continue;
                            */
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

                #region Insert the data in ViewSourceMaster Table...

                /*
                foreach (var file in fileMasters)
                {
                    var allLines = File.ReadAllLines(file.FilePath);
                
                    StringBuilder strBuilder = new StringBuilder();
                    foreach (var stmt in allLines)
                    {
                        strBuilder.AppendLine(stmt);
                    }
                    var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                    ViewSourceMaster viewsourcedata = new ViewSourceMaster()
                    {
                        ViewSourceId = 0,
                        FileId = file.FileId,
                        SourceData = strBuilder.ToString()
                    };
                    await viewSourceMasterRepository.AddNewItem(viewsourcedata);
                }
                */

                #endregion
                stringBuilder.AppendLine("Started executing next process: ProcessAllActionWorkflows(" + projectId + "," + currentJclFile.FileName + ")");
                LogMessage.WriteLogMessage(stringBuilder);
                await ProcessAllActionWorkflows(projectId, currentJclFile).ConfigureAwait(false);

                //var processStartingPointResult = await GetAllStartingPoints(projectId);
                //await processStartingPointResult.ExecuteAsync(CancellationToken.None);

                return Ok("File Processed");
            }
        }

        public async Task<IHttpActionResult> ParseCallAndIncludesFilesUniverse(string iName,
            List<FileMaster> copyOfFileMaster, int languageId, int solutionId)
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
                var methodEnd = methodIndicationEnd.Find(x => true);

                var anyFile = copyOfFileMaster.Any(
                    f =>
                        !string.IsNullOrEmpty(iName) && ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                        f.FileName.StartsWith(iName) &&
                        (f.Processed == 0));

                if (!anyFile)
                {
                    var fileMasterNew = await
                        _codeVortoService.FileMasterRepository.GetAllItems(
                            p => p.SolutionId == solutionId);
                    copyOfFileMaster = new List<FileMaster>(fileMasterNew as FileMaster[] ?? fileMasterNew.ToArray());
                    anyFile = copyOfFileMaster.Any(
                        f =>
                            !string.IsNullOrEmpty(iName) &&
                            ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                            f.FileName.StartsWith(iName) &&
                            f.SolutionId == solutionId &&
                            (f.Processed == 0));
                    if (!anyFile)
                        return Ok("File not found. " + iName);
                }
                var icdFile = copyOfFileMaster.Find(
                    f =>
                        !string.IsNullOrEmpty(iName) && ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                        f.FileName.StartsWith(iName) &&
                        (f.Processed == 0));

                if (icdFile == null)
                {
                    icdFile = copyOfFileMaster.Find(
                        f =>
                            !string.IsNullOrEmpty(iName) &&
                            ((f.FileTypeExtensionId == 12) || (f.FileTypeExtensionId == 9)) &&
                            f.FileName.StartsWith(iName) && f.SolutionId == solutionId &&
                            (f.Processed == 0));
                    if (icdFile == null)
                        return Ok("File not found. " + iName);
                }

                #endregion

                var programFileLines = File.ReadAllLines(icdFile.FilePath).ToList();
                int projectId = icdFile.ProjectId;
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
                stringBuilder.AppendLine("Started collecting all GOSUB: GetAllGoSub");
                var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                Dictionary<string, List<string>> methodBlockDictionary;
                if (icdFile.FileTypeExtensionId == 9)
                {
                    stringBuilder.AppendLine("Started collecting all certainpointProgram: GetListFromCertainPoint");
                    methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(copyOfFileLines,
                        lstAllGoSubs);
                }
                else
                {
                    stringBuilder.AppendLine("Started collecting all certainpointInclude: GetListFromCertainPoint");
                    methodBlockDictionary = universeBasicV1.GetListFromCertainPointInclude(copyOfFileLines,
                        lstAllGoSubs);
                }

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
                        stringBuilder.AppendLine("Started collecting all methodblocks: PickUpAllMethodBlocks");
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
                stringBuilder.AppendLine("Started update classcalled and methodcalled for basecommandId = 6 & 19 : (" + projectId + ")");
                //var execOrCallSql = " Select sm.* from StatementReferenceMaster as sm " +
                //                    " Inner Join FileMaster as fm ON sm.FileId = fm.FileId Where sm.ProjectId = " +
                //                    projectId + "  AND fm.Processed = 0 AND sm.BaseCommandId IN (6, 19); ";

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
                stringBuilder.AppendLine("Started update methodcalled for Jcl and program basecommandId = 6 : (" + projectId + ")");
                //var execSql = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                //              " AND BaseCommandId = 6 AND ClassCalled is not null; ";
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
                            /*
                            string checkSql;
                            if (methodName.Count > 1)
                                checkSql = " Select * from statementreferencemaster where FileId = " +
                                           files.FileId +
                                           " AND ProjectId = " + methodName[iMethodName].ProjectId +
                                           " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and StatementId < " +
                                           methodName[iMethodName + 1].StatementId + " and BaseCommandId != 9;";
                            else
                                checkSql = " Select * from statementreferencemaster where FileId = " +
                                           files.FileId +
                                           " AND ProjectId = " + methodName[iMethodName].ProjectId +
                                           " AND StatementId > " +
                                           methodName[iMethodName].StatementId + " and BaseCommandId != 9;";

                            var checkCnt = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(checkSql).ConfigureAwait(false);
                            if (checkCnt.Count <= 0) continue;
                            */
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
                stringBuilder.AppendLine("Started update field for basecommandId = 30 : (" + projectId + ")");
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

                stringBuilder.AppendLine("Started update Include program File Processed: (" + projectId + ")");
                if (icdFile.FileTypeExtensionId != 12) return Ok("Done");
                icdFile.Processed = 1;
                icdFile.ProjectMaster = null;
                await _codeVortoService.FileMasterRepository.UpdateItem(icdFile).ConfigureAwait(false);

                #endregion
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Done");
            }
        }

        [HttpGet]
        public string GetIncludeStatement(int projectId, int fileId, string methodName)
        {
            string result;
            var stmt = "$INSERT";
            var insertStmtData = GetInsertStmtData(projectId, fileId, stmt);
            var stringBuilder = new StringBuilder();
            if (insertStmtData.Count > 0)
            {
                var includeFileIds = new List<int>();
                using (_codeVortoService = new CodeVortoService())
                {
                    var fileMasterData = _codeVortoService.FileMasterRepository
                        .GetAllItems(f => f.FileTypeExtensionId == 12).Result.ToList();

                    foreach (var t in insertStmtData)
                    {
                        var insertStmt = t.OriginalStatement;
                        var splitInsert = insertStmt.Split('>');
                        var fileName = splitInsert.Last();
                        var anyFile = fileMasterData.Any(
                            f => !string.IsNullOrEmpty(fileName) &&
                                 (f.FileTypeExtensionId == 12) &&
                                 f.FileName.StartsWith(fileName) &&
                                 (f.Processed == 1));

                        if (!anyFile) continue;

                        var icdFile = fileMasterData.Find(
                            f => !string.IsNullOrEmpty(fileName) && (f.FileTypeExtensionId == 12) &&
                                 f.FileName.StartsWith(fileName) &&
                                 (f.Processed == 1));
                        if (icdFile != null) includeFileIds.Add(icdFile.FileId);
                    }
                }
                var includeFIles = string.Join(",", includeFileIds);
                if (string.IsNullOrEmpty(includeFIles))
                    return "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";

                //var query =
                //    " Select sm.* from StatementReferenceMaster as sm Where sm.ProjectId = " + projectId +
                //    " AND sm.FileId in (" + includeFIles + ") and sm.BaseCommandId = 8  AND sm.MethodName = '" +
                //    methodName + "';";

                stringBuilder.AppendLine("Called stored procedure: SpFindExternalCallMethodName(" + projectId + "," + methodName + "," + includeFIles + ")");
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                    new MySqlParameter("@fileId", MySqlDbType.Text) {Value = includeFIles}
                };

                var chkStatementExistOrNot = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindExternalCallMethodName", parameters)
                    .Result;

                if (chkStatementExistOrNot.Any())
                {
                    var statementReferenceMaster = chkStatementExistOrNot.FirstOrDefault();
                    if (statementReferenceMaster != null)
                    {
                        result =
                            "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                            statementReferenceMaster.FileId + ");'>[ " +
                            methodName + " ]</a>";
                        return result;
                    }
                }
            }
            else
            {
                result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                return result;
            }
            result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
            LogMessage.WriteLogMessage(stringBuilder);
            return result;
        }

        private List<StatementReferenceMaster> GetInsertStmtData(int projectId, int fileid, string insertStmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine("Called stored procedure: SpUbGetInsertStatementData(" + projectId + "," + fileid + "," + insertStmt + ")");
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileid},
                    new MySqlParameter("@insertStmt", MySqlDbType.VarChar) {Value = insertStmt}
                };
                var dictionaryData = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUbGetInsertStatementData", parameters)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return dictionaryData;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActionWorkflows(int projectId, FileMaster cFile)
        {
            #region Added the Action Workflow Table...
            var stringBuilder = new StringBuilder();
            if (string.IsNullOrEmpty(cFile.FileName) || (cFile.FileId == 0))
                return Ok("Action Workflows processed successfully");
            // Action Workflows data...
            var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
            Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
            var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
            var projectType = projectMasterData.ProjectConfigType;
            if (projectType != 8) return Ok("Action Workflows processed successfully"); //Remove Shubhangi

            var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
            var projectConfigData = projectConfig.GetItem(projectType);
            if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
            // Means the project type is windows application and we need to read starting point from that 
            // files respective code behind file...
            // var configFileName = projectConfigData.ToString();
            // if (string.IsNullOrEmpty(configFileName)) return Ok("Process completed successfully"); //Remove Shubhangi
            try
            {
                var actionWorkflowsRepository = new ActionWorkflowsRepository(new AppDbContext());
                var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                var allProcessedWorkflows = await actionWorkflowsRepository
                    .GetAllItems(r => (r.ProjectId == projectId) && (r.Processed == 1));

                var processedWorkflows = allProcessedWorkflows.ToList();


                stringBuilder.AppendLine("Called stored procedure: SpGetMethodData(" + projectId + "," + cFile.FileId + ")");
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = cFile.FileId}
                };

                var genericBlocks =
                    await statementReferenceRepository.ExecuteStoreProcedure<StatementReferenceMaster>(
                        "SpGetMethodData", param);
                foreach (var statement in genericBlocks)
                {
                    if (processedWorkflows.Any(s => statement.StatementId == s.MethodStatementId)) continue;

                    var actionWorkflow = new ActionWorkflows
                    {
                        ActionWorkflowId = 0,
                        CreatedBy = 1,
                        EndPointOrService = "Batch",
                        MethodStatementId = statement.StatementId,
                        OriginFileName = Path.GetFileName(cFile.FilePath),
                        OriginFilePath = cFile.FilePath,
                        ProjectId = projectId,
                        OriginEventMethod = statement.OriginalStatement,
                        OriginObject = projectMasterData.ProjectName +
                                       cFile.FilePath
                                           .Replace(".jcl", "")
                                           .Replace(projectMasterData.PhysicalPath, "")
                                           .Trim()
                                           .Replace("\\", "."),
                        WorkflowName = statement.OriginalStatement,
                        ServiceBaseAddress = null,
                        ServiceContract = null,
                        WorkflowBusinessName = statement.BusinessName,
                        IsDeleted = 0,
                        ReasonAboutDisable = null,
                        Processed = 0
                    };
                    var aWorkflow = await actionWorkflowsRepository.AddNewItem(actionWorkflow).ConfigureAwait(false);

                    #region Update Class starting points with Menu names

                    using (_appDbContext = new AppDbContext())
                    {
                        var jclMenuName = await _appDbContext.UniverseFileMenu
                            .ToListAsync().ContinueWith(t =>
                            {
                                var result = t.Result;
                                return result;
                            });
                        var menuName = (from menu in jclMenuName
                                        where menu.ActionExecuted == aWorkflow.OriginEventMethod
                                              || menu.ActionExecuted == aWorkflow.WorkflowName
                                        select menu.MenuTitle).ToList().FirstOrDefault();
                        if (string.IsNullOrEmpty(menuName)) continue;

                        aWorkflow.OriginEventMethod = menuName;
                        aWorkflow.WorkflowName = menuName;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(aWorkflow);
                    }

                    #endregion
                }
            }

            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }

            stringBuilder.AppendLine("Action workflows processed successfully:(" + projectId + "," + cFile.FileName + ")");
            LogMessage.WriteLogMessage(stringBuilder);
            return Ok("Action Workflows processed successfully");

            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseDataDictionaryReferenceCode(int projectId,
            List<string> allProgramNames, FileMaster programFile)
        {
            #region region 2...Data Dictionary Reference Code & Include (Subroutine) file logic

            try
            {
                var programNames =
                    (from callFile in allProgramNames
                     select callFile.Split('@').LastOrDefault()
                         into iName
                         where iName != null
                         select iName.Split('(').FirstOrDefault()).ToList();
                var allFileIds = new List<int>();
                using (_codeVortoService = new CodeVortoService())
                {
                    var query = "Select * from FileMaster Where FileTypeExtensionId = 9;";
                    var copyOfFileMaster =
                        await _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(query);
                    foreach (var iName in programNames)
                    {
                        var anyFile = copyOfFileMaster.Any(
                            f =>
                                !string.IsNullOrEmpty(iName) &&
                                f.FileName.StartsWith(iName) &&
                                (f.Processed == 0));

                        if (!anyFile) continue;

                        var icdFile = copyOfFileMaster.Find(
                            f =>
                                !string.IsNullOrEmpty(iName) &&
                                f.FileName.StartsWith(iName) &&
                                (f.Processed == 0));

                        if (icdFile != null) allFileIds.Add(icdFile.FileId);
                    }
                }
                allFileIds.Add(programFile.FileId);
                var fileId = string.Join(",", allFileIds);
                using (_appDbContext = new AppDbContext())
                {
                    var dictionaryData = await _appDbContext.DataDictionary.ToListAsync();

                    var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var dataDictionarySql =
                        " SELECT state.* from fileMaster as fm, statementreferencemaster state where fm.FileId = state.FileId " +
                        " AND fm.FileTypeExtensionId = 9 AND fm.ProjectId = " + projectId +
                        " AND  state.BaseCommandId Not In (8, 9, 2, 5, 6, 19, 20) AND  OriginalStatement like '%)%' AND fm.FileId IN (" +
                        fileId + ") AND fm.Processed = 0;";
                    var dataDictionaryNew = await
                        baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql);
                    var regExMatch = new Regex(@"([A-Z0-9]\.)\w+([A-Z0-9]\.)\w+(\([\[0-9]*\))");

                    var allDataDictStatements = dataDictionaryNew
                        .Where(s => regExMatch.IsMatch(s.OriginalStatement)).ToList();

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
    }
}