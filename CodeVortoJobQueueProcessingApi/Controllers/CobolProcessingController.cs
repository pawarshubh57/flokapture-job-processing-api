
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Formatting;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.CobolVersion;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;


namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public CobolProcessingController()
        {

        }

        public CobolProcessingController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        //[HttpGet]
        //public async Task<IHttpActionResult> ExecuteProcessActionsOneByOne(int projectId)
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {

        //    }
        //}

        [HttpGet]
        public async Task<HttpResponseMessage> StartCobolProcessing(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null)
                {
                    return new HttpResponseMessage(HttpStatusCode.BadRequest)
                    {
                        Content = new StringContent("Project not found: " + projectId)
                    };
                }
                var entensionList =
                        await
                            _codeVortoService.FileTypeExtensionRepository.GetAllListItems(
                                p => p.LanguageId == projectDetails.LanguageId).ConfigureAwait(true);
                var strExtensions = new List<string>();
                strExtensions.AddRange(entensionList.Select(extension => extension.FileExtension));
                var lstFileMasters = new List<FileMaster>();
                var projectPath = projectDetails.PhysicalPath;
                var solutionId = projectDetails.SolutionId;
                var directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId)
                            .ConfigureAwait(false);
                foreach (var directory in directoryList)
                {
                    try
                    {
                        var allFiles = Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                                .Where(s => strExtensions.Any(s.EndsWith));

                        var enumerator = allFiles.GetEnumerator();
                        while (enumerator.MoveNext())
                        {
                            var currentFile = enumerator.Current;
                            if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                            var fileName = Path.GetFileName(currentFile);
                            if (string.IsNullOrEmpty(fileName)) continue;
                            var extension = Path.GetExtension(currentFile);
                            var extensionId =
                                entensionList.First(e => e.FileExtension == extension).FileTypeExtensionId;
                            var fileLines = File.ReadAllLines(currentFile).ToList();
                            int fileLinesCount = fileLines.Count(l => !string.IsNullOrWhiteSpace(l));
                            var fileMaster = new FileMaster
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = currentFile,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                DoneParsing = 0,
                                LinesCount = fileLinesCount
                            };
                            lstFileMasters.Add(fileMaster);
                        }
                        enumerator.Dispose();
                        await
                            _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
                        await StartParseProcessingCobol(projectId);
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine(exception.InnerException);
                        return new HttpResponseMessage(HttpStatusCode.InternalServerError)
                        {
                            Content =
                                new ObjectContent(typeof(Exception), exception.InnerException,
                                    new JsonMediaTypeFormatter())
                        };
                    }
                }

                return new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new StringContent("Cobol processing is done")
                };
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartParseProcessingCobol(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);
                    int languageId = projectDetails.LanguageId;
                    var copyOfFileMasters =
                        await _codeVortoService.FileMasterRepository.GetAllListItems(p => p.ProjectId == projectId);

                    #region Start reading file lines and do process before dump to database

                    foreach (var fMaster in copyOfFileMasters.Where(f =>
                                    (f.FileTypeExtensionId == 6 || f.FileTypeExtensionId == 7 || f.FileTypeExtensionId == 8) && f.Processed == 0))
                    {
                        var allLines = File.ReadAllLines(fMaster.FilePath).ToList();

                        if (fMaster.FileTypeExtensionId == 7 && fMaster.Processed == 0)
                        {
                            await ParseJclFile(projectId, languageId, allLines, fMaster);
                            continue;
                        }
                        if (fMaster.FileTypeExtensionId == 8 && fMaster.Processed == 0)
                        {
                            await ParseProcFile(projectId, languageId, allLines, fMaster);
                            continue;
                        }
                        if (fMaster.FileTypeExtensionId == 6 && fMaster.Processed == 0)
                        {
                            await ParseCblFile(projectId, languageId, allLines, fMaster);
                        }

                    }
                    await ProcessCobolProject(projectId);

                    #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseJclFile(int projectId, int languageId, List<string> tempBlock, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var allLines = tempBlock;
                    allLines = allLines.RemoveStartCharacters(2);
                    allLines = allLines.RemoveAllCommentedLines("*");
                    allLines = allLines.RemoveAllEmptyLines();
                    allLines = allLines.CombineAllBrokenLines(',', false);
                    allLines = allLines.StatementTrim();
                    string methodBusinessName = string.Empty;
                    #region Get all primary command and baseCommand information to parse file

                    var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await generalRepository.GetAllItems().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });

                    var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var ifConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var endIfConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var methodStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var callExternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callInternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                    var methodStart = methodStartIndicator.Find(x => true);
                    var ifStart = ifConditionIndicator.Find(x => true);
                    var endIfStart = endIfConditionIndicator.Find(x => true);
                    var callExternal = callExternalIndicator.FindAll(x => true);
                    var callInternal = callInternalIndicator.Find(x => true);
                    var loopStart = loopIndicatorStart.Find(x => true);
                    var loopEnd = loopIndicatorEnd.Find(x => true);

                    #endregion

                    #region Add Default Entry for BaseCommandId =19
                    ClsCobolProcess clsCobolProcess = new ClsCobolProcess();
                    var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                        callClassIndicatorStart[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                    #endregion
                    /*
                var cobolKeyWordList = await _codeVortoService.LanguageKeywordRepository
                    .GetAllListItems(x => x.languageId == languageId).ConfigureAwait(false);
                List<string> cobolKeyWords = cobolKeyWordList.Select(keyWord => keyWord.KeywordName).ToList();
                */
                    #region Insert statement into statementmaster table


                    foreach (var line in allLines)
                    {
                        /*
                        if (methodList.Any(a => line.StartsWith(a)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = filemaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = line.Split('.').FirstOrDefault() + "()",
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
                            continue;
                        } */
                        if (line.StartsWith(ifStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = ifStart.BaseCommandId,
                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                BusinessName = methodBusinessName,
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }

                        if (line == "END ELSE" || line == "ELSE")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 10,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(endIfStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = endIfStart.BaseCommandId,
                                PrimaryCommandId = endIfStart.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (callExternal.Any(l => line.Contains(l.StartIndicator)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = callExternal.Find(s => line.Contains(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId = callExternal.Find(s => line.Contains(s.StartIndicator)).PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(callInternal.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = line.Split(' ').LastOrDefault() + "()",
                                VariableNameDeclared = null,
                                BaseCommandId = callInternal.BaseCommandId,
                                PrimaryCommandId = callInternal.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(loopStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopStart.BaseCommandId,
                                PrimaryCommandId = loopStart.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(loopEnd.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopEnd.BaseCommandId,
                                PrimaryCommandId = loopEnd.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line == "END")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 9,
                                PrimaryCommandId = 30,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                        else
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 0,
                                PrimaryCommandId = 0,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                    }

                    #endregion

                    #region Add Default entry for basecommandId = 20

                    var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                        callClassIndicatorEnd[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

                    #endregion

                    await ProcessAllActioWorkflows(projectId, fileMaster);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProcFile(int projectId, int languageId, List<string> tempBlock, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var allLines = tempBlock;
                    allLines = allLines.RemoveStartCharacters(2);
                    allLines = allLines.RemoveAllCommentedLines("*");
                    allLines = allLines.RemoveAllEmptyLines();
                    allLines = allLines.CombineAllBrokenLines(',', false);
                    allLines = allLines.StatementTrim();
                    string methodBusinessName = string.Empty;

                    #region Get all primary command and baseCommand information to parse file

                    var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await generalRepository.GetAllItems().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });

                    var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var ifConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var endIfConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var methodStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var callExternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callInternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                    var ifStart = ifConditionIndicator.Find(x => true);
                    var endIfStart = endIfConditionIndicator.Find(x => true);
                    var callExternal = callExternalIndicator.FindAll(x => true);
                    var callInternal = callInternalIndicator.Find(x => true);
                    var loopStart = loopIndicatorStart.Find(x => true);
                    var loopEnd = loopIndicatorEnd.Find(x => true);

                    #endregion

                    #region Add Default Entry for BaseCommandId =19

                    ClsCobolProcess clsCobolProcess = new ClsCobolProcess();
                    var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                        callClassIndicatorStart[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                    #endregion

                    /*
                var cobolKeyWordList = await _codeVortoService.LanguageKeywordRepository
                    .GetAllListItems(x => x.languageId == languageId).ConfigureAwait(false);
                List<string> cobolKeyWords = cobolKeyWordList.Select(keyWord => keyWord.KeywordName).ToList();
                */
                    #region Insert statement into statementmaster table
                    foreach (var line in allLines)
                    {
                        /*
                    if (methodList.Any(a => line.StartsWith(a)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = filemaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = line.Split('.').FirstOrDefault() + "()",
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
                        continue;
                    } */
                        if (line.StartsWith(ifStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = ifStart.BaseCommandId,
                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                BusinessName = methodBusinessName,
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }

                        if (line == "END ELSE" || line == "ELSE")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 10,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(endIfStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = endIfStart.BaseCommandId,
                                PrimaryCommandId = endIfStart.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (callExternal.Any(l => line.Contains(l.StartIndicator)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = callExternal.Find(s => line.Contains(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId =
                                    callExternal.Find(s => line.Contains(s.StartIndicator)).PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(callInternal.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = line.Split(' ').LastOrDefault() + "()",
                                VariableNameDeclared = null,
                                BaseCommandId = callInternal.BaseCommandId,
                                PrimaryCommandId = callInternal.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(loopStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopStart.BaseCommandId,
                                PrimaryCommandId = loopStart.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(loopEnd.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopEnd.BaseCommandId,
                                PrimaryCommandId = loopEnd.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line == "END")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 9,
                                PrimaryCommandId = 30,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                        else
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 0,
                                PrimaryCommandId = 0,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                    }

                    #endregion

                    #region Add Default entry for basecommandId = 20

                    var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                        callClassIndicatorEnd[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

                    #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseCblFile(int projectId, int languageId, List<string> tempBlock, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var mainListBlock = new List<string>();
                    var allLines = tempBlock;
                    allLines = allLines.RemoveStartCharacters(6);
                    allLines = allLines.RemoveEndCharacters(66);
                    allLines = allLines.RemoveAllCommentedLines("*");
                    allLines = allLines.RemoveAllCommentedLines("/");
                    allLines = allLines.RemoveAllEmptyLines();
                    allLines = allLines.RemoveAll("SKIP", "EJECT");
                    var allLinesOfProcedureDivision = allLines.GetAllDataBetweenProcedureDivision();
                    var methodList = allLinesOfProcedureDivision.GetAllMethods("PERFORM");
                    allLinesOfProcedureDivision = allLinesOfProcedureDivision.ModifyAllLinesMethodsName(methodList);
                    // var newLines = allLinesOfProcedureDivision.GetUptoStopRunStatement();
                    methodList = methodList.RemoveDotFromMethodName();
                    var methodsLines = allLinesOfProcedureDivision.CollectAllMethodsData(methodList);
                    mainListBlock.AddRange(allLinesOfProcedureDivision);
                    var allLinesOfWorkingStorageSection = allLines.GetAllDataBetweenWorkingStorageSection();
                    allLinesOfWorkingStorageSection =
                        allLinesOfWorkingStorageSection.CombineAllLinesOfWorkingStorageSection();
                    await InsertWorkingStorageSection(projectId, languageId, allLinesOfWorkingStorageSection, fileMaster);
                    string methodBusinessName = string.Empty;
                    #region Get all primary command and baseCommand information to parse file

                    var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await generalRepository.GetAllItems().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });

                    var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var ifConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var endIfConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var methodStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var callExternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                    var callInternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                    var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                    var methodStart = methodStartIndicator.Find(x => true);
                    var ifStart = ifConditionIndicator.Find(x => true);
                    var endIfStart = endIfConditionIndicator.Find(x => true);
                    var callExternal = callExternalIndicator.FindAll(x => true);
                    var callInternal = callInternalIndicator.Find(x => true);
                    var loopStart = loopIndicatorStart.FindAll(x => true);
                    var loopEnd = loopIndicatorEnd.FindAll(x => true);

                    #endregion
                    var clsCobolProcess = new ClsCobolProcess();

                    var cobolKeyWordList = await _codeVortoService.LanguageKeywordRepository
                     .GetAllListItems(x => x.languageId == languageId).ConfigureAwait(false);
                    var cobolKeyWords = cobolKeyWordList.Select(keyWord => keyWord.KeywordName).ToList();

                    #region Add Default Entry for BaseCommandId =19
                    var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                        callClassIndicatorStart[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);
                    #endregion

                    #region Insert statement into statementmaster table
                    var programFileLines = new List<string>();

                    #region MethodList
                    foreach (var keyValue in methodsLines)
                    {
                        var mainKey = keyValue.Key;
                        if (string.IsNullOrEmpty(mainKey)) continue;
                        programFileLines.Add(mainKey);
                        var mainLines = keyValue.Value;
                        mainLines = mainLines.ReplaceStatement("GO TO ", "PERFORM ");
                        mainLines = mainLines.CombineLineForMoveToStatement(cobolKeyWords);
                        mainLines = mainLines.ConvertAllMoveStatement(cobolKeyWords);
                        mainLines = mainLines.AddEndIfStatement();
                        mainLines = mainLines.AddNewLineForMultipleKeyoword(cobolKeyWords);
                        mainLines = mainLines.CombineAllExecSqlStatements();
                        mainLines = mainLines.CombineAllLines(methodList, cobolKeyWords);
                        mainLines = mainLines.SplitAllLinesAfterDot();
                        mainLines = mainLines.ConversionOfEvaluateStatement();
                        mainLines = mainLines.ConversionOfExecStatements();
                        mainLines = mainLines.ConvertionOfCrudActivity();
                        var cobolVariableList = await _codeVortoService.CobolVariableRepository.GetAllListItems(
                            x => x.ProjectId == projectId && x.FileId == fileMaster.FileId).ConfigureAwait(false);
                        mainLines = mainLines.PerfromVarying(methodList, cobolVariableList);
                        mainLines.VerifyMethodBlockForIfWithMatchingEndIfCount();
                        var lastItemOfList = mainLines.LastOrDefault();
                        if (mainKey != "PROCEDURE DIVISION")
                        {
                            if (!mainLines.Any(x => x == "STOP RUN" || x == "STOP RUN."))
                                if (lastItemOfList == ".")
                                {
                                    mainLines.Remove(mainLines.Last());
                                    mainLines.Add("END");
                                }
                                else
                                    mainLines.Add("END");
                        }
                        foreach (var mLine in mainLines)
                        {
                            if (string.IsNullOrEmpty(mLine)) continue;
                            programFileLines.Add(mLine);
                        }
                    }
                    #endregion

                    var stopLinesCount = programFileLines.Count(l => l == "STOP RUN" || l == "STOP RUN.");
                    var lastIndexOfStop = 0;

                    if (stopLinesCount == 0)
                    {
                        stopLinesCount = programFileLines.Count(l => l == "STOP RUN" || l == "STOP RUN.");
                    }
                    if (stopLinesCount == 1)
                    {
                        lastIndexOfStop = programFileLines.FindLastIndex(s => s == "STOP RUN" || s == "STOP RUN.");
                    }
                    //if (lastIndexOfStop == 0)
                    //{
                    //    methodList = new List<string>();
                    //}
                    int lineCnt = -1;
                    bool flag = programFileLines.Any(p => p.StartsWith("EXEC CICS  RECEIVE MAP") 
                    || p.StartsWith("EXEC CICS SEND MAP"));

                    #region Dump statement into StatementReferenceMaster table
                    foreach (var line in programFileLines)
                    {
                        if (string.IsNullOrWhiteSpace(line)) continue;
                        lineCnt++;
                        if (methodList.Any(a => line.StartsWith(a)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = line.Split('.').FirstOrDefault() + "()",
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = methodStart.BaseCommandId,
                                PrimaryCommandId = methodStart.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(ifStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = ifStart.BaseCommandId,
                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0,
                                ProjectId = projectId,
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }

                        if (line == "END ELSE" || line == "ELSE" || line == "ELSE-IF")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 10,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(endIfStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = endIfStart.BaseCommandId,
                                PrimaryCommandId = endIfStart.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        //if (callExternal.Any(l => line == l.StartIndicator || line.StartsWith(l.StartIndicator)))
                        if (line.StartsWith("EXEC CICS  RECEIVE MAP") || line.StartsWith("EXEC CICS SEND MAP"))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId =
                                    callExternal.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId =
                                    callExternal.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line.StartsWith(callInternal.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = line.Split(' ').LastOrDefault() + "()",
                                VariableNameDeclared = null,
                                BaseCommandId = callInternal.BaseCommandId,
                                PrimaryCommandId = callInternal.PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (loopStart.Any(l => line.StartsWith(l.StartIndicator)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopStart.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId =
                                    loopStart.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (loopEnd.Any(l => line.StartsWith(l.StartIndicator)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId =
                                    loopEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line == "END")
                        {
                            if (lineCnt < lastIndexOfStop) continue;
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 9,
                                PrimaryCommandId = 30,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        if (line == "STOP RUN" || line == "STOP RUN.")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 9,
                                PrimaryCommandId = 30,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                        else
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 0,
                                PrimaryCommandId = 0,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        }
                    }


                    if (programFileLines.Any(x => x == "STOP RUN" || x == "STOP RUN."))
                    {
                        var stmtRefeMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = "END",
                            OriginalStatement = "END",
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 9,
                            PrimaryCommandId = 30,
                            ParsedOrNot = "Y",
                            ProjectId = projectId,
                            BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await
                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtRefeMaster);
                    }

                    #endregion
                    #endregion
                    #region Add Default entry for basecommandId = 20

                    var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                        callClassIndicatorEnd[0].PrimaryReferenceId);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

                    #endregion

                    if (flag)
                        await ProcessAllActioWorkflows(projectId, fileMaster);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }

            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessCobolProject(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var clsCobolProcess = new ClsCobolProcess();
                    var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);

                    #region Update ClassNameDeclared
                    var statementReferenceMaster =
                        await
                            _codeVortoService.StatementReferenceMasterRepository.GetAllListItems(
                                x => x.ProjectId == projectId && x.BaseCommandId == 19).ConfigureAwait(false);
                    var projectName = projectDetails.ProjectName;
                    var physicalPath = projectDetails.PhysicalPath;

                    var copyOfFileMaster =
                        await
                            _codeVortoService.FileMasterRepository.GetAllListItems(x => x.ProjectId == projectId)
                                .ConfigureAwait(false);
                    foreach (var statement in statementReferenceMaster)
                    {
                        var pName = copyOfFileMaster.ToList().Where(x => x.FileId == statement.FileId).ToList();
                        var classNameFile =
                            pName[0].FilePath.Replace(physicalPath + "\\", "")
                                .Replace(pName[0].FileName, "")
                                .Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        var classNameDeclared = projectName + "." + classNameFile + fileName;
                        statement.ClassNameDeclared = classNameDeclared;
                        statement.FileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statement);
                    }
                    #endregion

                    #region Update MethodCalled & ClassCalled basecommandId = 6

                    var statementReferMaster =
                          await _codeVortoService.StatementReferenceMasterRepository.GetAllListItems(x => x.ProjectId == projectId && x.BaseCommandId == 6).ConfigureAwait(false);

                    foreach (var statement in statementReferMaster)
                    {
                        var originalStatement = statement.OriginalStatement;
                        var fileMaster = statement.FileMaster;
                        var pName = clsCobolProcess.GetExternalCallFiles(originalStatement,
                            fileMaster.FileTypeExtensionId, copyOfFileMaster);
                        if (pName == null)
                        {
                            if (fileMaster == null) continue;
                            pName = clsCobolProcess.GetExternalCallFilesWithoutFileType(originalStatement, fileMaster.FileTypeExtensionId, copyOfFileMaster);
                            if (pName == null) continue;
                        }
                        var projectDetails1 = _codeVortoService.ProjectMasterRepository.GetItem(pName.ProjectId);
                        var pNameNew = projectDetails1.ProjectName;
                        var pPathNew = projectDetails1.PhysicalPath;
                        var className = pName.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName.FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName.FilePath);

                        var classNameDeclared = pNameNew + "." + className + fileName;
                        statement.ClassCalled = classNameDeclared;
                        statement.MethodCalled = fileName + "()";
                        statement.ReferenceFileId = pName.FileId;
                        statement.FileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(statement).ConfigureAwait(false);
                    }
                    #endregion

                    #region Update MethodCalled for BasecommandId = 5
                    var statementmethodCalled =
                     await
                         _codeVortoService.StatementReferenceMasterRepository.GetAllListItems(
                             x => x.ProjectId == projectId && x.BaseCommandId == 5).ConfigureAwait(false);
                    foreach (var stateMathed in statementmethodCalled)
                    {
                        var originalStatememt = stateMathed.OriginalStatement;
                        var methodCalled = originalStatememt.Replace("PERFROM ", "").Replace("PERFORM", "").Trim();
                        stateMathed.MethodCalled = methodCalled + "()";
                        stateMathed.FileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateMathed);
                    }
                    #endregion

                    #region Update basecommandId = 30

                    string sqlQuery =
                        "SELECT * FROM statementreferencemaster WHERE ProjectId = " + projectId +
                        " AND (OriginalStatement LIKE 'OPEN%' " +
                        " OR OriginalStatement LIKE 'WRITE%'" +
                        " OR OriginalStatement LIKE 'READ%' " +
                        " OR OriginalStatement LIKE 'CLOSE%');";
                    var statementReferenceMst =
                        await
                            _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery).ConfigureAwait(false);

                    if (statementReferenceMst.Any())
                    {
                        foreach (var statement in statementReferenceMst)
                        {
                            statement.BaseCommandId = 30;
                            statement.FileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statement);
                        }
                    }

                    #endregion

                    await GetAllStartingPoints(projectId);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertWorkingStorageSection(int projectId, int languageId,
            List<string> tempBlock, FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                foreach (var line in tempBlock)
                {
                    var currentLine = line.TrimStart();
                    string level = currentLine.SplitLevel();
                    if (string.IsNullOrEmpty(level)) continue;
                    currentLine = currentLine.Substring(level.Length);
                    var varibleName = currentLine.SplitVaiableName();
                    string lenght = string.Empty;
                    var dataType = currentLine.SplitDataType();
                    if (!string.IsNullOrEmpty(dataType))
                    {
                        lenght = dataType.SplitLenght();
                    }
                    var defaultValue = currentLine.SplitDefaultValue();
                    defaultValue = defaultValue.Replace(".", "").Trim();
                    var pictureClause = currentLine.SplitPictureClause();
                    var computationBinary = currentLine.SplitComputationBinary();
                    var coboVaribale = new CobolVariable
                    {
                        FileId = fileMaster.FileId,
                        ProjectId = projectId,
                        VariableLevel = level,
                        VariableName = varibleName,
                        DefaultValue = defaultValue,
                        Lenght = lenght,
                        DataType = dataType,
                        PictureClause = pictureClause,
                        ComputationOrBinary = computationBinary
                    };
                    await _codeVortoService.CobolVariableRepository.AddNewItem(coboVaribale);
                }
            }
            return Ok("done");
        }
    } 
}
