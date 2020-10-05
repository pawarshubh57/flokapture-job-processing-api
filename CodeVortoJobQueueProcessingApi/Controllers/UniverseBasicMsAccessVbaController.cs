using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;
using System.Xml;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using BusinessLayer.UniverseBasic;
using BusinessLayer.VisualBasicVba;


namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class UniverseBasicMsAccessVBAController : ApiController
    {
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();
        private ICodeVortoService _codeVortoService;

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessMsAccessVBAProject(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectConfigFilesGeneralRepository = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var extensionList =
                    await
                        _codeVortoService.FileTypeExtensionRepository.GetAllItems(
                            p => p.LanguageId == projectDetails.LanguageId);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                string projectPath = projectDetails.PhysicalPath;
                List<string> directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                var projectImpFiles =
                    await
                        projectConfigFilesGeneralRepository.GetAllItems(p => p.ConfigFileId != 2 && p.ConfigFileId != 7
                                                                             && p.ConfigFileId != 8 &&
                                                                             p.ConfigFileId != 9 && p.ConfigFileId != 10);
                strExtensions.AddRange(projectImpFiles.Distinct().Select(e => e.ToString()));

                foreach (var directory in directoryList)
                {
                    var allFiles =
                        Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                            .Where(s => strExtensions.Any(s.EndsWith));
                    IEnumerator<string> enumerator = allFiles.GetEnumerator();
                    List<FileMaster> lstFileMasters = new List<FileMaster>();
                    while (enumerator.MoveNext())
                    {
                        string currentFile = enumerator.Current;
                        if (currentFile.Contains(".rpt.txt"))
                        {
                            currentFile = currentFile.Replace(".txt", "");
                        }
                        if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                        string fileName = Path.GetFileName(currentFile);
                        if (string.IsNullOrEmpty(fileName)) continue;
                        if (fileName.Contains(".dll config")) continue;
                        string extension = Path.GetExtension(currentFile);

                        int extensionId =
                            fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;
                        if (enumerator.Current.Contains(".rpt.txt"))
                        {
                            FileMaster fileMaster = new FileMaster()
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = enumerator.Current,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = projectDetails.SolutionId
                            };
                            lstFileMasters.Add(fileMaster);
                        }
                        else
                        {
                            FileMaster fileMaster = new FileMaster()
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = currentFile,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = projectDetails.SolutionId
                            };
                            lstFileMasters.Add(fileMaster);
                        }
                    }
                    enumerator.Dispose();
                    await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters);
                }
                IHttpActionResult processResult = await ParseMsAccessVBAProjectFiles(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseMsAccessVBAProjectFiles(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                IEnumerable<FileMaster> copyOfFileMasters = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                if (projectDetails.ProjectConfigType == 7)
                {
                    var vbprojFiles = Directory.GetFiles(projectDetails.PhysicalPath, "*.vbproj",
                        SearchOption.AllDirectories);
                    foreach (var vbprojFile in vbprojFiles)
                    {
                        var xmlDocument = new XmlDocument();
                        xmlDocument.Load(vbprojFile);
                        XmlNode xmlNode = xmlDocument.GetElementsByTagName("OutputType")[0];
                        if (xmlNode.InnerText == "Library")
                        {
                            copyOfFileMasters =
                                copyOfFileMasters.Where(f => f.FilePath.EndsWith(".Designer.vb")).ToList();
                        }
                    }
                }

                #region Get all Primary command and base command information to parse file...

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllItems().ContinueWith(t =>
                {
                    var result = t.Result;
                    return result.ToList();
                });
                var languageId = projectDetails.LanguageId;
                //var commentedBlockIndicators =
                //    baseCommandReference.Find(s => s.BaseCommand == "Block Comment")
                //        .PrimaryLanguageReference.ToList().FindAll(l => l.LanguageId == languageId);
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment").PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End").PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                //var structureIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Structure")
                //    .PrimaryLanguageReference
                //    .ToList().FindAll(l => l.LanguageId == languageId);
                //var structureIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Structure End")
                //    .PrimaryLanguageReference
                //    .ToList().FindAll(l => l.LanguageId == languageId);
                var classStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                //var classStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Begin Report")
                //    .PrimaryLanguageReference
                //    .ToList().FindAll(l => l.LanguageId == languageId);
                var classEndIndicator = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);

                var callInternalStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callExternalStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                //var functionIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Function End")
                //    .PrimaryLanguageReference
                //    .ToList().FindAll(l => l.LanguageId == languageId);
                //var classStart1 =
                //    baseCommandReference.Find(s => s.BaseCommand == "Begin Report")
                //        .PrimaryLanguageReference.ToList()
                //        .FindAll(l => l.LanguageId == languageId);
                //  var aaa = classStart.
                var visualBasicvba1 = new VisualBasicVba1();

                #endregion

                #region Start reading file lines and do process before dump to database

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();
                foreach (var file in copyOfFileMasters) //.Where(f => f.FileTypeExtensionId == 13 && f.Processed == 0))
                                                        // 16,13
                {
                    var allLines = File.ReadAllLines(file.FilePath);
                    var loopLines = allLines;
                    var strLines = new List<string>();
                    var fileLines = loopLines.ToArray();

                    #region Insert the data in ViewSourceMaster Table

                    //StringBuilder strBuilder = new StringBuilder();
                    //foreach (var stmt in allLines)
                    //{
                    //    strBuilder.AppendLine(stmt);
                    //}

                    //var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                    //ViewSourceMaster viewsourcedata = new ViewSourceMaster()
                    //{
                    //    ViewSourceId = 0,
                    //    FileId = file.FileId,
                    //    SourceData = strBuilder.ToString()
                    //};
                    //await viewSourceMasterRepository.AddNewItem(viewsourcedata);

                    #endregion

                    #region Add Default Entry for BaseCommandId = 19

                    //if (file.FileTypeExtensionId == 16 || file.FileTypeExtensionId == 13)
                    //{
                    //    lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterStart(file,
                    //        classStartIndicator[0].PrimaryReferenceId, methodIndicationStart[0].PrimaryReferenceId);
                    //}
                    //else
                    //{
                    //    lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterStart(file,
                    //        classStartIndicator[0].PrimaryReferenceId);
                    //}

                    await
                        _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    #endregion

                    fileLines = fileLines.CombineAllBrokenLines('_');
                    for (int l = 0; l < fileLines.Select(s => s != "").Count(); l++)
                    {
                        string tempString = fileLines[l].TrimStart().Trim(' ', '\t');
                        if (tempString == "") continue;
                        if (tempString.StartsWith("'")) continue;
                        if (tempString.Contains('(') || tempString.Contains(')'))
                            tempString = tempString.InsertExtraChars('(', ')', "");
                        if (!tempString.Contains('\''))
                            strLines.Add(tempString);
                        else
                        {
                            string newLine = tempString.RemoveVbaCommentedPartFromLine();
                            strLines.Add(newLine);
                        }
                    }

                    //fileLines = strLines.ToArray().CombineAllBrokenLines('_');
                    //fileLines = fileLines.ToArray().DoIfLoopsVbaAdjustment();

                    fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                    var programLineList = new List<string>();
                    programLineList = new List<string>(fileLines);
                    var tempBlock = new List<string>();
                    tempBlock = programLineList.AlterWithStatement();
                    tempBlock = tempBlock.ConvertCaseStatement();
                    tempBlock = tempBlock.RemoveBeginFormStatementNew();
                    tempBlock = tempBlock.CheckExitSubInIf();

                    if (file.FileTypeExtensionId == 16) // This is Query file
                    {
                        tempBlock = tempBlock.RemoveBeginStatement(); // Collect all Begin & query part
                    }
                    else if (file.FileTypeExtensionId == 13) // This id rpt file
                    {
                        tempBlock = tempBlock.GetRecordSourceStatement(); // Collect all the RecordSource statements
                    }

                    #region Starting parsing of file line by line

                    var indexPosition = -1;
                    foreach (var line in tempBlock)
                    {
                        indexPosition = indexPosition + 1;
                        if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                            continue;
                        bool lineDone = false;
                        var methodEnd = methodIndicationEnd.ToList();
                        foreach (var mEnd in methodEnd)
                        {
                            if (!line.StartsWith(mEnd.StartIndicator)) continue;
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = mEnd.BaseCommandId,
                                PrimaryCommandId = mEnd.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            lineDone = true;
                            break;
                        }
                        if (lineDone) continue;
                        var methodStart = methodIndicationStart.ToList();
                        foreach (var mStart in methodStart)
                        {
                            try
                            {
                                //if (!line.StartsWith(mStart.StartIndicator)) continue;
                                bool result = line.ContainsAll(true, mStart.StartIndicator.Split(' ').ToArray());
                                if (result == false)
                                {
                                    if (mStart.StartIndicator.StartsWith("Function"))
                                    {
                                        if (line.StartsWith("Function "))
                                        {
                                            result = true;
                                        }
                                    }
                                }
                                if (!result) continue;
                                var methodOrFunctionLine = line
                                    .Split(new[] { mStart.StartIndicator }, StringSplitOptions.None)[1];
                                var methodOrFunctionName = methodOrFunctionLine.Split('(')[0];
                                if (!line.StartsWith("Declare Function"))
                                {
                                    var stmtReferenceMaster = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = line,
                                        OriginalStatement = line,
                                        ClassCalled = null,
                                        MethodName = methodOrFunctionName.Trim() + "()",
                                        DataOrObjectType = null,
                                        MethodCalled = null,
                                        VariableNameDeclared = null,
                                        BaseCommandId = mStart.BaseCommandId,
                                        PrimaryCommandId = mStart.PrimaryReferenceId,
                                        ProjectId = projectId
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                                            stmtReferenceMaster);
                                    lineDone = true;
                                    break;
                                }
                            }
                            catch (Exception ex)
                            {
                                ex.ToString();
                            }
                        }
                        if (lineDone) continue;

                        var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                        if (line.StartsWith(ifStart.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 1,
                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                ProjectId = projectId
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }
                        var ifEnd = ifConditionEnd.Find(x => x.EndIndicator != null);
                        if (line.StartsWith(ifEnd.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 2,
                                PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;
                        }

                        var loopEnd = loopIndicatorEnd.ToList();
                        foreach (var lEnd in loopEnd)
                        {
                            if (!line.StartsWith(lEnd.StartIndicator)) continue;
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = lEnd.BaseCommandId,
                                PrimaryCommandId = lEnd.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            lineDone = true;
                            break;
                        }

                        if (lineDone) continue;
                        var loopStart = loopIndicatorStart.ToList();
                        foreach (var loop in loopStart)
                        {
                            if (!line.StartsWith(loop.StartIndicator)) continue;
                            if (line.StartsWith("Do "))
                            {
                                var stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = line,
                                    OriginalStatement = line,
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = loop.BaseCommandId,
                                    PrimaryCommandId = loop.PrimaryReferenceId,
                                    ProjectId = projectId
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
                            }
                        }
                        if (lineDone) continue;
                        var callInternal = callInternalStart.ToList();
                        foreach (var cStart in callInternal)
                        {
                            if (!line.StartsWith(cStart.StartIndicator)) continue;
                            if (line.Contains("DoCmd.OpenForm") || line.Contains("DoCmd.OpenReport") ||
                                line.Contains("DoCmd.OpenQuery") || line.StartsWith("Call"))
                            {
                                var stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = line,
                                    OriginalStatement = line,
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = cStart.BaseCommandId,
                                    PrimaryCommandId = cStart.PrimaryReferenceId,
                                    ParsedOrNot = null,
                                    ProjectId = projectId
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
                            }
                        }
                        if (lineDone) continue;

                        var callExternal = callExternalStart.ToList();
                        foreach (var cStart in callExternal)
                        {
                            if (!line.StartsWith(cStart.StartIndicator)) continue;

                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = cStart.BaseCommandId,
                                PrimaryCommandId = cStart.PrimaryReferenceId,
                                ParsedOrNot = null,
                                ProjectId = projectId
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            lineDone = true;
                            break;
                        }
                        if (lineDone) continue;


                        //var classEnd = classEndIndicator.ToList();
                        //foreach (var clsEnd in classEnd)
                        //{
                        //    if (!line.StartsWith(clsEnd.StartIndicator)) continue;
                        //    var stmtReferenceMaster = new StatementReferenceMaster
                        //    {
                        //        FileId = file.FileId,
                        //        ResolvedStatement = line,
                        //        OriginalStatement = line,
                        //        ClassCalled = null,
                        //        MethodName = null,
                        //        DataOrObjectType = null,
                        //        MethodCalled = null,
                        //        VariableNameDeclared = null,
                        //        BaseCommandId = clsEnd.BaseCommandId,
                        //        PrimaryCommandId = clsEnd.PrimaryReferenceId,
                        //        ProjectId = projectId
                        //    };
                        //    await
                        //        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        //    lineDone = true;
                        //    break;
                        //}
                        //if (lineDone) continue;
                        //var classStart = classStartIndicator.ToList();

                        //foreach (var cStart in classStart)
                        //{
                        //    if (!line.Contains(" " + cStart.StartIndicator + " ")) continue;
                        //    var className = line.Split(new[] {"Class"}, StringSplitOptions.RemoveEmptyEntries)[1];
                        //    if (className.Trim().Contains('(') || className.Trim().Contains(',') ||
                        //        className.Trim().Contains(' '))
                        //    {
                        //        var stmtReferenceMaster = new StatementReferenceMaster
                        //        {
                        //            FileId = file.FileId,
                        //            ResolvedStatement = line,
                        //            OriginalStatement = line,
                        //            ClassCalled = null,
                        //            MethodName = null,
                        //            DataOrObjectType = null,
                        //            MethodCalled = null,
                        //            VariableNameDeclared = null,
                        //            ClassNameDeclared = null,
                        //            PrimaryCommandId = 0,
                        //            ParsedOrNot = "Y",
                        //            ProjectId = projectId
                        //        };
                        //        await
                        //            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                        //                stmtReferenceMaster);
                        //        lineDone = true;
                        //    }
                        //    else
                        //    {
                        //        var stmtReferenceMaster = new StatementReferenceMaster
                        //        {
                        //            FileId = file.FileId,
                        //            ResolvedStatement = line,
                        //            OriginalStatement = line,
                        //            ClassCalled = null,
                        //            MethodName = null,
                        //            DataOrObjectType = null,
                        //            MethodCalled = null,
                        //            VariableNameDeclared = null,
                        //            BaseCommandId = cStart.BaseCommandId,
                        //            ClassNameDeclared = className.Trim(),
                        //            PrimaryCommandId = cStart.PrimaryReferenceId,
                        //            ParsedOrNot = "Y",
                        //            ProjectId = projectId
                        //        };
                        //        await
                        //            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                        //                stmtReferenceMaster);
                        //        lineDone = true;
                        //    }
                        //}
                        //if (lineDone) continue;
                        //var structureEnd = structureIndicatorEnd.ToList();
                        //foreach (var structure in structureEnd)
                        //{
                        //    if (!line.StartsWith(structure.StartIndicator)) continue;
                        //    var stmtReferenceMaster = new StatementReferenceMaster
                        //    {
                        //        FileId = file.FileId,
                        //        ResolvedStatement = line,
                        //        OriginalStatement = line,
                        //        ClassCalled = null,
                        //        MethodName = null,
                        //        DataOrObjectType = null,
                        //        MethodCalled = null,
                        //        VariableNameDeclared = null,
                        //        BaseCommandId = structure.BaseCommandId,
                        //        PrimaryCommandId = structure.PrimaryReferenceId,
                        //        ParsedOrNot = "Y",
                        //        ProjectId = projectId
                        //    };
                        //    await
                        //        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        //    lineDone = true;
                        //    break;
                        //}
                        //if (lineDone) continue;
                        //var structureStart = structureIndicatorStart.ToList();
                        //foreach (var structure in structureStart)
                        //{
                        //    if (!line.Contains(" " + structure.StartIndicator + " ")) continue;
                        //    var stmtReferenceMaster = new StatementReferenceMaster
                        //    {
                        //        FileId = file.FileId,
                        //        ResolvedStatement = line,
                        //        OriginalStatement = line,
                        //        ClassCalled = null,
                        //        MethodName = null,
                        //        DataOrObjectType = null,
                        //        MethodCalled = null,
                        //        VariableNameDeclared =
                        //            line.Split(new[] {structure.StartIndicator}, StringSplitOptions.RemoveEmptyEntries)[
                        //                1].Trim(),
                        //        BaseCommandId = structure.BaseCommandId,
                        //        PrimaryCommandId = structure.PrimaryReferenceId,
                        //        ParsedOrNot = "Y",
                        //        ProjectId = projectId
                        //    };
                        //    await
                        //        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        //    lineDone = true;
                        //    break;
                        //}
                        //if (lineDone) continue;
                        if (line == "Else")
                        {
                            var statementReferenceMasterElse = new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BaseCommandId = 10
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                                    statementReferenceMasterElse);
                            continue;
                        }
                        // Default situation...
                        //if (lineDone) continue;
                        var statementReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = file.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            PrimaryCommandId = 0,
                            ProjectId = projectId
                        };
                        await
                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                    }

                    #endregion

                    #region Structured data...

                    //var allStructuresData = await _codeVortoService.StatementReferenceMasterRepository
                    //    .GetDataFromSqlQuery<StatementReferenceMaster>(
                    //        " Select * from StatementReferenceMaster where BaseCommandId = 13; ");
                    //// 13 is for Structures

                    //foreach (var structure in allStructuresData)
                    //{
                    //    int stmtId = structure.StatementId;
                    //    object[] parameters =
                    //    {
                    //        new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId}
                    //    };
                    //    var structureVariables = await generalRepository
                    //        .ExecuteStoreProcedure<StatementReferenceMaster>("GetStructureBlocks", parameters);
                    //    foreach (var sVariables in structureVariables)
                    //    {
                    //        sVariables.AssociatedToParent = structure.StatementId;
                    //        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sVariables);
                    //    }
                    //}

                    #endregion

                    #region Add Default Entry for BaseCommandId = 20

                    if (file.FileTypeExtensionId == 16 || file.FileTypeExtensionId == 13)
                    {
                        lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterEnd(file,
                            classEndIndicator[0].PrimaryReferenceId, methodIndicationEnd[0].PrimaryReferenceId);
                    }
                    else
                    {
                        lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterEnd(file,
                            classEndIndicator[0].PrimaryReferenceId);
                    }
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    #endregion

                    int ifCount = tempBlock.Count(s => s.StartsWith("If"));
                    int endIfCount = tempBlock.Count(s => s.EndsWith("End If") || s.EndsWith("End If."));

                    tempBlock.Add("==========================");
                    tempBlock.Add("IF Counts: " + ifCount);
                    tempBlock.Add("END Count: " + endIfCount);
                }

                #endregion
            }
            IHttpActionResult processResult = await ProcessMsAccessVbaProjectFiles(projectId);
            var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
            //string data = await dataContent.Content.ReadAsStringAsync();
            return Ok("Process VBA completed");
        }

        [HttpGet]
        [SuppressMessage("ReSharper", "AccessToModifiedClosure")]
        public async Task<IHttpActionResult> ProcessMsAccessVbaProjectFiles(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region start processing

                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllItems().ContinueWith(t =>
                {
                    var result = t.Result;
                    return result.ToList();
                });
                var declarationIndicator =
                    baseCommandReference.Find(s => s.BaseCommand == "Declaration").PrimaryLanguageReference.ToList();
                var fileMasterData =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                List<int> fileIds = (from f in fileMasterData select f.FileId).ToList();
                object[] andParameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""},
                };
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", andParameters);
                var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;
                var emptyLines = statementReferenceMasters.ToList().FindAll(s => s.ResolvedStatement.StartsWith("Dim "));
                var declarations = declarationIndicator.ToList();
                //Replace all Me. with className .in ResolvedStatement

                object[] methodParameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                };
                var allMethodNames = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodNamesInProject", methodParameters);
                /*
               var allMethodNames = await _codeVortoService.StatementReferenceMasterRepository
                   .GetEntityData<StatementReferenceMaster>(m => !string.IsNullOrEmpty(m.MethodName)
                  && m.ProjectId == projectId).ContinueWith(t =>
                  {
                      var result = t.Result.ToList().FindAll(d => d.ProjectId == projectId);
                      return result;
                  });
               */
                allMethodNames = allMethodNames.GroupBy(x => x.MethodName).Select(g => g.First()).ToList();

                #endregion

                #region region 1....

                foreach (var fId in fileIds)
                {
                    var id = fId;
                    string sqlQuery = "Select * from StatementReferenceMaster where ProjectId =" + projectId +
                                      " AND FileId =" + id + " AND ClassNameDeclared is not null;";
                    var className =
                        await
                            _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                    if (!className.Any()) continue;
                    foreach (var clsName in className)
                    {
                        var genreicBlock = GetGenericBlock(clsName.StatementId, 19, 20);
                        var meContainsLines =
                            genreicBlock.ToList()
                                .FindAll(
                                    s =>
                                        s.OriginalStatement.Contains(" Me.") && s.ProjectId == projectId &&
                                        s.FileId == id);
                        foreach (var l in meContainsLines)
                        {
                            var l1 = l;
                            string methodCalled = null;
                            int firstCommaIndex = l1.ResolvedStatement.IndexOf(" Me.",
                                StringComparison.InvariantCultureIgnoreCase);
                            string m1 = l1.ResolvedStatement.Substring(firstCommaIndex + 4);
                            bool result = m1.ContainsAll("(", ")");
                            bool callInternalOrExternal = false;
                            if (result)
                            {
                                methodCalled = m1.Split('(')[0].TrimEnd().Trim();
                                if (allMethodNames.Any(method => methodCalled.Trim().Contains(method.MethodName)))
                                {
                                    callInternalOrExternal = true;
                                }
                                //var workFlowOrNot = !l.OriginalStatement.Contains("=");
                                var stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    ResolvedStatement =
                                        l1.ResolvedStatement.Replace("Me.", clsName.ClassNameDeclared + "."),
                                    ClassCalled = clsName.ClassNameDeclared.Trim(),
                                    ParsedOrNot = "Y",
                                    StatementId = l1.StatementId,
                                    OriginalStatement = l1.OriginalStatement,
                                    BaseCommandId = l1.BaseCommandId,
                                    PrimaryCommandId = l1.PrimaryCommandId,
                                    FileId = l1.FileId,
                                    MethodCalled = methodCalled,
                                    OtherBaseCommandId = callInternalOrExternal ? 5 : 0,
                                    ProjectId = projectId
                                    //WorkFlowRelevant = "I"
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stmtReferenceMaster);
                            }
                        }
                    }
                }

                #endregion

                #region region 2...

                // Replace all constructor Names...
                string construstorSql =
                    "  Select * from statementreferencemaster where OriginalStatement like 'Public Sub New (  ) %' AND ProjectId = " +
                    projectId + "; ";
                var constructors = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(construstorSql);
                foreach (var constructor in constructors)
                {
                    string temp = constructor.ResolvedStatement;
                    string classSql = " Select * from statementreferencemaster where FileId = " + constructor.FileId +
                                      " AND baseCommandId = 19 AND ProjectId = " + projectId + " " +
                                      " AND StatementId <= " + constructor.StatementId +
                                      " order by StatementId DESC LIMIT 1;";
                    var className = await
                        baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(classSql);
                    if (!className.Any()) continue;

                    temp = temp.Replace("New", className[0].ClassNameDeclared);
                    constructor.ResolvedStatement = temp;
                    constructor.ProjectId = projectId;
                    await
                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                }

                #endregion

                #region region 3.. Update the ClassCalled and methodCalled for base command id=6

                var execStmtName =
                    statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 6).ToList();
                foreach (var constructorSource in execStmtName)
                {
                    string fileName = "";
                    int fileId = constructorSource.FileId;
                    string currentStmt = constructorSource.OriginalStatement.Replace("\"", "'");
                    fileName = currentStmt.Between("\'", "\'");
                    var allCheckFiles = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllItems(f => f.ProjectId == projectId && f.ClassNameDeclared == fileName.Trim());
                    if (!allCheckFiles.Any())
                    {
                        constructorSource.ClassCalled = fileName;
                        await
                            _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructorSource);
                        continue;
                    }

                    foreach (var files in allCheckFiles)
                    {
                        #region Call External To Update the Method Called

                        var lstmethodName =
                            statementReferenceMasters.Where(
                                x => x.ProjectId == projectId && x.FileId == files.FileId && x.BaseCommandId == 8)
                                .ToList();
                        var lstCallDataBind =
                            statementReferenceMasters.Where(x => x.ProjectId == projectId && x.FileId == files.FileId);
                        if (!lstmethodName.Any()) continue;
                        for (int iMethodName = 0; iMethodName < lstmethodName.Count; iMethodName++)
                        {
                            //var checkSql = "";
                            List<StatementReferenceMaster> checkSql = new List<StatementReferenceMaster>();
                            if (lstmethodName.Count > 1)
                            {
                                checkSql = lstCallDataBind
                                    .Where(x => x.StatementId > lstmethodName[iMethodName].StatementId &&
                                                x.StatementId < lstmethodName[iMethodName + 1].StatementId &&
                                                x.BaseCommandId != 19).ToList();
                            }
                            else
                            {
                                checkSql = lstCallDataBind
                                    .Where(x => x.StatementId > lstmethodName[iMethodName].StatementId &&
                                                x.BaseCommandId != 19).ToList();
                            }

                            if (checkSql.Count > 0)
                            {
                                constructorSource.MethodCalled =
                                    lstmethodName[iMethodName].MethodName.Replace(".", "") + "()";
                                constructorSource.BaseCommandId = 6;
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructorSource);
                                break;
                            }
                        }

                        #endregion

                        #region External To Update the Class Called

                        var classCalledName =
                            statementReferenceMasters.Where(
                                x => x.ProjectId == projectId && x.FileId == files.FileId && x.BaseCommandId == 19)
                                .ToList();

                        for (int iCalledName = 0; ;)
                        {
                            constructorSource.ClassCalled = classCalledName[iCalledName].ClassNameDeclared;
                            await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructorSource);
                            break;
                        }

                        #endregion
                    }
                }

                #endregion

                #region region 4.. Update Method called for base command id = 5  for DoCmd...

                string variable1 = "\"";

                var execName = statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 5
                                                                    && x.OriginalStatement.Contains(variable1) &&
                                                                    x.OriginalStatement.StartsWith("DoCmd")).ToList();
                foreach (var constructor in execName)
                {
                    //string[] temp = constructor.ClassCalled.Split('.');
                    string fileName = "";
                    int fileId = constructor.FileId;
                    string currentStmt = constructor.OriginalStatement.Replace("\"", "'");
                    fileName = currentStmt.Between("\'", "\'");

                    var allCheckFiles = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllItems(f => f.ProjectId == projectId && f.ClassNameDeclared == fileName.Trim());
                    if (!allCheckFiles.Any())
                    {
                        constructor.ClassCalled = fileName;
                        constructor.BaseCommandId = 6;
                        await
                            _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                        continue;
                    }
                    foreach (var files in allCheckFiles)
                    {
                        if (files.FileId == fileId)
                        {
                            #region Call Internal

                            var methodName =
                                statementReferenceMasters.Where(
                                    x => x.ProjectId == projectId && x.FileId == files.FileId
                                         && x.StatementId < constructor.StatementId && x.BaseCommandId == 8)
                                    .ToList()
                                    .FirstOrDefault();

                            if (!string.IsNullOrEmpty(methodName.MethodName))
                            {
                                constructor.MethodCalled = methodName.MethodName;
                                constructor.BaseCommandId = 5;
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                            }

                            #endregion
                        }
                        else
                        {
                            #region Call External To Update the Method Called

                            var lstmethodName =
                                statementReferenceMasters.Where(
                                    x => x.ProjectId == projectId && x.FileId == files.FileId && x.BaseCommandId == 8)
                                    .ToList();

                            var lstCallDataBind =
                                statementReferenceMasters.Where(
                                    x => x.ProjectId == projectId && x.FileId == files.FileId).ToList();
                            if (!lstmethodName.Any()) continue;
                            for (int iMethodName = 0; iMethodName < lstmethodName.Count; iMethodName++)
                            {
                                List<StatementReferenceMaster> checkSql = new List<StatementReferenceMaster>();
                                if (lstmethodName.Count > 1)
                                {
                                    checkSql = lstCallDataBind
                                        .Where(x => x.StatementId > lstmethodName[iMethodName].StatementId &&
                                                    x.StatementId < lstmethodName[iMethodName + 1].StatementId &&
                                                    x.BaseCommandId != 19).ToList();
                                }
                                else
                                {
                                    checkSql = lstCallDataBind
                                        .Where(x => x.StatementId > lstmethodName[iMethodName].StatementId &&
                                                    x.BaseCommandId != 19).ToList();
                                }

                                if (checkSql.Count > 0)
                                {
                                    constructor.MethodCalled = lstmethodName[iMethodName].MethodName.Replace(".", "") +
                                                               "()";
                                    constructor.BaseCommandId = 6;
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                    break;
                                }
                            }

                            #endregion

                            #region External To Update the Class Called

                            var classCalledName =
                                statementReferenceMasters.Where(
                                    x => x.ProjectId == projectId && x.FileId == files.FileId && x.BaseCommandId == 19)
                                    .ToList();

                            for (int iCalledName = 0; iCalledName < classCalledName.Count;)
                            {
                                constructor.ClassCalled = classCalledName[iCalledName].ClassNameDeclared;
                                constructor.BaseCommandId = 6;
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }

                            #endregion
                        }
                    }
                }

                #endregion

                #region region 5.. Update baseCommand Id 5 for call

                string strFileName = String.Empty;
                //string execallSql = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                //                    " AND BaseCommandId = 5 and OriginalStatement like 'call%';";
                //var execallName = await
                //    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execallSql);

                var execallName = statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 5
                                                                       && x.OriginalStatement.StartsWith("Call"))
                    .ToList();

                //string callList = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                //                  " AND BaseCommandId = 8 ;";
                //var lstCallData = await
                //    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(callList);

                var lstCallData =
                    statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 8).ToList();
                foreach (var constructorCall in execallName)
                {
                    string currentStmt = String.Empty;
                    if (constructorCall.OriginalStatement.Contains("RunReportAsPDF"))
                    {
                        string currentStmtNew = constructorCall.OriginalStatement.Replace("\"", "'");
                        var mainStatment = currentStmtNew.Between("\'", "\'");
                        string orignalStatment = mainStatment.Split(' ').FirstOrDefault();
                        currentStmt = orignalStatment.Remove(orignalStatment.Length - 1, 1).Replace('\'', ' ').Trim();
                    }
                    else
                    {
                        string[] currentStat = constructorCall.OriginalStatement.Split(' ');
                        currentStmt = currentStat[1].ToString().Trim();
                        if (currentStmt.Contains("("))
                        {
                            currentStmt = currentStmt.Split('(').FirstOrDefault();
                        }
                    }
                    var methodName =
                        lstCallData.Where(x => x.FileId == constructorCall.FileId && x.MethodName == currentStmt + "()")
                            .ToList();
                    if (methodName.Any())
                    {
                        constructorCall.MethodCalled = currentStmt + "()";
                        constructorCall.BaseCommandId = 5;
                        await
                            _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructorCall);
                        continue;
                    }
                    else
                    {
                        var checkMethod = lstCallData.Where(x => x.MethodName == currentStmt).ToList();
                        if (checkMethod.Any())
                        {
                            #region External To Update the Class Called

                            foreach (var checkMt in checkMethod)
                            {
                                //string classCalledSql = " Select * from statementreferencemaster where FileId = " +
                                //                        checkMt.FileId +
                                //                        " AND ProjectId = " + projectId + " AND BaseCommandId=19;";
                                //var classCalledName = await
                                //    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                //        classCalledSql);

                                var classCalledName =
                                    statementReferenceMasters.Where(
                                        x =>
                                            x.ProjectId == projectId && x.FileId == checkMt.FileId &&
                                            x.BaseCommandId == 19).ToList();

                                for (int iCalledName = 0; iCalledName < classCalledName.Count; iCalledName++)
                                {
                                    constructorCall.ClassCalled = classCalledName[iCalledName].ClassNameDeclared;
                                    constructorCall.MethodCalled = currentStmt + "()";
                                    constructorCall.BaseCommandId = 6;
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(
                                            constructorCall);
                                    continue;
                                }
                            }

                            #endregion
                        }
                        else
                        {
                            constructorCall.MethodCalled = currentStmt + "()";
                            constructorCall.BaseCommandId = 5;
                            await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructorCall);
                            continue;
                        }
                    }
                }

                #endregion

                #region region 6...

                List<string> variablesCollection = new List<string>();
                foreach (var emptyLine in emptyLines)
                {
                    //var done = false;
                    if (emptyLine.ResolvedStatement.ToLower().StartsWith("get")
                        || emptyLine.ResolvedStatement.ToLower().StartsWith("set") ||
                        emptyLine.ResolvedStatement.ToLower().StartsWith("imports")) continue;
                    foreach (var declaration in declarations)
                    {
                        if (!emptyLine.ResolvedStatement.Contains(declaration.StartIndicator)) continue;
                        var dataObjectType = emptyLine.ResolvedStatement.Replace("Dim ", "");
                        string variableName =
                            dataObjectType.Split(new[] { " As " }, StringSplitOptions.None)[0];
                        string variableType = String.Empty;
                        try
                        {
                            variableType = dataObjectType.Split(new[] { " As " }, StringSplitOptions.None)[1];
                        }
                        catch (Exception)
                        {
                            if (!dataObjectType.Contains("As") && dataObjectType.Contains("="))
                            {
                                variableType = dataObjectType.Split(new[] { "=" }, StringSplitOptions.None)[1];
                            }
                        }

                        if (variableType.StartsWith("New"))
                        {
                            variableType =
                                variableType.Split(' ')[1];
                        }
                        if (variableType.Contains('='))
                        {
                            variableType =
                                variableType.Split('=').First();
                        }
                        variablesCollection.Add(variableType);
                        emptyLine.DataOrObjectType = variableType;
                        emptyLine.VariableNameDeclared = variableName;
                        emptyLine.BaseCommandId = declaration.BaseCommandId;
                        emptyLine.PrimaryCommandId = declaration.PrimaryReferenceId;
                        emptyLine.ParsedOrNot = "Y";
                        emptyLine.ProjectId = projectId;
                        // TODO: Remember to uncomment...
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(emptyLine);
                        //done = true;
                        break;
                    }
                }

                #endregion

                #region region 7...

                // Action Workflows data...
                var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                int projectType = projectMasterData.ProjectConfigType;
                if (projectType == 10) //ProjectType == 3 
                {
                    var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                    var projectConfigData = projectConfig.GetItem(projectType);
                    if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                    // Means the project type is windows application and we need to read starting point from that 
                    // files respective code behind file...
                    string configFileName = projectConfigData.ToString();
                    if (string.IsNullOrEmpty(configFileName)) return Ok("Process completed successfully");
                    var allConfigFiles =
                        await _codeVortoService.FileMasterRepository.GetAllItems(
                            f => f.FilePath.EndsWith(configFileName) && f.ProjectId == projectId);
                    foreach (var cFile in allConfigFiles)
                    {
                        var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(cFile.FilePath);
                        if (fileNameWithoutExtension == null) continue;
                        string className = fileNameWithoutExtension.Split('.')[0];
                        className = className.Split('_')[1];
                        var genericBlocks = GetAllMethodsForClass(className, projectId);
                        foreach (var statement in genericBlocks)
                        {
                            //   if (statement.OriginalStatement.StartsWith("Private")) continue;
                            var stat = statement.OriginalStatement;
                            if (stat.Contains("_DblClick") || stat.Contains("_Click") || stat.Contains("_Load"))
                            {
                                var clickIngnoreCode = GetGenericBlock(statement.StatementId, 8, 9);
                                var checkCancelStatemet =
                                    clickIngnoreCode.Exists(s => s.OriginalStatement == "Cancel = True");
                                if (checkCancelStatemet)
                                {
                                    continue;
                                }
                            }
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Service",
                                MethodStatementId = statement.StatementId,
                                OriginFileName =
                                    Path.GetFileName(
                                        cFile.FilePath),
                                OriginFilePath =
                                    cFile.FilePath,
                                ProjectId = projectId,
                                OriginEventMethod = statement.MethodName,
                                OriginObject = className,
                                WorkflowName = statement.OriginalStatement,
                                ServiceBaseAddress = null,
                                ServiceContract = null
                            };
                            Predicate<ActionWorkflows> predicate = p => p.WorkflowName == actionWorkflow.WorkflowName
                                                                        && p.ProjectId == projectId;
                            var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                .FindItem<ActionWorkflows>(predicate);
                            if (alreadyPresent == null)
                                await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                        }
                    }
                }

                //return Ok("Done");

                #endregion

                #region region 8...

                var regexSql = new Regex(@"[\""]SELECT \*");
                foreach (var constructor in fileMasterData)
                {
                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.Contains("\"Select") ||
                                                                               x.OriginalStatement.Contains("\"SELECT")))
                        .ToList();
                    string result = string.Empty;
                    List<DataDependency> lstDataDependency = new List<DataDependency>();

                    if (selectStatName.Any())
                    {
                        foreach (var itemSelect in selectStatName)
                        {
                            int aaa = 0;
                            string bbb = string.Empty;
                            string attrStmt = string.Empty;
                            string attributes = null;
                            //itemSelect.OriginalStatement = "RecordSource ='SELECT tblOperators.ID, tblOperators.OperatorID, tblOperators.SSN, tblOperators.OpLastName, tblOperators.OpFirstName, tblOperators.OpMember, tblOperators.OpBartender, tblOperators.OpAssign, tblOperators.OpAnnualBingo, tblOperators.OpPullTabs, tblOperators.OpSuspended FROM tblOperators WHERE (((tblOperators.OpMember)=True) AND ((tblOperators.OpSuspended)=False)) OR (((tblOperators.OpBartender)=True) AND ((tblOperators.OpSuspended)=False)) ORDER BY tblOperators.OpLastName, tblOperators.OpFirstName;'";

                            if (regexSql.IsMatch(itemSelect.OriginalStatement.ToUpper().TrimEnd()))
                            {
                                if (itemSelect.OriginalStatement.Contains("where") ||
                                    itemSelect.OriginalStatement.Contains("WHERE"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("where",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(0, aaa).Trim();
                                    //result = bbb.Split(' ').LastOrDefault();
                                    bbb = RemoveCheckSpecialChars(bbb);
                                    result = bbb.Trim().Split(' ').LastOrDefault();

                                    attrStmt = itemSelect.OriginalStatement.Substring(aaa).Trim();
                                    string[] value = attrStmt.Split(' ');
                                    for (int i = 0; i < value.Length; i++)
                                    {
                                        if (!value[i].StartsWith("=") && value[i].Contains("="))
                                        {
                                            attributes = attributes + "," + value[i].Split('=').FirstOrDefault();
                                        }
                                        else
                                        {
                                            if (value[i] == "=" || value[i] == "=\"")
                                            {
                                                attributes = attributes + "," + value[i - 1];
                                            }
                                        }
                                    }

                                    if (!string.IsNullOrEmpty(attributes))
                                    {
                                        attributes = RemoveSpecialChars(attributes);
                                        attributes = attributes.TrimStart(',').Trim();
                                    }
                                }
                                else
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim();
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);
                                }
                            }
                            else
                            {
                                if (itemSelect.OriginalStatement.Contains("from") ||
                                    itemSelect.OriginalStatement.Contains("FROM"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim().Replace("GROUP", "")
                                        .Replace("WHERE", "");
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);

                                    attrStmt = itemSelect.OriginalStatement.Substring(0, aaa).Split('=')
                                        .LastOrDefault().Trim().Replace("Select", "").Replace("SELECT", "")
                                        .Replace("DISTINCTROW", "").Trim();

                                    if (attrStmt.Contains(","))
                                    {
                                        string[] value = attrStmt.Split(',');
                                        for (int j = 0; j < value.Length; j++)
                                        {
                                            attributes = attributes + "," + value[j].TrimStart().TrimEnd();
                                        }
                                    }
                                    else
                                    {
                                        attributes = attrStmt;
                                    }
                                    attributes = RemoveSpecialChars(attributes);
                                    attributes = attributes.TrimStart(',').Trim();
                                }
                            }


                            var dataDependencyRepository = new DataDependencyRepository(new AppDbContext());
                            DataDependency datadependency = new DataDependency
                            {
                                DataDepedencyId = 0,
                                ProjectId = projectId,
                                FileId = constructor.FileId,
                                Entity = result,
                                Attributes = attributes
                            };

                            List<int> iCnt = (from dd in lstDataDependency
                                              where dd.Entity == result && dd.FileId == constructor.FileId
                                              select dd.FileId).ToList();
                            if (iCnt.Count == 0)
                            {
                                lstDataDependency.Add(datadependency);
                            }
                        }

                        await _codeVortoService.DataDependencyRepository.BulkInsert(listOfEntities: lstDataDependency);
                    }
                }

                #endregion

                #region Insert the data in ViewSourceMaster Table

                foreach (var file in fileMasterData)
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

                #endregion

                IHttpActionResult processResult = await ProcessForObjectConnectivityDiagramData(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);

                //// Start process for collecting workflow data here...
                //IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                //await processResult.ExecuteAsync(CancellationToken.None);
                //return Ok();
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster =
                    await _codeVortoService.ProjectMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                var fileElementMaster =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                var statementMaster =
                    await
                        _codeVortoService.StatementReferenceMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                List<Node> lstNodes = new List<Node>();
                var objFlowChart = new FlowChart();
                var languageId = 0;

                foreach (var languageTypeId in projectMaster)
                {
                    languageId = languageTypeId.LanguageId;
                }

                #region Runtime Created the flowchart

                try
                {
                    #region Add Nodes Logic

                    var allClassData = statementMaster.Where(p => p.ProjectId == projectId
                                                                  && p.BaseCommandId == 19 &&
                                                                  p.ClassNameDeclared != "null").ToList();

                    foreach (var s in allClassData)
                    {
                        Node nodeObject = new Node();

                        //var checkFrmFileOrNot = fileElementMaster.Where(p => p.ProjectId == projectId
                        //    && p.FileTypeExtensionId == 14 && p.FileId == s.FileId).ToList();

                        //if (checkFrmFileOrNot.Any())
                        //{
                        if (languageId == 1 || languageId == 6)
                        {
                            nodeObject.Id = s.StatementId;
                            nodeObject.Name = s.ClassNameDeclared;
                            nodeObject.ShapeId = "RoundRect";
                            nodeObject.Height = "15";
                            nodeObject.Width = "100";
                            nodeObject.Color = "#00ffff";
                            nodeObject.StatementId = s.StatementId;
                            nodeObject.BaseCommandId = s.BaseCommandId;
                            lstNodes.Add(nodeObject);
                        }
                        var fileNewId = s.FileId;

                        if (languageId == 6)
                        {
                            #region For VBA Application

                            if (!string.IsNullOrEmpty(s.ClassNameDeclared))
                            {
                                var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                                   && p.FileId == s.FileId &&
                                                                                   p.BaseCommandId == 6).ToList();

                                // Check the class the present or not.
                                // Class is present to add the color : Yellow
                                // Class is not present to add the color : Gray

                                var newList = callExecStatement.ToList();

                                foreach (var c in callExecStatement)
                                {
                                    if (!string.IsNullOrEmpty(c.ClassCalled))
                                    {
                                        var datatype = c.DataOrObjectType;
                                        var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "").Trim();
                                        string strName = c.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                        var checkClassPresentOrNot =
                                            fileElementMaster.Where(p => p.ProjectId == projectId
                                                                         && p.FileName.Contains(strName.Trim()))
                                                .ToList();

                                        if (checkClassPresentOrNot.Any())
                                        {
                                            var fileIdPresent = 0;
                                            foreach (var p in checkClassPresentOrNot)
                                            {
                                                fileIdPresent = p.FileId;
                                            }

                                            callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                                           && p.FileId == fileIdPresent &&
                                                                                           p.BaseCommandId == 6)
                                                .ToList();

                                            if (callExecStatement.Count > 0)
                                            {
                                                newList.AddRange(callExecStatement);
                                            }
                                        }
                                        else
                                        {
                                        }
                                    }

                                    if (newList.Count > 0)
                                    {
                                        foreach (var z in newList)
                                        {
                                            if (!string.IsNullOrEmpty(z.ClassCalled))
                                            {
                                                var datatype = z.DataOrObjectType;
                                                var dataObject = z.ClassCalled.Replace("'", "").Replace(",", "").Trim();
                                                string strName = z.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                                var checkClassPresentOrNot =
                                                    fileElementMaster.Where(p => p.ProjectId == projectId
                                                                                 && p.FileName.Contains(strName.Trim()))
                                                        .ToList();

                                                if (checkClassPresentOrNot.Any())
                                                {
                                                    var fileIdPresent = 0;
                                                    foreach (var p in checkClassPresentOrNot)
                                                    {
                                                        fileIdPresent = p.FileId;
                                                    }
                                                    Node nodeObject1 = new Node
                                                    {
                                                        Id = z.StatementId,
                                                        Name = z.ClassCalled,
                                                        ShapeId = "RoundRect",
                                                        Height = "15",
                                                        Width = "100",
                                                        Color = "#00ffff",
                                                        StatementId = s.StatementId,
                                                        BaseCommandId = s.BaseCommandId,
                                                    };

                                                    List<int> iCnt = (from dd in lstNodes
                                                                      where dd.Name == nodeObject1.Name
                                                                      select dd.Id).ToList();
                                                    if (iCnt.Count == 0)
                                                    {
                                                        lstNodes.Add(nodeObject1);
                                                    }
                                                }
                                                else
                                                {
                                                    Node nodeObject2 = new Node
                                                    {
                                                        Id = z.StatementId,
                                                        Name = z.ClassCalled,
                                                        ShapeId = "RoundRect",
                                                        Height = "15",
                                                        Width = "100",
                                                        Color = "#C0C0C0",
                                                        StatementId = s.StatementId,
                                                        BaseCommandId = s.BaseCommandId,
                                                        FileId = s.FileId
                                                    };

                                                    List<int> iCnt = (from dd in lstNodes
                                                                      where
                                                                          dd.Name == nodeObject2.Name &&
                                                                          dd.FileId == nodeObject2.FileId
                                                                      select dd.Id).ToList();
                                                    if (iCnt.Count == 0)
                                                    {
                                                        lstNodes.Add(nodeObject2);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            #endregion

                            //}
                        }
                    }

                    objFlowChart.Nodes = lstNodes;

                    #endregion

                    if (lstNodes.Count > 0)
                    {
                        #region Add Links

                        #region links variables

                        var lstLinks = new List<Link>();

                        #endregion

                        var allClassDataLinks = allClassData.ToList();

                        if (languageId == 6)
                        {
                            foreach (var s in allClassDataLinks)
                            {
                                //    var checkFrmFileOrNot = fileElementMaster.Where(p => p.ProjectId == projectId
                                //&& p.FileTypeExtensionId == 14 && p.FileId == s.FileId).ToList();

                                //if (checkFrmFileOrNot.Any())
                                //{
                                if (!string.IsNullOrEmpty(s.ClassNameDeclared))
                                {
                                    var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                                                                       && p.FileId == s.FileId &&
                                                                                       p.BaseCommandId == 6).ToList();

                                    // Check the class the present or not.
                                    // Class is present to add the color : Yellow
                                    // Class is not present to add the color : Gray 

                                    foreach (var c in callExecStatement)
                                    {
                                        if (!string.IsNullOrEmpty(c.ClassCalled))
                                        {
                                            var iDofNode = s.StatementId;
                                            var fileId = s.FileId;
                                            var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "");
                                            var checkClassPresentOrNotLink =
                                                statementMaster.Where(
                                                    p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                         && p.ClassCalled == dataObject).ToList();

                                            if (checkClassPresentOrNotLink.Count > 0)
                                            {
                                                foreach (var f in checkClassPresentOrNotLink)
                                                {
                                                    var checkClassCallMethod =
                                                        statementMaster.Where(
                                                            p => p.ProjectId == projectId && p.BaseCommandId == 19
                                                                 && p.FileId == f.FileId).ToList();

                                                    if (checkClassCallMethod.Count > 0)
                                                    {
                                                        string mulMethodName = "";

                                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                        {
                                                            var getClassCalled =
                                                                statementMaster.Where(
                                                                    p =>
                                                                        p.ProjectId == projectId && p.BaseCommandId == 6
                                                                        &&
                                                                        p.ClassCalled ==
                                                                        checkClassCallMethod[e].ClassNameDeclared)
                                                                    .ToList();

                                                            if (getClassCalled.Count > 0)
                                                            {
                                                                for (int g = 0; g < getClassCalled.Count; g++)
                                                                {
                                                                    var linkObject = new Link
                                                                    {
                                                                        Origin = getClassCalled[g].StatementId,
                                                                        Target = f.StatementId,
                                                                        BaseCommandId = f.BaseCommandId,
                                                                        StatementId = f.StatementId
                                                                    };

                                                                    var l =
                                                                        (from d in lstLinks
                                                                         where
                                                                             d.Origin ==
                                                                             getClassCalled[g].StatementId &&
                                                                             d.Target == f.StatementId
                                                                         select d).ToList();
                                                                    if (l.Count == 0)
                                                                    {
                                                                        if (f.MethodCalled != null)
                                                                        {
                                                                            if (f.MethodCalled.Trim().Contains("(") &&
                                                                                f.MethodCalled.Trim().EndsWith(")"))
                                                                            {
                                                                                if (!f.MethodCalled.Contains("=") &&
                                                                                    !f.MethodCalled.StartsWith("<"))
                                                                                {
                                                                                    int indexMethod =
                                                                                        f.MethodCalled.IndexOf("(",
                                                                                            StringComparison.Ordinal);
                                                                                    if (indexMethod > 0)
                                                                                    {
                                                                                        if (
                                                                                            !mulMethodName.Contains(
                                                                                                f.MethodCalled.Substring
                                                                                                    (0,
                                                                                                        indexMethod)))
                                                                                        {
                                                                                            mulMethodName =
                                                                                                mulMethodName + ", " +
                                                                                                f.MethodCalled.Substring
                                                                                                    (0,
                                                                                                        indexMethod);

                                                                                            linkObject.LinkText =
                                                                                                mulMethodName.Substring(
                                                                                                    1,
                                                                                                    mulMethodName.Length -
                                                                                                    1)
                                                                                                    .Replace("))", "");
                                                                                        }
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        if (
                                                                                            !mulMethodName.Contains(
                                                                                                f.MethodCalled))
                                                                                        {
                                                                                            mulMethodName =
                                                                                                mulMethodName + ", " +
                                                                                                f.MethodCalled;

                                                                                            linkObject.LinkText =
                                                                                                mulMethodName.Substring(
                                                                                                    1,
                                                                                                    mulMethodName.Length -
                                                                                                    1)
                                                                                                    .Replace("))", "");
                                                                                        }
                                                                                    }


                                                                                    if (linkObject.Origin !=
                                                                                        linkObject.Target)
                                                                                    {
                                                                                        if (e ==
                                                                                            checkClassCallMethod.Count -
                                                                                            1)
                                                                                        {
                                                                                            lstLinks.Add(linkObject);
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                        else
                                                                        {
                                                                            if (linkObject.Origin != linkObject.Target)
                                                                            {
                                                                                if (e == checkClassCallMethod.Count - 1)
                                                                                {
                                                                                    lstLinks.Add(linkObject);
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }

                                                                break;
                                                            }
                                                            else
                                                            {
                                                                var linkObject = new Link
                                                                {
                                                                    Origin = iDofNode,
                                                                    Target = f.StatementId,
                                                                    BaseCommandId = f.BaseCommandId,
                                                                    StatementId = f.StatementId
                                                                };

                                                                var k =
                                                                    (from d in lstLinks
                                                                     where
                                                                         d.Origin == iDofNode &&
                                                                         d.Target == f.StatementId
                                                                     select d).ToList();
                                                                if (k.Count == 0)
                                                                {
                                                                    if (f.MethodCalled != null)
                                                                    {
                                                                        if (f.MethodCalled.Trim().Contains("(") &&
                                                                            f.MethodCalled.Trim().EndsWith(")"))
                                                                        {
                                                                            if (!f.MethodCalled.Contains("=") &&
                                                                                !f.MethodCalled.StartsWith("<"))
                                                                            {
                                                                                int indexMethod =
                                                                                    f.MethodCalled.IndexOf("(",
                                                                                        StringComparison.Ordinal);
                                                                                if (indexMethod > 0)
                                                                                {
                                                                                    if (
                                                                                        !mulMethodName.Contains(
                                                                                            f.MethodCalled.Substring(0,
                                                                                                indexMethod)))
                                                                                    {
                                                                                        mulMethodName = mulMethodName +
                                                                                                        ", " +
                                                                                                        f.MethodCalled
                                                                                                            .Substring(
                                                                                                                0,
                                                                                                                indexMethod);

                                                                                        linkObject.LinkText =
                                                                                            mulMethodName.Substring(1,
                                                                                                mulMethodName.Length - 1)
                                                                                                .Replace("))", "");
                                                                                    }
                                                                                }
                                                                                else
                                                                                {
                                                                                    if (
                                                                                        !mulMethodName.Contains(
                                                                                            f.MethodCalled))
                                                                                    {
                                                                                        mulMethodName = mulMethodName +
                                                                                                        ", " +
                                                                                                        f.MethodCalled;

                                                                                        linkObject.LinkText =
                                                                                            mulMethodName.Substring(1,
                                                                                                mulMethodName.Length - 1)
                                                                                                .Replace("))", "");
                                                                                    }
                                                                                }


                                                                                if (linkObject.Origin !=
                                                                                    linkObject.Target)
                                                                                {
                                                                                    if (e ==
                                                                                        checkClassCallMethod.Count - 1)
                                                                                    {
                                                                                        lstLinks.Add(linkObject);
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                    else
                                                                    {
                                                                        if (linkObject.Origin != linkObject.Target)
                                                                        {
                                                                            if (e == checkClassCallMethod.Count - 1)
                                                                            {
                                                                                lstLinks.Add(linkObject);
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                //}
                            }
                        }

                        objFlowChart.Links = lstLinks;

                        #region Code to remove Missing Origin / missing target links

                        List<int> missingTargets =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Target == missingTargets[iNotlnkCnt]);
                            }
                        }

                        List<int> missingOrigins =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Origin == missingOrigins[iNotlnkCnt]);
                            }
                        }

                        List<int> missingTargets1 =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets1.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets1.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Target == missingTargets1[iNotlnkCnt]);
                            }
                        }

                        List<int> missingOrigins1 =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins1.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins1.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Origin == missingOrigins1[iNotlnkCnt]);
                            }
                        }

                        #endregion

                        #region Code to remove no links to the nodes

                        List<int> missingNodes =
                            lstNodes.Select(x => x.Id)
                                .ToList()
                                .Except(lstLinks.Select(y => y.Origin))
                                .ToList();
                        if (missingNodes.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingNodes.Count; iNotlnkCnt++)
                            {
                                List<int> iCnt = (from dd in lstLinks
                                                  where dd.Target == missingNodes[iNotlnkCnt]
                                                  select dd.Target).ToList();
                                if (iCnt.Count == 0)
                                {
                                    lstNodes.RemoveAll(x => x.Id == missingNodes[iNotlnkCnt]);
                                }
                            }
                        }

                        #endregion

                        #endregion
                    }

                    var startNodes = await _codeVortoService.ActionWorkflowsRepository
                        .GetDataFromSqlQuery<ActionWorkflows>(
                            " select * from actionworkFlows where ProjectId=" + projectId +
                            " and EndPointOrService is null;");
                    int tempI = 1;
                    var list =
                        startNodes.FindAll(
                            s => s.EndPointOrService != "Service");
                    foreach (var node in list)
                    {
                        Node nodeObject = new Node
                        {
                            StatementId = 99998 + tempI,
                            Id = 99998 + tempI,
                            Name = node.WorkflowName,
                            ShapeId = "Circle",
                            Height = "15",
                            Width = "100",
                            Color = "#ffcc00"
                        };
                        lstNodes.Add(nodeObject);
                        tempI++;
                    }

                    #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                    if (objFlowChart.Nodes.Count > 0)
                    {
                        var nodes = objFlowChart.Nodes.ToList();

                        if (languageId == 1)
                        {
                            foreach (var mNodes in nodes)
                            {
                                var nodesData = new ConnectivityStepReference
                                {
                                    Name = mNodes.Name,
                                    Shape = mNodes.ShapeId,
                                    Color = mNodes.Color,
                                    ParentId = mNodes.ParentId,
                                    StatementId = mNodes.StatementId,
                                    ProjectId = Convert.ToInt32(projectId)
                                };
                                await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
                            }
                        }
                        else
                        {
                            foreach (var mNodes in nodes)
                            {
                                string strName = mNodes.Name;
                                var nodesData = new ConnectivityStepReference
                                {
                                    Name = strName.Trim(),
                                    Shape = mNodes.ShapeId,
                                    Color = mNodes.Color,
                                    ParentId = mNodes.ParentId,
                                    StatementId = mNodes.Id,
                                    ProjectId = Convert.ToInt32(projectId)
                                };
                                await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
                            }
                        }
                    }

                    if (objFlowChart.Links.Count > 0)
                    {
                        var links = objFlowChart.Links.ToList();
                        foreach (var mLinks in links)
                        {
                            try
                            {
                                var liksData = new ConnectivityLinkReferece
                                {
                                    Origin = mLinks.Origin,
                                    Target = mLinks.Target,
                                    LinkText = mLinks.LinkText,
                                    StatementId = mLinks.StatementId,
                                    ProjectId = Convert.ToInt32(projectId)
                                };
                                await _codeVortoService.ConnectivityLinkRefereceRepository.AddNewItem(liksData);
                            }
                            catch (Exception ex)
                            {
                                Console.WriteLine(ex.InnerException);
                            }
                        }
                    }

                    #endregion
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }

                #endregion

                IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
            }
        }

        //[HttpGet]
        //public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {
        //        var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
        //        //var fileMasterRepository = new FileMasterRepository(new AppDbContext());
        //        var generalRepository = new GeneralRepository<DeadDataTypes>(new AppDbContext());
        //        List<Node> lstNodes = new List<Node>();
        //        var objFlowChart = new FlowChart();

        //        #region Using DataBase entries and generated the flowchart

        //        var languageType = await _codeVortoService.ProjectMasterRepository
        //            .GetDataFromSqlQuery<ProjectMaster>(
        //                "select * from ProjectMaster where ProjectId=" + projectId + "");
        //        var languageId = 0;

        //        foreach (var languageTypeId in languageType)
        //        {
        //            languageId = languageTypeId.LanguageId;
        //        }

        //        #endregion

        //        #region Runtime Created the flowchart

        //        try
        //        {
        //            #region Add Nodes Logic

        //            var allClassData =
        //                await statementReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
        //                    "SELECT * FROM statementreferencemaster where ProjectId='" + projectId +
        //                    "' and BaseCommandId=19 and ClassNameDeclared != 'null';");
        //            foreach (var s in allClassData)
        //            {
        //                Node nodeObject = new Node
        //                {
        //                    Id = s.StatementId,
        //                    Name = s.ClassNameDeclared,
        //                    ShapeId = "RoundRect",
        //                    Height = "15",
        //                    Width = "100",
        //                    Color = "#00ffff",
        //                    StatementId = s.StatementId,
        //                    BaseCommandId = s.BaseCommandId
        //                };
        //                lstNodes.Add(nodeObject);
        //                var fileNewId = s.FileId;

        //                //  Get the all Classes to file wise.
        //                var allCallingClassData = await statementReferenceRepository
        //                    .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                        "select * from statementreferencemaster where BaseCommandId=7" +
        //                        " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
        //                        fileNewId + "';");
        //                if (languageId == 1)
        //                {
        //                    #region For VB

        //                    foreach (var c in allCallingClassData)
        //                    {
        //                        var datatype = c.DataOrObjectType;
        //                        var allDeadDatatypes = await generalRepository.GetDataFromSqlQuery<DeadDataTypes>(
        //                            "SELECT * FROM deaddatatypes");
        //                        int j = 0;
        //                        for (int i = 0; i < allDeadDatatypes.Count; i++)
        //                        {
        //                            var keywordName = allDeadDatatypes[i].KaywordName;
        //                            if (datatype.Contains(keywordName))
        //                            {
        //                                j++;
        //                            }
        //                        }
        //                        if (j != 0) continue;
        //                        // Check the class the present or not.
        //                        // Class is present to add the color : Yellow
        //                        // Class is not present to add the color : Gray 

        //                        var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
        //                        var checkClassPresentOrNot = await statementReferenceRepository
        //                            .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                "SELECT * from statementreferencemaster where FileId In (" +
        //                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
        //                                "') and classNameDeclared like '%" + dataObject + "%';");
        //                        if (checkClassPresentOrNot.Count > 0)
        //                        {
        //                            Node nodeObject1 = new Node
        //                            {
        //                                Id = c.StatementId,
        //                                Name = c.DataOrObjectType,
        //                                ShapeId = "RoundRect",
        //                                Height = "15",
        //                                Width = "100",
        //                                Color = "#00ffff",
        //                                StatementId = c.StatementId,
        //                                BaseCommandId = c.BaseCommandId
        //                            };
        //                            List<int> iCnt = (from dd in lstNodes
        //                                              where dd.Name == nodeObject1.Name
        //                                              select dd.Id).ToList();
        //                            if (iCnt.Count == 0)
        //                            {
        //                                lstNodes.Add(nodeObject1);
        //                            }
        //                        }
        //                        else
        //                        {
        //                            Node nodeObject2 = new Node
        //                            {
        //                                Id = c.StatementId,
        //                                Name = c.DataOrObjectType,
        //                                ShapeId = "RoundRect",
        //                                Height = "15",
        //                                Width = "100",
        //                                Color = "#C0C0C0",
        //                                StatementId = c.StatementId,
        //                                BaseCommandId = c.BaseCommandId,
        //                                FileId = c.FileId
        //                            };

        //                            List<int> iCnt = (from dd in lstNodes
        //                                              where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
        //                                              select dd.Id).ToList();
        //                            if (iCnt.Count == 0)
        //                            {
        //                                lstNodes.Add(nodeObject2);
        //                            }
        //                        }
        //                    }

        //                    #endregion
        //                }
        //                else
        //                {
        //                    foreach (var c in allCallingClassData)
        //                    {
        //                        var dataObject = c.ClassCalled;
        //                        var checkClassPresentOrNot = await statementReferenceRepository
        //                            .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                "select * from statementreferencemaster where BaseCommandId=6 and projectId=" +
        //                                projectId + "" +
        //                                " and ClassCalled='" + dataObject + "';");
        //                        if (checkClassPresentOrNot.Count > 0)
        //                        {
        //                            Node nodeObject1 = new Node
        //                            {
        //                                Id = c.StatementId,
        //                                Name = c.ClassCalled,
        //                                ShapeId = "RoundRect",
        //                                Height = "15",
        //                                Width = "100",
        //                                Color = "#00ffff",
        //                                StatementId = c.StatementId,
        //                                BaseCommandId = c.BaseCommandId,
        //                            };


        //                            List<int> iCnt = (from dd in lstNodes
        //                                              where dd.Name == nodeObject1.Name
        //                                              select dd.Id).ToList();
        //                            if (iCnt.Count == 0)
        //                            {
        //                                lstNodes.Add(nodeObject1);
        //                            }
        //                        }
        //                        else
        //                        {
        //                            Node nodeObject2 = new Node
        //                            {
        //                                Id = c.StatementId,
        //                                Name = c.ClassCalled,
        //                                ShapeId = "RoundRect",
        //                                Height = "15",
        //                                Width = "100",
        //                                Color = "#C0C0C0",
        //                                StatementId = c.StatementId,
        //                                BaseCommandId = c.BaseCommandId,
        //                                FileId = c.FileId
        //                            };

        //                            List<int> iCnt = (from dd in lstNodes
        //                                              where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
        //                                              select dd.Id).ToList();
        //                            if (iCnt.Count == 0)
        //                            {
        //                                lstNodes.Add(nodeObject2);
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //            objFlowChart.Nodes = lstNodes;

        //            #endregion

        //            if (lstNodes.Count > 0)
        //            {
        //                #region Add Links

        //                #region links variables

        //                var lstLinks = new List<Link>();

        //                #endregion

        //                var allClassDataLinks = await statementReferenceRepository
        //                    .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                        "SELECT * FROM statementreferencemaster where ProjectId='" + projectId +
        //                        "' and BaseCommandId=19 and ClassNameDeclared!='null'");

        //                if (languageId == 1)
        //                {
        //                    #region Links For the VB

        //                    foreach (var s in allClassDataLinks)
        //                    {
        //                        var iDofNode = s.StatementId;
        //                        var fileId = s.FileId;
        //                        // Get the file wise "Dim" started classes
        //                        var allCallingClassDataLinks = await statementReferenceRepository
        //                            .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                "select * from statementreferencemaster where BaseCommandId=7" +
        //                                " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
        //                                fileId + "';");

        //                        foreach (var c in allCallingClassDataLinks)
        //                        {
        //                            var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
        //                            var calledDataObject = c.DataOrObjectType;
        //                            var statementId = c.StatementId;
        //                            var allDeadDatatypesLinks = await generalRepository
        //                                .GetDataFromSqlQuery<DeadDataTypes>(
        //                                    "SELECT * FROM deaddatatypes");

        //                            // Check the dead datatype in classes or Not

        //                            int j = 0;
        //                            foreach (DeadDataTypes t in allDeadDatatypesLinks)
        //                            {
        //                                var keywordName = t.KaywordName;
        //                                if (dataObject != null && dataObject.Contains(keywordName))
        //                                {
        //                                    j++;
        //                                }
        //                            }
        //                            if (j != 0) continue;
        //                            // Check the Class id called or not.
        //                            var checkClassPresentOrNotLink = await statementReferenceRepository
        //                                .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                    "SELECT * from statementreferencemaster where FileId In (" +
        //                                    " SELECT FileId FROM filemaster where ProjectId='" + projectId +
        //                                    "') and classNameDeclared like '%" + dataObject + "%';");

        //                            if (checkClassPresentOrNotLink.Count > 0)
        //                            {
        //                                foreach (var f in checkClassPresentOrNotLink)
        //                                {
        //                                    var checkClassCallMethod = await statementReferenceRepository
        //                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                            "SELECT * from statementreferencemaster where FileId In (" +
        //                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
        //                                            "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
        //                                            "';");

        //                                    if (checkClassCallMethod.Count > 0)
        //                                    {
        //                                        string mulMethodName = "";
        //                                        var linkObject = new Link
        //                                        {
        //                                            Origin = iDofNode,
        //                                            Target = f.StatementId,
        //                                            BaseCommandId = f.BaseCommandId,
        //                                            StatementId = f.StatementId
        //                                        };
        //                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
        //                                        {
        //                                            if (checkClassCallMethod[e].MethodCalled != null)
        //                                            {
        //                                                if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
        //                                                    checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
        //                                                {
        //                                                    if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
        //                                                        !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
        //                                                    {
        //                                                        int indexMethod =
        //                                                            checkClassCallMethod[e].MethodCalled.IndexOf("(",
        //                                                                StringComparison.Ordinal);
        //                                                        if (indexMethod > 0)
        //                                                        {
        //                                                            if (
        //                                                                !mulMethodName.Contains(
        //                                                                    checkClassCallMethod[e]
        //                                                                        .MethodCalled.Substring(0,
        //                                                                            indexMethod)))
        //                                                            {
        //                                                                mulMethodName = mulMethodName + ", " +
        //                                                                                checkClassCallMethod[e]
        //                                                                                    .MethodCalled.Substring(0,
        //                                                                                        indexMethod);
        //                                                                linkObject.LinkText =
        //                                                                    mulMethodName.Substring(1,
        //                                                                        mulMethodName.Length - 1)
        //                                                                        .Replace("))", "");
        //                                                            }
        //                                                        }
        //                                                        else
        //                                                        {
        //                                                            if (
        //                                                                !mulMethodName.Contains(
        //                                                                    checkClassCallMethod[e]
        //                                                                        .MethodCalled))
        //                                                            {
        //                                                                mulMethodName = mulMethodName + ", " +
        //                                                                                checkClassCallMethod[e]
        //                                                                                    .MethodCalled;
        //                                                                linkObject.LinkText =
        //                                                                    mulMethodName.Substring(1,
        //                                                                        mulMethodName.Length - 1)
        //                                                                        .Replace("))", "");
        //                                                            }
        //                                                        }


        //                                                        if (linkObject.Origin != linkObject.Target)
        //                                                        {
        //                                                            if (e == checkClassCallMethod.Count - 1)
        //                                                            {
        //                                                                lstLinks.Add(linkObject);
        //                                                            }
        //                                                        }
        //                                                    }
        //                                                }
        //                                            }
        //                                        }
        //                                    }
        //                                }
        //                            }
        //                            else
        //                            {
        //                                #region Class Is not Calling to add the Class as Misssing Element

        //                                var checkClassCallMethod = await statementReferenceRepository
        //                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                        "select * from statementreferencemaster where fileid= '" + fileId +
        //                                        "' and statementId > '" + statementId + "' and" +
        //                                        " statementId < (select statementId from statementreferencemaster where fileid= '" +
        //                                        fileId + "' and statementId > '" + statementId + "'" +
        //                                        " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
        //                                        " where fileid= '" + fileId + "' and statementId ='" + statementId +
        //                                        "') and MethodCalled is not null;");


        //                                if (checkClassCallMethod.Count == 0) // For Global variable
        //                                {
        //                                    checkClassCallMethod = await statementReferenceRepository
        //                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                            "select * from statementreferencemaster where fileid= '" + fileId +
        //                                            "' and statementId > '" + statementId +
        //                                            "' and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
        //                                            " where fileid= '" + fileId + "' and statementId ='" + statementId +
        //                                            "') and MethodCalled is not null;");
        //                                }


        //                                if (checkClassCallMethod.Count > 0)
        //                                {
        //                                    string mulMethodName = "";
        //                                    var l =
        //                                        (from d in lstNodes where d.Name == c.ClassNameDeclared select d).ToList
        //                                            ();
        //                                    Link linkObject1;
        //                                    if (l.Count > 0)
        //                                    {
        //                                        linkObject1 = new Link
        //                                        {
        //                                            Origin = iDofNode,
        //                                            Target = l.First().StatementId
        //                                        };
        //                                    }
        //                                    else
        //                                    {
        //                                        linkObject1 = new Link
        //                                        {
        //                                            Origin = iDofNode,
        //                                            Target = statementId
        //                                        };
        //                                    }
        //                                    //linkObject.target = c.StatementId;
        //                                    //foreach (var e in checkClassCallMethod)
        //                                    for (int e = 0; e < checkClassCallMethod.Count; e++)
        //                                    {
        //                                        if (checkClassCallMethod[e].MethodCalled != null)
        //                                        {
        //                                            if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
        //                                                checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
        //                                            {
        //                                                if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
        //                                                    !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
        //                                                {
        //                                                    int indexMethod =
        //                                                        checkClassCallMethod[e].MethodCalled.IndexOf("(",
        //                                                            StringComparison.Ordinal);
        //                                                    if (indexMethod > 0)
        //                                                    {
        //                                                        //linkObject1.linkText = e.MethodCalled.Substring(0, indexMethod);
        //                                                        if (
        //                                                            !mulMethodName.Contains(
        //                                                                checkClassCallMethod[e].MethodCalled.Substring(
        //                                                                    0, indexMethod)))
        //                                                        {
        //                                                            mulMethodName = mulMethodName + ", " +
        //                                                                            checkClassCallMethod[e].MethodCalled
        //                                                                                .Substring(0, indexMethod);
        //                                                            linkObject1.LinkText =
        //                                                                mulMethodName.Substring(1,
        //                                                                    mulMethodName.Length - 1).Replace("))", "");
        //                                                        }
        //                                                    }
        //                                                    else
        //                                                    {
        //                                                        if (
        //                                                            !mulMethodName.Contains(
        //                                                                checkClassCallMethod[e].MethodCalled))
        //                                                        {
        //                                                            mulMethodName = mulMethodName + ", " +
        //                                                                            checkClassCallMethod[e].MethodCalled;
        //                                                            linkObject1.LinkText =
        //                                                                mulMethodName.Substring(1,
        //                                                                    mulMethodName.Length - 1).Replace("))", "");
        //                                                        }
        //                                                    }

        //                                                    if (linkObject1.Origin == linkObject1.Target)
        //                                                        continue;
        //                                                    if (e == checkClassCallMethod.Count - 1)
        //                                                    {
        //                                                        lstLinks.Add(linkObject1);
        //                                                    }
        //                                                }
        //                                            }
        //                                        }
        //                                    }
        //                                }

        //                                #endregion
        //                            }
        //                        }
        //                    }

        //                    #endregion
        //                }
        //                else
        //                {
        //                    foreach (var s in allClassDataLinks)
        //                    {
        //                        var iDofNode = s.StatementId;
        //                        var dataObject = s.ClassCalled;
        //                        // Check the Class id called or not.
        //                        var checkClassPresentOrNotLink = await statementReferenceRepository
        //                            .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                "select * from statementreferencemaster where BaseCommandId=6 and projectId=" +
        //                                projectId + "" +
        //                                " and ClassCalled='" + dataObject + "';");

        //                        if (checkClassPresentOrNotLink.Count <= 0) continue;

        //                        foreach (var f in checkClassPresentOrNotLink)
        //                        {
        //                            var checkClassCallMethod = await statementReferenceRepository
        //                                .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                                    "SELECT * from statementreferencemaster where FileId In (" +
        //                                    " SELECT FileId FROM filemaster where ProjectId='" + projectId +
        //                                    "') and BasecommandId= 6 and ClassCalled='" + dataObject +
        //                                    "';");

        //                            if (checkClassCallMethod.Count <= 0) continue;
        //                            string mulMethodName = "";
        //                            var linkObject = new Link
        //                            {
        //                                Origin = iDofNode,
        //                                Target = f.StatementId,
        //                                BaseCommandId = f.BaseCommandId,
        //                                StatementId = f.StatementId
        //                            };
        //                            for (int e = 0; e < checkClassCallMethod.Count; e++)
        //                            {
        //                                if (checkClassCallMethod[e].MethodCalled != null)
        //                                {
        //                                    if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
        //                                        checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
        //                                    {
        //                                        if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
        //                                            !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
        //                                        {
        //                                            int indexMethod =
        //                                                checkClassCallMethod[e].MethodCalled.IndexOf("(",
        //                                                    StringComparison.Ordinal);
        //                                            if (indexMethod > 0)
        //                                            {
        //                                                if (
        //                                                    !mulMethodName.Contains(
        //                                                        checkClassCallMethod[e]
        //                                                            .MethodCalled.Substring(0,
        //                                                                indexMethod)))
        //                                                {
        //                                                    mulMethodName = mulMethodName + ", " +
        //                                                                    checkClassCallMethod[e]
        //                                                                        .MethodCalled.Substring(0,
        //                                                                            indexMethod);
        //                                                    linkObject.LinkText =
        //                                                        mulMethodName.Substring(1,
        //                                                            mulMethodName.Length - 1)
        //                                                            .Replace("))", "");
        //                                                }
        //                                            }
        //                                            else
        //                                            {
        //                                                if (
        //                                                    !mulMethodName.Contains(
        //                                                        checkClassCallMethod[e]
        //                                                            .MethodCalled))
        //                                                {
        //                                                    mulMethodName = mulMethodName + ", " +
        //                                                                    checkClassCallMethod[e]
        //                                                                        .MethodCalled;
        //                                                    linkObject.LinkText =
        //                                                        mulMethodName.Substring(1,
        //                                                            mulMethodName.Length - 1)
        //                                                            .Replace("))", "");
        //                                                }
        //                                            }


        //                                            if (linkObject.Origin != linkObject.Target)
        //                                            {
        //                                                if (e == checkClassCallMethod.Count - 1)
        //                                                {
        //                                                    lstLinks.Add(linkObject);
        //                                                }
        //                                            }
        //                                        }
        //                                    }
        //                                }
        //                            }
        //                        }
        //                    }
        //                }

        //                objFlowChart.Links = lstLinks;

        //                #region Code to remove Missing Origin / missing target links

        //                List<int> missingTargets =
        //                    lstLinks.Select(x => x.Target)
        //                        .ToList()
        //                        .Except(lstNodes.Select(y => y.Id))
        //                        .ToList();
        //                if (missingTargets.Count > 0)
        //                {
        //                    for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets.Count; iNotlnkCnt++)
        //                    {
        //                        lstLinks.RemoveAll(x => x.Target == missingTargets[iNotlnkCnt]);
        //                    }
        //                }

        //                List<int> missingOrigins =
        //                    lstLinks.Select(x => x.Origin)
        //                        .ToList()
        //                        .Except(lstNodes.Select(y => y.Id))
        //                        .ToList();
        //                if (missingOrigins.Count > 0)
        //                {
        //                    for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins.Count; iNotlnkCnt++)
        //                    {
        //                        lstLinks.RemoveAll(x => x.Origin == missingOrigins[iNotlnkCnt]);
        //                    }
        //                }

        //                List<int> missingTargets1 =
        //                    lstLinks.Select(x => x.Target)
        //                        .ToList()
        //                        .Except(lstNodes.Select(y => y.Id))
        //                        .ToList();
        //                if (missingTargets1.Count > 0)
        //                {
        //                    for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets1.Count; iNotlnkCnt++)
        //                    {
        //                        lstLinks.RemoveAll(x => x.Target == missingTargets1[iNotlnkCnt]);
        //                    }
        //                }

        //                List<int> missingOrigins1 =
        //                    lstLinks.Select(x => x.Origin)
        //                        .ToList()
        //                        .Except(lstNodes.Select(y => y.Id))
        //                        .ToList();
        //                if (missingOrigins1.Count > 0)
        //                {
        //                    for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins1.Count; iNotlnkCnt++)
        //                    {
        //                        lstLinks.RemoveAll(x => x.Origin == missingOrigins1[iNotlnkCnt]);
        //                    }
        //                }

        //                #endregion

        //                #region Code to remove no links to the nodes

        //                List<int> missingNodes =
        //                    lstNodes.Select(x => x.Id)
        //                        .ToList()
        //                        .Except(lstLinks.Select(y => y.Origin))
        //                        .ToList();
        //                if (missingNodes.Count > 0)
        //                {
        //                    for (int iNotlnkCnt = 0; iNotlnkCnt < missingNodes.Count; iNotlnkCnt++)
        //                    {
        //                        List<int> iCnt = (from dd in lstLinks
        //                                          where dd.Target == missingNodes[iNotlnkCnt]
        //                                          select dd.Target).ToList();
        //                        if (iCnt.Count == 0)
        //                        {
        //                            lstNodes.RemoveAll(x => x.Id == missingNodes[iNotlnkCnt]);
        //                        }
        //                    }
        //                }

        //                #endregion

        //                #endregion
        //            }
        //            var startNodes = await _codeVortoService.ActionWorkflowsRepository
        //                .GetDataFromSqlQuery<ActionWorkflows>(
        //                    " select * from actionworkFlows where ProjectId=" + projectId +
        //                    " and EndPointOrService is null;");
        //            int tempI = 1;
        //            var list =
        //                startNodes.FindAll(
        //                    s => s.EndPointOrService != "Service");
        //            foreach (var node in list)
        //            {
        //                Node nodeObject = new Node
        //                {
        //                    StatementId = 99998 + tempI,
        //                    Id = 99998 + tempI,
        //                    Name = node.WorkflowName,
        //                    ShapeId = "Circle",
        //                    Height = "15",
        //                    Width = "100",
        //                    //Color = "#a9d18e"
        //                    Color = "#ffcc00"
        //                };
        //                lstNodes.Add(nodeObject);
        //                tempI++;
        //            }
        //            //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
        //            var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
        //            //var enumerable = tempNodes.ToList();
        //            foreach (var tNode in tempNodes)
        //            {
        //                foreach (var allNode in objFlowChart.Nodes)
        //                {
        //                    var checkClassInNodes = await _codeVortoService.ActionWorkflowsRepository
        //                        .GetDataFromSqlQuery<ActionWorkflows>(
        //                            " select * from actionworkFlows where ProjectId=" + projectId +
        //                            " and EndPointOrService is not null group by OriginFileName;");

        //                    foreach (var chkNodes in checkClassInNodes)
        //                    {
        //                        //string oName = rNodes.OriginObject.Split('.').LastOrDefault();
        //                        if (allNode.Name == chkNodes.OriginFileName.Replace(".vb", ""))
        //                        {
        //                            int kk = allNode.StatementId;
        //                            objFlowChart.Links.Add(new Link
        //                            {
        //                                Origin = tNode.Id,
        //                                Target = kk
        //                            });
        //                        }
        //                    }
        //                }
        //            }

        //            #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

        //            if (objFlowChart.Nodes.Count > 0)
        //            {
        //                var nodes = objFlowChart.Nodes.ToList();
        //                foreach (var mNodes in nodes)
        //                {
        //                    var nodesData = new ConnectivityStepReference
        //                    {
        //                        Name = mNodes.Name,
        //                        Shape = mNodes.ShapeId,
        //                        Color = mNodes.Color,
        //                        ParentId = mNodes.ParentId,
        //                        StatementId = mNodes.StatementId,
        //                        ProjectId = Convert.ToInt32(projectId)
        //                    };
        //                    await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
        //                }
        //            }

        //            if (objFlowChart.Links.Count > 0)
        //            {
        //                var links = objFlowChart.Links.ToList();
        //                foreach (var mLinks in links)
        //                {
        //                    try
        //                    {
        //                        var liksData = new ConnectivityLinkReferece
        //                        {
        //                            Origin = mLinks.Origin,
        //                            Target = mLinks.Target,
        //                            LinkText = mLinks.LinkText,
        //                            StatementId = mLinks.StatementId,
        //                            ProjectId = Convert.ToInt32(projectId)
        //                        };
        //                        await _codeVortoService.ConnectivityLinkRefereceRepository.AddNewItem(liksData);
        //                    }
        //                    catch (Exception exception)
        //                    {
        //                        Console.WriteLine(exception.Message);
        //                    }
        //                }
        //            }

        //            #endregion
        //        }
        //        catch (Exception ex)
        //        {
        //            Console.WriteLine(ex.Message);
        //        }

        //        #endregion

        //        // Start process for collecting workflow data here...
        //        IHttpActionResult processResult = await GetAllStartingPoints(projectId);
        //        await processResult.ExecuteAsync(CancellationToken.None);
        //        return Ok("Connectivity diagram data process completed successfully.");
        //    }
        //}

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ");

                if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

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
                    //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                    //string workflowData = await dataContent.Content.ReadAsStringAsync();
                }
                return Ok("All Workflow processed successfully");
            }
        }

        private static IEnumerable<StatementReferenceMaster> GetAllMethodsForClass(string className, int projectId)
        {
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar) {Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className},
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                };
                var callExtExpandedCode = codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodsForClass", parametersExp)
                    .ContinueWith(t => t.Result).Result;
                return callExtExpandedCode;
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
                        .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                Expression<Func<StatementReferenceMaster, bool>> expression =
                    master =>
                        master.ProjectId == projectId &&
                        (master.ClassNameDeclared == clsName
                            );
                //master =>
                //    master.ProjectId == projectId &&
                //    (master.ClassNameDeclared == clsName ||
                //     master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(expression, 1);
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
                    GraphName = "<span class='nodeToBold'>" + baseStatementMaster.OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster.BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = clsName,
                    ActualStatementId = "Actual_" + baseStatementMaster.StatementId,
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
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                        treeItem.StatementReferenceMaster.FileId, indentLevel, ref auto, ref treeNodeId,
                        ref callingAndCalled);
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
                        treeItem.StatementReferenceMaster.FileId, indentLevel, ref auto, ref treeNodeId,
                        ref callingAndCalled);
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
                //lstTreeView = copyOfLstTreeView.ToList();
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
                            child.IndentLevel = child.IndentLevel + 1;
                            string groupName = string.Empty;
                            if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                            {
                                child.StatementReferenceMaster.MethodCalled =
                                    child.StatementReferenceMaster.MethodCalled + "()";
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

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                var allSeqListItemsNew = new List<TreeView>();
                foreach (var curItem in copyOfLstTreeView)
                {
                    allSeqListItemsNew.Add(curItem);
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == curItem.GraphId select s).ToList();
                    foreach (var cItem in childItems)
                    {
                        allSeqListItemsNew = AttachChildItems(allSeqListItemsNew, copyOfLstTreeView, cItem);
                    }
                    break;
                }

                lstTreeView = allSeqListItemsNew.ToList();
                foreach (var treeItem in allSeqListItemsNew)
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
                int loopCounter = 0;
                foreach (var treeItem in allSeqListItemsNew)
                {
                    indexPosition++;
                    if (treeItem.BaseCommandId != 3) continue;

                    var treeViewList = new List<TreeView>();
                    for (int i = indexPosition; i < lstTreeView.Count; i++)
                    {
                        treeViewList.Add(lstTreeView[i]);
                        if (lstTreeView[i].BaseCommandId == 3)
                            loopCounter++;
                        if (lstTreeView[i].BaseCommandId == 4)
                            loopCounter--;
                        if (loopCounter == 0)
                            break;
                    }
                    int curIndentLevel = treeViewList.First().IndentLevel;
                    var prevParentId = treeViewList.First().ParentId;
                    var graphId = "LoopStart" + indexPosition + treeItem.ActualStatementId;
                    treeViewList.First().GraphId = graphId;
                    treeViewList.First().IndentLevel = curIndentLevel + 1;
                    for (int j = 1; j < treeViewList.Count; j++)
                    {
                        if (treeViewList[j].ParentId != prevParentId) continue;
                        treeViewList[j].ParentId = graphId;

                        //if (treeViewList[j].BaseCommandId == 2) continue;
                        treeViewList[j].IndentLevel = treeViewList[j].IndentLevel + 2;
                    }
                }


                indexPosition = -1;
                foreach (var treeItem in allSeqListItemsNew)
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

                allSeqListItemsNew = allSeqListItemsNew.DistinctBy().ToList();

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                //secondTab.AddRange(allSeqListItemsNew
                //    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 ||
                //                   item.BaseCommandId == 1 || item.BaseCommandId == 25
                //                   || item.PrimaryCommandId == 1 || item.BaseCommandId == 5 || item.BaseCommandId == 10
                //                   || item.PrimaryCommandId == 2));

                secondTab.AddRange(allSeqListItemsNew
                    .Where(
                        item => (item.BaseCommandId == 6) || (item.BaseCommandId == 8) || (item.BaseCommandId == 10) ||
                                (item.BaseCommandId == 1) || (item.BaseCommandId == 25)
                                || (item.BaseCommandId == 5) || (item.BaseCommandId == 30)));

                //var tempList =
                //    (from d in secondTab
                //     where d.BaseCommandId == 1
                //           || d.BaseCommandId == 2
                //           || d.BaseCommandId == 10
                //           || d.BaseCommandId == 25
                //     select d).ToList();

                var tempList =
                    (from d in secondTab
                     where (d.BaseCommandId == 1)
                           || (d.BaseCommandId == 10)
                           || (d.BaseCommandId == 25)
                     select d).ToList();

                foreach (var sTab in tempList)
                {
                    var childItems = (from s in allSeqListItemsNew where s.ParentId == sTab.GraphId select s).ToList();
                    secondTab.AddRange(childItems);
                    /*  
                   if (sTab.BaseCommandId == 2 || childItems.Count == 1)
                       secondTab.Remove(sTab);
                   if (childItems.Any(k => k.BaseCommandId == 25)) continue;
                   if (!childItems.Any() && sTab.StatementReferenceMaster == null)
                       secondTab.Remove(sTab);
                     */
                }

                secondTab = secondTab.Distinct().ToList();
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();
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
                if (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23)
                {
                    string[] strSplit = secondTab.First().SpriteCssClass.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
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
                    (from a in allSeqListItems where firstItem.GraphId == a.ParentId select a).ToList();
                int linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
                    {
                        #region PrimaryCommandId == 1 || BaseCommandId == 1

                        string ifPart = Regex.Split(curItem.GraphName, "If", RegexOptions.IgnoreCase)[1];
                        var condition = ifPart.Contains("Then")
                            ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                            : ifPart;
                        nodeId++;
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "Decision2",
                            Name = condition,
                            Color = "#ff6600",
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
                            treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, node, ref nodeId,
                                ref linkSeqNumber, true);
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
                        if (curItem.PrimaryCommandId == 25)
                        {
                            string[] strSplit = curItem.StatementReferenceMaster.ClassCalled.Split('.');
                            string strName = "";
                            if (strSplit.Length > 2)
                            {
                                strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
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
                                Origin = nodeId - 1,
                                //Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                            });
                        }
                        else
                        {
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                //Origin = treeView.Nodes.First().Id,
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
                                ref nodeId, ref linkSeqNumber, true);
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
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = methodCalled[0].ClassNameDeclared,
                            Color = nodeColor,
                            StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                            GroupName = curItem.GroupName,
                            GroupId = curItem.GroupId
                        };
                        treeView.Nodes.Add(node);
                        string m = item.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                //Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                            });
                        }
                        else
                        {
                            treeView.Links.Add(new Link
                            {
                                Origin = nodeId - 1,
                                //Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] "
                            });
                        }
                        linkSeqNumber++;
                        var childItems =
                            (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                        foreach (var cItem in childItems)
                        {
                            treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView, node,
                                ref nodeId, ref linkSeqNumber, true);
                        }

                        #endregion
                    }
                    else if (curItem.BaseCommandId == 10)
                    {
                        #region BaseCommandId == 10

                        try
                        {
                            nodeId++;
                            var parentIf =
                                (from p in secondTab where curItem.ParentId == p.GraphId select p).FirstOrDefault();
                            var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                            var condition = ifPart.Contains("THEN")
                                ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                                : ifPart;
                            condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
                            var width = _clsUniverseBasic.CalculateWidth(condition.Length);
                            var height = _clsUniverseBasic.CalculateWidth(condition.Length);
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
                        }
                        catch (Exception ex)
                        {
                            ex.ToString();
                        }

                        #endregion

                        //#region BaseCommandId == 10
                        //try
                        //{
                        //    nodeId++;
                        //    var parentIf =
                        //        (from p in secondTab where curItem.ParentId == p.GraphId select p).FirstOrDefault();
                        //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
                        //    var condition = ifPart.Contains("Then")
                        //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
                        //        : ifPart;
                        //    condition = "If NOT " + condition + " Then ";
                        //    var width = _clsUniverseBasic.CalculateWidth(condition.Length);
                        //    var height = _clsUniverseBasic.CalculateHeight(condition.Length);
                        //    var node = new Node
                        //    {
                        //        Id = nodeId,
                        //        ShapeId = "Decision2",
                        //        Name = condition,
                        //        Color = "#ff6600",
                        //        Width = width,
                        //        Height = height,
                        //        StatementId = Int32.Parse(curItem.ActualStatementId.Split('_')[1]),
                        //        GroupName = curItem.GroupName,
                        //        GroupId = curItem.GroupId
                        //    };
                        //    treeView.Nodes.Add(node);
                        //    treeView.Links.Add(new Link
                        //    {
                        //        Origin = nodeId - 1,
                        //        Target = nodeId,
                        //        LinkText = "[" + linkSeqNumber + "] "
                        //    });
                        //    linkSeqNumber++;
                        //}
                        //catch (Exception ex)
                        //{
                        //    ex.ToString();
                        //}

                        //#endregion
                    }
                }

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

                #endregion

                // ReSharper disable once RedundantAssignment
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
                    Name = node.Name,
                    Height = node.Height,
                    ShapeId = node.ShapeId,
                    Color = node.Color,
                    MaxNodeId = nodeId
                }).ToList();

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
                                stmt.Name.Replace("</span>", "")
                                    .Replace(" LT", " is less than")
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
                    //GraphName = sTab.GraphName + "&nbsp;<img id='imgpseudo' src='../images/regex_icon.png' onclick='PseudoCodeDialog(" + sTab.ActualStatementId.Split('_')[1] + ")'/>",
                    GraphName = sTab.GraphName,
                    HasChild = sTab.HasChild ? "true" : "false",
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = int.Parse(sTab.ActualStatementId.Split('_')[1]),
                    IndentLevel = sTab.IndentLevel
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                var statementSecondTab = await generalRepositoryWorkflowTreeviewSecondTabDetails
                    .GetAllItems(p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                foreach (var stmt in statementSecondTab)
                {
                    if (stmt.BaseCommandId == 1 || stmt.BaseCommandId == 5 || stmt.BaseCommandId == 6)
                    {
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await generalRepositoryWorkflowTreeviewSecondTabDetails.UpdateItem(stmt);
                    }
                }

                #endregion

                var lsTreeviewTabFirstDetails = allSeqListItemsNew.Select(fTab => new WorkflowTreeviewTabFirstDetails
                {
                    BaseCommandId = fTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = fTab.ActualStatementId,
                    ClassCalled = fTab.ClassCalled,
                    GraphId = fTab.GraphId,
                    GraphName = fTab.GraphName,
                    HasChild = fTab.HasChild ? "true" : "false",
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
                lstData.Add(treeViewDataNew);
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */
                return Ok("Workflow data collected successfully");
            }
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
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
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref linkSeqNumber, true);
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref linkSeqNumber, true);
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref nodeId, ref linkSeqNumber, true);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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
            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf != null)
            //    {
            //        nodeId++;
            //        string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //        var condition = ifPart.Contains("Then")
            //            ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //            : ifPart;
            //        condition = "If NOT " + condition + " Then ";
            //        var width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //        var height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //        var node = new Node
            //        {
            //            Id = nodeId,
            //            ShapeId = "Decision2",
            //            Name = condition,
            //            Color = "#ff6600",
            //            Width = width,
            //            Height = height,
            //            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //            GroupName = treeView.GroupName,
            //            GroupId = treeView.GroupId
            //        };
            //        treeViewData.Nodes.Add(node);

            //        treeViewData.Links.Add(new Link
            //        {
            //            Origin = nodeId - 1,
            //            Target = nodeId,
            //            LinkText = "[" + linkSeqNumber + "] "
            //        });
            //        linkSeqNumber++;
            //    }

            //    #endregion
            //}
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;


                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
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
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref linkSeqNumber, true);
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor,
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
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref linkSeqNumber, true);
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref nodeId, ref linkSeqNumber, true);
                }

                #endregion
            }
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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
            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf == null) return treeViewData;

            //    nodeId++;
            //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //    var condition = ifPart.Contains("Then")
            //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //        : ifPart;
            //    condition = "If NOT " + condition + " Then ";
            //    var width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //    var height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //    var node = new Node
            //    {
            //        Id = nodeId,
            //        ShapeId = "Decision2",
            //        Name = condition,
            //        Color = "#ff6600",
            //        Width = width,
            //        Height = height,
            //        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //        GroupName = treeView.GroupName,
            //        GroupId = treeView.GroupId
            //    };
            //    treeViewData.Nodes.Add(node);

            //    treeViewData.Links.Add(new Link
            //    {
            //        Origin = nodeId - 1,
            //        Target = nodeId,
            //        LinkText = "[" + linkSeqNumber + "] "
            //    });
            //    linkSeqNumber++;

            //    #endregion
            //}
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            string width = "0";
            string height = "0";

            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                // treeView.GraphName.Split(new[] { "If" }, StringSplitOptions.None)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2);
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
                    // Origin = lastNode.Id,
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId,
                        ref linkSeqNumber, true);
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
                string andCondition = string.Empty;
                if (!string.IsNullOrEmpty(item.StatementReferenceMaster.ClassCalled))
                {
                    if (item.StatementReferenceMaster.ClassCalled.Contains('.'))
                    {
                        andCondition = " BaseCommandId = 19 AND " +
                                       " ( ClassNameDeclared = '" +
                                       item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                       " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "' ); ";
                    }
                    else
                    {
                        andCondition = " BaseCommandId = 19 AND " +
                                       " ( ClassNameDeclared = '" +
                                       item.StatementReferenceMaster.ClassCalled + "' OR " +
                                       " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "' ); ";
                    }
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
                    if (treeView.PrimaryCommandId == 25)
                    {
                        string[] strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                        string strName = "";
                        if (strSplit.Length > 2)
                        {
                            strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
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
                            // Origin = lastNode.Id,
                            Origin = nodeId - 1,
                            Target = nodeId,
                            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                        });
                    }
                    else
                    {
                        treeViewData.Links.Add(new Link
                        {
                            // Origin = lastNode.Id,
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
                            ref linkSeqNumber, true);
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
                        Origin = nodeId - 1,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        // Origin = lastNode.Id,
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
                        ref nodeId, ref linkSeqNumber, true);
                }

                #endregion
            }
            //else if (treeView.BaseCommandId == 8)
            //{
            //    #region BaseCommandId == 8
            //    string nodeColor = "#c0c0c0";
            //    var methodCalled = _codeVortoService.StatementReferenceMasterRepository
            //        .GetDataFromSqlQuery<StatementReferenceMaster>(
            //            "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
            //    if (methodCalled.Count != 0)
            //        nodeColor = "#00ffff";
            //    nodeId++;

            //    Node node;
            //    if (treeView.PrimaryCommandId == 51)
            //    {
            //        string[] strSplit = methodCalled[0].ClassNameDeclared.Split('.');
            //        string strName = "";
            //        if (strSplit.Length > 2)
            //        {
            //            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
            //        }
            //        node = new Node
            //        {
            //            Id = nodeId,
            //            ShapeId = "RoundRect",
            //            Name = strName.ToUpper(),
            //            Color = nodeColor,
            //            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //            GroupName = treeView.GroupName,
            //            GroupId = treeView.GroupId
            //        };
            //    }
            //    else
            //    {
            //        node = new Node
            //        {
            //            Id = nodeId,
            //            ShapeId = "RoundRect",
            //            Name = methodCalled[0].ClassNameDeclared,
            //            Color = nodeColor,
            //            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //            GroupName = treeView.GroupName,
            //            GroupId = treeView.GroupId
            //        };

            //    }
            //    treeViewData.Nodes.Add(node);
            //    string m = string.Empty;
            //    if (treeView.PrimaryCommandId == 51)
            //    {
            //        m = treeView.StatementReferenceMaster.MethodName;
            //    }
            //    else
            //    {
            //        m = treeView.StatementReferenceMaster.MethodCalled;
            //    }
            //    if (m != null)
            //    {
            //        m = m + "()";
            //        treeViewData.Links.Add(new Link
            //        {
            //            //Origin = lastNode.Id,
            //            Origin = nodeId - 1,
            //            Target = nodeId,
            //            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
            //            StatementId = treeView.StatementReferenceMaster.StatementId
            //        });
            //    }
            //    else
            //    {
            //        treeViewData.Links.Add(new Link
            //        {
            //            //Origin = lastNode.Id,
            //            Origin = nodeId - 1,
            //            Target = nodeId,
            //            LinkText = "[" + linkSeqNumber + "] "
            //        });
            //    }
            //    linkSeqNumber++;
            //    var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
            //        .ToList().Distinct();
            //    foreach (var cItem in childItems)
            //    {
            //        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
            //    }
            //    #endregion
            //}

            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId && treeView.BaseCommandId == 1 select p)
                        .FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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
            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf == null) return treeViewData;

            //    nodeId++;
            //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //    var condition = ifPart.Contains("Then")
            //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //        : ifPart;
            //    condition = "If NOT " + condition + " Then ";
            //    width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //    height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //    var node = new Node
            //    {
            //        Id = nodeId,
            //        ShapeId = "Decision2",
            //        Name = condition,
            //        Color = "#ff6600",
            //        Width = width,
            //        Height = height,
            //        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //        GroupName = treeView.GroupName,
            //        GroupId = treeView.GroupId
            //    };
            //    treeViewData.Nodes.Add(node);

            //    treeViewData.Links.Add(new Link
            //    {
            //        Origin = nodeId - 1,
            //        Target = nodeId,
            //        LinkText = "[" + linkSeqNumber + "] "
            //    });
            //    linkSeqNumber++;

            //    #endregion
            //}
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
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
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (!string.IsNullOrEmpty(m))
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf == null) return treeViewData;

            //    nodeId++;
            //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //    var condition = ifPart.Contains("Then")
            //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //        : ifPart;
            //    condition = "If NOT " + condition + " Then ";
            //    var width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //    var height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //    var node = new Node
            //    {
            //        Id = nodeId,
            //        ShapeId = "Decision2",
            //        Name = condition,
            //        Color = "#ff6600",
            //        Width = width,
            //        Height = height,
            //        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //        GroupName = treeView.GroupName,
            //        GroupId = treeView.GroupId
            //    };
            //    treeViewData.Nodes.Add(node);

            //    treeViewData.Links.Add(new Link
            //    {
            //        Origin = nodeId - 1,
            //        Target = nodeId,
            //        LinkText = "[" + linkSeqNumber + "] "
            //    });
            //    linkSeqNumber++;

            //    #endregion
            //}
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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
            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;


                nodeId++;
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "Decision2",
                    Name = condition,
                    Color = "#ff6600",
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
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = treeView.StatementReferenceMaster.ClassCalled,
                    Color = nodeColor,
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
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        //Origin = treeViewData.Nodes.First().Id,
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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
            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf == null) return treeViewData;

            //    nodeId++;
            //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //    var condition = ifPart.Contains("Then")
            //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //        : ifPart;
            //    condition = "If NOT " + condition + " Then ";
            //    var width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //    var height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //    var node = new Node
            //    {
            //        Id = nodeId,
            //        ShapeId = "Decision2",
            //        Name = condition,
            //        Color = "#ff6600",
            //        Width = width,
            //        Height = height,
            //        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //        GroupName = treeView.GroupName,
            //        GroupId = treeView.GroupId
            //    };
            //    treeViewData.Nodes.Add(node);

            //    treeViewData.Links.Add(new Link
            //    {
            //        Origin = nodeId - 1,
            //        Target = nodeId,
            //        LinkText = "[" + linkSeqNumber + "] "
            //    });
            //    linkSeqNumber++;

            //    #endregion
            //}
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
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];
                // treeView.GraphName.Split(new[] { "If" }, StringSplitOptions.None)[1];
                if (ifPart.Contains("Then"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("Then",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2);
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
                    Origin = lastNode.Id,
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
                if (treeView.PrimaryCommandId == 25)
                {
                    string[] strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                    string strName = "";
                    if (strSplit.Length > 2)
                    {
                        strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
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
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
                var node = new Node
                {
                    Id = nodeId,
                    ShapeId = "RoundRect",
                    Name = methodCalled[0].ClassNameDeclared,
                    Color = nodeColor,
                    StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
                    GroupName = treeView.GroupName,
                    GroupId = treeView.GroupId
                };
                treeViewData.Nodes.Add(node);
                string m = treeView.StatementReferenceMaster.MethodCalled;
                if (m != null)
                {
                    m = m + "()";
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
                        Target = nodeId,
                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
                    });
                }
                else
                {
                    treeViewData.Links.Add(new Link
                    {
                        Origin = lastNode.Id,
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
            //else if (treeView.BaseCommandId == 8)
            //{
            //    #region BaseCommandId == 8
            //    string nodeColor = "#c0c0c0";
            //    var methodCalled = _codeVortoService.StatementReferenceMasterRepository
            //        .GetDataFromSqlQuery<StatementReferenceMaster>(
            //            "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
            //    if (methodCalled.Count != 0)
            //        nodeColor = "#00ffff";
            //    nodeId++;

            //    Node node;
            //    if (treeView.PrimaryCommandId == 51)
            //    {
            //        string[] strSplit = methodCalled[0].ClassNameDeclared.Split('.');
            //        string strName = "";
            //        if (strSplit.Length > 2)
            //        {
            //            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
            //        }
            //        node = new Node
            //        {
            //            Id = nodeId,
            //            ShapeId = "RoundRect",
            //            Name = strName.ToUpper(),
            //            Color = nodeColor,
            //            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //            GroupName = treeView.GroupName,
            //            GroupId = treeView.GroupId
            //        };
            //    }
            //    else
            //    {
            //        node = new Node
            //        {
            //            Id = nodeId,
            //            ShapeId = "RoundRect",
            //            Name = methodCalled[0].ClassNameDeclared,
            //            Color = nodeColor,
            //            StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //            GroupName = treeView.GroupName,
            //            GroupId = treeView.GroupId
            //        };

            //    }
            //    treeViewData.Nodes.Add(node);
            //    string m = string.Empty;
            //    if (treeView.PrimaryCommandId == 51)
            //    {
            //        m = treeView.StatementReferenceMaster.MethodName;
            //    }
            //    else
            //    {
            //        m = treeView.StatementReferenceMaster.MethodCalled;
            //    }
            //    if (m != null)
            //    {
            //        m = m + "()";
            //        treeViewData.Links.Add(new Link
            //        {
            //            //Origin = lastNode.Id,
            //            Origin = nodeId - 1,
            //            Target = nodeId,
            //            LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
            //            StatementId = treeView.StatementReferenceMaster.StatementId
            //        });
            //    }
            //    else
            //    {
            //        treeViewData.Links.Add(new Link
            //        {
            //            //Origin = lastNode.Id,
            //            Origin = nodeId - 1,
            //            Target = nodeId,
            //            LinkText = "[" + linkSeqNumber + "] "
            //        });
            //    }
            //    linkSeqNumber++;
            //    var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
            //        .ToList().Distinct();
            //    foreach (var cItem in childItems)
            //    {
            //        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
            //    }
            //    #endregion
            //}

            //else if (treeView.BaseCommandId == 10)
            //{
            //    #region BaseCommandId == 10

            //    var parentIf =
            //        (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
            //    if (parentIf == null) return treeViewData;

            //    nodeId++;
            //    string ifPart = Regex.Split(parentIf.GraphName, "If", RegexOptions.IgnoreCase)[1];
            //    var condition = ifPart.Contains("Then")
            //        ? ifPart.Substring(0, ifPart.IndexOf("Then", StringComparison.InvariantCulture))
            //        : ifPart;
            //    condition = "If NOT " + condition + " Then ";
            //    width = _clsUniverseBasic.CalculateWidth(condition.Length);
            //    height = _clsUniverseBasic.CalculateHeight(condition.Length);
            //    var node = new Node
            //    {
            //        Id = nodeId,
            //        ShapeId = "Decision2",
            //        Name = condition,
            //        Color = "#ff6600",
            //        Width = width,
            //        Height = height,
            //        StatementId = Int32.Parse(treeView.ActualStatementId.Split('_')[1]),
            //        GroupName = treeView.GroupName,
            //        GroupId = treeView.GroupId
            //    };
            //    treeViewData.Nodes.Add(node);

            //    treeViewData.Links.Add(new Link
            //    {
            //        Origin = nodeId - 1,
            //        Target = nodeId,
            //        LinkText = "[" + linkSeqNumber + "] "
            //    });
            //    linkSeqNumber++;

            //    #endregion
            //}
            else if (treeView.BaseCommandId == 10)
            {
                #region BaseCommandId == 10

                var parentIf =
                    (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                if (parentIf == null) return treeViewData;

                nodeId++;
                string ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                var condition = ifPart.Contains("THEN")
                    ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                    : ifPart;
                condition = "IF NOT" + condition.Replace("Then", "") + " THEN";
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

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string color, string groupName, int groupId)
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
                childNodes.RemoveAll(item => item == null);
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

        //[HttpGet]
        //public async Task<IHttpActionResult> AppendTreeNodes(string statementId, int projectId, int treeNodeId)
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {
        //        var sqlQuery = "Select * from StatementReferenceMaster where StatementId = " + Int32.Parse(statementId);
        //        var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
        //            .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
        //        if (statementMaster == null) return NotFound();

        //        var treeView = new TreeView
        //        {
        //            ClassCalled = statementMaster[0].ClassCalled
        //        };
        //        int fileId = statementMaster[0].FileId;
        //        int auto = 300;
        //        var listItems = GetCallExternalDetails(statementId, treeView, new List<TreeView>(), projectId, fileId,
        //            ref auto, ref treeNodeId);
        //        return Ok(listItems);
        //    }
        //}

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int fileId, int indentLevel, ref int auto, ref int treeNodeId,
            ref Dictionary<string, List<string>> callingAndCalled)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (treeView.ClassCalled == null) return lstTreeView;

                string className = treeView.ClassCalled;
                var callExtExpandedCode = GetGenericBlock(className, projectId);

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
                            ActualStatementId = "Missing" + auto + "_99999999",
                            IndentLevel = indentLevel
                        });
                    }
                }
                else
                {
                    foreach (var statementMaster in callExtExpandedCode)
                    {
                        try
                        {
                            if (!treeView.MethodCalled.StartsWith(statementMaster.MethodName)) continue;
                            int blockStmtId = statementMaster.StatementId;

                            var stmtsBlock = GetMethodBlock(blockStmtId);

                            var calledInternals =
                                (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                            callingAndCalled.Add(treeView.MethodCalled + "_" + auto, calledInternals);

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
                                    indentLevel = indentLevel + 1;
                                    lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                        ref auto,
                                        ref treeNodeId, ref callingAndCalled);
                                    indentLevel = indentLevel - 1;
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
                                        lstTreeView, projectId, fileId, indentLevel, ref auto, ref treeNodeId,
                                        ref callingAndCalled);
                                    indentLevel = indentLevel - 1;
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
                        catch (Exception ex)
                        {
                            ex.ToString();
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
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                };
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters).Result;
                if (stmtMaster.Count == 0)
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
                            GraphName = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>",
                            BaseCommandId = 25,
                            StatementReferenceMaster = treeView.StatementReferenceMaster,
                            NodeId = ++treeNodeId,
                            ActualStatementId = "Missing" + autoInt + "_99999999",
                            IndentLevel = indentLevel
                        });
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
                                ActualStatementId = "Missing" + autoInt + "_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                    else
                    {
                        var calledInternals =
                            (from b in callExtExpandedCode where b.BaseCommandId == 5 select b.MethodCalled).ToList();
                        string mName = methodName + "_" + autoInt;
                        callingAndCalled.Add(mName, calledInternals);
                        bool result = false;

                        foreach (var block in callExtExpandedCode)
                        {
                            autoInt++;
                            if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
                            {
                                foreach (var keyValPair in callingAndCalled.Where(s => s.Key == mName))
                                {
                                    var internalVal = keyValPair.Value;
                                    var block1 = block;
                                    foreach (var gosub in internalVal.Where(s => s.StartsWith(block1.MethodCalled)))
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
                                    lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                    ref autoInt,
                                    ref treeNodeId, ref callingAndCalled);
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
                                indentLevel = indentLevel + 1;
                                lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                    ref autoInt,
                                    ref treeNodeId, ref callingAndCalled);
                                indentLevel = indentLevel - 1;
                            }
                            else
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

        //private List<StatementReferenceMaster> GetGenericBlock(string className)
        //{
        //    object[] parametersExp =
        //    {
        //        new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
        //    };
        //    var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
        //        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parametersExp)
        //        .ContinueWith(t => t.Result).Result;
        //    return callExtExpandedCode;
        //}

        private List<StatementReferenceMaster> GetGenericBlock(string className, int projectId)
        {
            object[] parametersExp =
            {
                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className},
                new MySqlParameter("@projId", MySqlDbType.VarChar) {Value = projectId}
            };
            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                .ExecuteStoreProcedure<StatementReferenceMaster>("SpVBAGetClassMethods", parametersExp)
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


        [HttpGet]
        public object AlterWithStatement(List<string> allLines)
        {
            allLines = allLines.Select(s => s.Trim()).ToList();
            var loopLines = allLines;
            var fileLines = loopLines.ToArray();
            var methodBlockListMain = new List<string>();
            var methodBlock = fileLines.ToList();
            var indexPositionPrgm = -1;
            var iIndex = -1;
            foreach (var method in methodBlock)
            {
                indexPositionPrgm++;
                if (!method.StartsWith("With "))
                    continue;
                var withBlock = methodBlock.GetBlockOfLinesAsList(indexPositionPrgm, "End With");
                foreach (var line in withBlock)
                {
                    iIndex++;
                    var newline = withBlock[0];
                    var aaaa = newline.Replace("With", "");
                    if (iIndex > 0)
                    {
                        string stratingDot = line;
                        string modifyLine;
                        if (stratingDot.StartsWith('.'.ToString()))
                        {
                            modifyLine = aaaa + stratingDot;
                            methodBlockListMain.Add(modifyLine);
                        }
                        else if (!stratingDot.StartsWith('.'.ToString()) && stratingDot.Contains('.'))
                        {
                            int index = stratingDot.IndexOf(".", StringComparison.Ordinal);
                            string d = stratingDot.Substring(index - 1);
                            if (string.IsNullOrEmpty(d))
                            {
                                modifyLine = aaaa + stratingDot;
                                methodBlockListMain.Add(modifyLine);
                            }
                            methodBlockListMain.Add(stratingDot);
                        }
                        else if (!stratingDot.StartsWith("End With"))
                        {
                            methodBlockListMain.Add(stratingDot);
                        }
                    }
                }
            }
            return methodBlockListMain;
        }

        [HttpGet]
        public object RemoveBeginStatement()
        {
            var allFiles = @"D:\Auctor\CodeVorto\ExportedApp\Reports\Report_rptRenewalForms.rpt";
            var allLines = File.ReadAllLines(allFiles).ToList();
            var selectBlock = new List<string>();
            var methodBlock = allLines.ToList();
            var methodMain = methodBlock.ToArray();
            methodMain = methodMain.CombineAllSelectStatmentForRecordSource('\"');
            var indexPositionPrgm = -1;
            int flag = 0;
            foreach (var method in methodMain)
            {
                indexPositionPrgm++;
                if (method.StartsWith("Begin Form") || method.StartsWith("Begin Report"))
                {
                    flag = 1;
                }
                if (method.StartsWith("CodeBehindForm"))
                {
                    flag = 0;
                }

                if (flag == 0 || method.TrimStart().StartsWith("RecordSource"))
                {
                    if (method.TrimStart().StartsWith("RecordSource"))
                    {
                        string finalStat = String.Empty;
                        string[] strSplit = method.Split('=');
                        string statement = strSplit[1].Replace('\"', ' ');
                        if (statement.StartsWith("Select"))
                        {
                            finalStat += statement;
                            indexPositionPrgm++;
                        }
                    }
                    selectBlock.Add(method);
                }
            }
            return selectBlock;
        }

        [HttpGet]
        public async Task<IHttpActionResult> EntityLogic(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var regexSql = new Regex(@"[\""]SELECT \*");
                var fileMasterData =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                List<int> fileIds = (from f in fileMasterData select f.FileId).ToList();
                object[] andParameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""},
                };
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", andParameters);
                var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;

                foreach (var constructor in fileMasterData)
                {
                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.Contains("\"Select") ||
                                                                               x.OriginalStatement.Contains("\"SELECT")))
                        .ToList();
                    string result = string.Empty;
                    List<DataDependency> lstDataDependency = new List<DataDependency>();

                    if (selectStatName.Any())
                    {
                        foreach (var itemSelect in selectStatName)
                        {
                            int aaa = 0;
                            string bbb = string.Empty;
                            string attrStmt = string.Empty;
                            string attributes = null;
                            //itemSelect.OriginalStatement = "RecordSource ='SELECT tblOperators.ID, tblOperators.OperatorID, tblOperators.SSN, tblOperators.OpLastName, tblOperators.OpFirstName, tblOperators.OpMember, tblOperators.OpBartender, tblOperators.OpAssign, tblOperators.OpAnnualBingo, tblOperators.OpPullTabs, tblOperators.OpSuspended FROM tblOperators WHERE (((tblOperators.OpMember)=True) AND ((tblOperators.OpSuspended)=False)) OR (((tblOperators.OpBartender)=True) AND ((tblOperators.OpSuspended)=False)) ORDER BY tblOperators.OpLastName, tblOperators.OpFirstName;'";

                            if (regexSql.IsMatch(itemSelect.OriginalStatement.ToUpper().TrimEnd()))
                            {
                                if (itemSelect.OriginalStatement.Contains("where") ||
                                    itemSelect.OriginalStatement.Contains("WHERE"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("where",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(0, aaa).Trim();
                                    //result = bbb.Split(' ').LastOrDefault();
                                    bbb = RemoveCheckSpecialChars(bbb);
                                    result = bbb.Trim().Split(' ').LastOrDefault();

                                    attrStmt = itemSelect.OriginalStatement.Substring(aaa).Trim();
                                    string[] value = attrStmt.Split(' ');
                                    for (int i = 0; i < value.Length; i++)
                                    {
                                        if (!value[i].StartsWith("=") && value[i].Contains("="))
                                        {
                                            attributes = attributes + "," + value[i].Split('=').FirstOrDefault();
                                        }
                                        else
                                        {
                                            if (value[i] == "=" || value[i] == "=\"")
                                            {
                                                attributes = attributes + "," + value[i - 1];
                                            }
                                        }
                                    }

                                    if (!string.IsNullOrEmpty(attributes))
                                    {
                                        attributes = RemoveSpecialChars(attributes);
                                        attributes = attributes.TrimStart(',').Trim();
                                    }
                                }
                                else
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim();
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);
                                }
                            }
                            else
                            {
                                if (itemSelect.OriginalStatement.Contains("from") ||
                                    itemSelect.OriginalStatement.Contains("FROM"))
                                {
                                    aaa = itemSelect.OriginalStatement.IndexOf("from",
                                        StringComparison.OrdinalIgnoreCase);
                                    bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim().Replace("GROUP", "")
                                        .Replace("WHERE", "");
                                    result = bbb.Split(' ').FirstOrDefault();
                                    result = RemoveSpecialChars(result);

                                    attrStmt = itemSelect.OriginalStatement.Substring(0, aaa).Split('=')
                                        .LastOrDefault().Trim().Replace("Select", "").Replace("SELECT", "")
                                        .Replace("DISTINCTROW", "").Trim();

                                    if (attrStmt.Contains(","))
                                    {
                                        string[] value = attrStmt.Split(',');
                                        for (int j = 0; j < value.Length; j++)
                                        {
                                            attributes = attributes + "," + value[j].TrimStart().TrimEnd();
                                        }
                                    }
                                    else
                                    {
                                        attributes = attrStmt;
                                    }
                                    attributes = RemoveSpecialChars(attributes);
                                    attributes = attributes.TrimStart(',').Trim();
                                }
                            }


                            var dataDependencyRepository = new DataDependencyRepository(new AppDbContext());
                            DataDependency datadependency = new DataDependency
                            {
                                DataDepedencyId = 0,
                                ProjectId = projectId,
                                FileId = constructor.FileId,
                                Entity = result,
                                Attributes = attributes
                            };

                            List<int> iCnt = (from dd in lstDataDependency
                                              where dd.Entity == result && dd.FileId == constructor.FileId
                                              select dd.FileId).ToList();
                            if (iCnt.Count == 0)
                            {
                                lstDataDependency.Add(datadependency);
                            }
                        }

                        await _codeVortoService.DataDependencyRepository.BulkInsert(listOfEntities: lstDataDependency);
                    }
                }
                return Ok("Entity inserted successfully");
            }
        }

        public string RemoveSpecialChars(string input)
        {
            return Regex.Replace(input, @"[^a-zA-Z\._,]", string.Empty);
        }

        public string RemoveCheckSpecialChars(string input)
        {
            return Regex.Replace(input, @"[^0-9a-zA-Z\._,*=() ]", string.Empty);
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseViewSourceData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                IEnumerable<FileMaster> copyOfFileMasters = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                if (projectDetails.ProjectConfigType == 7)
                {
                    var vbprojFiles = Directory.GetFiles(projectDetails.PhysicalPath, "*.vbproj",
                        SearchOption.AllDirectories);
                    foreach (var vbprojFile in vbprojFiles)
                    {
                        var xmlDocument = new XmlDocument();
                        xmlDocument.Load(vbprojFile);
                        XmlNode xmlNode = xmlDocument.GetElementsByTagName("OutputType")[0];
                        if (xmlNode.InnerText == "Library")
                        {
                            copyOfFileMasters =
                                copyOfFileMasters.Where(f => f.FilePath.EndsWith(".Designer.vb")).ToList();
                        }
                    }
                }

                foreach (var file in copyOfFileMasters) //.Where(f => f.FileTypeExtensionId == 13 && f.Processed == 0))
                                                        // 16,13
                {
                    var allLines = File.ReadAllLines(file.FilePath);

                    #region Insert the data in ViewSourceMaster Table

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

                    #endregion
                }
            }
            return Ok("Process View Source Data");
        }

        //[HttpGet]
        //public string RemoveBeginStatement(string method)
        //{

        //    method = "If rstLocations.EOF Then Exit Sub";
        //    int val = method.IndexOf("Exit");
        //    string firstVal = method.Substring(0, val);
        //    string secondVal = method.Substring(val);
        //    //string aaaaaa =
        //    //    "Call RunReportAsPDF('rptQualifiedOrganizations1-for-web', 'K:\\IT Div\\inetpub\\wwwroot\\gaming\\charity\\pdfspqualifiedorganizations.pdf')";
        //    //aaaaaa = aaaaaa.Between("\'", "\'").ToString();
        //    //string aaaaaa1 = aaaaaa.Split(' ').FirstOrDefault();
        //    //aaaaaa1 = aaaaaa1.Remove(aaaaaa1.Length - 1, 1).Replace('\'', '');
        //    ////int aaa = aaaaaa.IndexOf("'");
        //    ////string aaaaaa1 = aaaaaa.Substring(aaa, '\'');
        //    return method;
        //}
    }
}