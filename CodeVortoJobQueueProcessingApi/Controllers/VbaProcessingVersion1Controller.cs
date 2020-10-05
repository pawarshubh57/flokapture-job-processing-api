using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.VisualBasicVba;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;

// ReSharper disable AccessToModifiedClosure

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class VbaProcessingVersion1Controller : ApiController
    {
        private ICodeVortoService _codeVortoService;
        // private static readonly ILog Log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        public VbaProcessingVersion1Controller()
        {
        }

        public VbaProcessingVersion1Controller(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public IHttpActionResult StartProcess(int projectId)
        {
            Task.Factory.StartNew(async () => { await ExecuteProcessActionsOneByOne(projectId); });
            return Ok("Processing Started successfully!");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ExecuteProcessActionsOneByOne(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest("Details provided are not correct.");

                var solutionId = projectMaster.SolutionId ?? 6;
                // Update Project Processing Statud to 3, so that it can be shown on load projects page...
                projectMaster.Processed = 3;
                projectMaster.LanguageMaster = null;
                projectMaster.ProjectType = null;

                var updatedProjectMaster = await _codeVortoService.ProjectMasterRepository.UpdateItem(projectMaster);
                Console.WriteLine(JsonConvert.SerializeObject(updatedProjectMaster));

                var startDate = DateTime.Now;
                string startStatus;
                string endStatus;
                string dashLine = "========================================================================\n";
                var processStepList = await _codeVortoService.ProjectProcessingStepRepository
                    .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);


                // StartProcessUbProject

                var projectProcessStep = processStepList.Find(x => x.ProcessStep == "StartProcessVbaProject");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: StartProcessVbaProject\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await StartProcessVbaProject(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: StartProcessVbaProject\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ParseMsAccessVbaProject

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ParseMsAccessVbaProject");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ParseMsAccessVbaProject\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ParseMsAccessVbaProject(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ParseMsAccessVbaProject\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ProcessMsAccessVbaProject

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessMsAccessVbaProject");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessMsAccessVbaProject\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessMsAccessVbaProject(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ProcessMsAccessVbaProject\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }
                
                // ProcessDataDependency

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessDataDependency");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessDataDependency\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessDataDependency(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ProcessDataDependency\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ProcessDbCrudActivity

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessDbCrudActivity");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessDbCrudActivity\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessDbCrudActivity(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ProcessDbCrudActivity\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ViewSource

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ViewSource");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ViewSource\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ViewSource(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ViewSource\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ProcessAllActionWorkflows

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessAllActionWorkflows");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessAllActionWorkflows\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessAllActionWorkflows(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ProcessAllActionWorkflows\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ProcessObjectConnectivityDiagram

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessObjectConnectivityDiagram");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessObjectConnectivityDiagram\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessObjectConnectivityDiagram(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: ProcessObjectConnectivityDiagram\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // GetAllStartingPoints

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "GetAllStartingPoints");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: GetAllStartingPoints\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await GetAllStartingPoints(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    endStatus = "Completed executing Method: GetAllStartingPoints\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ApplyPseudoCodeConversion

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ApplyPseudoCodeConversion");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ApplyPseudoCodeConversion\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    ApplyPseudoCodeConversion(projectMaster.ProjectId);

                    endStatus = "Completed executing Method: ApplyPseudoCodeConversion\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }

                // ProcessForProjectInventory

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForProjectInventory");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: ProcessForProjectInventory\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await ProcessForProjectInventory(projectMaster.ProjectId, solutionId).ConfigureAwait(false);

                    endStatus = "Completed executing Method: ProcessForProjectInventory\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }
                // UpdateProjectStatus

                projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateProjectStatus");
                if (projectProcessStep != null && !projectProcessStep.Status)
                {
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await UpdateProjectStatus(projectMaster).ConfigureAwait(false);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                }
                var endDate = DateTime.Now;
                Console.WriteLine(dashLine);
                Console.WriteLine("Project processing started on: " + startDate);
                stringBuilder.AppendLine().AppendLine(dashLine)
                    .AppendLine("Project processing started on: " + startDate)
                    .AppendLine("Project processing completed on: " + endDate)
                    .AppendLine(dashLine).AppendLine();
                Console.WriteLine("Project processing completed on: " + endDate);
                Console.WriteLine();
                Console.WriteLine(dashLine);
                var projectJson = JsonConvert.SerializeObject(projectMaster, Formatting.Indented);

                stringBuilder.AppendLine(dashLine).AppendLine().AppendLine(projectJson)
                    .AppendLine(dashLine).AppendLine(dashLine);
                LogMessage.WriteLogMessage(stringBuilder);

                return Ok(projectJson);
            }
        }

        [HttpGet]
        public async Task<HttpResponseMessage> StartProcessVbaProject(int projectId)
        {
            var stringBuilder = new StringBuilder();

            using (_codeVortoService = new CodeVortoService())
            {
                var projectConfigFilesGeneralRepository = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectDetails == null)
                    return new HttpResponseMessage(HttpStatusCode.BadRequest)
                    {
                        Content = new StringContent("Project not found: " + projectId)
                    };
                stringBuilder.AppendLine("\n" + "Started process for project: " + projectId + " project name: " +
                                         projectDetails.ProjectName + " and project physical path is: " +
                                         projectDetails.PhysicalPath);
                var extensionList = await _codeVortoService.FileTypeExtensionRepository.GetAllItems(
                    p => p.LanguageId == projectDetails.LanguageId);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                string projectPath = projectDetails.PhysicalPath;
                var directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                var projectImpFiles =
                    await
                        projectConfigFilesGeneralRepository.GetAllItems(p => p.ConfigFileId != 2 && p.ConfigFileId != 7
                                                                             && p.ConfigFileId != 8 &&
                                                                             p.ConfigFileId != 9 && p.ConfigFileId != 10);
                strExtensions.AddRange(projectImpFiles.Distinct().Select(e => e.ToString()));
                stringBuilder.AppendLine("\n" + "Extensions: " + string.Join(",", strExtensions));
                var alreadyImportedFiles =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                var copyOfFileMaster = alreadyImportedFiles.ToList();

                foreach (var directory in directoryList)
                {
                    try
                    {
                        var allFiles =
                            Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                                .Where(s => strExtensions.Any(s.EndsWith));
                        var enumerator = allFiles.GetEnumerator();
                        var lstFileMasters = new List<FileMaster>();
                        while (enumerator.MoveNext())
                        {
                            string currentFile = enumerator.Current;
                            string fileName = Path.GetFileName(currentFile);
                            if (ignoredFile.Any(f => f.FileName == fileName)) continue;
                            if (copyOfFileMaster.Any(f => f.FilePath == currentFile)) continue;
                            if (string.IsNullOrEmpty(fileName)) continue;
                            if (fileName.Contains(".dll config")) continue;
                            string extension = Path.GetExtension(currentFile);

                            int extensionId =
                                fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                            var fileLines = File.ReadAllLines(currentFile).ToList();
                            fileLines = fileLines.Select(l => l.Trim()).ToList();
                            var regEx = new Regex("^\'", RegexOptions.Singleline);
                            int fileLinesCount = fileLines.Count(l => !string.IsNullOrWhiteSpace(l)
                            && (!l.StartsWith("'") || !regEx.IsMatch(l)));

                            var fileMaster = new FileMaster
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = currentFile,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = projectDetails.SolutionId,
                                LinesCount = fileLinesCount,
                                IsNewVersion = 0
                            };
                            lstFileMasters.Add(fileMaster);
                        }
                        enumerator.Dispose();
                        await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters);
                        stringBuilder.AppendLine("\n" + "Total files scaned: " + lstFileMasters.Count);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }
                LogMessage.WriteLogMessage(stringBuilder);

                return new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent("Done") };
                //stringBuilder.AppendLine("Started executing next process: ParseMsAccessVbaProject(" + projectId + ")");
                //LogMessage.WriteLogMessage(stringBuilder);

                //var processResult = await ParseMsAccessVbaProject(projectId);
                //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                //string data = await dataContent.Content.ReadAsStringAsync();
                //return Ok(data);
            }
        }


        [HttpGet]
        public async Task<IHttpActionResult> ParseMsAccessVbaProject(int projectId)
        {
            var stringBuilder = new StringBuilder();

            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                    var projectDetails = projectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);

                    int languageId = projectDetails.LanguageId;
                    var fileMaster =
                        await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                    IEnumerable<FileMaster> copyOfFileMasters = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                    stringBuilder.AppendLine("Total file for parsing: " + copyOfFileMasters.Count());

                    #region Start reading file lines and do process before dump to database

                    foreach (var file in copyOfFileMasters.Where(f => f.FileTypeExtensionId == 15 && f.Processed == 0))
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);

                        var allLines = File.ReadAllLines(file.FilePath).ToList();
                        allLines = allLines.Select(l => l.Trim()).ToList();
                        var regEx = new Regex("^\'", RegexOptions.Singleline);
                        allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && (!l.StartsWith("'") || !regEx.IsMatch(l))).ToList();
                        var loopLines = allLines.ToList();
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        foreach (string fLine in fileLines)
                        {
                            string tempString = fLine.TrimStart().Trim(' ', '\t');
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', "");
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.VbaRemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                        var programLineList = new List<string>(fileLines);
                        var tempBlock = programLineList.VbaAlterWithStatement();
                        tempBlock = tempBlock.ConvertCaseStatement();
                        tempBlock = tempBlock.VbaRemoveBeginFormStatement();
                        tempBlock = tempBlock.CheckExitSubInIf();
                        var mainBlock = tempBlock.SplitIfElseThenStatement(false);
                        mainBlock = mainBlock.SplitElseIfStatements();
                        stringBuilder.AppendLine("Started executing next process: ParseVbaFile(" + projectId + ")");

                        await ParseVbaFile(projectId, languageId, mainBlock, file);
                        file.Processed = 1;
                        file.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(file);
                    }
                    foreach (var file in copyOfFileMasters.Where(f => f.FileTypeExtensionId == 16 && f.Processed == 0))
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);

                        var allLines = File.ReadAllLines(file.FilePath).ToList();
                        allLines = allLines.Select(l => l.Trim()).ToList();
                        var regEx = new Regex("^\'", RegexOptions.Singleline);
                        allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && (!l.StartsWith("'") || !regEx.IsMatch(l))).ToList();
                        var loopLines = allLines.ToList();
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        foreach (string fLine in fileLines)
                        {
                            string tempString = fLine.TrimStart().Trim(' ', '\t');
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', "");
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.VbaRemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                        var programLineList = new List<string>(fileLines);
                        var tempBlock = programLineList.VbaAlterWithStatement();
                        tempBlock = tempBlock.ConvertCaseStatement();
                        tempBlock = tempBlock.VbaRemoveBeginFormStatement();
                        tempBlock = tempBlock.CheckExitSubInIf();
                        tempBlock = tempBlock.RemoveBeginStatement(); // Collect all Begin & query part
                        var mainBlock = tempBlock.SplitIfElseThenStatement(false);
                        mainBlock = mainBlock.SplitElseIfStatements();
                        stringBuilder.AppendLine("Started executing next process: ParseVbaFile(" + projectId + ")");
                        await ParseVbaFile(projectId, languageId, mainBlock, file);
                        file.Processed = 1;
                        file.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(file);
                    }

                    foreach (var file in copyOfFileMasters.Where(f => f.FileTypeExtensionId == 13 && f.Processed == 0))
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);

                        var allLines = File.ReadAllLines(file.FilePath).ToList();
                        allLines = allLines.Select(l => l.Trim()).ToList();
                        var regEx = new Regex("^\'", RegexOptions.Singleline);
                        allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && (!l.StartsWith("'") || !regEx.IsMatch(l))).ToList();
                        var loopLines = allLines.ToList();
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        foreach (string fLine in fileLines)
                        {
                            string tempString = fLine.TrimStart().Trim(' ', '\t');
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', "");
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.VbaRemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                        var programLineList = new List<string>(fileLines);
                        var tempBlock = programLineList.VbaAlterWithStatement();
                        tempBlock = tempBlock.ConvertCaseStatement();
                        tempBlock = tempBlock.VbaRemoveBeginFormStatement();
                        tempBlock = tempBlock.CheckExitSubInIf();
                        tempBlock = tempBlock.GetRecordSourceStatement(); // Collect all the RecordSource statements
                        var mainBlock = tempBlock.SplitIfElseThenStatement(false);
                        mainBlock = mainBlock.SplitElseIfStatements();
                        stringBuilder.AppendLine("Started executing next process: ParseVbaFile(" + projectId + ")");
                        await ParseVbaFile(projectId, languageId, mainBlock, file);
                        file.Processed = 1;
                        file.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(file);
                    }

                    foreach (var file in copyOfFileMasters.Where(f => f.FileTypeExtensionId == 14
                                                                      && f.Processed == 0 &&
                                                                      f.FileName.StartsWith("sub")))
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);

                        var allLines = File.ReadAllLines(file.FilePath).ToList();
                        allLines = allLines.Select(l => l.Trim()).ToList();
                        var regEx = new Regex("^\'", RegexOptions.Singleline);
                        allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && (!l.StartsWith("'") || !regEx.IsMatch(l))).ToList();
                        var loopLines = allLines.ToList();
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        foreach (string fLine in fileLines)
                        {
                            string tempString = fLine.TrimStart().Trim(' ', '\t');
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', "");
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.VbaRemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                        var programLineList = new List<string>(fileLines);
                        var tempBlock = programLineList.VbaAlterWithStatement();
                        tempBlock = tempBlock.ConvertCaseStatement();
                        tempBlock = tempBlock.VbaRemoveBeginFormStatement();
                        tempBlock = tempBlock.CheckExitSubInIf();
                        var mainBlock = tempBlock.SplitIfElseThenStatement(false);
                        mainBlock = mainBlock.SplitElseIfStatements();
                        await ParseVbaFile(projectId, languageId, mainBlock, file);
                        file.Processed = 1;
                        file.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(file);
                    }

                    fileMaster =
                        await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                    copyOfFileMasters = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                    foreach (var file in copyOfFileMasters.Where(f => f.FileTypeExtensionId == 14
                                                                      && f.Processed == 0))
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);

                        var allLines = File.ReadAllLines(file.FilePath).ToList();
                        allLines = allLines.Select(l => l.Trim()).ToList();
                        var regEx = new Regex("^\'", RegexOptions.Singleline);
                        allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && (!l.StartsWith("'") || !regEx.IsMatch(l))).ToList();
                        var loopLines = allLines.ToList();
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        foreach (string fLine in fileLines)
                        {
                            string tempString = fLine.TrimStart().Trim(' ', '\t');
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', "");
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.VbaRemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsVbaAdjustment();
                        var programLineList = new List<string>(fileLines);
                        var tempBlock = programLineList.VbaAlterWithStatement();
                        tempBlock = tempBlock.ConvertCaseStatement();
                        tempBlock = tempBlock.VbaRemoveBeginFormStatement();
                        tempBlock = tempBlock.CheckExitSubInIf();
                        var mainBlock = tempBlock.SplitIfElseThenStatement(false);
                        mainBlock = mainBlock.SplitElseIfStatements();
                        stringBuilder.AppendLine("Started executing next process: ParseVbaFile(" + projectId + ")");

                        await ParseVbaFile(projectId, languageId, mainBlock, file).ConfigureAwait(false);
                        file.Processed = 1;
                        file.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(file).ConfigureAwait(false);
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                #endregion
            }
            return Ok("Done");
            //stringBuilder.AppendLine("Started executing next process: ProcessMsAccessVbaProject(" + projectId + ")");
            //LogMessage.WriteLogMessage(stringBuilder);
            //IHttpActionResult processResult = await ProcessMsAccessVbaProject(projectId);
            //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
            //string data = await dataContent.Content.ReadAsStringAsync();
            //return Ok(data);
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseVbaFile(int projectId, int languageId, List<string> tempBlock,
            FileMaster file)
        {
            try
            {
                var stringBuilder = new StringBuilder();

                #region Get all Primary command and base command information to parse file...

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllItems().ContinueWith(t =>
                {
                    var result = t.Result;
                    return result.ToList();
                });
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
                var classStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference
                    .ToList().FindAll(l => l.LanguageId == languageId);
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
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callExternalStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                var visualBasicvba1 = new VisualBasicVba1();

                #endregion

                #region Add Default Entry for BaseCommandId = 19

                stringBuilder.AppendLine("Started dump statement into database of file: " + file.FileName);

                List<StatementReferenceMaster> lstStatementReferenceMaster;
                if (file.FileTypeExtensionId == 16 || file.FileTypeExtensionId == 13)
                {
                    lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterStart(file,
                        classStartIndicator[0].PrimaryReferenceId, methodIndicationStart[0].PrimaryReferenceId);
                }
                else
                {
                    lstStatementReferenceMaster = visualBasicvba1.PrepareStatementReferenceMasterStart(file,
                        classStartIndicator[0].PrimaryReferenceId);
                }

                await
                    _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                #endregion

                #region Starting parsing of file line by line

                if (file.FileTypeExtensionId == 14)
                {
                    int index = -1;
                    foreach (var temp in tempBlock)
                    {
                        index++;
                        if (!temp.StartsWith("Public Sub") && !temp.StartsWith("Private Sub")) continue;

                        tempBlock.Insert(index, "End Sub");
                        break;
                    }
                }

                var indexPosition = -1;
                foreach (var line in tempBlock)
                {
                    try
                    {
                        bool lineDone = false;
                        indexPosition = indexPosition + 1;
                        if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                            continue;

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
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
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
                                    BaseCommandId = 0,
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
                            StatementReferenceMaster stmtReferenceMaster;
                            var lineNew = line;

                            string aaaa = lineNew.Split('=')[1].Replace("\"", "");
                            if (aaaa.StartsWith("select") || aaaa.StartsWith("SELECT"))
                            {
                                stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = line,
                                    OriginalStatement = line,
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = 0,
                                    PrimaryCommandId = cStart.PrimaryReferenceId,
                                    ParsedOrNot = null,
                                    ProjectId = projectId
                                };
                            }
                            else
                            {
                                stmtReferenceMaster = new StatementReferenceMaster
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
                            }
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            lineDone = true;
                            break;
                        }
                        if (lineDone) continue;

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
                        // if (lineDone) continue;
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
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }

                #endregion

                #region Default entry for files / forms those has a subform definition...

                // We will add extra method for this subform entry with MethodName as "FloKapture_Temp_1234()" 
                // If Multiple subforms are  called then add something extra start with _ like FloKapture_Temp_1234_Xyz()
                // Directly add BaseCommandId = 6 with ClassCalled = "subForm class name" AND Method Called = "Form_Load()"
                var allLines = File.ReadAllLines(file.FilePath).ToList();
                allLines = allLines.Select(l => l.Trim()).ToList();
                allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && !l.StartsWith("'")).ToList();
                List<string> subForms = _visualBasicVba1.GetSubForms(allLines);
                int tempMehodId = 0;
                foreach (var subForm in subForms)
                {
                    try
                    {
                        string sForm = subForm;
                        if (sForm.Contains('.'))
                            sForm = sForm.Split('.').LastOrDefault();
                        if (string.IsNullOrEmpty(sForm)) continue;
                        tempMehodId++;
                        var lstStatementMaster = new List<StatementReferenceMaster>
                        {
                            new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = "Private Sub FloKapture_Temp_1234_" + tempMehodId + "()",
                                OriginalStatement = "Private Sub FloKapture_Temp_1234_" + tempMehodId + "()",
                                ClassCalled = null,
                                MethodName = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                MethodCalled = null,
                                DataOrObjectType = null,
                                VariableNameDeclared = null,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BaseCommandId = 8,
                                StatementComment = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                BusinessName = "FloKapture Adjustment"
                            },
                            new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                OriginalStatement = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                ClassCalled = sForm.Trim(),
                                MethodName = null,
                                MethodCalled = "Form_Load()",
                                DataOrObjectType = null,
                                VariableNameDeclared = null,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BaseCommandId = 6,
                                StatementComment = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                BusinessName = "FloKapture Adjustment"
                            },
                            new StatementReferenceMaster
                            {
                                FileId = file.FileId,
                                ResolvedStatement = "End Sub",
                                OriginalStatement = "End Sub",
                                ClassCalled = null,
                                MethodName = null,
                                MethodCalled = null,
                                DataOrObjectType = null,
                                VariableNameDeclared = null,
                                PrimaryCommandId = 0,
                                ProjectId = projectId,
                                BaseCommandId = 9,
                                StatementComment = "FloKapture_Temp_1234_" + tempMehodId + "()",
                                BusinessName = "FloKapture Adjustment"
                            }
                        };
                        await _codeVortoService.StatementReferenceMasterRepository
                            .BulkInsert(lstStatementMaster).ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }

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
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster).ConfigureAwait(false);

                #endregion

                int ifCount = tempBlock.Count(s => s.StartsWith("If"));
                int endIfCount = tempBlock.Count(s => s.EndsWith("End If") || s.EndsWith("End If."));

                tempBlock.Add("==========================");
                tempBlock.Add("IF Counts: " + ifCount);
                tempBlock.Add("END Count: " + endIfCount);
                stringBuilder.AppendLine("Inserted all statement into database");
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessMsAccessVbaProject(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    #region start processing
                    stringBuilder.AppendLine("Started executing next process: ProcessMsAccessVbaProject(" + projectId + ")");
                    var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await baseCommandReferenceRepository.GetAllItems().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });
                    var declarationIndicator =
                        baseCommandReference.Find(s => s.BaseCommand == "Declaration").PrimaryLanguageReference
                            .ToList();
                    var fileMasterData =
                        await _codeVortoService.FileMasterRepository.GetAllListItems(p => p.ProjectId == projectId);

                    List<int> fileIds = (from f in fileMasterData select f.FileId).ToList();
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             ",)");
                    object[] andParameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""}
                    };
                    var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition",
                            andParameters);
                    var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;
                    var emptyLines = statementReferenceMasters.ToList()
                        .FindAll(s => s.ResolvedStatement.StartsWith("Dim "));
                    var declarations = declarationIndicator.ToList();
                    //Replace all Me. with className .in ResolvedStatement

                    object[] methodParameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var allMethodNames = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodNamesInProject",
                            methodParameters);

                    allMethodNames = allMethodNames.GroupBy(x => x.MethodName).Select(g => g.First()).ToList();

                    #endregion

                    #region region 1....

                    foreach (var fId in fileIds)
                    {
                        try
                        {
                            var id = fId;
                            string sqlQuery = " Select * from StatementReferenceMaster where ProjectId =" + projectId +
                                              " AND FileId =" + id + " AND ClassNameDeclared is not null;";
                            var className =
                                await
                                    _codeVortoService.StatementReferenceMasterRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                            if (!className.Any()) continue;
                            foreach (var clsName in className)
                            {
                                stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                                         clsName.StatementId + ",8,9)");
                                var genreicBlock = GetGenericBlock(clsName.StatementId, 19, 20);
                                var meContainsLines =
                                    genreicBlock.ToList()
                                        .FindAll(s => s.OriginalStatement.Contains(" Me.")
                                                      && s.ProjectId == projectId && s.FileId == id);
                                foreach (var l in meContainsLines)
                                {
                                    var l1 = l;
                                    string methodCalled;
                                    int firstCommaIndex = l1.ResolvedStatement.IndexOf(" Me.",
                                        StringComparison.InvariantCultureIgnoreCase);
                                    string m1 = l1.ResolvedStatement.Substring(firstCommaIndex + 4);
                                    bool result = m1.ContainsAll("(", ")");
                                    bool callInternalOrExternal = false;
                                    if (result)
                                    {
                                        methodCalled = m1.Split('(')[0].TrimEnd().Trim();
                                        if (allMethodNames.Any(
                                            method => methodCalled.Trim().Contains(method.MethodName)))
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
                                            ProjectId = projectId,
                                            FileMaster = null,
                                            ReferenceFileMaster = null

                                            //WorkFlowRelevant = "I"
                                        };
                                        await
                                            _codeVortoService.StatementReferenceMasterRepository.UpdateItem(
                                                stmtReferenceMaster);
                                    }
                                }
                            }
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    #region region 2...

                    stringBuilder.AppendLine("Started process for replace all constructor names: (" + projectId + ")");

                    // Replace all constructor Names...
                    string construstorSql =
                        "  Select * from statementreferencemaster where OriginalStatement like 'Public Sub New (  ) %' AND ProjectId = " +
                        projectId + "; ";
                    var constructors = await
                        baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(construstorSql);
                    foreach (var constructor in constructors)
                    {
                        try
                        {
                            string temp = constructor.ResolvedStatement;
                            string classSql =
                                " Select * from statementreferencemaster where FileId = " + constructor.FileId +
                                " AND baseCommandId = 19 AND ProjectId = " + projectId + " " +
                                " AND StatementId <= " + constructor.StatementId +
                                " order by StatementId DESC LIMIT 1;";
                            var className = await
                                baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(classSql);
                            if (!className.Any()) continue;

                            temp = temp.Replace("New", className[0].ClassNameDeclared);
                            constructor.ResolvedStatement = temp;
                            constructor.ProjectId = projectId;
                            constructor.FileMaster = null;
                            constructor.ReferenceFileMaster = null;
                            await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    stringBuilder.AppendLine("Replaced all constructor names.");

                    #endregion

                    #region Update Data I/O Statements and all statements that using those variable, set BaseCommand Id = 45...

                    using (_codeVortoService = new CodeVortoService())
                    {
                        string sqlOpenRecordset = " SELECT * FROM StatementReferenceMaster" +
                                                  " WHERE  ProjectId = " + projectId + " AND " +
                                                  "(OriginalStatement  LIKE '%.OpenRecordset%'  OR" +
                                                  " OriginalStatement like '%.Execute%'  OR " +
                                                  " OriginalStatement like '%.dbs.Close%' OR" +
                                                  " OriginalStatement like '% DLookup%') ; ";
                        var allRecordSetStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(sqlOpenRecordset).ConfigureAwait(false);
                        foreach (var statementRef in allRecordSetStatements)
                        {
                            string oStatement = statementRef.OriginalStatement.ToLower();
                            string cCalled = "Database";
                            //string cCalled = oStatement.Contains("openrecordset")
                            //    ? "Database"
                            //    : oStatement.Contains("dlookup") ? "UnKnown" : "Database";
                            string mCalled = oStatement.Contains("openrecordset")
                                ? "OpenRecordset ( )"
                                : oStatement.Contains("dlookup")
                                    ? "DLookup ( )"
                                    : oStatement.Contains(".execute")
                                        ? "Execute ( )"
                                        : "Close ( )";

                            statementRef.BaseCommandId = 6;
                            statementRef.ClassCalled = cCalled;
                            statementRef.MethodCalled = mCalled;
                            statementRef.FileMaster = null;
                            statementRef.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository
                                .UpdateItem(statementRef).ConfigureAwait(false);

                            var dataObjectType = statementRef.OriginalStatement.Replace("Set ", "");
                            string variableName = dataObjectType.Split(new[] { "=" }, StringSplitOptions.None)[0];

                            variableName = variableName.Trim().Replace("'", "");
                            string sqlDataIoStats =
                                " SELECT * FROM StatementReferenceMaster WHERE FileId = " + statementRef.FileId +
                                " AND (OriginalStatement LIKE '%" + variableName + ".%' OR OriginalStatement LIKE '%" +
                                variableName + "!%'); ";
                            var allDataIoStatements = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataIoStats).ConfigureAwait(false);

                            foreach (var dataIoState in allDataIoStatements)
                            {
                                if (dataIoState.BaseCommandId != 0) continue;

                                dataIoState.VariableNameDeclared = variableName;
                                dataIoState.BaseCommandId = 45;
                                dataIoState.FileMaster = null;
                                dataIoState.ReferenceFileMaster = null;
                                await _codeVortoService.StatementReferenceMasterRepository
                                    .UpdateItem(dataIoState).ConfigureAwait(false);
                            }
                            string sqlDataOtherStatemt =
                                " SELECT * FROM StatementReferenceMaster WHERE  OriginalStatement LIKE '%.orderby%' " +
                                " OR OriginalStatement LIKE '%.groupby%' " +
                                " OR OriginalStatement LIKE '%.filter%'" +
                                " OR OriginalStatement LIKE '%.AllowAdditions%'" +
                                " OR OriginalStatement LIKE '%.AllowFilters%' " +
                                " AND ProjectId = " + projectId + "; ";
                            var allDataOtherStatements = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataOtherStatemt)
                                .ConfigureAwait(false);
                            foreach (var dataOtherStatemt in allDataOtherStatements)
                            {
                                if (dataOtherStatemt.BaseCommandId != 0) continue;

                                dataOtherStatemt.VariableNameDeclared = variableName;
                                dataOtherStatemt.BaseCommandId = 45;
                                dataOtherStatemt.FileMaster = null;
                                dataOtherStatemt.ReferenceFileMaster = null;
                                await _codeVortoService.StatementReferenceMasterRepository
                                    .UpdateItem(dataOtherStatemt).ConfigureAwait(false);
                            }
                        }
                    }

                    #endregion

                    #region region 6...

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
                            emptyLine.DataOrObjectType = variableType;
                            emptyLine.VariableNameDeclared = variableName;
                            emptyLine.BaseCommandId = declaration.BaseCommandId;
                            emptyLine.PrimaryCommandId = declaration.PrimaryReferenceId;
                            emptyLine.ParsedOrNot = "Y";
                            emptyLine.ProjectId = projectId;
                            emptyLine.FileMaster = null;
                            emptyLine.ReferenceFileMaster = null;
                            // TODO: Remember to uncomment...
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(emptyLine);
                            //done = true;
                            break;
                        }
                    }

                    #endregion

                    #region region 3.. Update the classCalled and MethodCalled for base commandId = 6

                    stringBuilder.AppendLine(
                        "Started update classcalled and methodcalled for basecommandId = 6 where recordsource statement: (" +
                        projectId + ")");
                    var execStmtName1 =
                        statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 6).ToList();
                    foreach (var constructor in execStmtName1)
                    {
                        if (constructor.OriginalStatement.StartsWith("FloKapture_Temp_1234_")) continue;
                        string currentStmt = constructor.OriginalStatement.Replace("\"", "");

                        currentStmt = currentStmt.Split('=')[1].Replace("\'", "");
                        string fileName = string.Empty;

                        var regexSql1 = new Regex("\'(.*)'");
                        if (regexSql1.IsMatch(currentStmt.ToUpper().TrimEnd()))
                        {
                            fileName = currentStmt.Between("\'", "\'");
                        }

                        if (string.IsNullOrEmpty(fileName))
                        {
                            fileName = currentStmt.Replace("\"", "");
                        }
                        var allCheckFiles = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(f => f.ProjectId == projectId &&
                                                  f.ClassNameDeclared == fileName.TrimEnd());


                        if (!allCheckFiles.Any())
                        {
                            // constructor.MethodCalled = "";
                            constructor.ClassCalled = fileName;
                            constructor.BaseCommandId = 6;
                            constructor.FileMaster = null;
                            constructor.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                            continue;
                        }
                        foreach (var files in allCheckFiles)
                        {
                            #region Call External To Update the Method Called

                            var lstmethodName = statementReferenceMasters.Where(
                                    x => x.ProjectId == projectId && x.FileId == files.FileId && x.BaseCommandId == 8)
                                .ToList();
                            if (!lstmethodName.Any()) continue;
                            foreach (StatementReferenceMaster t in lstmethodName)
                            {
                                if (!t.MethodName.EndsWith("()"))
                                {
                                    t.MethodName = t.MethodName + "()";
                                }
                                constructor.ClassCalled = fileName;
                                constructor.BaseCommandId = 6;
                                constructor.MethodCalled = t.MethodName.Replace(".", "");
                                constructor.FileMaster = null;
                                constructor.ReferenceFileMaster = null;
                                await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }

                            #endregion
                        }
                    }

                    #endregion

                    #region  region 4 ... Update the classCalled and methodCalled for DoCmd baseCommand Id = 5 and baseCommand Id =6

                    stringBuilder.AppendLine(
                        "Started update classcalled and methodcalled for basecommandId = 5 and 6 where DoCmd statement: (" +
                        projectId + ")");

                    var execNameClass = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                             &&
                                                                             x.OriginalStatement.StartsWith(
                                                                                 "DoCmd.OpenForm") ||
                                                                             x.OriginalStatement.StartsWith(
                                                                                 "DoCmd.OpenReport") ||
                                                                             x.OriginalStatement.StartsWith(
                                                                                 "DoCmd.OpenQuery")).ToList();

                    foreach (var constructor in execNameClass)
                    {
                        int fileId = constructor.FileId;
                        string currentStmt = constructor.OriginalStatement.Replace("\"", "'");
                        string fileName = string.Empty;
                        var regexNew = new Regex("\'(.*)'");
                        if (regexNew.IsMatch(currentStmt.ToUpper().TrimEnd()))
                        {
                            fileName = currentStmt.Between("\'", "\'");
                            if (fileName.Contains(","))
                            {
                                //fileName = fileName.Split(',')[0];
                                fileName = currentStmt.Between("\'", "\'");
                            }
                        }
                        if (string.IsNullOrEmpty(fileName))
                        {
                        }
                        var allCheckFiles = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(
                                f =>
                                    f.ProjectId == projectId && f.ClassNameDeclared == fileName.Trim() &&
                                    f.BaseCommandId == 19)
                            .ContinueWith(t =>
                            {
                                var result = t.Result;
                                return result.ToList();
                            });

                        if (!allCheckFiles.Any())
                        {
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
                                             && x.BaseCommandId == 8).ToList();

                                var methodNm =
                                    methodName.Where(x => x.MethodName.Contains("_Load") ||
                                                          x.MethodName.Contains("_load"))
                                        .ToList().FirstOrDefault();
                                // if (methodNm == null) continue;
                                if (methodNm != null)
                                {
                                    if (!methodNm.MethodName.EndsWith("()"))
                                    {
                                        methodNm.MethodName = methodNm.MethodName + "()";
                                    }
                                    constructor.MethodCalled = methodNm.MethodName.Replace(".", "");
                                    constructor.BaseCommandId = 5;
                                    constructor.FileMaster = null;
                                    constructor.ReferenceFileMaster = null;
                                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                    break;
                                }
                                foreach (var mName in methodName)
                                {
                                    if (!mName.MethodName.EndsWith("()"))
                                    {
                                        mName.MethodName = methodNm.MethodName + "()";
                                    }
                                    constructor.MethodCalled = mName.MethodName.Replace(".", "");
                                    constructor.BaseCommandId = 5;
                                    constructor.FileMaster = null;
                                    constructor.ReferenceFileMaster = null;
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                    break;
                                }

                                #endregion
                            }
                            else
                            {
                                #region Call External To Update the Method Called

                                var lstmethodName =
                                    statementReferenceMasters.Where(
                                            x => x.ProjectId == projectId && x.FileId == files.FileId &&
                                                 x.BaseCommandId == 8)
                                        .ToList();

                                if (!lstmethodName.Any()) continue;
                                foreach (StatementReferenceMaster t in lstmethodName)
                                {
                                    // if (lstmethodName.Count <= 1) continue;

                                    if (!t.MethodName.EndsWith("()"))
                                    {
                                        t.MethodName = t.MethodName + "()";
                                    }
                                    var methodName =
                                        lstmethodName.Where(
                                                x => x.MethodName.Contains("_Load") || x.MethodName.Contains("_load"))
                                            .ToList().FirstOrDefault();
                                    //if (methodName == null) continue;
                                    if (methodName != null)
                                    {
                                        if (!string.IsNullOrEmpty(methodName.MethodName))
                                        {
                                            if (!methodName.MethodName.EndsWith("()"))
                                            {
                                                methodName.MethodName = methodName.MethodName + "()";
                                            }
                                            constructor.MethodCalled = methodName.MethodName.Replace(".", "");
                                            constructor.BaseCommandId = 6;
                                            constructor.ClassCalled = fileName;
                                            constructor.FileMaster = null;
                                            constructor.ReferenceFileMaster = null;
                                            await
                                                _codeVortoService.StatementReferenceMasterRepository
                                                    .UpdateItem(constructor);
                                            break;
                                        }

                                        constructor.MethodCalled = t.MethodName.Replace(".", "");
                                        constructor.BaseCommandId = 6;
                                        constructor.ClassCalled = fileName;
                                        constructor.FileMaster = null;
                                        constructor.ReferenceFileMaster = null;
                                        await _codeVortoService.StatementReferenceMasterRepository
                                            .UpdateItem(constructor);
                                        break;
                                    }
                                    constructor.MethodCalled = t.MethodName.Replace(".", "");
                                    constructor.BaseCommandId = 6;
                                    constructor.ClassCalled = fileName;
                                    constructor.FileMaster = null;
                                    constructor.ReferenceFileMaster = null;
                                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                    break;
                                }

                                #endregion
                            }
                        }
                    }

                    #endregion

                    #region region5 update Method Called for Call baseCommandId = 5 And baseCommandId =6

                    stringBuilder.AppendLine(
                        "Started update classcalled and methodcalled for basecommandId = 5 and 6 where call statement: (" +
                        projectId + ")");

                    var execallNameNew = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.OriginalStatement.StartsWith("Call"))
                        .ToList();
                    var lstCallDataNew =
                        statementReferenceMasters.Where(x => x.ProjectId == projectId && x.BaseCommandId == 8).ToList();

                    foreach (var ccontructorCallNew in execallNameNew)
                    {
                        string[] currentStat = ccontructorCallNew.OriginalStatement.Split(' ');
                        var currentStatementCall = currentStat[1].Trim();
                        if (currentStatementCall.Contains("("))
                        {
                            currentStatementCall = currentStatementCall.Split('(').FirstOrDefault();
                        }
                        //List<StatementReferenceMaster> methodName;

                        if (ccontructorCallNew.OriginalStatement.Contains("RunReportAsPDF"))
                        {
                            string currentStmtNew = ccontructorCallNew.OriginalStatement.Replace("\"", "'");
                            var mainStatment = currentStmtNew.Between("\'", "\'");
                            var orignalStatment = mainStatment.Contains('(')
                                ? mainStatment.Split(',').FirstOrDefault()
                                : mainStatment.Split(' ').FirstOrDefault();
                            currentStatementCall =
                                orignalStatment.Remove(orignalStatment.Length - 1, 1).Replace('\'', ' ').Trim();
                        }
                        var methodName =
                            lstCallDataNew.Where(
                                    x => x.FileId == ccontructorCallNew.FileId &&
                                         x.MethodName == currentStatementCall + "()")
                                .ToList();

                        if (methodName.Count != 0)
                        {
                            ccontructorCallNew.MethodCalled = currentStatementCall + "()";
                            ccontructorCallNew.BaseCommandId = 5;
                            ccontructorCallNew.ReferenceFileMaster = null;
                            ccontructorCallNew.FileMaster = null;
                            await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(ccontructorCallNew);
                            continue;
                        }
                        var checkMethod = lstCallDataNew.Where(x => x.MethodName == currentStatementCall + "()")
                            .ToList();
                        if (!checkMethod.Any())
                        {
                            checkMethod = lstCallDataNew.Where(x => x.MethodName == currentStatementCall).ToList();
                            if (!checkMethod.Any())
                            {
                                continue;
                            }
                        }
                        foreach (var checkMt in checkMethod)
                        {
                            var classCalledName =
                                statementReferenceMasters.FirstOrDefault(
                                    x => x.ProjectId == projectId && x.FileId == checkMt.FileId &&
                                         x.BaseCommandId == 19);
                            if (!currentStatementCall.EndsWith("()"))
                            {
                                currentStatementCall = currentStatementCall + "()";
                            }
                            ccontructorCallNew.ClassCalled = classCalledName.ClassNameDeclared;
                            ccontructorCallNew.MethodCalled = currentStatementCall;
                            ccontructorCallNew.BaseCommandId = 6;
                            ccontructorCallNew.ReferenceFileMaster = null;
                            ccontructorCallNew.FileMaster = null;
                            await
                                _codeVortoService.StatementReferenceMasterRepository.UpdateItem(
                                    ccontructorCallNew);
                        }
                    }

                    #endregion


                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);

                }
                stringBuilder.AppendLine("Completed process: ProcessMsAccessVbaProject(" + projectId + ")");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Done");
                //stringBuilder.AppendLine("Started executing next process: ProcessAllActionWorkflows(" + projectId + ")");
                //LogMessage.WriteLogMessage(stringBuilder);
                //var processResult = await ProcessAllActionWorkflows(projectId);
                //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                //string data = await dataContent.Content.ReadAsStringAsync();
                //return Ok(data);

            }
        }

        #region Generic block code

        private List<StatementReferenceMaster> GetMethodBlock(int stmtId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("Called stored procedure: SpGetAnyGenericBlock(" + stmtId + ",8,9)");
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return workflowRef;
            }
        }

        private List<StatementReferenceMaster> GetGenericBlock(string className, int projectId)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("Called stored procedure: SpVBAGetClassMethods(" + className + "," + projectId + ")");
            object[] parametersExp =
            {
                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className},
                new MySqlParameter("@projId", MySqlDbType.VarChar) {Value = projectId}
            };
            var callExtExpandedCode = _codeVortoService.StatementReferenceMasterRepository
                .ExecuteStoreProcedure<StatementReferenceMaster>("SpVBAGetClassMethods", parametersExp)
                .ContinueWith(t => t.Result).Result;
            LogMessage.WriteLogMessage(stringBuilder);
            return callExtExpandedCode;
        }

        private List<StatementReferenceMaster> GetGenericBlock(int stmtId, int startBaseCommandId, int endBaseCommandId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("Called stored procedure: SpGetAnyGenericBlock(" + stmtId + "," + startBaseCommandId + "," + endBaseCommandId + ")");
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = startBaseCommandId},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = endBaseCommandId}
                };
                var workflowRef = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return workflowRef;
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

        private static IEnumerable<StatementReferenceMaster> GetAllMethodsForClass(string className, int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("Called stored procedure: SpGetAllMethodsForClass(," + className + "," + projectId + ")");
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar) {Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className},
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                };
                var callExtExpandedCode = codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodsForClass", parametersExp)
                    .ContinueWith(t => t.Result).Result;
                LogMessage.WriteLogMessage(stringBuilder);
                return callExtExpandedCode;
            }
        }

        #endregion
    }
}