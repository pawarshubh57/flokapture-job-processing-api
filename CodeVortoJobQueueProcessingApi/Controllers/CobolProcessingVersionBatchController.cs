using BusinessLayer.CobolVersion;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Formatting;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingVersionBatchController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public CobolProcessingVersionBatchController()
        {

        }

        public CobolProcessingVersionBatchController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ExecuteProcessActionsOneByOne(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();

                try
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectMaster == null) return BadRequest("Details provided are not correct.");

                    // Update Project Processing Statud to 3, so that it can be shown on load projects page...
                    projectMaster.Processed = 3;
                    projectMaster.LanguageMaster = null;
                    projectMaster.ProjectType = null;

                    var updatedProjectMaster = await _codeVortoService.ProjectMasterRepository.UpdateItem(projectMaster);
                    Console.WriteLine(JsonConvert.SerializeObject(updatedProjectMaster));

                    var startDate = DateTime.Now;

                    string startStatus;
                    string endStatus;

                    string dashLine = "=========================================================================\n";

                    var processStepList = await _codeVortoService.ProjectProcessingStepRepository
                        .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId)
                        .ConfigureAwait(false);



                    // ChangeFileExtensions

                    var projectProcessStep = processStepList.Find(x => x.ProcessStep == "ChangeFileExtensions");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ChangeFileExtensions\nDate: " +
                                             DateTime.Now.ToString("g") + "\nTime: " +
                                             DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        var iHttpActionResult = ChangeFileExtensions(projectMaster);

                        var responseMessage = await iHttpActionResult.ExecuteAsync(CancellationToken.None)
                            .ConfigureAwait(false);
                        if (responseMessage.StatusCode == HttpStatusCode.InternalServerError)
                            return Ok(await responseMessage.Content.ReadAsStringAsync());

                        endStatus = "Completed executing Method: ChangeFileExtensions\nDate: " +
                                           DateTime.Now.ToString("g") + "\nTime: " +
                                           DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }


                    // StartCobolProcessing
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "StartCobolProcessing");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: StartCobolProcessing\nDate: " +
                                            DateTime.Now.ToString("g") + "\nTime: " +
                                            DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine("Process Started on: " + startDate);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await StartCobolProcessing(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: StartCobolProcessing\nDate: " +
                                          DateTime.Now.ToString("g") + "\nTime: " +
                                          DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // StartParseProcessingCobol

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "StartParseProcessingCobol");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: StartParseProcessingCobol\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await StartParseProcessingCobol(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: StartCobolProcessing\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // Process All ActionWorkFlows for JCL
                    //   await ProcessAllActioWorkflows(projectId).ConfigureAwait(false);

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessAllActioWorkflows");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessAllActioWorkflows\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessAllActioWorkflows(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessAllActioWorkflows\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ViewSource
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ViewSource");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ViewSource\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ViewSource(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ViewSource\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }
                    //

                    // UpdateBaseCommandId6ForInputLibAndRunProgramStatement
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateBaseCommandId6ForInputLibAndRunProgramStatement");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: UpdateBaseCommandId6ForInputLibAndRunProgramStatement\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await UpdateBaseCommandId6ForInputLibAndRunProgramStatement(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: UpdateBaseCommandId6ForInputLibAndRunProgramStatement\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }


                    // await UpdateBaseCommandId6ForInputLibAndRunProgramStatement(projectMaster.ProjectId).ConfigureAwait(false);


                    // UpdateCallExternalForIncludeMemberStmt
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateCallExternalForIncludeMemberStmt");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: UpdateCallExternalForIncludeMemberStmt\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await UpdateCallExternalForIncludeMemberStmt(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: UpdateCallExternalForIncludeMemberStmt\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // await UpdateCallExternalForIncludeMemberStmt(projectMaster.ProjectId).ConfigureAwait(false);

                    //ProcessCobolProject
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessCobolProject");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessCobolProject\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessCobolProject(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessCobolProject\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    /*  await PseudoCodeConversion(projectId).ConfigureAwait(false); */

                    // PseudoCodeConversion
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "PseudoCodeConversion");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: PseudoCodeConversion\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await PseudoCodeConversion(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: PseudoCodeConversion\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }


                    // GetAllStartingPoints

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "GetAllStartingPoints");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: GetAllStartingPoints\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await GetAllStartingPoints(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: GetAllStartingPoints\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    //ProcessActionWorkflowDetails
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessActionWorkflowDetails");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessActionWorkflowDetails\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessActionWorkflowDetails(projectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessActionWorkflowDetails\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // 
                    // ProcessProgramWorkflows
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessProgramWorkflows");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessProgramWorkflows\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessProgramWorkflows(projectId).ConfigureAwait(false);
                        endStatus = "Completed executing Method: ProcessProgramWorkflows\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }


                    /* await ProcessProgramWorkflows(projectId).ConfigureAwait(false); */


                    // ProcessForDataDependency
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForDataDependency");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessForDataDependency\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessForDataDependency(projectId).ConfigureAwait(false);
                        endStatus = "Completed executing Method: ProcessForDataDependency\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }


                    /* await ProcessForDataDependency(projectId).ConfigureAwait(false); */

                    //

                    /* await ProcessProjectInventory(projectId).ConfigureAwait(false);*/

                    // ProcessProjectInventory
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessProjectInventory");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessProjectInventory\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessProjectInventory(projectId).ConfigureAwait(false);
                        endStatus = "Completed executing Method: ProcessProjectInventory\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    /* await ProcessForCrudActivity(projectId, "opt").ConfigureAwait(false);*/

                    // ProcessForCrudActivity
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForCrudActivity");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessForCrudActivity\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessForCrudActivity(projectId, "opt").ConfigureAwait(false);
                        endStatus = "Completed executing Method: ProcessForCrudActivity\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);
                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    //

                    /* await ProcessForDataInventory(projectMaster.ProjectId).ConfigureAwait(false); */

                    // ProcessForDataInventory
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForDataInventory");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        startStatus = "Started executing Method: ProcessForDataInventory\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);

                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await ProcessForDataInventory(projectMaster.ProjectId).ConfigureAwait(false);
                        endStatus = "Completed executing Method: ProcessForDataInventory\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(endStatus);
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

                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception.InnerException);
                }
            }
        }

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
                    await _codeVortoService.FileTypeExtensionRepository.GetAllListItems(
                        p => p.LanguageId == projectDetails.LanguageId).ConfigureAwait(true);
                var strExtensions = new List<string>();
                strExtensions.AddRange(entensionList.Select(extension => extension.FileExtension));
                var lstFileMasters = new List<FileMaster>();
                var projectPath = projectDetails.PhysicalPath;
                var solutionId = projectDetails.SolutionId;
                var directoryList = new List<string> { projectPath };
                var ignoredFile = await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId)
                    .ConfigureAwait(false);
                var regexPattern = new Regex(@"RECEIVE\s+MAP|SEND\s+MAP");
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
                            var lineCount = fileLines.Count(line => !string.IsNullOrWhiteSpace(line));
                            var exist = fileLines.Any(f => regexPattern.IsMatch(f));
                            var fileType = exist ? "Online" : "Batch";
                            var fileMaster = new FileMaster
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = currentFile,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                DoneParsing = 0,
                                LinesCount = lineCount,
                                FileType = fileType
                            };
                            lstFileMasters.Add(fileMaster);
                        }
                        enumerator.Dispose();
                        await _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
                        //  await StartParseProcessingCobol(projectId);
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
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectMaster == null) return Ok("Project not found");

                    var copyOfFileMaster = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId
                         && x.Processed == 0).ConfigureAwait(false);

                    foreach (var fileMaster in copyOfFileMaster)
                    {
                        var startStatus = "Started executin File: " + fileMaster.FileName + " \nDate: " +
                                          DateTime.Now.ToString("g") + "\nTime: " +
                                          DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        Console.WriteLine("-----------------------------------------------");

                        if (fileMaster.FileTypeExtensionId == 6)
                        {
                            // todo for new changes after removing comment
                           await ParseProcessCobolFile(fileMaster).ConfigureAwait(false);
                        }

                        if (fileMaster.FileTypeExtensionId == 7)
                        {
                            await ParseProcessJclFile(fileMaster).ConfigureAwait(false);
                        }

                        if (fileMaster.FileTypeExtensionId == 8)
                        {
                            await ParseProcessProcFile(fileMaster).ConfigureAwait(false);
                        }
                        if (fileMaster.FileTypeExtensionId == 19)
                        {
                            await ParseInputLibFile(fileMaster).ConfigureAwait(false);
                        }
                        if (fileMaster.FileTypeExtensionId == 20)
                        {
                            await ParseBmsFile(fileMaster).ConfigureAwait(false);
                        }

                        fileMaster.Processed = 1;
                        fileMaster.ProjectMaster = null;
                        fileMaster.FileTypeExtensionReference = null;
                        fileMaster.DoneParsing = 1;

                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);
                        var endStatus = "Ended executin File: " + fileMaster.FileName + " \nDate: " +
                                          DateTime.Now.ToString("g") + "\nTime: " +
                                          DateTime.Now.ToString("HH:m:s tt zzz");

                        Console.WriteLine(endStatus);

                        Console.WriteLine("=====================================================");
                    }

                    return Ok("All files procced sucessfully.");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception.InnerException);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProcessCobolFile(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileId = 169;
                // var fileMaster = _codeVortoService.FileMasterRepository.GetItem(fileId);
                if (!File.Exists(fileMaster.FilePath)) return NotFound();

                int languageId = fileMaster.ProjectMaster.LanguageId;
                int projectId = fileMaster.ProjectId;

                var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                if (allLines.Count <= 0) return Ok();

                allLines = allLines.PrepareSameLength();
                allLines = allLines.RemoveCharacter(6, 66);
                string[] comments = { "*", "/" };
                allLines = allLines.RemoveAllCommentedLines(comments);
                allLines = allLines.RemoveAll("SKIP", "EJECT");
                allLines = allLines.RemoveEmptyLines();

                var cobolKeyWordList = await _codeVortoService.LanguageKeywordRepository
                    .GetAllListItems(x => x.languageId == fileMaster.ProjectMaster.LanguageId).ConfigureAwait(false);
                var cobolKeyWords = cobolKeyWordList.Select(keyWord => keyWord.KeywordName).ToList();

                var cobolSection = await _codeVortoService.CobolSectionRepository.GetAllListItems(x => x.SectionId != 0)
                    .ConfigureAwait(false);
                var cobolSectionList = cobolSection.Select(x => x.SectionName).ToList();

                #region Dump data into ViewSourceMaster Table

                var regEx = new Regex(@"^\*", RegexOptions.IgnoreCase);
                var sourceData = File.ReadAllLines(fileMaster.FilePath).ToList();
                var strBuilder = new StringBuilder();
                foreach (var stmt in sourceData)
                {
                    strBuilder.AppendLine(stmt);
                }
                var allStatement = new List<string>();

                foreach (var line in allLines)
                {
                    var currentLine = line.Trim();
                    if (string.IsNullOrEmpty(currentLine)) continue;
                    var exist = cobolSectionList.Exists(x => x.StartsWith(currentLine));
                    if (!exist) continue;
                    allStatement.Add(currentLine);
                    var sectionList = allLines.GetStatementBetweenSection(cobolSectionList, currentLine);
                    if (sectionList.Count <= 0) continue;
                    var regexProcedure = new Regex(@"(.*PROCEDURE DIVISION.*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                    var regexWorking = new Regex(@"(.*WORKING-STORAGE SECTION.*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                    var regexIdentification = new Regex(@"(.*IDENTIFICATION DIVISION.*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                    // var regexPattern = new Regex(@"EXEC\s+SQL|EXEC\s+CICS");
                    if (regexProcedure.IsMatch(currentLine) | regexWorking.IsMatch(currentLine) | regexIdentification.IsMatch(currentLine))
                    {
                        // var newListOfStatement = sectionList.GetAllExecStatementUptoEndExec();
                        allStatement.AddRange(sectionList);
                        continue;
                    }
                    sectionList = sectionList.CombineAllLinesInWorkingStorageSection();
                    allStatement.AddRange(sectionList);
                }
                allStatement.RemoveAll(r => r.Length <= 0);
                allStatement = allStatement.Where(s => !regEx.IsMatch(s)).ToList();
                allStatement = allStatement.Select(s => s.Trim()).ToList();
                allStatement = allStatement.Select(s => s.CheckCommentInStatement()).ToList();
                string sourceWithoutComment = string.Join("\n", allStatement);
                var viewSourceMaster = new ViewSourceMaster
                {
                    ViewSourceId = 0,
                    FileId = fileMaster.FileId,
                    SourceData = strBuilder.ToString(),
                    ProjectId = fileMaster.ProjectId,
                    SourceWithoutComments = sourceWithoutComment
                };
                await _codeVortoService.ViewSourceMasterRepository.AddNewItem(viewSourceMaster).ConfigureAwait(false);

                #endregion

                var clsCobolProcess = new ClsCobolProcess();

                #region --- File Section -- 

                var allLinesOfFileSection = allLines.GetStatementBetweenSection(cobolSectionList, "FILE SECTION.");
                // todo remove commet after testing
                // await InsertFileSection(projectId, languageId, allLinesOfFileSection, fileMaster);

                #endregion

                #region --- WORKING-STORAGE SECTION ---

                var allLinesOfWorkingStorageSection = allLines.GetWorkingStorageSection(cobolSectionList, "WORKING-STORAGE SECTION.");
                // get statement between WORKING-STORAGE SECTION section 
                allLinesOfWorkingStorageSection = allLinesOfWorkingStorageSection.CombineAllLinesInWorkingStorageSection();
                await InsertWorkingStorageSection(projectId, languageId, allLinesOfWorkingStorageSection, fileMaster);

                var copyCallStatement = allLinesOfWorkingStorageSection.GetCopyStatementInWorkingStorageSection();
                copyCallStatement = copyCallStatement.RemoveDot();
                Console.WriteLine(copyCallStatement);

                #endregion

                #region Get all primary command and baseCommand information to parse file

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllListItems().ConfigureAwait(false);
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

                #region Add Default Entry for BaseCommandId =19

                var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                    callClassIndicatorStart[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                #endregion

                #region ---  PROCEDURE DIVISION section

                var allLinesOfProcedureDivision = allLines.GetProcedureDivisionSection();



                /*Test For EXCS statement*/
                // allLinesOfProcedureDivision = allLinesOfProcedureDivision.RemoveSpacesBetweenWords();
                // allLinesOfProcedureDivision = allLinesOfProcedureDivision.ConversionOfExecStatements();
                // allLinesOfProcedureDivision = allLinesOfProcedureDivision.CombineAllExecSqlStatements();


                var allMehthods = new List<string>();
                //  allMehthods.Add("WORKING-STORAGE SECTION");

                var methodsIntoProcedureDivision = allLinesOfProcedureDivision.GetAllCobolMethodsIntoProcedureDivision("");
                allLinesOfProcedureDivision = allLinesOfProcedureDivision
                    .ModifyMethodsNameAfterDot(methodsIntoProcedureDivision);
                methodsIntoProcedureDivision = methodsIntoProcedureDivision.RemoveDot();
                allMehthods.AddRange(methodsIntoProcedureDivision);
                var dictionary = new Dictionary<string, List<string>> { { "WORKING-STORAGE SECTION", copyCallStatement } };
                var methodStatements = allLinesOfProcedureDivision.CollectAllMethodsStatements(methodsIntoProcedureDivision, dictionary);


                #endregion

                #region --- Conversion Methodwise

                var programFileLines = new List<string>();
                var cobolVariableList = await _codeVortoService.CobolVariableRepository.GetAllListItems(
                    x => x.ProjectId == projectId && x.FileId == fileMaster.FileId).ConfigureAwait(false);
                foreach (var statement in methodStatements)
                {
                    var mainKey = statement.Key; // methodName
                    if (string.IsNullOrWhiteSpace(mainKey)) continue;
                    var mainLines = statement.Value; // mathodStatements
                    if (mainLines.Count <= 0)
                    {
                        programFileLines.Add(mainKey);
                        programFileLines.Add("END");
                        continue;
                    }
                    programFileLines.Add(mainKey);
                    mainLines = mainLines.ReplaceCurrentStatement("GO TO ", "PERFORM ");
                    mainLines = mainLines.CombineAllLines(methodsIntoProcedureDivision, cobolKeyWords);
                    mainLines = mainLines.CombineLineForMoveToStatement(cobolKeyWords);
                    mainLines = mainLines.ConvertAllMoveStatement(cobolKeyWords);
                    mainLines = mainLines.AddNewLineForMultipleKeyoword(cobolKeyWords);
                    mainLines = mainLines.AddEndIfStatement();
                    mainLines = mainLines.ConversionOfEvaluateStatement();
                    mainLines = mainLines.PerfromVarying(methodsIntoProcedureDivision, cobolVariableList);
                    // mainLines.VerifyMethodBlockForIfWithMatchingEndIfCount();
                    mainLines = mainLines.RemoveSpacesBetweenWords();
                    mainLines = mainLines.CombineAllExecSqlStatements();
                    mainLines = mainLines.ConversionOfExecStatements();
                    var lastItemOfList = mainLines.LastOrDefault();
                    if (mainKey != "PROCEDURE DIVISION")
                    {
                        if (!mainLines.Any(x => x == "STOP RUN" || x == "STOP RUN."))
                        {
                            if (lastItemOfList == ".")
                            {
                                mainLines.Remove(mainLines.Last());
                                mainLines.Add("END");
                            }
                            else
                                mainLines.Add("END");
                        }
                    }
                    programFileLines.AddRange(mainLines);
                }

                #endregion

                string methodBusinessName = string.Empty;

                #region Dump statement into StatementReferenceMaster table
                /*
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
                */

                // if (programFileLines.Any()) return Ok(programFileLines);
                // int lineCnt = -1;
                programFileLines = programFileLines.Select(s => s.Trim()).ToList();

                foreach (var line in programFileLines)
                {
                    // lineCnt++;
                    if (string.IsNullOrWhiteSpace(line)) continue;
                    if (allMehthods.Any(a => line.StartsWith(a)))
                    {
                        StatementReferenceMaster stmtReferenceMaster;
                        var regexCopy = new Regex(@"^COPY\s*(\w.*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                        if (regexCopy.IsMatch(line))
                        {
                            var copyMatch = regexCopy.Match(line);
                            var pgmName = copyMatch.Groups[1].Value;
                            if (string.IsNullOrEmpty(pgmName)) return null;
                            var objName = pgmName.Trim();
                            stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = objName,
                                OriginalStatement = objName,
                                ClassCalled = null,
                                MethodName = objName + "()",
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
                            stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = line,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 6,
                                PrimaryCommandId = 69,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = fileMaster.SolutionId ?? 0
                            };
                            await
                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            continue;

                        }
                        stmtReferenceMaster = new StatementReferenceMaster
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
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        string methodCalled = line.Split(' ').LastOrDefault() + "()";
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = methodCalled,
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
                    if (line.StartsWith("EXEC CICS "))
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
                            BaseCommandId = 6,
                            PrimaryCommandId = 0,
                            ProjectId = projectId,
                            BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (callExternal.Any(e => line.StartsWith(e.StartIndicator)))
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
                            BaseCommandId = callExternal.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                            PrimaryCommandId =
                                callExternal.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                            ProjectId = projectId,
                            BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        // TODO: Need to check for more than one STOP RUN || STOP RUN.
                        // if (lineCnt < lastIndexOfStop) continue;
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            //BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                }

                #endregion

                #region Add Default entry for basecommandId = 20

                var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                    callClassIndicatorEnd[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

                #endregion

                return Ok(programFileLines);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProcessJclFile(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var fileMaster = _codeVortoService.FileMasterRepository.GetItem(fileId);
                if (!File.Exists(fileMaster.FilePath)) return NotFound();
                int languageId = fileMaster.ProjectMaster.LanguageId;
                int projectId = fileMaster.ProjectId;
                var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                if (!allLines.Any()) return NotFound();
                allLines = allLines.PrepareSameLength();
                allLines = allLines.RemoveStartCharacter("/");
                string[] comments = { "*" };
                allLines = allLines.RemoveAllCommentedLines(comments);
                allLines = allLines.RemoveEmptyLines();
                allLines = allLines.CombineAllBrokenLines(',', false);
                allLines = allLines.CombineAllBrokenLines('-', false);
                allLines = allLines.RemoveInlineComment("(.*?)=+>", "(.*?)\\*", "(.*?)<+=");
                allLines = allLines.StatementTrim();
                allLines = allLines.RemoveSpacesBetweenWords();
                // var methodsList = allLines.CreateMethodForExec(fileName);

                /* This is the logic of multiple method for EXEC statements 
                    var methodsList = allLines.CreateMethodForExec(fileName);
                */
                string methodBusinessName = string.Empty;

                #region Get all primary command and baseCommand information to parse file

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllListItems().ConfigureAwait(false);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var ifConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                var endIfConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                // var methodStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callExternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
                var callInternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                // var methodStart = methodStartIndicator.Find(x => true);
                var ifStart = ifConditionIndicator.Find(x => true);
                var endIfStart = endIfConditionIndicator.Find(x => true);
                var callExternal = callExternalIndicator.FindAll(x => true);
                var callInternal = callInternalIndicator.Find(x => true);
                var loopStart = loopIndicatorStart.Find(x => true);
                var loopEnd = loopIndicatorEnd.Find(x => true);

                #endregion

                #region Add Default Entry for BaseCommandId =19

                var clsCobolProcess = new ClsCobolProcess();
                var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                    callClassIndicatorStart[0].PrimaryReferenceId);
                string methodName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                var baseCmd8 = clsCobolProcess.PrepareStatementReferenceMasterMethodStart(fileMaster, methodName);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(baseCmd8);

                #endregion

                #region for multiple exec statement methods.

                /*
                foreach (var method in methodsList)
                {
                    var mainKey = method.Key;
                    mainKey = mainKey.Trim();
                    if (mainKey != fileName)
                    {
                        var statmentRefStartMethod = clsCobolProcess
                            .PrepareStatementReferenceMasterMethodStart(fileMaster, mainKey);
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statmentRefStartMethod);
                    }
                    var valueStatements = method.Value;
                    foreach (var statement in valueStatements)
                    {
                        #region for loop
                        var line = statement.Trim();
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
                            string methodCalled = line.Split(' ').LastOrDefault() + "()";
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                DataOrObjectType = null,
                                MethodCalled = methodCalled,
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
                        #endregion
                    }
                    var checkEnd = valueStatements.LastOrDefault(x => x == "END");
                    if (checkEnd != null && (!string.IsNullOrWhiteSpace(checkEnd) || !string.IsNullOrEmpty(checkEnd))) continue;
    
                    var statmentRefStarEnd = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                    await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statmentRefStarEnd);
                }
                */

                #endregion

                #region Insert statement into statementmaster table

                foreach (var cline in allLines)
                {
                    #region for loop

                    var line = cline.Trim();
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (callExternal.Any(l => line.Contains(l.StartIndicator)))
                    {
                        StatementReferenceMaster stmtReferenceMaster;
                        if (line.Contains("COPY"))
                        {
                            var regexCopy = new Regex(@"^COPY\s*(\w.*)",
                                RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
                            if (!regexCopy.IsMatch(line))
                            {
                                stmtReferenceMaster = new StatementReferenceMaster
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
                                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                continue;
                            }
                        }
                        stmtReferenceMaster = new StatementReferenceMaster
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
                            PrimaryCommandId = callExternal.Find(s => line.Contains(s.StartIndicator))
                                  .PrimaryReferenceId,
                            ProjectId = projectId,
                            BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }

                    #endregion

                    // This is login of multiple methods
                    /*
                    var checkEnd = allLines.LastOrDefault(x => x.StartsWith("END") || x.StartsWith(" END"));
                    if (string.IsNullOrWhiteSpace(checkEnd) || !string.IsNullOrEmpty(checkEnd))
                    {
                        var statmentRefStarEnd = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statmentRefStarEnd);
                    }
                    */
                }

                #endregion

                #region Add Default entry for basecommandId = 20

                var methodEnd = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(methodEnd);

                var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                    callClassIndicatorEnd[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

                #endregion

                // await ProcessAllActioWorkflows(projectId, fileMaster);
                return Ok(allLines);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProcessProcFile(FileMaster fileMaster)
        {
            if (!File.Exists(fileMaster.FilePath)) return NotFound();
            int languageId = fileMaster.ProjectMaster.LanguageId;
            int projectId = fileMaster.ProjectId;
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
            if (!allLines.Any()) return NotFound();
            allLines = allLines.PrepareSameLength();
            allLines = allLines.RemoveStartCharacter("/");
            string[] comments = { "*" };
            allLines = allLines.RemoveAllCommentedLines(comments);
            allLines = allLines.RemoveEmptyLines();
            allLines = allLines.CombineAllBrokenLines(',', false);
            allLines = allLines.RemoveInlineComment("(.*?)=+>", "(.*?)\\*", "(.*?)<+=");
            allLines = allLines.StatementTrim();
            var methodsList = allLines.CreateMethodForExec(fileName);

            string methodBusinessName = string.Empty;

            #region Get all primary command and baseCommand information to parse file

            var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
            var baseCommandReference = await generalRepository.GetAllListItems().ConfigureAwait(false);
            var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
            var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var ifConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
            var endIfConditionIndicator = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
            //var methodStartIndicator = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
            //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var callExternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId).ToList();
            var callInternalIndicator = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
            var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

            // var methodStart = methodStartIndicator.Find(x => true);
            var ifStart = ifConditionIndicator.Find(x => true);
            var endIfStart = endIfConditionIndicator.Find(x => true);
            var callExternal = callExternalIndicator.FindAll(x => true);
            var callInternal = callInternalIndicator.Find(x => true);
            var loopStart = loopIndicatorStart.Find(x => true);
            var loopEnd = loopIndicatorEnd.Find(x => true);

            #endregion

            #region Add Default Entry for BaseCommandId = 19

            var clsCobolProcess = new ClsCobolProcess();
            var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                callClassIndicatorStart[0].PrimaryReferenceId);
            await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

            #endregion

            #region Insert statement into statementmaster table

            foreach (var method in methodsList)
            {
                var mainKey = method.Key.Trim();
                var statmentRefStartMethod = clsCobolProcess.PrepareStatementReferenceMasterMethodStart(fileMaster, mainKey);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statmentRefStartMethod);
                var valueStatements = method.Value;

                valueStatements = valueStatements.RemoveSpacesBetweenWords();
                foreach (var statement in valueStatements)
                {
                    #region for loop
                    var line = statement.Trim();
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
                        string methodCalled = line.Split(' ').LastOrDefault() + "()";
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = methodCalled,
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
                    #endregion
                }

                var statmentRefStarEnd = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statmentRefStarEnd);
            }

            #endregion

            #region Add Default entry for basecommandId = 20

            var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                callClassIndicatorEnd[0].PrimaryReferenceId);
            await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);

            #endregion

            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseInputLibFile(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (!File.Exists(fileMaster.FilePath)) return NotFound();

                int languageId = fileMaster.ProjectMaster.LanguageId;
                int projectId = fileMaster.ProjectId;

                var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                if (!allLines.Any()) return NotFound();
                allLines = allLines.PrepareSameLength();
                string[] comments = { "*" };
                allLines = allLines.RemoveAllCommentedLines(comments);
                allLines = allLines.Select(e => e.Trim()).ToList();
                allLines = allLines.RemoveAllCommentedLines(comments);
                allLines = allLines.CombineAllBrokenLines('-');
                allLines = allLines.RemoveSpacesBetweenWords();
                allLines = allLines.RemoveEmptyLines();
                var clsCobolProcess = new ClsCobolProcess();

                #region Get all primary command and baseCommand information to parse file

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllListItems().ConfigureAwait(false);
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

                #region Add Default Entry for BaseCommandId =19

                var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                    callClassIndicatorStart[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);
                var statementRefMethod =
                    clsCobolProcess.PrepareStatementReferenceMasterMethodStart(fileMaster, fileName);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementRefMethod)
                    .ConfigureAwait(false);


                #endregion

                string methodBusinessName = string.Empty;

                #region Dump statement into StatementReferenceMaster table

                foreach (var line in allLines)
                {
                    // lineCnt++;
                    if (string.IsNullOrWhiteSpace(line)) continue;
                    if (methodStart.StartIndicator.StartsWith(line))
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        string methodCalled = line.Split(' ').LastOrDefault() + "()";
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = methodCalled,
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
                    if (callExternal.Any(s => line.StartsWith(s.StartIndicator)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = "",
                            VariableNameDeclared = null,
                            BaseCommandId = callExternal.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                            PrimaryCommandId = callExternal.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
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
                        // TODO: Need to check for more than one STOP RUN || STOP RUN.
                        // if (lineCnt < lastIndexOfStop) continue;
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            //BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                }

                #endregion

                #region Add Default entry for basecommandId = 20

                var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                    callClassIndicatorEnd[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);
                var statementRefEndMethod = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementRefEndMethod)
                    .ConfigureAwait(false);
                #endregion

                return Ok(allLines);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseBmsFile(FileMaster fileMaster) // 
        {
            using (_codeVortoService = new CodeVortoService())
            {
                //  var fileMaster = _codeVortoService.FileMasterRepository.GetItem(fileId);
                if (!File.Exists(fileMaster.FilePath)) return NotFound();
                int languageId = fileMaster.ProjectMaster.LanguageId;
                int projectId = fileMaster.ProjectId;
                var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                if (!allLines.Any()) return NotFound();
                allLines = allLines.PrepareSameLength();
                allLines = allLines.RemoveCharacter(0, 66);
                allLines = allLines.RemoveStartCharacter("/");
                string[] comments = { "*" };
                allLines = allLines.RemoveAllCommentedLines(comments);
                allLines = allLines.RemoveEmptyLines();
                allLines = allLines.CombineAllBrokenLines(',', false);
                allLines = allLines.RemoveInlineComment("(.*?)=+>", "(.*?)\\*", "(.*?)<+=");
                allLines = allLines.StatementTrim();
                var clsCobolProcess = new ClsCobolProcess();

                #region Get all primary command and baseCommand information to parse file

                var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await generalRepository.GetAllListItems().ConfigureAwait(false);
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

                #region Add Default Entry for BaseCommandId =19

                var statmentRefStart = clsCobolProcess.PrepareStatementReferenceMasterStart(fileMaster,
                    callClassIndicatorStart[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);
                var statementRefMethod =
                    clsCobolProcess.PrepareStatementReferenceMasterMethodStart(fileMaster, fileName);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementRefMethod)
                    .ConfigureAwait(false);


                #endregion

                string methodBusinessName = string.Empty;

                #region Dump statement into StatementReferenceMaster table

                foreach (var line in allLines)
                {
                    // lineCnt++;
                    if (string.IsNullOrWhiteSpace(line)) continue;
                    if (methodStart.StartIndicator.StartsWith(line))
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        string methodCalled = line.Split(' ').LastOrDefault() + "()";
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = methodCalled,
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
                    if (callExternal.Any(s => line.StartsWith(s.StartIndicator)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileMaster.FileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = "",
                            VariableNameDeclared = null,
                            BaseCommandId = callExternal.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                            PrimaryCommandId = callExternal.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
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
                        // TODO: Need to check for more than one STOP RUN || STOP RUN.
                        // if (lineCnt < lastIndexOfStop) continue;
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    /*
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
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                    */
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
                            //BusinessName = methodBusinessName,
                            SolutionId = fileMaster.SolutionId ?? 0
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                }

                #endregion

                #region Add Default entry for basecommandId = 20


                var statementRefEndMethod = clsCobolProcess.PrepareStatementReferenceMasterMethodEnd(fileMaster);
                await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementRefEndMethod)
                    .ConfigureAwait(false);

                var statmentRefEndClass = clsCobolProcess.PrepareStatementReferenceMasterEnd(fileMaster,
                    callClassIndicatorEnd[0].PrimaryReferenceId);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEndClass);
                #endregion

                return Ok(allLines);
            }
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

                    var copyOfFileMaster = await _codeVortoService.FileMasterRepository.
                        GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);

                    // TODO: This is not in use now, comment this region later...
                    #region Update ClassNameDeclared

                    /*
                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId && x.BaseCommandId == 19)
                        .ConfigureAwait(false);

                    var projectName = projectDetails.ProjectName;
                    var physicalPath = projectDetails.PhysicalPath;

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
                        statement.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statement);
                    }
                    */

                    #endregion

                    #region Update MethodCalled & ClassCalled basecommandId = 6

                    var statementReferMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId && x.BaseCommandId == 6 && x.ReferenceFileId == 0).ConfigureAwait(false);

                    foreach (var statement in statementReferMaster)
                    {
                        var originalStatement = statement.OriginalStatement;
                        var fileMaster = statement.FileMaster;
                        var programFileMaster = clsCobolProcess.GetExternalCallFiles(originalStatement,
                            fileMaster.FileTypeExtensionId, copyOfFileMaster, out NameValue nameValue);

                        if (programFileMaster == null)
                        {
                            // This means this object is missing and needs to add 
                            // entry in missing object reference table...
                            var missingObject = new MissingObjects
                            {
                                ProjectId = statement.ProjectId,
                                FileId = fileMaster.FileId,
                                FromObject = fileMaster.FileName,
                                Statement = statement.OriginalStatement,
                                Type = nameValue.CalledObjectType, // TODO: This needs to be specific...
                                CalledObjectName = nameValue.CalledObjectName
                            };
                            var missingObjectExist = await _codeVortoService.MissingObjectsRepository
                                .GetAllListItems(x => x.ProjectId == projectId &&
                                                      x.FileId == missingObject.FileId &&
                                                      x.FromObject == missingObject.FromObject &&
                                                      x.CalledObjectName == missingObject.CalledObjectName)
                                .ConfigureAwait(false);
                            if (missingObjectExist.Any()) continue;
                            await _codeVortoService.MissingObjectsRepository.AddNewItem(missingObject)
                                .ConfigureAwait(false);
                            continue;
                        }

                        var projectDetails1 = _codeVortoService.ProjectMasterRepository.GetItem(programFileMaster.ProjectId);
                        var pNameNew = projectDetails1.ProjectName;
                        var pPathNew = projectDetails1.PhysicalPath;
                        var className = programFileMaster.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(programFileMaster.FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(programFileMaster.FilePath);

                        var classNameDeclared = pNameNew + "." + className + fileName;
                        statement.ClassCalled = classNameDeclared;
                        statement.MethodCalled = fileName + "()";
                        statement.ReferenceFileId = programFileMaster.FileId;
                        statement.ReferenceFileMaster = null;
                        statement.FileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(statement).ConfigureAwait(false);
                    }

                    #endregion

                    #region Update MethodCalled for BasecommandId = 5

                    var statementmethodCalled = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.ProjectId == projectId && x.BaseCommandId == 5
                                              && string.IsNullOrEmpty(x.MethodCalled)).ConfigureAwait(false);

                    foreach (var stateMathed in statementmethodCalled)
                    {

                        var originalStatememt = stateMathed.OriginalStatement;
                        string methodCalled = originalStatememt.Split(' ').LastOrDefault() + "()";
                        var mCalled = !string.IsNullOrEmpty(methodCalled) && methodCalled.Contains('.') ? methodCalled.Replace(".", "") : methodCalled;
                        stateMathed.MethodCalled = mCalled;
                        stateMathed.FileMaster = null;
                        stateMathed.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateMathed);
                    }
                    #endregion

                    return Ok("Done");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception.InnerException);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertWorkingStorageSection(int projectId, int languageId,
            List<string> tempBlock, FileMaster fileMaster)
        {
            var lstStatement = new List<CobolVariable>();
            using (_codeVortoService = new CodeVortoService())
            {
                int sectionId = 0;
                string sectionName = string.Empty;
                foreach (var line in tempBlock)
                {
                    var currentLine = line.TrimStart();
                    string level = currentLine.SplitLevel();
                    if (string.IsNullOrEmpty(level)) continue;
                    currentLine = currentLine.Substring(level.Length);
                    var varibleName = currentLine.SplitVaiableName();
                    string length = string.Empty;
                    var dataTypeField = currentLine.SplitDataTypeField();
                    var dataType = string.Empty;
                    if (!string.IsNullOrEmpty(dataTypeField))
                    {
                        length = dataTypeField.SplitLenght().Replace("(", "").Replace(")", "");
                        dataType = dataTypeField.SplitDataType();
                    }
                    if (level == "01")
                    {
                        sectionId++;
                        sectionName = varibleName.TrimEnd(' ').TrimEnd('.');
                    }
                    var defaultValue = currentLine.SplitDefaultValue();
                    defaultValue = defaultValue.Replace(".", "").Trim();
                    var pictureClause = currentLine.SplitPictureClause();
                    var computationBinary = currentLine.SplitComputationBinary();

                    var coboVarible = new CobolVariable
                    {
                        FileId = fileMaster.FileId,
                        SectionId = sectionId,
                        SectionName = sectionName,
                        ProjectId = projectId,
                        VariableLevel = level,
                        VariableName = varibleName,
                        DefaultValue = defaultValue,
                        Lenght = length,
                        DataTypeField = dataTypeField,
                        PictureClause = pictureClause,
                        ComputationOrBinary = computationBinary,
                        DataType = dataType,
                        ParentId = null, 
                        GraphId = null
                    };
                    lstStatement.Add(coboVarible);
                }
                var updatedList = lstStatement.GetWorkingStorageParentItem();
                foreach (var statement in updatedList)
                {
                    await _codeVortoService.CobolVariableRepository.AddNewItem(statement);
                }
                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertFileSection(int projectId, int langaugeId, List<string> tempBlock,
            FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                int sectionId = 0;
                string sectionName = string.Empty;
                foreach (var line in tempBlock)
                {
                    var currentLine = line.TrimStart();
                    string level = currentLine.SplitLevel();
                    string fdLevel = currentLine.SplitFdLevel();
                    if (string.IsNullOrEmpty(level) && string.IsNullOrEmpty(fdLevel)) continue;
                    if (string.IsNullOrEmpty(level)) level = "";
                    currentLine = currentLine.Substring(level.Length);
                    var varibleName = currentLine.SplitVaiableName();
                    string length = string.Empty;
                    var dataType = currentLine.SplitDataTypeField();
                    if (!string.IsNullOrEmpty(dataType))
                        length = dataType.SplitLenght();

                    if (currentLine.StartsWith("FD ") || currentLine.StartsWith(" FD "))
                    {
                        sectionId++;
                        sectionName = fdLevel.TrimEnd(' ').TrimEnd('.');
                        varibleName = fdLevel;
                    }
                    var defaultValue = currentLine.SplitDefaultValue();
                    defaultValue = defaultValue.Replace(".", "").Trim();
                    var pictureClause = currentLine.SplitPictureClause();
                    var computationBinary = currentLine.SplitComputationBinary();
                    var coboVaribale = new CobolVariable
                    {
                        FileId = fileMaster.FileId,
                        SectionId = sectionId,
                        SectionName = sectionName,
                        ProjectId = projectId,
                        VariableLevel = level,
                        VariableName = varibleName,
                        DefaultValue = defaultValue,
                        Lenght = length,
                        DataType = dataType,
                        PictureClause = pictureClause,
                        ComputationOrBinary = computationBinary
                    };
                    await _codeVortoService.CobolVariableRepository.AddNewItem(coboVaribale);
                }
            }
            return Ok("Insert all fileSection data successfully!");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBusinessNamesForMethods(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId && (f.FileTypeExtensionId == 6))
                    .ConfigureAwait(false);

                foreach (var fileMaster in allFiles)
                {
                    var fileStatements = File.ReadAllLines(fileMaster.FilePath).ToList();
                    fileStatements = fileStatements.PrepareSameLength();
                    fileStatements = fileStatements.RemoveCharacter(6, 66);
                    //foreach (var statement in fileStatements)
                    //{
                    //    var newLine = statement.Substring(6, 6);
                    //    newList.Add(newLine);
                    //}
                    // fileStatements = fileStatements.RemoveDot();
                    fileStatements.RemoveAll(s => s.Length <= 0);
                    fileStatements = fileStatements.Select(s => s.Trim('.')).ToList();
                    fileStatements = fileStatements.Select(s => s.TrimStart()).ToList();
                    var master = fileMaster;
                    var allMethods = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == master.FileId && s.BaseCommandId == 8)
                        .ConfigureAwait(false);
                    string fileName = Path.GetFileNameWithoutExtension(master.FilePath) + "()";

                    foreach (var stateRef in allMethods)
                    {
                        if (stateRef.MethodName == fileName)
                        {
                            var fileBusinessName = fileStatements.First().Trim('*').StartsWith("PA ")
                                ? fileStatements.First().Trim('*').Replace("PA ", "").Trim()
                                : fileStatements.First().Trim('*').Trim();
                            stateRef.BusinessName = fileBusinessName;
                            stateRef.FileMaster = null;
                            stateRef.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef)
                                .ConfigureAwait(false);
                            continue;
                        }

                        string originalStatement = stateRef.OriginalStatement.Trim();
                        int methodIndex = fileStatements.IndexOf(originalStatement.Trim());
                        if (methodIndex <= 0) methodIndex = fileStatements.IndexOf(originalStatement + "*");
                        if (methodIndex <= 0) continue;
                        var methodComments = string.Empty;

                        // var goupTo4Lines = methodIndex - 4;
                        for (var length = methodIndex; length >= 0; length--)
                        {
                            var line = fileStatements[length];
                            if (!line.StartsWith("*"))
                                //if (length == goupTo4Lines)
                                //{
                                //    if (methodComments == string.Empty) methodComments = null;
                                //    break;
                                //}
                                if (!line.StartsWith("* ")) continue;
                            methodComments = line.TrimStart('*').Trim();
                            break;
                        }
                        if (stateRef.BusinessName == methodComments) continue;
                        if (string.IsNullOrEmpty(methodComments)) continue;
                        stateRef.BusinessName = methodComments;
                        stateRef.FileMaster = null;
                        stateRef.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef)
                            .ConfigureAwait(false);
                    }
                }
                return Ok("Business names are updated successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessProjectInventory(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);

                var clsCobolProcess = new ClsCobolProcess();

                foreach (var fileMaster in fileMasters)
                {
                    // if (fileMaster.FileId != 531) continue;
                    var statementRefMasters = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == fileMaster.FileId).ConfigureAwait(false);
                    var statementRefCalledFrom = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.ReferenceFileId == fileMaster.FileId).ConfigureAwait(false);
                    var allDataDependancies = await _codeVortoService.DataDependencyRepository
                        .GetAllListItems(d => d.FileId == fileMaster.FileId).ConfigureAwait(false);
                    var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                        .GetAllListItems(a => a.FileId == fileMaster.FileId).ConfigureAwait(false);

                    int complexity = statementRefMasters.Count(c => c.BaseCommandId == 1 || c.BaseCommandId == 5) + 1;
                    string objectName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                    string extenstionType = fileMaster.FileTypeExtensionReference.FileTypeName;
                    int linesCount = fileMaster.LinesCount;
                    var internalCall = (from c in statementRefMasters where c.BaseCommandId == 5 select c).Count();
                    var calledExternalList = new List<string>();
                    var externalCallList = (from x in statementRefMasters
                                            where x.BaseCommandId == 6
                                            select x).ToList();
                    var refFileIds = new List<int>();
                    var addedMissings = new List<string>();
                    var addedMissingCalls = new List<string>();
                    foreach (var eCall in externalCallList)
                    {
                        string str;
                        int refFileId = eCall.ReferenceFileId;
                        if (addedMissings.Any(m => m == eCall.OriginalStatement)) continue;
                        addedMissings.Add(eCall.OriginalStatement);

                        var originalStatement = eCall.OriginalStatement.PadRight(20);

                        clsCobolProcess.GetExternalCallFiles(originalStatement,
                            fileMaster.FileTypeExtensionId, fileMasters, out NameValue nameValue);

                        if (refFileId == 0 && addedMissingCalls.All(c => c != nameValue.CalledObjectName))
                        {
                            addedMissingCalls.Add(nameValue.CalledObjectName);
                            str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                  "cursor: pointer; text-decoration: none;'>" + nameValue.CalledObjectName +
                                  " [ Missing ]</a>";
                            calledExternalList.Add(str);
                            continue;
                        }

                        var refFileMaster = eCall.ReferenceFileMaster;
                        if (refFileMaster == null) continue;
                        bool exist = refFileIds.Any(f => f == refFileMaster.FileId);
                        if (exist) continue;
                        refFileIds.Add(refFileMaster.FileId);
                        str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; " +
                              "text-decoration: underline;' onclick='includeStateDialog(" + refFileMaster.FileId + ");'>" +
                            refFileMaster.FileName + "</a>";
                        calledExternalList.Add(str);
                    }

                    if (calledExternalList.Any())
                    {
                        string str = "<li>" + calledExternalList[0] + "</li>";
                        calledExternalList[0] = str;
                    }

                    var calledFromList = new List<string>();
                    var addedFileId = new List<int>();
                    foreach (var uObj in statementRefCalledFrom)
                    {
                        if (addedFileId.Any(f => f == uObj.FileMaster.FileId)) continue;
                        var str = "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                  " onclick='includeStateDialog(" + uObj.FileMaster.FileId + ");'>" +
                                  uObj.FileMaster.FileName + "</a>";
                        addedFileId.Add(uObj.FileMaster.FileId);
                        calledFromList.Add(str);
                    }
                    if (calledFromList.Any())
                    {
                        string str = "<li>" + calledFromList[0] + "</li>";
                        calledFromList[0] = str;
                    }

                    var dataDependancies = (from d in allDataDependancies select d.Entity).Distinct().ToList();
                    if (dataDependancies.Any())
                    {
                        string str = "<li>" + dataDependancies[0] + "</li>";
                        dataDependancies[0] = str;
                    }

                    var allWorkFlowsTempList = new List<string>();
                    foreach (var workflow in allActionWorkflows)
                    {
                        string link = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                                      workflow.MethodStatementId +
                                      "&aId=" + workflow.ActionWorkflowId;
                        string workFlowNm = workflow.WorkflowName;
                        var str = !string.IsNullOrEmpty(workflow.TechnicalAndBusinessName)
                            ? "<a href='javascript:void(0);' onclick='openLink(aDiv_" + workflow.ActionWorkflowId +
                              ");' style='color: blue; text-decoration: underline;'>" +
                              workflow.TechnicalAndBusinessName + "</a> <div style='display: none;' id='aDiv_" +
                              workflow.ActionWorkflowId + "'>" + link + "</div> "
                            : "<a href='javascript:void(0);' onclick='openLink(aDiv_" + workflow.ActionWorkflowId +
                              ");' style='color: blue; text-decoration: underline;'>" + workFlowNm +
                              "</a> <div style='display: none;' id='aDiv_" +
                              workflow.ActionWorkflowId + "'>" + link + "</div> ";
                        allWorkFlowsTempList.Add(str);
                    }

                    string uEntities = string.Join("<li>", dataDependancies);
                    string cFrom = string.Join("<li>", calledFromList);
                    string cExternal = string.Join("<li>", calledExternalList);
                    string partInWorkflow = string.Join("<li>", allWorkFlowsTempList);

                    var callFormFinal = calledFromList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + fileMaster.FileId +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                          fileMaster.FileId + "'>" + cFrom + " </div> " : "-";

                    var inventoryDetails = new ProjectInventory
                    {
                        ObjectName =
                            "<pre><a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                            " onclick='includeStateDialog(" + fileMaster.FileId + ");'>" + objectName + "</a></pre>",
                        Description = fileMaster.FileName,
                        ExtenstionType = extenstionType,
                        LoC = linesCount,
                        Complexity = complexity,
                        InternalCall = internalCall,
                        CalledFrom = callFormFinal,
                        ProjectId = projectMaster.ProjectId,
                        SolutionId = fileMaster.SolutionId ?? 0,
                        FileId = fileMaster.FileId,
                        CallingTo = null,
                        UsesObjects = "-",
                        UsesReports = "-",
                        UsesQueries = "-",
                        ExternalCall = calledExternalList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvCExternal_" + fileMaster.FileId +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              calledExternalList.Count + " Call External(s) </a> <div style='display: none;' id='dvCExternal_" +
                              fileMaster.FileId + "'>" + cExternal + " </div> "
                            : "-",
                        UsesEntities = dataDependancies.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvDataDepend_" + fileMaster.FileId +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              dataDependancies.Count +
                              " Entity(ies) </a> <div style='display: none;' id='dvDataDepend_" + fileMaster.FileId + "'>" +
                              uEntities + " </div> "
                            : "-",
                        ParticipateInWorkflow = allWorkFlowsTempList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvPartInWork_" + fileMaster.FileId +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              allWorkFlowsTempList.Count +
                              " Workflow(s) </a> <div style='display: none;' id='dvPartInWork_" + fileMaster.FileId + "'>" +
                              partInWorkflow + " </div> "
                            : "-"
                    };
                    using (var appDbContext = new AppDbContext())
                    {
                        appDbContext.Set<ProjectInventory>().Add(inventoryDetails);
                        await appDbContext.SaveChangesAsync();
                    }
                }

                Console.WriteLine("=================================================================================");
                Console.WriteLine("Done processing inventory for Project Id: " + projectMaster.ProjectName);
                Console.WriteLine("=================================================================================");
                return Ok("Done processing inventory for Project Id: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForDataDependency(int projectId, string opt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);

                foreach (var fileMaster in fileMasters)
                {
                    var statementRefMasters = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == fileMaster.FileId && s.BaseCommandId == 45)
                        .ConfigureAwait(false);

                    if (!statementRefMasters.Any()) continue;

                    var readNextRegEx = new Regex(@"^(READ\s+NEXT\s+|READ\s+PREV\s+)",
                        RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);
                    var readNextFileNameRegEx = new Regex(@"(READ\s+NEXT|READ\s+PREV)\s+(?<table>\S+)",
                        RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);

                    var openRegEx = new Regex(@"^(OPEN\s+|READ\s+|DELETE\s+|START\s+|WRITE\s+|REWRITE\s+)",
                        RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);
                    var openFileNameRegEx =
                        new Regex(@"(INPUT|OUTPUT|I-O|EXTEND|READ|DELETE|START|FROM)\s+(?<table>\S+)",
                            RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);

                    var closeRegEx = new Regex(@"^(CLOSE\s+)", RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);
                    var closeFileNameRegEx =
                        new Regex(@"(CLOSE)\s+(?<table>\S+.*)",
                            RegexOptions.IgnoreCase & RegexOptions.CultureInvariant);

                    foreach (var statementReference in statementRefMasters)
                    {
                        string originalStatement = Regex.Replace(statementReference.OriginalStatement, @"\s+", " ");
                        if (readNextRegEx.IsMatch(originalStatement))
                        {
                            foreach (Match match in readNextFileNameRegEx.Matches(originalStatement))
                            {
                                if (match.Groups["table"] == null) continue;
                                string openFileName = match.Groups["table"].Value.TrimEnd('.', ')').TrimStart('(');
                                var openDataDependency = new DataDependency
                                {
                                    FileMaster = null,
                                    Attributes = null,
                                    ProjectId = fileMaster.ProjectId,
                                    FileId = fileMaster.FileId,
                                    Entity = openFileName,
                                    EntityOld = openFileName,
                                    FileIdOld = fileMaster.FileId,
                                    DataDepedencyId = 0
                                };
                                await _codeVortoService.DataDependencyRepository.AddNewItem(openDataDependency)
                                    .ConfigureAwait(false);
                                break;
                            }
                            continue;
                        }
                        if (openRegEx.IsMatch(originalStatement))
                        {
                            foreach (Match match in openFileNameRegEx.Matches(originalStatement))
                            {
                                if (match.Groups["table"] == null) continue;
                                string openFileName = match.Groups["table"].Value.TrimEnd('.', ')').TrimStart('(');
                                var openDataDependency = new DataDependency
                                {
                                    FileMaster = null,
                                    Attributes = null,
                                    ProjectId = fileMaster.ProjectId,
                                    FileId = fileMaster.FileId,
                                    Entity = openFileName,
                                    EntityOld = openFileName,
                                    FileIdOld = fileMaster.FileId,
                                    DataDepedencyId = 0
                                };
                                await _codeVortoService.DataDependencyRepository.AddNewItem(openDataDependency)
                                    .ConfigureAwait(false);
                                break;
                            }
                            continue;
                        }
                        if (closeRegEx.IsMatch(originalStatement))
                        {
                            foreach (Match match in closeFileNameRegEx.Matches(originalStatement))
                            {
                                if (match.Groups["table"] == null) continue;
                                string closeFileName = match.Groups["table"].Value.TrimEnd('.', ')').TrimStart('(');
                                var allClosingFiles = closeFileName.Split(' ').ToList();
                                foreach (var closeFile in allClosingFiles)
                                {
                                    if (string.IsNullOrEmpty(closeFile)) continue;
                                    var openDataDependency = new DataDependency
                                    {
                                        FileMaster = null,
                                        Attributes = null,
                                        ProjectId = fileMaster.ProjectId,
                                        FileId = fileMaster.FileId,
                                        Entity = closeFile,
                                        EntityOld = closeFile,
                                        FileIdOld = fileMaster.FileId,
                                        DataDepedencyId = 0
                                    };
                                    await _codeVortoService.DataDependencyRepository.AddNewItem(openDataDependency)
                                        .ConfigureAwait(false);
                                }
                                break;
                            }
                            continue;
                        }
                        if (!statementReference.OriginalStatement.StartsWith("EXEC SQL ")) continue;
                        string execSqlStatement = statementReference.OriginalStatement.Replace("EXEC SQL ", "").Trim();
                        string inputString = Regex.Replace(execSqlStatement, @"\s+", " ");
                        var regex = new Regex(@"^(SELECT\s+|INSERT\s+|DELETE\s+|UPDATE\s+)", RegexOptions.IgnoreCase);
                        if (!regex.IsMatch(inputString)) continue;
                        var tableRegEx = new Regex(@"(FROM|JOIN|INTO|TABLE|DECLARE)\s+(?<table>\S+)", RegexOptions.IgnoreCase);
                        if (!tableRegEx.IsMatch(inputString)) continue;

                        var lstTables = new List<string>();
                        foreach (Match match in tableRegEx.Matches(inputString))
                        {
                            if (match.Groups["table"] == null) continue;
                            string tableName = match.Groups["table"].Value.TrimEnd('.', ')').TrimStart('(');
                            lstTables.Add(tableName);
                        }
                        if (!lstTables.Any()) continue;

                        var dataDependency = new DataDependency
                        {
                            FileMaster = null,
                            Attributes = null,
                            ProjectId = fileMaster.ProjectId,
                            FileId = fileMaster.FileId,
                            Entity = lstTables.Last(),
                            EntityOld = lstTables.Last(),
                            FileIdOld = fileMaster.FileId,
                            DataDepedencyId = 0
                        };
                        await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependency).ConfigureAwait(false);
                    }
                }

                return Ok("Process for data-dependency completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForDataDependency(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var fileMasters = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(x => x.FileTypeExtensionId == 6).ConfigureAwait(false);
                    foreach (var fileMaster in fileMasters)
                    {
                        if (!File.Exists(fileMaster.FilePath)) return NotFound();

                        var statementRefMasters = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.FileId == fileMaster.FileId && s.BaseCommandId == 45)
                            .ConfigureAwait(false);

                        var allLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                        if (allLines.Count <= 0) return Ok();

                        allLines = allLines.PrepareSameLength();
                        allLines = allLines.RemoveCharacter(6, 66);
                        string[] comments = { "*", "/" };
                        allLines = allLines.RemoveAllCommentedLines(comments);
                        allLines = allLines.RemoveAll("SKIP", "EJECT");
                        allLines = allLines.RemoveEmptyLines();
                        if (!allLines.Any()) continue;
                        var cobolSection = await _codeVortoService.CobolSectionRepository
                            .GetAllListItems(x => x.SectionId != 0)
                            .ConfigureAwait(false);
                        var cobolSectionList = cobolSection.Select(x => x.SectionName).ToList();

                        // This is  FILE-CONTROL. Section Data
                        var allFileControlSection =
                            allLines.GetStatementBetweenSection(cobolSectionList, "FILE-CONTROL.");
                        foreach (var fileControl in allFileControlSection)
                        {
                            var statement = fileControl;
                            if (string.IsNullOrEmpty(statement)) continue;
                            Console.WriteLine(statement);
                        }

                        // This is FILE SECTION. Section Data

                        var allFileSection = allLines.GetStatementBetweenSection(cobolSectionList, "FILE SECTION.");
                        foreach (var fileSection in allFileSection)
                        {
                            var statement = fileSection;
                            if (string.IsNullOrEmpty(statement)) continue;
                            Console.WriteLine(statement);
                            var regexPattern = @"(^FD)";
                            if (!Regex.IsMatch(statement, regexPattern)) continue;
                            Console.WriteLine(statement);
                            var regexFd = new Regex(@"^FD\s+([A-z0-9\-]+)");
                            var matchRegex = regexFd.Match(statement);
                            var currentStatement = matchRegex.Groups[1].Value;
                            if (string.IsNullOrEmpty(currentStatement)) continue;
                            var openDataDependency = new DataDependency
                            {
                                FileMaster = null,
                                Attributes = null,
                                ProjectId = fileMaster.ProjectId,
                                FileId = fileMaster.FileId,
                                Entity = currentStatement,
                                EntityOld = currentStatement,
                                FileIdOld = fileMaster.FileId,
                                DataDepedencyId = 0
                            };
                            var dataDependancy = await _codeVortoService.DataDependencyRepository
                                .GetAllListItems(x => x.FileId == fileMaster.FileId && x.Entity == currentStatement)
                                .ConfigureAwait(false);
                            if (dataDependancy.Any()) continue;
                            await _codeVortoService.DataDependencyRepository.AddNewItem(openDataDependency)
                                .ConfigureAwait(false);
                        }

                        foreach (var statementReference in statementRefMasters)
                        {
                            if (!statementReference.OriginalStatement.StartsWith("EXEC SQL ")) continue;
                            string execSqlStatement = statementReference.OriginalStatement.Replace("EXEC SQL ", "").Trim();
                            string inputString = Regex.Replace(execSqlStatement, @"\s+", " ");
                            var regex = new Regex(@"^(SELECT\s+|INSERT\s+|DELETE\s+|UPDATE\s+)", RegexOptions.IgnoreCase);
                            if (!regex.IsMatch(inputString)) continue;
                            var tableRegEx = new Regex(@"(FROM|JOIN|INTO|TABLE|DECLARE)\s+(?<table>\S+)", RegexOptions.IgnoreCase);
                            if (!tableRegEx.IsMatch(inputString)) continue;

                            var lstTables = new List<string>();
                            foreach (Match match in tableRegEx.Matches(inputString))
                            {
                                if (match.Groups["table"] == null) continue;
                                string tableName = match.Groups["table"].Value.TrimEnd('.', ')').TrimStart('(');
                                lstTables.Add(tableName);
                            }
                            if (!lstTables.Any()) continue;
                            var entity = lstTables.Last();
                            var dataDependency = new DataDependency
                            {
                                FileMaster = null,
                                Attributes = null,
                                ProjectId = fileMaster.ProjectId,
                                FileId = fileMaster.FileId,
                                Entity = entity,
                                EntityOld = entity,
                                FileIdOld = fileMaster.FileId,
                                DataDepedencyId = 0
                            };
                            var dataDepdancy = await _codeVortoService.DataDependencyRepository
                                .GetAllListItems(x => x.FileId == fileMaster.FileId && x.Entity == entity)
                                .ConfigureAwait(false);
                            if (dataDepdancy.Any()) continue;
                            await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependency).ConfigureAwait(false);
                        }
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateProjectStatus(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                projectMaster.Processed = 1;
                projectMaster.ProcessedDate = DateTime.Now;
                projectMaster.ProcessedTime = DateTime.Now;
                await _codeVortoService.ProjectMasterRepository.UpdateItem(projectMaster);

                return Ok(projectMaster);
            }
        }

        public List<CobolVariable> GetParentItem(List<CobolVariable> workingStorageSection)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                /*
                var workingStorageSection = await _codeVortoService.CobolVariableRepository
                    .GetAllListItems(d => d.FileId == 169).ConfigureAwait(false);
                */
                workingStorageSection.ForEach(w => { w.GraphId = "GraphId-" + w.VariableId; });
                int indexPosition = -1;
                foreach (var cobolVariable in workingStorageSection)
                {
                    indexPosition++;
                    if (int.Parse(cobolVariable.VariableLevel) == 1)
                    {
                        cobolVariable.ParentId = "-1";
                        continue;
                    }
                    var itsLevel = int.Parse(cobolVariable.VariableLevel);
                    for (int idx = indexPosition; idx >= 0; idx--)
                    {
                        var element = workingStorageSection[idx];
                        if (itsLevel == int.Parse(element.VariableLevel)) continue;

                        if (int.Parse(element.VariableLevel) >= itsLevel) continue;

                        cobolVariable.ParentId = element.GraphId;
                        break;
                    }
                }
                /*
                foreach (var cobolVariable in workingStorageSection)
                {
                    await _codeVortoService.CobolVariableRepository.UpdateItem(cobolVariable).ConfigureAwait(false);
                }
                
                workingStorageSection.ForEach(ele =>
                {
                    Console.WriteLine("Variable Name==========, Variable Level, Graph Id, Parent Id");
                    Console.WriteLine($"{ele.VariableName} | {ele.VariableLevel} | {ele.GraphId} | {ele.ParentId}");
                    Console.WriteLine("============================================");
                });
                */
                return workingStorageSection;
            }
        }
    }
}
