using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using CsvHelper.Configuration;
using LumenWorks.Framework.IO.Csv;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public UniverseBasicController()
        {

        }

        public UniverseBasicController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public IHttpActionResult StartProcess(int projectId)
        {
            Task.Factory.StartNew(async () => { await ExecuteProcessActionsOneByOne(projectId); });
            return Ok("Processing started successfully");
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

                    bool isCtCode = projectMaster.IsCtCode;
                    int solutionId = projectMaster.SolutionId ?? 5;

                    var httpResponseMessage = ValidateDirectoryStructureAndRequiredFiles(projectMaster);
                    if (httpResponseMessage.StatusCode == HttpStatusCode.NotFound)
                        return Ok(await httpResponseMessage.Content.ReadAsStringAsync());

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

                    // StartProcessUbProject

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "StartProcessUbProject");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: StartProcessUbProject\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(dashLine);
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await StartProcessUbProject(projectMaster.ProjectId, isCtCode).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: StartProcessUbProject\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UploadFileMenuDataRevised

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UploadFileMenuDataRevised");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UploadFileMenuDataRevised\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UploadFileMenuDataRevised(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UploadFileMenuDataRevised\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UploadDataDictionary

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UploadDataDictionary");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UploadDataDictionary\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);


                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UploadDataDictionary(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UploadDataDictionary\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessForUniverseDescriptor

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForUniverseDescriptor");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessForUniverseDescriptor\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessForUniverseDescriptor(projectMaster, isCtCode).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessForUniverseDescriptor\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessUniverseJcls

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessUniverseJcls");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessUniverseJcls\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessUniverseJcls(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessUniverseJcls\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessUniversePrograms

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessUniversePrograms");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessUniversePrograms\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessUniversePrograms(projectMaster.ProjectId).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessUniversePrograms\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessUniverseSubRoutinesAndIncludes

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessUniverseSubRoutinesAndIncludes");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessUniverseSubRoutinesAndIncludes\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessUniverseSubRoutinesAndIncludes(projectMaster.ProjectId)
                            .ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessUniverseSubRoutinesAndIncludes\nDate: " +
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
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ViewSource(projectMaster).ConfigureAwait(false); // Done

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

                    // UpdateRefFileIdForProgramsAndSubRoutines

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateRefFileIdForProgramsAndSubRoutines");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateRefFileIdForProgramsAndSubRoutines\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateRefFileIdForProgramsAndSubRoutines(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateRefFileIdForProgramsAndSubRoutines\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateRefFileIdForIncludes 

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateRefFileIdForIncludes");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateRefFileIdForIncludes\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateRefFileIdForIncludes(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateRefFileIdForIncludes\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateReferenceFileIdForPrograms

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateReferenceFileIdForPrograms");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateReferenceFileIdForPrograms\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateReferenceFileIdForPrograms(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateReferenceFileIdForPrograms\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateBusinessNamesForMethods

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateBusinessNamesForMethods");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateBusinessNamesForMethods\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateBusinessNamesForMethods(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateBusinessNamesForMethods\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateBusinessNamesForIncludes

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateBusinessNamesForIncludes");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateBusinessNamesForIncludes\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateBusinessNamesForIncludes(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateBusinessNamesForIncludes\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateBusinessNameForBaseCommandId19

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateBusinessNameForBaseCommandId19");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateBusinessNameForBaseCommandId19\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateBusinessNameForBaseCommandId19(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateBusinessNameForBaseCommandId19\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // StartParsingProcessUniverseBasic

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "StartParsingProcessUniverseBasic");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: StartParsingProcessUniverseBasic\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await StartParsingProcessUniverseBasic(projectMaster.ProjectId).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: StartParsingProcessUniverseBasic\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // UpdateBusinessNameTo651Chars

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateBusinessNameTo651Chars");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: UpdateBusinessNameTo651Chars\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await UpdateBusinessNameTo651Chars(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: UpdateBusinessNameTo651Chars\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessPseudoCodeConversion

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessPseudoCodeConversion");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessPseudoCodeConversion\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessPseudoCodeConversion(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessPseudoCodeConversion\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // InsertActionWorkflowsFromUniverseMenuFile

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "InsertActionWorkflowsFromUniverseMenuFile");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: InsertActionWorkflowsFromUniverseMenuFile\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await InsertActionWorkflowsFromUniverseMenuFile(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: InsertActionWorkflowsFromUniverseMenuFile\nDate: " +
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
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await GetAllStartingPoints(projectMaster.ProjectId).ConfigureAwait(false);

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

                    // ProcessUniverseProgramWorkflows

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessUniverseProgramWorkflows");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessUniverseProgramWorkflows\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessUniverseProgramWorkflows(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessUniverseProgramWorkflows\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessCrudObjectReferences

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessCrudObjectReferences");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessCrudObjectReferences\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessCrudObjectReferences(projectMaster.ProjectId).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessCrudObjectReferences\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessIncludeToInclude

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessIncludeToInclude");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessIncludeToInclude\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessIncludeToInclude(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessIncludeToInclude\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessDataDependencyForUniVerse

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessDataDependencyForUniVerse");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessDataDependencyForUniVerse\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessDataDependencyForUniVerse(projectMaster).ConfigureAwait(false); // Done

                        endStatus = "Completed executing Method: ProcessDataDependencyForUniVerse\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessCrudActivity

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessCrudActivity");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessCrudActivity\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        // await InsertCrudActivityForUniverse(projectMaster); // Done
                        await ProcessCrudActivity(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessCrudActivity\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessActionWorkflowDetails

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessActionWorkflowDetails");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessActionWorkflowDetails\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessActionWorkflowDetails(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessActionWorkflowDetails\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessSubRoutineWorkflows

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessSubRoutineWorkflows");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessSubRoutineWorkflows\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessSubRoutineWorkflows(projectMaster).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessSubRoutineWorkflows\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessIncludeWorkflows

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessIncludeWorkflows");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessIncludeWorkflows\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessIncludeWorkflows(projectMaster).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessIncludeWorkflows\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // MissingEntitiesAndObjectsReport

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "MissingObjectsReport");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: MissingObjectsReport\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);
                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await MissingObjectsReport(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: MissingObjectsReport\nDate: " +
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

                    // ProcessForDataInventory

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessForDataInventory");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessForDataInventory\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        await ProcessForDataInventory(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessForDataInventory\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }

                    // ProcessEntityAttributeUsageReport
                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "ProcessEntityAttributeUsageReport");
                    if (projectProcessStep != null && !projectProcessStep.Status)
                    {
                        Console.WriteLine(dashLine);
                        startStatus = "Started executing Method: ProcessEntityAttributeUsageReport\nDate: " +
                                      DateTime.Now.ToString("g") + "\nTime: " +
                                      DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(startStatus);
                        stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                        projectProcessStep.StartDate = DateTime.Now;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                        // await ProcessCrudActivityReport(projectMaster.ProjectId).ConfigureAwait(false);
                        await ProcessEntityAttributeUsageReport(projectMaster.ProjectId).ConfigureAwait(false);

                        endStatus = "Completed executing Method: ProcessEntityAttributeUsageReport\nDate: " +
                                    DateTime.Now.ToString("g") + "\nTime: " +
                                    DateTime.Now.ToString("HH:m:s tt zzz");
                        Console.WriteLine(endStatus);
                        Console.WriteLine(dashLine);
                        stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                        projectProcessStep.EndDate = DateTime.Now;
                        projectProcessStep.Status = true;
                        await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    }
                    //

                    Console.WriteLine(dashLine);
                    startStatus = "Started executing Method: InsertSystemLevelCrudActivityForUniverse\nDate: " +
                                  DateTime.Now.ToString("g") + "\nTime: " +
                                  DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                    // await InsertSystemLevelCrudActivityForUniverse(projectMaster).ConfigureAwait(false);

                    endStatus = "Completed executing Method: InsertSystemLevelCrudActivityForUniverse\nDate: " +
                                DateTime.Now.ToString("g") + "\nTime: " +
                                DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    // UpdateProjectStatus

                    projectProcessStep = processStepList.Find(x => x.ProcessStep == "UpdateProjectStatus");
                    // if (projectProcessStep != null && !projectProcessStep.Status)
                    // {
                    projectProcessStep.StartDate = DateTime.Now;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);

                    await UpdateProjectStatus(projectMaster).ConfigureAwait(false);

                    projectProcessStep.EndDate = DateTime.Now;
                    projectProcessStep.Status = true;
                    await _codeVortoService.ProjectProcessingStepRepository.UpdateItem(projectProcessStep);
                    // }
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
                catch (MySqlException mySqlException)
                {
                    LogMessage.WriteLogMessage(stringBuilder);
                    LogMessage.WriteExceptionLogMessage(mySqlException);
                    return InternalServerError(mySqlException);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteLogMessage(stringBuilder);
                    LogMessage.WriteExceptionLogMessage(exception);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public HttpResponseMessage ValidateDirectoryStructureAndRequiredFiles(ProjectMaster projectMaster)
        {
            var allDirectories = Directory.GetDirectories(projectMaster.PhysicalPath).ToList();
            var requiredDirectories = new List<string> { "Menu", "I-Descriptors" };
            var directoryNames = (from d in allDirectories let dir = new DirectoryInfo(d) select dir.Name).ToList();
            bool isPresent = requiredDirectories.All(d => directoryNames.Any(r => r == d));
            if (!isPresent) return new HttpResponseMessage(HttpStatusCode.NotFound)
            {
                Content = new StringContent(" Project Directory not contains `Menu` Or `I-Descriptors` directories." +
                                            " Please add those directories with .csv files" +
                                            " (menu and I-Descriptors) respectively")
            };

            return new HttpResponseMessage(HttpStatusCode.OK);
        }

        [HttpGet]
        public async Task<HttpResponseMessage> StartProcessUbProject(int projectId, bool isCtCode)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null)
                    return new HttpResponseMessage(HttpStatusCode.BadRequest)
                    {
                        Content = new StringContent("Project not found: " + projectId)
                    };

                stringBuilder.AppendLine("================================================================");
                stringBuilder.AppendLine("\n" + "Started process for project: " + projectId + " project name: " +
                                       projectDetails.ProjectName + " and project physical path is: " +
                                       projectDetails.PhysicalPath);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId).ConfigureAwait(true);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                stringBuilder.AppendLine("==================================================================");
                stringBuilder.AppendLine("\n" + "Extensions: " + string.Join(",", strExtensions));
                var projectPath = projectDetails.PhysicalPath;
                var solutionId = projectDetails.SolutionId;
                var directoryList = new List<string> { projectPath };
                var ignoredFile = await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId)
                    .ConfigureAwait(false);
                var allPreviousFiles = await _codeVortoService.FileMasterRepository.GetAllListItems(f => f.ProjectId == projectId);

                var regExCommented = new Regex(@"^\/\/\*|^\/\*|^\*|^\'", RegexOptions.CultureInvariant);

                foreach (var directory in directoryList)
                {
                    try
                    {
                        var allFiles = Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                            .Where(s => strExtensions.Any(e => s.EndsWith(e) || s.ToUpper().EndsWith(e.ToUpper()))).ToList();
                        bool hasModified = false;
                        var enumerator = allFiles.GetEnumerator();
                        var lstFileMasters = new List<FileMaster>();
                        while (enumerator.MoveNext())
                        {
                            var currentFile = enumerator.Current;

                            var fileLines = File.ReadAllLines(currentFile).ToList();
                            int lineCount = fileLines.Count(line =>
                                !regExCommented.IsMatch(line) || !string.IsNullOrWhiteSpace(line));

                            if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                            var fileName = Path.GetFileName(currentFile);
                            if (string.IsNullOrEmpty(fileName)) continue;
                            if (fileName.Contains(".dll.config")) continue;
                            var extension = Path.GetExtension(currentFile);
                            var extensionId = fileTypeExtensionReferences.First(e => e.FileExtension == extension
                                            || string.Equals(e.FileExtension, extension, StringComparison.CurrentCultureIgnoreCase)).FileTypeExtensionId;
                            var fileTypeName = fileTypeExtensionReferences
                                .First(e => e.FileExtension == extension
                                || string.Equals(e.FileExtension, extension, StringComparison.CurrentCultureIgnoreCase))
                                .FileTypeName;

                            if (allPreviousFiles.Any(k => k.FileName == fileName && k.FilePath == currentFile))
                            {
                                var thisFile = allPreviousFiles.Find(
                                    k => k.FileName == fileName && k.FilePath == currentFile);
                                hasModified = true;
                                thisFile.IsNewVersion = 1;
                                thisFile.Processed = 0;
                                thisFile.DoneParsing = 0;
                                thisFile.LinesCount = lineCount;
                                thisFile.FileTypeExtensionReference = null;
                                thisFile.ProjectMaster = null;
                                thisFile.FileType = fileTypeName;
                                await _codeVortoService.FileMasterRepository.UpdateItem(thisFile);
                                continue;
                            }

                            var fileMaster = new FileMaster
                            {
                                FileId = 0,
                                FileName = fileName,
                                FilePath = currentFile,
                                FileTypeExtensionId = extensionId,
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                DoneParsing = 0,
                                IsNewVersion = 0,
                                LinesCount = lineCount,
                                FileType = fileTypeName,
                                Processed = 0,
                                FileTypeExtensionReference = null,
                                ProjectMaster = null
                            };
                            lstFileMasters.Add(fileMaster);
                        }

                        enumerator.Dispose();
                        await _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
                        if (isCtCode)
                        {
                            foreach (var fileMaster in lstFileMasters)
                            {
                                if (fileMaster.FileTypeExtensionId != 12) continue;
                                if (!fileMaster.FileName.StartsWith("I_", StringComparison.CurrentCultureIgnoreCase)) continue;

                                _codeVortoService.AppDbContextRepository.EntitiesToExclude
                                    .Add(new EntitiesToExclude
                                    {
                                        FileId = fileMaster.FileId,
                                        FileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                        FileMaster = null,
                                        RowId = 0,
                                        ProjectId = fileMaster.ProjectId
                                    });
                                await _codeVortoService.AppDbContextRepository.SaveChangesAsync().ConfigureAwait(false);
                            }
                        }
                        stringBuilder.AppendLine("============================================================");
                        stringBuilder.AppendLine("\n" + "Total files scaned: " + lstFileMasters.Count);

                        if (hasModified)
                        {
                            // Now, if any of the file is modified, then we need to execute delete command on various tables...
                            object[] sParamenters =
                            {
                                // new MySqlParameter("@fileIds", MySqlDbType.Text) { Value = string.Join(",", lstModifiedFiles)},
                                new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                            };
                            await _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure("SpDeletePreviouslyProcessedData", sParamenters);
                        }
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                    if (projectDetails.ProjectConfigType == 7)
                        return new HttpResponseMessage(HttpStatusCode.OK)
                        {
                            Content = new StringContent("Files extracted for Common Library type project for Universe Basic is completed.")
                        };
                }
                LogMessage.WriteLogMessage(stringBuilder);

                return new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent("Done") };
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniverseJcls(ProjectMaster projectDetails)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Load Project and Base Command Reference Details...

                /*
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                */

                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                int languageId = projectDetails.LanguageId;
                // var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await _codeVortoService.BaseCommandReferenceRepository.GetAllListItems()
                    .ConfigureAwait(false);

                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                 .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var universeBasicV1 = new UniverseBasicVersion1();
                var classStart = callClassIndicatorStart.Find(x => true);
                var classEnd = callClassIndicatorEnd.Find(x => true);
                var methodStart = methodIndicationStart.Find(x => true);
                var methodEnd = methodIndicationEnd.Find(x => true);
                var callExternalStart = callExternalIndicationStart.Find(x => true);
                var ifRegEx = new Regex(@"^IF\s", RegexOptions.IgnoreCase);
                var endIfRegex = new Regex(@"^END$|^END\sIF$|^END-IF$", RegexOptions.IgnoreCase);
                #endregion

                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectDetails.ProjectId && f.SolutionId == solutionId &&
                                          f.FileTypeExtensionId != 11 && f.FileTypeExtensionId != 12).ConfigureAwait(false);

                // TODO: Remove count condition...
                int count = -1;
                foreach (var fileMaster in allFiles.Where(f => f.FileTypeExtensionId == 10
                        && f.Processed == 0 && f.ProjectId == projectDetails.ProjectId))
                {
                    count++;
                    if (!File.Exists(fileMaster.FilePath)) continue;

                    Console.WriteLine("========================================================");
                    Console.WriteLine("Total Jcl's to process: " + allFiles.Count);
                    Console.WriteLine("Total Jcl's remaining: " + (allFiles.Count - count));
                    Console.WriteLine("========================================================");

                    var lstFileLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                    if (lstFileLines.Count <= 0) return Ok("No contents in file: " + fileMaster.FilePath);
                    lstFileLines.RemoveAll(s => s.Length <= 0);
                    lstFileLines = lstFileLines.Select(s => s.Trim()).ToList();
                    lstFileLines = lstFileLines.CombineAllBrokenLines('_');
                    lstFileLines = lstFileLines.ProcessForStatementsConversion(fileMaster);
                    var lstStatementReferenceMaster = universeBasicV1.PrepareStatementReferenceMasterStart(fileMaster, classStart.PrimaryReferenceId, methodStart.PrimaryReferenceId);
                    lstStatementReferenceMaster.ForEach(
                        s =>
                        {
                            var firstOrDefault = lstFileLines.FirstOrDefault();
                            if (firstOrDefault != null)
                                s.BusinessName = firstOrDefault.Replace("*", "");
                        });

                    var fileStatements = universeBasicV1
                        .PrepareParseJclFile(fileMaster.FileId, languageId, allFiles, baseCommandReference, lstFileLines,
                            callExternalStart.PrimaryReferenceId, "RUN", "CALL", "EXECUTE", "PH", "PHANTOM");

                    lstStatementReferenceMaster.AddRange(fileStatements);

                    var closeList = universeBasicV1.PrepareStatementReferenceMasterEnd(fileMaster,
                        classEnd.PrimaryReferenceId, methodEnd.PrimaryReferenceId);
                    lstStatementReferenceMaster.AddRange(closeList);
                    lstStatementReferenceMaster.ForEach(p =>
                    {
                        p.ProjectId = projectDetails.ProjectId;
                        p.SolutionId = solutionId;
                    });
                    int ifCount = lstStatementReferenceMaster.Count(s => ifRegEx.IsMatch(s.OriginalStatement));
                    int endIfCount = lstStatementReferenceMaster.Count(s => endIfRegex.IsMatch(s.OriginalStatement));
                    Console.WriteLine("========================================================================================");
                    Console.WriteLine("File Id: " + fileMaster.FileId + "\nFile Path: " +
                                      fileMaster.FilePath + "\nIF Count: " + ifCount + "\nEND IF Count: " +
                                      endIfCount);
                    Console.WriteLine("========================================================================================");
                    bool countMatch = ifCount == endIfCount;
                    if (!countMatch)
                    {
                        Console.WriteLine("========================================================================================");
                        Console.WriteLine("IF and END IF count is not matching in this file. Details as: ");
                        Console.WriteLine("File Id: " + fileMaster.FileId + "\nFile Name: " + fileMaster.FileName +
                                          "\nFile Path: " + fileMaster.FilePath);
                        Console.WriteLine("Skipped file for further processing ans marked file status as un-processed.");
                        Console.WriteLine("========================================================================================");
                        fileMaster.WorkFlowStatus = "IF and END-IF count is not matching";
                        fileMaster.ProjectMaster = null;
                        fileMaster.Processed = 4;
                        fileMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);

                        await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);
                        continue;
                    }

                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    // Update file status to processed = 1...
                    fileMaster.Processed = 1;
                    fileMaster.WorkFlowStatus = "Workflow processed successfully";
                    fileMaster.ProjectMaster = null;
                    fileMaster.FileTypeExtensionReference = null;

                    await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster);
                }
                Console.WriteLine("====================================================================================\n");
                Console.WriteLine("All file details are imported to database successfully for Project: " + projectDetails.ProjectName);
                Console.WriteLine();
                Console.WriteLine("====================================================================================\n");

                return Ok("All files are processed");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniversePrograms(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Initialize...
                var universeBasicV1 = new UniverseBasicVersion1();
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllListItems().ConfigureAwait(false);
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment")
                        .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
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
                var methodStart = methodIndicationStart.Find(x => true);
                var methodEnd = methodIndicationEnd.Find(x => true);
                #endregion

                var ifRegEx = new Regex(@"^IF\s", RegexOptions.IgnoreCase);
                var endIfRegex = new Regex(@"^END$|^END\sIF$|^END-IF$", RegexOptions.IgnoreCase);

                var allPrograms = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.ProjectId == projectId && p.SolutionId == solutionId &&
                                          p.FileTypeExtensionId == 9 && p.Processed == 0).ConfigureAwait(false);

                //var allPrograms = await _codeVortoService.FileMasterRepository
                //    .GetAllListItems(p => p.ProjectId == projectId && p.FileId == 2922).ConfigureAwait(false);

                // TODO: Remove count condition
                int count = -1;
                foreach (var fileMaster in allPrograms)
                {
                    count++;
                    if (!File.Exists(fileMaster.FilePath)) continue;

                    var programLineList = new List<string>();
                    var programFileLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                    string programName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                    programFileLines.RemoveAll(s => s.Length <= 0);
                    programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                    programFileLines = programFileLines.CombineAllBrokenLines('_');
                    programFileLines = programFileLines.ProcessForStatementsConversion(fileMaster);

                    int ifCount1 = programFileLines.Count(a => a.StartsWith("IF "));
                    int endIfCount1 = programFileLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");
                    bool result = ifCount1 == endIfCount1;
                    if (!result)
                    {
                        if (fileMaster.FileTypeExtensionId == 9 || fileMaster.FileTypeExtensionId == 17)
                            ifCount1 = ifCount1 + 1;
                    }
                    // result = ifCount == endIfCount;
                    // int endIfCount = lstConvertedLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");
                    // if (ifCount == endIfCount) return lstConvertedLines;

                    Console.WriteLine("=============================================================================");
                    Console.WriteLine("IF Count: " + ifCount1);
                    Console.WriteLine("END IF Count: " + endIfCount1);
                    Console.WriteLine("File Id: " + fileMaster.FileId);
                    Console.WriteLine("File Name: " + Path.GetFileName(fileMaster.FilePath));
                    Console.WriteLine("File Path: " + fileMaster.FilePath);
                    Console.WriteLine("Total Files Count: " + allPrograms.Count);
                    Console.WriteLine("Files Remaining: " + (allPrograms.Count - count));
                    Console.WriteLine("=============================================================================");

                    var copyOfFileLines = programFileLines.ToList();
                    if (copyOfFileLines.Count <= 0) continue;
                    var allIncludesFiles = programFileLines.GetAllIncludeStatements("$INSERT", "$INCLUDE");

                    var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                    var methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(programFileLines, lstAllGoSubs);
                    var tempBlock = new List<string>();
                    int initialCount = 0;
                    foreach (var dict in methodBlockDictionary)
                    {
                        if (dict.Key != "STOP") continue;

                        tempBlock = dict.Value;
                        initialCount = dict.Value.Count;

                        var caseStmtCount = tempBlock.Count(s => s == "BEGIN CASE");
                        if (caseStmtCount >= 1)
                        {
                            var tBlock = tempBlock.SimplifyCaseAndNestedCaseStatementsBlock();
                            tempBlock = tBlock;
                        }
                        else
                        {
                            var listBlock = tempBlock.ReModifyMethodBlockForIfWithMatchingEndIf();
                            tempBlock = listBlock;
                        }

                        break;
                    }
                    if (tempBlock.Count <= 0)
                        tempBlock = programFileLines.GetListUpToLastStop();

                    programFileLines.RemoveRange(0, initialCount);
                    string firstLine = copyOfFileLines.First();
                    tempBlock.Insert(0, firstLine.Replace("*", ""));
                    tempBlock.Insert(1, programName + ":");
                    methodBlockDictionary.Add(programName + ":", tempBlock);
                    lstAllGoSubs.Add(programName + ":");

                    allIncludesFiles = allIncludesFiles.Distinct().ToList();
                    foreach (var includeStatement in tempBlock)
                    {
                        if (allIncludesFiles.Any(i => i == includeStatement))
                            allIncludesFiles.Remove(includeStatement);
                    }

                    programLineList.InsertRange(0, tempBlock);
                    programLineList.InsertRange(2, allIncludesFiles.ToList());

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
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines, currentLine);
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        programLineList.AddRange(methodBlockOriginal);

                        if (methodBlock.Value == null) continue;

                        var lastSting = methodBlock.Value.Last();
                        if (lastSting != "RETURN")
                        {
                            Console.WriteLine("Method: " + currentLine + ":- RETURN Statement is missing. Added it manually");
                            methodBlock.Value.Add("RETURN");
                        }
                        programLineList.AddRange(methodBlock.Value);
                    }

                    // programLineList = programLineList.SplitIfElseThenStatement(true);
                    programLineList = programLineList.Where(s => !string.IsNullOrEmpty(s)).ToList();
                    var copyOfFileLinesWithComments = programLineList.ToList();
                    programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();

                    var programFilePath = fileMaster;
                    var statmentRefStart = universeBasicV1.PrepareStatementReferenceMasterStart(programFilePath, 42);
                    statmentRefStart.ForEach(s =>
                    {
                        s.SolutionId = solutionId;
                    });
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart)
                            .ConfigureAwait(false);

                    #region Insert into StatementReferenceMaster...

                    var ifStart = ifConditionStart.Find(s => true);
                    var ifEnd = ifConditionEnd.FindAll(s => true);
                    var loopStart = loopIndicatorStart.FindAll(l => true);
                    var loopEnd = loopIndicatorEnd.FindAll(l => true);
                    var callInternal = callInternalIndicationStart.Find(c => true);
                    var callExternal = callExternalIndicationStart.Find(c => true);
                    var methodBusinessName = string.Empty;
                    string businessName = null;

                    programLineList = programLineList.Where(s => !string.IsNullOrEmpty(s)).ToList();
                    programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();
                    // Here check whether file has multiple STOP... 
                    // Find out index position of last stop...
                    // Till that time don't close first method by base command id = 9
                    var stopLinesCount = programLineList.Count(l => l == "STOP" || l == "STOP @#THIS IS LAST STOP");
                    var lastIndexOfStop = 0;

                    if (stopLinesCount == 0)
                    {
                        programLineList.Add("STOP");
                        copyOfFileLines.Add("STOP");
                        stopLinesCount = programLineList.Count(l => l == "STOP");
                    }

                    if (stopLinesCount == 1)
                    {
                        lastIndexOfStop = programLineList.FindLastIndex(s => s == "STOP");
                        goto processFromHere;
                    }

                    var onlyGoSubsList = programLineList.GetOnlyGoSub("GOSUB");
                    int stopCnts = 0;
                    if (!onlyGoSubsList.Any())
                    {
                        var indexPos = -1;
                        foreach (var line in programLineList)
                        {
                            indexPos++;
                            if (line != "STOP") continue;
                            stopCnts++;
                            if (stopCnts != stopLinesCount) continue;

                            lastIndexOfStop = indexPos;
                            break;
                        }
                        goto processFromHere;
                    }

                    if (stopLinesCount > 1)
                    {
                        int sIndex = -1;
                        foreach (var line in programLineList)
                        {
                            sIndex++;
                            if (lastIndexOfStop > 0) break;
                            string thisLine = line.CheckCommentInStatement();
                            foreach (var goSub in onlyGoSubsList)
                            {
                                string methodName = goSub.CheckCommentInStatement();
                                string mName = methodName.Trim();
                                if (thisLine != mName && thisLine != mName + "*" && thisLine != mName + " *") continue;
                                lastIndexOfStop = sIndex - 2;
                                break;
                            }
                        }

                        if (lastIndexOfStop != 0 && programLineList[lastIndexOfStop] != "STOP")
                        {
                            programLineList.Insert(lastIndexOfStop, "STOP");
                            copyOfFileLinesWithComments.Insert(lastIndexOfStop, "STOP");
                        }
                    }

                    processFromHere:
                    Console.WriteLine("==========================================================================");
                    Console.WriteLine("STOP lines count is: " + stopLinesCount + Environment.NewLine +
                                      "Last index of stop is: " + lastIndexOfStop + Environment.NewLine +
                                      "File Path: " + programFilePath.FilePath + Environment.NewLine +
                                      "File Id: " + programFilePath.FileId);
                    Console.WriteLine("=============================================================================");

                    if (stopLinesCount == 0)
                        Console.WriteLine("No Stop...");

                    var indexPosition = -1;
                    foreach (var line in programLineList)
                    {
                        indexPosition = indexPosition + 1;

                        if (string.IsNullOrEmpty(line)) continue;

                        if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                            continue;

                        string lineWithComments = copyOfFileLinesWithComments.Count > indexPosition
                            ? copyOfFileLinesWithComments[indexPosition]
                            : null;
                        string lineComment = !string.IsNullOrEmpty(lineWithComments)
                            ? lineWithComments.GetLineComment()
                            : "";
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster).ConfigureAwait(false);
                            businessName = string.Empty;
                            continue;
                        }

                        if (line == "STOP")
                        {
                            var bCommandId = 0;
                            var pCommandId = 0;

                            if (indexPosition == lastIndexOfStop)
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                BaseCommandId = methodStart.BaseCommandId,
                                PrimaryCommandId = methodStart.PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                BusinessName = methodBusinessName,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (line == "END ELSE" || line == "ELSE")
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (loopStart.Any(l => line.StartsWith(l.StartIndicator)) || line == "LOOP")
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
                                BaseCommandId = line == "LOOP" ? 3 : loopStart.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId = line == "LOOP" ? 62 :
                                    loopStart.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (loopEnd.Any(l => line.StartsWith(l.StartIndicator)))
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
                                BaseCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
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
                                ProjectId = projectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };

                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                        }
                    }

                    #endregion

                    var statmentRefEnd = universeBasicV1.PrepareStatementReferenceMasterEnd(programFilePath, 43);
                    statmentRefEnd.ForEach(s =>
                    {
                        s.SolutionId = solutionId;
                    });
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);

                    var afterRemovingComments = new List<string>();
                    programLineList.ForEach(line =>
                    {
                        string withoutComments = line.CheckCommentInStatement();
                        afterRemovingComments.Add(withoutComments);
                    });
                    int ifCount = afterRemovingComments.Count(s => ifRegEx.IsMatch(s));
                    int endIfCount = afterRemovingComments.Count(s => endIfRegex.IsMatch(s));

                    Console.WriteLine("===========================================================================");
                    Console.WriteLine("File Id: " + fileMaster.FileId + "\nFile Path: " +
                                      fileMaster.FilePath + "\nIF Count: " + ifCount + "\nEND IF Count: " +
                                      endIfCount);
                    Console.WriteLine("========================================================================");
                    bool countMatch = ifCount == endIfCount;
                    if (!countMatch)
                    {
                        Console.WriteLine("============================================================================");
                        Console.WriteLine("IF and END IF count is not matching in this file. Details as: ");
                        Console.WriteLine("File Id: " + fileMaster.FileId + "\nFile Name: " + fileMaster.FileName +
                                          "\nFile Path: " + fileMaster.FilePath);
                        Console.WriteLine("Skipped file for further processing ans marked file status as un-processed.");
                        Console.WriteLine("======================================================================");
                        fileMaster.WorkFlowStatus = "IF and END IF count is not matching";
                        fileMaster.ProjectMaster = null;
                        fileMaster.Processed = 4;
                        fileMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);
                        continue;
                    }

                    // Update file status to processed = 1...
                    fileMaster.Processed = 1;
                    fileMaster.ProjectMaster = null;
                    fileMaster.FileTypeExtensionReference = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster);
                }
                Console.WriteLine("====================================================================================\n");
                Console.WriteLine("All Programs are processed successfully for Project: " + projectDetails.ProjectName);
                Console.WriteLine();
                Console.WriteLine("====================================================================================\n");

                return Ok("All Programs Processed Successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UploadFileMenuDataRevised(ProjectMaster projectMaster)
        {
            string directoryPath = projectMaster.PhysicalPath;
            directoryPath = Path.Combine(directoryPath, "Menu");

            var allFiles = Directory.GetFiles(directoryPath, "*.csv", SearchOption.TopDirectoryOnly);
            var listItems = new List<UniverseFileMenu>();
            using (var appDbContext = new AppDbContext())
            {
                foreach (var file in allFiles)
                {
                    var methodBlockList = File.ReadAllLines(file, Encoding.UTF7).ToList();

                    methodBlockList = methodBlockList.Skip(1).ToList();
                    methodBlockList.RemoveAll(l => l.Length <= 0);
                    var stream = new StreamReader(file);
                    var csvReader = new CsvReader(stream, true);
                    while (csvReader.ReadNextRecord())
                    {
                        listItems.Add(new UniverseFileMenu
                        {
                            ProjectId = projectMaster.ProjectId,
                            UserId = 1,
                            ActionExecuted = csvReader[3],
                            MenuId = csvReader[0],
                            MenuDescription = csvReader[2] ?? "",
                            MenuTitle = csvReader[1],
                            UploadedOn = DateTime.Now
                        });
                    }
                    appDbContext.UniverseFileMenu.AddRange(listItems);
                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                }

                Console.WriteLine("======================================================================================\n");
                Console.WriteLine("File Menu Data uploaded successfully\n");
                Console.WriteLine("======================================================================================\n");

                return Ok("File Menu Data uploaded successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniverseSubRoutinesAndIncludes(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest("Project Details not found for Id: " + projectId);
                if (projectMaster.SolutionId == null) return Ok("All subroutines are processed successfully.");
                int projectSolutionId = projectMaster.SolutionId.Value;
                int languageId = projectMaster.LanguageId;
                var universeBasicV1 = new UniverseBasicVersion1();
                var copyOfFileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId && f.Processed == 0
                    && (f.FileTypeExtensionId == 17 || f.FileTypeExtensionId == 12))
                    .ConfigureAwait(false); // || f.FileTypeExtensionId == 12

                var ifRegEx = new Regex(@"^IF\s", RegexOptions.IgnoreCase);
                var endIfRegex = new Regex(@"^END$|^END\sIF$|^END-IF$", RegexOptions.IgnoreCase);

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
                var methodEnd = methodIndicationEnd.Find(x => true);

                #endregion

                int count = -1;
                foreach (var fileMaster in copyOfFileMaster)
                {
                    count++;
                    if (!File.Exists(fileMaster.FilePath)) continue;

                    var programFileLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                    int solutionId = fileMaster.SolutionId ?? projectSolutionId;

                    programFileLines.RemoveAll(s => s.Length <= 0);
                    programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                    programFileLines = programFileLines.ProcessForStatementsConversion(fileMaster);
                    var copyOfFileLines = programFileLines.ToList();
                    programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                                   && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                                   !s.StartsWith("; *")).ToList();
                    var programLineList = new List<string>();
                    var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                    var methodBlockDictionary = fileMaster.FileTypeExtensionId == 17
                        ? universeBasicV1.GetListFromCertainPoint(copyOfFileLines, lstAllGoSubs, true)
                        : universeBasicV1.GetListFromCertainPointInclude(copyOfFileLines, lstAllGoSubs);
                    var allIncludesFiles = programFileLines.GetAllIncludeStatements("$INSERT", "$INCLUDE");
                    var startedBlock = false;
                    for (var length = 0; length < programFileLines.Count; length++)
                    {
                        var currentLine = programFileLines[length];
                        if (allIncludesFiles.Any(i => i == currentLine)) continue;
                        if (lstAllGoSubs.Any(l => currentLine.StartsWith(l)))
                        {
                            startedBlock = true;
                            var firstOrDefault = currentLine.Split('*').FirstOrDefault();
                            if (firstOrDefault != null)
                                currentLine = firstOrDefault.Trim();
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
                    allIncludesFiles = allIncludesFiles.Distinct().ToList();

                    foreach (var includeStatement in programLineList)
                    {
                        if (allIncludesFiles.Any(i => i == includeStatement))
                            allIncludesFiles.Remove(includeStatement);
                    }

                    if (programLineList.Any() && allIncludesFiles.Any()) programLineList.InsertRange(1, allIncludesFiles);

                    var afterRemovingComments = new List<string>();
                    programLineList.ForEach(line =>
                    {
                        string withoutComments = line.CheckCommentInStatement();
                        if (string.IsNullOrEmpty(withoutComments)) return;
                        afterRemovingComments.Add(withoutComments);
                    });
                    int ifCount = afterRemovingComments.Count(s => ifRegEx.IsMatch(s));
                    int endIfCount = afterRemovingComments.Count(s => endIfRegex.IsMatch(s));

                    Console.WriteLine("=============================================================================");
                    Console.WriteLine("IF Count: " + ifCount);
                    Console.WriteLine("END IF Count: " + endIfCount);
                    Console.WriteLine("File Id: " + fileMaster.FileId);
                    Console.WriteLine("File Name: " + Path.GetFileName(fileMaster.FilePath));
                    Console.WriteLine("File Path: " + fileMaster.FilePath);
                    Console.WriteLine("Total Files Count: " + copyOfFileMaster.Count);
                    Console.WriteLine("Files Remaining: " + (copyOfFileMaster.Count - count));
                    Console.WriteLine("=============================================================================");

                    var statmentRefStart = universeBasicV1.PrepareStatementReferenceMasterStart(fileMaster, 42);
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
                    var copyOfFileLinesWithComments = programLineList.ToList();
                    programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();
                    foreach (var line in programLineList)
                    {
                        indexPosition = indexPosition + 1;

                        if (string.IsNullOrEmpty(line)) continue;

                        if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
                            continue;

                        string lineWithComments = copyOfFileLinesWithComments.Count > indexPosition
                            ? copyOfFileLinesWithComments[indexPosition]
                            : null;
                        string lineComment = !string.IsNullOrEmpty(lineWithComments)
                            ? lineWithComments.GetLineComment()
                            : "";

                        if (line.StartsWith("RETURN"))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
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
                                ProjectId = fileMaster.ProjectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (lstAllGoSubs.Any(a => line.StartsWith(a)))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
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
                                ProjectId = fileMaster.ProjectId,
                                SolutionId = solutionId,
                                BusinessName = methodBusinessName,
                                StatementComment = lineComment
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
                                FileId = fileMaster.FileId,
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
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
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
                                BusinessName = businessName,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 10,
                                PrimaryCommandId = 0,
                                ParsedOrNot = "Y",
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (ifEnd.Any(l => line == l.StartIndicator))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
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
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (loopStart.Any(l => line.StartsWith(l.StartIndicator)) || line == "LOOP")
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
                                ResolvedStatement = line,
                                OriginalStatement = line,
                                ClassCalled = null,
                                MethodName = null,
                                BusinessName = businessName,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = line == "LOOP" ? 3 : loopStart.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId = line == "LOOP" ? 62 : loopStart.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
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
                                BusinessName = businessName,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).BaseCommandId,
                                PrimaryCommandId = loopEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                                ParsedOrNot = "Y",
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
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
                                FileId = fileMaster.FileId,
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
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                            continue;
                        }

                        if (line.StartsWith(callExternal.StartIndicator + " "))
                        {
                            var stmtReferenceMaster = new StatementReferenceMaster
                            {
                                FileId = fileMaster.FileId,
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
                                SolutionId = solutionId,
                                ProjectId = fileMaster.ProjectId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
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
                                BusinessName = businessName,
                                DataOrObjectType = null,
                                MethodCalled = null,
                                VariableNameDeclared = null,
                                BaseCommandId = 0,
                                PrimaryCommandId = 0,
                                ParsedOrNot = "Y",
                                ProjectId = fileMaster.ProjectId,
                                SolutionId = solutionId,
                                StatementComment = lineComment
                            };
                            await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                            businessName = string.Empty;
                        }
                    }

                    #endregion

                    var statmentRefEnd = universeBasicV1.PrepareStatementReferenceMasterEnd(fileMaster, 43);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);

                    fileMaster.FileTypeExtensionReference = null;
                    fileMaster.ProjectMaster = null;
                    // fileMaster.WorkFlowStatus = "Workflow processed successfully";
                    fileMaster.Processed = 1;
                    await _codeVortoService.FileMasterRepository.UpdateItem(fileMaster).ConfigureAwait(false);
                }

                return Ok("All subroutines are processed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> CheckSubRoutinesAndIncludes(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var universeBasicV1 = new UniverseBasicVersion1();

                var ifRegEx = new Regex(@"^IF\s", RegexOptions.IgnoreCase);
                var endIfRegex = new Regex(@"^END$|^END\sIF$|^END-IF$", RegexOptions.IgnoreCase);

                var allSubroutines = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId && f.FileTypeExtensionId == 17)
                    .ConfigureAwait(false);
                foreach (var fileMaster in allSubroutines)
                {
                    var programFileLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                    programFileLines.RemoveAll(s => s.Length <= 0);
                    programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                    programFileLines = programFileLines.ProcessForStatementsConversion(fileMaster);
                    var copyOfFileLines = programFileLines.ToList();
                    programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                                   && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                                   !s.StartsWith("; *")).ToList();
                    var programLineList = new List<string>();
                    var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                    var methodBlockDictionary = fileMaster.FileTypeExtensionId == 17
                        ? universeBasicV1.GetListFromCertainPoint(copyOfFileLines, lstAllGoSubs)
                        : universeBasicV1.GetListFromCertainPointInclude(copyOfFileLines, lstAllGoSubs);

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
                            programLineList.AddRange(methodBlock.Value);
                            length = length + methodBlockOriginal.Count - 1;
                            continue;
                        }
                        if (startedBlock) continue;

                        programLineList.Add(currentLine);
                    }

                    var afterRemovingComments = new List<string>();
                    programLineList.ForEach(line =>
                    {
                        string withoutComments = line.CheckCommentInStatement();
                        if (string.IsNullOrEmpty(withoutComments)) return;
                        afterRemovingComments.Add(withoutComments);
                    });

                    int ifCount = afterRemovingComments.Count(s => ifRegEx.IsMatch(s));
                    int endIfCount = afterRemovingComments.Count(s => endIfRegex.IsMatch(s));

                    Console.WriteLine("=============================================================================");
                    Console.WriteLine("IF Count: " + ifCount);
                    Console.WriteLine("END IF Count: " + endIfCount);
                    Console.WriteLine("File Id: " + fileMaster.FileId);
                    Console.WriteLine("File Name: " + Path.GetFileName(fileMaster.FilePath));
                    Console.WriteLine("File Path: " + fileMaster.FilePath);
                    Console.WriteLine("=============================================================================");
                    programLineList.ForEach(Console.WriteLine);
                    Console.WriteLine("=============================================================================");
                }
                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateRefFileIdForProgramsAndSubRoutines(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                string sqlQuery = " SELECT * FROM StatementReferenceMaster " +
                                  " WHERE BaseCommandId = 6 AND ReferenceFileId = 0 " +
                                  "AND ProjectId = " + projectMaster.ProjectId + " ;";
                var allCallExternals = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery).ConfigureAwait(false);
                var fileMaster = await _codeVortoService.FileMasterRepository.GetAllListItems(f =>
                    f.ProjectId == projectMaster.ProjectId && f.SolutionId == projectMaster.SolutionId &&
                    (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17 || f.FileTypeExtensionId == 12))
                    .ConfigureAwait(false);

                // var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s", RegexOptions.IgnoreCase);
                foreach (var callExternal in allCallExternals)
                {
                    if (!callExternal.OriginalStatement.StartsWith("CALL ")) continue;

                    string oStatement = callExternal.OriginalStatement;
                    string originalStatement = Regex.Replace(oStatement, @"\s+", " ").Trim();
                    try
                    {
                        string programName = originalStatement.ExtractUniverseFileName();

                        /*
                        string thisProgramName = null;
                        if (oStatement.StartsWith("CALL ") && !oStatement.StartsWith("CALL @"))
                            oStatement = oStatement.Replace("CALL ", "CALL @");

                        if (oStatement.ContainsAll("@", "("))
                        {
                            int atIndex = oStatement.IndexOf('@') + 1;
                            string subStrig = oStatement.Substring(atIndex);
                            thisProgramName = subStrig.Split('(').FirstOrDefault() ?? "File-Not-Found";
                        }
                        else
                        {
                            var pgmName = oStatement.Split(' ');
                            if (pgmName.Length > 2) thisProgramName = pgmName[2];
                        }

                        if (string.IsNullOrEmpty(thisProgramName)) continue;
                        string programName = thisProgramName.Trim().TrimEnd(' ').TrimStart(' ');
                        */

                        callExternal.BaseCommandId = 6;
                        var anyFile = fileMaster.Any(f => Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                                          && (f.FileTypeExtensionId == 9 ||
                                                              f.FileTypeExtensionId == 17 ||
                                                              f.FileTypeExtensionId == 12));
                        if (!anyFile)
                        {
                            callExternal.BaseCommandId = 6;
                            callExternal.FileMaster = null;
                            callExternal.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(callExternal)
                                .ConfigureAwait(false);
                            continue;
                        }

                        var pName = fileMaster.Find(f => Path.GetFileNameWithoutExtension(f.FilePath) == programName
                                                         && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17
                                                             || f.FileTypeExtensionId == 12));

                        if (pName == null) continue;

                        if (callExternal.ReferenceFileId == pName.FileId) continue;

                        // Console.WriteLine("Reference File Id is not Correct");
                        var pNameNew = pName.ProjectMaster.ProjectName;
                        var pPathNew = pName.ProjectMaster.PhysicalPath;
                        var className = pName.FilePath.Replace(pPathNew + "\\", "")
                            .Replace(pName.FileName, "").Replace("\\", ".");
                        var fileName = Path.GetFileNameWithoutExtension(pName.FilePath);
                        string classCalled = pNameNew + "." + className + fileName;
                        string methodCalled = Path.GetFileNameWithoutExtension(pName.FilePath) + "()";
                        callExternal.ClassCalled = classCalled;
                        callExternal.MethodCalled = methodCalled;
                        callExternal.FileMaster = null;
                        callExternal.ReferenceFileMaster = null;
                        callExternal.ReferenceFileId = pName.FileId;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(callExternal)
                            .ConfigureAwait(false);
                    }
                    catch (IndexOutOfRangeException indexOutOfRangeException)
                    {
                        Console.WriteLine(indexOutOfRangeException);
                        return InternalServerError(indexOutOfRangeException);
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine(exception);
                        return InternalServerError(exception);
                    }
                }
                return Ok("All Call Externals are processed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateRefFileIdForIncludes(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => (f.SolutionId == projectMaster.SolutionId &&
                                          f.ProjectId == projectMaster.ProjectId) && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);
                var filesToProcess = allFiles.Where(f => f.FileTypeExtensionId == 12
                                                        || f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17).ToList();
                foreach (var fileMaster in filesToProcess)
                {
                    var allIncludeStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == fileMaster.FileId && s.ProjectId == projectMaster.ProjectId
                                              && (s.OriginalStatement.StartsWith("$INSERT") ||
                                                  s.OriginalStatement.StartsWith("$INCLUDE"))).ConfigureAwait(false);

                    foreach (var includeStatement in allIncludeStatements)
                    {
                        // string includeFileName = includeStatement.OriginalStatement.Split('>').Last();
                        string includeFileName = includeStatement.OriginalStatement.ExtractIncludeFileName();
                        if (string.IsNullOrEmpty(includeFileName)) continue;

                        var includeFile = (from f in allFiles
                                           let fileName = Path.GetFileNameWithoutExtension(f.FilePath)
                                           where fileName == includeFileName && f.FileTypeExtensionId == 12
                                           select f).ToList();
                        if (!includeFile.Any())
                        {
                            includeStatement.ReferenceFileId = 0;
                            includeStatement.FileMaster = null;
                            includeStatement.ReferenceFileMaster = null;
                            includeStatement.BaseCommandId = 6;

                            await _codeVortoService.StatementReferenceMasterRepository
                                .UpdateItem(includeStatement).ConfigureAwait(false);
                            continue;
                        }

                        // if (includeFile.Any()) continue;
                        foreach (var includeFileMaster in includeFile)
                        {
                            if (includeStatement.ReferenceFileId != 0) continue;
                            includeStatement.ReferenceFileId = includeFileMaster.FileId;
                            includeStatement.FileMaster = null;
                            includeStatement.ReferenceFileMaster = null;
                            includeStatement.BaseCommandId = 6;

                            var pNameNew = includeFileMaster.ProjectMaster.ProjectName;
                            var pPathNew = includeFileMaster.ProjectMaster.PhysicalPath;
                            var classNameProgram = includeFileMaster.FilePath.Replace(pPathNew + "\\", "")
                                .Replace(includeFileMaster.FileName, "").Replace("\\", ".");
                            var fileNameProgram = Path.GetFileNameWithoutExtension(includeFileMaster.FilePath);
                            var classNameDeclared = pNameNew + "." + classNameProgram + fileNameProgram;

                            includeStatement.ClassCalled = classNameDeclared;
                            string fileMethodName = Path.GetFileNameWithoutExtension(includeFileMaster.FilePath) + "()";
                            includeStatement.MethodCalled = fileMethodName;

                            await _codeVortoService.StatementReferenceMasterRepository
                                .UpdateItem(includeStatement).ConfigureAwait(false);
                        }
                    }
                }
                return Ok("Reference File Id updated successfully for Include Files of Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBusinessNamesForMethods(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId
                    && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17)).ConfigureAwait(false);
                foreach (var fileMaster in allFiles)
                {
                    var fileStatements = File.ReadAllLines(fileMaster.FilePath).ToList();
                    fileStatements.RemoveAll(s => s.Length <= 0);
                    fileStatements = fileStatements.Select(s => s.Trim()).ToList();

                    var master = fileMaster;
                    var allMethods = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == master.FileId && s.ProjectId == projectMaster.ProjectId && s.BaseCommandId == 8)
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
                        int methodIndex = fileStatements.IndexOf(originalStatement);
                        if (methodIndex <= 0) methodIndex = fileStatements.IndexOf(originalStatement + "*");
                        if (methodIndex <= 0) continue;
                        var methodComments = string.Empty;

                        var goupTo4Lines = methodIndex - 4;
                        for (var length = methodIndex; length >= 0; length--)
                        {
                            var line = fileStatements[length];
                            if (length == goupTo4Lines)
                            {
                                if (methodComments == string.Empty) methodComments = null;
                                break;
                            }
                            if (!line.StartsWith("* ")) continue;
                            methodComments = line.TrimStart('*').Trim();
                            break;
                        }
                        if (stateRef.BusinessName == methodComments) continue;
                        if (string.IsNullOrEmpty(methodComments)) continue;
                        stateRef.BusinessName = methodComments;
                        stateRef.FileMaster = null;
                        stateRef.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef).ConfigureAwait(false);
                    }
                }
                return Ok("Business names are updated successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBusinessNameForBaseCommandId19(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                          f.SolutionId == projectMaster.SolutionId
                                          && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17 ||
                                              f.FileTypeExtensionId == 10 || f.FileTypeExtensionId == 12))
                    .ConfigureAwait(false);
                foreach (var fileMaster in allFiles)
                {
                    var fileStatements = File.ReadAllLines(fileMaster.FilePath).ToList();
                    fileStatements.RemoveAll(s => s.Length <= 0);
                    fileStatements = fileStatements.Select(s => s.Trim()).ToList();
                    if (!fileStatements.Any()) fileStatements.Add(Path.GetFileNameWithoutExtension(fileMaster.FilePath));
                    var master = fileMaster;
                    var allMethods = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == master.FileId && s.ProjectId == master.ProjectId
                                              && s.BaseCommandId == 19 && string.IsNullOrEmpty(s.BusinessName))
                        .ConfigureAwait(false);
                    foreach (var stateRef in allMethods)
                    {
                        var fileBusinessName = fileStatements.First().Trim('*').StartsWith("PA ")
                            ? fileStatements.First().Trim('*').Replace("PA ", "").Trim()
                            : fileStatements.First().Trim('*').Trim();
                        stateRef.BusinessName = fileBusinessName;
                        stateRef.FileMaster = null;
                        stateRef.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef)
                            .ConfigureAwait(false);
                    }
                }
                return Ok("Business names are updated successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBusinessNamesForIncludes(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.SolutionId == projectMaster.SolutionId
                    && f.FileTypeExtensionId == 12).ConfigureAwait(false);
                foreach (var fileMaster in allFiles)
                {
                    var fileStatements = File.ReadAllLines(fileMaster.FilePath).ToList();
                    fileStatements.RemoveAll(s => s.Length <= 0);
                    fileStatements = fileStatements.Select(s => s.Trim()).ToList();

                    var master = fileMaster;
                    var allMethods = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.FileId == master.FileId && s.ProjectId == master.ProjectId && (s.BaseCommandId == 8 || s.BaseCommandId == 19)).ConfigureAwait(false);
                    string fileName = Path.GetFileNameWithoutExtension(master.FilePath) + "()";
                    foreach (var stateRef in allMethods)
                    {
                        if (stateRef.BaseCommandId == 19)
                        {
                            string fileBusinessName = fileStatements.First().StartsWith("PA ")
                                ? fileStatements.First().Replace("PA ", "").Trim().TrimEnd('-').Trim()
                                : fileStatements.First().Trim() == "PA"
                                    ? ""
                                    : fileStatements.First().Trim('*').TrimEnd('-').Trim();
                            stateRef.BusinessName = fileBusinessName;
                            stateRef.FileMaster = null;
                            stateRef.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef).ConfigureAwait(false);
                            continue;
                        }

                        if (stateRef.MethodName == fileName)
                        {
                            string fileBusinessName = fileStatements.First().Trim('*').Replace("PA ", "").Trim();
                            stateRef.BusinessName = fileBusinessName;
                            stateRef.FileMaster = null;
                            stateRef.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stateRef)
                                .ConfigureAwait(false);
                            continue;
                        }
                        string originalStatement = stateRef.OriginalStatement.Trim();
                        int methodIndex = fileStatements.IndexOf(originalStatement);
                        if (methodIndex <= 0) methodIndex = fileStatements.IndexOf(originalStatement + "*");
                        if (methodIndex <= 0) continue;
                        var methodComments = string.Empty;

                        var goupTo4Lines = methodIndex - 4;
                        for (var length = methodIndex; length >= 0; length--)
                        {
                            var line = fileStatements[length];
                            if (length == goupTo4Lines)
                            {
                                if (methodComments == string.Empty) methodComments = null;
                                break;
                            }
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
                return Ok("Business names are updated successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UploadDataDictionary(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                string directoryPath = projectMaster.PhysicalPath;
                const string dashLine = "======================================================\n";
                var configuration = new Configuration
                {
                    IgnoreQuotes = true,
                    Delimiter = ",",
                    HasHeaderRecord = true,
                    IncludePrivateMembers = true,
                    CultureInfo = new CultureInfo("en-us"),
                    HeaderValidated = (a, b, c, r) => { },
                    MissingFieldFound = null
                };
                var allFiles = Directory.GetFiles(directoryPath, "*.txt", SearchOption.AllDirectories);
                stringBuilder.AppendLine(dashLine).AppendLine("Total Data Dictionary Files: " + allFiles.Length);
                int loopCount = -1;
                foreach (var file in allFiles)
                {
                    loopCount++;
                    Console.WriteLine(dashLine);
                    Console.WriteLine("Total Data Dictionary Files: " + allFiles.Length);
                    Console.WriteLine("Total Data Dictionary Remaining Files: " + (allFiles.Length - loopCount));
                    Console.WriteLine(dashLine);

                    var fileName = Path.GetFileNameWithoutExtension(file);
                    if (!File.Exists(file)) continue;

                    var methodBlockList = File.ReadAllLines(file).ToList();
                    if (!methodBlockList.Skip(1).Any())
                    {
                        var dataDictionary = new DataDictionary
                        {
                            FileName = fileName,
                            FieldNo = fileName,
                            Description = "",
                            FieldLabel = "",
                            RptFieldLength = "",
                            TypeOfData = "",
                            SingleArray = "",
                            DateOfCapture = "",
                            ReplacementName = "",
                            ProjectId = projectMaster.ProjectId
                        };
                        Console.WriteLine("==========================================\n\n\n");
                        Console.WriteLine(JsonConvert.SerializeObject(dataDictionary));

                        await _codeVortoService.DataDictionaryRepository.AddNewItem(dataDictionary)
                            .ConfigureAwait(false);
                        continue;
                    }

                    var fileLinesList = File.ReadAllText(file);
                    byte[] byteArray = Encoding.UTF8.GetBytes(fileLinesList);
                    var memoryStream = new MemoryStream(byteArray);
                    var streamReader = new StreamReader(memoryStream);
                    var csvReader = new CsvHelper.CsvReader(streamReader, configuration, true);
                    csvReader.Configuration.PrepareHeaderForMatch = h => h.ToUpper();
                    csvReader.Configuration.MemberTypes = MemberTypes.Fields | MemberTypes.Properties;
                    csvReader.Configuration.RegisterClassMap<DataDictMap>();

                    var indexPosition = -1;
                    while (csvReader.Read())
                    {
                        var readHeader = csvReader.ReadHeader();
                        Console.WriteLine(readHeader);
                        var rawRecords = csvReader.GetRecords<DataDict>();
                        foreach (var dataDict in rawRecords)
                        {
                            indexPosition++;
                            var dataDictionary = new DataDictionary
                            {
                                FileName = string.IsNullOrEmpty(dataDict.FileName)
                                    ? ""
                                    : dataDict.FileName.Replace("\"", ""),
                                FieldNo = string.IsNullOrEmpty(dataDict.FieldNo)
                                    ? ""
                                    : dataDict.FieldNo.Replace("\"", ""),
                                Description = string.IsNullOrEmpty(dataDict.Description)
                                    ? ""
                                    : dataDict.Description.Replace("\"", ""),
                                FieldLabel = string.IsNullOrEmpty(dataDict.FieldLabel)
                                    ? ""
                                    : dataDict.FieldLabel.Replace("\"", ""),
                                RptFieldLength = string.IsNullOrEmpty(dataDict.RptFieldLength)
                                    ? ""
                                    : dataDict.RptFieldLength.Replace("\"", ""),
                                TypeOfData = string.IsNullOrEmpty(dataDict.TypeOfData)
                                    ? ""
                                    : dataDict.TypeOfData.Replace("\"", ""),
                                SingleArray = string.IsNullOrEmpty(dataDict.SingleArray)
                                    ? ""
                                    : dataDict.SingleArray.Replace("\"", ""),
                                DateOfCapture = string.IsNullOrEmpty(dataDict.DateOfCapture)
                                    ? ""
                                    : dataDict.DateOfCapture.Replace("\"", ""),
                                ReplacementName = "",
                                ProjectId = projectMaster.ProjectId
                            };

                            var isValidNumber = Regex.IsMatch(dataDictionary.FieldNo, @"^[0-9]+(\.[0-9]+)?$");
                            var replacementName = isValidNumber
                                ? "R." + dataDictionary.FileName + "(" + dataDictionary.FieldNo + ")"
                                : "K." + dataDictionary.FileName;
                            if (indexPosition == 0) replacementName = dataDictionary.FileName;
                            dataDictionary.ReplacementName = replacementName;

                            Console.WriteLine("=======================================\n\n");
                            Console.WriteLine(JsonConvert.SerializeObject(dataDictionary));
                            Console.WriteLine("=======================================\n\n");
                            await _codeVortoService.DataDictionaryRepository.AddNewItem(dataDictionary)
                                .ConfigureAwait(false);
                        }
                    }
                    memoryStream.Flush();
                    memoryStream.Dispose();
                    streamReader.Close();
                }
                Console.WriteLine("====================================================================================\n");
                Console.WriteLine(
                    "Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
                Console.WriteLine();
                Console.WriteLine("====================================================================================\n");
                stringBuilder.Append(dashLine);
                stringBuilder.AppendLine("Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
                LogMessage.WriteLogMessage(stringBuilder);

                return Ok("Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
            }
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
    }
}
