using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Net.Http;
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
    public partial class UniverseBasicFinalVersionController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public UniverseBasicFinalVersionController()
        {

        }

        public UniverseBasicFinalVersionController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<HttpResponseMessage> StartProcessUbProject(int projectId)
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
                stringBuilder.AppendLine(
    "========================================================================================");
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
                            .Where(s => strExtensions.Any(s.EndsWith));
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
                            var extensionId = fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                            if (allPreviousFiles.Any(k => k.FileName == fileName && k.FilePath == currentFile))
                            {
                                var thisFile = allPreviousFiles.Find(
                                    k => k.FileName == fileName && k.FilePath == currentFile);
                                hasModified = true;
                                thisFile.IsNewVersion = 1;
                                thisFile.Processed = 0;
                                thisFile.DoneParsing = 0;
                                thisFile.ProjectMaster = null;
                                thisFile.LinesCount = lineCount;
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
                                Processed = 0
                            };
                            lstFileMasters.Add(fileMaster);
                        }

                        enumerator.Dispose();
                        await _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
                        stringBuilder.AppendLine("========================================================================================");
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
                            Content = new StringContent(
                                "Files extracted for Common Library type project for Universe Basic is completed.")
                        };
                }
                /*
                stringBuilder.AppendLine("Started processing for Parsing. " +
                                         "Function is: StartParsingProcessUniverseBasic(" + projectId + ");");

                await StartParsingProcessUniverseBasic(projectId).ConfigureAwait(false);

                stringBuilder.AppendLine(
                    "Completed Parsing process. Function was: StartParsingProcessUniverseBasic(" + projectId + ");");

                stringBuilder.AppendLine("Started processing for ActionWorkFlowDetails. " +
                                         "Function is ProcessForActionWorkflowDetailsUniverseOnly(" + projectId + ");");

                // await ProcessForActionWorkflowDetailsUniverseOnly(projectId).ConfigureAwait(false);

                stringBuilder.AppendLine("Completed process for ActionWorkFlowDetails. " +
                                         "Function was ProcessForActionWorkflowDetailsUniverseOnly(" + projectId + ");");

                stringBuilder.AppendLine("Starting process for Updating second tabs data tables Alternate names." +
                                         " Function is UpdateSecondTabAlternateNames(" + projectId + ");");

                var responseMessage = await UpdateSecondTabAlternateNames(projectId);

                stringBuilder.AppendLine("Completed process of Updating second tabs data tables Alternate names." +
                                        " Function was UpdateSecondTabAlternateNames(" + projectId + ");");
                 LogMessage.WriteLogMessage(stringBuilder);

                return responseMessage;

                */
                return new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent("Done") };
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForActionWorkflowDetailsUniverseOnlyOld(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlWorkflows = " SELECT * FROM ActionWorkflows WHERE ProjectId = " + projectId + "; ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(sqlWorkflows).ConfigureAwait(false);
                var sqlQueery = "Select * from WorkflowTreeviewTabFirstDetails Where ProjectId = " + projectId;
                var sqlAllStatements =
                    " SELECT sm.* FROM statementreferencemaster AS sm INNER JOIN ActionWorkflows AS aw " +
                    " ON aw.MethodStatementId = sm.StatementId WHERE sm.ProjectId = " + projectId + " " +
                    " GROUP BY sm.FileId;";
                var statementRefernce = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllStatements).ConfigureAwait(false);
                var workflowTreeviewTabFirstDetails = await _codeVortoService
                    .WorkflowTreeviewTabFirstDetailsRepository
                    .GetDataFromSqlQuery<WorkflowTreeviewTabFirstDetails>(sqlQueery).ConfigureAwait(false);

                var enable = workflowRef.Count(x => x.IsDeleted == 0);
                var hidden = workflowRef.Count(x => x.IsDeleted == 1);
                var count = "<b style='color: #7030a0;'>" + " (Total Workflows: " + workflowRef.Count +
                            " | Enabled: " + enable + " | Hidden: " + hidden + ")" + "</b>";
                foreach (var workflow in workflowRef)
                {
                    var objConnect = "btnObj_" + workflow.ActionWorkflowId;
                    var download = "btnDowload_" + workflow.ActionWorkflowId;
                    var fileId = (from s in statementRefernce
                                  where s.StatementId == workflow.MethodStatementId
                                  select s.FileId).FirstOrDefault();
                    var callExternal = workflowTreeviewTabFirstDetails
                        .Count(d => d.BaseCommandId == 6 && d.ActionWorkflowId == workflow.ActionWorkflowId);
                    var callInternal = workflowTreeviewTabFirstDetails
                        .Count(d => d.BaseCommandId == 5 && d.ActionWorkflowId == workflow.ActionWorkflowId);

                    var decisionCount = workflowTreeviewTabFirstDetails
                                            .Count(d => d.BaseCommandId == 1 &&
                                                        d.ActionWorkflowId == workflow.ActionWorkflowId) + 1;

                    string disabled = string.Empty;
                    string btnStyle = "style='margin-top :5px;height: 31px;'";
                    string pageUrl = "workflow_workspace.html?prjId=" + projectId + "&stId=" +
                                     workflow.MethodStatementId + "&aId=" + workflow.ActionWorkflowId + "";

                    var disable = "<button id=" + workflow.ActionWorkflowId +
                                  " style='margin-top: 5px;' class='btn btn-info btn-icon icon-lg fa fa-trash' onclick='workFlowDisable(" +
                                  workflow.ActionWorkflowId + ")'></button>";
                    var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                               "<button " + btnStyle + " " + disabled +
                               " class='btn btn-mint'>View</button> </a>" +
                               " &nbsp;<a href='#'><button style='margin-top : 5px;height: 31px;' class='btn btn-mint'>Rename</button> </a>&nbsp;<button id=" +
                               workflow.ActionWorkflowId + " style='margin-top : 4px;' " +
                               "class='btn btn-mint btn-icon icon-lg fa fa-upload' title='Upload image/file(s)'></button>&nbsp;" +
                               "<button id=" + download +
                               " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-primary btn-icon icon-lg fa fa-download' title='Download Requirement Specification Document(.docx)' ></button>&nbsp;" +
                               "<button id=" + objConnect +
                               " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-success btn-icon icon-lg fa fa-sitemap' onclick='downloadObjectConnectivityIndividualFlowchartUniverse(" +
                               fileId + ", " + workflow.ActionWorkflowId +
                               ");' title='Download object connectivity.'></button>";

                    var workflowDetails = new ActionWorkflowDetails
                    {
                        DecisionCount = decisionCount,
                        ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                        InternalCalls = callInternal > 0 ? "Yes" : "No",
                        View = view,
                        OriginObject = workflow.WorkflowName.ToUpper(),
                        WorkflowName = Path.GetFileNameWithoutExtension(workflow.OriginFilePath),
                        ProjectName = workflow.ProjectMaster.ProjectName,
                        ActionWorkflowId = workflow.ActionWorkflowId,
                        Disable = disable,
                        IsDeleted = workflow.IsDeleted,
                        ShortDetails = count
                    };
                    await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails).ConfigureAwait(false);
                }
                return Ok("Action Workflow Details process completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForActionWorkflowDetailsUniverseOnly(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                int solutionId = projectDetails.SolutionId ?? 0;
                string sqlWorkflows = " SELECT * FROM ActionWorkflows WHERE ProjectId = " + projectId + "; ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(sqlWorkflows).ConfigureAwait(false);
                var sqlQueery = "Select * from workflowtreeviewsecondtabdetails Where ProjectId = " + projectId;
                /*
                var sqlAllStatements =
                    " SELECT sm.* FROM statementreferencemaster AS sm INNER JOIN ActionWorkflows AS aw " +
                    " ON aw.MethodStatementId = sm.StatementId WHERE sm.ProjectId = " + projectId + " " +
                    " GROUP BY sm.FileId;";
                var statementRefernce = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllStatements).ConfigureAwait(false);
                */
                var workflowTreeviewTabSecondDetails = await _codeVortoService
                    .WorkflowTreeviewSecondTabDetailsRepository
                    .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(sqlQueery).ConfigureAwait(false);

                var enable = workflowRef.Count(x => x.IsDeleted == 0);
                var hidden = workflowRef.Count(x => x.IsDeleted == 1);
                var count = "<b style='color: #7030a0;'>" + " (Total Workflows: " + workflowRef.Count +
                            " | Enabled: " + enable + " | Hidden: " + hidden + ")" + "</b>";
                int parentRowId = 0;
                var fileMaster =
                    await
                        _codeVortoService.FileMasterRepository.GetAllListItems(x => x.SolutionId == solutionId)
                            .ConfigureAwait(false);
                foreach (var workflow in workflowRef)
                {
                    parentRowId++;
                    var graphId = "graph_" + parentRowId;
                    var objConnect = "btnObj_" + workflow.ActionWorkflowId;
                    var download = "btnDowload_" + workflow.ActionWorkflowId;
                    var fileId = workflow.FileId;
                    /*
                    var fileId = (from s in statementRefernce
                                  where s.StatementId == workflow.MethodStatementId
                                  select s.FileId).FirstOrDefault();
                    */
                    var callExternal = workflowTreeviewTabSecondDetails
                        .Count(d => d.BaseCommandId == 6 && d.ActionWorkflowId == workflow.ActionWorkflowId);
                    var callInternal = workflowTreeviewTabSecondDetails
                        .Count(d => d.BaseCommandId == 5 && d.ActionWorkflowId == workflow.ActionWorkflowId);

                    var decisionCount = workflowTreeviewTabSecondDetails
                                            .Count(d => d.BaseCommandId == 1 &&
                                                        d.ActionWorkflowId == workflow.ActionWorkflowId) + 1;

                    string disabled = string.Empty;
                    string btnStyle = "style='margin-top :5px;height: 31px;'";
                    string pageUrl = "workflow_workspace.html?prjId=" + projectId + "&stId=" +
                                     workflow.MethodStatementId + "&aId=" + workflow.ActionWorkflowId + "";

                    string onClickUploadFile = "onclick=uploadFilePopupShow('" + workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") + "'," + workflow.ActionWorkflowId + ");";
                    string onClickRename = "onclick=funWorkflowRename('" + workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") + "'," + workflow.ActionWorkflowId + ");";
                    var disable = "<button id=" + workflow.ActionWorkflowId +
                                  " style='margin-top: 5px;' class='btn btn-info btn-icon icon-lg fa fa-trash' onclick='workFlowDisable(" +
                                  workflow.ActionWorkflowId + ")'></button>";
                    var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                               "<button " + btnStyle + " " + disabled +
                               " class='btn btn-mint'>View</button> </a>" +
                               " &nbsp;<a href='#'><button style='margin-top : 5px;height: 31px;' class='btn btn-mint' " + onClickRename + " >Rename</button> </a>&nbsp;<button id=" +
                               workflow.ActionWorkflowId + " style='margin-top : 4px;' " +
                               "class='btn btn-mint btn-icon icon-lg fa fa-upload' " + onClickUploadFile + " title='Upload image/file(s)'></button>&nbsp;" +
                               "<button id=" + download +
                               " style='margin-bottom: -8px; margin-top: -3px;height: 31px;'" +
                               " class='btn btn-primary btn-icon icon-lg fa fa-download'  onclick=downloadRequirementDoc(" + workflow.ActionWorkflowId + ");" +
                               "title='Download Requirement Specification Document(.docx)' ></button>&nbsp;" +
                               "<button id=" + objConnect +
                               " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-success btn-icon icon-lg fa fa-sitemap' " +
                               "onclick='downloadObjectConnectivityIndividualFlowchartUniverse(" +
                               fileId + ", " + workflow.ActionWorkflowId +
                               ");' title='Download object connectivity.'></button>";

                    var workflowDetails = new ActionWorkflowDetails
                    {
                        DecisionCount = decisionCount,
                        ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                        InternalCalls = callInternal > 0 ? "Yes" : "No",
                        View = view,
                        OriginObject = workflow.OriginObject,
                        WorkflowName = workflow.WorkflowName.ToUpper(),
                        ProjectName = workflow.ProjectMaster.ProjectName,
                        ActionWorkflowId = workflow.ActionWorkflowId,
                        Disable = disable,
                        IsDeleted = workflow.IsDeleted,
                        ShortDetails = count,
                        GraphId = graphId,
                        ParentId = "-1",
                        ProjectId = projectId
                    };
                    await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails).ConfigureAwait(false);
                    var secondTabDetails =
                   (from s in workflowTreeviewTabSecondDetails
                    where s.ActionWorkflowId == workflow.ActionWorkflowId && s.BaseCommandId == 6
                    select s).ToList();
                    foreach (var sTab in secondTabDetails)
                    {
                        int programId = sTab.ProgramId ?? 0;
                        if (programId == 0) continue;
                        await ChildProcessForActionWorkflowDetailsUniverseOnlyTesting(projectId, fileMaster, workflowRef,
                            workflowTreeviewTabSecondDetails, sTab, graphId);
                    }
                }
                return Ok("Action Workflow Details process completed successfully.");
            }
        }

        public async Task<IHttpActionResult> ChildProcessForActionWorkflowDetailsUniverseOnlyTesting(int projectId,
            List<FileMaster> fileMaster, List<ActionWorkflows> actionWorkflows, List<WorkflowTreeviewSecondTabDetails> workflowTreeviewTabSecondDetails, WorkflowTreeviewSecondTabDetails sTab, string parentRowId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var actionWorkFlows = (from a in actionWorkflows
                                           where a.ProjectId == projectId && a.OriginObject == sTab.ClassCalled && a.FileId == sTab.ProgramId
                                           select a).ToList();
                    var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    var projectName = projectDetails.ProjectName;
                    var programFile =
                    (from f in fileMaster
                     where f.FileId == sTab.ProgramId && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17)
                     select f).ToList();
                    if (programFile.Any())
                    {
                        string disabled = string.Empty;
                        string btnStyle = "style='margin-top :5px;height: 31px;'";
                        string pageUrl = "customview.html?prjId=" + projectId + "&fileId=" + sTab.ProgramId + "";
                        var view = "<a " + disabled + " href=javascript:window.open('" + pageUrl + "')>" +
                                   "<button " + btnStyle + " " + disabled +
                                   " class='btn btn-mint'>View</button>";
                        var workflowDetails = new ActionWorkflowDetails
                        {
                            DecisionCount = 0,
                            ExtrernalCalls = "No",
                            InternalCalls = "No",
                            View = view,
                            OriginObject = sTab.ClassCalled,
                            WorkflowName = sTab.MethodCalled.Replace(")", "").Replace("(", ""),
                            ProjectName = projectName,
                            ActionWorkflowId = 0,
                            ShortDetails = "0",
                            ParentId = parentRowId,
                            GraphId = "graphId_" + sTab.ProgramId,
                            ProjectId = projectId
                        };
                        await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails).ConfigureAwait(false);
                    }
                    int gId = 0;
                    foreach (var workflow in actionWorkFlows)
                    {
                        // parentRowId++;
                        var graphId = "graph_" + gId + "_" + workflow.ActionWorkflowId;
                        var callExternal = workflowTreeviewTabSecondDetails
                            .Count(d => d.BaseCommandId == 6 && d.ActionWorkflowId == workflow.ActionWorkflowId);
                        var callInternal = workflowTreeviewTabSecondDetails
                            .Count(d => d.BaseCommandId == 5 && d.ActionWorkflowId == workflow.ActionWorkflowId);

                        var decisionCount = workflowTreeviewTabSecondDetails
                                                .Count(d => d.BaseCommandId == 1 &&
                                                            d.ActionWorkflowId == workflow.ActionWorkflowId) + 1;
                        string disabled = string.Empty;
                        string btnStyle = "style='margin-top :5px;height: 31px;'";
                        string pageUrl = "customview.html?prjId=" + projectId + "&fileId=" + workflow.FileId + "";
                        var view = "<a " + disabled + " href=javascript:window.open('" + pageUrl + "')>" +
                                   "<button " + btnStyle + " " + disabled +
                                   " class='btn btn-mint'>View</button>";
                        var workflowDetails = new ActionWorkflowDetails
                        {
                            DecisionCount = decisionCount,
                            ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                            InternalCalls = callInternal > 0 ? "Yes" : "No",
                            View = view,
                            OriginObject = workflow.OriginObject,
                            WorkflowName = workflow.WorkflowName.ToUpper(),
                            ProjectName = workflow.ProjectMaster.ProjectName,
                            ActionWorkflowId = workflow.ActionWorkflowId,
                            Disable = disabled,
                            IsDeleted = workflow.IsDeleted,
                            ShortDetails = "0",
                            ParentId = parentRowId,
                            GraphId = graphId
                        };
                        await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails).ConfigureAwait(false);
                        var secondTabDetails =
                       (from s in workflowTreeviewTabSecondDetails
                        where s.ActionWorkflowId == workflow.ActionWorkflowId && s.BaseCommandId == 6
                        select s).ToList();
                        foreach (var secondTab in secondTabDetails)
                        {
                            int pId = secondTab.ProgramId ?? 0;
                            if (pId == 0) continue;
                            await
                                 ChildProcessForActionWorkflowDetailsUniverseOnlyTesting(projectId, fileMaster, actionWorkflows, workflowTreeviewTabSecondDetails, secondTab, graphId);
                        }
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }

            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> OtherWorkflowsFromMenuNames(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allRevisedMenus = await _codeVortoService.AppDbContextRepository.UniverseFileMenu.ToListAsync()
                    .ConfigureAwait(false);

                var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(p => p.ProjectId == projectId)
                    .ConfigureAwait(false);

                foreach (var jclMenu in allRevisedMenus)
                {
                    string actionExecuted = jclMenu.ActionExecuted;
                    if (string.IsNullOrEmpty(actionExecuted)) continue;

                    var nameList = actionExecuted.Split(' ')[0];
                    var allActions = (from a in allActionWorkflows
                                      let fileName = Path.GetFileNameWithoutExtension(a.OriginFilePath)
                                      where fileName == nameList
                                      select a).ToList();
                    if (allActions.Any()) continue;
                    var actionWorkflowsCopy = new ActionWorkflows
                    {
                        ProjectId = projectId,
                        IsDeleted = 0,
                        OriginFilePath = actionExecuted,
                        ProjectMaster = null,
                        FileMaster = null,
                        CreatedBy = 1,
                        CreatedOn = DateTime.Now,
                        EndPointOrService = "Batch",
                        FileId = 0,
                        MethodStatementId = 0,
                        OriginEventMethod = "",
                        OriginFileName = actionExecuted,
                        OriginObject = actionExecuted,
                        Processed = 1,
                        WorkflowName = jclMenu.ActionExecuted
                    };
                    await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflowsCopy)
                        .ConfigureAwait(false);
                }
                var allNewWorkflows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);

                return Ok(allNewWorkflows);
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
                int solutionId = projectDetails.SolutionId ?? 0;
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId).ConfigureAwait(false);

                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                var copyOfFileMasterNew = copyOfFileMaster
                    .Where(f => (f.FileTypeExtensionId == 10 || f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17)
                                && f.ProjectId == projectId
                                && f.Processed == 0).ToList();
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started executing next process: ParseUniverseBasicFiles for project: " + projectId + "");
                foreach (var copyFile in copyOfFileMasterNew)
                {
                    await ParseUniverseBasicFiles(copyFile.ProjectId).ConfigureAwait(false);
                }

                #region Update field for basecommandId = 45

                stringBuilder.AppendLine(
    "========================================================================================");
                stringBuilder.AppendLine("Started update field for basecommandId = 45 for solution: " + solutionId + "");

                var sqlQuery = "Select FileName, RowId, FieldNo, Description, FieldLabel, RptFieldLength, TypeofData, " +
                               " SingleArray, DateOfCapture, ReplacementName FROM UniverseBasicDataDictionary " +
                               " WHERE ProjectId =" + projectId + " GROUP BY FileName;";
                var universeBasicDataDicyionary =
                    await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);
                // var dByGroup = universeBasicDataDicyionary.GroupBy(d => d.FileName).ToList();

                foreach (var dataDictionary in universeBasicDataDicyionary)
                {
                    string tableName = dataDictionary.Description;

                    string fileName = dataDictionary.FileName;
                    string sqlQry = "SELECT * from statementreferencemaster WHERE projectId = " + projectId +
                                    " AND OriginalStatement like '% " + fileName + "%';";
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
                stringBuilder.AppendLine("Ended update field for basecommandId = 45 for solution: " + solutionId + "");

                #endregion

                #region Insert DataDependancy data

                await InsertDataDependancyForUniverse(projectId);

                #endregion

                #region Insert data into DbCrudActivity

                await InsertCrudActivityForUniverse(projectId);
                #endregion

                stringBuilder.AppendLine(
    "========================================================================================");
                stringBuilder.AppendLine("Started executing next process: StartWorkflowProcess for solution: " + solutionId + "");
                LogMessage.WriteLogMessage(stringBuilder);
                await StartWorkflowProcess(solutionId, projectId).ConfigureAwait(false);
                return Ok("All files are processed successfully for Project Id: " + projectId);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseUniverseBasicFiles(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                // int commanClassProjId = 9;
                #region Load Project and Base Command Reference Details...

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                int solutionId = Convert.ToInt32(projectDetails.SolutionId);
                int languageId = projectDetails.LanguageId;
                // FileTypeExtensionId == 10 is for jcl files which is starting point of Universe Basics...
                var copyOfFileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.SolutionId == solutionId);

                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Total file parsing count: " + copyOfFileMaster.Count());

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

                #endregion

                // This is common for all statement that is ClassNameDeclared
                // var pPath = projectDetails.PhysicalPath;
                var currentJclFile = new FileMaster();

                var ifRegEx = new Regex(@"^IF\s", RegexOptions.IgnoreCase);
                var endIfRegex = new Regex(@"^END$|^END\sIF$|^END-IF$", RegexOptions.IgnoreCase);

                #region Jcl file processing...

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();

                foreach (var fMaster in copyOfFileMaster
                    .Where(f => f.FileTypeExtensionId == 10 && f.Processed == 0 && f.ProjectId == projectId))
                {
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started reading file lines for file name: " + fMaster.FileName +
                                               " and file path is: " + fMaster.FilePath);
                    var lstFileLines = File.ReadAllLines(fMaster.FilePath).ToList();
                    if (lstFileLines.Count <= 0) return Ok("No contents in file: " + fMaster.FilePath);
                    lstFileLines.RemoveAll(s => s.Length <= 0);
                    lstFileLines = lstFileLines.Select(s => s.Trim()).ToList();
                    lstFileLines = lstFileLines.CombineAllBrokenLines('_');
                    // TODO: We need to add some more statements to check like MATREAD, READU etc...
                    lstFileLines = lstFileLines.ProcessForStatementsConversion(fMaster);
                    // Prepare list to insert into statement reference master...
                    // As Jcl file will not hace class start and end indicator, so we need to add it to make generic class block...

                    lstStatementReferenceMaster = universeBasicV1
                        .PrepareStatementReferenceMasterStart(fMaster, classStart.PrimaryReferenceId, methodStart.PrimaryReferenceId);
                    lstStatementReferenceMaster.ForEach(
                        s =>
                        {
                            var firstOrDefault = lstFileLines.FirstOrDefault();
                            if (firstOrDefault != null)
                                s.BusinessName = firstOrDefault.Replace("*", "");
                        });
                    // List of statement reference master for all statements in Jcl file with association for base and primary command id's
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started executing next process: PrepareJclFile for file: " + fMaster.FileId);
                    var fileStatements = universeBasicV1.PrepareParseJclFile(fMaster.FileId, languageId, copyOfFileMaster.ToList(), baseCommandReference, lstFileLines, callExternalStart.PrimaryReferenceId, "RUN", "CALL", "EXECUTE", "PH", "PHANTOM");

                    lstStatementReferenceMaster.AddRange(fileStatements);
                    // End of statement reference...
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started executing next process: PrepareStatementReferenceMasterEnd for file: " + fMaster + "," + classEnd.PrimaryReferenceId + "," + methodEnd.PrimaryReferenceId + "");

                    var closeList = universeBasicV1.PrepareStatementReferenceMasterEnd(fMaster,
                        classEnd.PrimaryReferenceId, methodEnd.PrimaryReferenceId);
                    lstStatementReferenceMaster.AddRange(closeList);
                    lstStatementReferenceMaster.ForEach(p =>
                    {
                        p.ProjectId = projectId;
                        p.SolutionId = solutionId;
                    });

                    // Check here IF and END IF Count...
                    int ifCount = lstStatementReferenceMaster.Count(s => ifRegEx.IsMatch(s.OriginalStatement));
                    int endIfCount = lstStatementReferenceMaster.Count(s => endIfRegex.IsMatch(s.OriginalStatement));
                    Console.WriteLine("========================================================================================");
                    Console.WriteLine("File Id: " + fMaster.FileId + "\nFile Path: " +
                                      fMaster.FilePath + "\nIF Count: " + ifCount + "\nEND IF Count: " +
                                      endIfCount);
                    Console.WriteLine("========================================================================================");
                    bool countMatch = ifCount == endIfCount;
                    if (!countMatch)
                    {
                        Console.WriteLine("========================================================================================");
                        Console.WriteLine("IF and END IF count is not matching in this file. Details as: ");
                        Console.WriteLine("File Id: " + fMaster.FileId + "\nFile Name: " + fMaster.FileName +
                                          "\nFile Path: " + fMaster.FilePath);
                        Console.WriteLine("Skipped file for further processing ans marked file status as un-processed.");
                        Console.WriteLine("========================================================================================");
                        fMaster.WorkFlowStatus = "IF and END IF count is not matching";
                        fMaster.ProjectMaster = null;
                        fMaster.Processed = 4;
                        fMaster.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(fMaster).ConfigureAwait(false);

                        await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);
                        continue;
                    }

                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                    // Update file status to processed = 1...
                    fMaster.Processed = 1;
                    fMaster.ProjectMaster = null;
                    fMaster.FileTypeExtensionReference = null;
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
                    var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s", RegexOptions.IgnoreCase);
                    if (regEx.IsMatch(oStatement))
                    {
                        int splitLength = oStatement.Split(' ').Length;
                        if (splitLength <= 1) continue;
                    }

                    string programName = regEx.IsMatch(oStatement) ? oStatement.Split(' ')[1] : oStatement.Split(' ')[2];
                    var anyFile = copyOfFileMaster.ToList()
                        .Any(f => programName != null
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.Processed == 0);
                    if (!anyFile)
                        continue;
                    var programFilePath = copyOfFileMaster.ToList()
                        .Find(f => programName != null
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.Processed == 0);

                    if (string.IsNullOrEmpty(programFilePath.FilePath)) return Ok("File Not Found");

                    lstStateref.ReferenceFileId = programFilePath.FileId;
                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(lstStateref)
                        .ConfigureAwait(false);

                    var programFileLines = File.ReadAllLines(programFilePath.FilePath).ToList();

                    programFileLines.RemoveAll(s => s.Length <= 0);
                    programFileLines = programFileLines.Select(s => s.Trim()).ToList();
                    programFileLines = programFileLines.CombineAllBrokenLines('_');
                    // programFileLines = programFileLines.Select(s => s.CheckCommentInStatement()).ToList();
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine(
                        "Started executing next process: AdjustMatReadLockedLikeStatements for project: " + projectId +
                        " and file: " + programFilePath.FileName + " ");
                    // TODO: Change
                    programFileLines = programFileLines.ProcessForStatementsConversion(programFilePath);
                    // programFileLines = programFileLines.AdjustMatReadLockedLikeStatements(); 
                    /*
                    int afterConvertIfCount = programFileLines.Count(a => a.StartsWith("IF ")) + 1;
                    int afterConvertEndIfCount = programFileLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");
                    bool ifEndIfCntMatch = afterConvertIfCount == afterConvertEndIfCount;
                    if (!ifEndIfCntMatch)
                    {
                        Console.WriteLine("=============================================================================");
                        Console.WriteLine("IF Count: " + afterConvertIfCount);
                        Console.WriteLine("END IF Count: " + afterConvertEndIfCount);
                        Console.WriteLine("File Name: " + Path.GetFileName(programFilePath.FilePath));
                        Console.WriteLine("File Path: " + programFilePath.FilePath);
                        Console.WriteLine("Skipped file for further processing ans marked file status as un-processed.");
                        programFilePath.WorkFlowStatus = "IF and END IF count is not matching";
                        programFilePath.ProjectMaster = null;
                        programFilePath.Processed = 4;
                        programFilePath.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(programFilePath).ConfigureAwait(false);
                        Console.WriteLine("=============================================================================");
                        continue;
                    }
                    */

                    var copyOfFileLines = programFileLines.ToList();
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started executing next process: GetAllIncludeStatements for project: " +
                                             projectId +
                                             " and file: " + programFilePath.FileName + "");
                    var allIncludesFiles = programFileLines.GetAllIncludeStatements("$INSERT");
                    programFileLines = programFileLines.Where(s => !s.StartsWith("*")
                                                                   && !s.StartsWith("!!") && !s.StartsWith(";*") &&
                                                                   !s.StartsWith("; *")).ToList();
                    programFileLines = programFileLines.Where(a => !a.StartsWith("$INSERT")).ToList();

                    #region Process Include file statements...

                    List<string> fileParsed = new List<string>();
                    foreach (var includeFile in allIncludesFiles)
                    {
                        var iName = includeFile.Split('>').LastOrDefault();
                        stringBuilder.AppendLine(
                            "========================================================================================");
                        stringBuilder.AppendLine(
                            "Started executing next process: ParseCallAndIncludesFilesUniverse for project:" + projectId +
                            ", file is:" + iName + ", language is:" + projectDetails.LanguageId + ", solution is:" +
                            solutionId + ")");

                        if (fileParsed.Any(f => f == iName)) continue;

                        Console.WriteLine(
                            "========================================================================================");
                        Console.WriteLine(
                            "Started executing next process: ParseCallAndIncludesFilesUniverse for project:" + projectId +
                            ", file is:" + iName + ", language is:" + projectDetails.LanguageId + ", solution is:" +
                            solutionId + ")");

                        await ParseCallAndIncludesFilesUniverse(iName, copyOfFileMaster.ToList(),
                            projectDetails.LanguageId, solutionId).ConfigureAwait(false);

                        fileParsed.Add(iName);
                    }
                    stringBuilder.AppendLine("Completed process of all Include files in current program file. File is: " + programFilePath.FilePath);
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    #endregion

                    #region Process Call file statements... (BaseCommand Id= 6)

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started executing next process: GetAllIncludeStatementsfor project: " +
                                             projectId +
                                             " and file: " + programFilePath.FileName + "");
                    var allCallFiles = programFileLines.GetAllIncludeStatements("CALL").Distinct().ToList();
                    List<string> calledFileParse = new List<string>();
                    foreach (var callFile in allCallFiles)
                    {
                        try
                        {
                            var iName = callFile.Split('@').LastOrDefault();
                            var iExactFileName = iName.Split('(').FirstOrDefault();
                            stringBuilder.AppendLine(
                                "========================================================================================");
                            stringBuilder.AppendLine(
                                "Started executing next process: ParseCallAndIncludesFilesUniverse for project: " +
                                projectId + ", file is:" + iExactFileName + ", language is:" + projectDetails.LanguageId +
                                ", solution is:" + solutionId + ")");

                            if (calledFileParse.Any(f => f == iExactFileName)) continue;

                            Console.WriteLine(
                                "Started executing next process: ParseCallAndIncludesFilesUniverse for project: " +
                                projectId + ", file is:" + iExactFileName + ", language is:" + projectDetails.LanguageId +
                                ", solution is:" + solutionId + ")");
                            stringBuilder.AppendLine(
                                "========================================================================================");
                            await ParseCallAndIncludesFilesUniverse(iExactFileName, copyOfFileMaster.ToList(),
                                    projectDetails.LanguageId, solutionId).ConfigureAwait(false);

                            calledFileParse.Add(iExactFileName);
                        }
                        catch (Exception exception)
                        {
                            Console.WriteLine(exception.Message);
                        }
                    }
                    stringBuilder.AppendLine("Completed process of all Subroutines in current program file. File is: " + programFilePath.FilePath);
                    stringBuilder.AppendLine(
                        "========================================================================================");

                    #endregion

                    #region Correct all method blocks and statements...

                    // Program file processing started...

                    var programLineList = new List<string>();
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started collecting all GOSUB: GetAllGoSub for project: " + projectId +
                                             " and file is: " + programFilePath.FileName + "");
                    var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine(
                        "Started collecting all certainpoint: GetListFromCertainPoint for project: " + projectId +
                        " and file is: " + programFilePath.FileName + "");

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
                        stringBuilder.AppendLine(
                            "========================================================================================");
                        stringBuilder.AppendLine(
                            "Started collecting all methodblocks: PickUpAllMethodBlocks for project: " + projectId +
                            " and file is: " + programFilePath.FileName + "");
                        var methodBlockOriginal = universeBasicV1.PickUpAllMethodBlocks(copyOfFileLines, currentLine);
                        var methodBlock = methodBlockDictionary.FirstOrDefault(s => s.Key == currentLine);
                        programLineList.AddRange(methodBlockOriginal);

                        if (methodBlock.Value == null) continue;

                        var lastSting = methodBlock.Value.Last();
                        if (lastSting != "RETURN")
                        {
                            Console.WriteLine("Method: " + currentLine +
                                              ":- RETURN Statement is missing. Added it manually");
                            stringBuilder.AppendLine("Method: " + currentLine +
                                                     ":- RETURN Statement is missing. Added it manually");
                            methodBlock.Value.Add("RETURN");
                        }
                        programLineList.AddRange(methodBlock.Value);
                    }

                    // programLineList = programLineList.SplitIfElseThenStatement(true);

                    #endregion

                    var statmentRefStart = universeBasicV1.PrepareStatementReferenceMasterStart(programFilePath, 42);
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
                    var linesWithCommentedPart = programLineList.ToList();
                    programLineList = programLineList.Select(s => s.CheckCommentInStatement()).ToList();

                    // Here check whether file has multiple STOP... 
                    // Find out index position of last stop...
                    // Till that time don't close first method by base command id = 9
                    var stopLinesCount = programLineList.Count(l => l == "STOP" || l == "STOP @#THIS IS LAST STOP");
                    var lastIndexOfStop = 0;

                    if (stopLinesCount == 0)
                    {
                        programLineList.Add("STOP");
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

                        if (lastIndexOfStop != 0
                            && programLineList[lastIndexOfStop] != "STOP")
                            programLineList.Insert(lastIndexOfStop, "STOP");
                    }

                    processFromHere:
                    Console.WriteLine(
                        "========================================================================================");
                    Console.WriteLine("STOP lines count is: " + stopLinesCount + Environment.NewLine +
                                      "Last index of stop is: " + lastIndexOfStop + Environment.NewLine +
                                      "File Path: " + programFilePath.FilePath + Environment.NewLine +
                                      "File Id: " + programFilePath.FileId);
                    Console.WriteLine(
                        "========================================================================================");

                    if (stopLinesCount == 0)
                        Console.WriteLine("No Stop...");

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started dump statement into database of file is: " + programFilePath.FileId);

                    // Check here IF and END IF Count...
                    // TODO: We need to add some more statements to check like MATREAD, READU etc...
                    /*
                    int ifCount = programLineList.Count(s => ifRegEx.IsMatch(s));
                    int endIfCount = programLineList.Count(s => endIfRegex.IsMatch(s));
                    Console.WriteLine("========================================================================================");
                    Console.WriteLine("File Id: " + programFilePath.FileId + "\nFile Path: " +
                                      programFilePath.FilePath + "\nIF Count: " + ifCount + "\nEND IF Count: " +
                                      endIfCount);
                    Console.WriteLine("========================================================================================");
                    bool countMatch = ifCount == endIfCount;
                    if (!countMatch)
                    {
                        Console.WriteLine("========================================================================================");
                        Console.WriteLine("IF and END IF count is not matching in this file. Details as: ");
                        Console.WriteLine("File Id: " + programFilePath.FileId + "\nFile Name: " + programFilePath.FileName +
                                          "\nFile Path: " + programFilePath.FilePath);
                        Console.WriteLine("Skipped file for further processing ans marked file status as un-processed.");
                        Console.WriteLine("========================================================================================");
                        programFilePath.WorkFlowStatus = "IF and END IF count is not matching";
                        programFilePath.ProjectMaster = null;
                        programFilePath.Processed = 4;
                        programFilePath.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(programFilePath).ConfigureAwait(false);
                        continue;
                    }
                    */

                    var indexPosition = -1;
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

                        if (line.StartsWith(lineCommentedIndicators.Find(x => x.StartIndicator != null).StartIndicator))
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

                            /*
                            if (indexPosition <= lastIndexOfStop)
                            {
                                bCommandId = methodEnd.BaseCommandId;
                                pCommandId = methodEnd.PrimaryReferenceId;
                            }
                            else if (indexPosition > lastIndexOfStop)
                            {
                                bCommandId = 0;
                                pCommandId = 0;
                            }
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
                                BaseCommandId = bCommandId,
                                PrimaryCommandId = pCommandId,
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
                                BaseCommandId = methodStart.BaseCommandId,
                                PrimaryCommandId = methodStart.PrimaryReferenceId,
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
                                PrimaryCommandId =
                                    ifEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
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
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine(
                        "Started executing next process: ParseDataDictionaryReferenceCode for project: " + projectId +
                        ", all call files: " + allCallFiles + ", and file is: " + programFilePath.FileName + "");
                    var processDataDictionaryResult =
                        await ParseDataDictionaryReferenceCode(projectId, allCallFiles, programFilePath);
                    await processDataDictionaryResult.ExecuteAsync(CancellationToken.None);

                    #region Update Class starting points with Menu names

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("\n" + "Called stored procedure: SpGetAllClassNamesWithCondition: " +
                                             projectId);
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var allClassStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNamesWithCondition", parameters);

                    using (var appDbContext = new AppDbContext())
                    {
                        var jclMenuName = await appDbContext.UniverseFileMenu
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

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started update program file Processed for project: " + projectId +
                                             "  file is: " + programFilePath.FileName);
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
                                (f.FileTypeExtensionId == 12
                                || f.FileTypeExtensionId == 9
                                || f.FileTypeExtensionId == 17) &&
                                f.FileName.StartsWith(iExactFileName) &&
                                f.Processed == 0);

                        if (!anyFileNew)
                        {
                            anyFileNew = copyOfFileMaster.ToList().Any(
                                f =>
                                    !string.IsNullOrEmpty(iExactFileName) &&
                                    (f.FileTypeExtensionId == 12
                                    || f.FileTypeExtensionId == 9
                                    || f.FileTypeExtensionId == 17) &&
                                    f.FileName.StartsWith(iExactFileName) && f.SolutionId == solutionId &&
                                    f.Processed == 0);
                            if (!anyFileNew)
                                continue;
                        }

                        var icdFileThis = copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) && (f.FileTypeExtensionId == 12
                                                                                        || f.FileTypeExtensionId == 9
                                                                                        || f.FileTypeExtensionId == 17) &&
                                              f.FileName.StartsWith(iExactFileName) &&
                                              f.Processed == 0)
                                          ?? copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) &&
                                              (f.FileTypeExtensionId == 12
                                               || f.FileTypeExtensionId == 9
                                               || f.FileTypeExtensionId == 17) &&
                                              f.FileName.StartsWith(iExactFileName) &&
                                              f.ProjectId == projectId &&
                                              f.Processed == 0);
                        if (icdFileThis == null)
                            // continue;
                            icdFileThis = copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) && (f.FileTypeExtensionId == 12
                                                                                        || f.FileTypeExtensionId == 9
                                                                                        || f.FileTypeExtensionId == 17) &&
                                              f.FileName.StartsWith(iExactFileName) &&
                                              f.Processed == 0)
                                          ?? copyOfFileMaster.ToList().Find(f =>
                                              !string.IsNullOrEmpty(iExactFileName) &&
                                              (f.FileTypeExtensionId == 12
                                               || f.FileTypeExtensionId == 9
                                               || f.FileTypeExtensionId == 17) &&
                                              f.FileName.StartsWith(iExactFileName) && f.SolutionId == solutionId &&
                                              f.Processed == 0);

                        icdFileThis.Processed = 1;
                        icdFileThis.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(icdFileThis);
                    }

                    #endregion

                    #region update Includes file Processed

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started update Includes file Processed for project:" + projectId);
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
                            f => !string.IsNullOrEmpty(iName) && f.FileTypeExtensionId == 12 &&
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
                        "========================================================================================");
                    stringBuilder.AppendLine("Started update classcalled a for base command Id = 6 " +
                                             "where recordsource statement for project: " + projectId);

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
                                f => !string.IsNullOrEmpty(iName) && f.FileTypeExtensionId == 12 &&
                                     f.FileName.StartsWith(iName));

                            if (!anyFileNew)
                            {
                                anyFileNew = copyOfFileMaster.Any(
                                    f => !string.IsNullOrEmpty(iName) && f.FileTypeExtensionId == 12 &&
                                         f.FileName.StartsWith(iName) && f.SolutionId == solutionId);
                                // (f.ProjectId == projectId || f.ProjectId == commanClassProjId));
                                if (!anyFileNew)
                                    continue;
                            }

                            var icdFile = copyOfFileMaster.ToList().Find(
                                              f => !string.IsNullOrEmpty(iName) && f.FileTypeExtensionId == 12 &&
                                                   f.FileName.StartsWith(iName))
                                          ?? copyOfFileMaster.ToList().Find(
                                              f => !string.IsNullOrEmpty(iName) && f.FileTypeExtensionId == 12 &&
                                                   f.FileName.StartsWith(iName) && f.SolutionId == solutionId);
                            //(f.ProjectId == projectId || f.ProjectId == commanClassProjId));
                            if (icdFile == null)
                                continue;


                            fileMasterInclude.Add(icdFile);
                        }
                        var lstFIleIds = (from f in fileMasterInclude select f.FileId).ToList();
                        var fIds = string.Join(",", lstFIleIds);
                        if (fIds == "") continue;

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
                                    allStatements.Single(d => d.FileId == fileId && d.BaseCommandId == 19)
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
                        "========================================================================================");
                    stringBuilder.AppendLine(
                        "Started update $INSERT abd CALL statement for project: " + projectId + ", and file is:" +
                        programFilePath.FileId);

                    #region Update $INSERT and CALL statement (To apply the link)

                    try
                    {
                        var execCallInsertSqlNew = "Select * from StatementReferenceMaster where ProjectId=" + projectId +
                                                   "" +
                                                   " and OriginalStatement like 'CALL %' and FileId=" +
                                                   programFilePath.FileId + ";";

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
                                    (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                    f.FileName.StartsWith(fName));


                            if (!anyNewFile) continue;

                            var icdFile = copyOfFileMaster.Single(
                                f =>
                                    !string.IsNullOrEmpty(fName) &&
                                    (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) &&
                                    f.FileName.StartsWith(fName));

                            var neLink = stat.OriginalStatement +
                                         "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" +
                                         icdFile.FileId + ");'>[ " + fName + " ]</a>";
                            stat.OriginalStatement = neLink;
                            stat.ReferenceFileId = icdFile.FileId;
                            await _codeVortoService.StatementReferenceMasterRepository
                                .UpdateItem(stat).ConfigureAwait(false);
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(ex.Message);
                    }

                    #endregion

                    stringBuilder.AppendLine(
                        "========================================================================================");
                    stringBuilder.AppendLine(
                        "Started update classcalled and methodcalled for basecommandId = 30 and project: " + projectId);

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

                            if (exNmPg.OriginalStatement != "IF FOUND THEN"
                                && exNmPg.OriginalStatement != "IF FOUND"
                                && exNmPg.OriginalStatement != "IF NOT SUCCESS THEN"
                                && exNmPg.OriginalStatement != "IF NOT-SUCCESS THEN"
                                && exNmPg.OriginalStatement != "IF SUCCESS THEN"
                                && exNmPg.OriginalStatement != "IF NOT FOUND THEN"
                                && exNmPg.OriginalStatement != "IF NOT-FOUND THEN"
                                && exNmPg.OriginalStatement != "IF NOT FOUND"
                                && exNmPg.OriginalStatement != "IF NOT-FOUND")
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
                                                                     (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17)).ToList();
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
                            cExternal.ReferenceFileId = pName[0].FileId;
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
                        .Any(f => programName != null
                                  && f.FileName.StartsWith(programName)
                                  && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.SolutionId == solutionId);

                    if (!anyFile) continue;
                    var allCheckFiles = copyOfFileMaster.ToList()
                        .FindAll(f => programName != null
                                   && f.FileName.StartsWith(programName)
                                   && (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17) && f.SolutionId == solutionId);

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
                            constructor.ReferenceFileId = pName[0].FileId;
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

                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started executing next process: ProcessAllActionWorkflows for project: " + projectId + ", file is: " + currentJclFile.FileName);
                LogMessage.WriteLogMessage(stringBuilder);
                await ProcessAllActionWorkflows(projectId, currentJclFile).ConfigureAwait(false);

                //var processStartingPointResult = await GetAllStartingPoints(projectId);
                //await processStartingPointResult.ExecuteAsync(CancellationToken.None);

                return Ok("File Processed");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseCallAndIncludesFilesUniverse(string iName,
            List<FileMaster> copyOfFileMaster, int languageId, int solutionId)
        {
            var stringBuilder = new StringBuilder();
            // int commanClassProjId = 9;
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
                        !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12
                        || f.FileTypeExtensionId == 9
                        || f.FileTypeExtensionId == 17) &&
                        f.FileName.StartsWith(iName) && f.Processed == 0);

                if (!anyFile)
                {
                    var fileMasterNew = await _codeVortoService.FileMasterRepository
                        .GetAllItems(p => p.SolutionId == solutionId).ConfigureAwait(false);
                    copyOfFileMaster = new List<FileMaster>(fileMasterNew as FileMaster[] ?? fileMasterNew.ToArray());
                    anyFile = copyOfFileMaster.Any(
                        f =>
                            !string.IsNullOrEmpty(iName) &&
                            (f.FileTypeExtensionId == 12
                            || f.FileTypeExtensionId == 9
                            || f.FileTypeExtensionId == 17) &&
                            f.FileName.StartsWith(iName) &&
                            f.SolutionId == solutionId &&
                            f.Processed == 0);
                    if (!anyFile)
                        return Ok("File not found. " + iName);
                }
                var icdFile = copyOfFileMaster.Find(
                    f =>
                        !string.IsNullOrEmpty(iName) && (f.FileTypeExtensionId == 12
                        || f.FileTypeExtensionId == 9
                        || f.FileTypeExtensionId == 17) &&
                        f.FileName.StartsWith(iName) &&
                        f.Processed == 0);

                if (icdFile == null)
                {
                    icdFile = copyOfFileMaster.Find(
                        f =>
                            !string.IsNullOrEmpty(iName) &&
                            (f.FileTypeExtensionId == 12
                            || f.FileTypeExtensionId == 9
                            || f.FileTypeExtensionId == 17) &&
                            f.FileName.StartsWith(iName) && f.SolutionId == solutionId &&
                            f.Processed == 0);
                    if (icdFile == null)
                        return Ok("File not found. " + iName);
                }

                if (icdFile.DoneParsing == 1)
                    return Ok("Parsing is already done");

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
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Started collecting all GOSUB: GetAllGoSub for project:" + projectId);
                var lstAllGoSubs = programFileLines.GetAllGoSub("GOSUB");
                Dictionary<string, List<string>> methodBlockDictionary;
                if (icdFile.FileTypeExtensionId == 9)
                {
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started collecting all certainpointProgram: GetListFromCertainPoint for projectId: " + projectId);
                    methodBlockDictionary = universeBasicV1.GetListFromCertainPoint(copyOfFileLines,
                        lstAllGoSubs);
                }
                else
                {
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started collecting all certainpointInclude: GetListFromCertainPointInclude for project: " + projectId);
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
                        stringBuilder.AppendLine("========================================================================================");
                        stringBuilder.AppendLine("Started collecting all methodblocks: PickUpAllMethodBlocks for project:" + projectId);
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
                var programLineListMainList = new List<string>();

                foreach (var lineList in programLineList)
                {
                    if (string.IsNullOrEmpty(lineList)) continue;
                    if (lineList.StartsWith("If") || lineList.StartsWith("IF"))
                    {
                        if (lineList.ToUpper().Contains(" THEN"))
                        {
                            int index = lineList.ToUpper().IndexOf(" THEN", StringComparison.Ordinal);
                            string result = lineList.Substring(0, index + 5);
                            programLineListMainList.Add(result);
                            string result1 = lineList.Substring(index + 5);
                            if (!string.IsNullOrEmpty(result1))
                            {
                                programLineListMainList.Add(result1.Trim());
                            }
                            continue;
                        }
                    }
                    programLineListMainList.Add(lineList);
                }
                programLineList = new List<string>();
                programLineList.AddRange(programLineListMainList);
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

                    if (line == "END ELSE" || line == "ELSE")
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
                            PrimaryCommandId = line == "LOOP" ? 62 : loopStart.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
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
                                                                 f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17).ToList();
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
                        .GetAllItems(f => f.ProjectId == projectId && f.FileName == fName).ConfigureAwait(false);

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
                        if (exNmPg.OriginalStatement != "IF FOUND THEN"
                            && exNmPg.OriginalStatement != "IF FOUND"
                            && exNmPg.OriginalStatement != "IF NOT SUCCESS THEN"
                            && exNmPg.OriginalStatement != "IF NOT-SUCCESS THEN"
                            && exNmPg.OriginalStatement != "IF SUCCESS THEN"
                            && exNmPg.OriginalStatement != "IF NOT FOUND THEN"
                            && exNmPg.OriginalStatement != "IF NOT-FOUND THEN"
                            && exNmPg.OriginalStatement != "IF NOT FOUND"
                            && exNmPg.OriginalStatement != "IF NOT-FOUND")
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
                                 f.FileTypeExtensionId == 12 &&
                                 f.FileName.StartsWith(fileName) &&
                                 f.Processed == 1);

                        if (!anyFile) continue;

                        var icdFile = fileMasterData.Find(
                            f => !string.IsNullOrEmpty(fileName) && f.FileTypeExtensionId == 12 &&
                                 f.FileName.StartsWith(fileName) &&
                                 f.Processed == 1);
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
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpFindExternalCallMethodName for: (" + projectId + "," + methodName + "," + includeFIles + ")");
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
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpUbGetInsertStatementData for: (" + projectId + "," + fileid + "," + insertStmt + ")");
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
            if (string.IsNullOrEmpty(cFile.FileName) || cFile.FileId == 0)
                return Ok("Action Workflows processed successfully");
            // Action Workflows data...
            var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
            Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
            var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
            var projectType = projectMasterData.ProjectConfigType;
            if (projectType != 8) return Ok("Action Workflows processed successfully");
            var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
            var projectConfigData = projectConfig.GetItem(projectType);
            if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");

            try
            {
                var actionWorkflowsRepository = new ActionWorkflowsRepository(new AppDbContext());
                var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                var allProcessedWorkflows = await actionWorkflowsRepository
                    .GetAllListItems(r => r.ProjectId == projectId && r.Processed == 1);
                stringBuilder.AppendLine("========================================================================================");
                stringBuilder.AppendLine("Called stored procedure: SpGetMethodData for: (" + projectId + "," + cFile.FileId + ")");
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = cFile.FileId}
                };

                var genericBlocks = await statementReferenceRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);
                foreach (var statement in genericBlocks)
                {
                    if (allProcessedWorkflows.Any(s => statement.StatementId == s.MethodStatementId)) continue;

                    #region Update Class starting points with Menu names

                    using (var appDbContext = new AppDbContext())
                    {
                        //var jclMenuName = await appDbContext.UniverseFileMenu
                        //    .ToListAsync().ContinueWith(t =>
                        //    {
                        //        var result = t.Result;
                        //        return result;
                        //    });

                        var jclMenuName = await appDbContext.UniverseFileMenu.ToListAsync().ConfigureAwait(false);
                        var menuName = (from menu in jclMenuName
                                        where menu.ActionExecuted.StartsWith(statement.OriginalStatement)
                                        select menu).ToList();
                        if (!menuName.Any()) continue;
                        foreach (var jclMenu in menuName)
                        {
                            string actionExecuted = jclMenu.ActionExecuted;
                            if (string.IsNullOrEmpty(actionExecuted)) continue;
                            string nameList = actionExecuted.Split(' ')[0]; //.ToList();
                            var fileName = Path.GetFileNameWithoutExtension(cFile.FilePath);
                            if (nameList != fileName) continue;
                            string mainName = jclMenu.MenuDescription.Trim().StartsWith("-")
                              ? jclMenu.MenuTitle + " " + jclMenu.MenuDescription
                              : jclMenu.MenuTitle + " - " + jclMenu.MenuDescription;
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Batch",
                                MethodStatementId = statement.StatementId,
                                OriginFileName = Path.GetFileName(cFile.FilePath),
                                OriginFilePath = cFile.FilePath,
                                ProjectId = projectId,
                                OriginEventMethod = jclMenu.MenuTitle,
                                OriginObject = mainName,
                                WorkflowName = nameList,
                                ServiceBaseAddress = null,
                                ServiceContract = null,
                                WorkflowBusinessName = statement.BusinessName,
                                IsDeleted = 0,
                                ReasonAboutDisable = null,
                                Processed = 0,
                                FileId = statement.FileId
                            };
                            await actionWorkflowsRepository.AddNewItem(actionWorkflow).ConfigureAwait(false);
                        }

                        // await _codeVortoService.ActionWorkflowsRepository.UpdateItem(aWorkflow);
                    }

                    #endregion
                }
            }

            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }
            stringBuilder.AppendLine("========================================================================================");
            stringBuilder.AppendLine("Action workflows processed successfully for project: " + projectId + ", and file is: " + cFile.FileName);
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
                                f.Processed == 0);

                        if (!anyFile) continue;

                        var icdFile = copyOfFileMaster.Find(
                            f =>
                                !string.IsNullOrEmpty(iName) &&
                                f.FileName.StartsWith(iName) &&
                                f.Processed == 0);

                        if (icdFile != null) allFileIds.Add(icdFile.FileId);
                    }
                }
                allFileIds.Add(programFile.FileId);
                var fileId = string.Join(",", allFileIds);
                using (var appDbContext = _codeVortoService.AppDbContextRepository)
                {
                    var dictionaryData = await appDbContext.DataDictionary.ToListAsync();

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
                            appDbContext.StatementWithDataDictionary.Add(statWithDataDictionary);
                            await appDbContext.SaveChangesAsync();
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

        /* Call function for subroutine program added into statementReferenceMaster*/
        [HttpGet]
        public async Task<IHttpActionResult> ParseSubRoutineProject(int projectId)
        {
            try
            {
                var stringBuilder = new StringBuilder();
                using (_codeVortoService = new CodeVortoService())
                {
                    var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                    var projectDetails = projectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);
                    int solutionId = projectDetails.SolutionId ?? 0;
                    string sqlQy = "select * from filemaster where FileTypeExtensionId=17 and projectId =" + projectId + ";";
                    var fileMaster =
                        await
                            _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                                .ConfigureAwait(false);

                    var copyOfFileMaster = fileMaster; //as FileMaster[] ?? fileMaster.ToArray();
                    var copyOfFileMasterNew = copyOfFileMaster
                        .Where(f => f.FileTypeExtensionId == 17
                                    && f.ProjectId == projectId
                                    && f.Processed == 0).ToList();

                    foreach (var copyFile in copyOfFileMasterNew)
                    {
                        await ParseCallAndIncludesFilesUniverse(copyFile.FileName,
                                copyOfFileMaster.ToList(), projectDetails.LanguageId, solutionId);

                        copyFile.Processed = 1;
                        copyFile.ProjectMaster = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(copyFile);

                        #region Update field for basecommandId = 45

                        stringBuilder.AppendLine(
            "========================================================================================");
                        stringBuilder.AppendLine("Started update field for basecommandId = 45 for Project: " + projectId + "");

                        var sqlQuery = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectId + "  " +
                                       " GROUP BY FileName;";

                        var universeBasicDataDicyionary =
                            await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

                        foreach (var dataDictionary in universeBasicDataDicyionary)
                        {
                            string tableName = dataDictionary.Description;

                            string fileName = dataDictionary.FileName;
                            string sqlQry = "SELECT * from statementreferencemaster WHERE fileId=" + copyFile.FileId +
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


                    #region Insert DataDependancy data

                    await InsertDataDependancyForUniverse(projectId);

                    #endregion

                    #region Insert data into DbCrudActivity

                    await InsertCrudActivityForUniverse(projectId);
                    #endregion

                    stringBuilder.AppendLine(
        "========================================================================================");
                    stringBuilder.AppendLine("Started ProcessPseudoCodeConversion for Project: " + projectId + "");
                    await ProcessPseudoCodeConversion(projectId);
                    stringBuilder.AppendLine("========================================================================================");
                    stringBuilder.AppendLine("Started ProcessSubRoutineProject for Project: " + projectId + "");
                    await ProcessSubRoutineProject(projectId);
                    stringBuilder.AppendLine("========================================================================================");
                    //stringBuilder.AppendLine("Started ProcessIncludeFilesSubRoutineProject for Project: " + projectId + "");
                    //await ProcessIncludeFilesSubRoutineProject(projectId);


                    LogMessage.WriteLogMessage(stringBuilder);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
            }
            return Ok("All files are processed successfully for Project Id: " + projectId);
        }

        /* call function for subroutine program (pseudo code)added into secondTabSubRoutineProgramDetails*/
        [HttpGet]
        public async Task<IHttpActionResult> ProcessSubRoutineProject(int projectId)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);

                    string sqlQy = "SELECT * FROM FileMaster WHERE FileTypeExtensionId = 17 " +
                                   " AND ProjectId = " + projectDetails.ProjectId + ";";
                    var fileMaster = await _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                                .ConfigureAwait(false);

                    var copyOfFileMaster = fileMaster.Where(f => f.FileTypeExtensionId == 17
                                             && f.ProjectId == projectId && f.Processed == 1).ToList();

                    foreach (var copyFile in copyOfFileMaster)
                    {
                        await GetWorkFlowWorkSpaceForSubRoutine(copyFile.FileId, projectId).ConfigureAwait(false);
                    }
                    return Ok("All Subroutine files are processed successfully for Project: " + projectDetails.ProjectName);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return InternalServerError(exception);
            }
        }
        
        /* call function for include program (pseudo code)added into secondTabSubRoutineProgramDetails*/
        [HttpGet]
        public async Task<IHttpActionResult> ProcessIncludeFilesSubRoutineProject(int projectId)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                    var projectDetails = projectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);
                    var fileMaster = await _codeVortoService.FileMasterRepository
                        .GetAllItems(p => p.ProjectId == projectId).ConfigureAwait(false);

                    var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();
                    var copyOfFileMasterNew = copyOfFileMaster
                        .Where(f => f.FileTypeExtensionId == 12 && f.ProjectId == projectId && f.Processed == 1).ToList();

                    foreach (var copyFile in copyOfFileMasterNew)
                    {
                        await GetWorkFlowWorkSpaceForSubRoutine(copyFile.FileId, projectId);
                    }
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
            }
            return Ok("All files are processed successfully for Project Id: " + projectId);
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertDataDependancyForUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var sqlQry = " SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectId + "  GROUP BY FileName;";
                    // string sqlQry = "select * from universebasicdatadictionary where FileName like '%T.TYPE%';";
                    var universebasicdatadictionarywithFileName =
                        await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQry)
                            .ConfigureAwait(false);

                    string sqlQuery = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectId + " ;";
                    var universebasicdatadictionary =
                        await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

                    foreach (var dataDictionary in universebasicdatadictionarywithFileName)
                    {
                        string fileName = dataDictionary.FileName;
                        List<string> attribute = new List<string>();
                        var dataAttributeList =
                            (from d in universebasicdatadictionary where d.FileName == dataDictionary.FileName select d)
                                .ToList();
                        int index = -1;
                        foreach (var attList in dataAttributeList)
                        {
                            index++;
                            if (index == 0) continue;
                            string attr = attList.Description;
                            if (!string.IsNullOrEmpty(attr)) attribute.Add(attr);
                        }
                        string sqlStmtMaster = "SELECT * FROM statementreferencemaster WHERE projectId =" +
                                                projectId + " AND  OriginalStatement LIKE '% " + fileName + "%';";
                        var statementReferenceMaster =
                            await
                                _codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlStmtMaster).ConfigureAwait(false);
                        foreach (var statementRef in statementReferenceMaster)
                        {
                            int fileId = statementRef.FileId;
                            var dataDependancy = new DataDependency
                            {
                                FileId = statementRef.FileId,
                                Entity = dataDictionary.FileName,
                                Attributes = string.Join(",", attribute),
                                ProjectId = statementRef.ProjectId
                            };
                            Expression<Func<DataDependency, bool>> expression =
                                x => x.FileId == fileId && x.Entity == fileName;
                            var dataDepen =
                                await
                                    _codeVortoService.DataDependencyRepository.GetAllListItems(expression)
                                        .ConfigureAwait(false);
                            if (dataDepen.Any()) continue;

                            await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependancy);
                        }
                    }

                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
                return Ok("done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertCrudActivityForUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var insertList = new List<string> { "MATWRITE", "MATWRITEU", "WRITE", "WRITEU", "WRITEV" };
                var updateList = new List<string> { "MATREADU", "READU", "READVU", "WRITEU", "WRITEVU" };
                var deleteList = new List<string> { "DELETE" };
                var selectList = new List<string> { "MATREAD", "READ", "READL", "READU", "READV", "READVL", "READVU", "MATREADL" };
                var sqlQuery = "SELECT * FROM DataDependency WHERE projectId =" + projectId + ";";
                var dataDependancy = await _codeVortoService.DataDependencyRepository
                    .GetDataFromSqlQuery<DataDependency>(sqlQuery).ConfigureAwait(false);
                foreach (var dependancy in dataDependancy)
                {
                    string entityName = dependancy.Entity;

                    #region Added into DbCrudActivityTable

                    string iR = "N";
                    string update = "N";
                    string delete = "N";
                    string select = "N";
                    var dbCurdActivity = new DbCrudActivity
                    {
                        EntityName = entityName,
                        SelectOrRead = select,
                        InsertOrCreate = iR,
                        Update = update,
                        Delete = delete,
                        FileId = dependancy.FileId
                    };
                    var mainList = new List<string>();
                    mainList.AddRange(insertList);
                    mainList.AddRange(deleteList);
                    mainList.AddRange(updateList);
                    mainList.AddRange(selectList);
                    string sqlQery = " SELECT * from StatementReferenceMaster where ProjectId =" + projectId + "  " +
                                     " AND OriginalStatement like '%" + entityName + "%' " +
                                     " AND FileId = " + dependancy.FileId + " AND ( ";
                    int i = 0;
                    foreach (var list in mainList)
                    {
                        i++;
                        sqlQery += i == 1
                            ? " OriginalStatement LIKE '%" + list + "%'"
                            : " OR OriginalStatement LIKE '%" + list + "%'";
                    }
                    sqlQery += ")";

                    var statementList = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQery)
                                .ConfigureAwait(false);

                    if (statementList.Any())
                    {
                        foreach (string sss in statementList.Select(s => s.OriginalStatement))
                        {
                            if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                            if (updateList.Any(c => sss.Contains(c))) update = "Y";
                            if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                            if (selectList.Any(c => sss.Contains(c))) select = "Y";

                        }
                        dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = entityName,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = dependancy.FileId
                        };
                    }
                    string entity = entityName;
                    int fileIdNew = dependancy.FileId;
                    Expression<Func<DbCrudActivity, bool>> expression =
                        x => x.EntityName == entity && x.FileId == fileIdNew;
                    var crud = await _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                                .ConfigureAwait(false);
                    if (crud.Any()) continue;
                    await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                    #endregion
                }
            }
            return Ok("done");
        }

        /*
        [HttpGet]
        public async Task<IHttpActionResult> ProcessForActionWorkFlowDetails(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlWorkflows = " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                                      " and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(sqlWorkflows).ConfigureAwait(false);

                var actionWorkflowDetailsWithProjectIdRepository =
                    new ActionWorkflowDetailsRepository(new AppDbContext());

                var appDbContext = new AppDbContext();
                var sqlQueery = "Select * from WorkflowTreeviewTabFirstDetails Where ProjectId = " + projectId + ";";

                var workflowTreeviewTabFirstDetails = await appDbContext.WorkflowTreeviewTabFirstDetails
                    .SqlQuery(sqlQueery).ToListAsync().ConfigureAwait(false);
                List<int> firstTab = (from details in workflowTreeviewTabFirstDetails
                                      let pId = details.ProgramId ?? 0
                                      where pId != 0
                                      select details.ProgramId ?? 0).ToList();
                firstTab = firstTab.Distinct().ToList();
                var programId = string.Join(",", firstTab);
                if (string.IsNullOrEmpty(programId))
                {
                    programId = 0.ToString();
                }
                var sqlQueryFirst = " select * from firsttabprogramdetails where BaseCommandId IN(5, 6, 1) " +
                                    " and ProgramId IN(" + programId + ")";

                var firstTabProgramDetails = await appDbContext.FirstTabProgramDetails
                    .SqlQuery(sqlQueryFirst).ToListAsync().ConfigureAwait(false);

                var sqlAllStatements = " SELECT * FROM statementreferencemaster AS sm INNER JOIN actionworkflows AS aw " +
                                           " ON aw.MethodStatementId = sm.StatementId WHERE sm.ProjectId = " + projectId + " " +
                                           " GROUP BY sm.FileId;";
                var statementRefernce =
                    await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllStatements).ConfigureAwait(false);


                foreach (var workflow in workflowRef)
                {
                    var objConnect = "btnObj_" + workflow.ActionWorkflowId;
                    var fileId =
                         (from s in statementRefernce
                          where s.StatementId == workflow.MethodStatementId
                          select s.FileId).FirstOrDefault();
                    var allPrograms = (from w in workflowTreeviewTabFirstDetails
                                       where w.ActionWorkflowId == workflow.ActionWorkflowId
                                             && w.ProgramId != 0
                                       select w.ProgramId).Distinct().ToList();

                    var callExternal = workflowTreeviewTabFirstDetails
                        .Count(d => d.BaseCommandId == 6 && d.ActionWorkflowId == workflow.ActionWorkflowId);

                    var completeProgramData = firstTabProgramDetails.AsQueryable()
                        .Where(s => allPrograms.Any(g => g == s.ProgramId)).ToList();

                    var asd = completeProgramData.Where(s => s.BaseCommandId == 6).ToList().Count;

                    callExternal = callExternal + asd;

                    var asd12 = completeProgramData.Where(s => s.BaseCommandId == 5).ToList().Count;

                    var callInternal =
                        firstTabProgramDetails.Count(
                            d => d.BaseCommandId == 5 && d.ActionWorkflowId == workflow.ActionWorkflowId);

                    callInternal = callInternal + asd12;

                    var decisionCount = workflowTreeviewTabFirstDetails.Count(d =>
                        d.BaseCommandId == 1 && d.ActionWorkflowId == workflow.ActionWorkflowId) + 1;

                    decisionCount = decisionCount + completeProgramData.Count(d => d.BaseCommandId == 1);

                    var tspName = workflow.WorkflowName;
                    tspName = tspName.Split(new[] { " Sub ", " Function " }, StringSplitOptions.None).LastOrDefault();

                    string disabled = string.Empty;
                    string btnStyle = "style='margin-top :5px;'";
                    string pageUrl = "workflow_workspace.html?prjId=" + projectId + "&stId=" +
                                     workflow.MethodStatementId + "";
                    if (workflow.Processed != null && workflow.Processed.Value == 0)
                    {
                        disabled = "disabled='disabled' title='Workflow yet to process'";
                        btnStyle = "style='margin-top :5px; background-color: #b1c8c1;'";
                        pageUrl = "#";
                    }
                    var workFlowDetails = new ActionWorkflowDetails
                    {
                        DecisionCount = decisionCount,
                        ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                        InternalCalls = callInternal > 0 ? "Yes" : "No",
                        View =
                            "<a " + disabled + " href='" + pageUrl + "'>" +
                            "<button " + btnStyle + " " + disabled + " class='btn btn-mint'>View</button> </a>" +
                            " &nbsp;<a href='#'><button style='margin-top : 5px;' class='btn btn-mint'>Rename</button> </a>&nbsp;" +
                            "<button id=" + workflow.ActionWorkflowId + " style='margin-top : 4px;' " + disabled + " " +
                            "class='btn btn-mint btn-icon icon-lg fa fa-upload'></button>&nbsp;" +
                            "<button id=" + objConnect + " style='margin-bottom: -6px; margin-top: -2px;height: 31px;'" + disabled + " " +
                            "class='btn btn-success btn-icon icon-lg fa fa-sitemap' " +
                            "onclick='downloadObjectConnectivityIndividualFlowchartUniverse" +
                            "(" + fileId + ", " + workflow.ActionWorkflowId + ");' title='Download object connectivity.'></button>",

                        OriginObject = workflow.OriginEventMethod,
                        WorkflowName = tspName,
                        ProjectName = workflow.ProjectMaster.ProjectName, //  + "  " + count,
                        ActionWorkflowId = workflow.ActionWorkflowId,
                        ProjectId = projectId
                    };
                    await actionWorkflowDetailsWithProjectIdRepository.AddNewItem(workFlowDetails).ConfigureAwait(false);
                }
            }
            return Ok("done");
        }
        */
    }
}