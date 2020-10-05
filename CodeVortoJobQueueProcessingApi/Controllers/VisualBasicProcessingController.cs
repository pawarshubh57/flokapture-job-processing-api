using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
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
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;
using BusinessLayer.UniverseBasic;
using System.Net.Http;
using Formatting = Newtonsoft.Json.Formatting;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class VisualBasicProcessingController : ApiController
    {
        private readonly ClsUniverseBasic _clsUniverseBasic = new ClsUniverseBasic();
        private ICodeVortoService _codeVortoService;

        public VisualBasicProcessingController()
        {
        }

        public VisualBasicProcessingController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ExecuteProcessActionsOneByOne(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                    int solutionId = projectMaster.SolutionId ?? 1;
                    int languageId = projectMaster.LanguageId;
                    await StartProcessVbProject(projectId).ConfigureAwait(false);

                    await ViewSource(projectId).ConfigureAwait(false);

                    await ProcessForProjectInventory(projectId, solutionId, languageId);
                    var projectJson = JsonConvert.SerializeObject(projectMaster, Formatting.Indented);
                    return Ok("Project processed successfully with all methods one by one. Project Details: " +
                              projectJson);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessVbProject(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectConfigFilesGeneralRepository =
                        new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                    var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                    var projectDetails = projectMasterRepository.GetItem(projectId);

                    if (projectDetails == null) return Ok(projectId);
                    stringBuilder.AppendLine("Started process for project: " + projectId + " project name: " +
                                             projectDetails.ProjectName + " and project physical path is: " +
                                             projectDetails.PhysicalPath);
                    var extensionList = await _codeVortoService.FileTypeExtensionRepository
                        .GetAllItems(p => p.LanguageId == projectDetails.LanguageId);
                    var strExtensions = new List<string>();
                    var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                      extensionList.ToList();
                    strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                    stringBuilder.AppendLine("Extensions: " + string.Join(",", strExtensions));
                    string projectPath = projectDetails.PhysicalPath;
                    List<string> directoryList = new List<string> { projectPath };
                    var ignoredFile = await projectMasterRepository
                        .GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                    var projectImpFiles = await projectConfigFilesGeneralRepository
                        .GetAllItems(p => p.ConfigFileId != 2);

                    var projectConfigs = projectImpFiles as IList<ProjectConfigMaster> ?? projectImpFiles.ToList();
                    var extensionsList = projectConfigs.Where(e => !string.IsNullOrEmpty(e.ToString()) 
                                    && e.LanguageId == projectDetails.LanguageId);
                    strExtensions.AddRange(extensionsList.Select(e => e.ToString()));
                    foreach (string directory in directoryList)
                    {
                        try
                        {
                            var allFiles = Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                                .Where(s => strExtensions.Any(s.EndsWith));

                            IEnumerator<string> enumerator = allFiles.GetEnumerator();
                            var lstFileMasters = new List<FileMaster>();
                            while (enumerator.MoveNext())
                            {
                                string currentFile = enumerator.Current;
                                if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                                string fileName = Path.GetFileName(currentFile);
                                if (string.IsNullOrEmpty(fileName)) continue;
                                if (fileName.Contains(".dll.config")) continue;
                                string extension = Path.GetExtension(currentFile);
                                int extensionId = fileTypeExtensionReferences
                                    .First(e => e.FileExtension == extension).FileTypeExtensionId;

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
                                    Processed = 0,
                                    LinesCount = fileLinesCount
                                };
                                lstFileMasters.Add(fileMaster);
                                // Insert one by one...
                                // await _codeVortoService.FileMasterRepository.AddNewItem(fileMaster);
                            }
                            enumerator.Dispose();
                            // Bulk Insert
                            // EntityFramework.BulkInsert.ProviderFactory.Register<BulkInsertProvider>("MySql.Data.MySqlConnection");
                            await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters).ConfigureAwait(false);
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }
                    //return Ok(projectId);  
                    stringBuilder.AppendLine("Started executing next process: ParseVbProjectFiles(" + projectId + ")");
                    LogMessage.WriteLogMessage(stringBuilder);

                    var processResult = await ParseVbProjectFiles(projectId);
                    var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                    string data = await dataContent.Content.ReadAsStringAsync();

                    var totalFilesCount = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.ProjectId == projectDetails.ProjectId).ConfigureAwait(false);
                    projectDetails.ProcessedDate = DateTime.Now;
                    projectDetails.ProcessedTime = DateTime.Now;
                    projectDetails.TotalFiles = totalFilesCount.Count;
                    projectDetails.LanguageMaster = null;
                    projectDetails.ProjectType = null;
                    await _codeVortoService.ProjectMasterRepository.UpdateItem(projectDetails).ConfigureAwait(false);

                    await ProcessActionWorkflowDetailsVba(projectId);

                    return Ok(data);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessActionWorkflowDetailsVba(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var allActionWorkFlows = await _codeVortoService.ActionWorkflowsRepository
                        .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);

                    var allClassNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => (s.BaseCommandId == 19 || s.BaseCommandId == 1 || s.BaseCommandId == 6 ||
                                               s.BaseCommandId == 5) && s.ProjectId == projectId).ConfigureAwait(false);

                    int graphId = 0;
                    foreach (var actionWorkflow in allActionWorkFlows)
                    {
                        graphId++;
                        var aDetails = GiveMeActionWorkFlowDetail(allActionWorkFlows, allClassNameDeclared,
                            actionWorkflow, graphId, projectId, 1);
                        aDetails.GraphId = "Graph_" + graphId;
                        aDetails.ParentId = "-1";
                        await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(aDetails)
                            .ConfigureAwait(false);
                    }
                    Console.WriteLine("Action Workflow Details Processed successfully for Project: " + projectId);
                    return Ok("Done");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
        }

        public ActionWorkflowDetails GiveMeActionWorkFlowDetail(List<ActionWorkflows> workflowRef,
           List<StatementReferenceMaster> lstStateRef, ActionWorkflows workflow, int parentRowId, int projectId, int flag)
        {
            try
            {
                var enable = workflowRef.Count(x => x.IsDeleted == 0);
                var hidden = workflowRef.Count(x => x.IsDeleted == 1);
                var count = "<b style='color: #7030a0;'>" + " (Total Workflows: " + workflowRef.Count +
                            " | Enabled: " + enable + " | Hidden: " + hidden + ")" + "</b>";
                var graphId = "graph_" + parentRowId;
                var objConnect = "btnObj_" + workflow.ActionWorkflowId;
                var download = "btnDowload_" + workflow.ActionWorkflowId;
                var fileId = workflow.FileId;
                var callExternal = lstStateRef.Count(d => d.BaseCommandId == 6 && d.FileId == workflow.FileId);
                var callInternal = lstStateRef.Count(d => d.BaseCommandId == 5 && d.FileId == workflow.FileId);
                var decisionCount = lstStateRef.Count(d => d.BaseCommandId == 1 && d.FileId == workflow.FileId) + 1;

                string disabled = string.Empty;
                string btnStyle = "style='margin-top :5px;height: 31px;'";
                string pageUrl = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                                 workflow.MethodStatementId + "&aId=" + workflow.ActionWorkflowId + "";

                string onClickUploadFile = "onclick=uploadFilePopupShow('" +
                                           workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") +
                                           "'," + workflow.ActionWorkflowId + ");";
                string onClickRename = "onclick=funWorkflowRename('" +
                                       workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") + "'," +
                                       workflow.ActionWorkflowId + ");";
                var disable = "<button id=" + workflow.ActionWorkflowId +
                              " style='margin-top: 5px;' class='btn btn-info btn-icon icon-lg fa fa-trash' onclick='workFlowDisable(" +
                              workflow.ActionWorkflowId + ")'></button>";
                var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                           "<button " + btnStyle + " " + disabled +
                           " class='btn btn-mint'>View</button> </a>" +
                           " &nbsp;<a href='#'><button style='margin-top : 5px;height: 31px;' class='btn btn-mint' " +
                           onClickRename + " >Rename</button> </a>&nbsp;<button id=" +
                           workflow.ActionWorkflowId + " style='margin-top : 4px;' " +
                           "class='btn btn-mint btn-icon icon-lg fa fa-upload' " + onClickUploadFile +
                           " title='Upload image/file(s)'></button>&nbsp;" +
                           "<button id=" + download +
                           " style='margin-bottom: -8px; margin-top: -3px;height: 31px;'" +
                           " class='btn btn-primary btn-icon icon-lg fa fa-download'  onclick=downloadRequirementDoc(" +
                           workflow.ActionWorkflowId + ");" +
                           "title='Download Requirement Specification Document(.docx)' ></button>&nbsp;" +
                           "<button id=" + objConnect +
                           " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-success btn-icon icon-lg fa fa-sitemap' " +
                           "onclick='downloadObjectConnectivityIndividualFlowchartUniverse(" +
                           fileId + ", " + workflow.ActionWorkflowId +
                           ");' title='Download object connectivity.'></button>";
                string originObject = flag == 1
                    ? Path.GetFileNameWithoutExtension(workflow.OriginFilePath)
                    : workflow.OriginFilePath;
                string workflowName =
                    workflow.WorkflowName.Split(new[] {" Sub ", " Function "}, StringSplitOptions.None)[1];
                workflowName = workflowName.Split('(')[0].Trim();
                var workflowDetails = new ActionWorkflowDetails
                {
                    DecisionCount = decisionCount,
                    ExtrernalCalls = callExternal > 0 ? "Yes" : "No",
                    InternalCalls = callInternal > 0 ? "Yes" : "No",
                    View = view,
                    OriginObject = workflowName,
                    WorkflowName = originObject,
                    ProjectName = workflow.ProjectMaster.ProjectName,
                    ActionWorkflowId = workflow.ActionWorkflowId,
                    Disable = disable,
                    IsDeleted = workflow.IsDeleted,
                    ShortDetails = count,
                    GraphId = graphId,
                    ParentId = "-1",
                    ProjectId = projectId
                };
                return workflowDetails;
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception);
            }
            return new ActionWorkflowDetails();
        }


        [HttpGet]
        public async Task<IHttpActionResult> ParseVbProjectFiles(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                    var projectDetails = projectMasterRepository.GetItem(projectId);
                    if (projectDetails == null) return Ok(projectId);
                    if (projectDetails.ProjectConfigType == 6) return Ok("No need to process this project");

                    var copyOfFileMaster = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(p => p.ProjectId == projectId);

                    if (projectDetails.ProjectConfigType == 6)
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
                                copyOfFileMaster =
                                    copyOfFileMaster.Where(f => !f.FilePath.EndsWith(".Designer.vb")).ToList();
                            }
                        }
                    }
                    stringBuilder.AppendLine("Total file for parsing: " + copyOfFileMaster.Count());

                    #region Get all Primary command and base command information to parse file...

                    var generalRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await generalRepository.GetAllItems()
                        .ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        });
                    var languageId = projectDetails.LanguageId;
                    var commentedBlockIndicators =
                        baseCommandReference.Find(s => s.BaseCommand == "Block Comment")
                            .PrimaryLanguageReference.ToList().FindAll(l => l.LanguageId == languageId);
                    var lineCommentedIndicators =
                        baseCommandReference.Find(s => s.BaseCommand == "Line Comment").PrimaryLanguageReference
                            .ToList().FindAll(l => l.LanguageId == languageId);
                    var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start")
                        .PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                    var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End")
                        .PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                    var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                        .PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                    var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                        .PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                    var structureIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Structure")
                        .PrimaryLanguageReference
                        .ToList().FindAll(l => l.LanguageId == languageId);
                    var structureIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Structure End")
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

                    #endregion

                    #region Start reading file lines and do process before dump to database...

                    foreach (var file in copyOfFileMaster)
                    {
                        stringBuilder.AppendLine("Started reading file lines file name: " + file.FileName +
                                                 " and file path is: " + file.FilePath);
                        var allLines = File.ReadAllLines(file.FilePath);
                        var loopLines = allLines;
                        var strLines = new List<string>();
                        var fileLines = loopLines.ToArray();
                        fileLines = fileLines.CombineAllBrokenLines('_');
                        for (var l = 0; l < fileLines.Select(s => s != "").Count(); l++)
                        {
                            string tempString = fileLines[l].TrimStart().Trim(' ', '\t');
                            if (tempString == "") continue;
                            if (tempString.StartsWith("'")) continue;
                            if (tempString.Contains('(') || tempString.Contains(')'))
                                tempString = tempString.InsertExtraChars('(', ')', " ");
                            // This is to findout substring upto comment within line if any...
                            if (!tempString.Contains('\''))
                                strLines.Add(tempString);
                            else
                            {
                                string newLine = tempString.RemoveCommentedPartFromLine();
                                strLines.Add(newLine);
                            }
                        }

                        fileLines = strLines.ToArray().DoIfLoopsAdjustment();

                        // Remove all commented block from array of lines...
                        var blockStartExpression =
                            commentedBlockIndicators.Find(x => x.EndIndicator != null).StartIndicator;
                        var blockEndExpression = commentedBlockIndicators.Find(x => x.EndIndicator != null).EndIndicator;
                        fileLines = fileLines.RemoveAllCommentedBlocks(blockStartExpression, blockEndExpression);
                        var indexPosition = -1;
                        stringBuilder.AppendLine("Started dump statement into database of file: " + file.FileName);

                        #region Starting parsing of file line by line

                        foreach (var line in fileLines)
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
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
                            }
                            if (lineDone) continue;

                            var methodStart = methodIndicationStart.ToList();
                            foreach (var mStart in methodStart)
                            {
                                bool result = line.ContainsAll(true, mStart.StartIndicator.Split(' ').ToArray());
                                if (!result) continue;
                                var methodOrFunctionLine = line
                                    .Split(new[] { mStart.StartIndicator }, StringSplitOptions.RemoveEmptyEntries)[1];
                                var methodOrFunctionName = methodOrFunctionLine.Split('(')[0];
                                var stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = line,
                                    OriginalStatement = line,
                                    ClassCalled = null,
                                    MethodName = methodOrFunctionName.Trim(),
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = mStart.BaseCommandId,
                                    PrimaryCommandId = mStart.PrimaryReferenceId,
                                    ProjectId = projectId
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
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
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            if (lineDone) continue;

                            var classEnd = classEndIndicator.ToList();
                            foreach (var clsEnd in classEnd)
                            {
                                if (!line.StartsWith(clsEnd.StartIndicator)) continue;
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
                                    BaseCommandId = clsEnd.BaseCommandId,
                                    PrimaryCommandId = clsEnd.PrimaryReferenceId,
                                    ProjectId = projectId
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
                            }
                            if (lineDone) continue;
                            var classStart = classStartIndicator.ToList();

                            foreach (var cStart in classStart)
                            {
                                if (!line.Contains(" " + cStart.StartIndicator + " ")) continue;
                                var className = line.Split(new[] { "Class" }, StringSplitOptions.RemoveEmptyEntries)[1];
                                if (className.Trim().Contains('(') || className.Trim().Contains(',') ||
                                    className.Trim().Contains(' '))
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
                                        ClassNameDeclared = null,
                                        PrimaryCommandId = 0,
                                        ParsedOrNot = "Y",
                                        ProjectId = projectId
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                                            stmtReferenceMaster);
                                    lineDone = true;
                                }
                                else
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
                                        ClassNameDeclared = className.Trim(),
                                        PrimaryCommandId = cStart.PrimaryReferenceId,
                                        ParsedOrNot = "Y",
                                        ProjectId = projectId
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(
                                            stmtReferenceMaster);
                                    lineDone = true;
                                }
                            }
                            if (lineDone) continue;
                            var structureEnd = structureIndicatorEnd.ToList();
                            foreach (var structure in structureEnd)
                            {
                                if (!line.StartsWith(structure.StartIndicator)) continue;
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
                                    BaseCommandId = structure.BaseCommandId,
                                    PrimaryCommandId = structure.PrimaryReferenceId,
                                    ParsedOrNot = "Y",
                                    ProjectId = projectId
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                lineDone = true;
                                break;
                            }
                            if (lineDone) continue;
                            var structureStart = structureIndicatorStart.ToList();
                            foreach (var structure in structureStart)
                            {
                                if (!line.Contains(" " + structure.StartIndicator + " ")) continue;
                                var stmtReferenceMaster = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = line,
                                    OriginalStatement = line,
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared =
                                        line.Split(new[] { structure.StartIndicator },
                                            StringSplitOptions.RemoveEmptyEntries)[1].Trim(),
                                    BaseCommandId = structure.BaseCommandId,
                                    PrimaryCommandId = structure.PrimaryReferenceId,
                                    ParsedOrNot = "Y",
                                    ProjectId = projectId
                                };
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
                    }

                    #endregion

                    #region Structured data...

                    var allStructuresData = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                            " Select * from StatementReferenceMaster where BaseCommandId = 13; ");
                    // 13 is for Structures

                    foreach (var structure in allStructuresData)
                    {
                        int stmtId = structure.StatementId;
                        object[] parameters =
                        {
                            new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId}
                        };
                        var structureVariables = await generalRepository
                            .ExecuteStoreProcedure<StatementReferenceMaster>("GetStructureBlocks", parameters);
                        foreach (var sVariables in structureVariables)
                        {
                            sVariables.AssociatedToParent = structure.StatementId;
                            sVariables.FileMaster = null;
                            sVariables.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sVariables);
                        }
                    }

                    #endregion
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
            }
            stringBuilder.AppendLine("Started executing next process: ProcessVbProjectFiles(" + projectId + ")");
            LogMessage.WriteLogMessage(stringBuilder);
            IHttpActionResult processResult = await ProcessVbProjectFiles(projectId);
            var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
            string data = await dataContent.Content.ReadAsStringAsync();
            return Ok(data);
        }

        [HttpGet]
        [SuppressMessage("ReSharper", "AccessToModifiedClosure")]
        public async Task<IHttpActionResult> ProcessVbProjectFiles(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine("Started processing for update statement: (" + projectId + ")");

                    #region strart procesing

                    var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                    var baseCommandReference = await baseCommandReferenceRepository
                        .GetAllItems().ContinueWith(t =>
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

                    //var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    //    .GetAllItems(p => p.ProjectId == projectId).ContinueWith(t =>{ var result = t.Result.ToList(); return result; });

                    var statementReferenceMasters = (IList<StatementReferenceMaster>) allStatements;
                    var emptyLines = statementReferenceMasters.ToList()
                        .FindAll(s => s.ResolvedStatement.StartsWith("Dim ")); // && s.ParsedOrNot != 1);
                    var declarations = declarationIndicator.ToList();

                    // Replace all Me. with ClassName. in ResolvedStatement...
                    // Commented for some time to check following code

                    stringBuilder.AppendLine(
                        "Called stored procedure: SpGetAllMethodNamesInProject(" + projectId + ",)");
                    object[] methodParameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var allMethodNames = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodNamesInProject",
                            methodParameters);
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

                    #region region 1...

                    foreach (var fId in fileIds)
                    {
                        try
                        {
                            var id = fId;
                            //Predicate<StatementReferenceMaster> stmtPredicate =
                            //    p => p.FileId == id && p.ProjectId == projectId && p.ClassNameDeclared != null;
                            string sqlQuery = "SELECT * FROM StatementReferenceMaster WHERE ProjectId = " + projectId +
                                              " AND FileId = " + id + " AND ClassNameDeclared is not null; ";
                            var className = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                            if (!className.Any()) continue;

                            foreach (var clsName in className)
                            {
                                //var classMethods = GetAllMethodsForClass(clsName.ClassNameDeclared, projectId);
                                stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                                         clsName.StatementId + ", 8, 9)");
                                var genericBlock = GetGenericBlock(clsName.StatementId, 19, 20);
                                var meContainsLines = genericBlock.ToList()
                                    .FindAll(s => s.OriginalStatement.Contains(" Me.")
                                                  && s.ProjectId == projectId && s.FileId == id);

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
                                        if (allMethodNames.Any(
                                            method => methodCalled.Trim().Contains(method.MethodName)))
                                        {
                                            callInternalOrExternal = true;
                                        }
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
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    #region region 2...

                    // Replace all constructor Names...
                    string construstorSql =
                        " Select * from statementreferencemaster where OriginalStatement " +
                        " like 'Public Sub New (  ) %' AND ProjectId = " + projectId + "; ";
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

                    #endregion

                    #region region 3...

                    foreach (var fileId in fileIds)
                    {
                        try
                        {
                            var allClassDeclarations = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "SELECT * FROM statementreferencemaster where ClassNameDeclared IS NOT NULL " +
                                    "AND ProjectId = " + projectId + " AND FileId = " + fileId +
                                    " group by ClassNameDeclared;");
                            var classCollection =
                                allClassDeclarations.FindAll(s => s.ClassNameDeclared != null
                                                                  && s.FileId == fileId
                                                                  && s.ProjectId == projectId).ToList();
                            stringBuilder.AppendLine("Collect all statement where basecommandid=19.");
                            var statementReferenceMaster = new StatementReferenceRepository(new AppDbContext());
                            stringBuilder.AppendLine("Collect all Class name declaration block.");

                            #region Class name declaration block...

                            foreach (var c in classCollection)
                            {
                                var c2 = c.ClassNameDeclared.TrimStart();
                                var allReferences = await _codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                        "SELECT * FROM StatementReferenceMaster Where ResolvedStatement like '%" + c2 +
                                        "%' AND ProjectId = " + projectId + " AND FileId = " + fileId +
                                        " AND BaseCommandId Not IN (8, 19);");

                                foreach (var reference in allReferences)
                                {
                                    string className = String.Empty;
                                    string variableName = String.Empty;
                                    if (reference.ResolvedStatement.Contains(" As New "))
                                    {
                                        try
                                        {
                                            string[] tempArr = reference.ResolvedStatement
                                                .Split(new[] {" As New "}, StringSplitOptions.RemoveEmptyEntries);
                                            variableName = tempArr[0].Split(' ')[1];
                                            className = tempArr[1];
                                        }
                                        catch (Exception exception)
                                        {
                                            LogMessage.WriteExceptionLogMessage(exception);
                                        }
                                    }
                                    else if (reference.ResolvedStatement.Contains(" As "))
                                    {
                                        try
                                        {
                                            string[] tempArr = reference.ResolvedStatement
                                                .Split(new[] {" As "}, StringSplitOptions.RemoveEmptyEntries);
                                            variableName = tempArr[0].Split(' ')[1];
                                            className = tempArr[1];
                                        }
                                        catch (Exception exception)
                                        {
                                            LogMessage.WriteExceptionLogMessage(exception);
                                        }
                                    }
                                    if (className != c2) continue;
                                    if (variableName == "") continue;

                                    // TODO: string andCondition = " ResolvedStatement";

                                    string newSqlQuery =
                                        " SELECT * FROM StatementReferenceMaster WHERE ProjectId = " + projectId +
                                        " AND ResolvedStatement Like '%" + variableName + ".%';";
                                    var allFileLines =
                                        await statementReferenceMaster
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(newSqlQuery);
                                    //.GetAllItems(predicate).ContinueWith(s =>
                                    //{
                                    //    var result = s.Result;
                                    //    var allLines = result.ToList(); 
                                    //    return allLines;
                                    //});
                                    stringBuilder.AppendLine("Called stored procedure: SpGetClassMethods(" + className +
                                                             ",)");
                                    object[] parameters =
                                    {
                                        new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = className}
                                    };
                                    var allMethodsWithinClass = await baseCommandReferenceRepository
                                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods",
                                            parameters);
                                    var allMethods = (from m in allMethodsWithinClass select m.MethodName).ToList();
                                    if (allMethods.Count <= 0) continue;
                                    foreach (var line in allFileLines)
                                    {
                                        foreach (var m in allMethods)
                                        {
                                            if (!line.ResolvedStatement.Contains(m)) continue;
                                            // string[] asz = line.ResolvedStatement.Split('=');

                                            string resolvedLine = line.ResolvedStatement.Replace(variableName, c2);
                                            line.ClassCalled = c.ClassNameDeclared;
                                            line.MethodCalled = line.ResolvedStatement.Split('.')[1];
                                            //line.VariableNameDeclared = variableName;
                                            line.DataOrObjectType = reference.DataOrObjectType;
                                            if (line.BaseCommandId == 1)
                                                line.OtherBaseCommandId = 6;
                                            else
                                                line.BaseCommandId = 6;
                                            line.ResolvedStatement = resolvedLine;
                                            line.WorkFlowRelevant = "Y";
                                            line.ProjectId = projectId;
                                            line.ReferenceFileMaster = null;
                                            line.FileMaster = null;
                                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(line);
                                        }
                                    }
                                }
                            }

                            #endregion
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    #region region 4...

                    List<string> variablesCollection = new List<string>();
                    //stringBuilder.AppendLine("Replace dim statement");
                    foreach (var emptyLine in emptyLines)
                    {
                        try
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
                                    dataObjectType.Split(new[] {" As "}, StringSplitOptions.None)[0];
                                string variableType = String.Empty;
                                try
                                {
                                    variableType = dataObjectType.Split(new[] {" As "}, StringSplitOptions.None)[1];
                                }
                                catch (Exception)
                                {
                                    if (!dataObjectType.Contains("As") && dataObjectType.Contains("="))
                                    {
                                        variableType = dataObjectType.Split(new[] {"="}, StringSplitOptions.None)[1];
                                    }
                                }

                                if (variableType.StartsWith("New"))
                                {
                                    variableType = variableType.Split(' ')[1];
                                    if (variableType.Contains("."))
                                        variableType = variableType.Split('.').Last();
                                }
                                if (variableType.Contains('='))
                                {
                                    variableType = variableType.Split('=').First();
                                }
                                variablesCollection.Add(variableType);
                                emptyLine.DataOrObjectType = variableType;
                                emptyLine.VariableNameDeclared = variableName;
                                emptyLine.BaseCommandId = declaration.BaseCommandId;
                                emptyLine.PrimaryCommandId = declaration.PrimaryReferenceId;
                                emptyLine.ParsedOrNot = "Y";
                                emptyLine.ProjectId = projectId;
                                emptyLine.FileMaster = null;
                                emptyLine.ReferenceFileMaster = null;
                                // TODO: Remember to uncomment...
                                await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(emptyLine)
                                    .ConfigureAwait(false);
                                //done = true;
                                break;
                            }
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    #region region 5...

                    var deadDataTypes = await
                        baseCommandReferenceRepository.GetEntityData<DeadDataTypes>(k => k.KaywordName != null);

                    if (variablesCollection.Count > 0)
                    {
                        variablesCollection.RemoveAll(s => s.Contains('='));
                        foreach (var d in deadDataTypes)
                        {
                            //if (variablesCollection.Any(d.KaywordName.Contains))
                            variablesCollection.RemoveAll(s => s.Contains(d.KaywordName));
                        }
                        //variablesCollection = (List<string>)variablesCollection.Distinct();
                    }

                    var allCollections = variablesCollection.Distinct();
                    List<string> containsVariables = new List<string>();
                    var collections = allCollections as string[] ?? allCollections.ToArray();

                    //List<string> actualVariableNames = new List<string>();
                    if (collections.Length > 0)
                    {
                        foreach (var d in collections)
                        {
                            containsVariables.AddRange(from s1 in emptyLines.ToList()
                                    .FindAll(s => s.ResolvedStatement.Contains(d) && !s.ResolvedStatement.Contains("=")
                                                  && !s.ResolvedStatement.Contains(" = ") && s.ProjectId == projectId)
                                select s1.ResolvedStatement);
                        }
                    }
                    if (containsVariables.Count > 0)
                    {
                        foreach (var variable in containsVariables)
                        {
                            var dataObjectType = variable.Replace("Dim ", "");
                            string variableName =
                                dataObjectType.Split(new[] {" As "}, StringSplitOptions.None)[0];
                            string variableType = String.Empty;
                            try
                            {
                                variableType = dataObjectType.Split(new[] {" As "}, StringSplitOptions.None)[1];
                            }
                            catch (Exception)
                            {
                                if (!dataObjectType.Contains("As") && dataObjectType.Contains("="))
                                {
                                    variableType = dataObjectType.Split(new[] {"="}, StringSplitOptions.None)[1];
                                }
                            }

                            if (variableType.StartsWith("New"))
                            {
                                variableType = variableType.Split(' ')[1];
                                if (variableType.Contains("."))
                                    variableType = variableType.Split('.').Last();
                            }
                            if (variableType.Contains('='))
                            {
                                variableType = variableType.Split('=').First();
                            }
                            //actualVariableNames.Add(variableName);
                            var allLines = from l in statementReferenceMasters.ToList()
                                    .FindAll(s => s.ResolvedStatement.Contains(variableName + ".") &&
                                                  s.BaseCommandId != 6
                                                  && s.ProjectId == projectId)
                                select l;
                            foreach (var t in allLines)
                            {
                                string m = t.ResolvedStatement.Replace(variableName, variableType);
                                string[] chkExpOnRightPart = m.Split('=');
                                if (m.Length > 0)
                                {
                                    var startIndex = m.IndexOf(variableType, StringComparison.Ordinal);
                                    Regex r = new Regex(@"[\w.]+");
                                    Match mssss = r.Match(m, startIndex);
                                    int endIndex = mssss.Success ? mssss.Index + variableType.Length : -1;
                                    if (variableType.Contains("."))
                                        variableType = variableType.Split('.').Last();
                                    string variableS = m.Substring(endIndex);
                                    bool result = variableS.ContainsAll("(", ")");
                                    if (variableS.StartsWith("."))
                                        variableS = variableS.Substring(1);
                                    var bCommandId = 0;
                                    if (result)
                                        bCommandId = 6;
                                    t.ResolvedStatement = m;
                                    t.VariableNameDeclared = variableName;
                                    t.ClassCalled = variableType;
                                    t.MethodCalled = bCommandId == 6 ? variableS : null;
                                    if (t.BaseCommandId == 1)
                                        t.OtherBaseCommandId = bCommandId;
                                    else
                                        t.BaseCommandId = bCommandId;
                                    t.ProjectId = projectId;
                                    t.FileMaster = null;
                                    t.ReferenceFileMaster = null;
                                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(t);
                                    continue;
                                }
                                foreach (var expPart in chkExpOnRightPart)
                                {
                                    if (!expPart.Contains(variableType)) continue;
                                    bool result = expPart.ContainsAll("(", ")");
                                    var bCommandId = 0;
                                    if (result)
                                        bCommandId = 6;
                                    string[] m2 = t.ResolvedStatement.Split(new[] {" " + variableName + "."},
                                        StringSplitOptions.RemoveEmptyEntries);
                                    t.ResolvedStatement = m;
                                    t.VariableNameDeclared = variableName;
                                    if (variableType.Contains("."))
                                        variableType = variableType.Split('.').Last();
                                    t.ClassCalled = variableType;
                                    t.MethodCalled = bCommandId == 6 ? m2.Last() : null;
                                    if (t.BaseCommandId == 1)
                                        t.OtherBaseCommandId = bCommandId;
                                    else
                                        t.BaseCommandId = bCommandId;
                                    t.ProjectId = projectId;
                                    t.FileMaster = null;
                                    t.ReferenceFileMaster = null;
                                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(t);
                                }
                            }
                        }
                    }

                    #endregion

                    #region region 6...

                    stringBuilder.AppendLine(
                        "Started process update statement for called external: (" + projectId + ")");
                    var alwaysConsiderNames = await baseCommandReferenceRepository
                        .GetEntityData<AlwaysConsiderClasses>(l => !string.IsNullOrEmpty(l.ClassName));

                    if (alwaysConsiderNames.Count > 0)
                    {
                        int loopCntr = 0;
                        var qryStat =
                            "SELECT * FROM StatementReferenceMaster  WHERE BaseCommandId != 6 AND ProjectId = " +
                            projectId + " AND ";
                        // stringBuilder.AppendLine("Select * from statementreferencemaster where ");
                        foreach (var clsList in alwaysConsiderNames)
                        {
                            try
                            {
                                if (loopCntr == 0)
                                {
                                    qryStat += " OriginalStatement LIKE '%" + clsList.ClassName +
                                               "%' OR OriginalStatement LIKE '%" + clsList.Assembly +
                                               "%'"; // AND BaseCommandId != 6 AND ProjectId = " + projectId + " ;";
                                    // stringBuilder.AppendLine(" OriginalStatement LIKE '%" + clsList.ClassName + "%' OR OriginalStatement LIKE '%" + clsList.Assembly + "%'");
                                }
                                else
                                {
                                    qryStat += "  OriginalStatement LIKE '%" + clsList.ClassName +
                                               "%' OR OriginalStatement LIKE '%" + clsList.Assembly + "%';";
                                    // stringBuilder.AppendLine(" AND OriginalStatement LIKE '%" + clsList.ClassName + "%' OR OriginalStatement LIKE '%" + clsList.Assembly + "%'");
                                }
                                loopCntr++;
                                // var aa = " AND BaseCommandId != 6 AND ProjectId = " + projectId + " ;";
                                // qryStat += " AND BaseCommandId != 6 AND ProjectId = " + projectId + " ;";

                                // stringBuilder.AppendLine(" AND BaseCommandId != 6 AND ProjectId = " + projectId + " ;");

                                var allCLassStatements = await _codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(qryStat);

                                var classList = allCLassStatements.ToList();
                                foreach (var cList in classList)
                                {
                                    if (cList.BaseCommandId != 1 && cList.BaseCommandId != 2 &&
                                        cList.BaseCommandId != 6)
                                    {
                                        cList.BaseCommandId = 6;
                                        // TODO: do it later as database is adjusted...
                                        if (!string.IsNullOrEmpty(clsList.ClassName))
                                        {
                                            cList.ClassCalled = clsList.ClassName;
                                        }
                                        cList.ProjectId = projectId;
                                        cList.FileMaster = null;
                                        cList.ReferenceFileMaster = null;
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cList).Wait();
                                    }
                                    else
                                    {
                                        cList.OtherBaseCommandId = 6;
                                        // TODO: do it later as database is adjusted...
                                        if (!string.IsNullOrEmpty(clsList.ClassName))
                                        {
                                            cList.ClassCalled = clsList.ClassName;
                                        }
                                        cList.ProjectId = projectId;
                                        cList.FileMaster = null;
                                        cList.ReferenceFileMaster = null;
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cList).Wait();
                                    }
                                }
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }
                        }
                    }

                    #endregion

                    stringBuilder.AppendLine(
                        "Started process update statement for called internal: (" + projectId + ")");

                    #region Assign call internal... // BaseCommandId = 5

                    // Firstly get all class names...

                    stringBuilder.AppendLine("Called stored procedure: SpGetAllClassNames(" + projectId + ",)");
                    object[] cParameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var classMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllClassNames", cParameters);
                    foreach (var cClass in classMaster)
                    {
                        try
                        {
                            object[] nParameters =
                            {
                                new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                                new MySqlParameter("@clsDeclared", MySqlDbType.VarChar)
                                {
                                    Value = cClass.ClassNameDeclared
                                },
                            };
                            var eachMethodStmtId = await _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethods", nParameters);
                            var allMethodsList = (from m in eachMethodStmtId select m.MethodName).ToList();
                            foreach (var statementMaster in eachMethodStmtId)
                            {
                                int stmtId = statementMaster.StatementId;
                                stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                                         stmtId + ", 8, 9)");
                                var allStatementsBlock = GetGenericBlock(stmtId, 8, 9);

                                foreach (var methodName in allMethodsList)
                                {
                                    if (string.IsNullOrEmpty(methodName)) continue;

                                    foreach (var block in allStatementsBlock)
                                    {
                                        if (block.MethodName == methodName) break;

                                        if (block.OriginalStatement.StartsWith(methodName))
                                        {
                                            block.BaseCommandId = 5;
                                            block.MethodCalled = methodName;
                                            block.FileMaster = null;
                                            block.ReferenceFileMaster = null;
                                            await _codeVortoService.StatementReferenceMasterRepository
                                                .UpdateItem(block);
                                        }
                                        else if (block.OriginalStatement.Contains(methodName))
                                        {
                                            Match match1 = Regex.Match(block.OriginalStatement,
                                                "(\\=" + methodName + "\\s\\()",
                                                RegexOptions.IgnoreCase);
                                            if (!match1.Success)
                                            {
                                                Match match2 = Regex.Match(block.OriginalStatement,
                                                    "(If\\s" + methodName + "\\s\\()",
                                                    RegexOptions.IgnoreCase);
                                                if (match2.Success)
                                                {
                                                    block.OtherBaseCommandId = 5;
                                                    block.MethodCalled = methodName;
                                                    block.FileMaster = null;
                                                    block.ReferenceFileMaster = null;
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(
                                                            block);
                                                }
                                                else
                                                {
                                                    Match match = Regex.Match(block.OriginalStatement,
                                                        "(\\=\\s" + methodName + "\\s\\()",
                                                        RegexOptions.IgnoreCase);
                                                    if (!match.Success) continue;

                                                    block.BaseCommandId = 5;
                                                    block.MethodCalled = methodName;
                                                    block.FileMaster = null;
                                                    block.ReferenceFileMaster = null;
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(
                                                            block);
                                                }
                                            }
                                        }
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

                    await ProcessForAllActionWorkflows(projectId).ConfigureAwait(false);

                    stringBuilder.AppendLine("Started process dump file viewsource into database: (" + projectId + ")");

                    #region Insert the data in ViewSourceMaster Table...

                    foreach (var file in fileMasterData)
                    {
                        try
                        {
                            if (file.FilePath.EndsWith(".txt")) continue;
                            if (!File.Exists(file.FilePath)) continue;
                            var allLines = File.ReadAllLines(file.FilePath);
                            var strBuilder = new StringBuilder();
                            foreach (var stmt in allLines)
                            {
                                strBuilder.AppendLine(stmt);
                            }
                            string fileLines = strBuilder.ToString();
                            var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                            var viewsourcedata = new ViewSourceMaster
                            {
                                ViewSourceId = 0,
                                FileId = file.FileId,
                                SourceData = fileLines,
                                ProjectId = file.ProjectId
                            };
                            await viewSourceMasterRepository.AddNewItem(viewsourcedata).ConfigureAwait(false);
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }

                    #endregion

                    //return Ok(projectId);
                    stringBuilder.AppendLine("Started executing next process: ProcessObjectConnectivityDiagram(" +
                                             projectId + ")");
                    LogMessage.WriteLogMessage(stringBuilder);
                    IHttpActionResult processResult = await ProcessObjectConnectivityDiagram(projectId);
                    var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                    string data = await dataContent.Content.ReadAsStringAsync();
                    return Ok(data);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<HttpResponseMessage> UpdateAlternateNames(int solutionId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    Console.WriteLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started updateAlternateName for solution: " + solutionId);

                    var allFiles = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.SolutionId == solutionId).ConfigureAwait(false);
                    Console.WriteLine("Total files to process: " + allFiles.Count);
                    foreach (var slnFile in allFiles)
                    {
                        Console.WriteLine("Current file: " + slnFile.FileId);
                        var file = slnFile;
                        var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.BaseCommandId == 1
                                                  && !string.IsNullOrEmpty(s.AlternateName)
                                                  && s.FileId == file.FileId).ConfigureAwait(false);

                        foreach (var statementReference in allStatements)
                        {
                            var reference = statementReference;
                            var secondTabStatement = await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                                .GetAllListItems(f => f.StatementId == reference.StatementId
                                                      && f.BaseCommandId == reference.BaseCommandId)
                                .ConfigureAwait(false);

                            Console.WriteLine("Total statements to process: " + secondTabStatement.Count);
                            foreach (var sTabStatement in secondTabStatement)
                            {
                                string alternateName = reference.AlternateName;
                                if (!string.IsNullOrEmpty(reference.AlternateName))
                                    alternateName = alternateName.Replace("THEN", "").Replace("Then", "");

                                sTabStatement.AlternateName = alternateName;

                                await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                                    .UpdateItem(sTabStatement).ConfigureAwait(false);
                            }
                        }
                        stringBuilder.AppendLine("Completed updateAlternateName for solution: " + solutionId);
                        Console.WriteLine("Done processing file: " + slnFile.FileId);
                        Console.WriteLine("=========================================================");
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    // return InternalServerError(exception);
                }
            }
            LogMessage.WriteLogMessage(stringBuilder);
            var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Alternate names applied successfully");
            return responseMessage;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessObjectConnectivityDiagram(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var projectMaster = await _codeVortoService.ProjectMasterRepository
                        .GetItem<ProjectMaster>(p => p.ProjectId == projectId).ConfigureAwait(false);
                    int solutionId = projectMaster.SolutionId ?? 5;
                    string sqlAllClasses = " Select * from statementreferencemaster Where BaseCommandId = 19 AND " +
                                           " ClassNameDeclared IS NOT NULL AND ProjectId IN ( Select ProjectId FROM ProjectMaster " +
                                           " WHERE SolutionId = " + solutionId + ");";
                    // Collect all required information
                    var allClassNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllClasses).ConfigureAwait(false);

                    var allCallExternals = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.ProjectId == projectId && s.BaseCommandId == 6
                                              && !string.IsNullOrEmpty(s.ClassCalled)).ConfigureAwait(false);


                    stringBuilder.AppendLine("Started to bind all nodes: (" + projectId + ")");

                    var generalRepositoryNodeDetails =
                   new GeneralRepository<ConnectivityStepReference>(new AppDbContext());
                    var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<ConnectivityStepReference>(
                        " SELECT * FROM connectivitystepreference ORDER BY NodeId DESC LIMIT 1;").ConfigureAwait(false);
                    var nodeId = 1;
                    if (workflowMaxNode.Any())
                    {
                        var id = workflowMaxNode[0].NodeId;
                        nodeId = id + 1;
                    }

                    var lstNodes = new List<Node>();
                    foreach (var classNameDeclared in allClassNameDeclared.Where(s => s.ProjectId == projectId))
                    {
                        string cNameDeclared = classNameDeclared.OriginalStatement;
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classNameDeclared.BusinessName + " [" + cNameDeclared + "]",
                            ShapeId = "RoundRect",
                            Height = "15",
                            Width = "100",
                            Color = "#00ffff", //Color.FromName("aqua").Name, // "#00ffff",
                            StatementId = classNameDeclared.StatementId,
                            BaseCommandId = classNameDeclared.BaseCommandId,
                            OriginalClassName = classNameDeclared.ClassNameDeclared
                        });
                    }

                    // Now loop through allCallExternals for ClassCalled as those might be missing in
                    // nodes list and will be colored as gray
                    foreach (var classCalled in allCallExternals)
                    {
                        if (lstNodes.Any(n => n.OriginalClassName == classCalled.ClassCalled)) continue;
                        string cNameDeclared = classCalled.ClassCalled.Split('.').LastOrDefault();
                        string nodeColor = "#c0c0c0";
                        if (allClassNameDeclared.Any(c => c.ClassNameDeclared == classCalled.ClassCalled))
                            nodeColor = "#00ffff";

                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = classCalled.BusinessName + " [" + cNameDeclared + "]",
                            ShapeId = "RoundRect",
                            Height = "18",
                            Width = "100",
                            Color = nodeColor, //Color.FromName("gray").Name, // "#00ffff",
                            StatementId = classCalled.StatementId,
                            BaseCommandId = classCalled.BaseCommandId,
                            OriginalClassName = classCalled.ClassCalled
                        });
                    }

                    // Now loop through all Nodes and check whether this class is used in call external...
                    // And create list of links to connect them
                    stringBuilder.AppendLine("Started to bind all links: (" + projectId + ")");
                    var lstLinks = new List<Link>();
                    foreach (var node in lstNodes)
                    {
                        string className = node.OriginalClassName;
                        var allDistinctClasses =
                            (from cls in allCallExternals where cls.ClassCalled == className select cls).ToList();
                        foreach (var callExt in allDistinctClasses)
                        {
                            var cName = (from c in allClassNameDeclared
                                         where c.FileId == callExt.FileId && c.BaseCommandId == 19
                                         select c).ToList();

                            // var originStatement = cName.Find(n => n.ClassNameDeclared == className);
                            var originNode = lstNodes.Find(n =>
                            {
                                var firstOrDefault = cName.FirstOrDefault();
                                return firstOrDefault != null && n.OriginalClassName == firstOrDefault.ClassNameDeclared;
                            });
                            var targetNode = lstNodes.Find(n => n.OriginalClassName == className);
                            if (originNode == null || targetNode == null) continue;
                            if (lstLinks.Any(s => s.Origin == originNode.Id && s.Target == targetNode.Id)) continue;

                            lstLinks.Add(new Link
                            {
                                Origin = originNode.Id,
                                Target = targetNode.Id,
                                LinkText = callExt.MethodCalled
                            });
                        }
                    }

                    #region Code to remove Missing Origin / missing target links

                    stringBuilder.AppendLine("Started to remove Missing Origin / missing target links: (" +
                                             projectId +
                                             ")");
                    List<int> missingTargets =
                        lstLinks.Select(x => x.Target)
                            .ToList()
                            .Except(lstNodes.Select(y => y.Id))
                            .ToList();
                    if (missingTargets.Count > 0)
                    {
                        foreach (int link in missingTargets)
                        {
                            lstLinks.RemoveAll(x => x.Target == link);
                        }
                    }

                    List<int> missingOrigins =
                        lstLinks.Select(x => x.Origin)
                            .ToList()
                            .Except(lstNodes.Select(y => y.Id))
                            .ToList();
                    if (missingOrigins.Count > 0)
                    {
                        foreach (int l in missingOrigins)
                        {
                            lstLinks.RemoveAll(x => x.Origin == l);
                        }
                    }

                    List<int> missingTargets1 =
                        lstLinks.Select(x => x.Target)
                            .ToList()
                            .Except(lstNodes.Select(y => y.Id))
                            .ToList();
                    if (missingTargets1.Count > 0)
                    {
                        foreach (int t in missingTargets1)
                        {
                            lstLinks.RemoveAll(x => x.Target == t);
                        }
                    }

                    List<int> missingOrigins1 =
                        lstLinks.Select(x => x.Origin)
                            .ToList()
                            .Except(lstNodes.Select(y => y.Id))
                            .ToList();
                    if (missingOrigins1.Count > 0)
                    {
                        foreach (int t in missingOrigins1)
                        {
                            lstLinks.RemoveAll(x => x.Origin == t);
                        }
                    }

                    #endregion

                    #region Code to remove no links to the nodes

                    stringBuilder.AppendLine("Started to remove no links to the nodes: (" + projectId +
                                             ")");
                    List<int> missingNodes =
                        lstNodes.Select(x => x.Id)
                            .ToList()
                            .Except(lstLinks.Select(y => y.Origin))
                            .ToList();
                    if (missingNodes.Count > 0)
                    {
                        foreach (int t in missingNodes)
                        {
                            List<int> iCnt = (from dd in lstLinks
                                              where dd.Target == t
                                              select dd.Target).ToList();
                            if (iCnt.Count == 0)
                                lstNodes.RemoveAll(x => x.Id == t);
                        }
                    }

                    #endregion

                    var flowChart = new FlowChart
                    {
                        Nodes = lstNodes,
                        Links = lstLinks
                    };
                    var workflowNodeDetails = (from mNodes in flowChart.Nodes
                                               let strName = mNodes.Name
                                               select new ConnectivityStepReference
                                               {
                                                   NodeId = mNodes.Id,
                                                   Name = strName.Trim(),
                                                   Shape = mNodes.ShapeId,
                                                   Color = mNodes.Color,
                                                   ParentId = mNodes.ParentId,
                                                   StatementId = mNodes.Id,
                                                   ProjectId = Convert.ToInt32(projectId)
                                               }).ToList();
                    var workflowLinkDetails = flowChart.Links.Select(mLinks => new ConnectivityLinkReferece
                    {
                        Origin = mLinks.Origin,
                        Target = mLinks.Target,
                        LinkText = mLinks.LinkText,
                        StatementId = mLinks.StatementId,
                        ProjectId = Convert.ToInt32(projectId)
                    }).ToList();

                    var flowchartObject = new WorkFlowObjectDictionaryData
                    {
                        Nodes = workflowNodeDetails,
                        Links = workflowLinkDetails
                    };

                    #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                    stringBuilder.AppendLine("Started inserting nodes and links into database: (" + projectId +
                                             ")");
                    if (flowchartObject.Nodes.Count > 0)
                    {
                        var nodes = flowchartObject.Nodes.ToList();
                        foreach (var mNodes in nodes)
                        {
                            string strName = mNodes.Name;
                            var nodesData = new ConnectivityStepReference
                            {
                                NodeId = mNodes.NodeId,
                                Name = strName.Trim(),
                                Shape = mNodes.Shape,
                                Color = mNodes.Color,
                                ParentId = mNodes.ParentId,
                                StatementId = mNodes.StatementId,
                                ProjectId = Convert.ToInt32(projectId)
                            };
                            await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData).ConfigureAwait(false);
                        }
                    }

                    if (flowchartObject.Links.Count > 0)
                    {
                        var links = flowchartObject.Links.ToList();
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
                                await _codeVortoService.ConnectivityLinkRefereceRepository
                                    .AddNewItem(liksData).ConfigureAwait(false);
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }
                        }
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                #endregion

                await ProcessPseudoCodeConversion(projectId).ConfigureAwait(false);

                stringBuilder.AppendLine("Started executing next process: GetAllStartingPoints(" + projectId + ")");
                LogMessage.WriteLogMessage(stringBuilder);
                await GetAllStartingPoints(projectId).ConfigureAwait(false);
                //await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
                
                // return Ok(flowChart);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForAllActionWorkflows(int projectId)
        {
            #region region 8...
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine("Started process to insert statement into actionworkflows:(" + projectId +
                                             ")");
                    // Action Workflows data...
                    var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                    Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                    var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                    int projectType = projectMasterData.ProjectConfigType;
                    stringBuilder.AppendLine("ProjectConfigType: (" + projectType + ")");
                    if (projectType == 3)
                    {
                        try
                        {
                            var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                            var projectConfigData = projectConfig.GetItem(projectType);
                            if (projectConfigData.ConfigFileId != projectType)
                                return Ok("Process completed successfully");
                            // Means the project type is windows application and we need to read starting point from that 
                            // files respective code behind file...
                            string configFileName = projectConfigData.ToString();
                            if (string.IsNullOrEmpty(configFileName)) return Ok("Process completed successfully");
                            var allConfigFiles = await _codeVortoService.FileMasterRepository.GetAllListItems(
                                f => f.FilePath.EndsWith(configFileName) && f.ProjectId == projectId);
                            foreach (var cFile in allConfigFiles)
                            {
                                var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(cFile.FilePath);
                                if (fileNameWithoutExtension == null) continue;
                                string className = fileNameWithoutExtension.Split('.')[0];
                                stringBuilder.AppendLine(
                                    "Started process to get block of class: GetAllMethodsForClass(" +
                                    className + "," + projectId + ")");
                                var genericBlocks = GetAllMethodsForClass(className, projectId);
                                foreach (var statement in genericBlocks)
                                {
                                    //if (statement.OriginalStatement.StartsWith("Private")) continue;

                                    var actionWorkflow = new ActionWorkflows
                                    {
                                        ActionWorkflowId = 0,
                                        CreatedBy = 1,
                                        EndPointOrService = "Service",
                                        MethodStatementId = statement.StatementId,
                                        OriginFileName = Path.GetFileName(cFile.FilePath),
                                        OriginFilePath = cFile.FilePath,
                                        ProjectId = projectId,
                                        OriginEventMethod = statement.MethodName,
                                        OriginObject = className,
                                        WorkflowName = statement.OriginalStatement,
                                        ServiceBaseAddress = null,
                                        ServiceContract = null,
                                        FileId = cFile.FileId,
                                        Processed = 1
                                    };
                                    //Predicate<ActionWorkflows> predicate =
                                    //    p => p.WorkflowName == actionWorkflow.WorkflowName
                                    //         && p.ProjectId == projectId;
                                    //var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                    //    .FindItem<ActionWorkflows>(predicate);
                                    //if (alreadyPresent == null)
                                    await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow)
                                        .ConfigureAwait(false);
                                }
                            }
                        }
                        catch (Exception exception)
                        {
                            LogMessage.WriteExceptionLogMessage(exception);
                        }
                    }
                    else
                    {
                        var allConfigFiles = await _codeVortoService.FileMasterRepository
                            .GetAllListItems(f => f.ProjectId == projectId
                                                  && (f.FilePath.EndsWith(".config") || f.FilePath.EndsWith(".svc")))
                            .ConfigureAwait(false);
                        allConfigFiles = allConfigFiles.Where(s => s.ProjectId == projectId).ToList();
                        stringBuilder.AppendLine("Configfiles: " + string.Join(",", allConfigFiles));
                        foreach (var configFile in allConfigFiles)
                        {
                            try
                            {
                                //if (configFile.FileName == "Web.config") continue;
                                if (configFile.FilePath.EndsWith(".svc"))
                                {
                                    string[] fileLines = File.ReadAllLines(configFile.FilePath);
                                    foreach (var line in fileLines)
                                    {
                                        if (!line.Contains("CodeBehind=")) continue;
                                        string codeClass =
                                            line.Split(new[] {"CodeBehind="}, StringSplitOptions.None)[1];
                                        var match = Regex.Match(codeClass, "\"(.*?)\"");
                                        if (!match.Success) continue;
                                        string val = match.Groups[0].Value;
                                        if (val.Length <= 0) continue;

                                        val = val.Substring(1);
                                        string s = val.Substring(0,
                                            val.IndexOf("\"", StringComparison.InvariantCulture));
                                        var actionWorkflow = new ActionWorkflows
                                        {
                                            ActionWorkflowId = 0,
                                            CreatedBy = 1,
                                            EndPointOrService = null,
                                            ProjectId = projectId,
                                            WorkflowName = s,
                                            ServiceBaseAddress = null,
                                            OriginFileName = Path.GetFileName(configFile.FilePath)
                                        };
                                        Predicate<ActionWorkflows> predicate =
                                            p => p.WorkflowName == actionWorkflow.WorkflowName
                                                 && p.ProjectId == projectId;
                                        var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                            .FindItem<ActionWorkflows>(predicate);
                                        if (alreadyPresent == null)
                                            await _codeVortoService.ActionWorkflowsRepository
                                                .AddNewItem(actionWorkflow);
                                    }
                                }
                                else
                                {
                                    var fileMaster =
                                        await _codeVortoService.FileMasterRepository.GetAllListItems(
                                            f => f.ProjectId == projectId);
                                    var xmlDoc = new XmlDocument();
                                    //xmlDoc.Load(@"D:\NjSoft\CodeVorto\KDOTTRS\TRSWcfService\app.config");
                                    xmlDoc.Load(configFile.FilePath);
                                    List<string> startListNames = new List<string>();
                                    List<string> sContracts = new List<string>();
                                    List<string> sAddress = new List<string>();
                                    Dictionary<string, string> otherEntryPoints = new Dictionary<string, string>();

                                    foreach (XmlNode serviceModel in xmlDoc.GetElementsByTagName("client"))
                                    {
                                        foreach (XmlNode node in serviceModel)
                                        {
                                            if (node.Attributes == null) continue;
                                            string startPoint = node.Attributes["name"].Value;
                                            string baseAddress = node.Attributes["address"].Value;
                                            otherEntryPoints.Add(startPoint, baseAddress);
                                        }
                                    }

                                    foreach (XmlNode childNodes in xmlDoc.GetElementsByTagName("services"))
                                    {
                                        foreach (XmlNode node in childNodes)
                                        {
                                            if (node.Attributes == null) continue;
                                            startListNames.Add(node.Attributes["name"] == null
                                                ? ""
                                                : node.Attributes["name"].Value);
                                            //XmlNode firstNd = node.FirstChild;
                                            //XmlNode lastNd = node.LastChild.FirstChild.ChildNodes[0];
                                            sContracts.Add(node.Attributes["contract"] == null
                                                ? ""
                                                : node.Attributes["contract"].Value);
                                            sAddress.Add(node.Attributes["address"] == null
                                                ? ""
                                                : node.Attributes["address"].Value);
                                        }
                                    }
                                    foreach (var sPoint in otherEntryPoints)
                                    {
                                        var actionWorkflow = new ActionWorkflows
                                        {
                                            ActionWorkflowId = 0,
                                            CreatedBy = 1,
                                            EndPointOrService = null,
                                            ProjectId = projectId,
                                            WorkflowName = sPoint.Key,
                                            ServiceBaseAddress = sPoint.Value,
                                            OriginFileName = Path.GetFileName(
                                                fileMaster.ToList().Find(f => f.FileId == configFile.FileId).FilePath)
                                        };
                                        Predicate<ActionWorkflows> predicate =
                                            p => p.WorkflowName == actionWorkflow.WorkflowName
                                                 && p.ProjectId == projectId;
                                        var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                            .FindItem<ActionWorkflows>(predicate);
                                        if (alreadyPresent == null)
                                            await _codeVortoService.ActionWorkflowsRepository
                                                .AddNewItem(actionWorkflow);
                                    }
                                    int listPosition = 0;
                                    foreach (var clsName in startListNames.Where(s => s != ""))
                                    {
                                        stringBuilder.AppendLine("Called stored procedure: SpGetClassMethods(" +
                                                                 clsName.Split('.').Last() + ",)");
                                        object[] parameters =
                                        {
                                            new MySqlParameter("@clsName", MySqlDbType.VarChar)
                                            {
                                                Value = clsName.Split('.').Last()
                                            }
                                        };
                                        var allMethodStatements = await _codeVortoService
                                            .StatementReferenceMasterRepository
                                            .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods",
                                                parameters);
                                        foreach (var statement in allMethodStatements)
                                        {
                                            if (statement.OriginalStatement.StartsWith("Private")) continue;

                                            var actionWorkflow = new ActionWorkflows
                                            {
                                                ActionWorkflowId = 0,
                                                CreatedBy = 1,
                                                EndPointOrService = "Service",
                                                MethodStatementId = statement.StatementId,
                                                OriginFileName =
                                                    Path.GetFileName(
                                                        fileMaster.ToList().Find(f => f.FileId == statement.FileId)
                                                            .FilePath),
                                                OriginFilePath =
                                                    fileMaster.ToList().Find(f => f.FileId == statement.FileId)
                                                        .FilePath,
                                                ProjectId = projectId,
                                                OriginEventMethod = statement.MethodName,
                                                OriginObject = clsName,
                                                WorkflowName = statement.OriginalStatement,
                                                ServiceBaseAddress = sAddress.ElementAt(listPosition),
                                                ServiceContract = sContracts.ElementAt(listPosition)
                                            };
                                            Predicate<ActionWorkflows> predicate =
                                                p => p.WorkflowName == actionWorkflow.WorkflowName
                                                     && p.ProjectId == projectId;
                                            var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                                .FindItem<ActionWorkflows>(predicate);
                                            if (alreadyPresent == null)
                                                await _codeVortoService.ActionWorkflowsRepository
                                                    .AddNewItem(actionWorkflow);
                                        }
                                        listPosition++;
                                    }
                                }
                            }
                            catch (Exception exception)
                            {
                                LogMessage.WriteExceptionLogMessage(exception);
                            }
                        }
                    }
                    LogMessage.WriteLogMessage(stringBuilder);
                    return Ok("Done");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }

            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                    ////var fileMasterRepository = new FileMasterRepository(new AppDbContext());
                    var generalRepository = new GeneralRepository<DeadDataTypes>(new AppDbContext());
                    var projectMaster = await _codeVortoService.ProjectMasterRepository.GetAllListItems(p => p.ProjectId == projectId);
                    var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(p => p.ProjectId == projectId);

                    List<Node> lstNodes = new List<Node>();
                    var objFlowChart = new FlowChart();
                    stringBuilder.AppendLine(
                        "Started executing next process: ProcessObjectConnectivityDiagram(" + projectId + ")");

                    #region Using DataBase entries and generated the flowchart

                    //var languageType = await _codeVortoService.ProjectMasterRepository
                    //    .GetDataFromSqlQuery<ProjectMaster>(
                    //    "select * from ProjectMaster where ProjectId=" + projectId + "");
                    var languageId = 0;

                    foreach (var languageTypeId in projectMaster)
                    {
                        languageId = languageTypeId.LanguageId;
                    }

                    #endregion

                    #region Runtime Created the flowchart

                    try
                    {
                        #region Add Nodes Logic

                        //var allClassData =
                        //    await statementReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                        //        "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared != 'null';");
                        stringBuilder.AppendLine("Started to bind all nodes:  (" + projectId +
                                                 ")");
                        var allClassData = statementMaster.Where(p => p.ProjectId == projectId
                                                                      && p.BaseCommandId == 19 &&
                                                                      p.ClassNameDeclared != "null").ToList();

                        foreach (var s in allClassData)
                        {
                            Node nodeObject = new Node
                            {
                                Id = s.StatementId,
                                Name = s.ClassNameDeclared,
                                ShapeId = "RoundRect",
                                Height = "15",
                                Width = "100",
                                Color = "#00ffff",
                                StatementId = s.StatementId,
                                BaseCommandId = s.BaseCommandId
                            };
                            lstNodes.Add(nodeObject);
                            var fileNewId = s.FileId;

                            //  Get the all Classes to file wise.
                            var allCallingClassData = await statementReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "select * from statementreferencemaster where BaseCommandId=7" +
                                    " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                    fileNewId + "';");
                            if (languageId == 1)
                            {
                                #region For VB

                                foreach (var c in allCallingClassData)
                                {
                                    var datatype = c.DataOrObjectType;
                                    var allDeadDatatypes = await generalRepository.GetDataFromSqlQuery<DeadDataTypes>(
                                        "SELECT * FROM deaddatatypes");
                                    int j = 0;
                                    for (int i = 0; i < allDeadDatatypes.Count; i++)
                                    {
                                        var keywordName = allDeadDatatypes[i].KaywordName;
                                        if (datatype.Contains(keywordName))
                                        {
                                            j++;
                                        }
                                    }
                                    if (j != 0) continue;
                                    // Check the class the present or not.
                                    // Class is present to add the color : Yellow
                                    // Class is not present to add the color : Gray 

                                    var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                    var checkClassPresentOrNot = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "SELECT * from statementreferencemaster where FileId In (" +
                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                            "') and classNameDeclared like '%" + dataObject + "%';");

                                    if (checkClassPresentOrNot.Count > 0)
                                    {
                                        Node nodeObject1 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#00ffff",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId
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
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
                                            FileId = c.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                                          where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject2);
                                        }
                                    }
                                }

                                #endregion
                            }
                            else
                            {
                                foreach (var c in allCallingClassData)
                                {
                                    var dataObject = c.ClassCalled;
                                    //var checkClassPresentOrNot = await statementReferenceRepository
                                    //    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    //        "select * from statementreferencemaster where BaseCommandId=6 and projectId=" + projectId + "" +
                                    //        " and ClassCalled='" + dataObject + "';");

                                    var checkClassPresentOrNot =
                                        statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                                   && p.ClassCalled == dataObject).ToList();

                                    if (checkClassPresentOrNot.Count > 0)
                                    {
                                        Node nodeObject1 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.ClassCalled,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#00ffff",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
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
                                            Id = c.StatementId,
                                            Name = c.ClassCalled,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
                                            FileId = c.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                                          where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject2);
                                        }
                                    }
                                }
                            }
                        }
                        objFlowChart.Nodes = lstNodes;

                        #endregion

                        if (lstNodes.Count > 0)
                        {
                            stringBuilder.AppendLine("Started to bind all links: (" + projectId +
                                                     ")");

                            #region Add Links

                            #region links variables

                            var lstLinks = new List<Link>();

                            #endregion

                            //var allClassDataLinks = await statementReferenceRepository
                            //    .GetDataFromSqlQuery<StatementReferenceMaster>(
                            //        "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared!='null'");
                            var allClassDataLinks =
                                statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 19
                                                           && p.ClassNameDeclared != "null").ToList();

                            if (languageId == 1)
                            {
                                #region Links For the VB

                                foreach (var s in allClassDataLinks)
                                {
                                    var iDofNode = s.StatementId;
                                    var fileId = s.FileId;
                                    // Get the file wise "Dim" started classes
                                    var allCallingClassDataLinks = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "select * from statementreferencemaster where BaseCommandId=7" +
                                            " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                            fileId + "';");

                                    foreach (var c in allCallingClassDataLinks)
                                    {
                                        var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                        var calledDataObject = c.DataOrObjectType;
                                        var statementId = c.StatementId;
                                        var allDeadDatatypesLinks = await generalRepository
                                            .GetDataFromSqlQuery<DeadDataTypes>(
                                                "SELECT * FROM deaddatatypes");

                                        // Check the dead datatype in classes or Not

                                        int j = 0;
                                        foreach (DeadDataTypes t in allDeadDatatypesLinks)
                                        {
                                            var keywordName = t.KaywordName;
                                            if (dataObject != null && dataObject.Contains(keywordName))
                                            {
                                                j++;
                                            }
                                        }
                                        if (j != 0) continue;
                                        // Check the Class id called or not.
                                        var checkClassPresentOrNotLink = await statementReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "SELECT * from statementreferencemaster where FileId In (" +
                                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                "') and classNameDeclared like '%" + dataObject + "%';");

                                        if (checkClassPresentOrNotLink.Count > 0)
                                        {
                                            foreach (var f in checkClassPresentOrNotLink)
                                            {
                                                var checkClassCallMethod = await statementReferenceRepository
                                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                        "SELECT * from statementreferencemaster where FileId In (" +
                                                        " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                        "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
                                                        "';");

                                                if (checkClassCallMethod.Count > 0)
                                                {
                                                    string mulMethodName = "";
                                                    var linkObject = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = f.StatementId,
                                                        BaseCommandId = f.BaseCommandId,
                                                        StatementId = f.StatementId
                                                    };
                                                    for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled != null)
                                                        {
                                                            if (
                                                                checkClassCallMethod[e].MethodCalled.Trim()
                                                                    .Contains("(") &&
                                                                checkClassCallMethod[e].MethodCalled.Trim()
                                                                    .EndsWith(")"))
                                                            {
                                                                if (
                                                                    !checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                    !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                                {
                                                                    int indexMethod =
                                                                        checkClassCallMethod[e].MethodCalled.IndexOf(
                                                                            "(",
                                                                            StringComparison.Ordinal);
                                                                    if (indexMethod > 0)
                                                                    {
                                                                        if (
                                                                            !mulMethodName.Contains(
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled.Substring(0,
                                                                                        indexMethod)))
                                                                        {
                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled.Substring(
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
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled))
                                                                        {
                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled;
                                                                            linkObject.LinkText =
                                                                                mulMethodName.Substring(1,
                                                                                    mulMethodName.Length - 1)
                                                                                    .Replace("))", "");
                                                                        }
                                                                    }


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
                                        else
                                        {
                                            #region Class Is not Calling to add the Class as Misssing Element

                                            var checkClassCallMethod = await statementReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "select * from statementreferencemaster where fileid= '" + fileId +
                                                    "' and statementId > '" + statementId + "' and" +
                                                    " statementId < (select statementId from statementreferencemaster where fileid= '" +
                                                    fileId + "' and statementId > '" + statementId + "'" +
                                                    " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                    " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                    "') and MethodCalled is not null;");


                                            if (checkClassCallMethod.Count == 0) // For Global variable
                                            {
                                                checkClassCallMethod = await statementReferenceRepository
                                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                        "select * from statementreferencemaster where fileid= '" +
                                                        fileId +
                                                        "' and statementId > '" + statementId +
                                                        "' and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                        " where fileid= '" + fileId + "' and statementId ='" +
                                                        statementId +
                                                        "') and MethodCalled is not null;");
                                            }


                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var l =
                                                    (from d in lstNodes where d.Name == c.ClassNameDeclared select d)
                                                        .ToList();
                                                Link linkObject1;
                                                if (l.Count > 0)
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = l.First().StatementId
                                                    };
                                                }
                                                else
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = statementId
                                                    };
                                                }
                                                //linkObject.target = c.StatementId;
                                                //foreach (var e in checkClassCallMethod)
                                                for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled != null)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                            checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                        {
                                                            if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                            {
                                                                int indexMethod =
                                                                    checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                        StringComparison.Ordinal);
                                                                if (indexMethod > 0)
                                                                {
                                                                    //linkObject1.linkText = e.MethodCalled.Substring(0, indexMethod);
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled
                                                                                .Substring(
                                                                                    0, indexMethod)))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled
                                                                                            .Substring(0, indexMethod);
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled;
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }

                                                                if (linkObject1.Origin == linkObject1.Target)
                                                                    continue;
                                                                if (e == checkClassCallMethod.Count - 1)
                                                                {
                                                                    lstLinks.Add(linkObject1);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            #endregion
                                        }
                                    }
                                }

                                #endregion
                            }
                            else
                            {
                                foreach (var s in allClassDataLinks)
                                {
                                    var iDofNode = s.StatementId;
                                    var dataObject = s.ClassCalled;
                                    // Check the Class id called or not.
                                    //var checkClassPresentOrNotLink = await statementReferenceRepository
                                    //   .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    //        "select * from statementreferencemaster where BaseCommandId=6 and projectId=" + projectId + "" +
                                    //        " and ClassCalled='" + dataObject + "';");

                                    var checkClassPresentOrNotLink = statementMaster.Where(p => p.ProjectId == projectId
                                                                                                && p.BaseCommandId == 6 &&
                                                                                                p.ClassCalled ==
                                                                                                dataObject).ToList();
                                    if (checkClassPresentOrNotLink.Count <= 0) continue;

                                    foreach (var f in checkClassPresentOrNotLink)
                                    {
                                        var checkClassCallMethod = await statementReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "SELECT * from statementreferencemaster where FileId In (" +
                                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                "') and BasecommandId= 6 and ClassCalled='" + dataObject +
                                                "';");

                                        if (checkClassCallMethod.Count <= 0) continue;
                                        string mulMethodName = "";
                                        var linkObject = new Link
                                        {
                                            Origin = iDofNode,
                                            Target = f.StatementId,
                                            BaseCommandId = f.BaseCommandId,
                                            StatementId = f.StatementId
                                        };
                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
                                        {
                                            if (checkClassCallMethod[e].MethodCalled != null)
                                            {
                                                if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                    checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                {
                                                    if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                        !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                    {
                                                        int indexMethod =
                                                            checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                StringComparison.Ordinal);
                                                        if (indexMethod > 0)
                                                        {
                                                            if (
                                                                !mulMethodName.Contains(
                                                                    checkClassCallMethod[e]
                                                                        .MethodCalled.Substring(0,
                                                                            indexMethod)))
                                                            {
                                                                mulMethodName = mulMethodName + ", " +
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled.Substring(0,
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
                                                                    checkClassCallMethod[e]
                                                                        .MethodCalled))
                                                            {
                                                                mulMethodName = mulMethodName + ", " +
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled;
                                                                linkObject.LinkText =
                                                                    mulMethodName.Substring(1,
                                                                        mulMethodName.Length - 1)
                                                                        .Replace("))", "");
                                                            }
                                                        }


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

                            objFlowChart.Links = lstLinks;

                            #region Code to remove Missing Origin / missing target links

                            stringBuilder.AppendLine("Started to remove Missing Origin / missing target links: (" +
                                                     projectId +
                                                     ")");
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

                            stringBuilder.AppendLine("Started to remove no links to the nodes: (" + projectId +
                                                     ")");
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
                                //Color = "#a9d18e"
                                Color = "#ffcc00"
                            };
                            lstNodes.Add(nodeObject);
                            tempI++;
                        }
                        //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
                        var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
                        //var enumerable = tempNodes.ToList();
                        foreach (var tNode in tempNodes)
                        {
                            foreach (var allNode in objFlowChart.Nodes)
                            {
                                var checkClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                                    .GetDataFromSqlQuery<ActionWorkflows>(
                                        " select * from actionworkFlows where ProjectId=" + projectId +
                                        " and EndPointOrService is not null group by OriginFileName;");

                                foreach (var chkNodes in checkClassInNodes)
                                {
                                    //string oName = rNodes.OriginObject.Split('.').LastOrDefault();
                                    if (allNode.Name == chkNodes.OriginFileName.Replace(".vb", ""))
                                    {
                                        int kk = allNode.StatementId;
                                        objFlowChart.Links.Add(new Link
                                        {
                                            Origin = tNode.Id,
                                            Target = kk
                                        });
                                    }
                                }
                            }
                        }

                        #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                        stringBuilder.AppendLine("Started inserting nodes and links into database: (" + projectId +
                                                 ")");
                        if (objFlowChart.Nodes.Count > 0)
                        {
                            var nodes = objFlowChart.Nodes.ToList();
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
                                catch (Exception exception)
                                {
                                    LogMessage.WriteExceptionLogMessage(exception);
                                }
                            }
                        }

                        #endregion
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion

                    stringBuilder.AppendLine("Started executing next process: GetAllStartingPoints(" + projectId + ")");
                    LogMessage.WriteLogMessage(stringBuilder);
                    // Start process for collecting workflow data here...
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
            }
            IHttpActionResult processResult = await GetAllStartingPoints(projectId);
            await processResult.ExecuteAsync(CancellationToken.None);
            return Ok("Connectivity diagram data process completed successfully.");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData_Old_10262016(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                    //var fileMasterRepository = new FileMasterRepository(new AppDbContext());
                    var generalRepository = new GeneralRepository<DeadDataTypes>(new AppDbContext());
                    List<Node> lstNodes = new List<Node>();
                    var objFlowChart = new FlowChart();

                    #region Using DataBase entries and generated the flowchart

                    var languageType = await _codeVortoService.ProjectMasterRepository
                        .GetDataFromSqlQuery<ProjectMaster>(
                            "select * from ProjectMaster where ProjectId=" + projectId + "");
                    var languageId = 0;

                    foreach (var languageTypeId in languageType)
                    {
                        languageId = languageTypeId.LanguageId;
                    }

                    #endregion

                    #region Runtime Created the flowchart

                    try
                    {
                        #region Add Nodes Logic

                        var allClassData =
                            await statementReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                "SELECT * FROM statementreferencemaster where ProjectId='" + projectId +
                                "' and BaseCommandId=19 and ClassNameDeclared != 'null';");
                        foreach (var s in allClassData)
                        {
                            Node nodeObject = new Node
                            {
                                Id = s.StatementId,
                                Name = s.ClassNameDeclared,
                                ShapeId = "RoundRect",
                                Height = "15",
                                Width = "100",
                                Color = "#00ffff",
                                StatementId = s.StatementId,
                                BaseCommandId = s.BaseCommandId
                            };
                            lstNodes.Add(nodeObject);
                            var fileNewId = s.FileId;

                            //  Get the all Classes to file wise.
                            var allCallingClassData = await statementReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "select * from statementreferencemaster where BaseCommandId=7" +
                                    " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                    fileNewId + "';");
                            if (languageId == 1)
                            {
                                #region For VB

                                foreach (var c in allCallingClassData)
                                {
                                    var datatype = c.DataOrObjectType;
                                    var allDeadDatatypes = await generalRepository.GetDataFromSqlQuery<DeadDataTypes>(
                                        "SELECT * FROM deaddatatypes");
                                    int j = 0;
                                    for (int i = 0; i < allDeadDatatypes.Count; i++)
                                    {
                                        var keywordName = allDeadDatatypes[i].KaywordName;
                                        if (datatype.Contains(keywordName))
                                        {
                                            j++;
                                        }
                                    }
                                    if (j != 0) continue;
                                    // Check the class the present or not.
                                    // Class is present to add the color : Yellow
                                    // Class is not present to add the color : Gray 

                                    var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                    var checkClassPresentOrNot = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "SELECT * from statementreferencemaster where FileId In (" +
                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                            "') and classNameDeclared like '%" + dataObject + "%';");
                                    if (checkClassPresentOrNot.Count > 0)
                                    {
                                        Node nodeObject1 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#00ffff",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId
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
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
                                            FileId = c.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                            where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                            select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject2);
                                        }
                                    }
                                }

                                #endregion
                            }
                            else
                            {
                                foreach (var c in allCallingClassData)
                                {
                                    var dataObject = c.ClassCalled;
                                    var checkClassPresentOrNot = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "select * from statementreferencemaster where BaseCommandId=6 and projectId=" +
                                            projectId + "" +
                                            " and ClassCalled='" + dataObject + "';");
                                    if (checkClassPresentOrNot.Count > 0)
                                    {
                                        Node nodeObject1 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.ClassCalled,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#00ffff",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
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
                                            Id = c.StatementId,
                                            Name = c.ClassCalled,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
                                            FileId = c.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                            where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                            select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject2);
                                        }
                                    }
                                }
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

                            var allClassDataLinks = await statementReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "SELECT * FROM statementreferencemaster where ProjectId='" + projectId +
                                    "' and BaseCommandId=19 and ClassNameDeclared!='null'");

                            if (languageId == 1)
                            {
                                #region Links For the VB

                                foreach (var s in allClassDataLinks)
                                {
                                    var iDofNode = s.StatementId;
                                    var fileId = s.FileId;
                                    // Get the file wise "Dim" started classes
                                    var allCallingClassDataLinks = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "select * from statementreferencemaster where BaseCommandId=7" +
                                            " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                            fileId + "';");

                                    foreach (var c in allCallingClassDataLinks)
                                    {
                                        var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                        var calledDataObject = c.DataOrObjectType;
                                        var statementId = c.StatementId;
                                        var allDeadDatatypesLinks = await generalRepository
                                            .GetDataFromSqlQuery<DeadDataTypes>(
                                                "SELECT * FROM deaddatatypes");

                                        // Check the dead datatype in classes or Not

                                        int j = 0;
                                        foreach (DeadDataTypes t in allDeadDatatypesLinks)
                                        {
                                            var keywordName = t.KaywordName;
                                            if (dataObject != null && dataObject.Contains(keywordName))
                                            {
                                                j++;
                                            }
                                        }
                                        if (j != 0) continue;
                                        // Check the Class id called or not.
                                        var checkClassPresentOrNotLink = await statementReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "SELECT * from statementreferencemaster where FileId In (" +
                                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                "') and classNameDeclared like '%" + dataObject + "%';");

                                        if (checkClassPresentOrNotLink.Count > 0)
                                        {
                                            foreach (var f in checkClassPresentOrNotLink)
                                            {
                                                var checkClassCallMethod = await statementReferenceRepository
                                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                        "SELECT * from statementreferencemaster where FileId In (" +
                                                        " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                        "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
                                                        "';");

                                                if (checkClassCallMethod.Count > 0)
                                                {
                                                    string mulMethodName = "";
                                                    var linkObject = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = f.StatementId,
                                                        BaseCommandId = f.BaseCommandId,
                                                        StatementId = f.StatementId
                                                    };
                                                    for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled != null)
                                                        {
                                                            if (checkClassCallMethod[e].MethodCalled.Trim()
                                                                    .Contains("(") &&
                                                                checkClassCallMethod[e].MethodCalled.Trim()
                                                                    .EndsWith(")"))
                                                            {
                                                                if (!checkClassCallMethod[e].MethodCalled
                                                                        .Contains("=") &&
                                                                    !checkClassCallMethod[e].MethodCalled
                                                                        .StartsWith("<"))
                                                                {
                                                                    int indexMethod =
                                                                        checkClassCallMethod[e].MethodCalled.IndexOf(
                                                                            "(",
                                                                            StringComparison.Ordinal);
                                                                    if (indexMethod > 0)
                                                                    {
                                                                        if (
                                                                            !mulMethodName.Contains(
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled.Substring(0,
                                                                                        indexMethod)))
                                                                        {
                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled
                                                                                                .Substring(0,
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
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled))
                                                                        {
                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled;
                                                                            linkObject.LinkText =
                                                                                mulMethodName.Substring(1,
                                                                                        mulMethodName.Length - 1)
                                                                                    .Replace("))", "");
                                                                        }
                                                                    }


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
                                        else
                                        {
                                            #region Class Is not Calling to add the Class as Misssing Element

                                            var checkClassCallMethod = await statementReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "select * from statementreferencemaster where fileid= '" + fileId +
                                                    "' and statementId > '" + statementId + "' and" +
                                                    " statementId < (select statementId from statementreferencemaster where fileid= '" +
                                                    fileId + "' and statementId > '" + statementId + "'" +
                                                    " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                    " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                    "') and MethodCalled is not null;");


                                            if (checkClassCallMethod.Count == 0) // For Global variable
                                            {
                                                checkClassCallMethod = await statementReferenceRepository
                                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                        "select * from statementreferencemaster where fileid= '" +
                                                        fileId +
                                                        "' and statementId > '" + statementId +
                                                        "' and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                        " where fileid= '" + fileId + "' and statementId ='" +
                                                        statementId +
                                                        "') and MethodCalled is not null;");
                                            }


                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var l =
                                                    (from d in lstNodes where d.Name == c.ClassNameDeclared select d)
                                                    .ToList
                                                    ();
                                                Link linkObject1;
                                                if (l.Count > 0)
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = l.First().StatementId
                                                    };
                                                }
                                                else
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = statementId
                                                    };
                                                }
                                                //linkObject.target = c.StatementId;
                                                //foreach (var e in checkClassCallMethod)
                                                for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled != null)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                            checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                        {
                                                            if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                            {
                                                                int indexMethod =
                                                                    checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                        StringComparison.Ordinal);
                                                                if (indexMethod > 0)
                                                                {
                                                                    //linkObject1.linkText = e.MethodCalled.Substring(0, indexMethod);
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled
                                                                                .Substring(
                                                                                    0, indexMethod)))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled
                                                                                            .Substring(0, indexMethod);
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                    mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled;
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                    mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }

                                                                if (linkObject1.Origin == linkObject1.Target)
                                                                    continue;
                                                                if (e == checkClassCallMethod.Count - 1)
                                                                {
                                                                    lstLinks.Add(linkObject1);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            #endregion
                                        }
                                    }
                                }

                                #endregion
                            }
                            else
                            {
                                foreach (var s in allClassDataLinks)
                                {
                                    var iDofNode = s.StatementId;
                                    var dataObject = s.ClassCalled;
                                    // Check the Class id called or not.
                                    var checkClassPresentOrNotLink = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "select * from statementreferencemaster where BaseCommandId=6 and projectId=" +
                                            projectId + "" +
                                            " and ClassCalled='" + dataObject + "';");

                                    if (checkClassPresentOrNotLink.Count <= 0) continue;

                                    foreach (var f in checkClassPresentOrNotLink)
                                    {
                                        var checkClassCallMethod = await statementReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "SELECT * from statementreferencemaster where FileId In (" +
                                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                "') and BasecommandId= 6 and ClassCalled='" + dataObject +
                                                "';");

                                        if (checkClassCallMethod.Count <= 0) continue;
                                        string mulMethodName = "";
                                        var linkObject = new Link
                                        {
                                            Origin = iDofNode,
                                            Target = f.StatementId,
                                            BaseCommandId = f.BaseCommandId,
                                            StatementId = f.StatementId
                                        };
                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
                                        {
                                            if (checkClassCallMethod[e].MethodCalled != null)
                                            {
                                                if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                    checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                {
                                                    if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                        !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                    {
                                                        int indexMethod =
                                                            checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                StringComparison.Ordinal);
                                                        if (indexMethod > 0)
                                                        {
                                                            if (
                                                                !mulMethodName.Contains(
                                                                    checkClassCallMethod[e]
                                                                        .MethodCalled.Substring(0,
                                                                            indexMethod)))
                                                            {
                                                                mulMethodName = mulMethodName + ", " +
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled.Substring(0,
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
                                                                    checkClassCallMethod[e]
                                                                        .MethodCalled))
                                                            {
                                                                mulMethodName = mulMethodName + ", " +
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled;
                                                                linkObject.LinkText =
                                                                    mulMethodName.Substring(1,
                                                                            mulMethodName.Length - 1)
                                                                        .Replace("))", "");
                                                            }
                                                        }


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
                                //Color = "#a9d18e"
                                Color = "#ffcc00"
                            };
                            lstNodes.Add(nodeObject);
                            tempI++;
                        }
                        //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
                        var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
                        //var enumerable = tempNodes.ToList();
                        foreach (var tNode in tempNodes)
                        {
                            foreach (var allNode in objFlowChart.Nodes)
                            {
                                var checkClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                                    .GetDataFromSqlQuery<ActionWorkflows>(
                                        " select * from actionworkFlows where ProjectId=" + projectId +
                                        " and EndPointOrService is not null group by OriginFileName;");

                                foreach (var chkNodes in checkClassInNodes)
                                {
                                    //string oName = rNodes.OriginObject.Split('.').LastOrDefault();
                                    if (allNode.Name == chkNodes.OriginFileName.Replace(".vb", ""))
                                    {
                                        int kk = allNode.StatementId;
                                        objFlowChart.Links.Add(new Link
                                        {
                                            Origin = tNode.Id,
                                            Target = kk
                                        });
                                    }
                                }
                            }
                        }

                        #region  Insert the ConnectivityStepReference & ConnectivityLinkReference

                        if (objFlowChart.Nodes.Count > 0)
                        {
                            var nodes = objFlowChart.Nodes.ToList();
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
                                catch (Exception exception)
                                {
                                    LogMessage.WriteExceptionLogMessage(exception);
                                }
                            }
                        }

                        #endregion
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion

                    // Start process for collecting workflow data here...
                    IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                    await processResult.ExecuteAsync(CancellationToken.None);
                    return Ok("Connectivity diagram data process completed successfully.");
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    stringBuilder.AppendLine("Started to collect all actionworkflow for project: (" + projectId + ")");
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
                        //if (workflow.ActionWorkflowId == 5) continue;
                        if (workflow.ProjectId == null) return NotFound();
                        int workFlowProjectId = workflow.ProjectId.Value;
                        stringBuilder.AppendLine("Started executing next process: GetWorkFlowWorkSpace(" + projectId + ")");
                        IHttpActionResult processResult =
                            await GetWorkFlowWorkSpace(workFlowProjectId, workflow.MethodStatementId);
                        await processResult.ExecuteAsync(CancellationToken.None);
                        workflow.FileMaster = null;
                        workflow.ProjectMaster = null;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow).ConfigureAwait(false);
                        //var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                        //string workflowData = await dataContent.Content.ReadAsStringAsync();
                    }

                    if (projectMaster.SolutionId != null)
                        await UpdateAlternateNames(projectMaster.SolutionId.Value).ConfigureAwait(false);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> Test(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var dbContext = new AppDbContext();
                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(
                        " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ");
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    //return Ok(projectId);
                    if (workflow.ProjectId == null) return NotFound();
                    int workFlowProjectId = workflow.ProjectId.Value;
                    var processResult =
                        await GetWorkFlowWorkSpace(workFlowProjectId, workflow.MethodStatementId);
                    var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                    string workflowJsonData = await dataContent.Content.ReadAsStringAsync();
                    var lstWorkflowData = JsonConvert.DeserializeObject<List<TreeViewData>>(workflowJsonData);
                    var actionWorkflow = lstWorkflowData[0].ActionWorkflows;
                    var workflowNodeDetails =
                        lstWorkflowData[0].Nodes.Select(node => new WorkflowNodeDetails
                        {
                            BaseCommandId = node.BaseCommandId,
                            BusinessDescription = node.BusinessDescription,
                            BusinessName = node.BusinessName,
                            ChildId = node.ChildId,
                            Color = node.Color,
                            FileId = node.FileId,
                            Height = node.Height,
                            Id = node.Id,
                            Name = node.Name,
                            ParentId = node.ParentId,
                            ShapeId = node.ShapeId,
                            StatementId = node.StatementId,
                            StatementTypeId = node.StatementTypeId,
                            Width = node.Width,
                            ActionWorkflowId = actionWorkflow.ActionWorkflowId,
                            ProjectId = workFlowProjectId,
                            WorkflowStartStatementId = workflow.MethodStatementId
                        }).ToList();

                    var workflowLinkDetails = lstWorkflowData[0].Links.Select(link => new WorkflowLinkDetails
                    {
                        StatementId = link.StatementId,
                        BaseCommandId = link.BaseCommandId,
                        BusinessDescription = link.BusinessDescription,
                        BusinessName = link.BusinessName,
                        LinkText = link.LinkText,
                        Origin = link.Origin,
                        Target = link.Target,
                        ActionWorkflowId = actionWorkflow.ActionWorkflowId,
                        ProjectId = workFlowProjectId,
                        WorkflowStartStatementId = workflow.MethodStatementId
                    }).ToList();

                    var workflowTreeviewSecondTabDetails
                        = lstWorkflowData[0].TreeViewList.Select(m => new WorkflowTreeviewSecondTabDetails
                        {
                            ActualStatementId = m.ActualStatementId,
                            BaseCommandId = m.BaseCommandId,
                            ClassCalled = m.ClassCalled,
                            GraphId = m.GraphId,
                            GraphName = m.GraphName,
                            HasChild = m.HasChild ? "true" : "false",
                            MethodCalled = m.MethodCalled,
                            ParentId = m.ParentId,
                            PrimaryCommandId = m.PrimaryCommandId,
                            SpriteCssClass = m.SpriteCssClass,
                            ActionWorkflowId = actionWorkflow.ActionWorkflowId,
                            ProjectId = workFlowProjectId,
                            WorkflowStartStatementId = workflow.MethodStatementId
                        }).ToList();
                    var workflowTreeviewTabFirstDetails =
                        lstWorkflowData[1].TreeViewList.Select(m => new WorkflowTreeviewTabFirstDetails
                        {
                            ActualStatementId = m.ActualStatementId,
                            BaseCommandId = m.BaseCommandId,
                            ClassCalled = m.ClassCalled,
                            GraphId = m.GraphId,
                            GraphName = m.GraphName,
                            HasChild = m.HasChild ? "true" : "false",
                            MethodCalled = m.MethodCalled,
                            ParentId = m.ParentId,
                            PrimaryCommandId = m.PrimaryCommandId,
                            SpriteCssClass = m.SpriteCssClass,
                            ActionWorkflowId = actionWorkflow.ActionWorkflowId,
                            ProjectId = workFlowProjectId,
                            WorkflowStartStatementId = workflow.MethodStatementId
                        }).ToList();
                    dbContext.WorkflowNodeDetails.AddRange(workflowNodeDetails);
                    await dbContext.SaveChangesAsync();
                    dbContext.WorkflowLinkDetails.AddRange(workflowLinkDetails);
                    await dbContext.SaveChangesAsync();
                    dbContext.WorkflowTreeviewSecondTabDetails.AddRange(workflowTreeviewSecondTabDetails);
                    await dbContext.SaveChangesAsync();
                    dbContext.WorkflowTreeviewTabFirstDetails.AddRange(workflowTreeviewTabFirstDetails);
                    await dbContext.SaveChangesAsync();
                }
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> TestNew(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    #region region 8...

                    // Action Workflows data...
                    var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                    Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                    var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                    int projectType = projectMasterData.ProjectConfigType;
                    if (projectType == 3)
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
                            var genericBlocks = GetAllMethodsForClass(className, projectId);
                            foreach (var statement in genericBlocks)
                            {
                                ActionWorkflows actionWorkflow = new ActionWorkflows
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
                                Predicate<ActionWorkflows> predicate =
                                    p => p.WorkflowName == actionWorkflow.WorkflowName
                                         && p.ProjectId == projectId;
                                var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                    .FindItem<ActionWorkflows>(predicate);
                                if (alreadyPresent == null)
                                    await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                            }
                        }
                    }
                    else
                    {
                        var allConfigFiles =
                            await _codeVortoService.FileMasterRepository.GetAllItems(
                                f => f.ProjectId == projectId && (f.FilePath.EndsWith(".config")
                                                                  || f.FilePath.EndsWith(".svc")));
                        allConfigFiles = allConfigFiles.ToList().Where(s => s.ProjectId == projectId);
                        foreach (var configFile in allConfigFiles)
                        {
                            //if (configFile.FileName == "Web.config") continue;
                            if (configFile.FilePath.EndsWith(".svc"))
                            {
                                string[] fileLines = File.ReadAllLines(configFile.FilePath);
                                foreach (var line in fileLines)
                                {
                                    if (!line.Contains("CodeBehind=")) continue;
                                    string codeClass = line.Split(new[] {"CodeBehind="}, StringSplitOptions.None)[1];
                                    var match = Regex.Match(codeClass, "\"(.*?)\"");
                                    if (!match.Success) continue;
                                    string val = match.Groups[0].Value;
                                    if (val.Length <= 0) continue;

                                    val = val.Substring(1);
                                    string s = val.Substring(0, val.IndexOf("\"", StringComparison.InvariantCulture));
                                    var actionWorkflow = new ActionWorkflows
                                    {
                                        ActionWorkflowId = 0,
                                        CreatedBy = 1,
                                        EndPointOrService = null,
                                        ProjectId = projectId,
                                        WorkflowName = s,
                                        ServiceBaseAddress = null,
                                        OriginFileName = Path.GetFileName(configFile.FilePath)
                                    };
                                    Predicate<ActionWorkflows> predicate =
                                        p => p.WorkflowName == actionWorkflow.WorkflowName
                                             && p.ProjectId == projectId;
                                    var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                        .FindItem<ActionWorkflows>(predicate);
                                    if (alreadyPresent == null)
                                        await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                                }
                            }
                            else
                            {
                                var filePaths =
                                    await _codeVortoService.FileMasterRepository.GetAllItems(
                                        f => f.ProjectId == projectId);
                                var fileMaster = filePaths as FileMaster[] ?? filePaths.ToArray();
                                XmlDocument xmlDoc = new XmlDocument();
                                //xmlDoc.Load(@"D:\NjSoft\CodeVorto\KDOTTRS\TRSWcfService\app.config");
                                xmlDoc.Load(configFile.FilePath);
                                List<string> startListNames = new List<string>();
                                List<string> sContracts = new List<string>();
                                List<string> sAddress = new List<string>();
                                Dictionary<string, string> otherEntryPoints = new Dictionary<string, string>();

                                foreach (XmlNode serviceModel in xmlDoc.GetElementsByTagName("client"))
                                {
                                    foreach (XmlNode node in serviceModel)
                                    {
                                        if (node.Attributes == null) continue;
                                        string startPoint = node.Attributes["name"].Value;
                                        string baseAddress = node.Attributes["address"].Value;
                                        otherEntryPoints.Add(startPoint, baseAddress);
                                    }
                                }

                                foreach (XmlNode childNodes in xmlDoc.GetElementsByTagName("services"))
                                {
                                    foreach (XmlNode node in childNodes)
                                    {
                                        if (node.Attributes == null) continue;
                                        startListNames.Add(node.Attributes["name"] == null
                                            ? ""
                                            : node.Attributes["name"].Value);
                                        //XmlNode firstNd = node.FirstChild;
                                        //XmlNode lastNd = node.LastChild.FirstChild.ChildNodes[0];
                                        sContracts.Add(node.Attributes["contract"] == null
                                            ? ""
                                            : node.Attributes["contract"].Value);
                                        sAddress.Add(node.Attributes["address"] == null
                                            ? ""
                                            : node.Attributes["address"].Value);
                                    }
                                }
                                foreach (var sPoint in otherEntryPoints)
                                {
                                    var actionWorkflow = new ActionWorkflows
                                    {
                                        ActionWorkflowId = 0,
                                        CreatedBy = 1,
                                        EndPointOrService = null,
                                        ProjectId = projectId,
                                        WorkflowName = sPoint.Key,
                                        ServiceBaseAddress = sPoint.Value,
                                        OriginFileName = Path.GetFileName(
                                            fileMaster.ToList().Find(f => f.FileId == configFile.FileId).FilePath)
                                    };
                                    Predicate<ActionWorkflows> predicate =
                                        p => p.WorkflowName == actionWorkflow.WorkflowName
                                             && p.ProjectId == projectId;
                                    var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                        .FindItem<ActionWorkflows>(predicate);
                                    if (alreadyPresent == null)
                                        await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                                }
                                int listPosition = 0;
                                foreach (var clsName in startListNames.Where(s => s != ""))
                                {
                                    object[] parameters =
                                    {
                                        new MySqlParameter("@clsName", MySqlDbType.VarChar)
                                        {
                                            Value = clsName.Split('.').Last()
                                        }
                                    };
                                    var allMethodStatements = await _codeVortoService.StatementReferenceMasterRepository
                                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods",
                                            parameters);
                                    foreach (var statement in allMethodStatements)
                                    {
                                        //if (statement.OriginalStatement.StartsWith("Private")) continue;

                                        var actionWorkflow = new ActionWorkflows
                                        {
                                            ActionWorkflowId = 0,
                                            CreatedBy = 1,
                                            EndPointOrService = "Service",
                                            MethodStatementId = statement.StatementId,
                                            OriginFileName =
                                                Path.GetFileName(
                                                    fileMaster.ToList().Find(f => f.FileId == statement.FileId)
                                                        .FilePath),
                                            OriginFilePath =
                                                fileMaster.ToList().Find(f => f.FileId == statement.FileId).FilePath,
                                            ProjectId = projectId,
                                            OriginEventMethod = statement.MethodName,
                                            OriginObject = clsName,
                                            WorkflowName = statement.OriginalStatement,
                                            ServiceBaseAddress = sAddress.ElementAt(listPosition),
                                            ServiceContract = sContracts.ElementAt(listPosition)
                                        };
                                        Predicate<ActionWorkflows> predicate =
                                            p => p.WorkflowName == actionWorkflow.WorkflowName
                                                 && p.ProjectId == projectId;
                                        var alreadyPresent = await _codeVortoService.ActionWorkflowsRepository
                                            .FindItem<ActionWorkflows>(predicate);
                                        if (alreadyPresent == null)
                                            await _codeVortoService.ActionWorkflowsRepository
                                                .AddNewItem(actionWorkflow);
                                    }
                                    listPosition++;
                                }
                            }
                        }
                    }
                    return Ok("Done");

                    #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
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
        public async Task<HttpResponseMessage> ProcessPseudoCodeConversion(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var stringBuilder = new StringBuilder();
                    Expression<Func<StatementReferenceMaster, bool>> pExpression = e =>
                        e.ProjectId == projectId && e.BaseCommandId == 1 && string.IsNullOrEmpty(e.AlternateName);

                    var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(pExpression).ConfigureAwait(false);
                    var allRegEx = await _codeVortoService.RegexPatternMasterRepository
                        .GetAllListItems(e => e.BaseCommandId == 1).ConfigureAwait(false);
                    allRegEx = allRegEx.OrderByDescending(s => s.RegexPattern.Length).ToList();
                    var allAlternateStatements = new List<StatementReferenceMaster>();
                    var regexOptions = RegexOptions.CultureInvariant;
                    int loopCnt = 0;
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    Console.WriteLine(
                        "========================================================================================");
                    stringBuilder.AppendLine("Started Pseudo code conversion for project: " + projectId);
                    Console.WriteLine("Started Pseudo code conversion for project: " + projectId);

                    foreach (var regEx in allRegEx)
                    {
                        loopCnt++;
                        if (loopCnt > 1)
                            statementReferenceMasterData =
                                statementReferenceMasterData.Where(s => string.IsNullOrEmpty(s.AlternateName)).ToList();

                        Regex regex = new Regex(regEx.RegexPattern, regexOptions);
                        foreach (var statementReference in statementReferenceMasterData)
                        {
                            int index = 1;
                            foreach (Match match in regex.Matches(statementReference.OriginalStatement))
                            {
                                int groupIndex = -1;
                                string groupValue = string.Empty;
                                foreach (Group group in match.Groups)
                                {
                                    groupIndex++;
                                    if (groupIndex == 0) continue;
                                    if (string.IsNullOrEmpty(group.Value)) continue;
                                    groupValue += group.Value;
                                    if (groupIndex == match.Groups.Count - 2) break;
                                }
                                string alternateName =
                                    regEx.AlternateCodeRepresentation.Replace("<<" + index + ">>", groupValue);

                                if (!string.IsNullOrEmpty(alternateName))
                                    alternateName = ReplaceString(alternateName);

                                statementReference.AlternateName = alternateName;
                                allAlternateStatements.Add(statementReference);
                                index++;
                            }
                        }
                    }

                    stringBuilder.AppendLine("PseudoCode Statements to update: " + allAlternateStatements.Count);
                    Console.WriteLine("PseudoCode Statements to update: " + allAlternateStatements.Count);

                    foreach (var statementMaster in allAlternateStatements)
                    {
                        statementMaster.FileMaster = null;
                        statementMaster.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementMaster)
                            .ConfigureAwait(false);
                    }

                    stringBuilder.AppendLine("Completed Pseudo code conversion for project: " + projectId);
                    Console.WriteLine("Completed Pseudo code conversion for project: " + projectId);
                    stringBuilder.AppendLine(
                        "========================================================================================");
                    Console.WriteLine(
                        "========================================================================================");
                    LogMessage.WriteLogMessage(stringBuilder);
                    var reMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
                    return reMessage;
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    // return InternalServerError(exception);
                }
                var responseMessage = Request.CreateResponse(HttpStatusCode.OK, "Pseudo code applied successfully");
                return responseMessage;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace(int projectId, int stmtId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    #region Start pre-process

                    //var lstTreeViewData = new List<TreeViewData>();
                    List<TreeView> lstTreeView = new List<TreeView>();
                    List<TreeView> secondTab = new List<TreeView>();

                    var startClasses =
                        await _codeVortoService.ActionWorkflowsRepository
                            .GetAllListItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
                    var clsName =
                        (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList()
                            .First();
                    string cNameDeclared = clsName.Split('.').LastOrDefault();
                    Expression<Func<StatementReferenceMaster, bool>> expression =
                        master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName
                                                                    || master.ClassNameDeclared == cNameDeclared);
                    var baseStatementMaster = await
                        _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(
                            expression, 1);
                    //lstStatementMaster.Add(baseStatementMaster);
                    //lstNodes.Add(new Node { Name = clsName, Id = 1111, ParentId = 0, ShapeId = "RoundRect", Color = "#28c965", Height = "15", Width = clsName.Length.ToString() });
                    stringBuilder.AppendLine("Started process for get block: GetMethodBlock(" + stmtId + ")");
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
                    foreach (var treeItem in lstTreeView)
                    {
                        if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                            continue;

                        auto++;
                        stringBuilder.AppendLine("Started process for called internal: GetCallInternalDetails(" +
                                                 projectId + "," + treeItem.StatementReferenceMaster.FileId +
                                                 ")");
                        copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView,
                            projectId,
                            treeItem.StatementReferenceMaster.FileId, indentLevel, ref auto, ref treeNodeId);
                        if (!copyOfLstTreeView.Any()) continue;
                        treeItem.HasChild = true;
                    }

                    var newList = copyOfLstTreeView.ToList();
                    foreach (var treeItem in newList)
                    {
                        if (treeItem.BaseCommandId != 6) continue;
                        // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                        auto++;
                        stringBuilder.AppendLine("Started process for called external: GetCallExternalDetails(" +
                                                 projectId + "," + treeItem.StatementReferenceMaster.FileId +
                                                 ")");
                        copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView,
                            projectId, treeItem.StatementReferenceMaster.FileId, indentLevel, ref auto, ref treeNodeId);
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
                    lstTreeView = copyOfLstTreeView.ToList();
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
                                treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] +
                                                     ";'>" +
                                                     treeItem.GraphName +
                                                     "</span>";
                                stringBuilder.AppendLine(
                                    "Started process for assign color to childnodes: AssignColorsToChildNodes(" +
                                    projectId +
                                    ")");
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
                    foreach (var treeItem in copyOfLstTreeView)
                    {
                        indexPosition++;
                        if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
                        if (treeItem.BaseCommandId != 1 && treeItem.PrimaryCommandId != 1) continue;

                        var treeViewList = new List<TreeView>();
                        for (int i = indexPosition; i < lstTreeView.Count; i++)
                        {
                            treeViewList.Add(lstTreeView[i]);
                            if (lstTreeView[i].BaseCommandId == 1)
                                ifCounter++;
                            if (lstTreeView[i].BaseCommandId == 2)
                                ifCounter--;
                            if (ifCounter == 0)
                                break;
                        }
                        int curIndentLevel = treeViewList.First().IndentLevel;
                        var prevParentId = treeViewList.First().ParentId;
                        var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
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
                    int loopCounter = 0;
                    foreach (var treeItem in copyOfLstTreeView)
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
                        .Where(item => (item.BaseCommandId == 6)
                                       || (item.BaseCommandId == 8)
                                       || (item.BaseCommandId == 10)
                                       || (item.BaseCommandId == 1)
                                       || (item.BaseCommandId == 25)
                                       || (item.BaseCommandId == 5)
                                       || (item.BaseCommandId == 30)
                                       || (item.BaseCommandId == 3)
                                       || (item.BaseCommandId == 4)
                                       || (item.BaseCommandId == 45)));
                    var tempList =
                    (from d in secondTab
                        where (d.BaseCommandId == 1)
                              || (d.BaseCommandId == 10)
                              || (d.BaseCommandId == 25) || (d.BaseCommandId == 3)
                        select d).ToList();
                    foreach (var sTab in tempList)
                    {
                        var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
                        secondTab.AddRange(childItems);
                    }

                    secondTab = secondTab.Distinct().ToList();
                    secondTab = secondTab.OrderBy(k => k.NodeId).ToList();

                    #endregion

                    #region

                    var allSeqListItems = new List<TreeView>();
                    foreach (var curItem in secondTab)
                    {
                        allSeqListItems.Add(curItem);
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                        stringBuilder.AppendLine("Started process for attchchilditems: AttachChildItems(" + projectId +
                                                 ")");
                        foreach (var cItem in childItems)
                        {
                            allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                        }
                        break;
                    }
                    allSeqListItems = allSeqListItems.DistinctBy().ToList();

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
                                ShapeId = "Decision",
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
                            stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf(" +
                                                     projectId + ")");
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, ref nodeId,
                                    ref linkSeqNumber, true);
                            }

                            #endregion
                        }
                        else if (curItem.BaseCommandId == 6)
                        {
                            #region BaseCommandId == 6

                            var item = curItem;
                            string nodeColor = "#c0c0c0";
                            var ss = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                            var sss = item.StatementReferenceMaster.ClassCalled;
                            Expression<Func<StatementReferenceMaster, bool>> stmtExpression = master =>
                                master.BaseCommandId == 19 &&
                                (master.ClassNameDeclared == ss || master.ClassNameDeclared == sss);

                            var classNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
                                .GetAllListItems(stmtExpression);
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
                            stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                                     projectId + ")");
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView,
                                    ref nodeId,
                                    ref linkSeqNumber, true);
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
                                nodeColor = "#f5bd6a";
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
                            stringBuilder.AppendLine(
                                "Started process for called internal: ProcessChildItemsCallInternal(" +
                                projectId + ")");
                            foreach (var cItem in childItems)
                            {
                                treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView,
                                    ref nodeId, ref linkSeqNumber, true);
                            }

                            #endregion
                        }
                    }
                    stringBuilder.AppendLine("Started process for removenodes: RemoveMultipleLinks(" + projectId + ")");
                    treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                    stringBuilder.AppendLine("Started process for removehangingnodes: RemoveHangingNodes(" + projectId +
                                             ")");
                    treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                    // RemoveNodes(treeView.Nodes, treeView.Links);

                    #endregion

                    // ReSharper disable once RedundantAssignment
                    var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                        .GetEntityData<ActionWorkflows>(s => s.MethodStatementId == statementId && s.ProjectId == projectId);

                    #region Add to database table...

                    stringBuilder.AppendLine("Started to insert workflownodes into database: (" + projectId + ")");

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
                    stringBuilder.AppendLine("Started to insert workflowlink into database: (" + projectId + ")");
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

                    stringBuilder.AppendLine("Started to insert workflowtreeviewsecondtabdetails into database: (" +
                                             projectId + ")");
                    var lstWorkflowTreeviewSecondTabDetails =
                        secondTab.Select(sTab => new WorkflowTreeviewSecondTabDetails
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
                            IndentLevel = sTab.IndentLevel,
                            ProgramId = 0
                        }).ToList();

                    var generalRepositoryWorkflowTreeviewSecondTabDetails =
                        new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                    await
                        generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                    #region To Updated the GraphName against the BaseCommandId

                    var statementSecondTab = await generalRepositoryWorkflowTreeviewSecondTabDetails
                        .GetAllListItems(p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

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

                    stringBuilder.AppendLine("Started to insert workflowtreeviewfirsttabdetails into database: (" +
                                             projectId + ")");
                    var lsTreeviewTabFirstDetails = copyOfLstTreeView.Select(fTab => new WorkflowTreeviewTabFirstDetails
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

                    LogMessage.WriteLogMessage(stringBuilder);
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                return Ok("Workflow data collected successfully");
            }
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            var stringBuilder = new StringBuilder();
            try
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf(" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, ref nodeId,
                            ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 6)
                {
                    #region PrimaryCommandId == 6

                    var item = treeView;
                    string nodeColor = "#c0c0c0";
                    string cCalled = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    string andCondition = " BaseCommandId = 19 AND " +
                                          " ( ClassNameDeclared = '" + cCalled + "' OR " +
                                          " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";

                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             ",)");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;
                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                            ref nodeId,
                            ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 10)
                {
                    #region BaseCommandId == 10

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
                    string width = _clsUniverseBasic.CalculateWidth(condition.Length);
                    string height = _clsUniverseBasic.CalculateHeight(condition.Length);
                    var node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "Decision2",
                        Name = condition,
                        Color = "#ff6600",
                        Width = width,
                        Height = height,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" + projectId +
                                             ")");
                    try
                    {
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            var stringBuilder = new StringBuilder();
            try
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf(" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, ref nodeId,
                            ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 6)
                {
                    #region PrimaryCommandId == 6

                    var item = treeView;
                    string nodeColor = "#c0c0c0";
                    string cCalled = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    string andCondition = " BaseCommandId = 19 AND " +
                                          " ( ClassNameDeclared = '" + cCalled + "' OR " +
                                          " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             "," +
                                             andCondition + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;
                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for called internal: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                            ref nodeId,
                            ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 10)
                {
                    #region BaseCommandId == 10

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
                    string width = _clsUniverseBasic.CalculateWidth(condition.Length);
                    string height = _clsUniverseBasic.CalculateHeight(condition.Length);
                    var node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "Decision2",
                        Name = condition,
                        Color = "#ff6600",
                        Width = width,
                        Height = height,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" + projectId +
                                             ")");
                    try
                    {
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, ref int nodeId, ref int linkSeqNumber, bool continous)
        {
            var stringBuilder = new StringBuilder();
            string width = "0";
            string height = "0";
            try
            {
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf(" + projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, ref nodeId,
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
                    string cCalled = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                    string andCondition = " BaseCommandId = 19 AND " +
                                          " ( ClassNameDeclared = '" + cCalled + "' OR " +
                                          " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "' ); ";
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             "," +
                                             andCondition + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;

                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for called internal: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                            ref nodeId,
                            ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 10)
                {
                    #region BaseCommandId == 10

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
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
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" + projectId +
                                             ")");
                    try
                    {
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsElse(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            var width = "0";
            var height = "0";
            try
            {
                if ((treeView.BaseCommandId == 1) || (treeView.PrimaryCommandId == 1))
                {
                    #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1

                    string condition;
                    var ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                    if (ifPart != null && ifPart.Contains("THEN"))
                        condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                    else
                        condition = ifPart;

                    nodeId++;

                    if (condition != null)
                    {
                        var charCountOfText = condition.Length;
                        width = _clsUniverseBasic.CalculateWidth(charCountOfText);
                        height = _clsUniverseBasic.CalculateHeight(charCountOfText);
                    }

                    var node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "Decision2",
                        Name = condition,
                        Color = "#ff6600",
                        Width = width,
                        Height = height,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                        GroupName = treeView.GroupName,
                        GroupId = treeView.GroupId
                    };
                    treeViewData.Nodes.Add(node);

                    var m = treeView.StatementReferenceMaster.MethodCalled;
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
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifItems: ProcessChildItemsIf(" + projectId + ")");
                    foreach (var cItem in childItems)
                        treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, ref nodeId,
                            ref linkSeqNumber, true);

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
                        var nodeColor = "#c0c0c0";
                        string cCalled = item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault();
                        var andCondition = " BaseCommandId = 19 AND " +
                                           " ( ClassNameDeclared = '" +
                                           cCalled + "' OR " +
                                           " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                        stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                                 ", " + andCondition + ")");
                        object[] parameters =
                        {
                            new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                            new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                        };
                        var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                            .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition",
                                parameters)
                            .Result;
                        if (classNameDeclared.Count() != 0)
                            nodeColor = "#00ffff";
                        nodeId++;

                        Node node;
                        if ((treeView.PrimaryCommandId == 25) || (treeView.PrimaryCommandId == 38))
                        {
                            var strSplit = treeView.StatementReferenceMaster.ClassCalled.Split('.');
                            var strName = "";
                            if (strSplit.Length > 2)
                                strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = strName.ToUpper(),
                                Color = nodeColor,
                                StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                                StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                                GroupName = treeView.GroupName,
                                GroupId = treeView.GroupId
                            };
                        }
                        treeViewData.Nodes.Add(node);
                        var m = treeView.StatementReferenceMaster.MethodCalled;
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
                            (from s in secondTab
                             where (s.ParentId == treeView.GraphId) && (s.BaseCommandId != 25)
                             select s)
                                .ToList().Distinct();
                        stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                                 projectId + ")");
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber, true);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 5)
                {
                    #region BaseCommandId == 5

                    var nodeColor = "#c0c0c0";
                    var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                            "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " +
                            treeView.StatementReferenceMaster.FileId + ";").Result;
                    if (methodCalled.Count != 0)
                        nodeColor = "#00ffff";
                    nodeId++;
                    Node node;
                    if ((treeView.PrimaryCommandId == 24) || (treeView.PrimaryCommandId == 37))
                    {
                        var strSplit = methodCalled[0].ClassNameDeclared.ToUpper().Split('.');
                        var strName = "";
                        if (strSplit.Length > 2)
                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                        node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = strName.ToUpper(),
                            Color = nodeColor,
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                            StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
                            GroupName = treeView.GroupName,
                            GroupId = treeView.GroupId
                        };
                    }
                    treeViewData.Nodes.Add(node);
                    var m = treeView.StatementReferenceMaster.MethodCalled;
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
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for call external: ProcessChildItemsCallInternal(" +
                                             projectId +
                                             ")");
                    try
                    {
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber, true);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                else if (treeView.BaseCommandId == 30)
                {
                    #region BaseCommandId == 30

                    nodeId++;
                    var width1 =
                        _clsUniverseBasic.CalculateWidth(treeView.StatementReferenceMaster.OriginalStatement.Length);
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
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
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
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    try
                    {
                        stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" +
                                                 projectId +
                                                 ")");
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            try
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsIf(" + projectId +
                                             ")");
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
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             "," +
                                             andCondition + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;
                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node,
                            ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for called internal: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
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

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
                    string width = _clsUniverseBasic.CalculateWidth(condition.Length);
                    string height = _clsUniverseBasic.CalculateHeight(condition.Length);
                    var node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "Decision2",
                        Name = condition,
                        Color = "#ff6600",
                        Width = width,
                        Height = height,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" + projectId +
                                             ")");
                    try
                    {
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            try
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childifitems: ProcessChildItemsIf(" + projectId + ")");
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
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             "," +
                                             andCondition + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;
                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node,
                            ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
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

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
                    string width = _clsUniverseBasic.CalculateWidth(condition.Length);
                    string height = _clsUniverseBasic.CalculateHeight(condition.Length);
                    var node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "Decision2",
                        Name = condition,
                        Color = "#ff6600",
                        Width = width,
                        Height = height,
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    try
                    {
                        stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsElse(" +
                                                 projectId +
                                                 ")");
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView,
            TreeViewData treeViewData, Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            var stringBuilder = new StringBuilder();
            try
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
                        ShapeId = "Decision",
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsIf(" + projectId +
                                             ")");
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

                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(" + projectId +
                                             "," +
                                             andCondition + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = andCondition}
                    };
                    var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters)
                        .Result;

                    if (classNameDeclared.Count() != 0)
                        nodeColor = "#f5bd6a";
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
                    stringBuilder.AppendLine("Started process for called external: ProcessChildItemsCallExt(" +
                                             projectId +
                                             ")");
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node,
                            ref nodeId,
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
                        nodeColor = "#f5bd6a";
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    stringBuilder.AppendLine("Started process for calles internal: ProcessChildItemsCallInternal(" +
                                             projectId + ")");
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

                    var parentIf = (from p in secondTab where treeView.ParentId == p.GraphId select p).FirstOrDefault();
                    if (parentIf == null) return treeViewData;

                    if (parentIf.BaseCommandId != 1) return treeViewData;

                    nodeId++;
                    var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                    var condition = ifPart.Contains("THEN")
                        ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                        : ifPart;
                    condition = "IF NOT " + condition + " THEN ";
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
                        StatementId = int.Parse(treeView.ActualStatementId.Split('_')[1]),
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
                    var childItems =
                        (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                    try
                    {
                        stringBuilder.AppendLine("Started process for childelseitems: ProcessChildItemsIf(" + projectId +
                                                 ")");
                        foreach (var cItem in childItems)
                            treeViewData = ProcessChildItemsElse(projectId, secondTab, cItem, treeViewData,
                                ref nodeId, ref linkSeqNumber);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }

                    #endregion
                }
                LogMessage.WriteLogMessage(stringBuilder);
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return treeViewData;
        }

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string color, string groupName, int groupId)
        {
            try
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
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
        }

        private List<Link> RemoveMultipleLinks(List<Link> lstLinks, List<Node> lstNodes)
        {
            var copyOfLinks = lstLinks;
            var linksToRemove = new List<Link>();
            foreach (var node in lstNodes)
            {
                try
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
                            childNodes.Where(a => childNodes.Except(new List<Node> { a }).Any(x => x.Name == a.Name))
                                .ToList();
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
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
            }
            foreach (var l in linksToRemove)
            {
                copyOfLinks.Remove(l);
            }
            return copyOfLinks;
        }

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab, TreeView curItem)
        {
            try
            {
                allSeqListItems.Add(curItem);
                var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                foreach (var cItem in childItems)
                {
                    allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
                }
            }
            catch (Exception exception)
            {
                LogMessage.WriteExceptionLogMessage(exception);
            }
            return allSeqListItems;
        }

        [HttpGet]
        public async Task<IHttpActionResult> AppendTreeNodes(string statementId, int projectId, int treeNodeId, int indentLevel)
        {
            var stringBuilder = new StringBuilder();

            using (_codeVortoService = new CodeVortoService())
            {
                var sqlQuery = "Select * from StatementReferenceMaster where StatementId = " +
                               Int32.Parse(statementId);
                var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                if (statementMaster == null) return NotFound();

                var treeView = new TreeView
                {
                    ClassCalled = statementMaster[0].ClassCalled
                };
                int fileId = statementMaster[0].FileId;
                int auto = 300;
                stringBuilder.AppendLine("Started process for called external: GetCallExternalDetails(" + projectId +
                                         ")");

                var listItems = GetCallExternalDetails(statementId, treeView, new List<TreeView>(), projectId,
                    fileId,
                    indentLevel, ref auto, ref treeNodeId);
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok(listItems);
            }
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int fileId, int indentLevel, ref int auto, ref int treeNodeId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                if (treeView.ClassCalled == null) return lstTreeView;
                try
                {
                    string className = treeView.ClassCalled;
                    stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                             className + ")");
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
                                ActualStatementId = "Missing" + auto + "_99999999",
                                IndentLevel = indentLevel
                            });
                        }
                    }
                    else
                    {
                        foreach (var statementMaster in callExtExpandedCode)
                        {
                            if (!treeView.MethodCalled.StartsWith(statementMaster.MethodName)) continue;
                            int blockStmtId = statementMaster.StatementId;
                            stringBuilder.AppendLine("Started process to get block of method: GetMethodBlock(" +
                                                     blockStmtId + ")");
                            var stmtsBlock = GetMethodBlock(blockStmtId);

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
                                    stringBuilder.AppendLine(
                                        "Started process for called external: GetCallExternalDetails(" + projectId + ")");
                                    lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                        ref auto, ref treeNodeId);
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
                                    stringBuilder.AppendLine(
                                        "Started process for call internal: GetCallInternalDetails(" +
                                        projectId + ")");
                                    lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, fileId, indentLevel, ref auto, ref treeNodeId);
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
                    }
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, int fileId, int indentLevel, ref int autoInt, ref int treeNodeId)
        {
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;
                try
                {
                    string methodName = treeView.MethodCalled;
                    stringBuilder.AppendLine("Called stored procedure: SpGetAllProjectItemsByCondition(8," + projectId +
                                             "," +
                                             methodName + "," + fileId + ")");
                    object[] parameters =
                    {
                        new MySqlParameter("@bCommandId", MySqlDbType.Int32) {Value = 8},
                        new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                        new MySqlParameter("@methodNm", MySqlDbType.VarChar) {Value = methodName},
                        new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                    };
                    var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters)
                        .Result;
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
                        stringBuilder.AppendLine("Started process to get block of class: GetGenericBlock(" +
                                                 stmtMaster[0].StatementId + ",8,9)");
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
                            foreach (var block in callExtExpandedCode)
                            {
                                autoInt++;
                                if (block.BaseCommandId == 5 || block.OtherBaseCommandId == 5)
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
                                    stringBuilder.AppendLine(
                                        "Started process for call internal: GetCallInternalDetails(" +
                                        projectId + ")");
                                    lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                        ref autoInt, ref treeNodeId);
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
                                    stringBuilder.AppendLine(
                                        "Started process for call external: GetCallExternalDetails(" +
                                        projectId + ")");
                                    lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                        lstTreeView, projectId, treeView.StatementReferenceMaster.FileId, indentLevel,
                                        ref autoInt, ref treeNodeId);
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
                }
                catch (Exception exception)
                {
                    LogMessage.WriteExceptionLogMessage(exception);
                }
                LogMessage.WriteLogMessage(stringBuilder);
                return lstTreeView;
            }
        }

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

        private List<StatementReferenceMaster> GetGenericBlock(string className)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("Called stored procedure: SpGetClassMethods(" + className + ")");
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
            var stringBuilder = new StringBuilder();
            using (_codeVortoService = new CodeVortoService())
            {
                stringBuilder.AppendLine("Called stored procedure: SpGetClassMethods(" + stmtId + ",8,9)");

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

        private string ReplaceString(string input)
        {
            if (string.IsNullOrEmpty(input)) return input;

            input = input.Replace(" >= ", " is greater than or equal to ");
            input = input.Replace(" <= ", " is less than or equal to ");
            input = input.Replace(" => ", " is equal to or greater than ");
            input = input.Replace(" =< ", " is equal to or less than ");
            input = input.Replace(">= ", " is greater than or equal to ");
            input = input.Replace("<= ", " is less than or equal to ");
            input = input.Replace("=> ", " is equal to or greater than ");
            input = input.Replace("=< ", " is equal to or less than ");
            input = input.Replace(" != ", " is not equal to ");
            input = input.Replace(" Ne ", " is not equal to ");
            input = input.Replace(" <> ", " is not equal to ");
            input = input.Replace(" Gt ", " is greater than ");
            input = input.Replace(" Lt ", " is less than ");
            input = input.Replace(" > ", " is greater than ");
            input = input.Replace(" < ", " is less than ");
            input = input.Replace(" = ", " is equal to ");
            input = input.Replace(" Not= ", " is not equal to ");
            input = input.Replace(" NE ", " is not equal to ");
            input = input.Replace(")NE ", ") is not equal to ");
            return input;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ViewSource(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var fileMaster = await _codeVortoService.FileMasterRepository
                        .GetAllItems(p => p.ProjectId == projectId).ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        }).ConfigureAwait(false);
                    foreach (var file in fileMaster)
                    {
                        if (file.FilePath.EndsWith(".txt")) continue;
                        if (!File.Exists(file.FilePath)) continue;
                        var allLines = File.ReadAllLines(file.FilePath);
                        var strBuilder = new StringBuilder();
                        foreach (var stmt in allLines)
                        {
                            strBuilder.AppendLine(stmt);
                        }
                        string fileLines = strBuilder.ToString();
                        var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                        var viewsourcedata = new ViewSourceMaster
                        {
                            ViewSourceId = 0,
                            FileId = file.FileId,
                            SourceData = fileLines,
                            ProjectId = file.ProjectId
                        };
                        await viewSourceMasterRepository.AddNewItem(viewsourcedata).ConfigureAwait(false);
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
            return Ok("Processed");
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForProjectInventory(int projectId, int solutionId, int languageId, 
            bool businessName = false, bool skipSame = false)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var entitiesToExclude = _codeVortoService.AppDbContextRepository
                        .EntitiesToExclude.ToList();

                    string sqlQy = "SELECT * FROM FileMaster WHERE SolutionId = " + solutionId +
                                   " AND FileTypeExtensionId != 11;";
                    var allFiles = await _codeVortoService.FileMasterRepository
                        .GetDataFromSqlQuery<FileMaster>(sqlQy).ConfigureAwait(false);


                    string qryDependancy = "SELECT * FROM DataDependency WHERE FileId IN " +
                                           "(SELECT FileId FROM FileMaster WHERE SolutionId = " + solutionId + ");";

                    var allDataDependancies = await _codeVortoService.DataDependencyRepository
                        .GetDataFromSqlQuery<DataDependency>(qryDependancy).ConfigureAwait(false);

                    var newDataDependenciesList = new List<DataDependency>();
                    var excludedDependancy = new List<DataDependency>();
                    foreach (var dataDependancy in allDataDependancies)
                    {
                        if (entitiesToExclude.Any(e => e.FileId == dataDependancy.FileId))
                        {
                            excludedDependancy.Add(dataDependancy);
                            continue;
                        }
                        newDataDependenciesList.Add(dataDependancy);
                    }
                    allDataDependancies = newDataDependenciesList;

                    string missingObjSql = "SELECT * FROM StatementReferenceMaster WHERE " +
                                           "ReferenceFileId = 0 AND BaseCommandId = 6 AND SolutionId = " + solutionId +
                                           ";";
                    var allMissingRefs = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(missingObjSql).ConfigureAwait(false);

                    string sqlAllClasses =
                        " Select * from StatementReferenceMaster Where BaseCommandId IN (19, 5, 6, 1) AND " +
                        " ProjectId IN ( Select ProjectId FROM ProjectMaster " +
                        " WHERE SolutionId = " + solutionId + ") ORDER BY FileId ASC;";
                    var allClassNameDeclaredOld = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlAllClasses).ConfigureAwait(false);
                    var allClassNameDeclaredNew = allClassNameDeclaredOld.Where(x => x.BaseCommandId == 19)
                        .Where(classNm => !string.IsNullOrEmpty(classNm.ClassNameDeclared)).ToList();

                    string refSqlQuery = "SELECT * FROM StatementReferenceMaster WHERE ReferenceFileId " +
                                         "IN(SELECT FileId FROM FileMaster WHERE FileTypeExtensionId = 12); ";
                    var allRefStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(refSqlQuery).ConfigureAwait(false);

                    var allClassNameDeclared = new List<StatementReferenceMaster>();
                    allClassNameDeclared.AddRange(allClassNameDeclaredNew);

                    var allCallExternals = (from s in allClassNameDeclaredOld
                        where s.BaseCommandId == 6 && !string.IsNullOrEmpty(s.ClassCalled)
                        select s).ToList();

                    var allCallExternalsWithOutClassCalled = (from s in allClassNameDeclaredOld
                        where s.BaseCommandId == 6
                        select s).ToList();

                    var allComplexity = (from s in allClassNameDeclaredOld
                        where s.BaseCommandId == 1 || s.BaseCommandId == 5
                        select s).ToList();

                    string qryWorkflowStatements = " Select * from WorkflowTreeViewSecondTabDetails " +
                                                   " Where ProjectId = " + projectId + " AND ClassCalled IS NOT NULL;";

                    var allWorkflowStatements = await _codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                        .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(qryWorkflowStatements)
                        .ConfigureAwait(false);

                    var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                        .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);

                    var universeDescriptor = await _codeVortoService.UniverseDescriptorRepository
                        .GetAllListItems(x => x.ProjectId == projectId && x.DescriptorId != 0 &&
                                              x.StatementString.Contains("SUBR")).ConfigureAwait(false);
                    var regEx = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);

                    universeDescriptor = (from u in universeDescriptor
                        where u.StatementString.Contains("SUBR")
                        select u).ToList();
                    /*
                     foreach (var uDescriptor in universeDescriptor)
                     {
                         string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                         uDescriptor.CompleteName = entityName;
                     }
                     */

                    var universeDescriptorList = new List<UniverseDescriptorList>();
                    foreach (var uDescriptor in universeDescriptor)
                    {
                        string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var stringLstData = uDescriptor.StatementString.Split(';').ToList();
                        var dData = new List<string>();
                        foreach (var lstData in stringLstData)
                        {
                            // var regex = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);
                            var allGroups = regEx.Match(lstData).Groups;
                            int groupCnt = -1;
                            foreach (Group group in allGroups)
                            {
                                groupCnt++;
                                if (groupCnt == 0) continue;
                                string eName = group.Value;
                                eName = eName.Replace("'", "");
                                var subRoutineFile =
                                (from f in allFiles
                                    where Path.GetFileNameWithoutExtension(f.FilePath) == eName &&
                                          f.FileTypeExtensionId == 17
                                    select f).ToList();
                                foreach (var sFile in subRoutineFile)
                                {
                                    bool isPresentSubRoutine = universeDescriptorList.Any(x =>
                                        x.FileId == sFile.FileId && x.EntityName == entityName);
                                    if (isPresentSubRoutine) continue;

                                    universeDescriptorList.Add(new UniverseDescriptorList
                                    {
                                        FileId = sFile.FileId,
                                        FileName = eName,
                                        EntityName = entityName,
                                        StatementList = uDescriptor.StatementString
                                    });
                                }
                            }
                        }
                    }

                    #region Collect Nodes and Links...

                    var nodeId = 1;
                    var lstNodes = new List<Node>();
                    foreach (var file in allFiles)
                    {
                        // if (file.FileTypeExtensionId != 17) continue;
                        var cName = new List<StatementReferenceMaster>();
                        var name = Path.GetFileNameWithoutExtension(file.FileName);
                        var classNameDeclared = "";
                        if (solutionId == 5)
                        {
                            var pNameNew = file.ProjectMaster.ProjectName;
                            var pPathNew = file.ProjectMaster.PhysicalPath;
                            var classNameProgram = file.FilePath.Replace(pPathNew + "\\", "")
                                .Replace(file.FileName, "").Replace("\\", ".");
                            var fileNameProgram = Path.GetFileNameWithoutExtension(file.FilePath);
                            classNameDeclared = pNameNew + "." + classNameProgram + fileNameProgram;
                            cName = (from c in allClassNameDeclared
                                where c.FileId == file.FileId && c.BaseCommandId == 19
                                select c).ToList();
                        }
                        else
                        {
                            cName = (from c in allClassNameDeclared
                                where c.FileId == file.FileId && c.BaseCommandId == 19
                                select c).ToList();
                            if(!cName.Any()) continue;
                            classNameDeclared = cName[0].ClassNameDeclared;
                        }
                        if (cName.Any())
                        {
                            lstNodes.Add(new Node
                            {
                                Id = nodeId++,
                                Name = name,
                                ShapeId = "RoundRect",
                                Height = "15",
                                Width = "100",
                                Color = "#00ffff", //Color.FromName("aqua").Name, // "#00ffff",
                                StatementId = cName[0].StatementId,
                                BaseCommandId = 19,
                                OriginalClassName = classNameDeclared,
                                StatementReferenceMaster = cName[0],
                                FileId = file.FileId
                            });
                            continue;
                        }
                        lstNodes.Add(new Node
                        {
                            Id = nodeId++,
                            Name = name,
                            ShapeId = "RoundRect",
                            Height = "15",
                            Width = "100",
                            Color = "#00ffff",
                            StatementId = 0,
                            BaseCommandId = 19,
                            OriginalClassName = classNameDeclared,
                            StatementReferenceMaster = null,
                            FileId = file.FileId
                        });
                    }
                    var lstLinks = new List<Link>();
                    // lstNodes = lstNodes.OrderByDescending(s => s.FileId).ToList();
                    foreach (var node in lstNodes)
                    {
                        string className = node.OriginalClassName;
                        var allDistinctClasses =
                            (from cls in allCallExternals where cls.ClassCalled == className select cls).ToList();
                        foreach (var callExt in allDistinctClasses)
                        {
                           
                            var cName = (from c in allClassNameDeclared
                                where c.FileId == callExt.FileId && c.BaseCommandId == 19
                                select c).ToList();
                            if(! cName.Any()) continue;
                            var originNode = lstNodes.Find(n =>
                            {
                                var firstOrDefault = cName.FirstOrDefault();
                                return firstOrDefault != null &&
                                       n.OriginalClassName == firstOrDefault.ClassNameDeclared;
                            });
                            var targetNode = lstNodes.Find(n => n.OriginalClassName == className);
                            if(targetNode == null) continue;
                            if (lstLinks.Any(s => s.Origin == originNode.Id && s.Target == targetNode.Id)) continue;

                            lstLinks.Add(new Link
                            {
                                Origin = originNode.Id,
                                Target = targetNode.Id,
                                LinkText = callExt.MethodCalled
                            });
                        }
                    }

                    #endregion

                    var workflowDetailsForInventory = new List<ProjectInventory>();

                    foreach (var node in lstNodes)
                    {
                        var tempNodeList = new List<Node>();
                        var lstMainNodeList = new List<Node>();
                        var startNodes =
                            (from n in lstNodes where n.FileId == node.FileId && n.BaseCommandId == 19 select n)
                            .ToList();
                        tempNodeList.AddRange(startNodes);
                        lstMainNodeList.AddRange(startNodes);
                        foreach (var mNode in tempNodeList)
                        {
                            var origin = mNode.Id;
                            lstMainNodeList = lstLinks.GetAllChildNodesForLinkOrigin(lstNodes, origin, lstMainNodeList);
                        }

                        var file = allFiles.Find(f => f.FileId == node.FileId);
                        var allMissingCallExternals = (from s in allMissingRefs
                            where s.FileId == file.FileId && s.ReferenceFileId == 0 && s.BaseCommandId == 6
                            select s).ToList();
                        int fileTypeExtensionId = file.FileTypeExtensionId;
                        // if (fileTypeExtensionId != 17) continue;
                        bool isPgmOrSbr = fileTypeExtensionId == 9 || fileTypeExtensionId == 17 ||
                                          fileTypeExtensionId == 12;

                        var calledFrom =
                        (from c in allCallExternals
                            where c.ReferenceFileId == file.FileId
                            select c).Distinct().ToList();
                        int linesCount = allFiles.First(c => c.FileId == file.FileId).LinesCount;
                        int complexity = 0;
                        int internalCall = 0;
                        var allWorkFlowsTempList = new List<string>();
                        var userObjectTempList = new List<string>();
                        var userReportTempList = new List<string>();
                        var userQueriesTempList = new List<string>();
                        var dataDependancies = new List<string>();
                        var calledFromList = new List<string>();
                        var iDescriptorList = new List<string>();
                        var calledExternalList = new List<string>();

                        if (node.StatementReferenceMaster == null)
                            node.StatementReferenceMaster = new StatementReferenceMaster
                            {
                                BusinessName = ""
                            };

                        if (node.StatementReferenceMaster != null &&
                            string.IsNullOrEmpty(node.StatementReferenceMaster.BusinessName))
                            node.StatementReferenceMaster.BusinessName = "";
                        string description = node.StatementReferenceMaster.BusinessName
                            .Replace("PA ", "").Trim().TrimStart('-').Trim();
                        if (file.FileTypeExtensionId == 14 || file.FileTypeExtensionId == 15
                            || file.FileTypeExtensionId == 9 || file.FileTypeExtensionId == 10 ||
                            file.FileTypeExtensionId == 17)
                        {
                            complexity =
                                (from s in allComplexity where s.FileId == file.FileId && s.BaseCommandId == 1 select s)
                                .Count();
                            complexity = complexity + 1;
                        }
                        else
                        {
                            var allFileLines = await _codeVortoService.ViewSourceMasterRepository
                                .GetAllListItems(x => x.FileId == file.FileId).ConfigureAwait(false);
                            foreach (var lines in allFileLines)
                            {
                                var line =
                                    lines.SourceData.Split('\n')
                                        .Where(n => !string.IsNullOrWhiteSpace(n))
                                        .Select(m => m.Trim())
                                        .ToList();
                                line.ForEach(d =>
                                {
                                    complexity += d.Split(' ').Length;
                                });
                            }
                        }

                        string uObject = string.Empty;
                        string uReport = string.Empty;
                        string uQuery = string.Empty;
                        string uEntities = string.Empty;
                        string partInWorkflow = string.Empty;
                        string cFrom = string.Empty;
                        string cExternal = string.Empty;
                        if (node.StatementReferenceMaster != null)
                        {
                            var statementReferenceMaster = node.StatementReferenceMaster;
                            if (string.IsNullOrEmpty(statementReferenceMaster.ClassNameDeclared)) continue;
                            var allFileIds = lstMainNodeList.Select(nodeList => nodeList.FileId).ToList();
                            var usesObjects = (from f in allFileIds
                                where allFiles.Any(d =>
                                    d.FileId == f && (d.FileTypeExtensionId == 14 || d.FileTypeExtensionId == 15
                                                      || d.FileTypeExtensionId == 9 || d.FileTypeExtensionId == 10
                                                      || d.FileTypeExtensionId == 17))
                                select f).Distinct().ToList(); // Extension = 14
                            var usesReports =
                            (from f in allFileIds
                                where allFiles.Any(d => d.FileId == f && d.FileTypeExtensionId == 13)
                                select f).Distinct().ToList(); // Extension = 13
                            var usesQueries =
                            (from f in allFileIds
                                where allFiles.Any(d => d.FileId == f && d.FileTypeExtensionId == 16)
                                select f).Distinct().ToList(); // Extension = 16
                            var uObjects = (from t in lstNodes
                                where usesObjects.Any(
                                    f => f == t.FileId && t.FileId != file.FileId &&
                                         t.BaseCommandId == 19)
                                select t).Distinct().ToList();
                            var uReports = (from t in lstNodes
                                where usesReports.Any(f => f == t.FileId)
                                select t).Distinct().ToList();
                            var uQueries = (from t in lstNodes
                                where usesQueries.Any(f => f == t.FileId)
                                select t).Distinct().ToList();
                            allFileIds.Add(file.FileId);
                            var dataDependanciesList =
                                (from d in allDataDependancies where d.FileId == file.FileId select d.Entity)
                                .Distinct().ToList();
                            foreach (var dependancy in dataDependanciesList)
                            {
                                if (dataDependancies.Any(
                                    k => dependancy.ToUpper().StartsWith(k, StringComparison.OrdinalIgnoreCase)))
                                    continue;
                                dataDependancies.Add(dependancy);
                            }
                            if (isPgmOrSbr)
                            {
                                var otherEntities = ExtractDataDependencyForPgmAndSbr(file, allDataDependancies,
                                    allRefStatementMaster, dataDependancies);
                                dataDependancies.AddRange(otherEntities);
                            }

                            dataDependancies = dataDependancies.Distinct().ToList();
                            if (entitiesToExclude.Any(f => f.FileId == node.FileId))
                            {
                                dataDependancies = (from d in excludedDependancy
                                    where d.FileId == node.FileId
                                    select d.Entity).Distinct().ToList();
                            }

                            // Collect data for Participates in 
                            string className = node.StatementReferenceMaster.ClassNameDeclared;
                            var allClassCalled = allWorkflowStatements.Where(s => s.ClassCalled == className).ToList();
                            var allWorkflows = (from a in allActionWorkflows
                                where allClassCalled.Any(s => s.ActionWorkflowId == a.ActionWorkflowId)
                                select a).Distinct().ToList();
                            var fileWorkflows = (from a in allActionWorkflows
                                where a.FileId == node.FileId
                                select a).Distinct().ToList();
                            allWorkflows.AddRange(fileWorkflows);
                            var addedWorkflows = new List<int>();
                            foreach (var workflow in allWorkflows)
                            {
                                if (addedWorkflows.Any(s => s == workflow.ActionWorkflowId)) continue;
                                addedWorkflows.Add(workflow.ActionWorkflowId);
                                string link = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                                              workflow.MethodStatementId +
                                              "&aId=" + workflow.ActionWorkflowId;
                                var workFlowNm = "";
                                var originWfName = !string.IsNullOrEmpty(workflow.OriginEventMethod)
                                    ? workflow.OriginEventMethod
                                    : workflow.OriginObject;
                                switch (languageId)
                                {
                                    case 6:
                                        workFlowNm = workflow.WorkflowName;
                                        break;
                                    case 5:
                                        workFlowNm = workflow.OriginObject;
                                        break;
                                    case 1:
                                        workFlowNm = originWfName;
                                        break;
                                }
                                var str = businessName
                                    ? "<a href='javascript:void(0);' onclick='openLink(aDiv_" +
                                      workflow.ActionWorkflowId +
                                      ");' style='color: blue; text-decoration: underline;'>" +
                                      workflow.TechnicalAndBusinessName + "</a> <div style='display: none;' id='aDiv_" +
                                      workflow.ActionWorkflowId + "'>" + link + "</div> "
                                    : "<a href='javascript:void(0);' onclick='openLink(aDiv_" +
                                      workflow.ActionWorkflowId +
                                      ");' style='color: blue; text-decoration: underline;'>" + workFlowNm +
                                      "</a> <div style='display: none;' id='aDiv_" +
                                      workflow.ActionWorkflowId + "'>" + link + "</div> ";
                                allWorkFlowsTempList.Add(str);
                            }
                            foreach (var uObj in uObjects)
                            {
                                if (uObj.FileId == node.FileId && skipSame) continue;
                                var str =
                                    "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                    " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                    uObj.Name + "</a>";
                                userObjectTempList.Add(str);
                            }
                            foreach (var uObj in allMissingCallExternals)
                            {
                                if (string.IsNullOrEmpty(uObj.OriginalStatement)) continue;
                                int spaceFirstIndex = uObj.OriginalStatement.IndexOf(' ');
                                if (spaceFirstIndex < 0) spaceFirstIndex = 0;
                                string missingRef = uObj.OriginalStatement.Substring(spaceFirstIndex);
                                var str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                          "cursor: pointer; text-decoration: none;'>" + missingRef + " [ Missing ]</a>";
                                userObjectTempList.Add(str);
                            }

                            foreach (var uObj in uReports)
                            {
                                if (uObj.FileId == node.FileId && skipSame) continue;
                                var str =
                                    "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                    " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                    uObj.Name + "</a>";
                                userReportTempList.Add(str);
                            }
                            foreach (var uObj in uQueries)
                            {
                                if (uObj.FileId == node.FileId && skipSame) continue;
                                var str =
                                    "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                    " onclick='includeStateDialog(" + uObj.FileId + ");'>" +
                                    uObj.Name + "</a>";
                                userQueriesTempList.Add(str);
                            }

                            //if (dataDependancies.Any())
                            //{
                            //    string str = "<li>" + dataDependancies[0] + "</li>";
                            //    dataDependancies[0] = str;
                            //}

                            if (userObjectTempList.Any())
                            {
                                string str = "<li>" + userObjectTempList[0] + "</li>";
                                userObjectTempList[0] = str;
                            }
                            internalCall = (from c in allComplexity
                                where c.FileId == file.FileId && c.BaseCommandId == 5
                                select c).Count();
                            var externalCallList = (from x in allCallExternalsWithOutClassCalled
                                where x.FileId == file.FileId && x.BaseCommandId == 6
                                select x).ToList();
                            var refFileIds = new List<int>();
                            foreach (var eCall in externalCallList)
                            {
                                string str;
                                int refFileId = eCall.ReferenceFileId;
                                if (refFileId == 0)
                                {
                                    str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                          "cursor: pointer; text-decoration: none;'>" + eCall.OriginalStatement +
                                          " [ Missing ]</a>";
                                    calledExternalList.Add(str);
                                    continue;
                                }
                                var refFileMaster = allFiles.FirstOrDefault(x => x.FileId == refFileId);
                                if (refFileMaster == null) continue;
                                bool exist = refFileIds.Any(f => f == refFileMaster.FileId);
                                if (exist) continue;
                                refFileIds.Add(refFileMaster.FileId);
                                str =
                                    "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                    " onclick='includeStateDialog(" + refFileMaster.FileId + ");'>" +
                                    refFileMaster.FileName + "</a>";
                                calledExternalList.Add(str);
                            }

                            var addedFileId = new List<int>();
                            foreach (var uObj in calledFrom)
                            {
                                if (addedFileId.Any(f => f == uObj.FileMaster.FileId)) continue;
                                var str =
                                    "<a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
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

                            if (userReportTempList.Any())
                            {
                                string str = "<li>" + userReportTempList[0] + "</li>";
                                userReportTempList[0] = str;
                            }

                            if (userQueriesTempList.Any())
                            {
                                string str = "<li>" + userQueriesTempList[0] + "</li>";
                                userQueriesTempList[0] = str;
                            }

                            if (allWorkFlowsTempList.Any())
                            {
                                string str = "<li>" + allWorkFlowsTempList[0] + "</li>";
                                allWorkFlowsTempList[0] = str;
                            }

                            if (calledExternalList.Any())
                            {
                                string str = "<li>" + calledExternalList[0] + "</li>";
                                calledExternalList[0] = str;
                            }
                            if (dataDependancies.Any())
                            {
                                string str = "<li>" + dataDependancies[0] + "</li>";
                                dataDependancies[0] = str;
                            }
                            uObject = string.Join("<li>", userObjectTempList);
                            uReport = string.Join("<li>", userReportTempList);
                            uQuery = string.Join("<li>", userQueriesTempList);
                            uEntities = string.Join("<li>", dataDependancies);
                            cFrom = string.Join("<li>", calledFromList);
                            cExternal = string.Join("<li>", calledExternalList);
                            partInWorkflow = string.Join("<li>", allWorkFlowsTempList);

                        }
                        var callFormFinal = string.Empty;
                        callFormFinal = calledFromList.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                              node.Id + "'>" + cFrom + " </div> "
                            : "-";

                        // For I-Descriptor...
                        // string sbrFileName = Path.GetFileNameWithoutExtension(file.FilePath);
                        // var regEx = new Regex(@"SUBR\((.*?(?=,))", RegexOptions.Singleline);
                        /*
                        var iDescriptor = (from u in universeDescriptor
                                           where u.StatementString.Contains(sbrFileName) && regEx.IsMatch(u.StatementString)
                                           select u).Distinct().ToList();
                        */
                        var iDescriptor = (from u in universeDescriptorList
                            where file.FileId == u.FileId
                            select u).Distinct().ToList();
                        int iDescpt = 0;
                        if (iDescriptor.Any())
                        {
                            foreach (var iDescptor in iDescriptor)
                            {

                                var dData = iDescptor.StatementList.Split(';').ToList();
                                var dList = new List<string>();
                                int index = 0;
                                iDescpt++;
                                foreach (var uObj in dData)
                                {
                                    index++;
                                    var str =
                                        index +
                                        ")&nbsp;<a href='javascript:void(0);' style='color: black; font-size: 14px; cursor: default; line-height:22px;'>" +
                                        uObj + "</a>";

                                    dList.Add(str);
                                }
                                var uStatementList = string.Join("<br />", dList);

                                string divId = "iDept_" + iDescptor.FileId + "_" + iDescpt;
                                var strStatement = dList.Any()
                                    ? "<a href='javascript:void(0);' onclick=showIDespt('" + divId +
                                      "') style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                      iDescptor.EntityName + " </a> <div lang='" + iDescptor.EntityName +
                                      "' style='display: none;' id='" + divId + "'>" + uStatementList + " </div> "
                                    : "-";
                                iDescriptorList.Add(strStatement);
                            }

                            if (iDescriptorList.Any())
                            {
                                string str = "<li>" + iDescriptorList[0] + "</li>";
                                iDescriptorList[0] = str;
                            }
                            var iDescrt = string.Join("<li>", iDescriptorList);
                            int callFormCnt = 0;
                            if (iDescriptorList.Any())
                            {
                                if (callFormFinal == "-")
                                {
                                    callFormCnt = iDescriptorList.Count;
                                    callFormFinal =
                                        "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                                        ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                        callFormCnt +
                                        " Called From </a> <div style='display: none;' id='calledFrom_" +
                                        node.Id +
                                        "'><div style='height: 10px;margin-bottom: 20px;'><h4> I-Desc </h4></div> " +
                                        iDescrt + "  </div> ";
                                }
                                else
                                {
                                    callFormCnt = calledFromList.Count + iDescriptorList.Count;
                                    callFormFinal = calledFromList.Count > 0
                                        ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                          callFormCnt +
                                          " Called From </a> <div style='display: none;' id='calledFrom_" +
                                          node.Id + "'>" + cFrom +
                                          " <div style='height: 10px;margin-bottom: 20px; margin-top: 20px;'><h4> I-Desc </h4></div> " +
                                          iDescrt + "  </div>"
                                        : "-";
                                }
                            }

                            /*
                            callFormFinal = callFormFinal == "-"
                                ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                                  node.Id + "'>" + cFrom +
                                  "<div style='height: 10px;margin-bottom: 25px;'><h4> I-Descriptor </h4></div> " + iDescrt + "  </div> "
                                : callFormFinal = callFormFinal+ "<div style='height: 10px;margin-bottom: 25px;'><h4> I-Descriptor </h4></div> " + iDescrt + "  </div> ";
                            */
                        }
                        var inventoryDetails = new ProjectInventory // InventoryReport
                        {
                            ObjectName =
                                "<pre><a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                                " onclick='includeStateDialog(" + node.FileId + ");'>" + node.Name + "</a></pre>",
                            Description = description,
                            ExtenstionType = file.FileTypeExtensionReference.FileTypeName,
                            LoC = linesCount,
                            Complexity = complexity,
                            InternalCall = internalCall,
                            // ExternalCall = externalCall,
                            ExternalCall = calledExternalList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvCExternal_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  calledExternalList.Count +
                                  " Call External(s) </a> <div style='display: none;' id='dvCExternal_" +
                                  node.Id + "'>" + cExternal + " </div> "
                                : "-",
                            UsesObjects = userObjectTempList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvObjects_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  userObjectTempList.Count +
                                  " Object(s) </a> <div style='display: none;' id='dvObjects_" +
                                  node.Id + "'>" + uObject + " </div> "
                                : "-",
                            UsesReports = userReportTempList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvReports_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  userReportTempList.Count +
                                  " Report(s) </a> <div style='display: none;' id='dvReports_" +
                                  node.Id + "'>" + uReport + " </div> "
                                : "-",
                            UsesQueries = userQueriesTempList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvQuery_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  userQueriesTempList.Count +
                                  " Query(ies) </a> <div style='display: none;' id='dvQuery_" +
                                  node.Id + "'>" + uQuery + " </div> "
                                : "-",
                            UsesEntities = dataDependancies.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvDataDepend_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  dataDependancies.Count +
                                  " Entity(ies) </a> <div style='display: none;' id='dvDataDepend_" + node.Id + "'>" +
                                  uEntities + " </div> "
                                : "-",
                            ParticipateInWorkflow = allWorkFlowsTempList.Count > 0
                                ? "<a href='javascript:void(0);' onclick='showData(dvPartInWork_" + node.Id +
                                  ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                  allWorkFlowsTempList.Count +
                                  " Workflow(s) </a> <div style='display: none;' id='dvPartInWork_" + node.Id + "'>" +
                                  partInWorkflow + " </div> "
                                : "-",
                            CalledFrom = callFormFinal,
                            // calledFromList.Count > 0
                            //    ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + node.Id +
                            //      ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                            //      calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                            //      node.Id + "'>" + cFrom + " </div> "
                            //    : "-",
                            ProjectId = projectId,
                            SolutionId = solutionId,
                            FileId = file.FileId,
                            CallingTo = null
                        };
                      /*
                        using (var appDbContext = new AppDbContext())
                        {
                            appDbContext.Set<ProjectInventory>().Add(inventoryDetails);
                            await appDbContext.SaveChangesAsync();
                            workflowDetailsForInventory.Add(inventoryDetails);
                        }
                        */
                       
                    }
                    return Ok(workflowDetailsForInventory);
                    // return Ok("Done processing inventory for Project Id: " + solutionId);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception);
                    return InternalServerError(exception);
                }
            }
        }

        private List<string> ExtractDataDependencyForPgmAndSbr(FileMaster fileMaster,
            List<DataDependency> dataDependencies, List<StatementReferenceMaster> lstStatementRef,
            List<string> entitiesList)
        {
            var allIncludeRefs = (from s in lstStatementRef where s.FileId == fileMaster.FileId select s).ToList();
            foreach (var includeRef in allIncludeRefs)
            {
                var allIncludes = (from i in dataDependencies where i.FileId == includeRef.ReferenceFileId select i)
                    .ToList();

                entitiesList.AddRange(allIncludes.Select(include => include.Entity));

                foreach (var include in allIncludes)
                {
                    if (entitiesList.Any(i => i == include.Entity)) continue;
                    var newEntitiesList = ExtractDataDependencyForPgmAndSbr(include.FileMaster, dataDependencies,
                        lstStatementRef, new List<string>());
                    entitiesList.AddRange(newEntitiesList);
                }
            }
            return entitiesList;
        }
    }
}