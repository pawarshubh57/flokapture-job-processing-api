using System;
using System.Collections.Generic;
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
using MySql.Data.MySqlClient;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using BusinessLayer.Models;
using BusinessLayer.Cobol;
using System.Text;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class CobolProcessingVersionOldController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public CobolProcessingVersionOldController()
        {
        }

        public CobolProcessingVersionOldController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessCobolProject(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                var projectConfigFilesGeneralRepository = new  GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                if (projectDetails == null) return Ok(projectId);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));

                string projectPath = projectDetails.PhysicalPath;
                List<string> directoryList = new List<string> { projectPath };
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                var projectImpFiles = await  projectConfigFilesGeneralRepository.GetAllItems(p => p.ConfigFileId != 2);

                strExtensions.AddRange(projectImpFiles.Distinct().Select(e => e.ToString()));
                foreach (string directory in directoryList)
                {
                    var allFiles = Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)
                        .Where(s => strExtensions.Any(s.EndsWith));

                    IEnumerator<string> enumerator = allFiles.GetEnumerator();
                    List<FileMaster> lstFileMasters = new List<FileMaster>();
                    while (enumerator.MoveNext())
                    {
                        string currentFile = enumerator.Current;
                        if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                        string fileName = Path.GetFileName(currentFile);
                        if (string.IsNullOrEmpty(fileName)) continue;
                        if (fileName.Contains(".dll.config")) continue;
                        string extension = Path.GetExtension(currentFile);
                        int extensionId =
                            fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                        var fileMaster = new FileMaster
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
                    enumerator.Dispose();
                    await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters);
                }
                IHttpActionResult processResult = await ParseProjectFilesCobol(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok("Cobol application parsing successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProjectFilesCobol(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);

                // FileTypeExtensionId == 7 is for jcl files which is starting point of COBOL programs...
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.ProjectId == projectId);
                var copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                if (copyOfFileMaster.All(f => f.FileTypeExtensionId != 7))
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

                var cobolV1 = new CobolVersion1();
                var projectName = projectDetails.ProjectName;
                var classStart = callClassIndicatorStart.Find(x => true);
                var classEnd = callClassIndicatorEnd.Find(x => true);
                var methodStart = methodIndicationStart.Find(x => true);
                var methodEnd = methodIndicationEnd.Find(x => true);
                var callExternalStart = callExternalIndicationStart.Find(x => true);

                // This is common for all statement that is ClassNameDeclared
                string pPath = projectDetails.PhysicalPath;

                #region All file processing...

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();

                foreach (var fMaster in copyOfFileMaster.Where(f => (f.FileTypeExtensionId == 8 || f.FileTypeExtensionId == 7 || f.FileTypeExtensionId == 6) && f.Processed == 0))
                {
                    string className = fMaster.FilePath.Replace(pPath + "\\", "")
                        .Replace(fMaster.FileName, "").Replace("\\", ".");
                    string fileName = Path.GetFileNameWithoutExtension(fMaster.FilePath);
                    // Use it later...
                    string classNameDeclared = projectName + "." + className + fileName;

                    var lstFileLines = File.ReadAllLines(fMaster.FilePath).ToList();
                    if (lstFileLines.Count <= 0) return Ok("No contents in file: " + fMaster.FilePath);
                    // Prepare list to insert into statement reference master...
                    // As Jcl file will not hace class start and end indicator, so we need to add it to make generic class block...

                    #region JCL & Proc & Cobol Parsing Logic

                    var statmentRefStart = cobolV1.PrepareStatementReferenceMasterStart(fMaster, 31);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefStart);

                    if (fMaster.FileTypeExtensionId == 7)
                    {
                        await ParseJclFile(projectId, fMaster.FileId, copyOfFileMaster.ToList(), projectName, pPath, projectDetails.LanguageId, lstFileLines, fMaster.FilePath);
                    }
                    else if (fMaster.FileTypeExtensionId == 8)
                    {
                        await ParseProcFile(projectId, fMaster.FileId, copyOfFileMaster.ToList(), projectName, pPath, projectDetails.LanguageId, lstFileLines, fMaster.FilePath);
                    }
                    else if (fMaster.FileTypeExtensionId == 6)
                    {
                        await ParseCobolFile(projectId, fMaster.FileId, copyOfFileMaster.ToList(), projectName, pPath, projectDetails.LanguageId, lstFileLines, fMaster.FilePath);
                    }

                    var statmentRefEnd = cobolV1.PrepareStatementReferenceMasterEnd(fMaster, 32);
                    await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(statmentRefEnd);
                    #endregion

                    // Update file status to processed = 1...
                    fMaster.Processed = 1;
                    fMaster.ProjectMaster = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(fMaster);
                    //file = fMaster;
                    // As we are intended to process one Proc file (Starting point) at a time, so break the loop and exit...
                }
                #endregion

                #region Update ClassCalled field for StatementReferenceMaster...
                string execOrCallSql = " Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                 " AND BaseCommandId IN (6, 19); ";
                var callExternals = await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execOrCallSql);

                foreach (var cExternal in callExternals)
                {
                    if (cExternal.BaseCommandId == 6)
                    {
                        string pgName = "";
                        //if (cExternal.OriginalStatement.ContainsAll("EXEC", "PROC", "=", ","))
                        //{
                        //    pgName = cExternal.OriginalStatement.Split('=')[1] + ".proc";
                        //    if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".proc";
                        //}
                        //else if (cExternal.OriginalStatement.ContainsAll("EXEC", "PGM", "=", ","))
                        //{
                        //    pgName = cExternal.OriginalStatement.Split('=')[1] + ".cbl";
                        //    if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".cbl";
                        //}
                        //else 

                        int ichkEqual = cExternal.OriginalStatement.Count(x => x == '=');

                        if (cExternal.OriginalStatement.ContainsAll("EXEC", " PROC", "="))
                        {
                            if (ichkEqual > 1)
                            {
                                pgName = cExternal.OriginalStatement.Split('=')[1] + ".proc";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".proc";
                            }
                            else
                            {
                                pgName = cExternal.OriginalStatement.Split('=').LastOrDefault() + ".proc";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".proc";
                            }
                        }
                        else if (cExternal.OriginalStatement.ContainsAll("EXEC", " PGM", "="))
                        {
                            if (ichkEqual > 1)
                            {
                                pgName = cExternal.OriginalStatement.Split('=')[1] + ".cbl";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".cbl";
                            }
                            else
                            {
                                pgName = cExternal.OriginalStatement.Split('=').LastOrDefault() + ".cbl";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".cbl";
                            }
                        }
                        else if (cExternal.OriginalStatement.StartsWith("EXEC") && !cExternal.OriginalStatement.Contains("SQL") && !cExternal.OriginalStatement.Contains("CICS")
                            && !cExternal.OriginalStatement.Contains(" PGM") && !cExternal.OriginalStatement.Contains(" PROC"))
                        {
                            if (ichkEqual == 0)
                            {
                                pgName = cExternal.OriginalStatement.Split(' ').LastOrDefault() + ".proc";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".proc";
                            }
                            else
                            {
                                pgName = cExternal.OriginalStatement.Split(' ')[1] + ".proc";
                                if (pgName.Contains(",")) pgName = pgName.Split(',').FirstOrDefault() + ".proc";
                            }
                        }

                        var pName =
                            copyOfFileMaster.ToList().Where(f => !string.IsNullOrEmpty(pgName)
                                                                 && f.FileName.StartsWith(pgName, StringComparison.CurrentCultureIgnoreCase) && f.ProjectId == projectId &&
                                                                 (f.FileTypeExtensionId == 6 || f.FileTypeExtensionId == 7 || f.FileTypeExtensionId == 8)).ToList();

                        string className;
                        string fileName;
                        string classNameDeclared;

                        if (!pName.Any())
                        {
                            if (pgName.Contains(".proc"))
                            {
                                pName =
                                copyOfFileMaster.ToList().Where(f => f.ProjectId == projectId
                                                                     && f.FileTypeExtensionId == 8).ToList();
                                className = pName[0].FilePath.Replace(pPath + "\\", "")
                                    .Replace(pName[0].FileName, "").Replace("\\", ".");
                                fileName = Path.GetFileNameWithoutExtension(pgName);
                                // Use it later...
                                classNameDeclared = projectName + "." + className + fileName;
                                cExternal.ClassCalled = classNameDeclared;
                            }
                            else if (pgName.Contains(".jcl"))
                            {
                                pName =
                               copyOfFileMaster.ToList().Where(f => f.ProjectId == projectId
                                                                    && f.FileTypeExtensionId == 7).ToList();
                                className = pName[0].FilePath.Replace(pPath + "\\", "")
                                    .Replace(pName[0].FileName, "").Replace("\\", ".");
                                fileName = Path.GetFileNameWithoutExtension(pgName);
                                // Use it later...
                                classNameDeclared = projectName + "." + className + fileName;
                                cExternal.ClassCalled = classNameDeclared;
                            }
                            else
                            {
                                pName =
                                   copyOfFileMaster.ToList().Where(f => f.ProjectId == projectId
                                                                        && f.FileTypeExtensionId == 6).ToList();
                                className = pName[0].FilePath.Replace(pPath + "\\", "")
                                    .Replace(pName[0].FileName, "").Replace("\\", ".");
                                fileName = Path.GetFileNameWithoutExtension(pgName);
                                // Use it later...
                                classNameDeclared = projectName + "." + className + fileName;
                                cExternal.ClassCalled = classNameDeclared;
                            }
                        }
                        else
                        {
                            className = pName[0].FilePath.Replace(pPath + "\\", "")
                               .Replace(pName[0].FileName, "").Replace("\\", ".");
                            fileName = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                            // Use it later...
                            classNameDeclared = projectName + "." + className + fileName;
                            cExternal.ClassCalled = classNameDeclared;
                        }

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                        continue;
                    }
                    if (cExternal.BaseCommandId == 19)
                    {
                        int fileId = cExternal.FileId;
                        var pName =
                            copyOfFileMaster.ToList().Where(f => f.FileId == fileId).ToList();
                        string classNameProgram = pName[0].FilePath.Replace(pPath + "\\", "")
                            .Replace(pName[0].FileName, "").Replace("\\", ".");
                        string fileNameProgram = Path.GetFileNameWithoutExtension(pName[0].FilePath);
                        // Use it later...
                        string classNameDeclaredProgram = projectName + "." + classNameProgram + fileNameProgram;
                        cExternal.ClassNameDeclared = classNameDeclaredProgram;

                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(cExternal);
                    }
                }

                #endregion

                #region Update Method called for base command id = 6 in Jcl and program...


                string execSql = "Select * from StatementReferenceMaster Where ProjectId = " + projectId +
                                 " AND BaseCommandId = 6 AND ClassCalled is not null; ";
                var execName = await
                   baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSql);
                foreach (var constructor in execName)
                {
                    string[] temp = constructor.ClassCalled.Split('.');
                    string fileName = "";

                    if (constructor.OriginalStatement.ContainsAll("PROC"))
                    {
                        if (temp.Length > 0)
                        {
                            fileName = temp[temp.Length - 1] + ".proc";
                        }
                    }
                    else if (constructor.OriginalStatement.ContainsAll("PGM"))
                    {
                        if (temp.Length > 0)
                        {
                            fileName = temp[temp.Length - 1] + ".cbl";
                        }
                    }
                    else if (constructor.OriginalStatement.StartsWith("EXEC") && !constructor.OriginalStatement.Contains("SQL") && !constructor.OriginalStatement.Contains("CICS")
                            && !constructor.OriginalStatement.Contains("PGM") && !constructor.OriginalStatement.Contains("PROC"))
                    {
                        if (temp.Length > 0)
                        {
                            fileName = temp[temp.Length - 1] + ".proc";
                        }
                    }

                    var allCheckFiles = await _codeVortoService.FileMasterRepository
                        .GetAllItems(f => f.ProjectId == projectId && f.FileName == fileName.Trim());

                    foreach (var files in allCheckFiles)
                    {
                        string methodSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                         " AND ProjectId = " + projectId + " AND BaseCommandId=8;";
                        var methodName = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(methodSql);

                        for (int iMethodName = 0; iMethodName < methodName.Count; iMethodName++)
                        {
                            string checkSql = "";
                            if (methodName.Count > 1)
                            {
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                                " AND ProjectId = " + projectId + " AND StatementId > " + methodName[iMethodName].StatementId + " and StatementId < " + methodName[iMethodName + 1].StatementId + " and BaseCommandId!=9;";
                            }
                            else
                            {
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                                    " AND ProjectId = " + projectId + " AND StatementId > " + methodName[iMethodName].StatementId + " and BaseCommandId!=9;";
                            }

                            var checkCnt = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(checkSql);
                            if (checkCnt.Count > 0)
                            {
                                constructor.MethodCalled = methodName[iMethodName].MethodName.Replace(".", "");
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }
                        }
                    }
                }
                #endregion

                #region Added the Action Workflow Table...
                // Action Workflows data...
                var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                int projectType = projectMasterData.ProjectConfigType;
                if (projectType == 4) // For COBOL file
                {
                    var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                    var projectConfigData = projectConfig.GetItem(projectType);
                    if (projectConfigData.ConfigFileId != projectType) return Ok("Process completed successfully");
                    // Means the project type is windows application and we need to read starting point from that 
                    // files respective code behind file...
                    string configFileName = projectConfigData.ToString();
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
                            if (fileExtension == 7)
                            {
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
                                    ActionWorkflows actionWorkflow = new ActionWorkflows
                                    {
                                        ActionWorkflowId = 0,
                                        CreatedBy = 1,
                                        EndPointOrService = "Batch",
                                        MethodStatementId = statement.StatementId,
                                        OriginFileName =
                                            Path.GetFileName(
                                                cFile.FilePath),
                                        OriginFilePath =
                                            cFile.FilePath,
                                        ProjectId = projectId,
                                        OriginEventMethod = Path.GetFileNameWithoutExtension(cFile.FileName),
                                        OriginObject =
                                            projectName +
                                            cFile.FilePath.Replace(".jcl", "")
                                                .Replace(pPath, "")
                                                .Trim()
                                                .Replace("\\", "."),
                                        WorkflowName = Path.GetFileNameWithoutExtension(cFile.FileName),
                                        ServiceBaseAddress = null,
                                        ServiceContract = null,
                                        WorkflowBusinessName = statement.BusinessName
                                    };
                                    await actionWorkflowsRepository.AddNewItem(actionWorkflow);
                                }
                            }
                        }
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine(exception.Message);
                    }
                }

                #endregion

                #region Insert the data in ViewSourceMaster Table
                foreach (var file in copyOfFileMaster)
                {
                    var allLines = File.ReadAllLines(file.FilePath);

                    var strBuilder = new StringBuilder();
                    foreach (var stmt in allLines)
                    {
                        strBuilder.AppendLine(stmt);
                    }

                    var viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext());
                    var viewsourcedata = new ViewSourceMaster()
                    {
                        ViewSourceId = 0,
                        FileId = file.FileId,
                        SourceData = strBuilder.ToString()
                    };
                    await viewSourceMasterRepository.AddNewItem(viewsourcedata);
                }
                #endregion

                //return Ok("Cobol application parsed successfully..");
                IHttpActionResult processResult = await ProcessForObjectConnectivityDiagramData(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);

                //// Start process for collecting workflow data here...
                //IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                //await processResult.ExecuteAsync(CancellationToken.None);
                //return Ok("Cobol application parsed successfully.");
                ////return Ok("Connectivity diagram data process completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = await _codeVortoService.ProjectMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                var fileElementMaster = await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);
                var statementMaster = await _codeVortoService.StatementReferenceMasterRepository.GetAllItems(p => p.ProjectId == projectId);

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
                        && p.BaseCommandId == 19 && p.ClassNameDeclared != "null").ToList();

                    foreach (var s in allClassData)
                    {
                        Node nodeObject = new Node();

                        if (languageId == 1)
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
                        else
                        {
                            if (string.IsNullOrEmpty(s.ClassNameDeclared)) continue;
                           
                            if (s.ClassNameDeclared.Contains("JCL") || s.ClassNameDeclared.Contains("Jcl"))
                            {
                                nodeObject.Id = s.StatementId;
                                nodeObject.Name = s.ClassNameDeclared;
                                nodeObject.ShapeId = "Circle";
                                nodeObject.Height = "15";
                                nodeObject.Width = "100";
                                //nodeObject.Color = "#00ffff";
                                nodeObject.Color = "#ffcc00";
                                nodeObject.StatementId = s.StatementId;
                                nodeObject.BaseCommandId = s.BaseCommandId;
                                lstNodes.Add(nodeObject);
                            }
                        }
                        var fileNewId = s.FileId;

                        //  Get the all Classes to file wise.

                        if (languageId == 4 || languageId == 5)
                        {
                            #region For Cobol Application

                            if (s.ClassNameDeclared.Trim().Contains("JCL") || s.ClassNameDeclared.Trim().Contains("Jcl"))
                            {
                                var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                   && p.FileId == s.FileId && p.BaseCommandId == 6).ToList();

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

                                        string[] strSplit = c.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                        string strName = "";
                                        if (strSplit.Length > 2)
                                        {
                                            if (strSplit[strSplit.Length - 2].StartsWith("PROC"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".proc";
                                            }
                                            else if (strSplit[strSplit.Length - 2].StartsWith("COBOL"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".cbl";
                                            }
                                            else if (strSplit[strSplit.Length - 2].StartsWith("JCL"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".jcl";
                                            }
                                            else if (c.ClassCalled.ToUpper().Contains("PROGRAMS"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".pgm";
                                            }

                                        }

                                        var checkClassPresentOrNot = fileElementMaster.Where(p => p.ProjectId == projectId
                                            && p.FileName == strName.Trim()).ToList();

                                        if (checkClassPresentOrNot.Count > 0)
                                        {
                                            var fileIdPresent = 0;
                                            foreach (var p in checkClassPresentOrNot)
                                            {
                                                fileIdPresent = p.FileId;
                                            }

                                            callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                               && p.FileId == fileIdPresent && p.BaseCommandId == 6).ToList();

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

                                                string[] strSplit = z.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                                string strName = "";
                                                if (strSplit.Length > 2)
                                                {
                                                    if (strSplit[strSplit.Length - 2].StartsWith("PROC"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".proc";
                                                    }
                                                    else if (strSplit[strSplit.Length - 2].StartsWith("COBOL"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".cbl";
                                                    }
                                                    else if (strSplit[strSplit.Length - 2].StartsWith("JCL"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".jcl";
                                                    }
                                                    else if (z.ClassCalled.ToUpper().Contains("PROGRAMS"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".pgm";
                                                    }
                                                }

                                                var checkClassPresentOrNot = fileElementMaster.Where(p => p.ProjectId == projectId
                                                    && p.FileName == strName.Trim()).ToList();

                                                if (checkClassPresentOrNot.Count > 0)
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
                                }
                            }

                            #endregion
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

                        var allClassDataLinks = statementMaster.Where(p => p.ProjectId == projectId
                        && p.BaseCommandId == 19 && p.ClassNameDeclared != "null").ToList();

                        if (languageId == 4 || languageId == 5)
                        {
                            foreach (var s in allClassDataLinks)
                            {
                                if (string.IsNullOrEmpty(s.ClassNameDeclared)) continue;
                                if (!s.ClassNameDeclared.Contains(".cbl") && !s.ClassNameDeclared.Contains(".COBOL"))
                                {
                                    var callExecStatement = statementMaster.Where(p => p.ProjectId == projectId
                                        && p.FileId == s.FileId && p.BaseCommandId == 6).ToList();

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

                                            // Check the Class id called or not.
                                            var checkClassPresentOrNotLink = statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                && p.ClassCalled == dataObject).ToList();

                                            if (checkClassPresentOrNotLink.Count > 0)
                                            {
                                                foreach (var f in checkClassPresentOrNotLink)
                                                {
                                                    var checkClassCallMethod = statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 19
                                                        && p.FileId == f.FileId).ToList();

                                                    if (checkClassCallMethod.Count > 0)
                                                    {
                                                        string mulMethodName = "";

                                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                        {
                                                            var getClassCalled = statementMaster.Where(p => p.ProjectId == projectId && p.BaseCommandId == 6
                                                                && p.ClassCalled == checkClassCallMethod[e].ClassNameDeclared).ToList();

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

                                                                    var l = (from d in lstLinks where d.Origin == getClassCalled[g].StatementId && d.Target == f.StatementId select d).ToList();
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
                                                                                                f.MethodCalled.Substring(0,
                                                                                                        indexMethod)))
                                                                                        {

                                                                                            mulMethodName = mulMethodName + ", " +
                                                                                                            f.MethodCalled.Substring(0,
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

                                                                                            mulMethodName = mulMethodName + ", " +
                                                                                                           f.MethodCalled;

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

                                                                var k = (from d in lstLinks where d.Origin == iDofNode && d.Target == f.StatementId select d).ToList();
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

                                                                                        mulMethodName = mulMethodName + ", " +
                                                                                                        f.MethodCalled.Substring(0,
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

                                                                                        mulMethodName = mulMethodName + ", " +
                                                                                                       f.MethodCalled;

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
                           " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is null;");
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

                            var CheckClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                   .GetDataFromSqlQuery<ActionWorkflows>(
                       " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is not null group by OriginFileName;");

                            foreach (var chkNodes in CheckClassInNodes)
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
                                string[] strSplit = mNodes.Name.Split('.');
                                string strName = "";
                                if (strSplit.Length > 2)
                                {
                                    strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                }

                                var nodesData = new ConnectivityStepReference
                                {
                                    Name = strName.Trim().ToUpper(),
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

                // Start process for collecting workflow data here...

                IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramDataOld_10262016(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                var generalRepository = new GeneralRepository<DeadDataTypes>(new AppDbContext());
                var filemasterRepository = new FileMasterRepository(new AppDbContext());
                List<Node> lstNodes = new List<Node>();
                var objFlowChart = new FlowChart();

                var languageType = await _codeVortoService.ProjectMasterRepository
                    .GetDataFromSqlQuery<ProjectMaster>(
                    "select * from ProjectMaster where ProjectId=" + projectId + "");
                var languageId = 0;

                foreach (var languageTypeId in languageType)
                {
                    languageId = languageTypeId.LanguageId;
                }

                #region Runtime Created the flowchart

                try
                {
                    #region Add Nodes Logic

                    var allClassData =
                         await statementReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                             "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared != 'null';");

                    foreach (var s in allClassData)
                    {
                        Node nodeObject = new Node();

                        if (languageId == 1)
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
                        else
                        {
                            if (s.ClassNameDeclared.Contains("JCL"))
                            {
                                nodeObject.Id = s.StatementId;
                                nodeObject.Name = s.ClassNameDeclared;
                                nodeObject.ShapeId = "Circle";
                                nodeObject.Height = "15";
                                nodeObject.Width = "100";
                                //nodeObject.Color = "#00ffff";
                                nodeObject.Color = "#ffcc00";
                                nodeObject.StatementId = s.StatementId;
                                nodeObject.BaseCommandId = s.BaseCommandId;
                                lstNodes.Add(nodeObject);
                            }
                        }
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

                                if (j == 0)
                                {
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
                            }
                            #endregion
                        }
                        else
                        {
                            #region For Cobol Application
                            //foreach (var c in allCallingClassData)
                            //{
                            if (s.ClassNameDeclared.Trim().Contains("JCL"))
                            {

                                var callExecStatement = await statementReferenceRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                        "select * from statementreferencemaster where projectId=" + projectId + "" +
                                        " and FileId='" + s.FileId + "' and BaseCommandId=6;");

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

                                        string[] strSplit = c.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                        string strName = "";
                                        if (strSplit.Length > 2)
                                        {
                                            //strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                            if (strSplit[strSplit.Length - 2].StartsWith("PROC"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".proc";
                                            }
                                            else if (strSplit[strSplit.Length - 2].StartsWith("COBOL"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".cbl";
                                            }
                                            else if (strSplit[strSplit.Length - 2].StartsWith("JCL"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".jcl";
                                            }
                                            else if (c.ClassCalled.ToUpper().Contains("PROGRAMS"))
                                            {
                                                strName = strSplit[strSplit.Length - 1] + ".pgm";
                                            }

                                        }


                                        var checkClassPresentOrNot =
                                            await
                                                filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                                                    "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                                                    strName.Trim() + "';");

                                        if (checkClassPresentOrNot.Count > 0)
                                        {
                                            var fileIdPresent = 0;
                                            foreach (var p in checkClassPresentOrNot)
                                            {
                                                fileIdPresent = p.FileId;
                                            }

                                            callExecStatement = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "select * from statementreferencemaster where projectId=" + projectId + "" +
                                            " and FileId='" + fileIdPresent + "' and BaseCommandId=6;");

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

                                                string[] strSplit = z.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                                string strName = "";
                                                if (strSplit.Length > 2)
                                                {
                                                    //strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                                    if (strSplit[strSplit.Length - 2].StartsWith("PROC"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".proc";
                                                    }
                                                    else if (strSplit[strSplit.Length - 2].StartsWith("COBOL"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".cbl";
                                                    }
                                                    else if (strSplit[strSplit.Length - 2].StartsWith("JCL"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".jcl";
                                                    }
                                                    else if (z.ClassCalled.ToUpper().Contains("PROGRAMS"))
                                                    {
                                                        strName = strSplit[strSplit.Length - 1] + ".pgm";
                                                    }
                                                }

                                                var checkClassPresentOrNot =
                                                    await
                                                        filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                                                            "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                                                            strName.Trim() + "';");

                                                if (checkClassPresentOrNot.Count > 0)
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
                                }
                            }

                            #endregion
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
                                "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared!='null';");

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
                                    for (int i = 0; i < allDeadDatatypesLinks.Count; i++)
                                    {
                                        var keywordName = allDeadDatatypesLinks[i].KaywordName;
                                        if (dataObject != null && dataObject.Contains(keywordName))
                                        {
                                            j++;
                                        }

                                    }
                                    if (j == 0)
                                    {
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
                                                    "select * from statementreferencemaster where fileid= '" + fileId +
                                                    "' and statementId > '" + statementId + "' and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                    " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                    "') and MethodCalled is not null;");
                                            }


                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var l = (from d in lstNodes where d.Name == c.ClassNameDeclared select d).ToList();
                                                Link linkObject1 = new Link();
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
                                                                            checkClassCallMethod[e].MethodCalled.Substring(
                                                                                0, indexMethod)))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e].MethodCalled
                                                                                            .Substring(0, indexMethod);
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1).Replace("))", "");
                                                                    }

                                                                }
                                                                else
                                                                {

                                                                    //linkObject1.linkText = e.MethodCalled.ToString();
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e].MethodCalled;
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1).Replace("))", "");
                                                                    }
                                                                }


                                                                if (linkObject1.Origin != linkObject1.Target)
                                                                {
                                                                    if (e == checkClassCallMethod.Count - 1)
                                                                    {
                                                                        lstLinks.Add(linkObject1);
                                                                    }
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
                            }
                            #endregion
                        }
                        else
                        {
                            foreach (var s in allClassDataLinks)
                            {
                                if (!s.ClassNameDeclared.Contains(".cbl") && !s.ClassNameDeclared.Contains(".COBOL"))
                                {
                                    var callExecStatement = await statementReferenceRepository
                                   .GetDataFromSqlQuery<StatementReferenceMaster>(
                                       "select * from statementreferencemaster where projectId=" + projectId + "" +
                                       " and FileId='" + s.FileId + "' and BaseCommandId=6;");

                                    // Check the class the present or not.
                                    // Class is present to add the color : Yellow
                                    // Class is not present to add the color : Gray 

                                    #region Added the on dated 07.12.2016

                                    //var newList = callExecStatement.ToList();

                                    //foreach (var c in callExecStatement)
                                    //{

                                    //    var datatype = c.DataOrObjectType;
                                    //    var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                    //    string[] strSplit = c.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                    //    string strName = "";
                                    //    if (strSplit.Length > 2)
                                    //    {
                                    //        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                    //    }

                                    //    var checkClassPresentOrNot =
                                    //        await
                                    //            filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                                    //                "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                                    //                strName.Trim() + "';");

                                    //    if (checkClassPresentOrNot.Count > 0)
                                    //    {
                                    //        var fileIdPresent = 0;
                                    //        foreach (var p in checkClassPresentOrNot)
                                    //        {
                                    //            fileIdPresent = p.FileId;
                                    //        }
                                    //        //Node nodeObject1 = new Node
                                    //        //{
                                    //        //    Id = c.StatementId,
                                    //        //    Name = c.ClassCalled,
                                    //        //    ShapeId = "RoundRect",
                                    //        //    Height = "15",
                                    //        //    Width = "100",
                                    //        //    Color = "#00ffff",
                                    //        //    StatementId = s.StatementId,
                                    //        //    BaseCommandId = s.BaseCommandId,

                                    //        //};


                                    //        //List<int> iCnt = (from dd in lstNodes
                                    //        //                  where dd.Name == nodeObject1.Name
                                    //        //                  select dd.Id).ToList();
                                    //        //if (iCnt.Count == 0)
                                    //        //{
                                    //        //    lstNodes.Add(nodeObject1);
                                    //        //}

                                    //        callExecStatement = await statementReferenceRepository
                                    //    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    //        "select * from statementreferencemaster where projectId=" + projectId + "" +
                                    //        " and FileId='" + fileIdPresent + "' and BaseCommandId=6;");

                                    //        if (callExecStatement.Count > 0)
                                    //        {
                                    //            newList.AddRange(callExecStatement);
                                    //        }

                                    //    }
                                    //    else
                                    //    {
                                    //        //Node nodeObject2 = new Node
                                    //        //{
                                    //        //    Id = c.StatementId,
                                    //        //    Name = c.ClassCalled,
                                    //        //    ShapeId = "RoundRect",
                                    //        //    Height = "15",
                                    //        //    Width = "100",
                                    //        //    Color = "#C0C0C0",
                                    //        //    StatementId = s.StatementId,
                                    //        //    BaseCommandId = s.BaseCommandId,
                                    //        //    FileId = s.FileId
                                    //        //};

                                    //        //List<int> iCnt = (from dd in lstNodes
                                    //        //                  where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                    //        //                  select dd.Id).ToList();
                                    //        //if (iCnt.Count == 0)
                                    //        //{
                                    //        //    lstNodes.Add(nodeObject2);
                                    //        //}

                                    //    }
                                    //}

                                    //if (newList.Count > 0)
                                    //{
                                    //    foreach (var z in newList)
                                    //    {

                                    //        var datatype = z.DataOrObjectType;
                                    //        var dataObject = z.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                    //        string[] strSplit = z.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                    //        string strName = "";
                                    //        if (strSplit.Length > 2)
                                    //        {
                                    //            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                    //        }

                                    //        var checkClassPresentOrNot =
                                    //            await
                                    //                filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                                    //                    "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                                    //                    strName.Trim() + "';");

                                    //        if (checkClassPresentOrNot.Count > 0)
                                    //        {
                                    //            var fileIdPresent = 0;
                                    //            foreach (var p in checkClassPresentOrNot)
                                    //            {
                                    //                fileIdPresent = p.FileId;
                                    //            }
                                    //            //Node nodeObject1 = new Node
                                    //            //{
                                    //            //    Id = z.StatementId,
                                    //            //    Name = z.ClassCalled,
                                    //            //    ShapeId = "RoundRect",
                                    //            //    Height = "15",
                                    //            //    Width = "100",
                                    //            //    Color = "#00ffff",
                                    //            //    StatementId = s.StatementId,
                                    //            //    BaseCommandId = s.BaseCommandId,

                                    //            //};


                                    //            //List<int> iCnt = (from dd in lstNodes
                                    //            //                  where dd.Name == nodeObject1.Name
                                    //            //                  select dd.Id).ToList();
                                    //            //if (iCnt.Count == 0)
                                    //            //{
                                    //            //    lstNodes.Add(nodeObject1);
                                    //            //}

                                    //            //    callExecStatement = await statementReferenceRepository
                                    //            //.GetDataFromSqlQuery<StatementReferenceMaster>(
                                    //            //    "select * from statementreferencemaster where projectId=" + projectId + "" +
                                    //            //    " and FileId='" + fileIdPresent + "' and BaseCommandId=6;");

                                    //            //    if (callExecStatement.Count > 0)
                                    //            //    {
                                    //            //        newList.AddRange(callExecStatement);
                                    //            //    }

                                    //        }
                                    //        else
                                    //        {
                                    //            //Node nodeObject2 = new Node
                                    //            //{
                                    //            //    Id = z.StatementId,
                                    //            //    Name = z.ClassCalled,
                                    //            //    ShapeId = "RoundRect",
                                    //            //    Height = "15",
                                    //            //    Width = "100",
                                    //            //    Color = "#C0C0C0",
                                    //            //    StatementId = s.StatementId,
                                    //            //    BaseCommandId = s.BaseCommandId,
                                    //            //    FileId = s.FileId
                                    //            //};

                                    //            //List<int> iCnt = (from dd in lstNodes
                                    //            //                  where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                    //            //                  select dd.Id).ToList();
                                    //            //if (iCnt.Count == 0)
                                    //            //{
                                    //            //    lstNodes.Add(nodeObject2);
                                    //            //}

                                    //        }

                                    //    }
                                    //}

                                    #endregion

                                    foreach (var c in callExecStatement)
                                    //foreach (var c in newList)
                                    {
                                        if (!string.IsNullOrEmpty(c.ClassCalled))
                                        {
                                            var iDofNode = s.StatementId;
                                            var fileId = s.FileId;
                                            var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "");

                                            // Check the Class id called or not.
                                            var checkClassPresentOrNotLink = await statementReferenceRepository
                                               .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "select * from statementreferencemaster where BaseCommandId=6 and projectId=" + projectId + "" +
                                                    " and ClassCalled='" + dataObject + "';");

                                            if (checkClassPresentOrNotLink.Count > 0)
                                            {
                                                foreach (var f in checkClassPresentOrNotLink)
                                                {

                                                    var checkClassCallMethod = await statementReferenceRepository
                                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                            "select * from statementreferencemaster where BaseCommandId=19 and projectId=" + projectId + "" +
                                                            " and FileId='" + f.FileId + "';");

                                                    if (checkClassCallMethod.Count > 0)
                                                    {
                                                        string mulMethodName = "";

                                                        for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                        {
                                                            var getClassCalled = await statementReferenceRepository
                                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                             "select * from statementreferencemaster where BaseCommandId=6 and projectId=" + projectId + "" +
                                                            " and ClassCalled='" + checkClassCallMethod[e].ClassNameDeclared + "';");

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

                                                                    var l = (from d in lstLinks where d.Origin == getClassCalled[g].StatementId && d.Target == f.StatementId select d).ToList();
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
                                                                                                f.MethodCalled.Substring(0,
                                                                                                        indexMethod)))
                                                                                        {

                                                                                            mulMethodName = mulMethodName + ", " +
                                                                                                            f.MethodCalled.Substring(0,
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

                                                                                            mulMethodName = mulMethodName + ", " +
                                                                                                           f.MethodCalled;

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

                                                                var k = (from d in lstLinks where d.Origin == iDofNode && d.Target == f.StatementId select d).ToList();
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

                                                                                        mulMethodName = mulMethodName + ", " +
                                                                                                        f.MethodCalled.Substring(0,
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

                                                                                        mulMethodName = mulMethodName + ", " +
                                                                                                       f.MethodCalled;

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

                                                        //break;
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
                           " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is null;");
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

                            var CheckClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                   .GetDataFromSqlQuery<ActionWorkflows>(
                       " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is not null group by OriginFileName;");

                            foreach (var chkNodes in CheckClassInNodes)
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
                                string[] strSplit = mNodes.Name.Split('.');
                                string strName = "";
                                if (strSplit.Length > 2)
                                {
                                    strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
                                }

                                var nodesData = new ConnectivityStepReference
                                {
                                    Name = strName.Trim().ToUpper(),
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

                //#region region 2...To Updated ClassCalled Column in StatementreferenceMaster
                //string execSqlNew =
                //    "  Select * from statementreferencemaster where BaseCommandId=6 AND ProjectId = " + projectId + " and ClassCalled is not null;";
                //var execClassCalled = await
                //    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNew);
                //foreach (var constructor in execClassCalled)
                //{
                //    var classname = constructor.ClassCalled;
                //    constructor.ClassCalled = classname.Remove(classname.Remove(classname.Length - 1).LastIndexOf('.'));
                //    await
                //         _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                //}
                //#endregion

                //#region region 2...To Updated ClassNameDeclared Column in StatementreferenceMaster
                //string execSqlNewDec =
                //    "  SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared != 'null';";
                //var execClassNameDeclared = await
                //    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNewDec);
                //foreach (var constructor in execClassNameDeclared)
                //{
                //    var classnamedeclared = constructor.ClassNameDeclared;
                //    constructor.ClassNameDeclared = classnamedeclared.Remove(classnamedeclared.Remove(classnamedeclared.Length - 1).LastIndexOf('.'));
                //    await
                //         _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                //}
                //#endregion

                // Start process for collecting workflow data here...

                //IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                //await processResult.ExecuteAsync(CancellationToken.None);
                return Ok("Connectivity diagram data process completed successfully.");
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
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch'); ");
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
                object[] param = { 
                                   new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                                   new MySqlParameter("@className", MySqlDbType.VarChar){Value = clsName},
                                   new MySqlParameter("@classSplitName", MySqlDbType.VarChar){Value = clsName.Split('.').LastOrDefault()}
                               };
                //Expression<Func<StatementReferenceMaster, bool>> expression =
                //    master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName || master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.ExecuteStoreProcedure<StatementReferenceMaster>("SpBaseStatementMaster", param);

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

                foreach (var treeItem in lstTreeView)
                {
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;

                    auto++;
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId, treeItem.StatementReferenceMaster.FileId, indentLevel,
                        ref auto, ref treeNodeId);
                    if (!copyOfLstTreeView.Any()) continue;
                    treeItem.HasChild = true;
                }

                var newList = copyOfLstTreeView.ToList();
                foreach (var treeItem in newList)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
                    auto++;
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId, indentLevel,
                        ref auto, ref treeNodeId);
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
                foreach (var treeItem in copyOfLstTreeView.FindAll(t => t.BaseCommandId == 6 || t.BaseCommandId == 5).ToList())
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
                        //if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
                        //{
                        //    treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
                        //                "</span>";
                        //    AssignColorsToChildNodes(child, ref copyOfLstTreeView,
                        //        colorArray[tempId]);
                        //    tempId++;
                        //}
                        if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
                        {
                            try
                            {
                                string groupName = string.Empty;
                                if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                                {
                                    string iOreCall = child.BaseCommandId == 6 ? "External Call" : "Internal Call";
                                    string methodCalled = child.StatementReferenceMaster.MethodCalled.Substring(0,
                                        child.StatementReferenceMaster.MethodCalled.IndexOf('('));
                                    if (!string.IsNullOrEmpty(child.StatementReferenceMaster.BusinessName))
                                    {
                                        groupName = "(" + child.StatementReferenceMaster.BusinessName.Trim() + ")(" +
                                                           methodCalled + ")(" + iOreCall + ")";
                                    }
                                    else
                                    {
                                        groupName = "(" + methodCalled + ")(" +
                                                           methodCalled + ")(" + iOreCall + ")";
                                    }
                                    groupId = groupId + 1;
                                }
                                treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
                                            "</span>";
                                AssignColorsToChildNodes(child, ref copyOfLstTreeView, colorArray[tempId], groupName, groupId);
                                tempId++;
                            }
                            catch (Exception ex)
                            {
                                ex.ToString();
                            }

                        }
                        else
                            child.GraphName = "<span style='color: green;'>" + child.GraphName +
                                              "</span>";
                    }
                    //tempId++;
                }



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

                allSeqListItemsNew.Where(a => a.BaseCommandId == 10).ToList().ForEach(b =>
                {
                    b.Done = false;
                });

                allSeqListItemsNew = allSeqListItemsNew.DistinctBy().ToList();

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

                secondTab.AddRange(allSeqListItemsNew
                    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 || item.BaseCommandId == 8 ||
                                   item.BaseCommandId == 1 || item.BaseCommandId == 25
                                   || item.PrimaryCommandId == 1 || item.BaseCommandId == 5 || item.BaseCommandId == 10
                               || item.PrimaryCommandId == 2));

                var tempList =
                   (from d in secondTab
                    where d.BaseCommandId == 1
                        || d.BaseCommandId == 2
                        || d.BaseCommandId == 25
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
                indexPosition = -1;
                foreach (var treeItem in copyOfLstTreeView)
                {
                    indexPosition++;
                    if (treeItem.BaseCommandId != 10) continue;
                    for (var i = indexPosition; i >= 0; i--)
                    {
                        if (lstTreeView[i].BaseCommandId != 1) continue;
                        treeItem.ParentId = lstTreeView[i].GraphId;
                        break;
                    }
                }

                //int nodeId = 100;
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
                //var methodChildItems =
                //    (from a in allSeqListItems where firstItem.GraphId == a.ParentId select a).ToList();
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList().Distinct();
                int linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
                    {
                        #region PrimaryCommandId == 1 || BaseCommandId == 1
                        string condition;
                        string ifPart = Regex.Split(curItem.GraphName, "If", RegexOptions.IgnoreCase)[1];
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
                        var childItems = (from s in secondTab where curItem.GraphId == s.ParentId select s).ToList().Distinct();
                        foreach (var cItem in childItems)
                        {
                            treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
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
                        try
                        {
                            if (m != null && m != "NULL")
                            {
                                treeView.Links.Add(new Link
                                {
                                    //Origin = treeView.Nodes.First().Id,
                                    //Target = nodeId,
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId = Int32.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1])
                                });
                            }
                            else
                            {
                                treeView.Links.Add(new Link
                                {
                                    //Origin = treeView.Nodes.First().Id,
                                    //Target = nodeId,
                                    Origin = nodeId - 1,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] "
                                });
                            }
                        }
                        catch (Exception ex)
                        {
                            ex.ToString();
                        }
                        linkSeqNumber++;
                        var childItems = (from s in secondTab
                                          where s.ParentId == curItem.GraphId
                                              && s.BaseCommandId != 25
                                          select s).ToList().Distinct();
                        foreach (var cItem in childItems)
                        {
                            treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
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
                                "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + curItem.StatementReferenceMaster.FileId + ";");
                        if (methodCalled.Count != 0)
                            nodeColor = "#00ffff";
                        nodeId++;
                        Node node;
                        if (curItem.PrimaryCommandId == 24)
                        {
                            string[] strSplit = methodCalled[0].ClassNameDeclared.Split('.');
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
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
                        foreach (var cItem in childItems)
                        {
                            treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
                        }
                        #endregion
                    }
                    else if (curItem.BaseCommandId == 10)
                    {
                        #region BaseCommandId == 10
                        nodeId++;
                        var parentIf =
                            (from p in secondTab where curItem.ParentId == p.GraphId select p).FirstOrDefault();
                        var ifPart = Regex.Split(parentIf.GraphName, "IF", RegexOptions.IgnoreCase)[1];
                        var condition = ifPart.Contains("THEN")
                            ? ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture))
                            : ifPart;
                        condition = "IF NOT" + condition + " THEN";
                        var width = CalculateWidth(condition.Length);
                        var height = CalculateHeight(condition.Length);
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

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First()); // RemoveNodes(treeView.Nodes, treeView.Links);
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
                    //StatementId = sTab.StatementReferenceMaster.StatementId.ToString()
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel
                }).ToList();

                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

                #region To Updated the GraphName against the BaseCommandId

                var statementSecondTab = await generalRepositoryWorkflowTreeviewSecondTabDetails.GetAllItems(p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                foreach (var stmt in statementSecondTab)
                {
                    if (stmt.BaseCommandId == 1 || stmt.BaseCommandId == 5 || stmt.BaseCommandId == 6)
                    {
                        stmt.GraphName = stmt.GraphName + "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" + stmt.ActualStatementId.Split('_')[1] + ")'/>";

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
                lstData.Add(treeViewDataNew);
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */
                return Ok("Workflow data collected successfully");
            }
        }

        private TreeViewData ProcessChildItemsCallInternal(int projectId, List<TreeView> secondTab, TreeView treeView, TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

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
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6
                var item = treeView;
                string nodeColor = "#c0c0c0";
                string andCondition = " BaseCommandId = 19 AND " +
                                      " ( ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                      " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar){Value = andCondition}
                };
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters).Result;
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
                if (treeView.PrimaryCommandId == 25)
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
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5
                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 24)
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
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
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
                condition = "IF NOT" + condition + " THEN";
                width = CalculateWidth(condition.Length);
                height = CalculateHeight(condition.Length);
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

        private TreeViewData ProcessChildItemsIf(int projectId, List<TreeView> secondTab, TreeView treeView, TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

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
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 6)
            {
                #region PrimaryCommandId == 6
                var item = treeView;
                string nodeColor = "#c0c0c0";
                string andCondition = " BaseCommandId = 19 AND " +
                                      " ( ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                      " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "'); ";
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar){Value = andCondition}
                };
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters).Result;
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
                if (treeView.PrimaryCommandId == 25)
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
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5
                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 24)
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
                        //Origin = treeViewData.Nodes.First().Id,
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
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
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
                condition = "IF NOT" + condition + " THEN";
                width = CalculateWidth(condition.Length);
                height = CalculateHeight(condition.Length);
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

        private TreeViewData ProcessChildItemsCallExt(int projectId, List<TreeView> secondTab, TreeView treeView, TreeViewData treeViewData,
            Node lastNode, ref int nodeId, ref int linkSeqNumber)
        {
            string width = "0";
            string height = "0";

            if (treeView.BaseCommandId == 1 || treeView.PrimaryCommandId == 1)
            {
                #region BaseCommandId == 1 || treeView.PrimaryCommandId == 1
                string condition;
                string ifPart = Regex.Split(treeView.GraphName, "If", RegexOptions.IgnoreCase)[1];// treeView.GraphName.Split(new[] { "If" }, StringSplitOptions.None)[1];
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
                    //Origin = lastNode.Id,
                    Origin = nodeId - 1,
                    Target = nodeId,
                    LinkText = "[" + linkSeqNumber + "] "
                });
                linkSeqNumber++;
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsIf(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
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
                                     " ( ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled.Split('.').LastOrDefault() + "' OR " +
                                     " ClassNameDeclared = '" + item.StatementReferenceMaster.ClassCalled + "' ); ";
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar){Value = andCondition}
                };
                var classNameDeclared = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", parameters).Result;

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
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 5)
            {
                #region BaseCommandId == 5
                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;
                Node node;
                if (treeView.PrimaryCommandId == 24)
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
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                }
                #endregion
            }
            else if (treeView.BaseCommandId == 8)
            {
                #region BaseCommandId == 8
                string nodeColor = "#c0c0c0";
                var methodCalled = _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + treeView.StatementReferenceMaster.FileId + ";").Result;
                if (methodCalled.Count != 0)
                    nodeColor = "#00ffff";
                nodeId++;

                Node node;
                if (treeView.PrimaryCommandId == 23)
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
                if (treeView.PrimaryCommandId == 23)
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
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId && s.BaseCommandId != 25 select s)
                    .ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallExt(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
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
                condition = "IF NOT" + condition + " THEN";
                width = CalculateWidth(condition.Length);
                height = CalculateHeight(condition.Length);
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

        private void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView, string color, string groupName, int groupId)
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

        //protected void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
        //    string color)
        //{
        //    treeView.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeView.GraphName +
        //                         "</span>";
        //    var childItems =
        //        (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
        //    foreach (var child in childItems)
        //    {
        //        child.GraphName = "<span style='color: " + color + ";'>" + child.GraphName +
        //                          "</span>";
        //    }
        //    //return lstTreeView;
        //}

        protected List<Link> RemoveMultipleLinks(List<Link> lstLinks, List<Node> lstNodes)
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

        private static List<TreeView> AttachChildItems(List<TreeView> allSeqListItems, List<TreeView> secondTab, TreeView curItem)
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
          int projectId, int indentLevel, ref int auto, ref int treeNodeId)
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

                        var stmtsBlock = GetMethodBlock(blockStmtId);
                        int chkparentIdMethod = 0;

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
                                    lstTreeView, projectId, indentLevel, ref auto, ref treeNodeId);
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
                                    lstTreeView, projectId, block.FileId, indentLevel, ref auto, ref treeNodeId);
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
            int projectId, int fileId, int indentLevel, ref int autoInt, ref int treeNodeId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                string methodName = treeView.MethodCalled;
                object[] parameters = { 
                                          new MySqlParameter("@bCommandId", MySqlDbType.Int32){Value = 8},
                                          new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                                          new MySqlParameter("@methodNm", MySqlDbType.VarChar){Value = methodName},
                                          new MySqlParameter("@fileId", MySqlDbType.Int32){Value = fileId},
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
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, block.FileId, indentLevel, ref autoInt, ref treeNodeId);
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
                                lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, indentLevel, ref autoInt, ref treeNodeId);
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


        public async Task<IHttpActionResult> ParseJclFile(int projectId, int fileId, List<FileMaster> copyOfFileMaster, string projectName, string pPath, int languageId, List<string> inputList, string filePath)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                //string classNameDeclared = "";
                string parent = string.Empty, child = string.Empty;
                string RemainStat = string.Empty;
                string JclProgname = string.Empty;
                int methodJCL = 0;

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
                var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                //var findIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Find Start")
                //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                #region JCL files processing Logic

                try
                {
                    List<int> lstFirstIndexWS = new List<int>();
                    lstFirstIndexWS =
                        inputList.Select((s, i) => new { Str = s, Index = i })
                            .Where(x => x.Str.Contains(" exec ".ToUpper())
                                && ((x.Str.StartsWith("//*") == false)))
                            .Select(x => x.Index)
                            .ToList<int>();


                    if (lstFirstIndexWS.Count > 0)
                    {
                        for (int j = 0; j < lstFirstIndexWS[0] - 1; j++)
                        {
                            if (inputList[j].Trim() != "")
                            {
                                string stmt = "";
                                try
                                {
                                    if (inputList[j] != string.Empty && inputList[j].Trim().StartsWith("/"))
                                    {
                                        stmt = inputList[j].Trim().Substring(2);
                                    }
                                }
                                catch
                                {
                                    stmt = "";
                                }
                                Regex trimmer = new Regex(@"\s\s+");
                                stmt = trimmer.Replace(stmt, " ");
                                bool isAllSame = stmt.All(d => d == '*');
                                try
                                {
                                    if ((stmt.Length > 3) && (!isAllSame))
                                    {
                                        if (!stmt.Trim().StartsWith("*="))
                                        {
                                            if (!stmt.Trim().StartsWith("="))
                                            {
                                                if (!stmt.Trim().StartsWith("**"))
                                                {
                                                    stmt = stmt.Replace("'", "''");
                                                    if (stmt.EndsWith(","))
                                                    {
                                                        if (stmt.Contains(' '))
                                                        {
                                                            stmt = stmt.Split(' ').Last();
                                                        }
                                                    }

                                                    child = stmt.TrimStart().TrimEnd();
                                                    if (stmt.Trim().StartsWith("*") == false)
                                                    {
                                                        #region Insert normal statements logic

                                                        if (child.Length > 0)
                                                        {
                                                            if (!child.StartsWith("*"))
                                                            {
                                                                var statementReferenceMaster1 = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    PrimaryCommandId = 0,
                                                                    ProjectId = projectId
                                                                };
                                                                await
                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster1);
                                                            }
                                                        }
                                                        #endregion
                                                    }

                                                }
                                            }
                                        }
                                    }
                                }
                                catch (Exception ex)
                                {
                                    ex.ToString();
                                }
                            }
                        }

                        #region INSERTING into StatementReferenceMaster table
                        string strOnlyFileName = "";
                        int iIndex = 0;
                        iIndex = filePath.LastIndexOf("\\");
                        if (iIndex > 0)
                        {
                            strOnlyFileName = filePath.Substring(iIndex + 1);
                            strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "").Replace(".JCL", "");
                        }
                        for (int iCnt = 0; iCnt < lstFirstIndexWS.Count; iCnt = iCnt + 1)
                        {
                            List<string> textLineWSNew = new List<string>();
                            if (iCnt + 1 < lstFirstIndexWS.Count)
                            {
                                textLineWSNew =
                                    inputList.Skip(lstFirstIndexWS[iCnt])
                                        .Take((lstFirstIndexWS[iCnt + 1]) - (lstFirstIndexWS[iCnt]))
                                        .ToList<string>();
                            }
                            else if (iCnt + 1 == lstFirstIndexWS.Count)
                            {
                                //textLineWSNew =
                                //    inputList.Skip(lstFirstIndexWS[iCnt]).Take((lstFirstIndexWS[iCnt])).ToList<string>();
                                textLineWSNew = inputList.Skip(lstFirstIndexWS[iCnt]).Take((inputList.Count - 1)).ToList<string>();
                            }

                            try
                            {
                                if (textLineWSNew.Count > 0)
                                {
                                    for (int i = 0; i < textLineWSNew.Count; i++)
                                    {
                                        string Statement = "";
                                        Statement = textLineWSNew[i];
                                        if (Statement.Length < 6)
                                        {

                                        }
                                        else
                                        {
                                            if (Statement.Length >= 60 && Statement.Length <= 70)
                                            {
                                                if (Statement != string.Empty && Statement.Trim().StartsWith("/"))
                                                {
                                                    Statement = Statement.Substring(2, Statement.Length - 2);
                                                }
                                            }
                                            else if (Statement.Length > 70)
                                            {
                                                if (Statement != string.Empty && Statement.Trim().StartsWith("/"))
                                                {
                                                    Statement = Statement.Substring(2, 68);
                                                }
                                            }
                                            else
                                            {
                                                if (Statement != string.Empty && Statement.Trim().StartsWith("/"))
                                                {
                                                    Statement = Statement.Substring(2);
                                                }
                                            }

                                            if (Statement.Contains(" EXEC ") &&
                                                (Statement.TrimStart().StartsWith("*") == false))
                                            {
                                                //insert the parent
                                                parent = Statement.Split(' ')[0];
                                                if (parent == "")
                                                {
                                                    parent = strOnlyFileName;
                                                }

                                                #region Insert Only Method logic
                                                var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                if (parent.Length > 0)
                                                {
                                                    if (methodJCL == 0)
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = fileId,
                                                            ResolvedStatement = parent.Trim().Replace("//", ""),
                                                            OriginalStatement = parent.Trim().Replace("//", ""),
                                                            ClassCalled = null,
                                                            MethodName = parent.Trim() + "()",
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = methodStart.BaseCommandId,
                                                            PrimaryCommandId = methodStart.PrimaryReferenceId,
                                                            ProjectId = projectId
                                                        };

                                                        await
                                                  _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                    }
                                                    else
                                                    {

                                                        var stmtReferenceMasterNew = new StatementReferenceMaster
                                                        {
                                                            FileId = fileId,
                                                            ResolvedStatement = parent,
                                                            OriginalStatement = parent,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            PrimaryCommandId = 0,
                                                            BaseCommandId = 0,
                                                            ProjectId = projectId
                                                        };

                                                        await
                                                  _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterNew);
                                                    }
                                                    methodJCL++;
                                                }

                                                #endregion


                                                var start = Statement.IndexOf(" ");
                                                child = Statement.Substring(start).TrimStart().TrimEnd();
                                                if (child.EndsWith(","))
                                                {
                                                    child = child + " " + textLineWSNew[i + 1].Replace("//", "").Trim();
                                                    i++;
                                                }

                                                if (child.Length > 0)
                                                {

                                                    #region Insert EXEC statement logic
                                                    var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                    if (child.Length > 0)
                                                    {
                                                        if (!child.StartsWith("*"))
                                                        {

                                                            if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = execCicsJclStart.BaseCommandId,
                                                                    PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                    ProjectId = projectId
                                                                };

                                                                await
                                                      _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                continue;
                                                            }
                                                        }
                                                    }

                                                    #endregion

                                                    #region Insert END-EXEC statement logic
                                                    var execCicsEnd = callExternalIndicationEnd.Find(x => x.EndIndicator != null);
                                                    if (child.Length > 0)
                                                    {
                                                        if (!child.StartsWith("*"))
                                                        {
                                                            if (child.StartsWith(execCicsEnd.StartIndicator))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = execCicsEnd.BaseCommandId,
                                                                    PrimaryCommandId = execCicsEnd.PrimaryReferenceId,
                                                                    ParsedOrNot = "1",
                                                                    ProjectId = projectId
                                                                };
                                                                await
                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                    #endregion

                                                    #region Insert If statement logic
                                                    var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                                    if (child.Length > 0)
                                                    {
                                                        if (!child.StartsWith("*"))
                                                        {
                                                            if (child.StartsWith(ifStart.StartIndicator))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = ifStart.BaseCommandId,
                                                                    PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                                    ProjectId = projectId
                                                                };

                                                                await
                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                continue;
                                                            }

                                                        }

                                                    }

                                                    #endregion

                                                    #region Insert End If statement logic
                                                    var ifEnd = ifConditionEnd.Find(x => x.EndIndicator != null);
                                                    if (child.Length > 0)
                                                    {
                                                        child = child.Replace("ENDIF", "END-IF").Replace("END IF", "END-IF");
                                                        if (child.StartsWith(ifEnd.StartIndicator))
                                                        {
                                                            if (!child.StartsWith("*"))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = ifEnd.BaseCommandId,
                                                                    PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                    ParsedOrNot = "1",
                                                                    ProjectId = projectId
                                                                };
                                                                await
                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                continue;

                                                            }
                                                        }
                                                    }
                                                    #endregion

                                                    #region Insert normal statements logic

                                                    if (child.Length > 0)
                                                    {
                                                        if (!child.Trim().Replace("//", "").StartsWith("*"))
                                                        {
                                                            var statementReferenceMasterNor = new StatementReferenceMaster
                                                            {
                                                                FileId = fileId,
                                                                ResolvedStatement = child,
                                                                OriginalStatement = child,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                PrimaryCommandId = 0,
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterNor);
                                                        }
                                                    }
                                                    #endregion

                                                }

                                            }
                                            else
                                            {
                                                Regex trimmer = new Regex(@"\s\s+");
                                                Statement = trimmer.Replace(Statement, " ");
                                                bool isAllSame = Statement.All(d => d == '*');
                                                if ((Statement.Length > 3) && (!isAllSame))
                                                {
                                                    if ((!Statement.Trim().StartsWith("*=")) &&
                                                        (!Statement.Trim().StartsWith("*")))
                                                    {
                                                        if (!Statement.Trim().StartsWith("="))
                                                        {
                                                            if (!Statement.Trim().StartsWith("**"))
                                                            {
                                                                Statement = Statement.Replace("'", "''").Trim();
                                                                if (Statement.EndsWith(","))
                                                                {
                                                                    for (int j = 1; j < textLineWSNew[i].Length; j++)
                                                                    {
                                                                        if (Statement.EndsWith(","))
                                                                        {
                                                                            Statement = Statement + " " + textLineWSNew[i + 1].Replace("//", "").Trim();
                                                                            i++;
                                                                        }
                                                                        else
                                                                        {
                                                                            break;
                                                                        }
                                                                    }
                                                                }
                                                                child = Statement.TrimStart().TrimEnd();

                                                                if (child.Length > 0)
                                                                {

                                                                    #region Insert EXEC  statement logic
                                                                    var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                                    if (child.Length > 0)
                                                                    {
                                                                        if (!child.StartsWith("*"))
                                                                        {
                                                                            if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = fileId,
                                                                                    ResolvedStatement = child,
                                                                                    OriginalStatement = child,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                                    ProjectId = projectId
                                                                                };

                                                                                await
                                                                      _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                continue;
                                                                            }
                                                                        }
                                                                    }

                                                                    #endregion

                                                                    #region Insert END-EXEC statement logic
                                                                    var execCicsEnd = callExternalIndicationEnd.Find(x => x.EndIndicator != null);
                                                                    if (child.Length > 0)
                                                                    {
                                                                        if (!child.StartsWith("*"))
                                                                        {
                                                                            if (child.StartsWith(execCicsEnd.StartIndicator))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = fileId,
                                                                                    ResolvedStatement = child,
                                                                                    OriginalStatement = child,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = execCicsEnd.BaseCommandId,
                                                                                    PrimaryCommandId = execCicsEnd.PrimaryReferenceId,
                                                                                    ParsedOrNot = "1",
                                                                                    ProjectId = projectId
                                                                                };
                                                                                await
                                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                continue;
                                                                            }
                                                                        }
                                                                    }
                                                                    #endregion

                                                                    #region Insert If statement logic
                                                                    var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                                                    if (child.Length > 0)
                                                                    {
                                                                        if (!child.StartsWith("*"))
                                                                        {
                                                                            if (child.StartsWith(ifStart.StartIndicator))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = fileId,
                                                                                    ResolvedStatement = child,
                                                                                    OriginalStatement = child,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = ifStart.BaseCommandId,
                                                                                    PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                                                    ProjectId = projectId
                                                                                };

                                                                                await
                                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                                continue;
                                                                            }

                                                                        }

                                                                    }

                                                                    #endregion

                                                                    #region Insert End If statement logic
                                                                    var ifEnd = ifConditionEnd.Find(x => x.EndIndicator != null);
                                                                    if (child.Length > 0)
                                                                    {
                                                                        child = child.Replace("ENDIF", "END-IF").Replace("END IF", "END-IF");
                                                                        if (child.StartsWith(ifEnd.StartIndicator))
                                                                        {
                                                                            if (!child.StartsWith("*"))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = fileId,
                                                                                    ResolvedStatement = child,
                                                                                    OriginalStatement = child,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = ifEnd.BaseCommandId,
                                                                                    PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                                    ParsedOrNot = "1",
                                                                                    ProjectId = projectId
                                                                                };
                                                                                await
                                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                continue;

                                                                            }
                                                                        }
                                                                    }
                                                                    #endregion

                                                                    #region Insert normal statements logic

                                                                    if (child.Length > 0)
                                                                    {
                                                                        if (!child.StartsWith("*"))
                                                                        {
                                                                            var statementReferenceMasterNew = new StatementReferenceMaster
                                                                            {
                                                                                FileId = fileId,
                                                                                ResolvedStatement = child,
                                                                                OriginalStatement = child,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                PrimaryCommandId = 0,
                                                                                ProjectId = projectId
                                                                            };
                                                                            await
                                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterNew);
                                                                        }
                                                                    }
                                                                    #endregion

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
                            catch
                            {
                            }
                        }

                        #endregion
                    }

                    #region Insert Method End logic
                    var methodEnd = methodIndicationEnd.Find(x => x.StartIndicator == null);
                    var stmtReferenceMasterEndMet = new StatementReferenceMaster
                    {
                        FileId = fileId,
                        ResolvedStatement = "END",
                        OriginalStatement = "END",
                        ClassCalled = null,
                        MethodName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        BaseCommandId = methodEnd.BaseCommandId,
                        PrimaryCommandId = methodEnd.PrimaryReferenceId,
                        ProjectId = projectId
                    };

                    await
              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterEndMet);
                    #endregion
                }
                catch (Exception ex)
                {
                    ex.ToString();
                }

                #endregion
            }

            return Ok("Jcl file parssed successfully. ");

        }

        public async Task<IHttpActionResult> ParseProcFile(int projectId, int fileId, List<FileMaster> copyOfFileMaster, string projectName, string pPath, int languageId, List<string> inputList, string filePath)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string parent = string.Empty, child = string.Empty;
                string RemainStat = string.Empty;
                string JclProgname = string.Empty;
                int methodProc = 0;

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
                var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                //var findIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Find Start")
                //    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == projectDetails.LanguageId);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                #region PROCS files processing Logic

                if (inputList.Count > 0)
                {
                    try
                    {

                        List<int> lstFirstIndexWS =
            inputList.Select((s, i) => new { Str = s, Index = i })
                .Where(x => x.Str.Contains("exec".ToUpper())
                        && x.Str.StartsWith("//*") == false)
                .Select(x => x.Index)
                .ToList<int>();


                        if (lstFirstIndexWS.Count > 0)
                        {
                            for (int j = 0; j < lstFirstIndexWS[0] - 1; j++)
                            {
                                if (inputList[j] == "")
                                {
                                    break;
                                }

                                string stmt = string.Empty;
                                try
                                {
                                    if (inputList[j].Length > 70)
                                    {
                                        stmt = inputList[j].Trim().Substring(2, 70);
                                    }
                                    else
                                    {
                                        stmt = inputList[j].Trim();
                                    }
                                }
                                catch (Exception ex)
                                {
                                    Console.WriteLine(ex.InnerException);
                                }
                                Regex trimmer = new Regex(@"\s\s+");
                                stmt = trimmer.Replace(stmt, " ");
                                bool isAllSame = stmt.All(d => d == '*');
                                if ((stmt.Length > 3) && (!isAllSame))
                                {
                                    try
                                    {
                                        if (!stmt.Trim().StartsWith("*="))
                                        {
                                            if (!stmt.Trim().StartsWith("="))
                                            {
                                                if (!stmt.Trim().StartsWith("**"))
                                                {
                                                    stmt = stmt.Replace("'", "").Replace("\"", "");
                                                    if (stmt.EndsWith(","))
                                                    {
                                                        stmt = stmt.Split(' ').Last();
                                                    }
                                                    child = stmt.TrimStart().TrimEnd();
                                                    if (stmt.Trim().StartsWith("*") == false)
                                                    {
                                                        #region Insert normal statements logic

                                                        if (child.Length > 0)
                                                        {
                                                            if (!child.Replace("//", "").StartsWith("*"))
                                                            {
                                                                var statementReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = fileId,
                                                                    ResolvedStatement = child,
                                                                    OriginalStatement = child,
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
                                                        }
                                                        #endregion
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    catch (Exception ee)
                                    {
                                        Console.WriteLine(ee.InnerException);
                                    }
                                }
                            }

                            #region INSERT THE STATEMENT IN STATEMENTREFERENCE TABLE

                            string strOnlyFileName = "";
                            int iIndex = 0;
                            iIndex = filePath.LastIndexOf("\\");
                            if (iIndex > 0)
                            {
                                strOnlyFileName = filePath.Substring(iIndex + 1);
                                strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "").Replace("PROC", "");
                            }

                            for (int iCnt = 0; iCnt < lstFirstIndexWS.Count; iCnt++)
                            {
                                List<string> textLineWSProc = new List<string>();
                                if (iCnt < lstFirstIndexWS.Count - 1)
                                {
                                    textLineWSProc =
                                        inputList.Skip(lstFirstIndexWS[iCnt])
                                            .Take((lstFirstIndexWS[iCnt + 1]) - (lstFirstIndexWS[iCnt]))
                                            .ToList<string>();
                                }
                                else
                                {
                                    textLineWSProc =
                                        inputList.Skip(lstFirstIndexWS[iCnt])
                                            .Take((inputList.Count) - (lstFirstIndexWS[iCnt]))
                                            .ToList<string>();
                                }

                                string stmtNew = "";
                                int stmtFlag = 0;

                                for (int i = 0; i < textLineWSProc.Count; i++)
                                {
                                    string currentState = textLineWSProc[i].ToString();
                                    string Statement = "";
                                    if (currentState.Length > 71)
                                    {
                                        Statement = currentState.Substring(2, 70);
                                    }
                                    else
                                    {
                                        Statement = currentState;
                                    }
                                    string stmt = "";
                                    if (stmtFlag > 0)
                                    {
                                        stmtNew = stmtNew.Replace("//", "").Trim() + Statement.Replace("//", "").TrimStart().TrimEnd();
                                    }
                                    else
                                    {
                                        stmtNew = Statement.TrimStart().TrimEnd();
                                    }

                                    if (Statement.Contains("EXEC") && (Statement.TrimStart().StartsWith("*") == false))
                                    {
                                        //insert the parent
                                        if (Statement.Split(' ').Length > 1)
                                        {
                                            parent = Statement.Split(' ')[0];
                                            if (parent == "")
                                            {
                                                parent = strOnlyFileName;
                                            }
                                            #region Insert Only Method logic
                                            var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                            if (parent.Length > 0)
                                            {
                                                if (methodProc == 0)
                                                {
                                                    var stmtReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = fileId,
                                                        ResolvedStatement = parent.Replace("//", "").Trim(),
                                                        OriginalStatement = parent.Replace("//", "").Trim(),
                                                        ClassCalled = null,
                                                        MethodName = parent.Replace("//", "").Trim() + "()",
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        BaseCommandId = methodStart.BaseCommandId,
                                                        PrimaryCommandId = methodStart.PrimaryReferenceId,
                                                        ProjectId = projectId
                                                    };

                                                    await
                                              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                }
                                                else
                                                {

                                                    var stmtReferenceMasterNew = new StatementReferenceMaster
                                                    {
                                                        FileId = fileId,
                                                        ResolvedStatement = parent.Replace("//", "").Trim(),
                                                        OriginalStatement = parent.Replace("//", "").Trim(),
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        BaseCommandId = 0,
                                                        ProjectId = projectId
                                                    };

                                                    await
                                              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterNew);
                                                }
                                                methodProc++;
                                            }

                                            #endregion


                                            var start = Statement.IndexOf(" ");
                                            child = Statement.Substring(start).TrimStart().TrimEnd();
                                            stmt = child;
                                            if (parent.StartsWith("*"))
                                            {
                                                child = "*" + child;
                                            }
                                            if (child.EndsWith(","))
                                            {
                                                child = child + " " + textLineWSProc[i + 1].Substring(2, 70).Trim();
                                                i++;
                                            }

                                            if (child.Length > 0)
                                            {

                                                #region Insert EXEC  statement logic
                                                var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                if (child.Length > 0)
                                                {
                                                    if (!child.StartsWith("*"))
                                                    {
                                                        if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = fileId,
                                                                ResolvedStatement = child,
                                                                OriginalStatement = child,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = execCicsJclStart.BaseCommandId,
                                                                PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                ProjectId = projectId
                                                            };

                                                            await
                                                  _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }
                                                    }
                                                }

                                                #endregion

                                                #region Insert END-EXEC statement logic
                                                var execCicsEnd = callExternalIndicationEnd.Find(x => x.EndIndicator != null);
                                                if (child.Length > 0)
                                                {
                                                    if (!child.StartsWith("*"))
                                                    {
                                                        if (child.StartsWith(execCicsEnd.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = fileId,
                                                                ResolvedStatement = child,
                                                                OriginalStatement = child,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = execCicsEnd.BaseCommandId,
                                                                PrimaryCommandId = execCicsEnd.PrimaryReferenceId,
                                                                ParsedOrNot = "1",
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            continue;
                                                        }
                                                    }
                                                }
                                                #endregion

                                                #region Insert If statement logic
                                                var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                                if (child.Length > 0)
                                                {
                                                    if (!child.StartsWith("*"))
                                                    {
                                                        if (child.StartsWith(ifStart.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = fileId,
                                                                ResolvedStatement = child,
                                                                OriginalStatement = child,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = ifStart.BaseCommandId,
                                                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                                ProjectId = projectId
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }

                                                    }

                                                }

                                                #endregion

                                                #region Insert End If statement logic
                                                var ifEnd = ifConditionEnd.Find(x => x.EndIndicator != null);
                                                if (child.Length > 0)
                                                {
                                                    child = child.Replace("ENDIF", "END-IF").Replace("END IF", "END-IF");
                                                    if (child.StartsWith(ifEnd.StartIndicator))
                                                    {
                                                        if (!child.StartsWith("*"))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = fileId,
                                                                ResolvedStatement = child,
                                                                OriginalStatement = child,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = ifEnd.BaseCommandId,
                                                                PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                ParsedOrNot = "1",
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            continue;

                                                        }
                                                    }
                                                }
                                                #endregion

                                                #region Insert normal statements logic

                                                if (child.Length > 0)
                                                {
                                                    if (!child.Replace("//", "").StartsWith("*"))
                                                    {
                                                        var statementReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = fileId,
                                                            ResolvedStatement = child,
                                                            OriginalStatement = child,
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
                                                }
                                                #endregion

                                            }
                                        }

                                    }
                                    else
                                    {
                                        Regex trimmer = new Regex(@"\s\s+");
                                        Statement = trimmer.Replace(Statement, " ");
                                        bool isAllSame = Statement.All(d => d == '*');
                                        if ((Statement.Length > 3) && (!isAllSame))
                                        {
                                            if (!Statement.Trim().StartsWith("*="))
                                            {
                                                if (!Statement.Trim().StartsWith("="))
                                                {
                                                    if (!Statement.Trim().StartsWith("**"))
                                                    {
                                                        Statement.Replace("'", "").Replace("\"", "").Replace("//", "");
                                                        if (Statement.Trim().EndsWith(","))
                                                        {
                                                            stmtNew = stmtNew.TrimStart().TrimEnd().ToString();
                                                            stmtFlag = 1;

                                                            continue;
                                                        }
                                                        if (stmtFlag > 0)
                                                        {
                                                            child = stmtNew.TrimStart().TrimEnd();
                                                        }
                                                        else
                                                        {
                                                            child = Statement.TrimStart().TrimEnd();
                                                        }

                                                        if (child.Length > 0)
                                                        {
                                                            #region Insert EXEC  statement logic
                                                            var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                            if (child.Length > 0)
                                                            {
                                                                if (!child.StartsWith("*"))
                                                                {

                                                                    if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                                    {
                                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                                        {
                                                                            FileId = fileId,
                                                                            ResolvedStatement = child,
                                                                            OriginalStatement = child,
                                                                            ClassCalled = null,
                                                                            MethodName = null,
                                                                            DataOrObjectType = null,
                                                                            MethodCalled = null,
                                                                            VariableNameDeclared = null,
                                                                            PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                            ProjectId = projectId
                                                                        };

                                                                        await
                                                              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                        continue;
                                                                    }
                                                                }
                                                            }

                                                            #endregion

                                                            #region Insert END-EXEC statement logic
                                                            var execCicsEnd = callExternalIndicationEnd.Find(x => x.EndIndicator != null);
                                                            if (child.Length > 0)
                                                            {
                                                                if (!child.StartsWith("*"))
                                                                {
                                                                    if (child.StartsWith(execCicsEnd.StartIndicator))
                                                                    {
                                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                                        {
                                                                            FileId = fileId,
                                                                            ResolvedStatement = child,
                                                                            OriginalStatement = child,
                                                                            ClassCalled = null,
                                                                            MethodName = null,
                                                                            DataOrObjectType = null,
                                                                            MethodCalled = null,
                                                                            VariableNameDeclared = null,
                                                                            BaseCommandId = execCicsEnd.BaseCommandId,
                                                                            PrimaryCommandId = execCicsEnd.PrimaryReferenceId,
                                                                            ParsedOrNot = "1",
                                                                            ProjectId = projectId
                                                                        };
                                                                        await
                                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                        continue;
                                                                    }
                                                                }
                                                            }
                                                            #endregion

                                                            #region Insert normal statements logic

                                                            if (child.Length > 0)
                                                            {
                                                                if (!child.Replace("//", "").StartsWith("*"))
                                                                {
                                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = fileId,
                                                                        ResolvedStatement = child.Replace("//", ""),
                                                                        OriginalStatement = child.Replace("//", ""),
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
                                                            }
                                                            #endregion

                                                        }

                                                    }
                                                }
                                            }
                                        }
                                    }

                                    stmtNew = "";
                                    stmtFlag = 0;
                                }
                            }
                            #endregion

                        }

                        #region Insert Method End logic
                        var methodEnd = methodIndicationEnd.Find(x => x.StartIndicator == null);
                        var stmtReferenceMasterEndMet = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = "END",
                            OriginalStatement = "END",
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = methodEnd.BaseCommandId,
                            PrimaryCommandId = methodEnd.PrimaryReferenceId,
                            ProjectId = projectId
                        };

                        await
                  _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterEndMet);
                        #endregion
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(ex.InnerException);
                    }
                }

                #endregion

                return Ok("Proc file parssed successfully.");
            }
        }

        public async Task<IHttpActionResult>  ParseCobolFile(int projectId, int fileId, List<FileMaster> copyOfFileMaster, string projectName, string pPath, int languageId, List<string> lstAllLines, string filePath)
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
                var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End")
                    .PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == languageId);

                List<string> textComment = lstAllLines.CollectAllComments();
                lstAllLines = lstAllLines.RemoveAllCommentedLines();
                List<string> textLineWs = lstAllLines.CollectWorkingStorageSection();
                List<string> textLineFc = lstAllLines.CollectFileControlSection();
                List<string> textLineDd = lstAllLines.CollectDataDefinationSection();
                List<string> textLineLs = lstAllLines.CollectLinkageSection();
                List<string> textLinePd = lstAllLines.CollectProcedureDivisionSection();

                var lstStatementReferenceMaster = new List<StatementReferenceMaster>();
                var cobolV1 = new CobolVersion1();
                lstStatementReferenceMaster = cobolV1.PrepareStatementReferenceMasterWorkingStorage(textLineWs, fileId, projectId);
                var linkageSectionStatement = cobolV1.PrepareStatementreferenceLinkageSection(textLineLs, fileId, projectId);
                lstStatementReferenceMaster.AddRange(linkageSectionStatement);
                await _codeVortoService.StatementReferenceMasterRepository.BulkInsert(lstStatementReferenceMaster);

                List<string> programList = textLinePd.RemoveUnnecessaryDigit();
                List<string> keywordTextLinePd = programList.LanguageKeywordStatementSplit();
                List<string> fileSortedList = keywordTextLinePd.SplitStatementCobolKeywords();
                var fileLine = fileSortedList.ToArray().IfAdjustmentCobol();
                fileSortedList = fileLine.ToList();

                List<string> mainProgramList = fileSortedList.ReplaceGoToPerformSpace();
                var copyOfFileLine = fileSortedList.ToList();
                List<string> lstAllMethod = fileSortedList.GetAllMethods();
                var actualFileLine = mainProgramList.ToArray().MethodEndLogicCobol(lstAllMethod);
                mainProgramList = actualFileLine.ToList();

                #region Insert into StatementReferenceMaster...

                var ifStart = ifConditionStart.Find(s => true);
                var ifEnd = ifConditionEnd.FindAll(s => true);
                var loopStart = loopIndicatorStart.Find(l => true);
                var loopEnd = loopIndicatorEnd.Find(l => true);
                var callInternal = callInternalIndicationStart.Find(c => true);
                var callExternal = callExternalIndicationStart.Find(c => true);
                var indexPosition = -1;
                string methodBusinessName = string.Empty;

                foreach (var line in mainProgramList)
                {
                    indexPosition = indexPosition + 1;

                    if (line == "END")
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
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
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (lstAllMethod.Any(a => line.StartsWith(a)))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            //ResolvedStatement = line.Replace(" EXIT.","").Replace(" SECTION.","").Replace(".", ""),
                            //OriginalStatement = line.Replace(" EXIT.", "").Replace(" SECTION.", "").Replace(".", ""),
                            //ClassCalled = null,
                            //MethodName = line.Replace(" EXIT.", "").Replace(" SECTION.", "").Replace(".", "") + "()",
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = line + "()",
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 8,
                            PrimaryCommandId = 23,
                            ParsedOrNot = "Y",
                            ProjectId = projectId,
                            BusinessName = methodBusinessName
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        methodBusinessName = string.Empty;
                        continue;
                    }

                    if (line.StartsWith(ifStart.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = ifStart.BaseCommandId,
                            PrimaryCommandId = ifStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }

                    if (line == "ELSE")
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 10,
                            PrimaryCommandId = 0,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }

                    if (ifEnd.Any(l => line == l.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = 2,
                            PrimaryCommandId = ifEnd.Find(s => line.StartsWith(s.StartIndicator)).PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
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
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = loopStart.BaseCommandId,
                            PrimaryCommandId = loopStart.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (line.StartsWith(loopEnd.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = loopEnd.BaseCommandId,
                            PrimaryCommandId = loopEnd.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (line.StartsWith(callInternal.StartIndicator))
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = line.Replace("PERFORM", "").Trim().Split(' ').FirstOrDefault() + "()",
                            VariableNameDeclared = null,
                            BaseCommandId = callInternal.BaseCommandId,
                            PrimaryCommandId = callInternal.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                        continue;
                    }
                    if (line.StartsWith(callExternal.StartIndicator))
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
                            FileId = fileId,
                            ResolvedStatement = line,
                            OriginalStatement = line,
                            ClassCalled = null,
                            MethodName = null,
                            DataOrObjectType = null,
                            MethodCalled = null,
                            VariableNameDeclared = null,
                            BaseCommandId = callExternal.BaseCommandId,
                            PrimaryCommandId = callExternal.PrimaryReferenceId,
                            ParsedOrNot = "Y",
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                    else
                    {
                        var stmtReferenceMaster = new StatementReferenceMaster
                        {
                            FileId = fileId,
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
                            ProjectId = projectId
                        };
                        await _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                    }
                }

                #endregion
                int ifCount = mainProgramList.Count(s => s.StartsWith("IF"));
                int endIfCount = mainProgramList.Count(s => s.EndsWith("END-IF") || s.EndsWith("END-IF."));

                mainProgramList.Add("==========================");
                mainProgramList.Add("IF Counts: " + ifCount);
                mainProgramList.Add("END Count: " + endIfCount);
            }

            return Ok("Cobol parsing success");
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

        public string CalculateWidth(int textLength)
        {
            int widthCalculation = (textLength * 2) + 20;
            if (widthCalculation > 200 || textLength > 100)
            {
            }
            else
            {
                widthCalculation = widthCalculation * 1;
            }

            var width = widthCalculation.ToString();
            return width;
        }

        public string CalculateHeight(int charCountOfText)
        {
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
            string height = heightCalculation.ToString();
            return height;
        }

    }
}
