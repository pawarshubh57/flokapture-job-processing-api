using System;
using System.Collections.Generic;
using System.Data.Entity;
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
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class UniverseBasicController
    {
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
                var lstFileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(p => p.ProjectId == projectDetails.ProjectId &&
                                          p.SolutionId == projectDetails.SolutionId).ConfigureAwait(false);
                var copyOfFileMaster = lstFileMaster
                    .Where(f => f.FileTypeExtensionId == 10
                                && f.ProjectId == projectId
                                && f.Processed == 1).ToList();
                stringBuilder.AppendLine("===========================================================================");
                stringBuilder.AppendLine("Started executing next process: ParseUniverseBasicFiles for project: " +
                                         projectId + "");

                foreach (var fileMaster in copyOfFileMaster)
                {
                    await ParseUniverseBasicFiles(fileMaster).ConfigureAwait(false); // Done
                }

                await ParseUniverseBasicPrograms(projectDetails).ConfigureAwait(false);

                /*
                var copyOfPgmAndSbr = lstFileMaster.Where(f => (f.FileTypeExtensionId == 9 || f.FileTypeExtensionId == 17)
                                 && f.ProjectId == projectId && (f.Processed == 1 || f.Processed == 4)).ToList();
                
                foreach (var fileMaster in copyOfPgmAndSbr)
                {
                    await UpdateBaseCommandId30(fileMaster).ConfigureAwait(false); // Done
                }
                */

                #region Update field for basecommandId = 45

                stringBuilder.AppendLine("=============================================================================");
                stringBuilder.AppendLine("Started update field for basecommandId = 45 for solution: " + solutionId + "");

                // await UpdateToBaseCommandId45(projectDetails).ConfigureAwait(false); // Done

                stringBuilder.AppendLine("==========================================================================");
                stringBuilder.AppendLine("Ended update field for basecommandId = 45 for solution: " + solutionId + "");

                #endregion

                stringBuilder.AppendLine("=============================================================================");
                stringBuilder.AppendLine("Started executing: StartWorkflowProcess for solution: " + solutionId + "");
                LogMessage.WriteLogMessage(stringBuilder);

                return Ok("Parsing process completed successfully for Project: " + projectDetails.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseUniverseBasicFiles(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Update Method called for base command id = 6 in Jcl and program...
                var execSql = " SELECT sm.* from StatementReferenceMaster AS sm " +
                " INNER JOIN FileMaster AS fm ON fm.FileId = sm.FileId Where sm.FileId = " + fileMaster.FileId +
                " AND sm.BaseCommandId = 6 AND sm.ReferenceFileId != 0 AND sm.ClassCalled IS NOT NULL;";
                var execName = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(execSql).ConfigureAwait(false);

                foreach (var constructor in execName)
                {
                    var referenceFile = constructor.ReferenceFileMaster;
                    if (constructor.ReferenceFileMaster == null) continue;
                    if (referenceFile.FileTypeExtensionId != 9) continue;
                    var methodSql = " SELECT DISTINCT sm.* FROM StatementReferenceMaster as sm " +
                                  " INNER JOIN FileMaster as fm where sm.FileId = " + referenceFile.FileId +
                                  " AND fm.SolutionId = " + referenceFile.SolutionId + " AND sm.BaseCommandId = 8;";

                    var methodName = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(methodSql).ConfigureAwait(false);

                    foreach (var statementReference in methodName)
                    {
                        if (string.IsNullOrEmpty(statementReference.MethodName)) continue;
                        constructor.MethodCalled = statementReference.MethodName;
                        constructor.FileMaster = null;
                        constructor.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor)
                            .ConfigureAwait(false);
                        break;
                    }
                }

                #endregion
                return Ok("File processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseUniverseBasicPrograms(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Update ClassCalled field for BaseCommandId = 5

                try
                {
                    var execSqlNew = "SELECT * FROM StatementReferenceMaster WHERE SolutionId = " +
                                     projectMaster.SolutionId + " AND ProjectId = " + projectMaster.ProjectId +
                                     " AND BaseCommandId = 5 AND ClassCalled IS NULL;";
                    var execNameNew = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNew).ConfigureAwait(false);

                    var mysqlQuery = " SELECT * FROM  StatementReferenceMaster WHERE FileId IN " +
                                     "( SELECT FileId FROM FileMaster WHERE FileTypeExtensionId = 12 ) " +
                                     " AND BaseCommandId IN (8, 19) AND ProjectId = " +
                                     projectMaster.ProjectId + " AND SolutionId = " + projectMaster.SolutionId + "; ";

                    var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(mysqlQuery).ConfigureAwait(false);
                    foreach (var constrctor in execNameNew)
                    {
                        if (allStatements.All(s => s.MethodName != constrctor.MethodCalled)) continue;

                        var fileId = allStatements.Find(s => s.MethodName == constrctor.MethodCalled).FileId;
                        var classCalled = allStatements.Single(d => d.FileId == fileId
                        && d.BaseCommandId == 19).ClassNameDeclared;
                        constrctor.ClassCalled = classCalled;
                        constrctor.FileMaster = null;
                        constrctor.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constrctor).ConfigureAwait(false);
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    Console.WriteLine("==================================");
                }

                #endregion

                return Ok("ParseUniverseBasicPrograms Method completed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBaseCommandId30(FileMaster fileMaster)
        {
            #region Update field for basecommandId = 30

            using (_codeVortoService = new CodeVortoService())
            {
                var execSqlPrgm = " SELECT * FROM StatementReferenceMaster WHERE FileId = " + fileMaster.FileId +
                                  " AND ProjectId = " + fileMaster.ProjectId + " AND BaseCommandId IN (0, 1);";
                var execNamePrgm = await _codeVortoService.StatementReferenceMasterRepository
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

                    if (indexPositionPrgm <= 0 || execNamePrgm.Count < indexPositionPrgm - 1) continue;

                    try
                    {
                        var aboveLine = execNamePrgm[indexPositionPrgm - 1];
                        aboveLine.BaseCommandId = 30;
                        aboveLine.FileMaster = null;
                        aboveLine.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(aboveLine)
                            .ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine(exception.Message);
                        Console.WriteLine(exNmPg.StatementId);
                        return InternalServerError(exception);
                    }
                }
            }

            return Ok("Update to BaseCommandId = 30 process completed successfully " +
                      "for Project: " + fileMaster.ProjectMaster.ProjectName);

            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessAllActionWorkflows(int projectId, FileMaster fileMaster)
        {
            #region Added the Action Workflow Table...

            if (string.IsNullOrEmpty(fileMaster.FileName) || fileMaster.FileId == 0)
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
                object[] param =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileMaster.FileId}
                };

                var genericBlocks = await statementReferenceRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);
                foreach (var statement in genericBlocks)
                {
                    if (allProcessedWorkflows.Any(s => statement.StatementId == s.MethodStatementId)) continue;

                    #region Update Class starting points with Menu names

                    using (var appDbContext = new AppDbContext())
                    {
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
                            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
                            if (nameList != fileName) continue;
                            string mainName = jclMenu.MenuDescription.Trim().TrimStart('-').Trim();
                            mainName = jclMenu.MenuTitle + " - " + mainName;
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Batch",
                                MethodStatementId = statement.StatementId,
                                OriginFileName = Path.GetFileName(fileMaster.FilePath),
                                OriginFilePath = fileMaster.FilePath,
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
                    }

                    #endregion
                }
            }

            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }

            return Ok("Action Workflows processed successfully");
            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertActionWorkflowsFromUniverseMenuFile(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                using (var appDbContext = new AppDbContext())
                {
                    var allJclFiles = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.FileTypeExtensionId == 10 && f.ProjectId == projectId).ConfigureAwait(false);
                    var universeFileMenus = await appDbContext.UniverseFileMenu.AsQueryable()
                        .Where(u => u.ProjectId == projectId).ToListAsync().ConfigureAwait(false);
                    foreach (var universeFileMenu in universeFileMenus)
                    {
                        int fileMenuId = universeFileMenu.FileMenuId;
                        if (string.IsNullOrEmpty(universeFileMenu.MenuDescription))
                            universeFileMenu.MenuDescription = "-";
                        string menuDesc = universeFileMenu.MenuDescription.Trim().TrimStart('-').Trim();
                        string workflowName = universeFileMenu.MenuTitle + " - " + menuDesc;
                        string actionExecutedJcl = !string.IsNullOrEmpty(universeFileMenu.ActionExecuted)
                            ? universeFileMenu.ActionExecuted.Trim()
                            : "";
                        // if (string.IsNullOrWhiteSpace(actionExecutedJcl)) continue;
                        if (string.IsNullOrEmpty(actionExecutedJcl)) actionExecutedJcl = "";
                        string jclMenuFile = actionExecutedJcl.Split(' ').First();

                        // Now find the JCL file associated with this menu...
                        var jclFileMaster = (from j in allJclFiles
                                             let fileName = Path.GetFileNameWithoutExtension(j.FilePath)
                                             where fileName == jclMenuFile
                                             select j).ToList();
                        if (jclFileMaster.Any())
                        {
                            var fileMaster = jclFileMaster.First();
                            object[] param =
                            {
                                new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                                new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileMaster.FileId}
                            };
                            var genericBlocks = await _codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);
                            foreach (var statement in genericBlocks)
                            {
                                string wBusinessName = statement.BusinessName;
                                if (string.IsNullOrEmpty(wBusinessName)) wBusinessName = "";
                                string wbName = wBusinessName.Replace("PA ", "").Trim().TrimStart('-').Trim();
                                var actionWorkflow = new ActionWorkflows
                                {
                                    ActionWorkflowId = 0,
                                    CreatedBy = 1,
                                    EndPointOrService = "Batch",
                                    MethodStatementId = statement.StatementId,
                                    OriginFileName = Path.GetFileName(fileMaster.FilePath),
                                    OriginFilePath = fileMaster.FilePath,
                                    ProjectId = projectId,
                                    OriginEventMethod = universeFileMenu.MenuTitle,
                                    OriginObject = workflowName,
                                    WorkflowName = jclMenuFile,
                                    ServiceBaseAddress = null,
                                    ServiceContract = null,
                                    WorkflowBusinessName = wbName,
                                    IsDeleted = 0,
                                    ReasonAboutDisable = null,
                                    Processed = 0,
                                    FileId = statement.FileId,
                                    FileMenuId = fileMenuId
                                };
                                await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow).ConfigureAwait(false);
                            }
                        }
                        else
                        {
                            var actionWorkflow = new ActionWorkflows
                            {
                                ActionWorkflowId = 0,
                                CreatedBy = 1,
                                EndPointOrService = "Batch",
                                MethodStatementId = 0,
                                OriginFileName = null,
                                OriginFilePath = null,
                                ProjectId = projectId,
                                OriginEventMethod = universeFileMenu.MenuTitle,
                                OriginObject = workflowName,
                                WorkflowName = string.IsNullOrEmpty(jclMenuFile) ? workflowName : jclMenuFile,
                                ServiceBaseAddress = null,
                                ServiceContract = null,
                                WorkflowBusinessName = null,
                                IsDeleted = 0,
                                ReasonAboutDisable = null,
                                Processed = 0,
                                FileId = 0,
                                FileMenuId = fileMenuId
                            };
                            await _codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow).ConfigureAwait(false);
                        }
                    }
                }
                return Ok("Action Workflows has been added successfully.");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateToBaseCommandId45(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var sqlQuery = " Select FileName, ProjectId, RowId, FieldNo, Description, FieldLabel, " +
                               " RptFieldLength, TypeofData, SingleArray, DateOfCapture, ReplacementName " +
                               " FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectMaster.ProjectId +
                               " GROUP BY FileName;";
                var universeBasicDataDicyionary =
                    await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);
                foreach (var dataDictionary in universeBasicDataDicyionary)
                {
                    string tableName = dataDictionary.Description;
                    string fileName = dataDictionary.FileName;
                    if (string.IsNullOrEmpty(fileName)) continue;
                    string sqlQry = " SELECT * FROM StatementReferenceMaster WHERE " +
                                    " SolutionId = " + projectMaster.SolutionId + " AND " +
                                    " ProjectId = " + projectMaster.ProjectId + " AND " +
                                    " OriginalStatement LIKE '% " + fileName + "%';";
                    var statementRefMaster = await _codeVortoService.StatementReferenceMasterRepository
                           .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                    foreach (var statementRef in statementRefMaster)
                    {
                        var newStatement = Regex.Replace(!string.IsNullOrEmpty(statementRef.AlternateName)
                            ? statementRef.AlternateName :
                            statementRef.OriginalStatement, fileName, tableName, RegexOptions.IgnoreCase);
                        newStatement = newStatement.Length > 651 ? newStatement.Substring(0, 650) : newStatement;

                        if (statementRef.BaseCommandId == 0)
                        {
                            statementRef.BaseCommandId = 45;
                            statementRef.AlternateName = newStatement;
                            statementRef.BusinessName = newStatement;
                            statementRef.ResolvedStatement = newStatement;
                        }
                        else
                        {
                            statementRef.AlternateName = newStatement;
                            statementRef.BusinessName = newStatement;
                            statementRef.ResolvedStatement = newStatement;
                        }

                        statementRef.FileMaster = null;
                        statementRef.ReferenceFileMaster = null;
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef);
                    }
                }

                return Ok("Update to BaseCommandId = 45 process completed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateBusinessNameTo651Chars(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var sqlQry = " SELECT * FROM UniverseBasicDataDictionary WHERE " +
                                 " ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName;";
                    var dataDictionaryWithFileName = await _codeVortoService.DataDictionaryRepository
                        .GetDataFromSqlQuery<DataDictionary>(sqlQry).ConfigureAwait(false);

                    /*
                    string sqlQuery = "SELECT * FROM UniverseBasicDataDictionary WHERE " +
                                      " ProjectId = " + projectMaster.ProjectId + ";";
                    var universeBasicDataDictionary = await _codeVortoService.DataDictionaryRepository
                        .GetDataFromSqlQuery<DataDictionary>(sqlQuery);
                    */

                    foreach (var dataDictionary in dataDictionaryWithFileName)
                    {
                        string fileName = dataDictionary.FileName;
                        /*
                        var recordRegEx = new Regex(@"R." + fileName + "|K." + fileName + @"|[\'\(\)\""\*\:\;\,\s+]" + fileName,
                            RegexOptions.IgnoreCase);
                        var attribute = new List<string>();
                        var dataAttributeList = (from d in universeBasicDataDictionary
                                                 where d.FileName == dataDictionary.FileName
                                                 select d).ToList();
                        int index = -1;
                        foreach (var attList in dataAttributeList)
                        {
                            index++;
                            if (index == 0) continue;
                            string attr = attList.Description;
                            if (!string.IsNullOrEmpty(attr)) attribute.Add(attr);
                        }
                        */
                        string sqlStmtMaster = " SELECT * FROM StatementReferenceMaster " +
                                               " WHERE ProjectId = " + projectMaster.ProjectId +
                                               " AND OriginalStatement LIKE '%" + fileName + "%';";

                        var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlStmtMaster).ConfigureAwait(false);
                        /*
                        var omitListForJcl = new List<string> { "PA ", "*", "PH ", "PHANTOM ", "DISPLAY " };
                        foreach (var statementRef in statementReferenceMaster)
                        {
                            string originalStatement = statementRef.OriginalStatement;
                            if (omitListForJcl.Any(originalStatement.StartsWith) &&
                                statementRef.FileMaster.FileTypeExtensionId == 10) continue;
                            if (!recordRegEx.IsMatch(originalStatement)) continue;
                            int fileId = statementRef.FileId;
                            var dataDependancy = new DataDependency
                            {
                                FileId = statementRef.FileId,
                                Entity = dataDictionary.FileName,
                                Attributes = string.Join(", ", attribute),
                                ProjectId = statementRef.ProjectId,
                                EntityOld = dataDictionary.FileName,
                                FileIdOld = statementRef.FileId
                            };

                            var dataDepen = await _codeVortoService.DataDependencyRepository
                                .GetAllListItems(x => x.FileId == fileId && x.Entity == fileName &&
                                                      x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);
                            if (dataDepen.Any()) continue;

                            await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependancy);
                        }
                        */
                        foreach (var statementRef in statementReferenceMaster)
                        {
                            if (statementRef.BaseCommandId != 0) continue;
                            statementRef.BaseCommandId = 45;
                            string tableName = dataDictionary.Description;
                            var newStatement = Regex.Replace(!string.IsNullOrEmpty(statementRef.AlternateName)
                            ? statementRef.AlternateName :
                            statementRef.OriginalStatement, fileName, tableName, RegexOptions.IgnoreCase);
                            newStatement = newStatement.Length > 651 ? newStatement.Substring(0, 650) : newStatement;
                            statementRef.AlternateName = newStatement;
                            statementRef.BusinessName = newStatement;
                            statementRef.ResolvedStatement = newStatement;
                            statementRef.FileMaster = null;
                            statementRef.ReferenceFileMaster = null;

                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef)
                                .ConfigureAwait(false);
                        }
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }

                return Ok("Insert Data-Dependancy process completed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertCrudActivityForUniverse(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var insertList = new List<string> { "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var updateList = new List<string>{ "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var deleteList = new List<string> { "DELETE ", "DELETEU " };
                var selectList = new List<string>
                {
                    "MATREAD ", "MATREADL ", "MATREADU ", "OPENCHECK ", "OPENDEV ", "OPENPATH ", "OPENSEQ ", "OPEN ",
                    "READ ", "READL ", "READU ", "READV ", "REAFV ", "READSEQ ", "READVL ", "READVU ",
                    "READNEXT ", "READT ", "SSELECT ", "SELECT "
                };

                var mainList = new List<string>();
                mainList.AddRange(insertList);
                mainList.AddRange(updateList);
                mainList.AddRange(deleteList);
                mainList.AddRange(selectList);

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var allEntities = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId == 11)
                    .ConfigureAwait(false);

                var insertOrWritePartsList = new List<string> { "TO", "ON" };
                var readOrSelectPartsList = new List<string> { "FROM", "TO", "WITH" };
                foreach (var fileMaster in fileMasters)
                {
                    var linesWithCrud = new List<string>();
                    var crudWithEntityDictionary = new Dictionary<string, List<string>>();

                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    var fileLines = contentsWithoutComment.Where(fileLine => mainList.Any(fileLine.StartsWith)).ToList();
                    /* // This is to check for specific statement types.
                    var fileLines = contentsWithoutComment.Where(fileLine => fileLine.StartsWith("SSELECT ")
                                                          || fileLine.StartsWith("SELECT ")).ToList();
                    */
                    var rawCrudLinesList = fileLines.Select(fileLine => fileLine.Replace(",", ", ")).ToList();
                    var crudLinesList = rawCrudLinesList.RemoveSpacesBetweenWords();

                    foreach (var entityMaster in allEntities)
                    {
                        string entityNm = Path.GetFileNameWithoutExtension(entityMaster.FilePath);
                        if (string.IsNullOrEmpty(entityNm)) continue;
                        var linesWithEntity = crudLinesList.Where(line => line.Contains(entityNm)).ToList();

                        if (!linesWithEntity.Any()) continue;
                        linesWithCrud.AddRange(linesWithEntity);

                        bool isPresent = crudWithEntityDictionary.ContainsKey(entityNm);
                        if (!isPresent) { crudWithEntityDictionary.Add(entityNm, linesWithEntity); continue; }
                        foreach (var crudDictionary in crudWithEntityDictionary)
                        {
                            if (crudDictionary.Key != entityNm) continue;
                            crudDictionary.Value.AddRange(linesWithEntity);
                        }
                    }

                    foreach (var crudDictionary in crudWithEntityDictionary)
                    {
                        string keyEntityName = crudDictionary.Key;
                        var crudLines = crudDictionary.Value;
                        foreach (var crudLine in crudLines)
                        {
                            if (insertList.Any(crudLine.StartsWith) || updateList.Any(crudLine.StartsWith))
                            {
                                if (!insertOrWritePartsList.Any(crudLine.Contains)) continue;
                                var sParts = crudLine.Split(' ').ToList();

                                int indexPosition = -1;
                                foreach (var part in sParts)
                                {
                                    indexPosition++;
                                    if (insertOrWritePartsList.All(p => p != part)) continue;
                                    if (sParts.Count < indexPosition + 1) break;
                                    string entityName = sParts[indexPosition + 1];
                                    string eName = entityName.Trim().Replace(",", "");
                                    if (string.IsNullOrEmpty(eName)) continue;

                                    var newCrudActivity = new DbCrudActivity
                                    {
                                        EntityName = keyEntityName,
                                        ReferenceObjectName = eName,
                                        Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        FileId = fileMaster.FileId,
                                        FileMaster = null,
                                        ProjectId = projectMaster.ProjectId,
                                        HasIndicator = false,
                                        OpenStatement = crudLine,
                                        IsOpen = false
                                    };

                                    using (var appDbContext = new AppDbContext())
                                    {
                                        appDbContext.DbCrudActivity.Add(newCrudActivity);
                                        await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                    }
                                    break;
                                }
                                continue;
                            }
                            if (selectList.Any(crudLine.StartsWith))
                            {
                                if (!readOrSelectPartsList.Any(crudLine.Contains)) continue;
                                var selectParts = crudLine.Split(' ').ToList();

                                int indexPosition = -1;
                                foreach (var selectPart in selectParts)
                                {
                                    indexPosition++;
                                    if (readOrSelectPartsList.All(p => p != selectPart)) continue;
                                    if (selectParts.Count < indexPosition + 1) break;
                                    string entityName = selectParts[indexPosition + 1];
                                    string eName = entityName.Trim().Replace(",", "");
                                    if (string.IsNullOrEmpty(eName)) continue;

                                    var newCrudActivity = new DbCrudActivity
                                    {
                                        EntityName = keyEntityName,
                                        ReferenceObjectName = eName,
                                        Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        FileId = fileMaster.FileId,
                                        FileMaster = null,
                                        ProjectId = projectMaster.ProjectId,
                                        HasIndicator = false,
                                        OpenStatement = crudLine,
                                        IsOpen = false
                                    };

                                    using (var appDbContext = new AppDbContext())
                                    {
                                        appDbContext.DbCrudActivity.Add(newCrudActivity);
                                        await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                    }
                                    break;
                                }
                                continue;
                            }
                            if (crudLine.StartsWith("DELETE "))
                            {
                                var deleteRegEx = new Regex(@"DELETE\s+([\w\d\.]+)", RegexOptions.IgnoreCase);
                                var matchGroups = deleteRegEx.Match(crudLine).Groups;
                                string objName = matchGroups[1].Value;
                                if (string.IsNullOrEmpty(objName)) continue;

                                var existingFiles = await _codeVortoService.FileMasterRepository
                                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                                          f.SolutionId == projectMaster.SolutionId &&
                                                          f.FileName.StartsWith(objName)).ConfigureAwait(false);
                                var actualFiles = (from f in existingFiles
                                                   let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                                   where fName == objName
                                                   select f).ToList();
                                var hasIndicator = !actualFiles.Any();
                                var newCrudActivity = new DbCrudActivity
                                {
                                    ReferenceObjectName = objName,
                                    EntityName = keyEntityName,
                                    Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    FileId = fileMaster.FileId,
                                    FileMaster = null,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = hasIndicator,
                                    OpenStatement = crudLine,
                                    IsOpen = false
                                };
                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                continue;
                            }
                            var crudActivity = new DbCrudActivity
                            {
                                ReferenceObjectName = "",
                                EntityName = "-",
                                Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                FileId = fileMaster.FileId,
                                FileMaster = null,
                                ProjectId = projectMaster.ProjectId,
                                HasIndicator = true,
                                OpenStatement = crudLine,
                                IsOpen = false
                            };
                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.DbCrudActivity.Add(crudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                        }
                    }

                    // Lines with entity name should be removed from other lines in which 
                    // entity name is not present and has reference object name.

                    foreach (var crudLine in linesWithCrud)
                    {
                        crudLinesList.Remove(crudLine);
                    }

                    foreach (var crudLine in crudLinesList)
                    {
                        if (insertList.Any(crudLine.StartsWith) || updateList.Any(crudLine.StartsWith))
                        {
                            if (!insertOrWritePartsList.Any(crudLine.Contains)) continue;
                            var sParts = crudLine.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (insertOrWritePartsList.All(p => p != part)) continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                string eName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(eName)) continue;

                                var newCrudActivity = new DbCrudActivity
                                {
                                    EntityName = "-",
                                    ReferenceObjectName = eName,
                                    Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    FileId = fileMaster.FileId,
                                    FileMaster = null,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false,
                                    OpenStatement = crudLine,
                                    IsOpen = false
                                };

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                break;
                            }
                            continue;
                        }
                        if (selectList.Any(crudLine.StartsWith))
                        {
                            if (!readOrSelectPartsList.Any(crudLine.Contains)) continue;
                            var sParts = crudLine.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (readOrSelectPartsList.All(p => p != part)) continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                string eName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(eName)) continue;

                                var newCrudActivity = new DbCrudActivity
                                {
                                    EntityName = "-",
                                    ReferenceObjectName = eName,
                                    Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    FileId = fileMaster.FileId,
                                    FileMaster = null,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = false,
                                    OpenStatement = crudLine,
                                    IsOpen = false
                                };

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                break;
                            }
                            continue;
                        }
                        if (crudLine.StartsWith("DELETE "))
                        {
                            var deleteRegEx = new Regex(@"DELETE\s+([\w\d\.]+)", RegexOptions.IgnoreCase);
                            var matchGroups = deleteRegEx.Match(crudLine).Groups;
                            string objName = matchGroups[1].Value;
                            if (string.IsNullOrEmpty(objName)) continue;

                            var existingFiles = await _codeVortoService.FileMasterRepository
                                .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                                      f.SolutionId == projectMaster.SolutionId &&
                                                      // ReSharper disable once AccessToModifiedClosure
                                                      f.FileName.StartsWith(objName)).ConfigureAwait(false);
                            var actualFiles = (from f in existingFiles
                                               let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                               where fName == objName
                                               select f).ToList();
                            var hasIndicator = !actualFiles.Any();
                            var newCrudActivity = new DbCrudActivity
                            {
                                ReferenceObjectName = objName,
                                EntityName = "-",
                                Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                FileId = fileMaster.FileId,
                                FileMaster = null,
                                ProjectId = projectMaster.ProjectId,
                                HasIndicator = hasIndicator,
                                OpenStatement = crudLine,
                                IsOpen = false
                            };
                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.DbCrudActivity.Add(newCrudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                            continue;
                        }
                        var crudActivity = new DbCrudActivity
                        {
                            ReferenceObjectName = "",
                            EntityName = "-",
                            Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                            InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                            Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                            SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                            FileId = fileMaster.FileId,
                            FileMaster = null,
                            ProjectId = projectMaster.ProjectId,
                            HasIndicator = true,
                            OpenStatement = crudLine,
                            IsOpen = false
                        };
                        using (var appDbContext = new AppDbContext())
                        {
                            appDbContext.DbCrudActivity.Add(crudActivity);
                            await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                        }
                    }
                }

                // Pick up all records where entity name = "-" and place all those entries into missing objects report.
                // In this case reference object name will be missing entity name.

                var allMissingEntities = await _codeVortoService.DbCrudActivityRepository
                    .GetAllListItems(d => d.EntityName == "-" && d.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);

                foreach (var missingEntity in allMissingEntities)
                {
                    string refObjName = missingEntity.ReferenceObjectName;

                    var missingObject = new MissingObjects
                    {
                        ProjectId = projectMaster.ProjectId,
                        FileId = missingEntity.FileId,
                        FileMaster = null,
                        FromObject = missingEntity.FileMaster.FileName,
                        Statement = missingEntity.OpenStatement,
                        CalledObjectName = refObjName,
                        Type = "Entity",
                        MissingObjectId = 0
                    };

                    await _codeVortoService.MissingObjectsRepository.AddNewItem(missingObject).ConfigureAwait(false);
                }

                return Ok("Insert CRUD-Activity process completed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessCrudActivity(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest();

                var insertList = new List<string> { "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var updateList = new List<string>{ "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var deleteList = new List<string> { "DELETE ", "DELETEU " };
                var selectList = new List<string>
                {
                    "MATREAD ", "MATREADL ", "MATREADU ", "OPENCHECK ", "OPENDEV ", "OPENPATH ", "OPENSEQ ", "OPEN ",
                    "READ ", "READL ", "READU ", "READV ", "REAFV ", "READSEQ ", "READVL ", "READVU ",
                    /* "READNEXT ",*/ "READT ", "SSELECT ", "SELECT "
                };

                var mainList = new List<string>();
                mainList.AddRange(insertList);
                mainList.AddRange(updateList);
                mainList.AddRange(deleteList);
                mainList.AddRange(selectList);

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var entityFileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId == 11)
                    .ConfigureAwait(false);

                var entityMasters = (from en in entityFileMasters
                                     let eName = Path.GetFileNameWithoutExtension(en.FilePath)
                                     select eName).ToList();

                var insertOrWritePartsList = new List<string> { "TO", "ON" };
                var readOrSelectPartsList = new List<string> { "FROM", "TO", "WITH" };
                foreach (var fileMaster in fileMasters)
                {
                    // if(fileMaster.FileId != 3420) continue;
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    var fileLines = contentsWithoutComment.Where(fileLine => mainList.Any(fileLine.StartsWith)).ToList();
                    /* // This is to check for specific statement types.
                    var fileLines = contentsWithoutComment.Where(fileLine => fileLine.StartsWith("SSELECT ")
                                                          || fileLine.StartsWith("SELECT ")).ToList();
                    */
                    var rawCrudLinesList = fileLines.Select(fileLine => fileLine.Replace(",", ", ")).ToList();
                    var crudLinesList = rawCrudLinesList.RemoveSpacesBetweenWords();

                    var allCrudReferences = new List<CrudObjectReferences>();
                    var crudReferences = await _codeVortoService.CrudObjectReferenceRepository
                        .GetAllListItems(c => c.ObjectFileId == fileMaster.FileId).ConfigureAwait(false);
                    allCrudReferences.AddRange(crudReferences);
                    var addedReferences = new List<int?>();
                    foreach (var crudReference in crudReferences)
                    {
                        if (crudReference.UsedByFileId == null) continue;
                        if (addedReferences.Any(a => a == crudReference.UsedByFileId)) continue;
                        addedReferences.Add(crudReference.UsedByFileId);
                        var remainingCrudRef = await _codeVortoService.CrudObjectReferenceRepository
                            .GetAllListItems(c => c.ObjectFileId == crudReference.UsedByFileId).ConfigureAwait(false);
                        allCrudReferences.AddRange(remainingCrudRef);
                    }

                    var usedByFileIdRecords = allCrudReferences.ToList();
                    addedReferences = new List<int?>();
                    foreach (var crudReference in usedByFileIdRecords)
                    {
                        if (crudReference.UsedByFileId == null) continue;
                        if (addedReferences.Any(a => a == crudReference.UsedByFileId)) continue;
                        addedReferences.Add(crudReference.UsedByFileId);
                        var remainingCrudRef = await _codeVortoService.CrudObjectReferenceRepository
                            .GetAllListItems(c => c.ObjectFileId == crudReference.UsedByFileId).ConfigureAwait(false);
                        allCrudReferences.AddRange(remainingCrudRef);
                    }

                    foreach (var crudLine in crudLinesList)
                    {
                        if (insertList.Any(crudLine.StartsWith) || updateList.Any(crudLine.StartsWith))
                        {
                            if (!insertOrWritePartsList.Any(crudLine.Contains)) continue;

                            var sParts = crudLine.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (insertOrWritePartsList.All(p => p != part)) continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                string eName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(eName)) continue;
                                var actualFiles =
                                    (from c in allCrudReferences where c.LogicalEntityName == eName select c).ToList();
                                string actualEntityName = actualFiles.Any() ? actualFiles.First().PhysicalEntityName : "-";

                                var entityActualFile = (from e in entityMasters where e == eName select e).ToList();
                                if (actualEntityName == "-" && entityActualFile.Any())
                                    actualEntityName = entityActualFile.First();

                                var hasIndicator = actualFiles.Any(e => e.EntityFileId != null) || entityActualFile.Any();
                                var newCrudActivity = new DbCrudActivity
                                {
                                    EntityName = actualEntityName,
                                    ReferenceObjectName = eName,
                                    Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    FileId = fileMaster.FileId,
                                    FileMaster = null,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = !hasIndicator,
                                    OpenStatement = crudLine,
                                    IsOpen = false
                                };

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                break;
                            }
                            continue;
                        }
                        if (selectList.Any(crudLine.StartsWith))
                        {
                            if (crudLine.StartsWith("SELECT ") || crudLine.StartsWith("SSELECT "))
                            {
                                var currentLinePart = crudLine.Split(' ').ToList();
                                if (currentLinePart.Any() && currentLinePart.Count >= 2)
                                {
                                    var sStamt = currentLinePart[1].Trim();
                                    if (string.IsNullOrEmpty(sStamt)) continue;
                                    var actualFiles =
                                        (from c in allCrudReferences where c.LogicalEntityName == sStamt select c).ToList();
                                    string actualEntityName = actualFiles.Any() ? actualFiles.First().PhysicalEntityName : "-";
                                    var entityActualFile = (from e in entityMasters where e == sStamt select e).ToList();
                                    if (actualEntityName == "-" && entityActualFile.Any())
                                        actualEntityName = entityActualFile.First();
                                    var hasIndicator = actualFiles.Any(e => e.EntityFileId != null) || entityActualFile.Any();
                                    var newCrudActivity = new DbCrudActivity
                                    {
                                        EntityName = actualEntityName,
                                        ReferenceObjectName = sStamt,
                                        Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        FileId = fileMaster.FileId,
                                        FileMaster = null,
                                        ProjectId = projectMaster.ProjectId,
                                        HasIndicator = !hasIndicator,
                                        OpenStatement = crudLine,
                                        IsOpen = false
                                    };

                                    using (var appDbContext = new AppDbContext())
                                    {
                                        appDbContext.DbCrudActivity.Add(newCrudActivity);
                                        await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                    }
                                    // break;
                                }
                                continue;
                            }
                            if (!readOrSelectPartsList.Any(crudLine.Contains))
                            {
                                var crudLinePart = crudLine.Split(' ').ToList();
                                var upTo2 = -1;
                                foreach (var onlyRef in crudLinePart)
                                {
                                    upTo2++;
                                    if (upTo2 != 1) continue;
                                    string eName = onlyRef.Trim().Replace(",", "");
                                    if (string.IsNullOrEmpty(eName)) continue;
                                    var actualFiles =
                                        (from c in allCrudReferences where c.LogicalEntityName == eName select c).ToList();
                                    string actualEntityName = actualFiles.Any() ? actualFiles.First().PhysicalEntityName : "-";
                                    var entityActualFile = (from e in entityMasters where e == eName select e).ToList();
                                    if (actualEntityName == "-" && entityActualFile.Any())
                                        actualEntityName = entityActualFile.First();
                                    var hasIndicator = actualFiles.Any(e => e.EntityFileId != null) || entityActualFile.Any();
                                    var newCrudActivity = new DbCrudActivity
                                    {
                                        EntityName = actualEntityName,
                                        ReferenceObjectName = eName,
                                        Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                        FileId = fileMaster.FileId,
                                        FileMaster = null,
                                        ProjectId = projectMaster.ProjectId,
                                        HasIndicator = !hasIndicator,
                                        OpenStatement = crudLine,
                                        IsOpen = false
                                    };

                                    using (var appDbContext = new AppDbContext())
                                    {
                                        appDbContext.DbCrudActivity.Add(newCrudActivity);
                                        await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                    }
                                    // break;
                                }
                                continue;
                            }
                            var sParts = crudLine.Split(' ').ToList();

                            int indexPosition = -1;
                            foreach (var part in sParts)
                            {
                                indexPosition++;
                                if (readOrSelectPartsList.All(p => p != part)) continue;
                                if (sParts.Count < indexPosition + 1) break;
                                string entityName = sParts[indexPosition + 1];
                                string eName = entityName.Trim().Replace(",", "");
                                if (string.IsNullOrEmpty(eName)) continue;
                                var actualFiles =
                                    (from c in allCrudReferences where c.LogicalEntityName == eName select c).ToList();
                                string actualEntityName = actualFiles.Any() ? actualFiles.First().PhysicalEntityName : "-";
                                var entityActualFile = (from e in entityMasters where e == eName select e).ToList();
                                if (actualEntityName == "-" && entityActualFile.Any())
                                    actualEntityName = entityActualFile.First();

                                var hasIndicator = actualFiles.Any(e => e.EntityFileId != null) || entityActualFile.Any();
                                var newCrudActivity = new DbCrudActivity
                                {
                                    EntityName = actualEntityName,
                                    ReferenceObjectName = eName,
                                    Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                    FileId = fileMaster.FileId,
                                    FileMaster = null,
                                    ProjectId = projectMaster.ProjectId,
                                    HasIndicator = !hasIndicator,
                                    OpenStatement = crudLine,
                                    IsOpen = false
                                };

                                using (var appDbContext = new AppDbContext())
                                {
                                    appDbContext.DbCrudActivity.Add(newCrudActivity);
                                    await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                                }
                                break;
                            }
                            continue;
                        }
                        if (crudLine.StartsWith("DELETE "))
                        {
                            var deleteRegEx = new Regex(@"DELETE\s+([\w\d\.]+)", RegexOptions.IgnoreCase);
                            var matchGroups = deleteRegEx.Match(crudLine).Groups;
                            string objName = matchGroups[1].Value;
                            if (string.IsNullOrEmpty(objName)) continue;
                            var actualFiles =
                                (from c in allCrudReferences where c.LogicalEntityName == objName select c).ToList();
                            var entityActualFile = (from e in entityMasters where e == objName select e).ToList();
                            string actualEntityName = actualFiles.Any() ? actualFiles.First().PhysicalEntityName : "-";
                            if (actualEntityName == "-" && entityActualFile.Any())
                                actualEntityName = entityActualFile.First();

                            var hasIndicator = actualFiles.Any(e => e.EntityFileId != null) || entityActualFile.Any();

                            var newCrudActivity = new DbCrudActivity
                            {
                                ReferenceObjectName = objName,
                                EntityName = actualEntityName,
                                Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                                InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                                Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                                SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                                FileId = fileMaster.FileId,
                                FileMaster = null,
                                ProjectId = projectMaster.ProjectId,
                                HasIndicator = !hasIndicator,
                                OpenStatement = crudLine,
                                IsOpen = false
                            };
                            using (var appDbContext = new AppDbContext())
                            {
                                appDbContext.DbCrudActivity.Add(newCrudActivity);
                                await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                            }
                            continue;
                        }
                        var crudActivity = new DbCrudActivity
                        {
                            ReferenceObjectName = "",
                            EntityName = "-",
                            Delete = deleteList.Any(crudLine.StartsWith) ? "Y" : "N",
                            InsertOrCreate = insertList.Any(crudLine.StartsWith) ? "Y" : "N",
                            Update = updateList.Any(crudLine.StartsWith) ? "Y" : "N",
                            SelectOrRead = selectList.Any(crudLine.StartsWith) ? "Y" : "N",
                            FileId = fileMaster.FileId,
                            FileMaster = null,
                            ProjectId = projectMaster.ProjectId,
                            HasIndicator = true,
                            OpenStatement = crudLine,
                            IsOpen = false
                        };
                        using (var appDbContext = new AppDbContext())
                        {
                            appDbContext.DbCrudActivity.Add(crudActivity);
                            await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                        }
                    }
                }

                return Ok("Insert CRUD-Activity process completed successfully for Project: " + projectMaster.ProjectName);
            }
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
        public async Task<IHttpActionResult> ProcessPseudoCodeConversion(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlQy = "SELECT * FROM StatementReferenceMaster WHERE ProjectId = " + projectMaster.ProjectId + " AND BaseCommandId = 1;";
                var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);
                var allRegEx = await _codeVortoService.RegexPatternMasterRepository
                             .GetAllListItems(e => e.BaseCommandId == 1).ConfigureAwait(false);
                allRegEx = allRegEx.OrderByDescending(s => s.RegexPattern.Length).ToList();
                // var allAlternateStatements = new List<StatementReferenceMaster>();
                var regexOptions = RegexOptions.CultureInvariant;

                foreach (var regEx in allRegEx)
                {
                    var regex = new Regex(regEx.RegexPattern, regexOptions);
                    foreach (var statementReference in statementReferenceMasterData)
                    {
                        int index = 1;
                        if (!string.IsNullOrEmpty(statementReference.AlternateName)) continue;
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
                            string alternateName = regEx.AlternateCodeRepresentation.Replace("<<" + index + ">>", groupValue);

                            if (!string.IsNullOrEmpty(alternateName))
                                alternateName = alternateName.ReplaceString();

                            statementReference.AlternateName = alternateName;
                            // allAlternateStatements.Add(statementReference);
                            index++;

                            if (!string.IsNullOrEmpty(statementReference.AlternateName))
                                statementReference.AlternateName = statementReference.AlternateName.ToUpper();

                            statementReference.FileMaster = null;
                            statementReference.ReferenceFileMaster = null;
                            await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementReference).ConfigureAwait(false);
                        }
                    }
                }

                Console.WriteLine("Pseudo code conversion process completed successfully for Project: " + projectMaster.ProjectName);

                /*
                foreach (var statementMaster in allAlternateStatements)
                {
                    if (!string.IsNullOrEmpty(statementMaster.AlternateName))
                        statementMaster.AlternateName = statementMaster.AlternateName.ToUpper();
                    statementMaster.FileMaster = null;
                    statementMaster.ReferenceFileMaster = null;
                    await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementMaster)
                        .ConfigureAwait(false);
                }
                */
                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllStartingPoints(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine("=============================");
                stringBuilder.AppendLine("\n" + "Started to collect all actionworkflow for project: " + projectId);

                string selectSql = " SELECT * FROM actionworkflows where ProjectId = " + projectId +
                                   " and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') " +
                                   " AND CreatedBy = 1 AND Processed = 0; ";

                var workflowRef = await _codeVortoService.ActionWorkflowsRepository
                    .GetDataFromSqlQuery<ActionWorkflows>(selectSql).ConfigureAwait(false);

                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (!workflowRef.Any()) return Ok("All Workflow processed successfully");

                workflowRef[0].ProjectMaster = projectMaster;
                var lstWorkflowRef = workflowRef.ToList();

                foreach (var workflow in lstWorkflowRef)
                {
                    try
                    {
                        // if (workflow.ActionWorkflowId <= 1372) continue;
                        if (workflow.ProjectId == null) return NotFound();
                        var workFlowProjectId = workflow.ProjectId.Value;
                        stringBuilder.AppendLine("=============================================");
                        stringBuilder.AppendLine("\n" + "Started executing next process: GetWorkFlowWorkSpaceParallel for projectId:" +
                                                 workFlowProjectId + ", and MethodStatementId is:" + workflow.MethodStatementId + ")");
                        var actionResult = await GetWorkFlowWorkSpaceParallel(workFlowProjectId, workflow.MethodStatementId)
                                .ConfigureAwait(false);

                        var dataContent = await actionResult.ExecuteAsync(CancellationToken.None).ConfigureAwait(false);
                        var responseMessage = await dataContent.Content.ReadAsStringAsync().ConfigureAwait(false);

                        if (responseMessage == "\"Too Many Nodes\"")
                        {
                            Console.WriteLine("Too Many Nodes for ActionWorkflowId: " + workflow.ActionWorkflowId);
                            Console.WriteLine("Origin file path: " + workflow.OriginFilePath);
                            Console.WriteLine("=======================================================");
                            continue;
                        }

                        workflow.Processed = 1;
                        workflow.ProjectMaster = null;
                        workflow.FileMaster = null;
                        await _codeVortoService.ActionWorkflowsRepository.UpdateItem(workflow).ConfigureAwait(false);
                    }
                    catch (Exception exception)
                    {
                        LogMessage.WriteExceptionLogMessage(exception);
                    }
                }
                stringBuilder.AppendLine("================================================");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("All Workflow processed successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceParallel(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var stringBuilder = new StringBuilder();
                stringBuilder.AppendLine("====================================");
                Console.WriteLine("===========================================");
                stringBuilder.AppendLine("\n" + " Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);
                Console.WriteLine("Started GetWorkFlowWorkSpaceParallel for projectId: " + projectId +
                                         " and StatementId: " + stmtId);
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                var secondTab = new List<TreeView>();

                var startClasses = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(p => p.MethodStatementId != 0
                                          && p.ProjectId == projectId && p.Processed == 0).ConfigureAwait(false);

                var actionWorkflows = (from s in startClasses where s.MethodStatementId == stmtId select s).ToList().First();
                if (actionWorkflows.FileId == 0) return Ok("Action Workflow processed successfully.");

                stringBuilder.AppendLine("=====================================================");
                stringBuilder.AppendLine("\n" + "Called stored procedure: SpBaseStatementMaster : " + projectId + "," + actionWorkflows);

                string sqlQuery = " SELECT * from statementreferencemaster " +
                                  " Where FileId = " + actionWorkflows.FileId + " AND BaseCommandId = 19;";
                var baseStatementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery).ConfigureAwait(false);

                stringBuilder.AppendLine("=================================================");
                stringBuilder.AppendLine("\n" + "Started process for get block: GetMethodBlock for statememtId: " + stmtId + ")");
                var workflowRef = await GetMethodBlock(stmtId).ConfigureAwait(false);

                if (!workflowRef.Any()) return Ok("Workflow data collected successfully");

                int currentFileId = actionWorkflows.FileId;
                var isThisProgramIdExists = await _codeVortoService.FirstTabProgramDetailsRepository
                    .GetAllListItems(e => e.ProgramId == currentFileId).ConfigureAwait(false);
                if (isThisProgramIdExists.Any())
                    return Ok("Workflow data collected successfully");

                var jclMenuTitle = startClasses.First().OriginObject;

                var bId = workflowRef[0].BaseCommandId;
                var statementId = workflowRef[0].StatementId;
                var treeNodeId = 1;
                lstTreeView.Add(new TreeView
                {
                    GraphId = "StartNode_1",
                    GraphName = "<span class='nodeToBold'>" + jclMenuTitle + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster[0].BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = jclMenuTitle,
                    ActualStatementId = "Actual_" + baseStatementMaster[0].StatementId,
                    NodeId = treeNodeId,
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
                    AlternateName = statementMaster.AlternateName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2
                }));

                #endregion

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;
                    treeItem.BaseCommandId = 5;
                    treeItem.ProgramId = treeItem.StatementReferenceMaster.ReferenceFileId;
                }
                foreach (var treeItem in copyOfLstTreeView)
                {
                    if (treeItem.BaseCommandId != 6) continue;
                    // string methodCalled = 
                    //    Path.GetFileNameWithoutExtension(treeItem.StatementReferenceMaster.ReferenceFileMaster.FilePath) +"()";
                    treeItem.MethodCalled = treeItem.StatementReferenceMaster.MethodCalled;
                    treeItem.ProgramId = treeItem.StatementReferenceMaster.ReferenceFileId;
                }

                #region Process for base command id = 5 and 6

                copyOfLstTreeView.Where(a => a.BaseCommandId == 10).ToList().ForEach(b => { b.Done = false; });
                copyOfLstTreeView = copyOfLstTreeView.DistinctBy().ToList();

                copyOfLstTreeView = copyOfLstTreeView.IfBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.LoopBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.ElseBlockStatement(copyOfLstTreeView);

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));
                secondTab.AddRange(copyOfLstTreeView
                    .Where(
                        item => item.BaseCommandId == 6
                            || item.BaseCommandId == 8
                            || item.BaseCommandId == 10
                            || item.BaseCommandId == 1
                            || item.BaseCommandId == 25
                            || item.BaseCommandId == 5
                            || item.BaseCommandId == 30
                            || item.BaseCommandId == 3
                            || item.BaseCommandId == 4
                            || item.BaseCommandId == 45));
                var tempList =
                    (from d in secondTab
                     where d.BaseCommandId == 1
                           || d.BaseCommandId == 10
                           || d.BaseCommandId == 25 || d.BaseCommandId == 3
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
                    secondTab.AddRange(childItems);
                }

                secondTab = secondTab.Distinct().ToList();
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();

                #endregion

                var allSeqListItems = new List<TreeView>();
                foreach (var curItem in secondTab)
                {
                    stringBuilder.AppendLine("Started process for attchchilditems: AttachChildItems(" + projectId + ")");
                    allSeqListItems.Add(curItem);
                    var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
                    foreach (var cItem in childItems)
                    {
                        allSeqListItems = allSeqListItems.AttachChildItems(secondTab, cItem);
                    }
                    break;
                }
                allSeqListItems = allSeqListItems.DistinctBy().ToList();

                #region Process the details

                // ReSharper disable once RedundantAssignment
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>
                    (s => s.MethodStatementId == statementId && s.ProjectId == projectId).ConfigureAwait(false);

                var secondTabDetails = allSeqListItems.Select(sTab => new WorkflowTreeviewSecondTabDetails
                {
                    BaseCommandId = sTab.BaseCommandId,
                    ProjectId = projectId,
                    ActionWorkflowId = actionWorkflow.First().ActionWorkflowId,
                    ActualStatementId = sTab.ActualStatementId,
                    ClassCalled = sTab.ClassCalled,
                    GraphId = sTab.GraphId,
                    GraphName = sTab.GraphName,
                    AlternateName = sTab.AlternateName,
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = sTab.StatementReferenceMaster.StatementId,
                    IndentLevel = sTab.IndentLevel,
                    ProgramId = sTab.StatementReferenceMaster.ReferenceFileId,
                    AnnotateStatement = null
                }).ToList();
                var generalRepositorySecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                int actionWorkFlowId = actionWorkflow.First().ActionWorkflowId;
                int methodStartStatementId = actionWorkflow.First().MethodStatementId;
                // Before inserting these records, check whether this table has already data for this action workflow id.
                var generalRepositoryFirstDetails = new GeneralRepository<WorkflowTreeviewTabFirstDetails>(new AppDbContext());

                var chkHasResults = await generalRepositoryFirstDetails
                    .GetAllListItems(t => t.ActionWorkflowId == actionWorkFlowId &&
                        t.WorkflowStartStatementId == methodStartStatementId).ConfigureAwait(false);

                if (!chkHasResults.Any())
                    await generalRepositorySecondTabDetails.BulkInsert(secondTabDetails).ConfigureAwait(false);

                string mySqlQry = " Select * from WorkflowTreeviewSecondTabDetails Where " +
                                  " ProjectId = " + projectId + " AND WorkflowStartStatementId = " + statementId + " ";

                if (!chkHasResults.Any())
                {
                    var statementSecTab = await generalRepositorySecondTabDetails
                        .GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(mySqlQry).ConfigureAwait(false);
                    foreach (var stmt in statementSecTab)
                    {
                        if (stmt.BaseCommandId != 1 && stmt.BaseCommandId != 5 && stmt.BaseCommandId != 6)
                            continue;
                        stmt.GraphName = stmt.GraphName + "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' " +
                                         "onclick='pseudoCodeDialog(" + stmt.ProgramId + ")'/>";
                        await generalRepositorySecondTabDetails.UpdateItem(stmt).ConfigureAwait(false);
                    }
                }

                var lstTabFirstDetails = copyOfLstTreeView.Select(fTab => new WorkflowTreeviewTabFirstDetails
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
                    IndentLevel = fTab.IndentLevel,
                    ProgramId = fTab.StatementReferenceMaster.ReferenceFileId
                }).ToList();

                if (!chkHasResults.Any())
                    await generalRepositoryFirstDetails.BulkInsert(lstTabFirstDetails).ConfigureAwait(false);

                // Forth tabs data is about Nodes and Links which are created from second tabs data, so we will add default node 
                // from seconds tabs first element, which is a starting point of workflow.

                var generalRepositoryNodeDetails =
                   new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                var workflowMaxNode = await generalRepositoryNodeDetails.GetDataFromSqlQuery<WorkflowNodeDetails>(
                    "SELECT * FROM workflownodedetails ORDER BY RowId DESC LIMIT 1;").ConfigureAwait(false);
                var nodeId = 1;
                if (workflowMaxNode.Any())
                {
                    nodeId = workflowMaxNode[0].MaxNodeId;
                    nodeId = nodeId + 1;
                }

                var listNodes = new List<Node>();
                var treeView = new TreeViewData
                {
                    Nodes = listNodes
                };

                var widthCnt = Convert.ToInt32(jclMenuTitle.Length.ToString()) * 3;
                treeView.Nodes.Add(new Node
                {
                    Id = nodeId,
                    Name = jclMenuTitle.ToUpper(),
                    ShapeId = "Circle",
                    Color = "#ffcc00",
                    Width = widthCnt.ToString(),
                    StatementId = int.Parse(secondTab.First().ActualStatementId.Split('_')[1]),
                    GroupName = secondTab.First().GroupName,
                    GroupId = secondTab.First().GroupId,
                    ProgramId = secondTab.First().ProgramId
                });

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
                    GroupId = node.GroupId,
                    ProgramId = node.ProgramId
                }).ToList();

                generalRepositoryNodeDetails = new GeneralRepository<WorkflowNodeDetails>(new AppDbContext());
                if (!chkHasResults.Any())
                    await generalRepositoryNodeDetails.BulkInsert(lstWorkflowNodeDetails).ConfigureAwait(false);

                // Since now we have all records about primary first and second tabs data, 
                // then check whether those programs / Jcls are processed or not.
                // If not, then process those and then insert those records into FirstTabProgramDetails and 
                // SecondTabProgramDetails tables respectively...

                // Both the tables will contain data from same program, so we will start processing from first tab data...
                #endregion

                Console.WriteLine("====================================");
                LogMessage.WriteLogMessage(stringBuilder);
                return Ok("Workflow data collected successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessSubRoutineWorkflows(ProjectMaster projectMaster)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    string sqlQy = "SELECT * FROM FileMaster WHERE FileTypeExtensionId = 17 " +
                                   " AND ProjectId = " + projectMaster.ProjectId + ";";
                    var fileMaster = await _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                        .ConfigureAwait(false);

                    var copyOfFileMaster = fileMaster.Where(f => f.FileTypeExtensionId == 17
                                                && f.ProjectId == projectMaster.ProjectId && f.Processed == 1).ToList();
                    int totalCount = copyOfFileMaster.Count;
                    int loopCount = 0;
                    foreach (var copyFile in copyOfFileMaster)
                    {
                        loopCount++;
                        Console.WriteLine("Total Workflows for Subroutines to process: " + totalCount);

                        await WorkFlowWorkSpaceForSubRoutineAndIncludes(copyFile).ConfigureAwait(false);

                        Console.WriteLine("Total Remaining Workflows for Subroutines to process: " + (totalCount - loopCount));
                        Console.WriteLine("=============================================================");
                        copyFile.WorkFlowStatus = "Workflow processed successfully";
                        copyFile.ProjectMaster = null;
                        copyFile.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(copyFile).ConfigureAwait(false);
                    }
                    return Ok("All Subroutine files are processed successfully for Project: " + projectMaster.ProjectName);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return InternalServerError(exception);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessIncludeWorkflows(ProjectMaster projectMaster)
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    string sqlQy = "SELECT * FROM FileMaster WHERE FileTypeExtensionId = 12 " +
                                   " AND ProjectId = " + projectMaster.ProjectId + ";";
                    var fileMaster = await _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(sqlQy)
                        .ConfigureAwait(false);

                    var copyOfFileMaster = fileMaster.Where(f => f.FileTypeExtensionId == 12
                                            && f.ProjectId == projectMaster.ProjectId && f.Processed == 1).ToList();

                    int totalCount = copyOfFileMaster.Count;
                    int loopCount = 0;

                    foreach (var copyFile in copyOfFileMaster)
                    {
                        loopCount++;
                        Console.WriteLine("Total Workflows for Includes to process: " + totalCount);

                        await WorkFlowWorkSpaceForSubRoutineAndIncludes(copyFile).ConfigureAwait(false);

                        Console.WriteLine("Total Remaining Workflows for Includes to process: " + (totalCount - loopCount));
                        Console.WriteLine("=============================================================");
                        copyFile.WorkFlowStatus = "Workflow processed successfully";
                        copyFile.ProjectMaster = null;
                        copyFile.FileTypeExtensionReference = null;
                        await _codeVortoService.FileMasterRepository.UpdateItem(copyFile).ConfigureAwait(false);
                    }
                    return Ok("All Include files are processed successfully for Project: " + projectMaster.ProjectName);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
                return InternalServerError(exception);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> MissingObjectsReport(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest();

                #region Missing entities report...
                /*
                var missingEnitiesList = new List<MissingObjects>();                
                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => (x.FileTypeExtensionId == 9 || x.FileTypeExtensionId == 17 ||
                                           x.FileTypeExtensionId == 12 || x.FileTypeExtensionId == 10) &&
                                          x.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);

                var entitiesData = await _codeVortoService.FileMasterRepository.GetAllListItems(
                    x => x.FileTypeExtensionId == 11 && x.ProjectId == projectMaster.ProjectId);

                var keywordList = new List<string>
                {
                    "MATWRITE ", "MATWRITEL ", "MATWRITEU ", "WRITEVU ", "WRITE ", "WRITEU ", "WRITEV ", "WRITET ", "WRITEBLK ",
                    "WRITESEQF ", "WRITESEQ ", "DELETE ", "DELETELIST ", "DELETEU ", "MATREAD ", "MATREADL ", "MATREADU ",
                    "OPENCHECK ", "OPENDEV ", "OPENPATH ", "OPENSEQ ", "OPEN ", "LOCATE ", "READ ", "READL ", "READU ",
                    "READV ", "REAFV ", "READSEQ ", "READVL ", "READVU ", "READBLK ", "READLIST ", "READNEXT ", "READT ",
                    "SSELECT ", "SELECT "
                };
                // TODO: Before start uncomment commented part for CRUD Activity...
                var insertList = new List<string> { "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var updateList = new List<string>{ "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ",
                    "WRITE ", "WRITEU ", "WRITEV ", "WRITET ",
                    "WRITEBLK ", "WRITESEQF ", "WRITESEQ " };
                var deleteList = new List<string> { "DELETE ", "DELETELIST ", "DELETEU " };
                var selectList = new List<string> { "MATREAD ","MATREADL ","MATREADU ",
                    "OPENCHECK ","OPENDEV ","OPENPATH ","OPENSEQ ","OPEN ","LOCATE ",
                    "READ ", "READL ", "READU ", "READV ","REAFV ","READSEQ ",
                    "READVL ","READVU ","READBLK ","READLIST ","READNEXT ","READT ",
                    "SSELECT ", "SELECT "};

                var regex = new Regex(@"^LIST\s+");
                var regexClear =
                    new Regex(@"(^EXECUTE\s+""CLEAR.FILE)|(^EXECUTE\s+'CLEAR.FILE)|(^CLEAR.FILE\s+)|(^EXECUTE\s+CLEAR.FILE)");
                var regexEd = new Regex(@"^ED\s+");
                var regexTo = new Regex(@"\s+TO\s+([\w\d\.]+)");
                var recordRegex = new Regex(@"R{1,}[.]([\w\d\.]+)");
                var keyRegex = new Regex(@"K{1,}[.]([\w\d\.]+)");                
                int loopCount = 0;
                int totalCount = fileMasters.Count;
                foreach (var fileMaster in fileMasters)
                {
                    loopCount++;
                    Console.WriteLine("===========================================================");
                    Console.WriteLine("Processing Missing Entities Report. Files remaining: " + (totalCount - loopCount));
                    Console.WriteLine("Current File: " + fileMaster.FileName);
                    var entityMissingFileWise = new List<MissingObjects>();
                    var currentFileId = fileMaster.FileId;
                    var viewSource = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(x => x.FileId == currentFileId && x.ProjectId == projectMaster.ProjectId)
                        .ConfigureAwait(false);
                    if (string.IsNullOrEmpty(viewSource?.SourceWithoutComments)) continue;
                    string[] sourceData = viewSource.SourceWithoutComments.Split('\n');
                    // LINQ to following commented foreach statements block...
                    var onContainsStatements = (from line in sourceData
                                                let currentLine = line.Trim()
                                                let check = keywordList.Any(currentLine.StartsWith)
                                                where check
                                                where regexTo.IsMatch(line)
                                                select currentLine).ToList();                    
                    // foreach (var line in sourceData)
                    // {
                    //    var currentLine = line.Trim();
                    //    var check = keywordList.Any(x => currentLine.StartsWith(x));
                    //    if (!check) continue;
                    //    if (!regexTo.IsMatch(line)) continue;
                    //    onContainsStatements.Add(currentLine);
                    // }
                    if (!onContainsStatements.Any()) continue;
                    foreach (var statement in onContainsStatements)
                    {
                        var onContent = regexTo.Match(statement).Groups[1].Value;
                        if (string.IsNullOrEmpty(onContent)) continue;
                        var regexPattern = new Regex(@"R{1,}[.]([\w\d\.]+)|K{1,}[.]([\w\d\.]+)");
                        var matches = regexPattern.Matches(statement);
                        string recordValue = string.Empty;
                        string keyValue = string.Empty;
                        foreach (var match in matches)
                        {
                            var matchWord = match.ToString().Trim();
                            if (!recordRegex.IsMatch(matchWord) && !keyRegex.IsMatch(matchWord)) continue;
                            if (recordRegex.IsMatch(matchWord))
                            {
                                var rVal = recordRegex.Match(matchWord).Groups[1].Value;
                                if (!string.IsNullOrEmpty(rVal)) recordValue = rVal;
                            }
                            else if (keyRegex.IsMatch(matchWord))
                            {
                                var kVal = keyRegex.Match(matchWord).Groups[1].Value;
                                if (!string.IsNullOrEmpty(kVal)) keyValue = kVal;
                            }
                        }
                        var verifyEntityCheck = (from e in entitiesData
                                                 let f = Path.GetFileNameWithoutExtension(e.FileName)
                                                 where recordValue != null && recordValue == f
                                                 where keyValue != null && keyValue == f
                                                 select e).ToList();

                        if (verifyEntityCheck.Any()) continue;

                        var regExOpen = new Regex("(\".*?\")|(\'.*?\')", RegexOptions.IgnoreCase);
                        var beforeElseStatement = statement.Split(new[] { " ELSE " }, StringSplitOptions.None).First();
                        var openMatches = regExOpen.Matches(beforeElseStatement);
                        bool openObjPresent = false;
                        bool hasIndicator = true;
                        foreach (Group part in openMatches)
                        {
                            if (!(part.Value.StartsWith("'") || part.Value.StartsWith("\""))
                                && !(part.Value.EndsWith("'") || part.Value.StartsWith("\""))) continue;
                            if (string.IsNullOrEmpty(part.Value)) continue;
                            var objectName = part.Value.Replace("'", "").Replace("&", "").Replace("\"", "");
                            if (string.IsNullOrEmpty(objectName) || string.IsNullOrWhiteSpace(objectName)) continue;
                            objectName = objectName.Trim().TrimStart(',').TrimEnd(',');
                            var existingFiles = await _codeVortoService.FileMasterRepository
                                .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId &&
                                                      f.SolutionId == projectMaster.SolutionId &&
                                                      f.FileName.StartsWith(objectName)).ConfigureAwait(false);
                            var actualFiles = (from f in existingFiles
                                               let fName = Path.GetFileNameWithoutExtension(f.FilePath)
                                               where fName == objectName
                                               select f).ToList();

                            hasIndicator = !actualFiles.Any();

                            if (actualFiles.Any()) openObjPresent = true;

                            // finalValue = objectName;
                        }
                        if (openObjPresent) continue;

                        entityMissingFileWise.Add(new MissingObjects
                        {
                            FileId = fileMaster.FileId,
                            ProjectId = projectMaster.ProjectId,
                            CalledObjectName = onContent,
                            Type = "Entity",
                            Statement = statement,
                            FromObject = fileMaster.FileName,
                            FileMaster = null,
                            MissingObjectId = 0
                        });

                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "N";

                        if (insertList.Any(c => statement.Contains(c))) iR = "Y";
                        if (updateList.Any(c => statement.Contains(c))) update = "Y";
                        if (deleteList.Any(c => statement.Contains(c))) delete = "Y";
                        if (selectList.Any(c => statement.Contains(c))) select = "Y";
                        if (regex.IsMatch(statement)) select = "Y";
                        if (regexClear.IsMatch(statement)) delete = "Y";
                        if (regexEd.IsMatch(statement)) update = "Y";

                        var newCrudActivity = new DbCrudActivity
                        {
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileMaster = null,
                            EntityName = onContent,
                            IsOpen = statement.StartsWith("OPEN "),
                            OpenStatement = statement,
                            HasIndicator = hasIndicator,
                            FileId = fileMaster.FileId,
                            ProjectId = fileMaster.ProjectId
                        };
                        await _codeVortoService.DbCrudActivityRepository.AddNewItem(newCrudActivity).ConfigureAwait(false);
                    }

                    missingEnitiesList.AddRange(entityMissingFileWise);
                }
                await _codeVortoService.MissingObjectsRepository.BulkInsert(missingEnitiesList).ConfigureAwait(false);
                */
                #endregion

                #region This is new modified code for missing entities report.

                string sqlQuery = " SELECT * FROM CrudObjectReferences WHERE ProjectId = " + projectMaster.ProjectId +
                                  " AND EntityFileId IS NULL; /* " +
                                  " AND PhysicalEntityName IS NOT NULL GROUP BY PhysicalEntityName; */";
                var missingEntities = await _codeVortoService.CrudObjectReferenceRepository
                    .GetDataFromSqlQuery<CrudObjectReferences>(sqlQuery).ConfigureAwait(false);

                var allEntities = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId == 11)
                    .ConfigureAwait(false);

                if (!missingEntities.Any()) return NotFound();

                var distinctMissingEntities = new List<MissingObjects>();

                foreach (var missingEntity in missingEntities)
                {
                    var usedByFileId = missingEntity.UsedByFileId;
                    var usedCrudReferences = await _codeVortoService.CrudObjectReferenceRepository
                        .GetAllListItems(c => c.ObjectFileId == usedByFileId).ConfigureAwait(false);

                    foreach (var crudReference in usedCrudReferences)
                    {
                        if (string.IsNullOrEmpty(crudReference.Statement)) continue;
                        if (crudReference.ObjectFileMaster.FileTypeExtensionId != 12) continue;
                        if (string.IsNullOrEmpty(crudReference.PhysicalEntityName)) continue;

                        var anyExistingEntity = (from e in allEntities
                                                 let entityName = Path.GetFileNameWithoutExtension(e.FilePath)
                                                 where entityName == crudReference.PhysicalEntityName
                                                 select e).ToList();

                        if (anyExistingEntity.Any()) continue;

                        var missingObjReference = new MissingObjects
                        {
                            FileId = missingEntity.ObjectFileId ?? 0,
                            ProjectId = crudReference.ProjectId,
                            FromObject = crudReference.ObjectName,
                            Statement = crudReference.Statement,
                            Type = "Entity",
                            CalledObjectName = crudReference.LogicalEntityName,
                            EntityName = crudReference.PhysicalEntityName,
                            FileMaster = null
                        };
                        bool alreadyExistsRef =
                            distinctMissingEntities.Any(e => e.FileId == missingObjReference.FileId &&
                                                             e.ProjectId == missingObjReference.ProjectId &&
                                                             e.CalledObjectName == missingObjReference.CalledObjectName &&
                                                             e.FromObject == missingObjReference.FromObject &&
                                                             e.EntityName == missingObjReference.EntityName);
                        if (alreadyExistsRef) continue;

                        distinctMissingEntities.Add(missingObjReference);

                        await _codeVortoService.MissingObjectsRepository
                            .AddNewItem(missingObjReference).ConfigureAwait(false);
                    }

                    if (string.IsNullOrEmpty(missingEntity.Statement)) continue;
                    var missingObjects = new MissingObjects
                    {
                        FileId = missingEntity.ObjectFileId ?? 0,
                        ProjectId = missingEntity.ProjectId,
                        FromObject = missingEntity.ObjectName,
                        Statement = missingEntity.Statement,
                        Type = "Entity",
                        CalledObjectName = missingEntity.LogicalEntityName,
                        EntityName = missingEntity.PhysicalEntityName,
                        FileMaster = null
                    };
                    bool alreadyExists =
                        distinctMissingEntities.Any(e => e.FileId == missingObjects.FileId &&
                                                         e.ProjectId == missingObjects.ProjectId &&
                                                         e.CalledObjectName == missingObjects.CalledObjectName &&
                                                         e.FromObject == missingObjects.FromObject &&
                                                         e.EntityName == missingObjects.EntityName);
                    if (alreadyExists) continue;

                    distinctMissingEntities.Add(missingObjects);

                    await _codeVortoService.MissingObjectsRepository.AddNewItem(missingObjects).ConfigureAwait(false);
                }

                #endregion

                Console.WriteLine("Processing Completed for Missing Entities Report");
                Console.WriteLine("=============================================================");
                Console.WriteLine("Started Processing for Missing Objects Report");

                var missingEnitiesList = new List<MissingObjects>();
                var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(x => x.BaseCommandId == 6 && x.ReferenceFileId == 0 &&
                                          x.ProjectId == projectMaster.ProjectId)
                    .ConfigureAwait(false);
                var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s"); //|^RUN\s

                int loopCount = 0;
                int totalCount = statementReferenceMaster.Count;
                foreach (var statementReference in statementReferenceMaster)
                {
                    loopCount++;
                    Console.WriteLine("=============================================================");
                    Console.WriteLine("Processing Missing Objects Report. Statements remaining: " + (totalCount - loopCount));

                    var fileId = statementReference.FileId;
                    var oStatement = statementReference.OriginalStatement;
                    var missingFileName = string.Empty;
                    string type;
                    if (regEx.IsMatch(oStatement))
                    {
                        oStatement = Regex.Replace(oStatement, @"\s+", " ").Trim();
                        int splitLength = oStatement.Split(' ').Length;
                        if (splitLength <= 1) continue;
                        missingFileName = regEx.IsMatch(oStatement) ? oStatement.Split(' ')[1] : oStatement.Split(' ')[2];
                        type = "Jcl";
                    }
                    else if (oStatement.StartsWith("CALL "))
                    {
                        type = "SubRoutine";
                        oStatement = Regex.Replace(oStatement, @"\s+", " ").Trim();
                        if (oStatement.StartsWith("CALL ") && !oStatement.StartsWith("CALL @"))
                            oStatement = oStatement.Replace("CALL ", "CALL @");
                        if (oStatement.ContainsAll("@", "("))
                        {
                            int atIndex = oStatement.IndexOf('@') + 1;
                            string subStrig = oStatement.Substring(atIndex);
                            missingFileName = subStrig.Split('(').FirstOrDefault() ?? "File-Not-Found";
                        }
                        else
                        {
                            var pgmName = oStatement.Split(' ');
                            missingFileName = pgmName.Length > 2
                                ? pgmName[2]
                                : oStatement.Replace("CALL ", "").Replace("@", "").Trim().Split('(').First();
                        }
                    }
                    else if (oStatement.StartsWith("$INSERT") || oStatement.StartsWith("$INCLUDE"))
                    {
                        type = "Include";
                        missingFileName = oStatement.ExtractIncludeFileName();
                    }
                    else
                    {
                        type = "Program";
                        var pgmName = oStatement.Split(' ');
                        if (pgmName.Length > 2)
                            missingFileName = pgmName[2];
                    }
                    if (string.IsNullOrEmpty(missingFileName)) continue;
                    var objectExist = missingEnitiesList
                        .Where(x => x.FileId == fileId && x.CalledObjectName == missingFileName).ToList();
                    if (objectExist.Any()) continue;

                    if (missingEnitiesList.Any(m => m.CalledObjectName == missingFileName
                                                    && m.FileId == statementReference.FileId && m.Type == type)) continue;

                    var missingObject = new MissingObjects
                    {
                        FileId = fileId,
                        CalledObjectName = missingFileName,
                        ProjectId = statementReference.ProjectId,
                        Type = type,
                        FromObject = statementReference.FileMaster.FileName,
                        FileMaster = null,
                        Statement = statementReference.OriginalStatement,
                        MissingObjectId = 0
                    };
                    missingEnitiesList.Add(missingObject);
                }
                if (!missingEnitiesList.Any()) return Ok(missingEnitiesList);

                await _codeVortoService.MissingObjectsRepository.BulkInsert(missingEnitiesList).ConfigureAwait(false);

                Console.WriteLine("Processing Completed for Missing Objects Report");
                Console.WriteLine("=============================================================");

                return Ok(missingEnitiesList);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessDataDependencyForUniVerse(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                // if (projectMaster == null) return BadRequest();

                var openList = new List<string> { "OPEN " };

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var universeBasicDataDictionary = await _codeVortoService.DataDictionaryRepository
                    .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);

                var readOrSelectPartsList = new[] { "FROM", "TO", "WITH" }; //new []{"FROM", "TO", "WITH"};
                var openRegEx = new Regex("\'(.*?)\'");

                foreach (var fileMaster in fileMasters.Where(f => f.FileTypeExtensionId != 10))
                {
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    // var fileLines = contentsWithoutComment.Where(fileLine => mainList.Any(fileLine.StartsWith)).ToList();
                    // This is to check for specific statement types.
                    var fileLines = contentsWithoutComment.Where(fileLine => fileLine.StartsWith("OPEN ")).ToList();

                    var rawCrudLinesList = fileLines.Select(fileLine => fileLine.Replace(",", ", ")).ToList();
                    rawCrudLinesList = rawCrudLinesList.Distinct().ToList();
                    var crudLinesList = rawCrudLinesList.RemoveSpacesBetweenWords();

                    if (!fileLines.Any()) continue;

                    foreach (var crudLine in crudLinesList)
                    {
                        if (!openList.Any(crudLine.StartsWith)) continue;
                        var currentPart = crudLine.Split(readOrSelectPartsList, StringSplitOptions.None).First();
                        currentPart = currentPart.Replace("'',", "").Replace("' ',", "");
                        var matches = openRegEx.Matches(currentPart);
                        foreach (Match match in matches)
                        {
                            var currentEntity = match.Groups[1].Value;
                            if (string.IsNullOrEmpty(currentEntity)) continue;
                            currentEntity = currentEntity.Trim();
                            if (string.IsNullOrEmpty(currentEntity)) continue;
                            string eName = currentEntity.Trim().Replace(",", "");
                            if (string.IsNullOrEmpty(eName)) continue;

                            var attribute = new List<string>();
                            var dataAttributeList = (from d in universeBasicDataDictionary
                                                     where d.FileName == eName
                                                     select d).ToList();
                            int index = -1;
                            foreach (var attList in dataAttributeList)
                            {
                                index++;
                                if (index == 0) continue;
                                string attr = attList.Description;
                                if (!string.IsNullOrEmpty(attr)) attribute.Add(attr);
                            }

                            var dataDependency = new DataDependency
                            {
                                ProjectId = projectMaster.ProjectId,
                                Attributes = string.Join(", ", attribute),
                                Entity = eName,
                                EntityOld = eName,
                                FileId = fileMaster.FileId,
                                FileIdOld = fileMaster.FileId,
                                FileMaster = null
                            };

                            await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependency)
                                .ConfigureAwait(false);
                            Console.WriteLine("=================================================");
                            Console.WriteLine(JsonConvert.SerializeObject(dataDependency));
                        }
                    }
                }

                // This is only for Jcl files...

                var sqlQry = " SELECT * FROM UniverseBasicDataDictionary WHERE " +
                             " ProjectId = " + projectMaster.ProjectId + " GROUP BY FileName;";
                var dataDictionaryWithFileName = await _codeVortoService.DataDictionaryRepository
                    .GetDataFromSqlQuery<DataDictionary>(sqlQry).ConfigureAwait(false);

                var allJclFiles = fileMasters.Where(f => f.FileTypeExtensionId == 10).ToList();

                foreach (var jclFile in allJclFiles)
                {
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == jclFile.FileId).ConfigureAwait(false);
                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;
                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    var omitListForJcl = new List<string> { "PA ", "*", "PH ", "PHANTOM ", "DISPLAY " };

                    foreach (var dataDictionary in dataDictionaryWithFileName)
                    {
                        string entityName = dataDictionary.FileName;
                        var recordRegEx = new Regex(@"[\'\(\)\""\*\:\;\,\s]+" + entityName, RegexOptions.IgnoreCase);

                        var nameValueList = new List<NameValue>();

                        foreach (var fileLine in contentsWithoutComment)
                        {
                            if (omitListForJcl.Any(fileLine.StartsWith)) continue;
                            if (!recordRegEx.IsMatch(fileLine)) continue;
                            if (nameValueList.Any(n => n.Value == projectMaster.ProjectId
                            && n.MethodName == dataDictionary.FileName && n.ProgramId == jclFile.FileId)) continue;
                            var dataDependancy = new DataDependency
                            {
                                FileId = jclFile.FileId,
                                Entity = dataDictionary.FileName,
                                Attributes = "",
                                ProjectId = projectMaster.ProjectId,
                                EntityOld = dataDictionary.FileName,
                                FileIdOld = jclFile.FileId
                            };
                            nameValueList.Add(new NameValue
                            {
                                ProgramId = jclFile.FileId,
                                Value = projectMaster.ProjectId,
                                MethodName = dataDictionary.FileName
                            });

                            var attribute = new List<string>();
                            var dataAttributeList = (from d in universeBasicDataDictionary
                                                     where d.FileName == dataDictionary.FileName
                                                     select d).ToList();
                            int index = -1;
                            foreach (var attList in dataAttributeList)
                            {
                                index++;
                                if (index == 0) continue;
                                string attr = attList.Description;
                                if (!string.IsNullOrEmpty(attr)) attribute.Add(attr);
                            }
                            dataDependancy.Attributes = string.Join(", ", attribute);

                            /*
                            var dataDepen = await _codeVortoService.DataDependencyRepository
                                .GetAllListItems(x => x.FileId == jclFile.FileId &&
                                                      x.Entity == dataDictionary.FileName &&
                                                      x.ProjectId == projectMaster.ProjectId).ConfigureAwait(false);
                            if (dataDepen.Any()) continue;
                            */

                            await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependancy);
                        }
                    }
                }

                return Ok("Data Dependency from source extracted successfully for project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> WorkFlowWorkSpaceForSubRoutineAndIncludes(FileMaster fileMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start pre-process

                var lstTreeView = new List<TreeView>();
                string qry = "SELECT * FROM SecondTabSubroutineProgramDetails " +
                             " WHERE ProjectId = " + fileMaster.ProjectId + " AND ProgramId = " + fileMaster.FileId + ";";
                var subRoutine = await _codeVortoService.SecondTabSubRoutineProgramRepository
                    .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(qry).ConfigureAwait(false);
                if (subRoutine.Any()) return Ok("File already exists");
                var dataDictionarySql =
                    "SELECT * FROM StatementReferenceMaster WHERE FileId = " + fileMaster.FileId + " ";
                var statementReferencesList = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql).ConfigureAwait(false);
                if (!statementReferencesList.Any()) return Ok("Record not found");
                var treeNodeId = 0;

                lstTreeView.Add(new TreeView
                {
                    GraphId = "MethodNode_" + statementReferencesList[0].StatementId,
                    GraphName = "<span class='nodeToBold'>" + statementReferencesList[0].OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "StartNode_1",
                    BaseCommandId = statementReferencesList[0].BaseCommandId,
                    StatementReferenceMaster = statementReferencesList[0],
                    ActualStatementId = "Actual_" + statementReferencesList[0].StatementId,
                    NodeId = ++treeNodeId,
                    IndentLevel = 1,
                    ProgramId = statementReferencesList[0].FileId
                });
                var copyofStatementReferece = statementReferencesList.Skip(1).ToList();
                lstTreeView.AddRange(copyofStatementReferece.Select(statementMaster => new TreeView
                {
                    ActualStatementId = "Actual_" + statementMaster.StatementId,
                    GraphId = "Node_" + statementMaster.StatementId,
                    GraphName = statementMaster.OriginalStatement,
                    HasChild = false,
                    SpriteCssClass = "",
                    ParentId = "MethodNode_" + statementReferencesList[0].StatementId,
                    BaseCommandId = statementMaster.BaseCommandId,
                    PrimaryCommandId = statementMaster.PrimaryCommandId,
                    ClassCalled = statementMaster.ClassCalled,
                    MethodCalled = statementMaster.MethodCalled,
                    StatementReferenceMaster = statementMaster,
                    AlternateName = !string.IsNullOrEmpty(statementMaster.AlternateName)
                            ? statementMaster.AlternateName : statementMaster.BusinessName,
                    NodeId = ++treeNodeId,
                    IndentLevel = 2,
                    ProgramId = statementMaster.FileId
                }));

                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);

                copyOfLstTreeView = copyOfLstTreeView.AssignColorsToMethodBlocks();
                copyOfLstTreeView = copyOfLstTreeView.IfBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.LoopBlockStatement(copyOfLstTreeView);
                copyOfLstTreeView = copyOfLstTreeView.ElseBlockStatement(copyOfLstTreeView);

                #region Second tab data

                try
                {
                    var lstWorkflowTreeviewSecondTabDetails =
                        copyOfLstTreeView.Select(sTab => new SecondTabSubRoutineProgramDetails
                        {
                            BaseCommandId = sTab.BaseCommandId,
                            ProjectId = fileMaster.ProjectId,
                            ActionWorkflowId = 0,
                            ActualStatementId = sTab.ActualStatementId,
                            ClassCalled = sTab.ClassCalled,
                            GraphId = sTab.GraphId,
                            GraphName = sTab.GraphName,
                            HasChild = sTab.HasChild.ToString(),
                            MethodCalled = sTab.MethodCalled,
                            AlternateName = sTab.AlternateName,
                            ParentId = sTab.ParentId,
                            PrimaryCommandId = sTab.PrimaryCommandId,
                            SpriteCssClass = sTab.SpriteCssClass,
                            WorkflowStartStatementId = sTab.StatementReferenceMaster?.StatementId ?? 0,
                            IndentLevel = sTab.IndentLevel,
                            ProgramId = sTab.ProgramId
                        }).ToList();

                    await _codeVortoService.SecondTabSubRoutineProgramRepository
                        .BulkInsert(lstWorkflowTreeviewSecondTabDetails)
                        .ConfigureAwait(false);

                    #region To Updated the GraphName against the BaseCommandId

                    string mySqlQuery = " SELECT * FROM SecondTabSubroutineProgramDetails WHERE " +
                                        " ProjectId = " + fileMaster.ProjectId + " AND ProgramId = " + fileMaster.FileId;

                    var statementSecondTab = await _codeVortoService.SecondTabSubRoutineProgramRepository
                        .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(mySqlQuery).ConfigureAwait(false);

                    foreach (var stmt in statementSecondTab)
                    {
                        if (stmt.BaseCommandId != 1 && stmt.BaseCommandId != 5 && stmt.BaseCommandId != 6) continue;
                        stmt.GraphName = stmt.GraphName +
                                         "&nbsp;<img id='imgpseudo' src='images/regex_icon.png' onclick='PseudoCodeDialog(" +
                                         stmt.ActualStatementId.Split('_')[1] + ")'/>";

                        await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(stmt).ConfigureAwait(false);
                    }

                    #endregion
                }
                catch (Exception exception)
                {
                    Console.WriteLine("Issues occured in file: " + fileMaster.FileName);
                    Console.WriteLine("Project Id: " + fileMaster.ProjectId);
                    Console.WriteLine(exception.InnerException);
                    LogMessage.WriteLogMessage(new StringBuilder("Issues occured in file: " + fileMaster.FileName)
                        .Append("Project Id: " + fileMaster.ProjectId));
                    LogMessage.WriteExceptionLogMessage(exception);
                }

                #endregion

                return Ok("SubRoutine or Include workflow data collected successfully");

                #endregion
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessCrudObjectReferences(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest();

                var openList = new List<string> { "OPEN ", "OPENSEQ " };

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var allEntities = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId == 11)
                    .ConfigureAwait(false);

                var readOrSelectPartsList = new List<string> { "FROM", "TO", "WITH" };
                var openRegEx = new Regex("\'(.*?)\'");

                foreach (var fileMaster in fileMasters)
                {
                    // if (fileMaster.FileId != 8042) continue;
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    // var fileLines = contentsWithoutComment.Where(fileLine => mainList.Any(fileLine.StartsWith)).ToList();
                    // This is to check for specific statement types.
                    var fileLines = contentsWithoutComment
                        .Where(fileLine => fileLine.StartsWith("OPEN ") || fileLine.StartsWith("OPENSEQ ")).ToList();

                    var rawCrudLinesList = fileLines.Select(fileLine => fileLine.Replace(",", ", ")).ToList();
                    rawCrudLinesList = rawCrudLinesList.Distinct().ToList();
                    var crudLinesList = rawCrudLinesList.RemoveSpacesBetweenWords();

                    var allReferencedFileMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(s => s.BaseCommandId == 6 && s.FileId == fileMaster.FileId).ConfigureAwait(false);

                    foreach (var statementReferenceMaster in allReferencedFileMaster)
                    {
                        if (statementReferenceMaster.ReferenceFileId == 0) continue;
                        var refFileMaster = statementReferenceMaster.ReferenceFileMaster;
                        var crudObjectReference = new CrudObjectReferences
                        {
                            CrudObjectReferenceId = 0,
                            Statement = null,
                            ObjectName = fileMaster.FileName,
                            EntityFileId = null,
                            EntityFileMaster = null,
                            LogicalEntityName = null,
                            ObjectFileId = fileMaster.FileId,
                            ObjectFileMaster = null,
                            ObjectType = fileMaster.FileTypeExtensionReference.FileTypeName,
                            UsedBy = refFileMaster.FileName,
                            PhysicalEntityName = null,
                            UsedByFileId = refFileMaster.FileId,
                            UsedByFileMaster = null,
                            ProjectId = projectMaster.ProjectId
                        };

                        await _codeVortoService.CrudObjectReferenceRepository.AddNewItem(crudObjectReference)
                            .ConfigureAwait(false);
                    }

                    if (!fileLines.Any()) continue;

                    foreach (var crudLine in crudLinesList)
                    {
                        if (!openList.Any(crudLine.StartsWith)) continue;
                        string currentEntity = string.Empty;
                        var currentPart = crudLine.Split(new[] { " TO " }, StringSplitOptions.None).First();
                        currentPart = currentPart.Replace("'',", "").Replace("' ',", "");
                        int gCount = -1;
                        foreach (Group cGroup in openRegEx.Match(currentPart).Groups)
                        {
                            gCount++;
                            if (gCount == 0) continue;
                            currentEntity = cGroup.Value;
                            if (string.IsNullOrEmpty(currentEntity)) continue;
                            currentEntity = currentEntity.Trim();
                        }
                        if (string.IsNullOrEmpty(currentEntity)) continue;
                        if (!readOrSelectPartsList.Any(crudLine.Contains)) continue;
                        var selectParts = crudLine.Split(' ').ToList();

                        var anyExistingEntity = (from e in allEntities
                                                 let entityName = Path.GetFileNameWithoutExtension(e.FilePath)
                                                 where entityName == currentEntity
                                                 select e).ToList();

                        int indexPosition = -1;
                        foreach (var selectPart in selectParts)
                        {
                            indexPosition++;
                            if (readOrSelectPartsList.All(p => p != selectPart)) continue;
                            if (selectParts.Count < indexPosition + 1) break;
                            string entityName = selectParts[indexPosition + 1];
                            string eName = entityName.Trim().Replace(",", "");
                            if (string.IsNullOrEmpty(eName)) continue;

                            int? entityFileId = anyExistingEntity.Any() ? (int?)anyExistingEntity.First().FileId : null;

                            var crudReference = new CrudObjectReferences
                            {
                                CrudObjectReferenceId = 0,
                                Statement = crudLine,
                                ObjectName = fileMaster.FileName,
                                EntityFileId = entityFileId,
                                EntityFileMaster = null,
                                LogicalEntityName = eName,
                                ObjectFileId = fileMaster.FileId,
                                ObjectFileMaster = null,
                                ObjectType = fileMaster.FileTypeExtensionReference.FileTypeName,
                                UsedBy = null,
                                PhysicalEntityName = currentEntity,
                                UsedByFileId = null,
                                UsedByFileMaster = null,
                                ProjectId = projectMaster.ProjectId
                            };

                            await _codeVortoService.CrudObjectReferenceRepository.AddNewItem(crudReference)
                                .ConfigureAwait(false);

                            break;
                        }
                    }
                }
                return Ok("Insert CRUD-Activity process completed successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessIncludeToInclude(ProjectMaster projectMaster)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var includeFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => x.ProjectId == projectMaster.ProjectId && x.FileTypeExtensionId == 12)
                    .ConfigureAwait(false);
                if (!includeFiles.Any()) return NotFound();
                var goSubDictionary = new Dictionary<FileMaster, List<string>>();
                var methodDictionary = new Dictionary<FileMaster, List<string>>();
                var mappingList = new List<string>();
                foreach (var includeFile in includeFiles)
                {
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == includeFile.FileId).ConfigureAwait(false);
                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n').ToList();
                    var allGosub = contentsWithoutComment.GetAllGoSub("GOSUB");
                    goSubDictionary.Add(includeFile, allGosub);
                    var methodRegex = new Regex(@"(^[a-zA-z0-9\.]+:)", RegexOptions.IgnoreCase);
                    var methodNames = contentsWithoutComment.Where(x => methodRegex.IsMatch(x)).ToList();
                    methodDictionary.Add(includeFile, methodNames);
                }

                foreach (var goSubDict in goSubDictionary)
                {
                    var methodNameList = goSubDict.Value;
                    foreach (var methodName in methodNameList)
                    {
                        string mName = methodName.Replace("GOSUB ", "").Trim();

                        foreach (var methodDict in methodDictionary)
                        {
                            if (methodDict.Key.FileName == goSubDict.Key.FileName) continue;
                            var methodDictNames = methodDict.Value;

                            foreach (var methodDictName in methodDictNames)
                            {
                                string matchingMethodName = methodDictName.TrimEnd('*').Trim();
                                if (matchingMethodName != mName) continue;

                                // Now, associate this object's entities to current parent object...
                                Console.WriteLine("Current Object: " + goSubDict.Key.FileName);
                                Console.WriteLine("Reference to Object: " + methodDict.Key.FileName);
                                Console.WriteLine("Method Name: " + matchingMethodName);
                                Console.WriteLine("====================================================");

                                mappingList.Add("Current Object: " + goSubDict.Key.FileName);
                                mappingList.Add("Reference to Object: " + methodDict.Key.FileName);
                                mappingList.Add("Method Name: " + matchingMethodName);
                                mappingList.Add("====================================================");

                                var crudObjectReferences =
                                    await _codeVortoService.CrudObjectReferenceRepository
                                        .GetAllListItems(c => c.ObjectFileId == methodDict.Key.FileId
                                                              && c.ProjectId == projectMaster.ProjectId)
                                        .ConfigureAwait(false);
                                foreach (var crudObjectReference in crudObjectReferences)
                                {
                                    var crudReference = new CrudObjectReferences
                                    {
                                        CrudObjectReferenceId = 0,
                                        ObjectName = goSubDict.Key.FileName,
                                        ObjectFileId = goSubDict.Key.FileId,
                                        ProjectId = crudObjectReference.ProjectId,
                                        Statement = crudObjectReference.Statement,
                                        UsedByFileId = crudObjectReference.UsedByFileId,
                                        PhysicalEntityName = crudObjectReference.PhysicalEntityName,
                                        EntityFileId = crudObjectReference.EntityFileId,
                                        LogicalEntityName = crudObjectReference.LogicalEntityName,
                                        ObjectType = crudObjectReference.ObjectType,
                                        UsedBy = crudObjectReference.UsedBy,
                                        EntityFileMaster = null,
                                        ObjectFileMaster = null,
                                        UsedByFileMaster = null
                                    };

                                    await _codeVortoService.CrudObjectReferenceRepository
                                        .AddNewItem(crudReference).ConfigureAwait(false);

                                    break;
                                }
                            }
                        }
                    }
                }
                return Ok(mappingList);
            }
        }

        [HttpGet]
        public bool ChangeExtensions(string rootPath, string extension, string dToSkip, params string[] directoryName)
        {
            dToSkip = dToSkip ?? "";
            var directoriesToSkip = dToSkip.Split(',').ToList();
            var directories = Directory.GetDirectories(rootPath, "*.*", SearchOption.AllDirectories).ToList();
            directoriesToSkip.RemoveAll(string.IsNullOrEmpty);

            if (directoriesToSkip.Any())
                directories.RemoveAll(d => directoriesToSkip.Any(n => Regex.IsMatch(d, n, RegexOptions.IgnoreCase)));

            var dirToProcess = (from d in directories
                                let dir = new DirectoryInfo(d)
                                where null != dir &&
                                      directoryName.Any(n => Regex.IsMatch(n, dir.Name, RegexOptions.IgnoreCase))
                                select d).ToList();

            foreach (var directory in dirToProcess)
            {
                var currentDirectories = Directory.GetDirectories(directory, "*.*", SearchOption.AllDirectories).ToList();
                if (directoriesToSkip.Any())
                    currentDirectories.RemoveAll(d => directoriesToSkip.Any(n => Regex.IsMatch(d, n, RegexOptions.IgnoreCase)));
                foreach (var cDir in currentDirectories)
                {
                    var allFiles = Directory.GetFiles(cDir, "*.*", SearchOption.AllDirectories).ToList();
                    foreach (var file in allFiles)
                    {
                        string fileExtension = Path.GetExtension(file);
                        if (fileExtension == extension) continue;

                        var newFile = File.ReadAllLines(file);
                        var newFileWithExtension = file + extension;
                        if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                        File.WriteAllLines(newFileWithExtension, newFile);
                        File.Delete(file);
                    }
                }
                var rootFiles = Directory.GetFiles(directory, "*.*").ToList();
                foreach (var file in rootFiles)
                {
                    string fileExtension = Path.GetExtension(file);
                    if (fileExtension == extension) continue;

                    var newFile = File.ReadAllLines(file);
                    var newFileWithExtension = file + extension;
                    if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                    File.WriteAllLines(newFileWithExtension, newFile);
                    File.Delete(file);
                }
            }
            var allRootFiles = Directory.GetFiles(rootPath, "*.*").ToList();
            foreach (var file in allRootFiles)
            {
                var newFile = File.ReadAllLines(file);
                var newFileWithExtension = file + extension;
                if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                File.WriteAllLines(newFileWithExtension, newFile);
                File.Delete(file);
            }

            return true;
        }

        private IHttpActionResult ChangeFileExtensions(ProjectMaster projectMaster)
        {
            try
            {
                var response = ChangeExtensions(projectMaster.PhysicalPath, ".jcl", "", "jcl");
                Console.WriteLine(response);

                var icdResponse = ChangeExtensions(projectMaster.PhysicalPath, ".icd", "", "include,includes");
                Console.WriteLine("Change Extensions Includes: " + icdResponse);

                var pgmResponse = ChangeExtensions(projectMaster.PhysicalPath, ".pgm",
                    "sbr,sbr.bp,subroutine,subroutines", "program,programs");
                Console.WriteLine("Change Extensions Programs: " + pgmResponse);

                var sbrResponse = ChangeExtensions(projectMaster.PhysicalPath, ".sbr", "", "sbr,sbr.bp,subroutine,subroutines");
                Console.WriteLine("Change Extensions SubRoutines: " + sbrResponse);

                var menuResponse = ChangeExtensions(projectMaster.PhysicalPath, ".csv", "", "menu,menus");
                Console.WriteLine("Change Extensions Menu File: " + menuResponse);

                Console.WriteLine("=========================================");
                Console.WriteLine("Extensions changed for all related files.");
                Console.WriteLine("=========================================");

                return Ok();
            }
            catch (Exception exception)
            {
                return InternalServerError(exception);
            }
        }

        private async Task<List<StatementReferenceMaster>> GetMethodBlock(int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId},
                    new MySqlParameter("@startBaseCommandId", MySqlDbType.Int32) {Value = 8},
                    new MySqlParameter("@endBaseCommandId", MySqlDbType.Int32) {Value = 9}
                };
                var workflowRef = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAnyGenericBlock", parameters)
                    .ConfigureAwait(false);

                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<StatementReferenceMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(workflowRef, settings);
                var listData = JsonConvert.DeserializeObject<List<StatementReferenceMaster>>(json);
                return listData;
            }
        }
    }
}
