using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using System.Xml;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class GeneralController : GeneralBase
    {
        [HttpGet]
        public async Task<IHttpActionResult> GetEntity(string entity)
        {
            var instance = GetType(entity);
            if (instance == null) return BadRequest("Entity name not found");
            var nameValue = await GetNameValue(instance, entity);
            return Ok(nameValue);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetEntity(string entity, int id)
        {
            var instance = GetType(entity);
            if (instance == null) return BadRequest("Entity name not found");

            var nameValue = await GetNameValue(instance, entity, id);
            return Ok(nameValue);
        }

        public async Task<List<NameValue>> GetNameValue(DbSet dbSet, string entity)
        {
            var data = await dbSet.ToListAsync().ContinueWith(t =>
            {
                var result = t.Result;
                var dataNew = result;
                return dataNew;
            });
            var list = new List<NameValue>();
            switch (entity)
            {
                case "StatementReferenceMaster":
                    list = (from dynamic d in data
                            select new NameValue
                            { Name = d.OriginalStatement, Value = d.StatementId }).ToList();
                    break;
            }
            var defaultList = await base.GetNameValue();
            defaultList.AddRange(list);
            return defaultList;
        }

        public async Task<List<NameValue>> GetNameValue(DbSet dbSet, string entity, int id)
        {
            var data = await dbSet.ToListAsync();
            var list = new List<NameValue>();
            switch (entity)
            {
                case "TaskReference":
                    list = (from dynamic d in data select new NameValue { Name = d.TaskType, Value = d.TaskId }).ToList();
                    break;
            }
            var defaultList = await base.GetNameValue();
            defaultList.AddRange(list);
            return defaultList;
        }

        [HttpGet]
        public async void StartingPoints(int projectId)
        {
            // Action Workflows data...
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                int projectType = projectMasterData.ProjectConfigType;
                if (projectType == 3)
                {
                    var projectConfig = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                    var projectConfigData = projectConfig.GetItem(projectType);
                    if (projectConfigData.ConfigFileId == projectType)
                    {
                        // Means the project type is windows application and we need to read starting point from that 
                        // files respective code behind file...
                        string configFileName = projectConfigData.ToString();
                        if (!string.IsNullOrEmpty(configFileName))
                        {
                            var allConfigFiles =
                                await codeVortoService.FileMasterRepository.GetAllItems(
                                    f => f.FilePath.EndsWith(configFileName) && f.ProjectId == projectId);
                            foreach (var cFile in allConfigFiles)
                            {
                                var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(cFile.FilePath);
                                if (fileNameWithoutExtension != null)
                                {
                                    string className = fileNameWithoutExtension.Split('.')[0];
                                    var genericBlocks = GetAllMethodsForClass(className);
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
                                        await codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    var allConfigFiles =
                        await codeVortoService.FileMasterRepository.GetAllItems(
                            f => f.FilePath.EndsWith(".config") && f.ProjectId == projectId);
                    foreach (var configFile in allConfigFiles)
                    {
                        if (configFile.FileName == "Web.config") continue;
                        var filePaths =
                            await codeVortoService.FileMasterRepository.GetAllItems(f => f.ProjectId == projectId);
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
                                string startPoint = node.Attributes["name"].Value;
                                string baseAddress = node.Attributes["address"].Value;
                                otherEntryPoints.Add(startPoint, baseAddress);
                            }
                        }

                        foreach (XmlNode childNodes in xmlDoc.GetElementsByTagName("services"))
                        {
                            foreach (XmlNode node in childNodes)
                            {
                                startListNames.Add(node.Attributes["name"].Value);
                                XmlNode firstNd = node.FirstChild;
                                XmlNode lastNd = node.LastChild.FirstChild.ChildNodes[0];
                                sContracts.Add(firstNd.Attributes["contract"].Value);
                                sAddress.Add(lastNd.Attributes["baseAddress"].Value);
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
                            await codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                        }
                        int listPosition = 0;
                        foreach (var clsName in startListNames)
                        {
                            object[] parameters =
                            {
                                new MySqlParameter("@clsName", MySqlDbType.VarChar) {Value = clsName.Split('.').Last()}
                            };
                            var allMethodStatements = await codeVortoService.StatementReferenceMasterRepository
                                .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetClassMethods", parameters);
                            foreach (var statement in allMethodStatements)
                            {
                                //if (statement.OriginalStatement.StartsWith("Private")) continue;

                                ActionWorkflows actionWorkflow = new ActionWorkflows
                                {
                                    ActionWorkflowId = 0,
                                    CreatedBy = 1,
                                    EndPointOrService = "Service",
                                    MethodStatementId = statement.StatementId,
                                    OriginFileName =
                                        Path.GetFileName(
                                            fileMaster.ToList().Find(f => f.FileId == statement.FileId).FilePath),
                                    OriginFilePath =
                                        fileMaster.ToList().Find(f => f.FileId == statement.FileId).FilePath,
                                    ProjectId = projectId,
                                    OriginEventMethod = statement.MethodName,
                                    OriginObject = clsName,
                                    WorkflowName = statement.OriginalStatement,
                                    ServiceBaseAddress = sAddress.ElementAt(listPosition),
                                    ServiceContract = sContracts.ElementAt(listPosition)
                                };
                                await codeVortoService.ActionWorkflowsRepository.AddNewItem(actionWorkflow);
                            }
                            listPosition++;
                        }
                    }
                }
            }
        }

        private List<StatementReferenceMaster> GetAllMethodsForClass(string className)
        {
            using (ICodeVortoService codeVortoService = new CodeVortoService())
            {
                object[] parametersExp =
                {
                    new MySqlParameter("@delim", MySqlDbType.VarChar){Value = ","},
                    new MySqlParameter("@className", MySqlDbType.VarChar) {Value = className}
                };
                var callExtExpandedCode = codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllMethodsForClass", parametersExp)
                    .ContinueWith(t => t.Result).Result;
                return callExtExpandedCode;
            }
        }
    }
}