using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using CsvHelper.Configuration;
using LumenWorks.Framework.IO.Csv;
using Newtonsoft.Json;
using CsvReader = LumenWorks.Framework.IO.Csv.CsvReader;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class TestResultController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniVerseProjectInventory(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return NotFound();

                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectMaster.ProjectId && f.FileTypeExtensionId != 11)
                    .ConfigureAwait(false);

                var entitiesToExclude = _codeVortoService.AppDbContextRepository
                    .EntitiesToExclude.Where(p => p.ProjectId == projectMaster.ProjectId).ToList();

                var allExistingEntities = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.FileTypeExtensionId == 11 && f.ProjectId == projectMaster.ProjectId &&
                                          f.SolutionId == projectMaster.SolutionId).ConfigureAwait(false);
                var allExistingEntityNames = (from entity in allExistingEntities
                                              let eName = Path.GetFileNameWithoutExtension(entity.FilePath)
                                              select eName).ToList();

                var allUniverseDescriptor = await _codeVortoService.UniverseDescriptorRepository
                    .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);

                var statementRegEx = new Regex(@"^(SELECT\s+|SSELECT\s+|LIST\s+|DOWNLOAD\s+|HEADING\s+|SORT\s+)",
                    RegexOptions.Singleline);

                var jclDictionary = new Dictionary<FileMaster, List<string>>();
                var iDescMatchDesc = new List<LstObject>();
                // UniVerse JCL Files...
                foreach (var fileMaster in fileMasters.Where(f => f.FileTypeExtensionId == 10))
                {
                    var newDataDependancy = new List<DataDependency>();
                    var lstDescriptors = new List<Descriptors>();
                    var universeDescriptorList = new List<UniverseDescriptorList>();
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) continue;

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    var allStatements = (from a in contentsWithoutComment
                                         where statementRegEx.IsMatch(a)
                                         select a).ToList();
                    if (!allStatements.Any()) continue;
                    var dataDependancy = await _codeVortoService.DataDependencyRepository
                        .GetAllListItems(x => x.FileId == fileMaster.FileId).ConfigureAwait(false);

                    newDataDependancy.AddRange(from dataD in dataDependancy
                        let excludeFile = entitiesToExclude.Where(x => x.FileId == dataD.FileId).ToList()
                        where !excludeFile.Any()
                        select dataD);
                    var dataEntity = (from d in newDataDependancy where d.FileId == fileMaster.FileId select d)
                        .ToList();
                    var universeDesc = (from d in allUniverseDescriptor
                                        where dataEntity.Any(e => d.Entity == e.Entity)
                                        select d).ToList();
                    foreach (var uDescriptor in universeDesc)
                    {
                        var spName = uDescriptor.StoredProcedureName.Trim();
                        if (spName.StartsWith("*") || spName.StartsWith(".") || spName.Contains("*")) continue;
                        var entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var iDescRegEx = new Regex("(\\s" + spName + "\\s)", RegexOptions.Singleline);
                        var isPresent = allStatements.Any(s => iDescRegEx.IsMatch(s));
                        if (!isPresent) continue;

                        // Here we need find index position of entity and spName (Stored Procedure Name).
                        // Index of entity must be less than spName.
                        // int entityNameLn = uDescriptor.Entity.Length;
                        // int spNameLn = spName.Length;

                        // Get actual matching statement
                        var statementStringList = allStatements.FindAll(s => iDescRegEx.IsMatch(s));
                        foreach (var statementString in statementStringList)
                        {
                            int indexOfEntity = statementString
                                .IndexOf(uDescriptor.Entity, StringComparison.CurrentCultureIgnoreCase);
                            int indexOfSpName = statementString
                                .IndexOf(spName, StringComparison.CurrentCultureIgnoreCase);

                            // If index of entity name is greater than index of spName, then continue.
                            if (indexOfEntity > indexOfSpName) continue;

                            lstDescriptors.Add(new Descriptors
                            {
                                CompleteName = entityName,
                                DescriptorId = uDescriptor.DescriptorId,
                                SpName = spName
                            });
                        }
                    }
                    if (!lstDescriptors.Any()) continue;
                    var file = new FileDescriptors
                    {
                        FileId = fileMaster.FileId,
                        FileName = fileMaster.FileName
                    };
                    iDescMatchDesc.Add(new LstObject
                    {
                        Descriptors = lstDescriptors,
                        FileDescriptors = file
                    });

                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.FileId == fileMaster.FileId && x.ProjectId == projectId)
                        .ConfigureAwait(false);

                    var callExternal = (from s in statementReferenceMaster
                                        where s.BaseCommandId == 6 &&
                                              !string.IsNullOrEmpty(s.ClassCalled)
                                        select s).ToList();

                    var calledFrom = (from s in statementReferenceMaster
                                      where s.ReferenceFileId == fileMaster.FileId
                                      select s).ToList();

                    var complexity = (from s in statementReferenceMaster
                                      where (s.BaseCommandId == 1 || s.BaseCommandId == 5)
                                      select s).Count() + 1;
                    var linesCount = fileMaster.LinesCount;

                    var description = statementReferenceMaster.First(x => x.BaseCommandId == 19).BusinessName;

                    var internalCall = (from s in statementReferenceMaster where s.BaseCommandId == 5 select s).Count();
                    var refFileIds = new List<int>();
                    var addedMissings = new List<string>();
                    var calledExternalList = new List<string>();
                    foreach (var eCall in callExternal)
                    {
                        string str;
                        int refFileId = eCall.ReferenceFileId;

                        if (addedMissings.Any(m => m == eCall.OriginalStatement)) continue;

                        addedMissings.Add(eCall.OriginalStatement);

                        if (refFileId == 0)
                        {
                            str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                  "cursor: pointer; text-decoration: none;'>" + eCall.OriginalStatement +
                                  " [ Missing ]</a>";
                            calledExternalList.Add(str);
                            continue;
                        }

                        var refFileMaster = fileMasters.FirstOrDefault(x => x.FileId == refFileId);
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
                    string cExternal = string.Join("<li>", calledExternalList);

                    // calledFrom


                    var iDescriptorList = new List<string>();
                    var regEx = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);

                    var universeDescriptor = (from u in allUniverseDescriptor
                                              where u.StatementString.Contains("SUBR")
                                              select u).ToList();

                    foreach (var uDescriptor in universeDescriptor)
                    {
                        string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var stringLstData = uDescriptor.StatementString.Split(';').ToList();
                        // var dData = new List<string>();
                        foreach (var lstData in stringLstData)
                        {
                            // var regex = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);
                            var allGroups = regEx.Match(lstData).Groups;
                            int groupCnt = -1;
                            foreach (Group group in allGroups)
                            {
                                groupCnt++;
                                if (groupCnt == 0) continue;
                                string groupName = group.Value;
                                string eName = groupName.Replace("'", "");
                                var subRoutineFile =
                                (from f in fileMasters
                                 where Path.GetFileNameWithoutExtension(f.FilePath) == eName && f.FileTypeExtensionId == 17
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

                    var addedFileId = new List<int>();
                    var calledFromList = new List<string>();

                    var iDescriptorExternalCall =
                        (from d in iDescMatchDesc where d.FileDescriptors.FileId == file.FileId select d.Descriptors)
                        .ToList().FirstOrDefault();

                    if (iDescriptorExternalCall != null)
                    {
                        foreach (var eDescriptor in iDescriptorExternalCall)
                        {
                            // var iDescriptorId = eDescriptor.DescriptorId;
                            var completeName = eDescriptor.CompleteName;
                            var str = "<a href='javascript:void(0);'>" + completeName + "</a>";
                            calledExternalList.Add(str);
                        }
                    }
                    foreach (var uObj in calledFrom)
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
                    string cFrom = string.Join("<li>", calledFromList);
                    var callFormFinal = calledFromList.Count > 0
                        ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + fileMaster.FileId +
                          ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                          calledFromList.Count + " Called From </a> <div style='display: none;' id='calledFrom_" +
                          fileMaster.FileId + "'>" + cFrom + " </div> " : "-";
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
                                var str = index + ")&nbsp;<a href='javascript:void(0);'" +
                                          " style='color: black; font-size: 14px; cursor: default; line-height:22px;'>" +
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
                        if (iDescriptorList.Any())
                        {
                            int callFormCnt;
                            if (callFormFinal == "-")
                            {
                                callFormCnt = iDescriptorList.Count;
                                callFormFinal =
                                    "<a href='javascript:void(0);' onclick='showData(calledFrom_" + fileMaster.FileId +
                                    ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                    callFormCnt +
                                    " Called From </a> <div style='display: none;' id='calledFrom_" +
                                    fileMaster.FileId +
                                    "'><div style='height: 10px;margin-bottom: 20px;'><h4> I-Desc </h4></div> " +
                                    iDescrt + "  </div> ";
                            }
                            else
                            {
                                callFormCnt = calledFromList.Count + iDescriptorList.Count;
                                callFormFinal = calledFromList.Count > 0
                                    ? "<a href='javascript:void(0);' onclick='showData(calledFrom_" + fileMaster.FileId +
                                      ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                                      callFormCnt +
                                      " Called From </a> <div style='display: none;' id='calledFrom_" +
                                      fileMaster.FileId + "'>" + cFrom +
                                      " <div style='height: 10px;margin-bottom: 20px; margin-top: 20px;'><h4> I-Desc </h4></div> " +
                                      iDescrt + "  </div>"
                                    : "-";
                            }
                        }
                    }

                    var inventoryDetails = new ProjectInventory // InventoryReport
                    {
                        ObjectName =
                            "<pre><a href='javascript:void(0);' style='color: blue; font-size: 14px; text-decoration: underline;' " +
                            " onclick='includeStateDialog(" + fileMaster.FileId + ");'>" + fileMaster.FileName + "</a></pre>",
                        Description = description,
                        ExtenstionType = fileMaster.FileTypeExtensionReference.FileTypeName,
                        LoC = linesCount,
                        Complexity = complexity,
                        InternalCall = internalCall,
                        // ExternalCall = externalCall,
                        ExternalCall = callExternal.Count > 0
                            ? "<a href='javascript:void(0);' onclick='showData(dvCExternal_" + fileMaster.FileId +
                              ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                              callExternal.Count + " Call External(s) </a> <div style='display: none;' id='dvCExternal_" +
                              fileMaster.FileId + "'>" + cExternal + " </div> "
                            : "-",
                        UsesObjects = "-",
                        UsesReports = "-",
                        UsesQueries = "-",
                        UsesEntities = "-",
                        ParticipateInWorkflow = "-",
                        //ParticipateInWorkflow = allWorkFlowsTempList.Count > 0
                        //    ? "<a href='javascript:void(0);' onclick='showData(dvPartInWork_" + node.Id +
                        //      ")' style='color: blue; font-size: 14px; text-decoration: underline;'>" +
                        //      allWorkFlowsTempList.Count +
                        //      " Workflow(s) </a> <div style='display: none;' id='dvPartInWork_" + node.Id + "'>" +
                        //      partInWorkflow + " </div> "
                        //    : "-",
                        CalledFrom = callFormFinal,
                        ProjectId = projectMaster.ProjectId,
                        SolutionId = projectMaster.SolutionId ?? 2,
                        FileId = fileMaster.FileId,
                        CallingTo = null
                    };

                }

                foreach (var fileMaster in fileMasters.Where(f => f.FileTypeExtensionId == 9))
                {
                    // if (fileMaster.FileId != 181) continue;
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
                                            where x.BaseCommandId == 6 && !string.IsNullOrEmpty(x.ClassCalled)
                                            select x)
                                            .ToList();
                    var refFileIds = new List<int>();
                    var addedMissings = new List<string>();
                    var addedMissingCalls = new List<string>();
                    foreach (var eCall in externalCallList)
                    {
                        string str;
                        int refFileId = eCall.ReferenceFileId;
                        if (addedMissings.Any(m => m == eCall.OriginalStatement)) continue;
                        addedMissings.Add(eCall.OriginalStatement);

                        var originalStatement = eCall.OriginalStatement;

                        var nameValue = new NameValue();
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

                foreach (var fileMaster in fileMasters.Where(f => f.FileTypeExtensionId == 17))
                {

                }

                foreach (var fileMaster in fileMasters.Where(f => f.FileTypeExtensionId == 12))
                {

                }
                return Ok(projectMaster);
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
                                ReplacementName = ""
                                // ProjectId = projectMaster.ProjectId
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
                Console.WriteLine(
                    "====================================================================================\n");
                Console.WriteLine(
                    "Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
                Console.WriteLine();
                Console.WriteLine(
                    "====================================================================================\n");
                stringBuilder.Append(dashLine);
                stringBuilder.AppendLine("Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
                LogMessage.WriteLogMessage(stringBuilder);

                return Ok("Data dictionary data uploaded successfully for Project: " + projectMaster.ProjectName);
            }
        }

        [HttpGet]
        public async Task<List<string>> CallExternal(int projectId) // , int fileId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);
                var calledExternalList = new List<string>();
                foreach (var allFile in allFiles.Where(x => x.FileTypeExtensionId == 10))
                {
                    int fileId = allFile.FileId;
                    var fileMaster = _codeVortoService.FileMasterRepository.GetItem(fileId);
                    var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetAllListItems(x => x.FileId == fileId && x.ProjectId == projectId).ConfigureAwait(false);
                    var externalCallList = (from s in statementReferenceMaster
                                            where s.BaseCommandId == 6 && !string.IsNullOrEmpty(s.ClassCalled)
                                            select s).ToList();

                    var refFileIds = new List<int>();
                    var addedMissings = new List<string>();
                    var statementRegEx = new Regex(@"^(SELECT\s+|SSELECT\s+|LIST\s+|DOWNLOAD\s+|HEADING\s+|SORT\s+)",
                        RegexOptions.Singleline);
                    foreach (var eCall in externalCallList)
                    {
                        string str;
                        int refFileId = eCall.ReferenceFileId;

                        if (addedMissings.Any(m => m == eCall.OriginalStatement)) continue;

                        addedMissings.Add(eCall.OriginalStatement);

                        if (refFileId == 0)
                        {
                            str = "<a href='javascript:void(0);' style='color: red; font-size: 14px; " +
                                  "cursor: pointer; text-decoration: none;'>" + eCall.OriginalStatement +
                                  " [ Missing ]</a>";
                            calledExternalList.Add(str);
                            continue;
                        }

                        var refFileMaster = _codeVortoService.FileMasterRepository.GetItem(refFileId);
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

                    // i-descriptor
                    var iDescMatchDesc = new List<LstObject>();
                    var universeDescriptorList = new List<UniverseDescriptorList>();

                    var allUniverseDescriptor = await _codeVortoService.UniverseDescriptorRepository
                        .GetAllListItems(x => x.ProjectId == projectId).ConfigureAwait(false);
                    var allDataDependancies = await _codeVortoService.DataDependencyRepository
                        .GetAllListItems(d => d.ProjectId == projectId).ConfigureAwait(false);
                    var viewSourceMaster = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(v => v.FileId == fileMaster.FileId).ConfigureAwait(false);

                    if (string.IsNullOrEmpty(viewSourceMaster?.SourceWithoutComments)) return new List<string>();

                    var contentsWithoutComment = viewSourceMaster.SourceWithoutComments.Split('\n');
                    var allStatements = (from a in contentsWithoutComment
                                         where statementRegEx.IsMatch(a)
                                         select a).ToList();
                    if (!allStatements.Any()) return new List<string>();

                    var lstDescriptors = new List<Descriptors>();
                    var dataEntity = (from d in allDataDependancies where d.FileId == fileId select d)
                        .ToList();
                    var universeDesc = (from d in allUniverseDescriptor
                                        where dataEntity.Any(e => d.Entity == e.Entity)
                                        select d).ToList();
                    foreach (var uDescriptor in universeDesc)
                    {
                        string spName = uDescriptor.StoredProcedureName.Trim();
                        if (spName.StartsWith("*") || spName.StartsWith(".") || spName.Contains("*")) continue;
                        string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var iDescRegEx = new Regex("(\\s" + spName + "\\s)", RegexOptions.Singleline);
                        bool isPresent = allStatements.Any(s => iDescRegEx.IsMatch(s));
                        if (!isPresent) continue;

                        // Here we need find index position of entity and spName (Stored Procedure Name).
                        // Index of entity must be less than spName.
                        // int entityNameLn = uDescriptor.Entity.Length;
                        // int spNameLn = spName.Length;
                        // Get actual matching statement
                        var statementStringList = allStatements.FindAll(s => iDescRegEx.IsMatch(s));
                        foreach (var statementString in statementStringList)
                        {
                            int indexOfEntity = statementString
                                .IndexOf(uDescriptor.Entity, StringComparison.CurrentCultureIgnoreCase);
                            int indexOfSpName = statementString
                                .IndexOf(spName, StringComparison.CurrentCultureIgnoreCase);

                            // If index of entity name is greater than index of spName, then continue.
                            if (indexOfEntity > indexOfSpName) continue;

                            lstDescriptors.Add(new Descriptors
                            {
                                CompleteName = entityName,
                                DescriptorId = uDescriptor.DescriptorId,
                                SpName = spName
                            });
                        }
                    }
                    if (lstDescriptors.Any())
                    {
                        var file = new FileDescriptors
                        {
                            FileId = fileId,
                            FileName = fileMaster.FileName
                        };
                        iDescMatchDesc.Add(new LstObject
                        {
                            Descriptors = lstDescriptors,
                            FileDescriptors = file
                        });
                    }

                    var regEx = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);

                    var universeDescriptor = (from u in allUniverseDescriptor
                                              where u.StatementString.Contains("SUBR")
                                              select u).ToList();

                    foreach (var uDescriptor in universeDescriptor)
                    {
                        string entityName = uDescriptor.Entity + " - " + uDescriptor.StoredProcedureName;
                        var stringLstData = uDescriptor.StatementString.Split(';').ToList();
                        // var dData = new List<string>();
                        foreach (var lstData in stringLstData)
                        {
                            // var regex = new Regex(@"SUBR\(\'(.*?(?=))\'", RegexOptions.Singleline);
                            var allGroups = regEx.Match(lstData).Groups;
                            int groupCnt = -1;
                            foreach (Group group in allGroups)
                            {
                                groupCnt++;
                                if (groupCnt == 0) continue;
                                string groupName = group.Value;
                                string eName = groupName.Replace("'", "");
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
                    var iDescriptorExternalCall =
                        iDescMatchDesc.Where(d => d.FileDescriptors.FileId == fileId).Select(d => d.Descriptors)
                        .First();
                    if (iDescriptorExternalCall == null) continue;

                    foreach (var eDescriptor in iDescriptorExternalCall)
                    {
                        // var iDescriptorId = eDescriptor.DescriptorId;
                        var completeName = eDescriptor.CompleteName;
                        var str = "<a href='javascript:void(0);'>" + completeName + "</a>";
                        calledExternalList.Add(str);
                    }

                }
                return calledExternalList;
            }
        }

        [HttpGet]
        public async Task<List<string>> CalledFrom(int projectId, int fileId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var calledFrom = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(x => x.ReferenceFileId == fileId).ConfigureAwait(false);
                var calledFromList = new List<string>();
                if (!calledFrom.Any()) return calledFromList;

                return calledFromList;
            }
        }
    }
}

