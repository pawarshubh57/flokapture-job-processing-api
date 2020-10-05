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
using System.Data;
using DataAccessLayer;
using System.Configuration;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class UniverseProcessingController : ApiController
    {
        private ICodeVortoService _codeVortoService;
        private readonly List<string> _finalStringWorkStor = new List<string>();
        private int _flag;
        string _tempStrNoPeriod = string.Empty;
        public List<string> LstLanguageKeywords = new List<string>();

        public UniverseProcessingController()
        {
        }

        public UniverseProcessingController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessUniverseProject(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectConfigFilesGeneralRepository = new GeneralRepository<ProjectConfigMaster>(new AppDbContext());
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ?? extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));

                string projectPath = projectDetails.PhysicalPath;
                List<string> directoryList = new List<string> { projectPath };
                var ignoredFile = await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId);
                var projectImpFiles = await projectConfigFilesGeneralRepository.GetAllItems(p => p.ConfigFileId != 2);

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
                        int extensionId = fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                        FileMaster fileMaster = new FileMaster
                        {
                            FileId = 0,
                            FileName = fileName,
                            FilePath = currentFile,
                            FileTypeExtensionId = extensionId,
                            ProjectId = projectId
                        };
                        lstFileMasters.Add(fileMaster);
                        // Insert one by one...
                        // await _codeVortoService.FileMasterRepository.AddNewItem(fileMaster);
                    }
                    // Bulk Insert
                    // EntityFramework.BulkInsert.ProviderFactory.Register<BulkInsertProvider>("MySql.Data.MySqlConnection");
                    await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters);
                }
                //return Ok(projectId);
                IHttpActionResult processResult = await ParseProjectFilesUniverse(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProjectFilesUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Universe File Parsing (PRGRAMS, JCL, TEXT files)

                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);

                IEnumerable<FileMaster> copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllItems()
                    .ContinueWith(t => { var result = t.Result; return result.ToList(); });
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);

                var findIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Find Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                //var beginCaseConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                //var caseConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                //var caseConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);

                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 5);

                string tempInsertToStr = string.Empty;
                var projectPath = projectDetails.PhysicalPath.ToString();
                var projectName = string.Empty;
                string[] stmtprojectPath = projectPath.Split('\\');
                if (stmtprojectPath.Length > 0)
                {
                    projectName = stmtprojectPath[stmtprojectPath.Length - 1].ToString();
                }

                foreach (var file in copyOfFileMaster)
                {
                    char[] TRIM_CHARACTER = { ' ', '0', '+' };
                    // Define the list for all sections.
                    List<string> lstAllLines = new List<string>();
                    //List<string> textLineWS = new List<string>();
                    //List<string> textLineDD = new List<string>();
                    //List<string> textLineFC = new List<string>();
                    List<string> textLineIP = new List<string>();
                    List<string> textLinePD = new List<string>();
                    List<string> textComment = new List<string>();
                    List<string> textBusinessName = new List<string>();

                    // Read the all data in the file.
                    lstAllLines = File.ReadAllLines(file.FilePath).ToList<string>();

                    if (file.FileTypeExtensionId == 9)
                    {
                        #region PROGRAM File processing

                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                //--------------------------------INITIALIZATION PART ----------------------------------
                                List<int> lstFirstIndexIP =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where((x => ((x.Str.StartsWith("*") == false))))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                List<int> lstlastIndexIP =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("BEGIN MAINLINE".ToUpper())
                                            && ((x.Str.StartsWith("*") == true)))
                                        .Select(x => x.Index)
                                        .ToList<int>();

                                if (lstlastIndexIP.Count == 0)
                                {
                                    lstlastIndexIP =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                            .Where(x => x.Str.Contains("MAINLINE ROUTINE".ToUpper())
                                                && ((x.Str.StartsWith("*") == true)))
                                            .Select(x => x.Index)
                                            .ToList<int>();
                                }

                                if (lstlastIndexIP.Count == 0)
                                {
                                    lstlastIndexIP =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                            .Where(x => x.Str.Contains("* MAINLINE".ToUpper())
                                                && ((x.Str.StartsWith("*") == true)))
                                            .Select(x => x.Index)
                                            .ToList<int>();
                                }

                                if (lstFirstIndexIP.Count != 0 && lstlastIndexIP.Count != 0)
                                {
                                    lstlastIndexIP[0] = lstlastIndexIP[0] - 1;
                                    lstFirstIndexIP[0] = lstFirstIndexIP[0] + 1;
                                    textLineIP =
                                        lstAllLines.Skip(lstFirstIndexIP[0])
                                            .Take(lstlastIndexIP[0] - lstFirstIndexIP[0])
                                            .ToList<string>();
                                }


                                //-------------------------------- BEGIN MAINLINE------------------------------------------
                                List<int> lstFirstIndexBM =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("BEGIN MAINLINE".ToUpper())
                                            && ((x.Str.StartsWith("*") == true)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                if (lstFirstIndexBM.Count == 0)
                                {
                                    lstFirstIndexBM =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                            .Where(x => x.Str.Contains("MAINLINE ROUTINE".ToUpper())
                                                && ((x.Str.StartsWith("*") == true)))
                                            .Select(x => x.Index)
                                            .ToList<int>();
                                }
                                if (lstFirstIndexBM.Count == 0)
                                {
                                    lstFirstIndexBM =
                                        lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                            .Where(x => x.Str.Contains("* MAINLINE".ToUpper())
                                                && ((x.Str.StartsWith("*") == true)))
                                            .Select(x => x.Index)
                                            .ToList<int>();
                                }
                                // instead of above -> try to read the file till end.
                                int lstlastIndexBM = lstAllLines.Count;
                                if (lstFirstIndexBM.Count != 0 && lstlastIndexBM != 0)
                                {
                                    textLinePD =
                                        lstAllLines.Skip(lstFirstIndexBM[lstFirstIndexBM.Count - 1])
                                            .Take(lstlastIndexBM - lstFirstIndexBM[lstFirstIndexBM.Count - 1])
                                            .ToList<string>();
                                }


                                #region INITIALIZATION PROCESSING

                                if (textLineIP.Count > 0)
                                {
                                    List<string> lstringWorkingStorage = new List<string>();
                                    textLineIP.RemoveAll(stringToCheck => stringToCheck.Trim().StartsWith("*"));
                                    List<string> lstCopyWS = textLineIP.Where(stringToCheck => stringToCheck.Contains("COPY")).ToList();
                                    List<string> lstCopyExecInclude =
                                        textLineIP.Where(stringToCheck => stringToCheck.Contains("EXEC SQL INCLUDE ")).ToList();
                                    textLineIP.RemoveAll(stringToCheck => stringToCheck.Contains("EXEC SQL INCLUDE "));

                                    #region Declaration Part

                                    string currentSentance;
                                    if ((lstCopyExecInclude.Count == 0))
                                    {
                                        lstCopyExecInclude = textLineIP.Where(stringToCheck => stringToCheck.Contains(" INCLUDE ")).ToList();
                                        textLineIP.RemoveAll(stringToCheck => stringToCheck.Contains(" INCLUDE "));

                                        for (int iWS = 0; iWS < textLineIP.Count; iWS++)
                                        {
                                            string strConditions = string.Empty;
                                            bool lineDone = false;
                                            currentSentance = textLineIP[iWS].ToString().Trim();
                                            string strComments = null;

                                            if (!currentSentance.StartsWith("*") || !currentSentance.StartsWith(""))
                                            {
                                                if (currentSentance.Contains("; "))
                                                {
                                                    string[] splitVal = currentSentance.Split(';');
                                                    if (splitVal.Length > 1)
                                                    {
                                                        currentSentance = splitVal[0].ToString().Trim();
                                                        strComments = splitVal[1].ToString().Trim();
                                                    }
                                                }

                                                #region Replacing the LOCATE statements
                                                if (currentSentance.Trim().StartsWith("LOCATE "))
                                                {
                                                    currentSentance = currentSentance.Replace("LOCATE", "FIND").Replace("THEN", "INTO FOUND");
                                                }
                                                #endregion

                                                #region Insert Find statement logic
                                                var findStart = findIndicatorStart.Find(x => x.StartIndicator != null);
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith(findStart.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = findStart.BaseCommandId,
                                                                PrimaryCommandId = findStart.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            //continue;
                                                            currentSentance = "IF FOUND";
                                                        }

                                                    }

                                                }
                                                #endregion

                                                #region Insert MATREADU and READVU and READ and OPEN and READLIST statement

                                                if (currentSentance.Trim().StartsWith("MATREADU ") || currentSentance.Trim().StartsWith("READVU ") || currentSentance.Trim().StartsWith("READV "))
                                                {
                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = currentSentance,
                                                        OriginalStatement = currentSentance,
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = strComments
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    currentSentance = "IF RECORD-ID-EXISTS";

                                                }
                                                if (currentSentance.Trim().StartsWith("READ "))
                                                {
                                                    strConditions = currentSentance;
                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = strComments
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    currentSentance = "IF FOUND " + currentSentance.Split(' ').LastOrDefault();
                                                }
                                                if (currentSentance.Trim().StartsWith("OPEN "))
                                                {
                                                    strConditions = currentSentance;
                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = strComments
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    currentSentance = "IF NOT-SUCCESS " + currentSentance.Split(' ').LastOrDefault();
                                                }
                                                if (currentSentance.Trim().StartsWith("READLIST "))
                                                {
                                                    strConditions = currentSentance;
                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = strComments
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    currentSentance = "IF NOT-FOUND " + currentSentance.Split(' ').LastOrDefault();
                                                }
                                                #endregion

                                                #region Insert If statement logic
                                                var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith(ifStart.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = ifStart.BaseCommandId,
                                                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
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
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (currentSentance.StartsWith(ifEnd.StartIndicator))
                                                    {
                                                        if (!currentSentance.StartsWith("*"))
                                                        {
                                                            if (!currentSentance.StartsWith("END ELSE") && !currentSentance.StartsWith("END CASE") && !currentSentance.StartsWith("END."))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = file.FileId,
                                                                    ResolvedStatement = currentSentance,
                                                                    OriginalStatement = currentSentance,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = ifEnd.BaseCommandId,
                                                                    PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                    ParsedOrNot = "1",
                                                                    ProjectId = projectId,
                                                                    StatementComment = strComments
                                                                };
                                                                await
                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                continue;
                                                            }

                                                        }
                                                    }
                                                }
                                                #endregion

                                                #region Insert Call External (CALL) statement logic
                                                var execCallStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith("CALL "))
                                                        {
                                                            if (currentSentance.StartsWith(execCallStart.StartIndicator))
                                                            {
                                                                string[] stmtSplit = currentSentance.Replace("CALL", "").Trim().Split('(');

                                                                if (stmtSplit.Length > 0)
                                                                {
                                                                    int fileExtension = 0;
                                                                    string classNamed = "";
                                                                    string checkSplitVal = "";

                                                                    if (stmtSplit.Length > 1)
                                                                    {
                                                                        checkSplitVal = stmtSplit[0].Trim().ToString().Replace("@", "");
                                                                    }
                                                                    //else
                                                                    //{
                                                                    //    checkSplitVal = stmtSplit[1].Trim().ToString();
                                                                    //}

                                                                    if (checkSplitVal.Length > 0)
                                                                    {

                                                                        string className = checkSplitVal.ToString();
                                                                        var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                        if (callExtExpandedCode.Count == 0)
                                                                        {
                                                                            fileExtension = 9;
                                                                            callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                            classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                            string strSplit = classNamed.Split('\\').Last();
                                                                            classNamed = classNamed.Replace(strSplit, "").Replace("\\", ".") + className;
                                                                        }
                                                                        else
                                                                        {
                                                                            classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                        }

                                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                                        {
                                                                            FileId = file.FileId,
                                                                            ResolvedStatement = currentSentance,
                                                                            OriginalStatement = currentSentance,
                                                                            //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                            ClassCalled = classNamed,
                                                                            MethodName = null,
                                                                            DataOrObjectType = null,
                                                                            MethodCalled = null,
                                                                            VariableNameDeclared = null,
                                                                            //BaseCommandId = execCallStart.BaseCommandId,
                                                                            //PrimaryCommandId = execCallStart.PrimaryReferenceId,
                                                                            BaseCommandId = 0,
                                                                            PrimaryCommandId = 0,
                                                                            ProjectId = projectId,
                                                                            StatementComment = strComments
                                                                        };

                                                                        await
                                                              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                    }
                                                                }
                                                            }

                                                            continue;
                                                        }
                                                    }
                                                }

                                                #endregion

                                                #region Insert Call Internal (GOSUB) statement logic
                                                var callInternalStart = callInternalIndicationStart.Find(x => x.StartIndicator != null);

                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith(callInternalStart.StartIndicator))
                                                        {

                                                            string[] stmtSplit = currentSentance.Split(' ');

                                                            if (stmtSplit.Length > 1)
                                                            {
                                                                var performMethodCalled = stmtSplit[1].ToString().Trim();
                                                                if (string.IsNullOrEmpty(performMethodCalled))
                                                                {
                                                                    performMethodCalled = stmtSplit[2].ToString().Trim();
                                                                }

                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = file.FileId,
                                                                    ResolvedStatement = currentSentance,
                                                                    OriginalStatement = currentSentance,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = performMethodCalled,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = callInternalStart.BaseCommandId,
                                                                    PrimaryCommandId = callInternalStart.PrimaryReferenceId,
                                                                    ProjectId = projectId,
                                                                    StatementComment = strComments
                                                                };
                                                                await
                                                          _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                continue;
                                                            }


                                                        }
                                                    }


                                                }

                                                #endregion

                                                #region Insert Loop start [FOR] statement logic
                                                var loopStart = loopIndicatorStart.Find(x => x.StartIndicator != null);

                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith("FOR "))
                                                        {
                                                            if (currentSentance.StartsWith(loopStart.StartIndicator))
                                                            {
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = file.FileId,
                                                                    ResolvedStatement = currentSentance,
                                                                    OriginalStatement = currentSentance,
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = loopStart.BaseCommandId,
                                                                    PrimaryCommandId = loopStart.PrimaryReferenceId,
                                                                    ProjectId = projectId,
                                                                    StatementComment = strComments
                                                                };

                                                                await
                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                continue;
                                                            }
                                                        }
                                                    }


                                                }

                                                #endregion

                                                #region Insert Loop end [NEXT] statement logic
                                                var loopEnd = loopIndicatorEnd.Find(x => x.StartIndicator != null);

                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.StartsWith(loopEnd.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = loopEnd.BaseCommandId,
                                                                PrimaryCommandId = loopEnd.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }
                                                    }


                                                }

                                                #endregion

                                                #region Insert normal statements logic
                                                if (lineDone) continue;
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (currentSentance.Trim().StartsWith("BEGIN CASE"))
                                                        {
                                                            continue;
                                                            //var statementReferenceMaster = new StatementReferenceMaster
                                                            //{
                                                            //    FileId = file.FileId,
                                                            //    ResolvedStatement = currentSentance,
                                                            //    OriginalStatement = currentSentance,
                                                            //    ClassCalled = null,
                                                            //    MethodName = null,
                                                            //    DataOrObjectType = null,
                                                            //    MethodCalled = null,
                                                            //    VariableNameDeclared = null,
                                                            //    BaseCommandId = 1,
                                                            //    PrimaryCommandId = 33,
                                                            //    ProjectId = projectId,
                                                            //    StatementComment = strComments
                                                            //};
                                                            //await
                                                            //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }
                                                        else if (currentSentance.Trim().StartsWith("CASE "))
                                                        {
                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = 1,
                                                                PrimaryCommandId = 33,
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }
                                                        else if (currentSentance.Trim().StartsWith("END CASE"))
                                                        {
                                                            continue;
                                                            //var statementReferenceMaster = new StatementReferenceMaster
                                                            //{
                                                            //    FileId = file.FileId,
                                                            //    ResolvedStatement = currentSentance,
                                                            //    OriginalStatement = currentSentance,
                                                            //    ClassCalled = null,
                                                            //    MethodName = null,
                                                            //    DataOrObjectType = null,
                                                            //    MethodCalled = null,
                                                            //    VariableNameDeclared = null,
                                                            //    BaseCommandId = 2,
                                                            //    PrimaryCommandId = 34,
                                                            //    ProjectId = projectId,
                                                            //    StatementComment = strComments
                                                            //};
                                                            //await
                                                            //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }
                                                        else if (currentSentance.Trim().StartsWith("MATREADU ") || currentSentance.Trim().StartsWith("READVU "))
                                                        {
                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = 1,
                                                                PrimaryCommandId = 33,
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }
                                                        else
                                                        {
                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                PrimaryCommandId = 0,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }
                                                    }

                                                    #region Comment Section 07/27/2016
                                                    //if (iWS == textLineIP.Count - 1)
                                                    //{
                                                    //    var stmtReferenceMaster = new StatementReferenceMaster
                                                    //    {
                                                    //        FileId = file.FileId,
                                                    //        ResolvedStatement = "END",
                                                    //        OriginalStatement = "END",
                                                    //        ClassCalled = null,
                                                    //        MethodName = null,
                                                    //        DataOrObjectType = null,
                                                    //        MethodCalled = null,
                                                    //        VariableNameDeclared = null,
                                                    //        BaseCommandId = 9,
                                                    //        PrimaryCommandId = 31,
                                                    //        ProjectId = projectId
                                                    //    };

                                                    //    await
                                                    //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                    //}
                                                    #endregion
                                                }
                                                #endregion

                                            }

                                        }
                                    }
                                    #endregion

                                    #region Parsing Begin Mainline Section -- Start

                                    if (textLinePD.Count > 0)
                                    {
                                        #region Insert Class logic
                                        var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                        var statementReferenceMasterClass = new StatementReferenceMaster
                                        {
                                            FileId = file.FileId,
                                            ResolvedStatement = file.FileName.Replace(".pgm", "").ToUpper(),
                                            OriginalStatement = file.FileName.Replace(".pgm", "").ToUpper(),
                                            ClassCalled = null,
                                            MethodName = null,
                                            DataOrObjectType = null,
                                            MethodCalled = null,
                                            VariableNameDeclared = null,
                                            //ClassNameDeclared = file.FilePath.Substring(2, file.FilePath.Length - 2).Trim().Replace("\\", "."),
                                            //ClassNameDeclared = projectName + file.FilePath.Replace(".cbl", "").Replace(projectPath, "").Trim().Replace("\\", "."),
                                            ClassNameDeclared = projectName + file.FilePath.Replace(projectPath, "").Trim().Replace("\\", "."),
                                            PrimaryCommandId = classStart.PrimaryReferenceId,
                                            BaseCommandId = classStart.BaseCommandId,
                                            ProjectId = projectId,
                                            StatementComment = null,
                                            BusinessName = lstAllLines[0].ToString().Replace("*", "").TrimStart()
                                        };
                                        await
                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClass);

                                        #endregion

                                        int iMethodEnd = 0;
                                        int iLastMethodEnd = 0;

                                        #region Collect the Business Name (Above the comments of method Name)
                                        textBusinessName = textLinePD.Where(stringToCheck => stringToCheck.StartsWith("* ")).ToList();
                                        textBusinessName.RemoveAll(x => x.TrimStart().StartsWith("**"));
                                        int iBusinessName = 0;
                                        #endregion

                                        // Remove the list items which starts with * as they are comments.
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("*"));

                                        var fileLines = textLinePD.ToArray().CaseAdjustmentUniverse();
                                        textLinePD = fileLines.ToList();

                                        for (int iPD = 0; iPD < textLinePD.Count - 1; iPD++)
                                        {
                                            string strConditions = string.Empty;
                                            string strComments = null;
                                            bool lineDone = false;
                                            int cntSpaces = 0;
                                            currentSentance = textLinePD[iPD].ToString().Trim();

                                            if (currentSentance.Contains(";*") || currentSentance.Contains("; *"))
                                            {
                                                string[] splitVal = currentSentance.Split(';');
                                                if (splitVal.Length > 1)
                                                {
                                                    currentSentance = splitVal[0].ToString().Trim();
                                                    strComments = splitVal[1].ToString().Trim();
                                                }
                                            }

                                            #region Insert Only Method logic
                                            var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                cntSpaces = GetLeadingWhitespaceLength(textLinePD[iPD].ToString());
                                            }

                                            if ((cntSpaces == 0) && ((textLinePD[iPD].ToString().Trim().StartsWith("-")) == false) && ((textLinePD[iPD].ToString().Trim().EndsWith("*'.")) == false))
                                            {
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (Regex.IsMatch(currentSentance, @"^\d"))
                                                    {
                                                        currentSentance = Regex.Replace(currentSentance, @"[^0-9\.]?", string.Empty);
                                                        if (cntSpaces == Convert.ToInt32(methodStart.StartIndicator))
                                                        {
                                                            iBusinessName = iBusinessName + 1;

                                                            if (iMethodEnd > 0)
                                                            {
                                                                var methodEnd = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = file.FileId,
                                                                    ResolvedStatement = "END",
                                                                    OriginalStatement = "END",
                                                                    ClassCalled = null,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = 9,
                                                                    PrimaryCommandId = 42,
                                                                    ProjectId = projectId,
                                                                    StatementComment = strComments
                                                                };

                                                                await
                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            }


                                                            var stmtReferenceMasterNew = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", ""),
                                                                OriginalStatement = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", ""),
                                                                ClassCalled = null,
                                                                MethodName = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", "").Replace(".", "").ToString(),
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = methodStart.BaseCommandId,
                                                                PrimaryCommandId = methodStart.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments,
                                                                BusinessName = textBusinessName[iBusinessName - 1].Replace("*", "").TrimStart()
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterNew);
                                                            iMethodEnd++;
                                                            continue;
                                                        }
                                                    }
                                                    else
                                                    {
                                                        if (iLastMethodEnd == 0)
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = "END",
                                                                OriginalStatement = "END",
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = 9,
                                                                PrimaryCommandId = 42,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            iLastMethodEnd++;
                                                        }
                                                    }
                                                }
                                            }

                                            #endregion

                                            #region Replacing the LOCATE statements
                                            if (currentSentance.Trim().StartsWith("LOCATE "))
                                            {
                                                currentSentance = currentSentance.Replace("LOCATE", "FIND").Replace("THEN", "INTO FOUND");
                                            }
                                            #endregion

                                            #region Insert Find statement logic
                                            var findStart = findIndicatorStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(findStart.StartIndicator))
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = findStart.BaseCommandId,
                                                            PrimaryCommandId = findStart.PrimaryReferenceId,
                                                            ProjectId = projectId,
                                                            StatementComment = strComments
                                                        };

                                                        await
                                                       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                        //continue;
                                                        currentSentance = "IF FOUND";
                                                    }

                                                }

                                            }
                                            #endregion

                                            #region Insert MATREADU and READVU and READ and OPEN and READLIST statement

                                            if (currentSentance.Trim().StartsWith("MATREADU ") || currentSentance.Trim().StartsWith("READVU ") || currentSentance.Trim().StartsWith("READV "))
                                            {
                                                var statementReferenceMaster = new StatementReferenceMaster
                                                {
                                                    FileId = file.FileId,
                                                    ResolvedStatement = currentSentance,
                                                    OriginalStatement = currentSentance,
                                                    ClassCalled = null,
                                                    MethodName = null,
                                                    DataOrObjectType = null,
                                                    MethodCalled = null,
                                                    VariableNameDeclared = null,
                                                    PrimaryCommandId = 0,
                                                    ProjectId = projectId,
                                                    StatementComment = strComments
                                                };
                                                await
                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                currentSentance = "IF RECORD-ID-EXISTS";

                                            }
                                            if (currentSentance.Trim().StartsWith("READ "))
                                            {
                                                strConditions = currentSentance;
                                                var statementReferenceMaster = new StatementReferenceMaster
                                                {
                                                    FileId = file.FileId,
                                                    ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    ClassCalled = null,
                                                    MethodName = null,
                                                    DataOrObjectType = null,
                                                    MethodCalled = null,
                                                    VariableNameDeclared = null,
                                                    PrimaryCommandId = 0,
                                                    ProjectId = projectId,
                                                    StatementComment = strComments
                                                };
                                                await
                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                currentSentance = "IF FOUND " + currentSentance.Split(' ').LastOrDefault();
                                            }
                                            if (currentSentance.Trim().StartsWith("OPEN "))
                                            {
                                                strConditions = currentSentance;
                                                var statementReferenceMaster = new StatementReferenceMaster
                                                {
                                                    FileId = file.FileId,
                                                    ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    ClassCalled = null,
                                                    MethodName = null,
                                                    DataOrObjectType = null,
                                                    MethodCalled = null,
                                                    VariableNameDeclared = null,
                                                    PrimaryCommandId = 0,
                                                    ProjectId = projectId,
                                                    StatementComment = strComments
                                                };
                                                await
                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                currentSentance = "IF NOT-SUCCESS " + currentSentance.Split(' ').LastOrDefault();
                                            }
                                            if (currentSentance.Trim().StartsWith("READLIST "))
                                            {
                                                strConditions = currentSentance;
                                                var statementReferenceMaster = new StatementReferenceMaster
                                                {
                                                    FileId = file.FileId,
                                                    ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                    ClassCalled = null,
                                                    MethodName = null,
                                                    DataOrObjectType = null,
                                                    MethodCalled = null,
                                                    VariableNameDeclared = null,
                                                    PrimaryCommandId = 0,
                                                    ProjectId = projectId,
                                                    StatementComment = strComments
                                                };
                                                await
                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                currentSentance = "IF NOT-FOUND " + currentSentance.Split(' ').LastOrDefault();
                                            }
                                            #endregion

                                            #region Insert If statement logic
                                            var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(ifStart.StartIndicator))
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = ifStart.BaseCommandId,
                                                            PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                            ProjectId = projectId,
                                                            StatementComment = strComments
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
                                            if (currentSentance.Length > 0)
                                            {
                                                if (currentSentance.StartsWith(ifEnd.StartIndicator))
                                                {
                                                    if (!currentSentance.StartsWith("*"))
                                                    {
                                                        if (!currentSentance.StartsWith("END ELSE") && !currentSentance.StartsWith("END CASE") && !currentSentance.StartsWith("END."))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = ifEnd.BaseCommandId,
                                                                PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                ParsedOrNot = "1",
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            continue;
                                                        }

                                                    }
                                                }
                                            }
                                            #endregion

                                            #region Insert Call External (CALL) statement logic
                                            var execCallStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith("CALL "))
                                                    {
                                                        if (currentSentance.StartsWith(execCallStart.StartIndicator))
                                                        {
                                                            string[] stmtSplit = currentSentance.Replace("CALL", "").Trim().Split('(');

                                                            if (stmtSplit.Length > 0)
                                                            {
                                                                int fileExtension = 0;
                                                                string classNamed = "";
                                                                string checkSplitVal = "";

                                                                if (stmtSplit.Length > 1)
                                                                {
                                                                    checkSplitVal = stmtSplit[0].Trim().ToString().Replace("@", "");
                                                                }
                                                                //else
                                                                //{
                                                                //    checkSplitVal = stmtSplit[1].Trim().ToString();
                                                                //}

                                                                if (checkSplitVal.Length > 0)
                                                                {

                                                                    string className = checkSplitVal.ToString();
                                                                    var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                    if (callExtExpandedCode.Count == 0)
                                                                    {
                                                                        fileExtension = 9;
                                                                        callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                        classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                        string strSplit = classNamed.Split('\\').Last();
                                                                        if (strSplit.Contains(".pgm"))
                                                                        {
                                                                            classNamed = classNamed.Replace(strSplit, "").Replace("\\", ".") + className + ".pgm";
                                                                        }
                                                                        else
                                                                        {
                                                                            classNamed = classNamed.Replace(strSplit, "").Replace("\\", ".") + className;
                                                                        }
                                                                    }
                                                                    else
                                                                    {
                                                                        classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                    }

                                                                    var stmtReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = currentSentance,
                                                                        OriginalStatement = currentSentance,
                                                                        //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                        ClassCalled = classNamed,
                                                                        MethodName = null,
                                                                        DataOrObjectType = null,
                                                                        MethodCalled = null,
                                                                        VariableNameDeclared = null,
                                                                        BaseCommandId = execCallStart.BaseCommandId,
                                                                        PrimaryCommandId = execCallStart.PrimaryReferenceId,
                                                                        ProjectId = projectId,
                                                                        StatementComment = strComments
                                                                    };

                                                                    await
                                                          _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                }
                                                            }
                                                        }

                                                        continue;
                                                    }
                                                }
                                            }

                                            #endregion

                                            #region Insert Call Internal (GOSUB) statement logic
                                            var callInternalStart = callInternalIndicationStart.Find(x => x.StartIndicator != null);

                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(callInternalStart.StartIndicator))
                                                    {

                                                        string[] stmtSplit = currentSentance.Split(' ');

                                                        if (stmtSplit.Length > 1)
                                                        {
                                                            var performMethodCalled = stmtSplit[1].ToString().Trim();
                                                            if (string.IsNullOrEmpty(performMethodCalled))
                                                            {
                                                                performMethodCalled = stmtSplit[2].ToString().Trim();
                                                            }

                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = performMethodCalled,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = callInternalStart.BaseCommandId,
                                                                PrimaryCommandId = callInternalStart.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };
                                                            await
                                                      _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }


                                                    }
                                                }


                                            }

                                            #endregion

                                            #region Insert Loop start [FOR] statement logic
                                            var loopStart = loopIndicatorStart.Find(x => x.StartIndicator != null);

                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith("FOR "))
                                                    {
                                                        if (currentSentance.StartsWith(loopStart.StartIndicator))
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = loopStart.BaseCommandId,
                                                                PrimaryCommandId = loopStart.PrimaryReferenceId,
                                                                ProjectId = projectId,
                                                                StatementComment = strComments
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }
                                                    }
                                                }


                                            }

                                            #endregion

                                            #region Insert Loop end [NEXT] statement logic
                                            var loopEnd = loopIndicatorEnd.Find(x => x.StartIndicator != null);

                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(loopEnd.StartIndicator))
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = loopEnd.BaseCommandId,
                                                            PrimaryCommandId = loopEnd.PrimaryReferenceId,
                                                            ProjectId = projectId,
                                                            StatementComment = strComments
                                                        };

                                                        await
                                                       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                        continue;
                                                    }
                                                }


                                            }

                                            #endregion

                                            #region Insert normal statements logic
                                            if (lineDone) continue;
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.Trim().StartsWith("BEGIN CASE"))
                                                    {
                                                        continue;
                                                        //var statementReferenceMaster = new StatementReferenceMaster
                                                        //{
                                                        //    FileId = file.FileId,
                                                        //    ResolvedStatement = currentSentance,
                                                        //    OriginalStatement = currentSentance,
                                                        //    ClassCalled = null,
                                                        //    MethodName = null,
                                                        //    DataOrObjectType = null,
                                                        //    MethodCalled = null,
                                                        //    VariableNameDeclared = null,
                                                        //    BaseCommandId = 1,
                                                        //    PrimaryCommandId = 33,
                                                        //    ProjectId = projectId,
                                                        //    StatementComment = strComments
                                                        //};
                                                        //await
                                                        //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    }
                                                    else if (currentSentance.Trim().StartsWith("CASE "))
                                                    {
                                                        var statementReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = 1,
                                                            PrimaryCommandId = 33,
                                                            ProjectId = projectId
                                                        };
                                                        await
                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    }
                                                    else if (currentSentance.Trim().StartsWith("END CASE"))
                                                    {
                                                        continue;

                                                        //var statementReferenceMaster = new StatementReferenceMaster
                                                        //{
                                                        //    FileId = file.FileId,
                                                        //    ResolvedStatement = currentSentance,
                                                        //    OriginalStatement = currentSentance,
                                                        //    ClassCalled = null,
                                                        //    MethodName = null,
                                                        //    DataOrObjectType = null,
                                                        //    MethodCalled = null,
                                                        //    VariableNameDeclared = null,
                                                        //    BaseCommandId = 2,
                                                        //    PrimaryCommandId = 34,
                                                        //    ProjectId = projectId,
                                                        //    StatementComment = strComments
                                                        //};
                                                        //await
                                                        //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    }
                                                    else
                                                    {
                                                        #region Comment Section 07/27/2016
                                                        //string[] stringArray = new string[] { "READ", "WRITE", "DELETE", "UPDATE", "RECEIVE", "SEND", "RETURN", "REWRITE", "READNEXT", "WRITEQ", "CLOSE", "OPEN" };

                                                        //if (stringArray.Any(strTemp => currentSentance.Trim().ToUpper().StartsWith(strTemp)))
                                                        //{
                                                        //    if (!currentSentance.EndsWith("."))
                                                        //    {
                                                        //        for (int iEndpoint = 0; iEndpoint < textLinePD.Count; iEndpoint++)
                                                        //        {
                                                        //            try
                                                        //            {
                                                        //                currentSentance = currentSentance + " " + textLinePD[iWS + 1].ToString().Trim();
                                                        //                textLinePD.RemoveAt(iWS + 1);
                                                        //                if (currentSentance.EndsWith("."))
                                                        //                {
                                                        //                    break;
                                                        //                }
                                                        //            }
                                                        //            catch (Exception ex)
                                                        //            {
                                                        //                ex.ToString();
                                                        //            }
                                                        //        }
                                                        //    }
                                                        //}
                                                        #endregion

                                                        var statementReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            PrimaryCommandId = 0,
                                                            ProjectId = projectId,
                                                            StatementComment = strComments
                                                        };
                                                        await
                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    }

                                                }

                                                #region Comment Section 07/27/2016
                                                //if (iWS == textLineIP.Count - 1)
                                                //{
                                                //    var stmtReferenceMaster = new StatementReferenceMaster
                                                //    {
                                                //        FileId = file.FileId,
                                                //        ResolvedStatement = "END",
                                                //        OriginalStatement = "END",
                                                //        ClassCalled = null,
                                                //        MethodName = null,
                                                //        DataOrObjectType = null,
                                                //        MethodCalled = null,
                                                //        VariableNameDeclared = null,
                                                //        BaseCommandId = 9,
                                                //        PrimaryCommandId = 31,
                                                //        ProjectId = projectId
                                                //    };

                                                //    await
                                                //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                //}
                                                #endregion
                                            }
                                            #endregion

                                            #region last method END logic
                                            //if (iPD == textLinePD.Count - 2)
                                            //{
                                            //    var stmtReferenceMaster = new StatementReferenceMaster
                                            //    {
                                            //        FileId = file.FileId,
                                            //        ResolvedStatement = "END",
                                            //        OriginalStatement = "END",
                                            //        ClassCalled = null,
                                            //        MethodName = null,
                                            //        DataOrObjectType = null,
                                            //        MethodCalled = null,
                                            //        VariableNameDeclared = null,
                                            //        BaseCommandId = 9,
                                            //        PrimaryCommandId = 42,
                                            //        ProjectId = projectId
                                            //    };

                                            //    await
                                            //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                            //}
                                            #endregion
                                        }

                                        #region Insert Method End logic
                                        if (iLastMethodEnd == 0)
                                        {
                                            var methodLastEnd = methodIndicationStart.Find(x => x.StartIndicator != null);
                                            var stmtReferenceMasterMethodEnd = new StatementReferenceMaster
                                            {
                                                FileId = file.FileId,
                                                ResolvedStatement = "END",
                                                OriginalStatement = "END",
                                                ClassCalled = null,
                                                MethodName = null,
                                                DataOrObjectType = null,
                                                MethodCalled = null,
                                                VariableNameDeclared = null,
                                                BaseCommandId = 9,
                                                PrimaryCommandId = 42,
                                                ProjectId = projectId,
                                                StatementComment = null
                                            };

                                            await
                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterMethodEnd);
                                        }
                                        #endregion

                                        #region Insert Class End logic
                                        var classEnd = callClassIndicatorEnd.Find(x => x.StartIndicator == null);
                                        var statementReferenceMasterClassEnd = new StatementReferenceMaster
                                        {
                                            FileId = file.FileId,
                                            ResolvedStatement = "END " + file.FileName.Replace(".pgm", "").ToUpper(),
                                            OriginalStatement = "END " + file.FileName.Replace(".pgm", "").ToUpper(),
                                            ClassCalled = null,
                                            MethodName = null,
                                            DataOrObjectType = null,
                                            MethodCalled = null,
                                            VariableNameDeclared = null,
                                            PrimaryCommandId = classEnd.PrimaryReferenceId,
                                            BaseCommandId = classEnd.BaseCommandId,
                                            ProjectId = projectId,
                                            StatementComment = null
                                        };
                                        await
                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClassEnd);
                                        #endregion
                                    }
                                    #endregion Parsing Mainline Section Section -- End

                                }

                                #endregion

                                #region -- Psuedo Code conversion code --
                                // TODO: check if setting flag is set to 1 before processing.
                                // FileMasterController objFileMaster = new FileMasterController();
                                // objFileMaster.FunctionToConvertToThePseudoCode(file.FileId, file.ProjectId, file.ProjectMaster.LanguageId);
                                #endregion

                            }
                            catch (Exception ex)
                            {
                                ex.ToString();
                            }

                        }
                        #endregion

                        #region INCLUDES File processing

                        try
                        {
                            var stmt = "$INSERT";
                            var insertStmtData = GetInsertStmtData(projectId, file.FileId, stmt);
                            if (insertStmtData.Count > 0)
                            {
                                for (int iInsertStmt = 0; iInsertStmt < insertStmtData.Count; iInsertStmt++)
                                {
                                    var insertStmt = insertStmtData[iInsertStmt].OriginalStatement;
                                    string[] splitInsert = insertStmt.Split('>');
                                    if (splitInsert.Length > 1)
                                    {
                                        string fileName = splitInsert.Last();

                                        #region Check the file is exit or not (12 is include file)
                                        var checkFileExistOrNot = GetFileCount(fileName, projectId, 12, 0);

                                        if (checkFileExistOrNot.Count > 0)
                                        {
                                            // Check the data against that file present or not.
                                            var checkFileDataExistOrNot = GetFileDataExitOrNot(projectId, checkFileExistOrNot[0].FileId);
                                            if (checkFileDataExistOrNot.Count == 0)
                                            {
                                                #region Parsing Logic of INCLUDES and SUBROUTINE files
                                                // Read the all data in the file.
                                                textLinePD = File.ReadAllLines(checkFileExistOrNot[0].FilePath).ToList<string>();
                                                if (checkFileExistOrNot[0].FileTypeExtensionId == 12)
                                                {
                                                    #region Parsing Begin INCLUDES -- Start

                                                    if (textLinePD.Count > 0)
                                                    {

                                                        int iMethodEnd = 0;
                                                        //int iLastMethodEnd = 0;

                                                        #region Collect the Business Name (Above the comments of method Name)
                                                        //textBusinessName = textLinePD.Where(stringToCheck => stringToCheck.StartsWith("* ")).ToList();
                                                        //textBusinessName.RemoveAll(x => x.TrimStart().StartsWith("**"));
                                                        //int iBusinessName = 0;
                                                        #endregion

                                                        // Remove the list items which starts with * as they are comments.
                                                        //textLinePD.RemoveAll(x => x.TrimStart().StartsWith("*"));

                                                        //var fileLines = textLinePD.ToArray().CaseAdjustmentUniverse();
                                                        //textLinePD = fileLines.ToList();

                                                        for (int iPD = 0; iPD < textLinePD.Count; iPD++)
                                                        {
                                                            string strConditions = string.Empty;
                                                            string strComments = null;
                                                            bool lineDone = false;
                                                            int cntSpaces = 0;
                                                            string currentSentance = textLinePD[iPD].ToString().Trim();
                                                            string indentCurrentSentance = textLinePD[iPD].ToString();
                                                            if (!currentSentance.StartsWith("*"))
                                                            {
                                                                if (currentSentance.Contains(";*") || currentSentance.Contains("; *"))
                                                                {
                                                                    string[] splitVal = currentSentance.Split(';');
                                                                    if (splitVal.Length > 1)
                                                                    {
                                                                        indentCurrentSentance = splitVal[0].ToString().Trim();
                                                                        currentSentance = splitVal[0].ToString().Trim();
                                                                        strComments = splitVal[1].ToString().Trim();
                                                                    }
                                                                }

                                                                #region Insert Only Method logic
                                                                var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    cntSpaces = GetLeadingWhitespaceLength(textLinePD[iPD].ToString());
                                                                }

                                                                if ((cntSpaces == 0) && ((textLinePD[iPD].ToString().Trim().StartsWith("-")) == false) && ((textLinePD[iPD].ToString().Trim().EndsWith("*'.")) == false))
                                                                {
                                                                    if (currentSentance.Length > 0)
                                                                    {
                                                                        if (Regex.IsMatch(currentSentance, @"^\d"))
                                                                        {
                                                                            currentSentance = Regex.Replace(currentSentance, @"[^0-9\.]?", string.Empty);
                                                                            if (cntSpaces == Convert.ToInt32(methodStart.StartIndicator))
                                                                            {
                                                                                //iBusinessName = iBusinessName + 1;

                                                                                if (iMethodEnd > 0)
                                                                                {
                                                                                    var methodEnd = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                                                    var stmtReferenceMaster = new StatementReferenceMaster
                                                                                    {
                                                                                        FileId = checkFileExistOrNot[0].FileId,
                                                                                        ResolvedStatement = "END",
                                                                                        OriginalStatement = "END",
                                                                                        ClassCalled = null,
                                                                                        MethodName = null,
                                                                                        DataOrObjectType = null,
                                                                                        MethodCalled = null,
                                                                                        VariableNameDeclared = null,
                                                                                        BaseCommandId = 9,
                                                                                        PrimaryCommandId = 42,
                                                                                        ProjectId = projectId,
                                                                                        StatementComment = strComments
                                                                                    };

                                                                                    await
                                                                                   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                }


                                                                                var stmtReferenceMasterNew = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = checkFileExistOrNot[0].FileId,
                                                                                    ResolvedStatement = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", ""),
                                                                                    OriginalStatement = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", ""),
                                                                                    ClassCalled = null,
                                                                                    MethodName = currentSentance.Replace(" SECTION.", "").Replace(" EXIT.", "").Replace(".", "").ToString(),
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = methodStart.BaseCommandId,
                                                                                    PrimaryCommandId = methodStart.PrimaryReferenceId,
                                                                                    ProjectId = projectId,
                                                                                    StatementComment = strComments,
                                                                                    BusinessName = textLinePD[iPD - 2].Replace("*", "").TrimStart()
                                                                                };

                                                                                await
                                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterNew);
                                                                                iMethodEnd++;
                                                                                continue;
                                                                            }
                                                                        }
                                                                        //else
                                                                        //{
                                                                        //    if (iLastMethodEnd == 0)
                                                                        //    {
                                                                        //        var stmtReferenceMaster = new StatementReferenceMaster
                                                                        //        {
                                                                        //            FileId = checkFileExistOrNot[0].FileId,
                                                                        //            ResolvedStatement = "END",
                                                                        //            OriginalStatement = "END",
                                                                        //            ClassCalled = null,
                                                                        //            MethodName = null,
                                                                        //            DataOrObjectType = null,
                                                                        //            MethodCalled = null,
                                                                        //            VariableNameDeclared = null,
                                                                        //            BaseCommandId = 9,
                                                                        //            PrimaryCommandId = 42,
                                                                        //            ProjectId = projectId,
                                                                        //            StatementComment = strComments
                                                                        //        };

                                                                        //        await
                                                                        //       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                        //        iLastMethodEnd++;
                                                                        //    }
                                                                        //}
                                                                    }
                                                                }

                                                                #endregion

                                                                #region Replacing the LOCATE statements
                                                                if (currentSentance.Trim().StartsWith("LOCATE "))
                                                                {
                                                                    currentSentance = currentSentance.Replace("LOCATE", "FIND").Replace("THEN", "INTO FOUND");
                                                                    indentCurrentSentance = indentCurrentSentance.Replace("LOCATE", "FIND").Replace("THEN", "INTO FOUND");
                                                                }
                                                                #endregion

                                                                #region Insert Find statement logic
                                                                var findStart = findIndicatorStart.Find(x => x.StartIndicator != null);
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith(findStart.StartIndicator))
                                                                        {
                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                BaseCommandId = findStart.BaseCommandId,
                                                                                PrimaryCommandId = findStart.PrimaryReferenceId,
                                                                                ProjectId = projectId,
                                                                                StatementComment = strComments
                                                                            };

                                                                            await
                                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                            //continue;
                                                                            currentSentance = "IF FOUND";
                                                                        }

                                                                    }

                                                                }
                                                                #endregion

                                                                #region Insert MATREADU and READVU and READ and OPEN and READLIST statement

                                                                if (currentSentance.Trim().StartsWith("MATREADU ") || currentSentance.Trim().StartsWith("READVU ") || currentSentance.Trim().StartsWith("READV "))
                                                                {
                                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = currentSentance,
                                                                        OriginalStatement = currentSentance,
                                                                        ClassCalled = null,
                                                                        MethodName = null,
                                                                        DataOrObjectType = null,
                                                                        MethodCalled = null,
                                                                        VariableNameDeclared = null,
                                                                        PrimaryCommandId = 0,
                                                                        ProjectId = projectId,
                                                                        StatementComment = strComments
                                                                    };
                                                                    await
                                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                    currentSentance = "IF RECORD-ID-EXISTS";

                                                                }
                                                                if (currentSentance.Trim().StartsWith("READ "))
                                                                {
                                                                    strConditions = currentSentance;
                                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        ClassCalled = null,
                                                                        MethodName = null,
                                                                        DataOrObjectType = null,
                                                                        MethodCalled = null,
                                                                        VariableNameDeclared = null,
                                                                        PrimaryCommandId = 0,
                                                                        ProjectId = projectId,
                                                                        StatementComment = strComments
                                                                    };
                                                                    await
                                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                    currentSentance = "IF FOUND " + currentSentance.Split(' ').LastOrDefault();
                                                                }
                                                                if (currentSentance.Trim().StartsWith("OPEN "))
                                                                {
                                                                    strConditions = currentSentance;
                                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        ClassCalled = null,
                                                                        MethodName = null,
                                                                        DataOrObjectType = null,
                                                                        MethodCalled = null,
                                                                        VariableNameDeclared = null,
                                                                        PrimaryCommandId = 0,
                                                                        ProjectId = projectId,
                                                                        StatementComment = strComments
                                                                    };
                                                                    await
                                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                    currentSentance = "IF NOT-SUCCESS " + currentSentance.Split(' ').LastOrDefault();
                                                                }
                                                                if (currentSentance.Trim().StartsWith("READLIST "))
                                                                {
                                                                    strConditions = currentSentance;
                                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                                    {
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        OriginalStatement = strConditions.Replace("ELSE", "").Replace("THEN", ""),
                                                                        ClassCalled = null,
                                                                        MethodName = null,
                                                                        DataOrObjectType = null,
                                                                        MethodCalled = null,
                                                                        VariableNameDeclared = null,
                                                                        PrimaryCommandId = 0,
                                                                        ProjectId = projectId,
                                                                        StatementComment = strComments
                                                                    };
                                                                    await
                                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                    currentSentance = "IF NOT-FOUND " + currentSentance.Split(' ').LastOrDefault();
                                                                }
                                                                #endregion

                                                                #region Insert If statement logic
                                                                var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith(ifStart.StartIndicator))
                                                                        {
                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                BaseCommandId = ifStart.BaseCommandId,
                                                                                PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                                                ProjectId = projectId,
                                                                                StatementComment = strComments
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
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (currentSentance.StartsWith(ifEnd.StartIndicator))
                                                                    {
                                                                        if (!currentSentance.StartsWith("*"))
                                                                        {
                                                                            if (!currentSentance.StartsWith("END ELSE") && !currentSentance.StartsWith("END CASE") && !currentSentance.StartsWith("END."))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = checkFileExistOrNot[0].FileId,
                                                                                    ResolvedStatement = indentCurrentSentance,
                                                                                    OriginalStatement = indentCurrentSentance,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = ifEnd.BaseCommandId,
                                                                                    PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                                                    ParsedOrNot = "1",
                                                                                    ProjectId = projectId,
                                                                                    StatementComment = strComments
                                                                                };
                                                                                await
                                                                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                continue;
                                                                            }

                                                                        }
                                                                    }
                                                                }
                                                                #endregion

                                                                #region Insert Call External (CALL) statement logic
                                                                var execCallStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith("CALL "))
                                                                        {
                                                                            if (currentSentance.StartsWith(execCallStart.StartIndicator))
                                                                            {
                                                                                string[] stmtSplit = currentSentance.Replace("CALL", "").Trim().Split('(');

                                                                                if (stmtSplit.Length > 0)
                                                                                {
                                                                                    int fileExtension = 0;
                                                                                    string classNamed = "";
                                                                                    string checkSplitVal = "";

                                                                                    if (stmtSplit.Length > 1)
                                                                                    {
                                                                                        checkSplitVal = stmtSplit[0].Trim().ToString().Replace("@", "");
                                                                                    }
                                                                                    //else
                                                                                    //{
                                                                                    //    checkSplitVal = stmtSplit[1].Trim().ToString();
                                                                                    //}

                                                                                    if (checkSplitVal.Length > 0)
                                                                                    {

                                                                                        string className = checkSplitVal.ToString();
                                                                                        var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                                        if (callExtExpandedCode.Count == 0)
                                                                                        {
                                                                                            fileExtension = 9;
                                                                                            callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                            classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                            string strSplit = classNamed.Split('\\').Last();
                                                                                            classNamed = classNamed.Replace(strSplit, "").Replace("\\", ".") + className;
                                                                                        }
                                                                                        else
                                                                                        {
                                                                                            classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                                        }

                                                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                                                        {
                                                                                            FileId = checkFileExistOrNot[0].FileId,
                                                                                            ResolvedStatement = indentCurrentSentance,
                                                                                            OriginalStatement = indentCurrentSentance,
                                                                                            //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                            ClassCalled = classNamed,
                                                                                            MethodName = null,
                                                                                            DataOrObjectType = null,
                                                                                            MethodCalled = null,
                                                                                            VariableNameDeclared = null,
                                                                                            //BaseCommandId = execCallStart.BaseCommandId,
                                                                                            //PrimaryCommandId = execCallStart.PrimaryReferenceId,
                                                                                            BaseCommandId = 0,
                                                                                            PrimaryCommandId = 0,
                                                                                            ProjectId = projectId,
                                                                                            StatementComment = strComments
                                                                                        };

                                                                                        await
                                                                              _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                                    }
                                                                                }
                                                                            }

                                                                            continue;
                                                                        }
                                                                    }
                                                                }

                                                                #endregion

                                                                #region Insert Call Internal (GOSUB) statement logic
                                                                var callInternalStart = callInternalIndicationStart.Find(x => x.StartIndicator != null);

                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith(callInternalStart.StartIndicator))
                                                                        {

                                                                            string[] stmtSplit = currentSentance.Split(' ');

                                                                            if (stmtSplit.Length > 1)
                                                                            {
                                                                                var performMethodCalled = stmtSplit[1].ToString().Trim();
                                                                                if (string.IsNullOrEmpty(performMethodCalled))
                                                                                {
                                                                                    performMethodCalled = stmtSplit[2].ToString().Trim();
                                                                                }

                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = checkFileExistOrNot[0].FileId,
                                                                                    ResolvedStatement = indentCurrentSentance,
                                                                                    OriginalStatement = indentCurrentSentance,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = performMethodCalled,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = callInternalStart.BaseCommandId,
                                                                                    PrimaryCommandId = callInternalStart.PrimaryReferenceId,
                                                                                    ProjectId = projectId,
                                                                                    StatementComment = strComments
                                                                                };
                                                                                await
                                                                          _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                                continue;
                                                                            }


                                                                        }
                                                                    }


                                                                }

                                                                #endregion

                                                                #region Insert Loop start [FOR] statement logic
                                                                var loopStart = loopIndicatorStart.Find(x => x.StartIndicator != null);

                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith("FOR "))
                                                                        {
                                                                            if (currentSentance.StartsWith(loopStart.StartIndicator))
                                                                            {
                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = checkFileExistOrNot[0].FileId,
                                                                                    ResolvedStatement = indentCurrentSentance,
                                                                                    OriginalStatement = indentCurrentSentance,
                                                                                    ClassCalled = null,
                                                                                    MethodName = null,
                                                                                    DataOrObjectType = null,
                                                                                    MethodCalled = null,
                                                                                    VariableNameDeclared = null,
                                                                                    BaseCommandId = loopStart.BaseCommandId,
                                                                                    PrimaryCommandId = loopStart.PrimaryReferenceId,
                                                                                    ProjectId = projectId,
                                                                                    StatementComment = strComments
                                                                                };

                                                                                await
                                                                               _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                                continue;
                                                                            }
                                                                        }
                                                                    }


                                                                }

                                                                #endregion

                                                                #region Insert Loop end [NEXT] statement logic
                                                                var loopEnd = loopIndicatorEnd.Find(x => x.StartIndicator != null);

                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.StartsWith(loopEnd.StartIndicator))
                                                                        {
                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                BaseCommandId = loopEnd.BaseCommandId,
                                                                                PrimaryCommandId = loopEnd.PrimaryReferenceId,
                                                                                ProjectId = projectId,
                                                                                StatementComment = strComments
                                                                            };

                                                                            await
                                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                            continue;
                                                                        }
                                                                    }


                                                                }

                                                                #endregion

                                                                #region Insert normal statements logic
                                                                if (lineDone) continue;
                                                                if (currentSentance.Length > 0)
                                                                {
                                                                    if (!currentSentance.StartsWith("*"))
                                                                    {
                                                                        if (currentSentance.Trim().StartsWith("BEGIN CASE"))
                                                                        {
                                                                            continue;
                                                                            //var statementReferenceMaster = new StatementReferenceMaster
                                                                            //{
                                                                            //    FileId = checkFileExistOrNot[0].FileId,
                                                                            //    ResolvedStatement = currentSentance,
                                                                            //    OriginalStatement = currentSentance,
                                                                            //    ClassCalled = null,
                                                                            //    MethodName = null,
                                                                            //    DataOrObjectType = null,
                                                                            //    MethodCalled = null,
                                                                            //    VariableNameDeclared = null,
                                                                            //    BaseCommandId = 1,
                                                                            //    PrimaryCommandId = 33,
                                                                            //    ProjectId = projectId,
                                                                            //    StatementComment = strComments
                                                                            //};
                                                                            //await
                                                                            //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                        }
                                                                        else if (currentSentance.Trim().StartsWith("CASE "))
                                                                        {
                                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                BaseCommandId = 1,
                                                                                PrimaryCommandId = 33,
                                                                                ProjectId = projectId
                                                                            };
                                                                            await
                                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                        }
                                                                        else if (currentSentance.Trim().StartsWith("MATREADU ") || currentSentance.Trim().StartsWith("READVU "))
                                                                        {
                                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                BaseCommandId = 1,
                                                                                PrimaryCommandId = 33,
                                                                                ProjectId = projectId
                                                                            };
                                                                            await
                                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                        }
                                                                        else if (currentSentance.Trim().StartsWith("END CASE"))
                                                                        {
                                                                            continue;
                                                                            //var statementReferenceMaster = new StatementReferenceMaster
                                                                            //{
                                                                            //    FileId = checkFileExistOrNot[0].FileId,
                                                                            //    ResolvedStatement = currentSentance,
                                                                            //    OriginalStatement = currentSentance,
                                                                            //    ClassCalled = null,
                                                                            //    MethodName = null,
                                                                            //    DataOrObjectType = null,
                                                                            //    MethodCalled = null,
                                                                            //    VariableNameDeclared = null,
                                                                            //    BaseCommandId = 2,
                                                                            //    PrimaryCommandId = 34,
                                                                            //    ProjectId = projectId,
                                                                            //    StatementComment = strComments
                                                                            //};
                                                                            //await
                                                                            //    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                        }
                                                                        else
                                                                        {
                                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                                ResolvedStatement = indentCurrentSentance,
                                                                                OriginalStatement = indentCurrentSentance,
                                                                                ClassCalled = null,
                                                                                MethodName = null,
                                                                                DataOrObjectType = null,
                                                                                MethodCalled = null,
                                                                                VariableNameDeclared = null,
                                                                                PrimaryCommandId = 0,
                                                                                ProjectId = projectId,
                                                                                StatementComment = strComments
                                                                            };
                                                                            await
                                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                                        }

                                                                    }


                                                                }
                                                                #endregion

                                                                #region Last Method End logic
                                                                //if (iPD == textLineIP.Count - 1)
                                                                //{
                                                                //    var stmtReferenceMaster = new StatementReferenceMaster
                                                                //    {
                                                                //        FileId = checkFileExistOrNot[0].FileId,
                                                                //        ResolvedStatement = "END",
                                                                //        OriginalStatement = "END",
                                                                //        ClassCalled = null,
                                                                //        MethodName = null,
                                                                //        DataOrObjectType = null,
                                                                //        MethodCalled = null,
                                                                //        VariableNameDeclared = null,
                                                                //        BaseCommandId = 9,
                                                                //        PrimaryCommandId = 42,
                                                                //        ProjectId = projectId
                                                                //    };

                                                                //    await
                                                                //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                //}
                                                                #endregion
                                                            }
                                                        }

                                                        #region Insert Method End logic
                                                        if (iMethodEnd == 0)
                                                        {
                                                            var methodLastEnd = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                            var stmtReferenceMasterMethodEnd = new StatementReferenceMaster
                                                            {
                                                                FileId = checkFileExistOrNot[0].FileId,
                                                                ResolvedStatement = "END",
                                                                OriginalStatement = "END",
                                                                ClassCalled = null,
                                                                MethodName = null,
                                                                DataOrObjectType = null,
                                                                MethodCalled = null,
                                                                VariableNameDeclared = null,
                                                                BaseCommandId = 9,
                                                                PrimaryCommandId = 42,
                                                                ProjectId = projectId,
                                                                StatementComment = null
                                                            };

                                                            await
                                                           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterMethodEnd);
                                                        }
                                                        #endregion

                                                    }
                                                    #endregion Parsing ending INCLUDES -- End
                                                }
                                                #endregion
                                            }
                                        }
                                        #endregion
                                    }
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            ex.ToString();
                        }

                        #endregion
                    }

                    else if (file.FileTypeExtensionId == 10)
                    {

                        #region JCL files processing Logic
                        string currentStatementJcl = string.Empty;
                        string jclProgname = string.Empty;
                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                List<string> lstata = new List<string>();

                                lstata = File.ReadAllLines(file.FilePath).ToList();

                                if (lstata.Count > 0)
                                {
                                    #region Insert Class logic
                                    var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                    var statementReferenceMaster = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        OriginalStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        ClassCalled = null,
                                        MethodName = file.FileName.Replace(".jcl", "").ToUpper(),
                                        DataOrObjectType = null,
                                        MethodCalled = null,
                                        VariableNameDeclared = null,
                                        ClassNameDeclared = projectName + file.FilePath.Replace(projectPath, "").Trim().Replace("\\", "."),
                                        PrimaryCommandId = classStart.PrimaryReferenceId,
                                        BaseCommandId = classStart.BaseCommandId,
                                        ProjectId = projectId,
                                        StatementComment = null,
                                        BusinessName = lstata[0].ToString()
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);

                                    #endregion

                                    #region Insert Method Start logic
                                    var methodStart = methodIndicationStart.Find(x => x.StartIndicator == "0");
                                    var statementReferenceMasterMethod = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        OriginalStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        ClassCalled = null,
                                        MethodName = null,
                                        DataOrObjectType = null,
                                        MethodCalled = null,
                                        VariableNameDeclared = null,
                                        ClassNameDeclared = null,
                                        PrimaryCommandId = methodStart.PrimaryReferenceId,
                                        BaseCommandId = methodStart.BaseCommandId,
                                        ProjectId = projectId,
                                        StatementComment = null,
                                        BusinessName = lstata[0].ToString()
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterMethod);

                                    #endregion

                                    #region INSERTING into StatementReferenceMaster table
                                    string strOnlyFileName = "";
                                    int iIndex = 0;
                                    iIndex = file.FilePath.LastIndexOf("\\");
                                    if (iIndex > 0)
                                    {
                                        strOnlyFileName = file.FilePath.Substring(iIndex + 1);
                                        strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "").Replace(".JCL", "");
                                    }

                                    var fileLines = lstata.ToArray().IfAdjustmentUniverseJcl();
                                    lstata = fileLines.ToList();

                                    for (int iCnt = 0; iCnt < lstata.Count; iCnt = iCnt + 1)
                                    {
                                        try
                                        {
                                            currentStatementJcl = lstata[iCnt].ToString();

                                            #region Insert Call External (RUN) statement logic
                                            var execCallStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                            if (currentStatementJcl.Length > 0)
                                            {
                                                if (!currentStatementJcl.StartsWith("*"))
                                                {
                                                    if (currentStatementJcl.StartsWith("RUN "))
                                                    {
                                                        //if (currentStatementJcl.StartsWith(execCallStart.StartIndicator))
                                                        //{
                                                        string[] stmtSplit = currentStatementJcl.Replace("RUN", "").Trim().Split(' ');

                                                        if (stmtSplit.Length > 0)
                                                        {
                                                            int fileExtension = 0;
                                                            string classNamed = "";
                                                            string checkSplitVal = "";
                                                            string folderPath = "";

                                                            if (stmtSplit.Length > 1)
                                                            {
                                                                checkSplitVal = stmtSplit[1].Trim().ToString().Replace("@", "");
                                                                folderPath = stmtSplit[0].Trim().ToString();
                                                            }

                                                            if (checkSplitVal.Length > 0)
                                                            {

                                                                string className = checkSplitVal.ToString();
                                                                var callExtExpandedCode = GetFileCountJcl(className, projectId, fileExtension, 0, folderPath);

                                                                if (callExtExpandedCode.Count == 0)
                                                                {
                                                                    fileExtension = 9;
                                                                    callExtExpandedCode = GetFileCountJcl(className, projectId, fileExtension, 1, folderPath);
                                                                    classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                    string strSplit = classNamed.Split('\\').Last();
                                                                    classNamed = classNamed.Replace(strSplit, "").Replace("\\", ".") + className;
                                                                }
                                                                else
                                                                {
                                                                    classNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                }

                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                {
                                                                    FileId = file.FileId,
                                                                    ResolvedStatement = currentStatementJcl,
                                                                    OriginalStatement = currentStatementJcl,
                                                                    //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                    ClassCalled = classNamed,
                                                                    MethodName = null,
                                                                    DataOrObjectType = null,
                                                                    MethodCalled = null,
                                                                    VariableNameDeclared = null,
                                                                    BaseCommandId = execCallStart.BaseCommandId,
                                                                    PrimaryCommandId = execCallStart.PrimaryReferenceId,
                                                                    ProjectId = projectId,
                                                                    StatementComment = null
                                                                };

                                                                await
                                                      _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                            }
                                                        }
                                                        //}

                                                        continue;
                                                    }
                                                }
                                            }

                                            #endregion

                                            #region Insert If statement logic
                                            var ifStart = ifConditionStart.Find(x => x.StartIndicator != null);
                                            if (currentStatementJcl.Length > 0)
                                            {
                                                if (!currentStatementJcl.StartsWith("*"))
                                                {
                                                    if (currentStatementJcl.StartsWith(ifStart.StartIndicator))
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentStatementJcl,
                                                            OriginalStatement = currentStatementJcl,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = ifStart.BaseCommandId,
                                                            PrimaryCommandId = ifStart.PrimaryReferenceId,
                                                            ProjectId = projectId,
                                                            StatementComment = null
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
                                            if (currentStatementJcl.Length > 0)
                                            {
                                                currentStatementJcl = currentStatementJcl.Replace("ENDIF", "END-IF").Replace("END IF", "END-IF");
                                                if (currentStatementJcl.StartsWith(ifEnd.StartIndicator))
                                                {
                                                    if (!currentStatementJcl.StartsWith("*") && !currentStatementJcl.StartsWith("END:"))
                                                    {
                                                        var stmtReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentStatementJcl,
                                                            OriginalStatement = currentStatementJcl,
                                                            ClassCalled = null,
                                                            MethodName = null,
                                                            DataOrObjectType = null,
                                                            MethodCalled = null,
                                                            VariableNameDeclared = null,
                                                            BaseCommandId = ifEnd.BaseCommandId,
                                                            PrimaryCommandId = ifEnd.PrimaryReferenceId,
                                                            ParsedOrNot = "1",
                                                            ProjectId = projectId,
                                                            StatementComment = null
                                                        };
                                                        await
                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                        continue;

                                                    }
                                                }
                                            }
                                            #endregion

                                            #region Insert normal statements logic

                                            if (currentStatementJcl.Length > 0)
                                            {
                                                if (!currentStatementJcl.Trim().Replace("//", "").StartsWith("*"))
                                                {
                                                    var statementReferenceMasterNor = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = currentStatementJcl,
                                                        OriginalStatement = currentStatementJcl,
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = null
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterNor);
                                                }
                                            }
                                            #endregion

                                        }
                                        catch (Exception ex)
                                        {
                                            ex.ToString();
                                        }
                                    }

                                    #endregion
                                }

                                #region Insert Method End logic
                                var methodEnd = methodIndicationEnd.Find(x => x.StartIndicator == null);
                                var statementReferenceMasterMethodEnd = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = "END",
                                    OriginalStatement = "END",
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = 9,
                                    PrimaryCommandId = 30,
                                    ProjectId = projectId,
                                    StatementComment = null
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterMethodEnd);

                                #endregion

                                #region Insert Class End logic
                                var classEnd = callClassIndicatorEnd.Find(x => x.StartIndicator == null);
                                var statementReferenceMasterEnd = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = "END " + file.FileName.Replace(".jcl", "").ToUpper(),
                                    OriginalStatement = "END " + file.FileName.Replace(".jcl", "").ToUpper(),
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    PrimaryCommandId = classEnd.PrimaryReferenceId,
                                    BaseCommandId = classEnd.BaseCommandId,
                                    ProjectId = projectId,
                                    StatementComment = null
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterEnd);
                                #endregion

                            }
                            catch (Exception ex)
                            {
                                Console.WriteLine(ex.InnerException);
                            }
                        }

                        #endregion

                    }
                    else if (file.FileTypeExtensionId == 11)
                    {
                        #region Text files (Data Dictionary) processing Logic

                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                List<string> lstata = new List<string>();

                                lstata = File.ReadAllLines(file.FilePath).ToList();

                                if (lstata.Count > 0)
                                {
                                    #region Insert Class logic
                                    var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                    var statementReferenceMasterClassStart = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = file.FileName.Replace(".TXT", "").Replace(".txt", "").ToUpper(),
                                        OriginalStatement = file.FileName.Replace(".TXT", "").Replace(".txt", "").ToUpper(),
                                        ClassCalled = null,
                                        MethodName = null,
                                        DataOrObjectType = null,
                                        MethodCalled = null,
                                        VariableNameDeclared = null,
                                        //ClassNameDeclared = file.FilePath.Substring(2, file.FilePath.Length - 2).Trim().Replace("\\", "."),
                                        //ClassNameDeclared = projectName + file.FilePath.Replace(".proc", "").Replace(projectPath, "").Trim().Replace("\\", "."),
                                        ClassNameDeclared = projectName + file.FilePath.Replace(projectPath, "").Trim().Replace("\\", "."),
                                        PrimaryCommandId = classStart.PrimaryReferenceId,
                                        BaseCommandId = classStart.BaseCommandId,
                                        ProjectId = projectId,
                                        StatementComment = null
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClassStart);

                                    #endregion

                                    #region INSERT THE STATEMENT IN STATEMENTREFERENCE TABLE

                                    string strOnlyFileName = "";
                                    int iIndex = 0;
                                    iIndex = file.FilePath.LastIndexOf("\\");
                                    if (iIndex > 0)
                                    {
                                        strOnlyFileName = file.FilePath.Substring(iIndex + 1);
                                        strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "");
                                    }

                                    for (int iCnt = 0; iCnt < lstata.Count; iCnt++)
                                    {
                                        string currentState = lstata[iCnt].ToString();

                                        if (currentState.Length > 0)
                                        {
                                            #region Insert normal statements logic

                                            if (currentState.Length > 0)
                                            {
                                                if (!currentState.Replace("//", "").StartsWith("*"))
                                                {
                                                    var statementReferenceMaster = new StatementReferenceMaster
                                                    {
                                                        FileId = file.FileId,
                                                        ResolvedStatement = currentState,
                                                        OriginalStatement = currentState,
                                                        ClassCalled = null,
                                                        MethodName = null,
                                                        DataOrObjectType = null,
                                                        MethodCalled = null,
                                                        VariableNameDeclared = null,
                                                        PrimaryCommandId = 0,
                                                        ProjectId = projectId,
                                                        StatementComment = null
                                                    };
                                                    await
                                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                }
                                            }
                                            #endregion
                                        }
                                    }
                                    #endregion

                                }

                                #region Insert Class End logic
                                var classEnd = callClassIndicatorEnd.Find(x => x.StartIndicator == null);
                                var statementReferenceMasterEnd = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = "END " + file.FileName.Replace(".TXT", "").ToUpper(),
                                    OriginalStatement = "END " + file.FileName.Replace(".TXT", "").ToUpper(),
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    PrimaryCommandId = classEnd.PrimaryReferenceId,
                                    BaseCommandId = classEnd.BaseCommandId,
                                    ProjectId = projectId,
                                    StatementComment = null
                                };
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterEnd);
                                #endregion

                            }
                            catch (Exception ex)
                            {
                                Console.WriteLine(ex.InnerException);
                            }
                        }

                        #endregion
                    }
                }

                #region Added the Action Workflow Table...
                // Action Workflows data...
                var projectMaster = new GeneralRepository<ProjectMaster>(new AppDbContext());
                Expression<Func<ProjectMaster, bool>> expression = e => e.ProjectId == projectId;
                var projectMasterData = await projectMaster.GetItem<ProjectMaster>(expression, projectId);
                int projectType = projectMasterData.ProjectConfigType;
                if (projectType == 5) // For COBOL file
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
                        //var allConfigFiles =
                        //    await _codeVortoService.FileMasterRepository.GetAllItems(
                        //        f => f.ProjectId == projectId);

                        var fileMasterRepository = new FileMasterRepository(new AppDbContext());
                        var actionWorkflowsRepository = new ActionWorkflowsRepository(new AppDbContext());
                        var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());

                        var allConfigFiles = await fileMasterRepository.GetAllItems(
                                f => f.ProjectId == projectId);

                        foreach (var cFile in allConfigFiles)
                        {
                            var fileNameWithoutExtension = Path.GetFileNameWithoutExtension(cFile.FilePath);
                            if (fileNameWithoutExtension == null) continue;
                            string className = fileNameWithoutExtension.Split('.')[0];

                            var fileExtension = cFile.FileTypeExtensionId;
                            if (fileExtension == 10)
                            {
                                //     var genericBlocks = await statementReferenceRepository.GetAllItems(
                                //f => f.ProjectId == projectId && f.FileId == cFile.FileId && f.BaseCommandId == 8);

                                object[] param = { 
                                   new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                                   new MySqlParameter("@fileId", MySqlDbType.Int32){Value = cFile.FileId}
                               };

                                var genericBlocks = await
                                   statementReferenceRepository.ExecuteStoreProcedure<StatementReferenceMaster>("SpGetMethodData", param);

                                foreach (var statement in genericBlocks)
                                {
                                    ActionWorkflows actionWorkflow = new ActionWorkflows
                                    {
                                        ActionWorkflowId = 0,
                                        CreatedBy = 1,
                                        EndPointOrService = "Batch",
                                        //MethodStatementId = cFile.FileId,
                                        MethodStatementId = statement.StatementId,
                                        OriginFileName =
                                            Path.GetFileName(
                                                cFile.FilePath),
                                        OriginFilePath =
                                            cFile.FilePath,
                                        ProjectId = projectId,
                                        OriginEventMethod = cFile.FileName,
                                        //OriginObject = cFile.FilePath.Substring(2, cFile.FilePath.Length - 2).Trim().Replace("\\", "."),
                                        OriginObject = projectName + cFile.FilePath.Replace(".jcl", "").Replace(projectPath, "").Trim().Replace("\\", "."),
                                        WorkflowName = cFile.FileName,
                                        ServiceBaseAddress = null,
                                        ServiceContract = null,
                                        WorkflowBusinessName = statement.BusinessName
                                    };
                                    await actionWorkflowsRepository.AddNewItem(actionWorkflow);
                                }
                            }
                        }
                    }
                    catch (Exception)
                    {

                    }


                }

                #endregion

                #region Added for Pseudo Code

                #endregion

                #endregion
            }

            IHttpActionResult processResult = await ProcessUniverseProjectFiles(projectId);
            var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
            string data = await dataContent.Content.ReadAsStringAsync();
            return Ok(data);
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUniverseProjectFiles(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var fileMasterRepository = new FileMasterRepository(new AppDbContext());

           #region region 1...
                string execSql =
                    "  Select * from statementreferencemaster where BaseCommandId=6 AND ProjectId = " + projectId + " and ClassCalled is not null;";
                var execName = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSql);
                foreach (var constructor in execName)
                {
                    string[] temp = constructor.ClassCalled.Split('.');
                    string fileName = "";

                    if (temp.Length > 0)
                    {
                        fileName = temp[temp.Length - 2].ToString() + "." + temp[temp.Length - 1].ToString();
                    }

                    var allCheckFiles =
                       await _codeVortoService.FileMasterRepository.GetAllItems(
                           f => f.ProjectId == projectId && f.FileName == fileName.Trim());

                    foreach (var files in allCheckFiles)
                    {
                        string methodSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                         " AND ProjectId = " + projectId + " AND BaseCommandId=8;";

                        var MethodName = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(methodSql);

                        for (int iMethodName = 0; iMethodName < MethodName.Count; iMethodName++)
                        {
                            string checkSql = "";
                            if (MethodName.Count > 1)
                            {
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                                " AND ProjectId = " + projectId + " AND StatementId > " + MethodName[iMethodName].StatementId + " and StatementId < " + MethodName[iMethodName + 1].StatementId + " and BaseCommandId!=9;";
                            }
                            else
                            {
                                checkSql = " Select * from statementreferencemaster where FileId = " + files.FileId +
                                                    " AND ProjectId = " + projectId + " AND StatementId > " + MethodName[iMethodName].StatementId + " and BaseCommandId!=9;";
                            }

                            var checkCnt = await
                            baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(checkSql);
                            if (checkCnt.Count > 0)
                            {
                                constructor.MethodCalled = MethodName[iMethodName].MethodName + "()";
                                await
                                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                                break;
                            }
                            else
                            {
                                continue;
                            }


                        }
                    }


                }
                #endregion

                #region region 2...Data Dictionary Reference Code & Include (Subroutine) file logic
                try
                {
                    string fileMasterSql =
                        "  Select * from fileMaster where FileTypeExtensionId=9 and ProjectId = " + projectId + "; ";
                    var fileMasterData = await
                        fileMasterRepository.GetDataFromSqlQuery<FileMaster>(fileMasterSql);
                    foreach (var datafile in fileMasterData)
                    {
                        if (datafile.FileTypeExtensionId == 9)
                        {
                            #region Data Dictionary
                            var description = string.Empty;
                            string dataDictionarySql =
                                " Select * from statementreferencemaster where fileid=" + datafile.FileId + " and basecommandId Not In (8,9,2,5,6,19,20) and ProjectId=" + projectId + " and OriginalStatement like '%)%';";
                            var dataDictionary = await
                                baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(dataDictionarySql);

                            foreach (var dataDictionaryStmt in dataDictionary)
                            {
                                var currentSentance = dataDictionaryStmt.OriginalStatement;
                                var statementId = dataDictionaryStmt.StatementId;
                                var fileId = dataDictionaryStmt.FileId;

                                Regex word = new Regex(@"([A-Z0-9]\.)\w+([A-Z0-9]\.)\w+(\([\[0-9]*\))");
                                var aa = "";
                                foreach (Match match in word.Matches(currentSentance))
                                {
                                    aa = match.Value;
                                    string[] strSplit = aa.Split('(');
                                    if (strSplit.Length > 1)
                                    {
                                        string output = strSplit[0].Replace("(", "").Trim().ToString();
                                        var fileName = output.Substring(output.IndexOf(".") + 1).Trim();
                                        string idDictinary = strSplit[1].Replace(")", "").Trim().ToString();

                                        #region Check the file is exit or not (11 is data dictionary file)
                                        var checkFileExistOrNot = GetFileCount(fileName, projectId, 11, 0);
                                        if (checkFileExistOrNot.Count > 0)
                                        {
                                            var dictionaryStmt = GetDictionaryData(projectId, checkFileExistOrNot[0].FileId, idDictinary);

                                            for (int i = 0; i < dictionaryStmt.Count; i++)
                                            {
                                                string originalStatement = dictionaryStmt[0].OriginalStatement.ToString();
                                                string[] splitOrginalStatement = originalStatement.Split(',');
                                                if (splitOrginalStatement.Length > 1)
                                                {
                                                    description = splitOrginalStatement[2].ToString().Replace("\"", "") + " field of " + fileName;
                                                }
                                            }

                                            currentSentance = currentSentance.Replace(aa, description);
                                        }
                                        #endregion
                                    }

                                    dataDictionaryStmt.OriginalStatement = currentSentance;
                                    dataDictionaryStmt.ResolvedStatement = currentSentance;
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.UpdateItem(dataDictionaryStmt);
                                }

                            }
                            #endregion

                            #region INSERT command (statements) Updated
                            //try
                            //{
                            //    var stmt = "$INSERT";
                            //    var insertStmtData = GetInserStmtData(projectId, datafile.FileId, stmt);
                            //    if (insertStmtData.Count > 0)
                            //    {
                            //        for (int iInsertStmt = 0; iInsertStmt < insertStmtData.Count; iInsertStmt++)
                            //        {
                            //            var insertStmt = insertStmtData[iInsertStmt].OriginalStatement;
                            //            string[] splitInsert = insertStmt.Split('>');
                            //            if (splitInsert.Length > 1)
                            //            {
                            //                string fileName = splitInsert.Last();
                            //                string currentSentance = string.Empty;

                            //                #region Check the file is exit or not (12 is include file)
                            //                var checkFileExistOrNot = GetFileCount(fileName, projectId, 12, 0);
                            //                if (checkFileExistOrNot.Count > 0)
                            //                {
                            //                    //currentSentance = insertStmtData[iInsertStmt].OriginalStatement + "&nbsp;<a id='include' href='includeStateDialog(" + insertStmtData[iInsertStmt].FileId + ")'>" + fileName + "</a>";
                            //                    currentSentance = insertStmtData[iInsertStmt].OriginalStatement + "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" + checkFileExistOrNot[0].FileId + ");'>[ " + fileName + " ]</a>";
                            //                }
                            //                else
                            //                {
                            //                    currentSentance = insertStmtData[iInsertStmt].OriginalStatement;
                            //                }

                            //                insertStmtData[iInsertStmt].OriginalStatement = currentSentance;
                            //                //insertStmtData[iInsertStmt].ResolvedStatement = currentSentance;
                            //                await
                            //                    _codeVortoService.StatementReferenceMasterRepository.UpdateItem(insertStmtData[iInsertStmt]);
                            //                #endregion
                            //            }
                            //        }
                            //    }
                            //}
                            //catch (Exception ex)
                            //{
                            //    ex.ToString();
                            //}
                            #endregion
                        }
                    }
                }
                catch (Exception ex)
                {
                    ex.ToString();
                }

                #endregion

                var processResult = await ProcessForObjectConnectivityDiagramData(projectId);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);

                //return Ok();
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessForObjectConnectivityDiagramData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
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
                            if (s.ClassNameDeclared.Contains(".jcl"))
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
                            if (s.ClassNameDeclared.Trim().Contains(".jcl"))
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

                                    var datatype = c.DataOrObjectType;
                                    var dataObject = c.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                    string[] strSplit = c.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                    string strName = "";
                                    if (strSplit.Length > 2)
                                    {
                                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
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

                                        var datatype = z.DataOrObjectType;
                                        var dataObject = z.ClassCalled.Replace("'", "").Replace(",", "").Trim();

                                        string[] strSplit = z.ClassCalled.Replace("'", "").Replace(",", "").Split('.');
                                        string strName = "";
                                        if (strSplit.Length > 2)
                                        {
                                            strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
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
                                "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared!='null'");

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
                                if (!s.ClassNameDeclared.Contains(".TXT"))
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

                                                    //break;
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
                            catch (Exception exception)
                            {
                                Console.WriteLine(exception.Message);
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

                #region region 2...To Updated ClassCalled Column in StatementreferenceMaster
                string execSqlNew =
                    "  Select * from statementreferencemaster where BaseCommandId=6 AND ProjectId = " + projectId + " and ClassCalled is not null;";
                var execClassCalled = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNew);
                foreach (var constructor in execClassCalled)
                {
                    var classname = constructor.ClassCalled;
                    constructor.ClassCalled = classname.Remove(classname.Remove(classname.Length - 1).LastIndexOf('.'));
                    await
                         _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                }
                #endregion

                #region region 2...To Updated ClassNameDeclared Column in StatementreferenceMaster
                string execSqlNewDec =
                    "  SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and BaseCommandId=19 and ClassNameDeclared != 'null';";
                var execClassNameDeclared = await
                    baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(execSqlNewDec);
                foreach (var constructor in execClassNameDeclared)
                {
                    var classnamedeclared = constructor.ClassNameDeclared;
                    constructor.ClassNameDeclared = classnamedeclared.Remove(classnamedeclared.Remove(classnamedeclared.Length - 1).LastIndexOf('.'));
                    await
                         _codeVortoService.StatementReferenceMasterRepository.UpdateItem(constructor);
                }
                #endregion

                // Start process for collecting workflow data here...
                IHttpActionResult processResult = await GetAllStartingPoints(projectId);
                await processResult.ExecuteAsync(CancellationToken.None);
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
                        "  and MethodStatementId != 0 AND (EndPointOrService = 'Service' OR EndPointOrService = 'Batch') AND CreatedBy=1 ; ");
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

        //[HttpGet]
        //public async Task<IHttpActionResult> GetWorkFlowWorkSpaceOLD(int projectId, int stmtId)
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {
        //        //var lstTreeViewData = new List<TreeViewData>();
        //        List<TreeView> lstTreeView = new List<TreeView>();
        //        List<TreeView> secondTab = new List<TreeView>();

        //        var startClasses =
        //            await _codeVortoService.ActionWorkflowsRepository
        //            .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
        //        var clsName =
        //            (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
        //        Expression<Func<StatementReferenceMaster, bool>> expression =
        //            master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName || master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
        //        var baseStatementMaster = await
        //            _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(expression, 1);
        //        //lstStatementMaster.Add(baseStatementMaster);
        //        //lstNodes.Add(new Node { Name = clsName, Id = 1111, ParentId = 0, ShapeId = "RoundRect", Color = "#28c965", Height = "15", Width = clsName.Length.ToString() });

        //        var workflowRef = GetMethodBlock(stmtId);

        //        int bId = workflowRef[0].BaseCommandId;
        //        int statementId = workflowRef[0].StatementId;
        //        int treeNodeId = 1;
        //        // This is class name where that method is defined...
        //        lstTreeView.Add(new TreeView
        //        {
        //            GraphId = "StartNode_1",
        //            GraphName = "<span class='nodeToBold'>" + baseStatementMaster.OriginalStatement + "</span>",
        //            HasChild = true,
        //            ParentId = "-1",
        //            BaseCommandId = baseStatementMaster.BaseCommandId,
        //            StatementReferenceMaster = workflowRef[0],
        //            SpriteCssClass = clsName,
        //            ActualStatementId = "Actual_" + baseStatementMaster.StatementId,
        //            NodeId = treeNodeId
        //        });
        //        // This is Method start statement...
        //        lstTreeView.Add(new TreeView
        //        {
        //            GraphId = "MethodNode_" + statementId,
        //            GraphName = "<span class='nodeToBold'>" + workflowRef[0].OriginalStatement + "</span>",
        //            HasChild = true,
        //            ParentId = "StartNode_1",
        //            BaseCommandId = bId,
        //            StatementReferenceMaster = workflowRef[0],
        //            ActualStatementId = "Actual_" + statementId,
        //            NodeId = ++treeNodeId
        //        });
        //        workflowRef.RemoveAt(0);

        //        lstTreeView.AddRange(workflowRef.Select(statementMaster => new TreeView
        //        {
        //            ActualStatementId = "Actual_" + statementMaster.StatementId,
        //            GraphId = "Node_" + statementMaster.StatementId,
        //            GraphName = statementMaster.OriginalStatement,
        //            HasChild = false,
        //            SpriteCssClass = "",
        //            ParentId = "MethodNode_" + statementId,
        //            BaseCommandId = statementMaster.BaseCommandId,
        //            PrimaryCommandId = statementMaster.PrimaryCommandId,
        //            ClassCalled = statementMaster.ClassCalled,
        //            MethodCalled = statementMaster.MethodCalled,
        //            StatementReferenceMaster = statementMaster,
        //            NodeId = ++treeNodeId
        //        }));
        //        var copyOfLstTreeView = new List<TreeView>();
        //        copyOfLstTreeView.AddRange(lstTreeView);
        //        int auto = 0;

        //        foreach (var treeItem in lstTreeView)
        //        {
        //            if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
        //                continue;

        //            auto++;
        //            copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
        //                ref auto, ref treeNodeId);
        //            if (!copyOfLstTreeView.Any()) continue;
        //            treeItem.HasChild = true;
        //        }

        //        var newList = copyOfLstTreeView.ToList();
        //        foreach (var treeItem in newList)
        //        {
        //            if (treeItem.BaseCommandId != 6) continue;
        //            // Very Imp: If want to take all items of tree, uncomment following 3 lines and comment rest of the code...
        //            auto++;
        //            copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
        //                ref auto, ref treeNodeId);
        //            if (!copyOfLstTreeView.Any()) continue;
        //            treeItem.HasChild = true;
        //            /*
        //            copyOfLstTreeView.Add(new TreeView
        //            {
        //                ParentId = treeItem.GraphId,
        //                GraphId = "CallExt_" + treeItem.StatementReferenceMaster.StatementId,
        //                HasChild = true,
        //                GraphName = "<span class='nodemissingelement'>&nbsp;Loading...&nbsp;</span>",
        //                BaseCommandId = 500,
        //                SpriteCssClass = treeItem.StatementReferenceMaster.StatementId.ToString()
        //            });
        //            treeItem.HasChild = true;
        //            */
        //        }
        //        #region Extra blocks...
        //        var indexPosition = -1;
        //        lstTreeView = copyOfLstTreeView;
        //        int ifCounter = 0;
        //        string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
        //        int tempId = 0;

        //        foreach (var treeItem in copyOfLstTreeView.FindAll(t => t.BaseCommandId == 6 || t.BaseCommandId == 5).ToList())
        //        {
        //            //string color = "#2d5b7" + indexPosition;                    
        //            treeItem.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeItem.GraphName +
        //                                 "</span>";
        //            var childItems =
        //                (from s in copyOfLstTreeView where s.ParentId == treeItem.GraphId select s).ToList();
        //            foreach (var child in childItems)
        //            {
        //                if (tempId > 3)
        //                    tempId = 0;
        //                if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
        //                {
        //                    treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
        //                                "</span>";
        //                    AssignColorsToChildNodes(child, ref copyOfLstTreeView,
        //                        colorArray[tempId]);
        //                    tempId++;
        //                }
        //                else
        //                    child.GraphName = "<span style='color: green;'>" + child.GraphName +
        //                                      "</span>";
        //            }
        //            //tempId++;
        //        }
        //        /*
        //        var allOtherCommand = copyOfLstTreeView.FindAll(s => s.StatementReferenceMaster.OtherBaseCommandId == 5);
        //        var oChildItems =
        //               (from s in copyOfLstTreeView where s.ParentId == allOtherCommand[0].GraphId select s).ToList();
        //        foreach (var cItem in oChildItems)
        //        {
        //            var m = cItem.GraphName;
        //        }
        //        */

        //        foreach (var treeItem in copyOfLstTreeView)
        //        {
        //            indexPosition++;
        //            if (treeItem.StatementReferenceMaster.OtherBaseCommandId == 5) continue;
        //            if (treeItem.BaseCommandId != 1 && treeItem.PrimaryCommandId != 1) continue;

        //            var treeViewList = new List<TreeView>();
        //            for (int i = indexPosition; i < lstTreeView.Count; i++)
        //            {
        //                treeViewList.Add(lstTreeView[i]);
        //                if (lstTreeView[i].BaseCommandId == 1)
        //                    ifCounter++;
        //                if (lstTreeView[i].BaseCommandId == 2)
        //                    ifCounter--;
        //                if (ifCounter == 0)
        //                    break;
        //            }
        //            var prevParentId = treeViewList.First().ParentId;
        //            var graphId = "IfBlockStart" + indexPosition + treeItem.ActualStatementId;
        //            treeViewList.First().GraphId = graphId;
        //            for (int j = 1; j < treeViewList.Count; j++)
        //            {
        //                if (treeViewList[j].ParentId == prevParentId)
        //                    treeViewList[j].ParentId = graphId;
        //            }
        //        }

        //        secondTab.Add(lstTreeView.ElementAt(0));
        //        secondTab.Add(lstTreeView.ElementAt(1));

        //        secondTab.AddRange(copyOfLstTreeView
        //            .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 ||
        //                           item.BaseCommandId == 1 || item.BaseCommandId == 25
        //                           || item.PrimaryCommandId == 1 || item.BaseCommandId == 5
        //                           || item.PrimaryCommandId == 2));
        //        var tempList =
        //            (from d in secondTab
        //             where d.BaseCommandId == 1
        //                 || d.BaseCommandId == 2
        //                 || d.BaseCommandId == 25
        //             select d).ToList();
        //        foreach (var sTab in tempList)
        //        {
        //            var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
        //            secondTab.AddRange(childItems);
        //            /*
        //            if (sTab.BaseCommandId == 2 || childItems.Count == 1)
        //                secondTab.Remove(sTab);
        //            if (childItems.Any(k => k.BaseCommandId == 25)) continue;
        //            if (!childItems.Any() && sTab.StatementReferenceMaster == null)
        //                secondTab.Remove(sTab);
        //            */
        //        }
        //        secondTab = secondTab.OrderBy(k => k.NodeId).ToList();
        //        /*
        //        var tempListNew = secondTab.ToList().FindAll(k => k.BaseCommandId == 1);
        //        foreach (var sTab in tempListNew)
        //        {
        //            var allChilds = new List<TreeView> { sTab };
        //            var childItems = (from s in secondTab where s.ParentId == sTab.GraphId select s).ToList();
        //            if (!childItems.Any())
        //                secondTab.Remove(sTab);
        //            else
        //            {
        //                foreach (var c in childItems)
        //                {
        //                    allChilds = AttachChildItems(allChilds, secondTab, c);
        //                }
        //                var s = (from child in allChilds where child.BaseCommandId == 6 || child.BaseCommandId == 5 select child).ToList();
        //                if (s.Any()) continue;
        //                foreach (var c in allChilds)
        //                {
        //                    secondTab.Remove(c);
        //                }
        //            }
        //        }
        //        */

        //        #endregion
        //        #region
        //        var allSeqListItems = new List<TreeView>();
        //        foreach (var curItem in secondTab)
        //        {
        //            allSeqListItems.Add(curItem);
        //            var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList();
        //            foreach (var cItem in childItems)
        //            {
        //                allSeqListItems = AttachChildItems(allSeqListItems, secondTab, cItem);
        //            }
        //            break;
        //        }

        //        int nodeId = 100;
        //        List<Node> listNodes = new List<Node>();
        //        List<Link> listLinks = new List<Link>();
        //        TreeViewData treeView = new TreeViewData(lstTreeView)
        //        {
        //            Nodes = listNodes,
        //            Links = listLinks
        //        };
        //        int widthCnt = Convert.ToInt32(secondTab.First().SpriteCssClass.Length.ToString()) * 3;
        //        if (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23)
        //        {
        //            string[] strSplit = secondTab.First().SpriteCssClass.Split('.');
        //            string strName = "";
        //            if (strSplit.Length > 2)
        //            {
        //                strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
        //            }
        //            treeView.Nodes.Add(new Node
        //            {
        //                Id = nodeId,
        //                Name = strName.ToUpper(),
        //                ShapeId = "Circle",
        //                Color = "#ffcc00",
        //                Width = widthCnt.ToString()
        //            });
        //        }
        //        else
        //        {
        //            treeView.Nodes.Add(new Node
        //            {
        //                Id = nodeId,
        //                Name = secondTab.First().SpriteCssClass,
        //                ShapeId = "Circle",
        //                Color = "#ffcc00",
        //                Width = widthCnt.ToString()
        //            });
        //        }

        //        allSeqListItems = allSeqListItems.Skip(1).ToList();
        //        var firstItem = allSeqListItems.First();
        //        var methodChildItems =
        //            (from a in allSeqListItems where firstItem.GraphId == a.ParentId select a).ToList();
        //        int linkSeqNumber = 1;
        //        foreach (var curItem in methodChildItems)
        //        {
        //            if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
        //            {
        //                #region PrimaryCommandId == 1 || BaseCommandId == 1
        //                string condition;
        //                string ifPart = Regex.Split(curItem.GraphName, "If", RegexOptions.IgnoreCase)[1];
        //                if (ifPart.Contains("Then"))
        //                {
        //                    condition = ifPart.Substring(0,
        //                        ifPart.IndexOf("Then",
        //                            StringComparison.InvariantCulture));
        //                }
        //                else
        //                    condition = ifPart;

        //                nodeId++;
        //                var node = new Node
        //                {
        //                    Id = nodeId,
        //                    ShapeId = "Decision",
        //                    Name = condition,
        //                    Color = "#ff6600"
        //                };

        //                treeView.Nodes.Add(node);
        //                treeView.Links.Add(new Link
        //                {
        //                    Origin = nodeId - 1,
        //                    //Origin = treeView.Nodes.First().Id,
        //                    Target = nodeId,
        //                    LinkText = "[" + linkSeqNumber + "] "
        //                });
        //                linkSeqNumber++;
        //                var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
        //                foreach (var cItem in childItems)
        //                {
        //                    treeView = ProcessChildItemsIf(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
        //                }
        //                #endregion
        //            }
        //            else if (curItem.BaseCommandId == 6)
        //            {
        //                #region BaseCommandId == 6
        //                var item = curItem;
        //                string nodeColor = "#c0c0c0";
        //                var classNameDeclared = await _codeVortoService.StatementReferenceMasterRepository
        //                    .GetAllItems(s => s.BaseCommandId == 19
        //                                      && s.ClassNameDeclared == item.StatementReferenceMaster
        //                                          .ClassCalled.Split('.').LastOrDefault() ||
        //                                      s.ClassNameDeclared == item.StatementReferenceMaster
        //                                          .ClassCalled);
        //                if (classNameDeclared.Count() != 0)
        //                    nodeColor = "#00ffff";
        //                nodeId++;
        //                Node node;
        //                if (curItem.PrimaryCommandId == 25)
        //                {
        //                    string[] strSplit = curItem.StatementReferenceMaster.ClassCalled.Split('.');
        //                    string strName = "";
        //                    if (strSplit.Length > 2)
        //                    {
        //                        strName = strSplit[strSplit.Length - 2] + "." + strSplit[strSplit.Length - 1];
        //                    }
        //                    node = new Node
        //                    {
        //                        Id = nodeId,
        //                        ShapeId = "RoundRect",
        //                        Name = strName.ToUpper(),
        //                        Color = nodeColor
        //                    };
        //                }
        //                else
        //                {
        //                    node = new Node
        //                    {
        //                        Id = nodeId,
        //                        ShapeId = "RoundRect",
        //                        Name = curItem.StatementReferenceMaster.ClassCalled,
        //                        Color = nodeColor
        //                    };
        //                }
        //                treeView.Nodes.Add(node);
        //                string m = curItem.StatementReferenceMaster.MethodCalled;
        //                if (m != null)
        //                {
        //                    treeView.Links.Add(new Link
        //                    {
        //                        Origin = treeView.Nodes.First().Id,
        //                        Target = nodeId,
        //                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
        //                    });
        //                }
        //                else
        //                {
        //                    treeView.Links.Add(new Link
        //                    {
        //                        Origin = treeView.Nodes.First().Id,
        //                        Target = nodeId,
        //                        LinkText = "[" + linkSeqNumber + "] "
        //                    });
        //                }
        //                linkSeqNumber++;
        //                var childItems = (from s in secondTab
        //                                  where s.ParentId == curItem.GraphId
        //                                      && s.BaseCommandId != 25
        //                                  select s).ToList().Distinct();
        //                foreach (var cItem in childItems)
        //                {
        //                    treeView = ProcessChildItemsCallExt(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
        //                }
        //                #endregion
        //            }
        //            else if (curItem.BaseCommandId == 5)
        //            {
        //                #region BaseCommandId == 5
        //                var item = curItem;
        //                string nodeColor = "#c0c0c0";
        //                var methodCalled = await _codeVortoService.StatementReferenceMasterRepository
        //                    .GetDataFromSqlQuery<StatementReferenceMaster>(
        //                        "Select * from statementreferencemaster Where BaseCommandId = 19 ANd FileId = " + curItem.StatementReferenceMaster.FileId + ";");
        //                if (methodCalled.Count != 0)
        //                    nodeColor = "#f5bd6a";
        //                nodeId++;
        //                var node = new Node
        //                {
        //                    Id = nodeId,
        //                    ShapeId = "RoundRect",
        //                    Name = methodCalled[0].ClassNameDeclared,
        //                    Color = nodeColor
        //                };
        //                treeView.Nodes.Add(node);
        //                string m = item.StatementReferenceMaster.MethodCalled;
        //                if (m != null)
        //                {
        //                    m = m + "()";
        //                    treeView.Links.Add(new Link
        //                    {
        //                        Origin = treeView.Nodes.First().Id,
        //                        Target = nodeId,
        //                        LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
        //                    });
        //                }
        //                else
        //                {
        //                    treeView.Links.Add(new Link
        //                    {
        //                        Origin = treeView.Nodes.First().Id,
        //                        Target = nodeId,
        //                        LinkText = "[" + linkSeqNumber + "] "
        //                    });
        //                }
        //                linkSeqNumber++;
        //                var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
        //                foreach (var cItem in childItems)
        //                {
        //                    treeView = ProcessChildItemsCallInternal(projectId, allSeqListItems, cItem, treeView, node, ref nodeId, ref linkSeqNumber);
        //                }
        //                #endregion
        //            }
        //        }

        //        treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
        //        treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First()); // RemoveNodes(treeView.Nodes, treeView.Links);
        //        #endregion

        //        //Expression<Func<ActionWorkflows, bool>> actionExpression = e => e.MethodStatementId == statementId &&
        //        //                                                          e.ProjectId == projectId;
        //        var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
        //            .GetEntityData<ActionWorkflows>(s => s.MethodStatementId == statementId && s.ProjectId == projectId);
        //        List<TreeViewData> lstData = new List<TreeViewData>();
        //        TreeViewData treeViewDataNew = new TreeViewData(secondTab)
        //        {
        //            Nodes = treeView.Nodes,
        //            Links = treeView.Links,
        //            ActionWorkflows = actionWorkflow.First()
        //        };
        //        lstData.Add(treeViewDataNew);
        //        lstData.Add(new TreeViewData(lstTreeView));
        //        return Ok(lstData);
        //    }
        //}

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
                    NodeId = treeNodeId
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
                    NodeId = ++treeNodeId
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
                    NodeId = ++treeNodeId
                }));
                var copyOfLstTreeView = new List<TreeView>();
                copyOfLstTreeView.AddRange(lstTreeView);
                int auto = 0;

                foreach (var treeItem in lstTreeView)
                {
                    if (treeItem.BaseCommandId != 5 && treeItem.StatementReferenceMaster.OtherBaseCommandId != 5)
                        continue;

                    auto++;
                    //copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
                    //    ref auto, ref treeNodeId);
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId, treeItem.StatementReferenceMaster.FileId,
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
                    copyOfLstTreeView = GetCallExternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
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
                lstTreeView = copyOfLstTreeView;
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
                        if (child.BaseCommandId == 6 || child.BaseCommandId == 5)
                        {
                            string groupName = string.Empty;
                            if (!string.IsNullOrEmpty(child.StatementReferenceMaster.MethodCalled))
                            {
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
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
                                        "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView, colorArray[tempId], groupName, groupId);
                            tempId++;
                        }
                        else
                            child.GraphName = "<span style='color: green;'>" + child.GraphName +
                                              "</span>";
                    }
                    //tempId++;
                }
                /*
                var allOtherCommand = copyOfLstTreeView.FindAll(s => s.StatementReferenceMaster.OtherBaseCommandId == 5);
                var oChildItems =
                       (from s in copyOfLstTreeView where s.ParentId == allOtherCommand[0].GraphId select s).ToList();
                foreach (var cItem in oChildItems)
                {
                    var m = cItem.GraphName;
                }
                */

                foreach (var treeItem in copyOfLstTreeView)
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
                        if (treeViewList[j].ParentId == prevParentId)
                            treeViewList[j].ParentId = graphId;
                    }
                }

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                secondTab.AddRange(copyOfLstTreeView
                    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 || item.BaseCommandId == 8 ||
                                   item.BaseCommandId == 1 || item.BaseCommandId == 25
                                   || item.PrimaryCommandId == 1 || item.BaseCommandId == 5
                                   || item.PrimaryCommandId == 2));
                var tempList =
                    (from d in secondTab
                     where d.BaseCommandId == 1
                         || d.BaseCommandId == 2
                         || d.BaseCommandId == 25
                     select d).ToList();
                foreach (var sTab in tempList)
                {
                    var childItems = (from s in copyOfLstTreeView where s.ParentId == sTab.GraphId select s).ToList();
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
                if (secondTab.First().StatementReferenceMaster.PrimaryCommandId == 23 || secondTab.First().StatementReferenceMaster.PrimaryCommandId == 36)
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
                var methodChildItems =
                    (from a in allSeqListItems.Distinct() where firstItem.GraphId == a.ParentId select a).ToList().Distinct();
                int linkSeqNumber = 1;
                foreach (var curItem in methodChildItems)
                {
                    string nm = curItem.GraphName.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                    if (!string.IsNullOrEmpty(nm))
                    {
                        if (curItem.PrimaryCommandId == 1 || curItem.BaseCommandId == 1)
                        {
                            string width = "0";
                            string height = "0";

                            #region PrimaryCommandId == 1 || BaseCommandId == 1
                            string condition;
                            string ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                            if (ifPart.Contains("THEN"))
                            {
                                condition = ifPart.Substring(0, ifPart.IndexOf("THEN", StringComparison.InvariantCulture));
                            }
                            else
                                condition = ifPart;

                            nodeId++;

                            int charCountOfText = condition.Length;
                            if (charCountOfText > 0)
                            {
                                int widthCalculation = (charCountOfText * 2) + 20;
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
                            if (curItem.PrimaryCommandId == 25 || curItem.PrimaryCommandId == 38)
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
                            if (m != null)
                            {
                                treeView.Links.Add(new Link
                                {
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('(')),
                                    StatementId = Int32.Parse(treeView.TreeViewList.Last().ActualStatementId.Split('_')[1])
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
                            if (curItem.PrimaryCommandId == 24 || curItem.PrimaryCommandId == 37)
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
                    //Name = node.Name,
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


                #region To Updated the Node Name against the BaseCommandId=1 [Remove the HTML tags]

                var statementNodes = await generalRepositoryNodeDetails.GetAllItems(p => p.ProjectId == projectId && p.WorkflowStartStatementId == statementId);

                foreach (var stmt in statementNodes)
                {
                    if (stmt.ShapeId == "Decision" || stmt.ShapeId == "Decision2")
                    {
                        Regex word = new Regex(@"\<span ([\w\(\)\\_\,\<\#\;\:\&\$\-\.\'\\+\=EQ\sEQUAL\sAND\sNOT\sOR\s\/]*)\>");
                        var removeHtmlTag = "";
                        foreach (Match match in word.Matches(stmt.Name))
                        {
                            removeHtmlTag = match.Value;
                            stmt.Name = stmt.Name.Replace(removeHtmlTag, "");
                        }

                        if (removeHtmlTag != "")
                        {
                            stmt.Name = stmt.Name.Replace("</span>", "").Replace(" LT", " is less than").Replace(" GT", " is greater than").Replace("GE", " is greater than or equal to").Replace(" #", " not equal to") + "?";
                        }
                        else
                        {
                            stmt.Name = stmt.Name.Replace(" LT", " is less than").Replace(" GT", " is greater than").Replace("GE", " is greater than or equal to").Replace(" #", " not equal to") + "?";
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
                    GraphName = sTab.GraphName,
                    //GraphName = sTab.GraphName + "&nbsp;<img id='imgpseudo' src='../images/regex_icon.png' onclick='PseudoCodeDialog(" + sTab.ActualStatementId.Split('_')[1] + ")'/>",
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    StatementId = sTab.StatementReferenceMaster.StatementId
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
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId
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
                lstData.Add(treeViewDataNew);s
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */

                #region Decision Chart Code
                //IHttpActionResult decisionChartResult =
                //        await GetDecisionChart(projectId, stmtId);
                //await decisionChartResult.ExecuteAsync(CancellationToken.None);
                #endregion

                return Ok("Workflow data collected successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetDecisionChart(int iProjectId, int iStatementId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                DataSet dsReturnStringAndFilePath = new DataSet();
                var path = ConfigurationManager.AppSettings["GlobalPath"];
                string datetime = DateTime.Now.ToString("MM-dd-yyyy_HH-mm-ss");
                string saveUrlPath = string.Empty;
                #region Variable Declaration

                string finalFileName = string.Empty;
                string outPutString = string.Empty;
                string outPutStringxml = string.Empty;

                #endregion

                //var serverPath = Request.Headers.Referrer.Segments[1];
                //var serverPath = Request.Headers.Host;
                //if (Request.Headers.Referrer.Host.Contains("192.168.0.15") || Request.Headers.Referrer.Host.Contains("localhost"))
                //if (Request.Headers.Host.Contains("192.168.0.15") || Request.Headers.Host.Contains("localhost"))
                //{
                //    //saveUrlPath =
                //    //    @"D:\CodeVortoProjects\trunk\CodeVortoWebClientApplication\CodeVortoWebClientApplication\Downloads\";

                //    saveUrlPath =
                //        @"D:\BRIEN\VERTO_LATEST\trunk\CodeVortoWebClientApplication\CodeVortoWebClientApplication\DecisionTable\";
                //}
                //else
                //{
                //serverPath = serverPath.Replace("/", "\\");
                //saveUrlPath = saveUrlPath + path.Replace("ApplicationName//", serverPath);
                //}

                saveUrlPath = path.Replace("/", "\\");

                try
                {
                    if (iStatementId > 0 && iProjectId > 0)
                    {
                        GeneralRepository<WorkflowTreeviewSecondTabDetails> generalRepositoryWorkflowTreeviewTabFirstDetails
                       = new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());

                        string sqlQuery = " SELECT * FROM workflowtreeviewsecondtabdetails where WorkflowStartStatementId='" + iStatementId + "' and BaseCommandId In (1)" +
                       " and ProjectId='" + iProjectId + "' Union All" +
                       " SELECT * FROM workflowtreeviewsecondtabdetails where WorkflowStartStatementId='" + iStatementId + "' and GraphName like '%Else%'" +
                       " and ProjectId='" + iProjectId + "' order by RowId;";

                        var dsConditions = generalRepositoryWorkflowTreeviewTabFirstDetails.GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(sqlQuery);

                        #region Creating the required format DATA in DataSet --
                        DataSet dsTemp = new DataSet();
                        if (dsConditions.Result.Count > 0)
                        {
                            // 1. Fill DataTable with Only Conditional statements
                            #region -- Initializing Variable's --
                            DataTable dtConditionalStatementsCount = new DataTable();
                            dtConditionalStatementsCount.Columns.Add("GraphId", typeof(string));
                            dtConditionalStatementsCount.Columns.Add("ActualId", typeof(string));
                            dtConditionalStatementsCount.Columns.Add("GraphName", typeof(string));
                            dtConditionalStatementsCount.Columns.Add("ParentId", typeof(string));
                            dtConditionalStatementsCount.Columns.Add("BaseCommandId", typeof(int));
                            dtConditionalStatementsCount.Columns.Add("Sequence", typeof(Int32));
                            int idd = 0;
                            #endregion

                            #region -- filling DT --

                            #region ==Adding CONDITION Row Brut-Force==
                            DataRow dtrowCA = dtConditionalStatementsCount.NewRow(); // Create New Row
                            dtrowCA["GraphId"] = "0";
                            dtrowCA["ActualId"] = "Business Rule";
                            dtrowCA["GraphName"] = "Workflow Tasks / Decision Points";
                            dtrowCA["ParentId"] = "0";
                            dtrowCA["BaseCommandId"] = "0";
                            dtrowCA["Sequence"] = "0";

                            dtConditionalStatementsCount.Rows.Add(dtrowCA);
                            #endregion

                            for (int iState = 0; iState < dsConditions.Result.Count; iState++)
                            {
                                string strStatement = Regex.Replace(dsConditions.Result[iState].GraphName, "<.*?>", String.Empty);
                                string parentID = dsConditions.Result[iState].GraphId;
                                string parentIDProcedureDiv = dsConditions.Result[iState].ParentId;
                                int BaseCommandId = dsConditions.Result[iState].BaseCommandId;
                                string ActualId = dsConditions.Result[iState].ActualStatementId.ToUpper().Replace("ACTUAL_", "");

                                if ((dsConditions.Result[iState].BaseCommandId == 1 || strStatement.ToUpper().StartsWith("ELSE") || strStatement.ToUpper().StartsWith("END ELSE")))
                                {
                                    DataRow dtrow = dtConditionalStatementsCount.NewRow(); // Create New Row
                                    dtrow["GraphId"] = parentID;
                                    dtrow["ActualId"] = ActualId;
                                    dtrow["GraphName"] = strStatement.Trim();
                                    dtrow["ParentId"] = parentIDProcedureDiv;
                                    dtrow["BaseCommandId"] = BaseCommandId;
                                    dtrow["Sequence"] = iState + 1;
                                    dtConditionalStatementsCount.Rows.Add(dtrow);
                                }
                            }

                            #region ==Adding ACTION Row Brut-Force==
                            DataRow dtrowAct = dtConditionalStatementsCount.NewRow(); // Create New Row
                            dtrowAct["GraphId"] = "0";
                            dtrowAct["ActualId"] = "0";
                            dtrowAct["GraphName"] = "Actions";
                            dtrowAct["ParentId"] = "0";
                            dtrowAct["BaseCommandId"] = "0";
                            dtrowAct["Sequence"] = "0";

                            dtConditionalStatementsCount.Rows.Add(dtrowAct);
                            #endregion

                            #endregion

                            #region--added by ASHISH --
                            if (dtConditionalStatementsCount.Rows.Count > 0)
                            {
                                for (int iState = 0; iState < dtConditionalStatementsCount.Rows.Count - 2; iState++)
                                //for (int iState = 0; iState < dtConditionalStatementsCount.Rows.Count; iState++)
                                {
                                    //added +4 : It will be helpful later on when adding the value T,F and A values in the respective columns
                                    dtConditionalStatementsCount.Columns.Add(iState + 7 + "TrueOrFalse");
                                    dtConditionalStatementsCount.AcceptChanges();
                                }
                            }
                            #endregion

                            #region -- filling out DT --
                            for (int iWork = 0; iWork < dsConditions.Result.Count; iWork++)
                            {
                                //if (chkCount > 0)
                                //{

                                string strStatement = dsConditions.Result[iWork].GraphName.ToString();
                                idd = iWork + 1;
                                string parentID = dsConditions.Result[iWork].GraphId;
                                string parentIDProcedureDiv = dsConditions.Result[iWork].ParentId;

                                string sqlQueryNew = "SELECT * FROM workflowtreeviewsecondtabdetails WHERE ProjectId='" + iProjectId + "' and WorkflowStartStatementId='" + iStatementId + "' and ParentId='" + parentID + "' and BasecommandId Not In (1,2);";
                                var dsStatementsChilds = generalRepositoryWorkflowTreeviewTabFirstDetails.GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(sqlQueryNew);

                                if ((dsConditions.Result[iWork].BaseCommandId != 6) || (dsConditions.Result[iWork].BaseCommandId != 1) || Convert.ToInt32(parentIDProcedureDiv) > 0)
                                {
                                    if (dsStatementsChilds.Result.Count > 0)
                                    {
                                        for (int iChild = 0; iChild < dsStatementsChilds.Result.Count; iChild++)
                                        {
                                            DataRow dtrow = dtConditionalStatementsCount.NewRow(); // Create New Row
                                            //dtrow["Id"] = idd;             //Bind Data to Columns
                                            dtrow["GraphId"] = dsStatementsChilds.Result[iChild].GraphId;
                                            dtrow["ActualId"] = "0";
                                            dtrow["GraphName"] = dsStatementsChilds.Result[iChild].GraphName.ToString();
                                            dtrow["ParentId"] = dsStatementsChilds.Result[iChild].ParentId;
                                            dtrow["BaseCommandId"] = dsStatementsChilds.Result[iChild].BaseCommandId;
                                            dtrow["Sequence"] = "0";
                                            dtConditionalStatementsCount.Rows.Add(dtrow);
                                        }
                                    }
                                }
                                //}
                            }
                            #endregion


                            #region --

                            for (int iWork = 0; iWork < dtConditionalStatementsCount.Rows.Count; iWork++)
                            {
                                string strStatement = dtConditionalStatementsCount.Rows[iWork]["GraphName"].ToString();
                                idd = int.Parse(dtConditionalStatementsCount.Rows[iWork]["Sequence"].ToString());
                                string parentID = dtConditionalStatementsCount.Rows[iWork]["GraphId"].ToString();
                                string parentIDProcedureDiv = dtConditionalStatementsCount.Rows[iWork]["ParentId"].ToString();
                                int baseCommandId = int.Parse(dtConditionalStatementsCount.Rows[iWork]["BaseCommandId"].ToString());
                                string sequenceId = idd.ToString();

                                #region -- CHECK IF--
                                //if ((strStatement.ToUpper().StartsWith("CHECK IF ")) && (strStatement.ToUpper().StartsWith("ELSE") == false))
                                if (baseCommandId == 1)
                                {

                                    try
                                    {
                                        dtConditionalStatementsCount.Rows[iWork][6 + iWork + "TrueOrFalse"] = "T";
                                        //dtConditionalStatementsCount.Rows[iWork][5 + iWork + "R"] = sequenceId;
                                    }
                                    catch (Exception ex)
                                    {
                                        ex.ToString();
                                    }

                                    //check 2 things: 
                                    //1. It's Statement
                                    //2. It's child 
                                    #region -- 1. It's Statements & It's Child
                                    //DataSet dsActions = new DataSet();
                                    //dsActions = objFDVerbose.getActionsStatementIDOfThisCheckIFCSAdvantageWeb(parentID);

                                    string sqlQueryNew = "SELECT * FROM workflowtreeviewsecondtabdetails WHERE ProjectId='" + iProjectId + "' and WorkflowStartStatementId='" + iStatementId + "' and ParentId='" + parentID + "';";
                                    var dsActions = generalRepositoryWorkflowTreeviewTabFirstDetails.GetDataFromSqlQuery<WorkflowTreeviewSecondTabDetails>(sqlQueryNew);


                                    if (dsActions != null && dsActions.Result.Count > 0)
                                    {
                                        int flagElse = 0;
                                        for (int jDSAct = 0; jDSAct < dsActions.Result.Count; jDSAct++)
                                        {
                                            string iFDVerboseID = dsActions.Result[jDSAct].GraphId;
                                            if (iFDVerboseID.Length > 0)
                                            {
                                                for (int jWork = 0; jWork < dtConditionalStatementsCount.Rows.Count; jWork++)
                                                {
                                                    if (dtConditionalStatementsCount.Rows[jWork]["GraphId"].ToString() == iFDVerboseID)
                                                    {
                                                        string elsePart = Regex.Replace(dsActions.Result[jDSAct].GraphName.ToString().Trim().ToUpper(), "<.*?>", String.Empty);
                                                        if (elsePart.StartsWith("ELSE") || elsePart.StartsWith("END ELSE"))
                                                        {
                                                            //dtConditionalStatementsCount.Rows[iWork][jWork + 5 + "R"] = "F";
                                                            dtConditionalStatementsCount.Rows[iWork][6 + iWork + 1 + "TrueOrFalse"] = "F";
                                                            flagElse = 1;
                                                            //mark the statements against this else
                                                        }
                                                        else if (dsActions.Result[jDSAct].BaseCommandId == 1)
                                                        //if ((dsActions.Result[jDSAct]["Statement"].ToString().Trim().ToUpper().StartsWith("CHECK IF ")))
                                                        {
                                                            dtConditionalStatementsCount.Rows[iWork][jWork + 6 + "TrueOrFalse"] = "T";

                                                            //dtConditionalStatementsCount.Rows[iWork][jWork + 5 + "R"] = sequenceId;
                                                            // Yes.. this is right that you are doing this as T, but what about it's all child elements?
                                                            // Need to add function here again..
                                                            // Do it in later loops
                                                        }
                                                        else if (dsActions.Result[jDSAct].BaseCommandId == 2)
                                                        {
                                                            flagElse = 0;
                                                        }
                                                        else
                                                        {
                                                            //dtConditionalStatementsCount.Rows[jWork][iWork + 5 + "R"] = "A";
                                                            if (flagElse == 1)
                                                            {
                                                                dtConditionalStatementsCount.Rows[jWork][iWork + 1 + 6 + "TrueOrFalse"] = sequenceId;
                                                            }
                                                            else
                                                            {
                                                                dtConditionalStatementsCount.Rows[jWork][iWork + 6 + "TrueOrFalse"] = sequenceId;
                                                            }
                                                        }
                                                        break;
                                                    }
                                                }

                                            }
                                        }
                                    }

                                    #endregion
                                }
                                #endregion

                            }

                            #region -- Deleting the Else Statements--
                            for (int i = dtConditionalStatementsCount.Rows.Count - 1; i >= 0; i--)
                            {
                                DataRow dr = dtConditionalStatementsCount.Rows[i];
                                string elsePart = Regex.Replace(dr["GraphName"].ToString().Trim().ToUpper(), "<.*?>", String.Empty);
                                if (elsePart.StartsWith("ELSE") || elsePart.StartsWith("END ELSE"))
                                {
                                    dr.Delete();
                                    dtConditionalStatementsCount.AcceptChanges();
                                }
                            }
                            #endregion

                            #endregion

                            dsTemp.Tables.Add(dtConditionalStatementsCount);

                        }

                        #endregion

                        #region -- Formatting the DataSet to desired output --

                        if (dsTemp != null && dsTemp.Tables.Count > 0 && dsTemp.Tables[0].Rows.Count > 0)
                        {
                            DataSet dsTemp1 = new DataSet();
                            if (dsTemp1 != null && dsTemp1.Tables.Count > 0 && dsTemp1.Tables[0].Rows.Count > 0)
                            {
                                dsTemp = dsTemp1;
                            }
                        }


                        #endregion

                        #region --Converting the Dataset in required HTML format --
                        // Reason is: We have Code in HTML inside the jQWidgets DataTable control's Event.

                        try
                        {
                            //var generalRepository =
                            //    new GeneralRepository<StatementRuleReference>();
                            //var allRuleRef =
                            //    generalRepository.GetDataFromSqlQuery<StatementRuleReference>(
                            //        "Select * from StatementRuleReference;").Result;
                            //var statementIdsFrom = (from a in allRuleRef select a.StatementIdFrom).ToList();
                            if (dsTemp.Tables.Count > 0 && dsTemp.Tables[0].Rows.Count > 0)
                            {
                                outPutString = "";
                                //outPutString += "<html>";
                                //outPutString += "<body>";
                                outPutString += "<Table id='tblDecision' style='border: 1px solid #AAA; table-layout:fixed; border-collapse:collapse;'>";
                                outPutString += "<tr>";
                                outPutString += "<td class='td1' style='background-color: #4D92EA;'>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td class='td2' style='background-color: #4D92EA;'>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td class='td3' style='background-color: #4D92EA;'>";
                                outPutString += "Title/Statements";
                                outPutString += "</td>";
                                outPutString += "<td class='td4' style='background-color: #4D92EA;'>";
                                //outPutString += "TrueOrFalse";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td class='td5' style='background-color: #4D92EA;'>";
                                //outPutString += "TrueOrFalse";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "</tr>";
                                outPutString += "<tr>";
                                outPutString += "<td>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "<td>";
                                outPutString += "";
                                outPutString += "</td>";
                                outPutString += "</tr>";

                                int checkActionCondition = 0;
                                for (int iDS = 0; iDS < dsTemp.Tables[0].Rows.Count; iDS++)
                                {
                                    outPutString += "<tr> ";

                                    string strCurrentStatementID = "";
                                    string strCurrentStatement = "";
                                    string strSequenceId = "";
                                    string strBaseCommandId = "";
                                    string strType = "";

                                    string strTF = "";
                                    string strActualId = "";

                                    int strId = 0;
                                    string typeColor = "";

                                    strCurrentStatementID = dsTemp.Tables[0].Rows[iDS]["GraphId"].ToString().Trim();
                                    var startStmtId = strCurrentStatementID.Split('_').LastOrDefault();
                                    strCurrentStatement = dsTemp.Tables[0].Rows[iDS]["GraphName"].ToString().Trim();
                                    strSequenceId = dsTemp.Tables[0].Rows[iDS]["Sequence"].ToString().Trim();
                                    strBaseCommandId = dsTemp.Tables[0].Rows[iDS]["BaseCommandId"].ToString().Trim();
                                    strActualId = dsTemp.Tables[0].Rows[iDS]["ActualId"].ToString().Trim();

                                    if (strBaseCommandId == "1")
                                    {
                                        strType = "DECISION";
                                        typeColor = "#cc9900";
                                    }
                                    else if (strBaseCommandId == "5")
                                    {
                                        strType = "INTERNAL";
                                        typeColor = "#b3ffb3";
                                    }
                                    else if (strBaseCommandId == "6")
                                    {
                                        strType = "EXTERNAL";
                                        typeColor = "#ff9900";
                                    }
                                    else
                                    {
                                        strType = "PRIMARY";
                                        typeColor = "#99ccff";
                                    }

                                    strCurrentStatement = Regex.Replace(dsTemp.Tables[0].Rows[iDS]["GraphName"].ToString().Trim(), "<.*?>", String.Empty);

                                    if (strCurrentStatement.StartsWith("Workflow Tasks / Decision Points"))
                                    {
                                        checkActionCondition = 1;
                                    }
                                    else if (strCurrentStatement.StartsWith("Actions"))
                                    {
                                        checkActionCondition = 2;
                                    }

                                    if (strCurrentStatement.StartsWith("Workflow Tasks / Decision Points"))
                                    {
                                        outPutString += "<td class='td6' style='background-color: #D1D1D1;'>Seq#</td>";
                                        outPutString += "<td class='td7' style='background-color: #D1D1D1;'>&nbsp;Business <br> Function</td>";
                                    }
                                    else if (strCurrentStatement.StartsWith("Actions"))
                                    {
                                        outPutString += "<td class='td8' style='background-color: #D1D1D1;'>&nbsp;</td>";
                                        outPutString += "<td class='td8' style='background-color: #D1D1D1;'>&nbsp;</td>";
                                    }
                                    else
                                    {
                                        if (strSequenceId != "0")
                                        {
                                            outPutString += "<td class='td9' style='background-color: #ffe066;'>" + strSequenceId + "</td>";
                                        }
                                        else
                                        {
                                            outPutString += "<td class='td9' style='background-color: #b3ffb3;'>&nbsp;</td>";
                                        }

                                        if (strActualId != "0")
                                        {
                                            //if (statementIdsFrom.Any(s => startStmtId != null && s == int.Parse(startStmtId)))
                                            //{
                                            //    outPutString += "<td class='td10' id=" + strCurrentStatementID +
                                            //                    " style='background-color: #800000;'></td>";
                                            //}
                                            //else
                                            //{

                                            outPutString += "<td class='td10' id=" + strCurrentStatementID +
                                                            " style='background-color: #ffe066;'><input id='btnDefineRule' " +
                                                            " type='button' class='btn btn-primary btn-sm' style='height: 25px; margin: 3px;' value='Define' onclick='funDefineBusinessRule(" +
                                                            strActualId + ", this);' /></td>";
                                            //}
                                        }
                                        else
                                        {
                                            outPutString += "<td class='td9' style='background-color: #b3ffb3;'>&nbsp;</td>";
                                        }
                                    }

                                    if ((strCurrentStatement.StartsWith("Workflow Tasks / Decision Points")) || (strCurrentStatement.StartsWith("Actions")))
                                    {
                                        outPutString += "<td class='td11' style='background-color: #D1D1D1;'> ";
                                        //outPutString += "TITLE: " + strCurrentStatement;
                                        outPutString += strCurrentStatement;
                                    }
                                    else
                                    {
                                        if (checkActionCondition == 1)
                                        {
                                            outPutString += "<td class='td12' style='background-color: #ffe066;'> ";
                                        }
                                        else if (checkActionCondition == 2)
                                        {
                                            outPutString += "<td class='td12' style='background-color: #b3ffb3;'> ";
                                        }

                                        outPutString += strCurrentStatement;
                                    }
                                    outPutString += "</td>";

                                    if ((strCurrentStatement.StartsWith("Workflow Tasks / Decision Points")))
                                    {
                                        outPutString += "<td class='td8' style='background-color: #D1D1D1;'>Type</td>";
                                    }
                                    else if ((strCurrentStatement.StartsWith("Actions")))
                                    {
                                        outPutString += "<td class='td8' style='background-color: #D1D1D1;'></td>";
                                    }
                                    else
                                    {
                                        outPutString += "<td class='td9' style='background-color: " + typeColor + ";'>" + strType + "</td>";
                                    }

                                    outPutString += "<td class='td13'>";
                                    outPutString += "<Table>";
                                    outPutString += "<tr>";
                                    for (int jColumn = 6; jColumn < dsTemp.Tables[0].Columns.Count; jColumn++)
                                    {
                                        strId++;
                                        strTF = "";
                                        string strStatement = "";
                                        strStatement = dsTemp.Tables[0].Rows[iDS]["GraphName"].ToString().Trim();
                                        strTF = dsTemp.Tables[0].Rows[iDS][jColumn].ToString().Trim();

                                        if (strTF.ToString().Trim() == "")
                                        {
                                            strTF = " ";
                                        }

                                        if ((strStatement.StartsWith("Workflow Tasks / Decision Points")))
                                        {
                                            outPutString += "<td class='td14' style='background-color: #D1D1D1;height:30px;width:30px;text-align:center;''>";
                                            //outPutString += "<td class='td14' style='background-color: #D1D1D1;'>";
                                            if (strId.ToString().Length == 1)
                                            {
                                                outPutString += "" + "R" + strId + "";
                                            }
                                            else if (strId.ToString().Length == 2)
                                            {
                                                outPutString += "R" + strId + "";
                                            }
                                            else if (strId.ToString().Length > 2)
                                            {
                                                outPutString += "R" + strId + "";
                                            }
                                            outPutString += "</td>";
                                        }
                                        else if (strStatement.StartsWith("Actions"))
                                        {
                                            outPutString += "<td class='td15' style='background-color: #D1D1D1;height:35px;width:30px;text-align:center;'>";
                                            //outPutString += "<td class='td15' style='background-color: #D1D1D1;'>";
                                            outPutString += "";
                                            outPutString += "</td>";
                                        }
                                        else if (strTF.Contains("T"))
                                        //else if (strTF.Length > 0 && strTF!="N")
                                        {
                                            outPutString += "<td class='td15' style='background-color: #39ac39;height:35px;width:30px;text-align:center;'>";
                                            //outPutString += "<td class='td15' style='background-color: #39ac39;'>";
                                            outPutString += "" + strTF + "";
                                            outPutString += "</td>";
                                        }
                                        else if (strTF.Contains("F"))
                                        {
                                            outPutString += "<td class='td15' style='background-color: #FF5757;height:35px;width:30px;text-align:center;'>";
                                            //outPutString += "<td class='td15' style='background-color: #FF5757;'>";
                                            outPutString += "" + strTF + "";
                                            outPutString += "</td>";
                                        }
                                        else if (strTF.Length > 0 && strTF != "N" && strTF != " ")
                                        {
                                            outPutString += "<td class='td15' style='background-color: #39ac39;height:35px;width:30px;text-align:center;'>";
                                            //outPutString += "<td class='td15' style='background-color: #39ac39;'>";
                                            outPutString += "" + strTF + "";
                                            outPutString += "</td>";
                                        }

                                        else
                                        {
                                            if (strStatement.ToUpper().StartsWith("CHECK IF"))
                                            {
                                                outPutString += "<td class='td15' style='background-color: #B7FFEF;height:35px;width:30px;text-align:center;'>";
                                                //outPutString += "<td class='td15' style='background-color: #B7FFEF;'>";
                                                outPutString += "" + strTF + "";
                                                outPutString += "</td>";
                                            }
                                            else
                                            {
                                                outPutString += "<td class='td15' style='background-color: #D1D1D1;height:35px;width:30px;text-align:center;'>";
                                                //outPutString += "<td class='td15' style='background-color: #D1D1D1;'>";
                                                outPutString += "" + strTF + "";
                                                outPutString += "</td>";
                                            }
                                        }

                                    }
                                    outPutString += " </tr>";
                                    outPutString += " </Table>";
                                    outPutString += "</td>";
                                    outPutString += " </tr>";
                                }
                                outPutString += "</Table>";
                                //outPutString += "      </body>";
                                //outPutString += "      </html>";
                            }


                            #region Write this data to html file

                            if (!Directory.Exists(saveUrlPath))
                            {
                                Directory.CreateDirectory(saveUrlPath);
                            }

                            if (outPutString != "" && outPutString != string.Empty)
                            {
                                try
                                {
                                    var tmpFn = "DecisionTable_" + "_" + datetime + ".html";
                                    finalFileName = saveUrlPath + tmpFn;

                                    if (File.Exists(finalFileName))
                                    {
                                        File.Delete(finalFileName);
                                    }
                                    FileStream fileGraphMl = new FileStream(finalFileName, FileMode.Create, FileAccess.ReadWrite,
                                        FileShare.Write);
                                    fileGraphMl.Close();
                                    fileGraphMl.Dispose();
                                }
                                catch (Exception ee)
                                {
                                    Console.WriteLine(ee.Message);
                                }

                                using (StreamWriter stremWrite = new StreamWriter(finalFileName))
                                {
                                    stremWrite.WriteLine(outPutString);
                                    stremWrite.Flush();
                                    stremWrite.Close();
                                }
                            }

                            #endregion
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine(ex.Message);
                        }
                        #endregion

                        #region --Converting the Dataset in required XML format --
                        // Reason is: We have Code in HTML inside the jQWidgets DataTable control's Event.

                        //try
                        //{
                        //    if (dsTemp.Tables.Count > 0 && dsTemp.Tables[0].Rows.Count > 0)
                        //    {
                        //        outPutStringxml = "";
                        //        outPutStringxml += "<DATA>";
                        //        for (int iDS = 0; iDS < dsTemp.Tables[0].Rows.Count; iDS++)
                        //        {
                        //            outPutStringxml += "<ROW> ";

                        //            string strCurrentStatementID = "";
                        //            string strCurrentStatement = "";
                        //            string strTF = "";
                        //            string strSequence = "";

                        //            //strCurrentStatementID = dsTemp.Tables[0].Rows[iDS]["GraphId"].ToString().Trim();
                        //            strCurrentStatement = dsTemp.Tables[0].Rows[iDS]["GraphName"].ToString().Trim();
                        //            strSequence = dsTemp.Tables[0].Rows[iDS]["Sequence"].ToString().Trim();

                        //            if (strCurrentStatement.Trim().ToUpper().Contains(" < "))
                        //            {
                        //                strCurrentStatement = strCurrentStatement.Replace("<", "Less Than");
                        //            }
                        //            if (strCurrentStatement.Trim().ToUpper().Contains(" > "))
                        //            {
                        //                strCurrentStatement = strCurrentStatement.Replace(">", "Greater Than");
                        //            }

                        //            if (strCurrentStatement.Trim().Contains("<"))
                        //            {
                        //                strCurrentStatement = strCurrentStatement.Replace("<", "&lt;");
                        //            }
                        //            if (strCurrentStatement.Trim().Contains(">"))
                        //            {
                        //                strCurrentStatement = strCurrentStatement.Replace(">", "&gt;");
                        //            }

                        //            outPutStringxml += "<Sequence>" + strSequence + "</Sequence>";
                        //            //outPutStringxml += "<GraphId>" + strCurrentStatementID + "</GraphId>";
                        //            outPutStringxml += "<GraphName>" + strCurrentStatement + "</GraphName>";

                        //            for (int jColumn = 4; jColumn < dsTemp.Tables[0].Columns.Count; jColumn++)
                        //            {
                        //                strTF = "";
                        //                strTF = dsTemp.Tables[0].Rows[iDS][jColumn].ToString().Trim();
                        //                if (strTF.ToString().Trim() == "")
                        //                {
                        //                    strTF = " ";
                        //                }
                        //                outPutStringxml += "<TrueOrFalse>" + "" + strTF + "" + "</TrueOrFalse>";
                        //            }

                        //            outPutStringxml += " </ROW>";
                        //        }
                        //        outPutStringxml += "      </DATA>";
                        //    }
                        //}
                        //catch (Exception ex)
                        //{
                        //    ex.ToString();
                        //}
                        #endregion

                        // 4. Create a XML file to write brabove string - Why XML? No fancy. Use jSON/Array also.  
                        #region Write this data to File

                        //if (outPutStringxml != "" && outPutStringxml != string.Empty)
                        //{
                        //    try
                        //    {
                        //        var strGlobalPath = "C:\\inetpub\\wwwroot\\VERTO\\Reports\\XML\\";
                        //        var tmpFn = "DecisionTable_" + "_" + datetime + ".xml";
                        //        finalFileName = strGlobalPath + tmpFn;

                        //        if (File.Exists(finalFileName))
                        //        {
                        //            File.Delete(finalFileName);
                        //        }
                        //        FileStream fileGraphMl = new FileStream(finalFileName, FileMode.Create, FileAccess.ReadWrite,
                        //            FileShare.Write);
                        //        fileGraphMl.Close();
                        //        fileGraphMl.Dispose();
                        //    }
                        //    catch (Exception ee)
                        //    {
                        //        Console.WriteLine(ee.Message);
                        //    }

                        //    using (StreamWriter stremWrite = new StreamWriter(finalFileName))
                        //    {
                        //        stremWrite.WriteLine(outPutStringxml);
                        //        stremWrite.Flush();
                        //        stremWrite.Close();
                        //    }
                        //}

                        #endregion
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }

                #region Add String And FilePath in Dataset
                //DataTable dtTemp = new DataTable();
                //dtTemp.Columns.Add("StringName");
                //dtTemp.Columns.Add("FilePath");
                //DataRow drNew;
                //drNew = dtTemp.NewRow();
                //drNew["StringName"] = outPutString;
                //drNew["FilePath"] = finalFileName;

                //dtTemp.Rows.Add(drNew);
                //dsReturnStringAndFilePath.Merge(dtTemp);
                #endregion

                #region Insert the Data DecisionChart table
                try
                {
                    #region Commentted Code On Date 08082016
                    //string outPutString1 = string.Empty;
                    //string outPutString2 = string.Empty;
                    //string outPutString3 = string.Empty;
                    //string outPutString4 = string.Empty;
                    //string outPutString5 = string.Empty;
                    //string outPutString6 = string.Empty;
                    //string outPutString7 = string.Empty;
                    //string outPutString8 = string.Empty;
                    //string outPutString9 = string.Empty;
                    //string outPutString10 = string.Empty;

                    //    int chunkSize = Convert.ToInt32(outPutString.Length) / 10;
                    //    int stringLength = outPutString.Length;
                    //    int j = 0;
                    //    for (int i = 0; i < stringLength; i += chunkSize)
                    //    {
                    //        string result = string.Empty;
                    //        j = j + 1;

                    //        if (j == 1)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString1 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString1;
                    //        }
                    //        else if (j == 2)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString2 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString2;
                    //        }
                    //        else if (j == 3)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString3 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString3;
                    //        }
                    //        else if (j == 4)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString4 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString4;
                    //        }
                    //        else if (j == 5)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString5 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString5;
                    //        }
                    //        else if (j == 6)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString6 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString6;
                    //        }
                    //        else if (j == 7)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString7 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString7;
                    //        }
                    //        else if (j == 8)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString8 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString8;
                    //        }
                    //        else if (j == 9)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString9 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString9;
                    //        }
                    //        else if (j == 10)
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString10 = outPutString.Substring(i, chunkSize);
                    //            result = outPutString10;
                    //        }
                    //        else
                    //        {
                    //            if (i + chunkSize > stringLength) chunkSize = stringLength - i;
                    //            outPutString10 = outPutString10 + outPutString.Substring(i, chunkSize);
                    //            result = outPutString10;
                    //        }
                    #endregion

                    var DecisionChart = new DecisionChart
                    {
                        ProjectId = iProjectId,
                        StatementId = iStatementId,
                        DecisionHtml = finalFileName,
                        DecisionXml = "null"
                    };
                    await
                        _codeVortoService.DecisionChartRepository.AddNewItem(DecisionChart);
                    //}

                }
                catch (Exception ex)
                {
                    ex.ToString();
                }
                #endregion

                return Ok("Decision chart inserted successfully");
            }
        }

        public List<string> FunctionReturnWholeStatementWs(string str)
        {
            var formattedString = str;


            // if that line contains PERIOD / FULLSTOP
            if ((formattedString.Contains(".")) && (_flag == 0) && (formattedString.Contains("*") == false) && (formattedString.Trim().StartsWith("/") == false))
            {
                // if that is NOT the LAST FULLSTOP
                if ((!formattedString.Trim().EndsWith(".")))
                {

                    List<string> lstValuesToAdd = formattedString.Split('.').ToList();
                    for (int counter = 0; counter < lstValuesToAdd.Count - 1; counter++)
                    {
                        _finalStringWorkStor.Add(lstValuesToAdd[counter] + ".");
                    }
                    _tempStrNoPeriod += lstValuesToAdd[lstValuesToAdd.Count - 1];
                    _flag = 1;
                }
                //If that FULLSTOP occures at the END - Insert that line directly in DB
                else
                {
                    _finalStringWorkStor.Add(formattedString);
                    _flag = 0;
                }
            }

            else if (((formattedString.TrimStart().StartsWith("*") == false)) && ((formattedString.Trim() != "")))
            {
                // Try to find period in next lines. Traverse upto Period.
                // This will be controlled by flag [flag=1 means it will traverse again for next line]
                if (formattedString.Contains("."))
                {
                    _finalStringWorkStor.Add(_tempStrNoPeriod + " " + formattedString);
                    // Empty tempStrNoPeriod, else it will append again next time
                    _tempStrNoPeriod = string.Empty;
                    _flag = 0;
                }
                else
                {
                    _tempStrNoPeriod += " " + formattedString;
                    _flag = 1;
                }
            }
            return _finalStringWorkStor;
        }

        private List<FileMaster> GetFileCount(string fileName, int projectId, int fextension, int flag)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fName", MySqlDbType.VarChar) {Value = fileName},
                    new MySqlParameter("@fextension", MySqlDbType.VarChar) {Value = fextension},
                    new MySqlParameter("@flag", MySqlDbType.Int32) {Value = flag}
                    
                };
                var fileData = _codeVortoService.FileMasterRepository
                    .ExecuteStoreProcedure<FileMaster>("SpGetFileData", parameters)
                    .ContinueWith(t => t.Result).Result;
                return fileData;
            }
        }

        private List<FileMaster> GetFileCountJcl(string fileName, int projectId, int fextension, int flag, string folderpath)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fName", MySqlDbType.VarChar) {Value = fileName},
                    new MySqlParameter("@fextension", MySqlDbType.VarChar) {Value = fextension},
                    new MySqlParameter("@flag", MySqlDbType.Int32) {Value = flag},
                    new MySqlParameter("@folderpath", MySqlDbType.VarChar) {Value = folderpath}
                    
                };
                var fileData = _codeVortoService.FileMasterRepository
                    .ExecuteStoreProcedure<FileMaster>("SpGetFileDataJcl", parameters)
                    .ContinueWith(t => t.Result).Result;
                return fileData;
            }
        }

        public int GetLeadingWhitespaceLength(string s)
        {
            int whiteSpaceCount = 0;
            while (Char.IsWhiteSpace(s[whiteSpaceCount]))
                whiteSpaceCount++;

            return whiteSpaceCount;
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
                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();
                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
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
                if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
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
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
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

                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();

                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;


                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
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
                if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
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
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
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
                try
                {
                    foreach (var cItem in childItems)
                    {
                        treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
                    }
                }
                catch (Exception)
                {

                }
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
                string ifPart = Regex.Split(treeView.GraphName, "IF", RegexOptions.None).LastOrDefault();// treeView.GraphName.Split(new[] { "If" }, StringSplitOptions.None)[1];
                if (ifPart.Contains("THEN"))
                {
                    condition = ifPart.Substring(0,
                        ifPart.IndexOf("THEN",
                            StringComparison.InvariantCulture));
                }
                else
                    condition = ifPart;

                nodeId++;

                int charCountOfText = condition.Length;
                if (charCountOfText > 0)
                {
                    int widthCalculation = (charCountOfText * 2) + 20;
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
                if (treeView.PrimaryCommandId == 25 || treeView.PrimaryCommandId == 38)
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
                if (treeView.PrimaryCommandId == 24 || treeView.PrimaryCommandId == 37)
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
                if (treeView.PrimaryCommandId == 23 || treeView.PrimaryCommandId == 36)
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
                if (treeView.PrimaryCommandId == 23 || treeView.PrimaryCommandId == 36)
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

        [HttpGet]
        public async Task<IHttpActionResult> AppendTreeNodes(string statementId, int projectId, int treeNodeId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var sqlQuery = "Select * from StatementReferenceMaster where StatementId = " + Int32.Parse(statementId);
                var statementMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                if (statementMaster == null) return NotFound();

                var treeView = new TreeView
                {
                    ClassCalled = statementMaster[0].ClassCalled
                };
                int auto = 300;
                var listItems = GetCallExternalDetails(statementId, treeView, new List<TreeView>(), projectId, ref auto, ref treeNodeId);
                return Ok(listItems);
            }
        }

        private List<TreeView> GetCallExternalDetails(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, ref int auto, ref int treeNodeId)
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
                            ActualStatementId = "Missing_99999999"
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
                                    NodeId = ++treeNodeId
                                });
                                lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, ref auto, ref treeNodeId);
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
                                    NodeId = ++treeNodeId
                                });
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, block.FileId, ref auto, ref treeNodeId);
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
                                    NodeId = ++treeNodeId
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
                                        NodeId = ++treeNodeId
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
                                        NodeId = ++treeNodeId
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
            int projectId, int fileId, ref int autoInt, ref int treeNodeId)
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
                        #region For the only Universe Basic launguage
                        string graphName = string.Empty;
                        graphName = GetIncludeStatement(projectId, fileId, methodName);
                        #endregion

                        if (!string.IsNullOrEmpty(graphName))
                        {
                            lstTreeView.Add(new TreeView
                            {
                                ParentId = statememtId,
                                GraphId = "111" + autoInt + "_" + statememtId,
                                HasChild = true,
                                //GraphName = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>",
                                GraphName = graphName,
                                BaseCommandId = 25,
                                StatementReferenceMaster = treeView.StatementReferenceMaster,
                                NodeId = ++treeNodeId,
                                ActualStatementId = "Missing_99999999"
                            });
                        }

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
                                ActualStatementId = "Missing_99999999"
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
                                    NodeId = ++treeNodeId
                                });
                                lstTreeView = GetCallInternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, block.FileId, ref autoInt, ref treeNodeId);
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
                                    NodeId = ++treeNodeId
                                });
                                lstTreeView = GetCallExternalDetails(lstTreeView.Last().GraphId, lstTreeView.Last(),
                                    lstTreeView, projectId, ref autoInt, ref treeNodeId);
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
                                        NodeId = ++treeNodeId
                                    });
                                }
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

        public static List<string> GetLanguageKeyword()
        {
            DataSet dsLanguageKeyword = new DataSet();
            List<string> lstLanguageKeywordsNew = new List<string>();
            string strQuery = "Select KeywordName from languagekeywords";
            MySqlDbConnectionBaseClass _appBlock = new MySqlDbConnectionBaseClass();
            dsLanguageKeyword = _appBlock.ExecuteNonQuery(strQuery, "");

            if (dsLanguageKeyword != null && dsLanguageKeyword.Tables.Count > 0)
            {
                foreach (var row in dsLanguageKeyword.Tables[0].AsEnumerable())
                {
                    lstLanguageKeywordsNew.Add(Convert.ToString(row[0]));
                }
            }
            return lstLanguageKeywordsNew;
        }

        private List<StatementReferenceMaster> GetDictionaryData(int projectId, int fileid, string dictionary)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileid},
                    new MySqlParameter("@dictionary", MySqlDbType.VarChar) {Value = dictionary}
                };
                var dictionaryData = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetDictionaryData", parameters)
                    .ContinueWith(t => t.Result).Result;
                return dictionaryData;
            }
        }

        private List<StatementReferenceMaster> GetInsertStmtData(int projectId, int fileid, string insertStmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileid},
                    new MySqlParameter("@insertStmt", MySqlDbType.VarChar) {Value = insertStmt}
                };
                var dictionaryData = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetInsertStatementData", parameters)
                    .ContinueWith(t => t.Result).Result;
                return dictionaryData;
            }
        }

        public string GetIncludeStatement(int projectId, int fileId, string methodName)
        {
            string result = string.Empty;
            try
            {
                var languageType = _codeVortoService.ProjectMasterRepository
                    .GetDataFromSqlQuery<ProjectMaster>(
                    "select * from ProjectMaster where ProjectId=" + projectId + "");
                var languageId = languageType.Result[0].LanguageId;

                if (languageId == 5)
                {
                    var stmt = "$INSERT";
                    var insertStmtData = GetInsertStmtData(projectId, fileId, stmt);
                    if (insertStmtData.Count > 0)
                    {
                        for (int iInsertStmt = 0; iInsertStmt < insertStmtData.Count; iInsertStmt++)
                        {
                            var insertStmt = insertStmtData[iInsertStmt].OriginalStatement;
                            string[] splitInsert = insertStmt.Split('>');
                            if (splitInsert.Length > 1)
                            {
                                string fileName = splitInsert.Last();

                                #region Check the file is exit or not (12 is include file)
                                var checkFileExistOrNot = GetFileCount(fileName, projectId, 12, 0);
                                if (checkFileExistOrNot.Count > 0)
                                {
                                    for (int iCheckFileExistOrNot = 0; iCheckFileExistOrNot < checkFileExistOrNot.Count; iCheckFileExistOrNot++)
                                    {
                                        var checkMethodExitOrNot = GetMethodExistOrNot(projectId, checkFileExistOrNot[iCheckFileExistOrNot].FileId, methodName);
                                        if (checkMethodExitOrNot.Count > 0)
                                        {
                                            result = insertStmt + "&nbsp;<a href='#' style='color: blue; text-decoration: underline;' onclick='includeStateDialog(" + checkFileExistOrNot[iCheckFileExistOrNot].FileId + ");'>[ " + fileName + " ]</a>"; ;
                                            break;
                                        }
                                    }
                                }
                                #endregion
                            }

                            if (!string.IsNullOrEmpty(result))
                            {
                                break;
                            }
                        }

                        if (string.IsNullOrEmpty(result))
                        {
                            result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                        }
                    }
                }
                else
                {
                    result = "<span class='nodemissingelement'>&nbsp;Method Not Found&nbsp;</span>";
                }

            }
            catch (Exception ex)
            {
                ex.ToString();
            }

            return result;
        }

        private List<StatementReferenceMaster> GetMethodExistOrNot(int projectId, int fileId, string stmt)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId},
                    new MySqlParameter("@statement", MySqlDbType.VarChar) {Value = stmt}
                    
                };
                var fileData = _codeVortoService.FileMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetMethodExistOrNot", parameters)
                    .ContinueWith(t => t.Result).Result;
                return fileData;
            }
        }

        private List<StatementReferenceMaster> GetFileDataExitOrNot(int projectId, int fileId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@fileId", MySqlDbType.Int32) {Value = fileId}
                };
                var statementData = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpUBGetFileDataExistOrNot", parameters)
                    .ContinueWith(t => t.Result).Result;
                return statementData;
            }
        }

        [HttpGet]
        public void Test1()
        {
            try
            {
                //var currentSentance = "<span style='color: #1dc5d8;'>CASE UPDATE.BP45.BP46 # 1</span>";
                //Regex word = new Regex(@"\<span ([\w\(\)\\_\,\<\#\;\:\&\$\-\.\'\\+\=EQ\sEQUAL\sAND\sNOT\sOR\s\/]*)\>");
                //var aa = "";
                //foreach (Match match in word.Matches(currentSentance))
                //{
                //    aa = match.Value;
                //    string[] strSplit = aa.Split('(');
                //    if (strSplit.Length > 1)
                //    {
                //        string output = strSplit[0].Replace("(", "").Trim().ToString();
                //        var fileName = output.Substring(output.IndexOf(".") + 1).Trim();
                //        string idDictinary = strSplit[1].Replace(")", "").Trim().ToString();
                //    }
                //}

                string xyz = "IF NEW.89 NE This Field Will be A Fixed Amount Still Owed on this Payorder if NO Longer Charging field of PO.FLE THEN";
                string nm = xyz.Split(new[] { "THEN" }, StringSplitOptions.None).FirstOrDefault();
                if (!string.IsNullOrEmpty(nm))
                {
                    string condition;
                    string ifPart = Regex.Split(nm, "IF", RegexOptions.None).LastOrDefault();
                    if (ifPart.Contains("THEN"))
                    {
                        condition = ifPart.Substring(0,
                            ifPart.IndexOf("THEN",
                                StringComparison.InvariantCulture));
                    }
                    else
                        condition = ifPart;
                }

            }
            catch (Exception ex)
            {
                ex.ToString();
            }

        }
    }
}
