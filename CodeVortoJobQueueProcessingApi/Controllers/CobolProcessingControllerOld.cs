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

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class CobolProcessingControllerOld : ApiController
    {
        private ICodeVortoService _codeVortoService;
        private readonly List<string> _finalStringWorkStor = new List<string>();
        private int _flag;
        string _tempStrNoPeriod = string.Empty;
        public List<string> lstLanguageKeywords = new List<string>();


        public CobolProcessingControllerOld()
        {
        }

        public CobolProcessingControllerOld(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> StartProcessCobolProject(int projectId)
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

                    var enumerator = allFiles.GetEnumerator();
                    var lstFileMasters = new List<FileMaster>();
                    while (enumerator.MoveNext())
                    {
                        string currentFile = enumerator.Current;
                        if (ignoredFile.Any(f => f.FileName == Path.GetFileName(currentFile))) continue;
                        string fileName = Path.GetFileName(currentFile);
                        if (string.IsNullOrEmpty(fileName)) continue;
                        if (fileName.Contains(".dll.config")) continue;
                        string extension = Path.GetExtension(currentFile);
                        int extensionId = fileTypeExtensionReferences.First(e => e.FileExtension == extension).FileTypeExtensionId;

                        var fileMaster = new FileMaster
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
                    enumerator.Dispose();
                    // Bulk Insert
                    // EntityFramework.BulkInsert.ProviderFactory.Register<BulkInsertProvider>("MySql.Data.MySqlConnection");
                    await _codeVortoService.FileMasterRepository.BulkInsert(listOfEntities: lstFileMasters).ConfigureAwait(false);
                }
                //return Ok(projectId);
                var processResult = await ParseProjectFilesCobol(projectId).ConfigureAwait(false);
                var dataContent = await processResult.ExecuteAsync(CancellationToken.None);
                string data = await dataContent.Content.ReadAsStringAsync();
                return Ok(data);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> ParseProjectFilesCobol(int projectId)
        {
            const StringComparison noCase = StringComparison.OrdinalIgnoreCase;

            using (_codeVortoService = new CodeVortoService())
            {
                #region Cobol File Parsing (COBOL, PROCS, JCL)


                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                var fileMaster = await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId);

                IEnumerable<FileMaster> copyOfFileMaster = fileMaster as FileMaster[] ?? fileMaster.ToArray();

                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var baseCommandReference = await baseCommandReferenceRepository.GetAllItems()
                    .ContinueWith(t => { var result = t.Result; return result.ToList(); });
                var lineCommentedIndicators =
                    baseCommandReference.Find(s => s.BaseCommand == "Line Comment").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var ifConditionStart = baseCommandReference.Find(s => s.BaseCommand == "IF Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var ifConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "IF End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var callExternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call External").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var callExternalIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "End Call External").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var methodIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Method Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var methodIndicationEnd = baseCommandReference.Find(s => s.BaseCommand == "Method End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var callInternalIndicationStart = baseCommandReference.Find(s => s.BaseCommand == "Call Internal").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var loopIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Loop Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                // var loopIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Loop End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);

                var callClassIndicatorStart = baseCommandReference.Find(s => s.BaseCommand == "Class Start").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                var callClassIndicatorEnd = baseCommandReference.Find(s => s.BaseCommand == "Class End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);


                #region For JCL && PROCS
                //var execConditionStart = baseCommandReference.Find(s => s.BaseCommand == "Call External").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                //var execConditionEnd = baseCommandReference.Find(s => s.BaseCommand == "Exec End").PrimaryLanguageReference.ToList().FindAll(p => p.LanguageId == 4);
                #endregion

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
                    List<string> textLineWS = new List<string>();
                    List<string> textLineDD = new List<string>();
                    List<string> textLinePD = new List<string>();
                    List<string> textLineFC = new List<string>();
                    List<string> textLineLS = new List<string>();
                    List<string> textComment = new List<string>();

                    // Read the all data in the file.
                    lstAllLines = File.ReadAllLines(file.FilePath).ToList<string>();

                    textComment = lstAllLines.CollectAllComments();
                    textLineWS = lstAllLines.CollectWorkingStorageSection();
                    lstAllLines = lstAllLines.RemoveAllCommentedLines();

                    if (file.FileTypeExtensionId == 6)
                    {
                        #region COBOL File processing

                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                //--------------------------------FILE-CONTROL------------------------------------------
                                List<int> lstFirstIndexFC =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("FILE-CONTROL".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                List<int> lstlastIndexFC =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("DATA DIVISION".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                if (lstFirstIndexFC.Count != 0 && lstlastIndexFC.Count != 0)
                                {
                                    lstlastIndexFC[0] = lstlastIndexFC[0] - 1;
                                    lstFirstIndexFC[0] = lstFirstIndexFC[0] + 1;
                                    textLineFC =
                                        lstAllLines.Skip(lstFirstIndexFC[0] + 1)
                                            .Take(lstlastIndexFC[0] - lstFirstIndexFC[0])
                                            .ToList<string>();
                                }

                                //--------------------------------Data Definition------------------------------------------
                                List<int> lstFirstIndexDD =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("DATA DIVISION".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                List<int> lstlastIndexDD =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("WORKING-STORAGE SECTION.".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                if (lstFirstIndexDD.Count != 0 && lstlastIndexDD.Count != 0)
                                {
                                    lstlastIndexDD[0] = lstlastIndexDD[0] - 1;
                                    lstFirstIndexDD[0] = lstFirstIndexDD[0] + 1;
                                    textLineDD =
                                        lstAllLines.Skip(lstFirstIndexDD[0] + 1)
                                            .Take(lstlastIndexDD[0] - lstFirstIndexDD[0])
                                            .ToList<string>();
                                }

                                //--------------------------------Linkage Section[If present]--> SECTIONTYPE:4 ----------------------------------
                                List<int> lstFirstIndexLS =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where((x => x.Str.Contains("LINKAGE SECTION.".ToUpper())
                                               && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false))))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                List<int> lstlastIndexLS =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("PROCEDURE DIVISION".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                if (lstFirstIndexLS.Count != 0 && lstlastIndexLS.Count != 0)
                                {
                                    lstlastIndexLS[0] = lstlastIndexLS[0] - 1;
                                    lstFirstIndexLS[0] = lstFirstIndexLS[0] + 1;
                                    textLineLS =
                                        lstAllLines.Skip(lstFirstIndexLS[0])
                                            .Take(lstlastIndexLS[0] - lstFirstIndexLS[0])
                                            .ToList<string>();
                                }

                                //--------------------------------PROCEDURE DIVISION------------------------------------------
                                List<int> lstFirstIndexPD =
                                    lstAllLines.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains("PROCEDURE DIVISION".ToUpper())
                                            && ((x.Str.Substring(6, x.Str.Length - 6).StartsWith("*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();
                                // instead of above -> try to read the file till end.
                                int lstlastIndexPD = lstAllLines.Count;
                                if (lstFirstIndexPD.Count != 0 && lstlastIndexPD != 0)
                                {
                                    textLinePD =
                                        lstAllLines.Skip(lstFirstIndexPD[lstFirstIndexPD.Count - 1])
                                            .Take(lstlastIndexPD - lstFirstIndexPD[lstFirstIndexPD.Count - 1])
                                            .ToList<string>();
                                }


                                #region Working-Storage section

                                if (textLineWS.Count > 0)
                                {
                                    for (int iCnt = 0; iCnt < textLineWS.Count; iCnt++)
                                    {
                                        string strListStr = textLineWS[iCnt].ToString();
                                        if (strListStr.Length > 5)
                                        {
                                            strListStr = strListStr.Remove(0, 6);
                                            textLineWS[iCnt] = textLineWS[iCnt].Replace(textLineWS[iCnt].ToString(), strListStr);
                                            if (textLineWS[iCnt].Length >= 67)
                                            {
                                                textLineWS[iCnt] = textLineWS[iCnt].Remove(66);
                                            }
                                        }
                                    }
                                    List<string> lstringWorkingStorage = new List<string>();
                                    foreach (string str in textLineWS)
                                    {
                                        if (str.Contains("XFCL-RES-ADDR-STATE"))
                                        {

                                        }
                                        if (str != "" && (str.StartsWith("*") == false))
                                        {

                                            lstringWorkingStorage = FunctionReturnWholeStatementWs(str);
                                        }
                                    }
                                    if (lstringWorkingStorage != null && lstringWorkingStorage.Count > 0)
                                    {
                                        textLineWS = new List<string>();
                                        textLineWS = lstringWorkingStorage;
                                    }

                                    textLineWS.RemoveAll(stringToCheck => stringToCheck.Trim().StartsWith("*"));
                                    List<string> lstCopyWS = textLineWS.Where(stringToCheck => stringToCheck.Contains("COPY")).ToList();
                                    List<string> lstCopyExecInclude =
                                        textLineWS.Where(stringToCheck => stringToCheck.Contains("EXEC SQL INCLUDE ")).ToList();
                                    textLineWS.RemoveAll(stringToCheck => stringToCheck.Contains("EXEC SQL INCLUDE "));

                                    #region Variable Declaration Part

                                    string currentSentance;
                                    if ((lstCopyExecInclude.Count == 0))
                                    {
                                        lstCopyExecInclude = textLineWS.Where(stringToCheck => stringToCheck.Contains(" INCLUDE ")).ToList();
                                        textLineWS.RemoveAll(stringToCheck => stringToCheck.Contains(" INCLUDE "));

                                        for (int iWS = 0; iWS < textLineWS.Count; iWS++)
                                        {
                                            currentSentance = textLineWS[iWS].ToString().Trim();

                                            if (!currentSentance.StartsWith("*") || !currentSentance.StartsWith(""))
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
                                                    BaseCommandId = 7,
                                                    ClassNameDeclared = null,
                                                    PrimaryCommandId = 15,
                                                    ProjectId = projectId,
                                                    ParsedOrNot = "1"

                                                };
                                                await _codeVortoService.StatementReferenceMasterRepository
                                                    .AddNewItem(stmtReferenceMaster);
                                            }
                                        }
                                    }
                                    #endregion

                                    #region Parsing Linkage Storage Section -- Start
                                    if (textLineLS.Count > 0)
                                    {
                                        for (int iLS = 0; iLS < textLineLS.Count; iLS++)
                                        {
                                            currentSentance = textLineLS[iLS].ToString().Trim();
                                            if (!currentSentance.StartsWith("*") || !currentSentance.StartsWith(""))
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
                                                    BaseCommandId = 7,
                                                    ClassNameDeclared = null,
                                                    PrimaryCommandId = 15,
                                                    ProjectId = projectId,
                                                    ParsedOrNot = "1"
                                                };
                                                await _codeVortoService.StatementReferenceMasterRepository
                                                    .AddNewItem(stmtReferenceMaster);
                                            }
                                        }
                                    }
                                    #endregion Parsing Linkage Storage Section -- End

                                    #region Parsing Procedure Division Section -- Start

                                    if (textLinePD.Count > 0)
                                    {
                                        #region Insert Class logic
                                        var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                        var statementReferenceMasterClass = new StatementReferenceMaster
                                        {
                                            FileId = file.FileId,
                                            ResolvedStatement = file.FileName.Replace(".cbl", "").ToUpper(),
                                            OriginalStatement = file.FileName.Replace(".cbl", "").ToUpper(),
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
                                            ProjectId = projectId
                                        };
                                        await
                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClass);

                                        #endregion

                                        int iMethodEnd = 0;
                                        #region-remove unnecessary first 6 & last 8 digits

                                        //List<string> tempListPd = listPd;

                                        string strListStr = "";
                                        try
                                        {
                                            for (int iCnt = 0; iCnt < textLinePD.Count; iCnt++)
                                            {
                                                strListStr = textLinePD[iCnt].ToString();
                                                if (strListStr != "" && strListStr.Length > 5)
                                                {
                                                    strListStr = strListStr.Remove(0, 6);
                                                    textLinePD[iCnt] = textLinePD[iCnt].Replace(textLinePD[iCnt].ToString(), strListStr);
                                                    if (textLinePD[iCnt].Length >= 67)
                                                    {
                                                        textLinePD[iCnt] = textLinePD[iCnt].Remove(66);
                                                        string strTemppppppp = textLinePD[iCnt];
                                                    }
                                                }
                                                else if (strListStr != "")
                                                {
                                                    //determine position
                                                    //if string startswith ' '
                                                    if (strListStr.StartsWith(" "))
                                                    {

                                                    }
                                                    else if (textLinePD[iCnt].Length != 0)
                                                    {
                                                        strListStr = strListStr.Remove(0, ((textLinePD[iCnt].Length)));
                                                        textLinePD[iCnt] = textLinePD[iCnt].Remove(0, ((textLinePD[iCnt].Length)));
                                                    }
                                                }
                                            }
                                        }
                                        catch (Exception e)
                                        {
                                            Console.WriteLine(e.InnerException);
                                        }

                                        #endregion

                                        // Remove the list items which starts with * as they are comments.
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("EJECT "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP1 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP2 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP3 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP4 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP5 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP6 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP7 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP8 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP9 "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("SKIP"));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("COPY "));
                                        textLinePD.RemoveAll(x => x.TrimStart().StartsWith("/ "));
                                        textLinePD.RemoveAll(x => x.Trim() == "");



                                        #region --By split the statements to Using the language keywords--

                                        List<string> lstFinalBlock = new List<string>();
                                        string currentSentence1 = "";
                                        tempInsertToStr = "";
                                        string nextSentence = "";

                                        List<string> textLinePDTemp = new List<string>();

                                        lstLanguageKeywords.AddRange(getLanguageKeyword());

                                        #region  -- Setting language keywords logic --

                                        for (int ilstCNt = 0; ilstCNt < textLinePD.Count; ilstCNt++)
                                        {
                                            currentSentence1 = textLinePD[ilstCNt];

                                            #region ------------ KEYWORD LOGIC -------------------

                                            if (currentSentence1.Trim() != "" && currentSentence1.Trim() != "+" && currentSentence1.Trim() != "0" &&
                                                currentSentence1.Trim() != "0+" && !currentSentence1.StartsWith("*"))
                                            {
                                                if (currentSentence1.Trim().EndsWith(".") || tempInsertToStr.Trim().EndsWith(".") ||
                                                    currentSentence1.Contains(" := ") || (currentSentence1.Contains("END-SUBROUTINE")))
                                                {
                                                    if (currentSentence1.Trim().EndsWith(".") && tempInsertToStr.Trim().EndsWith("."))
                                                    {
                                                        textLinePDTemp.Add(currentSentence1);
                                                        textLinePDTemp.Add(tempInsertToStr);
                                                    }
                                                    else if (currentSentence1.Trim().EndsWith(".") && tempInsertToStr != "")
                                                    {
                                                        textLinePDTemp.Add(tempInsertToStr);
                                                        textLinePDTemp.Add(currentSentence1);
                                                    }
                                                    else
                                                    {
                                                        textLinePDTemp.Add(currentSentence1);
                                                    }
                                                    tempInsertToStr = string.Empty;
                                                }

                                                else
                                                {
                                                    for (int jlstCNt = ilstCNt; jlstCNt < textLinePD.Count; jlstCNt++)
                                                    {
                                                        if (jlstCNt + 1 < textLinePD.Count)
                                                        {
                                                            nextSentence = textLinePD[jlstCNt + 1];
                                                            if (nextSentence.Trim() != "")
                                                            {
                                                                string tmpStrMatchNS = string.Empty;
                                                                string tmpStrMatchNS1 = string.Empty;
                                                                string tmpStrMatchCS = string.Empty;
                                                                string tmpStrMatchCS1 = string.Empty;
                                                                string tmpStrMatchTemp = string.Empty;
                                                                List<string> matchingCS = lstLanguageKeywords.Intersect(currentSentence1);
                                                                List<string> matchingNS = lstLanguageKeywords.Intersect(nextSentence);

                                                                if (nextSentence.IndexOf("ON ERROR", noCase) != -1)
                                                                {
                                                                    matchingNS.Add("ON ERROR");
                                                                    tmpStrMatchNS = "ON ERROR";
                                                                }
                                                                else if (nextSentence.IndexOf("CONTINUE", noCase) != -1)
                                                                {
                                                                    matchingNS.Add("CONTINUE");
                                                                    tmpStrMatchNS = "CONTINUE";
                                                                }

                                                                List<string> matchingTmp = lstLanguageKeywords.Intersect(tempInsertToStr);

                                                                if ((matchingNS.Count > 0) || (matchingCS.Count > 0))
                                                                {
                                                                    if ((matchingNS.Count > 0))
                                                                    {
                                                                        tmpStrMatchNS = matchingNS[0];
                                                                        if (matchingNS.Count > 1)
                                                                        {
                                                                            tmpStrMatchNS1 = matchingNS[1];
                                                                        }
                                                                    }
                                                                    if ((matchingCS.Count > 0))
                                                                    {
                                                                        tmpStrMatchCS = matchingCS[0];
                                                                        if (matchingCS.Count > 1)
                                                                        {
                                                                            tmpStrMatchCS1 = matchingCS[1];
                                                                        }
                                                                    }
                                                                    if ((matchingTmp.Count > 0))
                                                                    {
                                                                        tmpStrMatchTemp = matchingTmp[0];
                                                                    }
                                                                }

                                                                if ((nextSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchNS) && matchingNS.Count > 0) ||
                                                                    (nextSentence.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchNS1) && matchingNS.Count > 0 && tmpStrMatchNS1 != ""))
                                                                {
                                                                    if (
                                                                        ((currentSentence1.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchCS)) && matchingCS.Count > 0) ||
                                                                        ((tmpStrMatchCS1 != "") && (((currentSentence1.TrimStart(TRIM_CHARACTER).StartsWith(tmpStrMatchCS1)) && matchingCS.Count > 0)) ||
                                                                         currentSentence1.StartsWith("REINPUT ")))
                                                                    {

                                                                        textLinePDTemp.Add(Convert.ToString(currentSentence1));
                                                                        currentSentence1 = string.Empty;
                                                                        currentSentence1 = nextSentence;

                                                                        break;
                                                                    }
                                                                    else
                                                                    {
                                                                        if ((nextSentence.Trim().EndsWith(".")) ||
                                                                            (nextSentence.Trim().ToUpper().Contains("ON ERROR")))
                                                                        {
                                                                            if (currentSentence1.Trim().StartsWith("+++++++++++++++++"))
                                                                            {
                                                                                textLinePDTemp.Add(currentSentence1);
                                                                                textLinePDTemp.Add(nextSentence);
                                                                                currentSentence1 = string.Empty;
                                                                                nextSentence = string.Empty;
                                                                                ilstCNt = jlstCNt + 1;
                                                                                break;
                                                                            }
                                                                            else
                                                                            {
                                                                                if (matchingCS.Count == 0)
                                                                                {
                                                                                    textLinePDTemp.Add(currentSentence1);
                                                                                    textLinePDTemp.Add(nextSentence);
                                                                                    currentSentence1 = string.Empty;
                                                                                    nextSentence = string.Empty;
                                                                                    ilstCNt = jlstCNt + 1;
                                                                                    break;
                                                                                }
                                                                                else
                                                                                {
                                                                                    if (nextSentence.Trim().StartsWith(Convert.ToString(tmpStrMatchNS)))
                                                                                    {
                                                                                        textLinePDTemp.Add(Convert.ToString(currentSentence1));
                                                                                        textLinePDTemp.Add(Convert.ToString(nextSentence));
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        textLinePDTemp.Add(
                                                                                        Convert.ToString(currentSentence1 + nextSentence));
                                                                                    }

                                                                                    currentSentence1 = string.Empty;
                                                                                    nextSentence = string.Empty;
                                                                                    ilstCNt = jlstCNt + 1;
                                                                                    break;
                                                                                }
                                                                            }
                                                                        }
                                                                        else
                                                                        {
                                                                            currentSentence1 = currentSentence1 + nextSentence;
                                                                            continue;
                                                                        }
                                                                    }

                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        ((currentSentence1.TrimStart(TRIM_CHARACTER)
                                                                            .StartsWith(tmpStrMatchCS)) && matchingCS.Count > 0))
                                                                    {
                                                                        if (nextSentence.Trim().EndsWith(".") ||
                                                                            (nextSentence.Trim().Contains(" := ") ||
                                                                            (nextSentence.Trim().StartsWith("REINPUT ")) ||
                                                                            (nextSentence.Trim().Contains("AT TOP OF PAGE")) ||
                                                                            (nextSentence.Trim().Contains("AT BREAK OF ARTIST")) ||
                                                                            (nextSentence.Trim().Contains("AT BREAK OF")) ||
                                                                            (nextSentence.Trim().Contains("NEWPAGE")) ||
                                                                            (nextSentence.Trim().StartsWith("IGNORE"))))
                                                                        {
                                                                            if (currentSentence1.Trim().EndsWith(".") ||
                                                                                (nextSentence.Trim().Contains(" := ")) ||
                                                                                (nextSentence.Trim().StartsWith("REINPUT ")) ||
                                                                                (nextSentence.Trim().Contains("AT TOP OF PAGE")) ||
                                                                                (nextSentence.Trim().Contains("AT BREAK OF ARTIST")) ||
                                                                                (nextSentence.Trim().Contains("AT BREAK OF")) ||
                                                                                (nextSentence.Trim().Contains("NEWPAGE")) ||
                                                                                (nextSentence.Trim().StartsWith("IGNORE")) ||
                                                                                (nextSentence.Trim().StartsWith("CONTINUE"))
                                                                                )
                                                                            {
                                                                                if (nextSentence.Trim().StartsWith("REINPUT "))
                                                                                {
                                                                                    textLinePDTemp.Add(Convert.ToString(currentSentence1));
                                                                                    currentSentence1 = string.Empty;
                                                                                    break;
                                                                                }
                                                                                else if (nextSentence.Trim().StartsWith("IGNORE"))
                                                                                {
                                                                                    textLinePDTemp.Add(Convert.ToString(currentSentence1));
                                                                                    textLinePDTemp.Add(Convert.ToString(nextSentence));
                                                                                    currentSentence1 = string.Empty;
                                                                                    nextSentence = string.Empty;
                                                                                    ilstCNt = jlstCNt + 1;
                                                                                    break;
                                                                                }
                                                                                else
                                                                                {
                                                                                    textLinePDTemp.Add(Convert.ToString(currentSentence1));
                                                                                    textLinePDTemp.Add(Convert.ToString(nextSentence));
                                                                                    currentSentence1 = string.Empty;
                                                                                    nextSentence = string.Empty;
                                                                                    ilstCNt = jlstCNt + 1;
                                                                                    break;
                                                                                }
                                                                            }
                                                                            else
                                                                            {
                                                                                textLinePDTemp.Add(
                                                                                    Convert.ToString(currentSentence1 + nextSentence));
                                                                                currentSentence1 = string.Empty;
                                                                                nextSentence = string.Empty;
                                                                                ilstCNt = jlstCNt + 1;
                                                                                break;
                                                                            }
                                                                        }
                                                                        else
                                                                        {
                                                                            currentSentence1 = currentSentence1 + " " + nextSentence;
                                                                            ilstCNt = jlstCNt + 1;
                                                                            continue;
                                                                        }
                                                                    }
                                                                    else
                                                                    {
                                                                        if (nextSentence.Trim().EndsWith(".") ||
                                                                            (nextSentence.Trim().Contains(" := ")))
                                                                        {
                                                                            if (nextSentence.Contains(" := ") ||
                                                                                nextSentence.TrimStart(TRIM_CHARACTER).ToUpper().StartsWith("INCLUDE") ||
                                                                                nextSentence.TrimStart(TRIM_CHARACTER).ToUpper().StartsWith("INCLUDE"))
                                                                            {
                                                                                textLinePDTemp.Add(currentSentence1 + tempInsertToStr);
                                                                                textLinePDTemp.Add(nextSentence);
                                                                                currentSentence1 = string.Empty;
                                                                                tempInsertToStr = string.Empty;
                                                                                nextSentence = string.Empty;
                                                                                ilstCNt = jlstCNt + 1;
                                                                                break;
                                                                            }
                                                                            else
                                                                            {
                                                                                textLinePDTemp.Add(currentSentence1 + tempInsertToStr + nextSentence);
                                                                                currentSentence1 = string.Empty;
                                                                                tempInsertToStr = string.Empty;
                                                                                nextSentence = string.Empty;
                                                                                ilstCNt = jlstCNt + 1;
                                                                                break;
                                                                            }
                                                                        }
                                                                        else
                                                                        {
                                                                            currentSentence1 += " " + nextSentence;
                                                                            ilstCNt = jlstCNt + 1;
                                                                            continue;
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            #endregion
                                        }

                                        #endregion

                                        tempInsertToStr = string.Empty;

                                        textLinePD = textLinePDTemp;

                                        #endregion

                                        var fileLines = textLinePD.ToArray().IfLoopsAdjustmentCobol();
                                        textLinePD = fileLines.ToList();

                                        for (int iPD = 0; iPD < textLinePD.Count; iPD++)
                                        {
                                            bool lineDone = false;
                                            int cntSpaces = 0;
                                            currentSentance = textLinePD[iPD].ToString().Trim();

                                            if (currentSentance.StartsWith("GO TO "))
                                            {
                                                currentSentance = currentSentance.Replace("GO TO", "PERFORM");
                                            }

                                            #region Insert Only Method logic
                                            var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                cntSpaces = GetLeadingWhitespaceLength(textLinePD[iPD].ToString());
                                            }

                                            if ((cntSpaces == 1) && ((textLinePD[iPD].ToString().Trim().StartsWith("-")) == false) && ((textLinePD[iPD].ToString().Trim().EndsWith("*'.")) == false))
                                            {
                                                if (currentSentance.Length > 0)
                                                {
                                                    if (cntSpaces + 6 == Convert.ToInt32(methodStart.StartIndicator))
                                                    {
                                                        if (iMethodEnd > 0)
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
                                                                PrimaryCommandId = 31,
                                                                ProjectId = projectId
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
                                                            ProjectId = projectId
                                                        };

                                                        await
                                                       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterNew);
                                                        iMethodEnd++;

                                                        continue;
                                                    }
                                                }
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
                                            if (currentSentance.Length > 0)
                                            {
                                                if (currentSentance.StartsWith(ifEnd.StartIndicator))
                                                {
                                                    if (!currentSentance.StartsWith("*"))
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
                                                            ProjectId = projectId
                                                        };
                                                        await
                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                        continue;

                                                    }
                                                }
                                            }
                                            #endregion

                                            #region Insert EXEC CICS statement logic
                                            var execCicsStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(execCicsStart.StartIndicator))
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
                                                            BaseCommandId = execCicsStart.BaseCommandId,
                                                            PrimaryCommandId = execCicsStart.PrimaryReferenceId,
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
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(execCicsEnd.StartIndicator))
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

                                            #region Insert Call Internal (PERFORM) statement logic
                                            var callInternalStart = callInternalIndicationStart.Find(x => x.StartIndicator != null);

                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.StartsWith(callInternalStart.StartIndicator))
                                                    {
                                                        #region Comment the code on date 07-22-2016
                                                        //  string[] stringArray = new string[] { "PERFORM" };

                                                        //  if (stringArray.Any(strTemp => currentSentance.Trim().ToUpper().StartsWith(strTemp)))
                                                        //  {
                                                        //      if (!currentSentance.EndsWith("."))
                                                        //      {
                                                        //          for (int iEndpoint = 0; iEndpoint < textLinePD.Count; iEndpoint++)
                                                        //          {
                                                        //              try
                                                        //              {
                                                        //                  if (textLinePD[iPD + 1].ToString().Contains("UNTIL"))
                                                        //                  {
                                                        //                      currentSentance = currentSentance + " " + textLinePD[iPD + 1].ToString().Trim();
                                                        //                      textLinePD.RemoveAt(iPD + 1);
                                                        //                      if (currentSentance.EndsWith("."))
                                                        //                      {
                                                        //                          break;
                                                        //                      }
                                                        //                  }
                                                        //                  else
                                                        //                  {
                                                        //                      currentSentance = currentSentance + " " + textLinePD[iPD + 1].ToString().Trim();
                                                        //                      textLinePD.RemoveAt(iPD + 1);
                                                        //                      if (currentSentance.EndsWith("."))
                                                        //                      {
                                                        //                          break;
                                                        //                      }
                                                        //                  }
                                                        //              }
                                                        //              catch (Exception ex)
                                                        //              {
                                                        //                  ex.ToString();
                                                        //              }
                                                        //          }
                                                        //      }
                                                        //  }
                                                        #endregion Comment Code

                                                        string[] stmtSplit = currentSentance.Split(' ');

                                                        if (stmtSplit.Length > 1)
                                                        {
                                                            var performMethodCalled = stmtSplit[1].ToString().Trim().Replace(".", "");
                                                            if (string.IsNullOrEmpty(performMethodCalled))
                                                            {
                                                                performMethodCalled = stmtSplit[2].ToString().Trim().Replace(".", "");
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
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                      _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                            continue;
                                                        }


                                                    }
                                                }


                                            }

                                            #endregion

                                            #region Insert Loop start [PERFORM VARYING] statement logic
                                            var loopStart = loopIndicatorStart.Find(x => x.StartIndicator != null);

                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
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
                                            if (lineDone) continue;
                                            if (currentSentance.Length > 0)
                                            {
                                                if (!currentSentance.StartsWith("*"))
                                                {
                                                    if (currentSentance.Trim().StartsWith("XCTL"))
                                                    {
                                                        //string[] stmtSplit = currentSentance.Split(' ');
                                                        if (currentSentance.Contains("('") && currentSentance.Contains("')"))
                                                        {
                                                            var statementReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = currentSentance,
                                                                OriginalStatement = currentSentance,
                                                                ClassCalled = currentSentance.Trim().Replace("XCTL", "").Replace("PROGRAM", "").Replace("('", "").Replace("')", "").ToString(),
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
                                                                ProjectId = projectId
                                                            };
                                                            await
                                                                _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                        }

                                                    }
                                                    else if (currentSentance.Trim().StartsWith("MAPSET"))
                                                    {
                                                        //string[] stmtSplit = currentSentance.Split(' ');

                                                        var statementReferenceMaster = new StatementReferenceMaster
                                                        {
                                                            FileId = file.FileId,
                                                            ResolvedStatement = currentSentance,
                                                            OriginalStatement = currentSentance,
                                                            ClassCalled = currentSentance.Trim().Replace("MAPSET", "").Replace("('", "").Replace("')", "").ToString(),
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
                                                    else
                                                    {
                                                        string[] stringArray = new string[] { "READ", "WRITE", "DELETE", "UPDATE", "RECEIVE", "SEND", "RETURN", "REWRITE", "READNEXT", "WRITEQ", "CLOSE", "OPEN" };

                                                        if (stringArray.Any(strTemp => currentSentance.Trim().ToUpper().StartsWith(strTemp)))
                                                        {
                                                            if (!currentSentance.EndsWith("."))
                                                            {
                                                                for (int iEndpoint = 0; iEndpoint < textLinePD.Count; iEndpoint++)
                                                                {
                                                                    try
                                                                    {
                                                                        currentSentance = currentSentance + " " + textLinePD[iPD + 1].ToString().Trim();
                                                                        textLinePD.RemoveAt(iPD + 1);
                                                                        if (currentSentance.EndsWith("."))
                                                                        {
                                                                            break;
                                                                        }
                                                                    }
                                                                    catch (Exception ex)
                                                                    {
                                                                        ex.ToString();
                                                                    }
                                                                }
                                                            }
                                                        }
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
                                                            ProjectId = projectId
                                                        };
                                                        await
                                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);
                                                    }

                                                    if (iPD == textLinePD.Count - 1)
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
                                                            PrimaryCommandId = 31,
                                                            ProjectId = projectId
                                                        };

                                                        await
                                                       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                    }
                                                }

                                                if (iPD == textLinePD.Count - 1)
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
                                                        PrimaryCommandId = 31,
                                                        ProjectId = projectId
                                                    };

                                                    await
                                                   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                }
                                            }
                                            #endregion

                                        }

                                        #region Insert Class End logic
                                        var classEnd = callClassIndicatorEnd.Find(x => x.StartIndicator == null);
                                        var statementReferenceMasterClassEnd = new StatementReferenceMaster
                                        {
                                            FileId = file.FileId,
                                            ResolvedStatement = "END " + file.FileName.Replace(".cbl", "").ToUpper(),
                                            OriginalStatement = "END " + file.FileName.Replace(".cbl", "").ToUpper(),
                                            ClassCalled = null,
                                            MethodName = null,
                                            DataOrObjectType = null,
                                            MethodCalled = null,
                                            VariableNameDeclared = null,
                                            PrimaryCommandId = classEnd.PrimaryReferenceId,
                                            BaseCommandId = classEnd.BaseCommandId,
                                            ProjectId = projectId
                                        };
                                        await
                                            _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClassEnd);
                                        #endregion
                                    }
                                    #endregion Parsing Procedure Division Section -- End
                                }

                                #endregion

                                #region -- Psuedo Code conversion code --
                                //TODO: check if setting flag is set to 1 before processing.
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
                    }

                    else if (file.FileTypeExtensionId == 7)
                    {
                        #region JCL files processing Logic
                        string parent = string.Empty, child = string.Empty;
                        string RemainStat = string.Empty;
                        string JclProgname = string.Empty;
                        int methodJCL = 0;
                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                List<string> lstata = new List<string>();

                                lstata = File.ReadAllLines(file.FilePath).ToList();

                                List<int> lstFirstIndexWS = new List<int>();
                                lstFirstIndexWS =
                                    lstata.Select((s, i) => new { Str = s, Index = i })
                                        .Where(x => x.Str.Contains(" exec ".ToUpper())
                                            && ((x.Str.StartsWith("//*") == false)))
                                        .Select(x => x.Index)
                                        .ToList<int>();


                                if (lstFirstIndexWS.Count > 0)
                                {
                                    #region Insert Class logic
                                    var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                    var statementReferenceMaster = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        OriginalStatement = file.FileName.Replace(".jcl", "").ToUpper(),
                                        ClassCalled = null,
                                        MethodName = null,
                                        DataOrObjectType = null,
                                        MethodCalled = null,
                                        VariableNameDeclared = null,
                                        ClassNameDeclared = projectName + file.FilePath.Replace(projectPath, "").Trim().Replace("\\", "."),
                                        PrimaryCommandId = classStart.PrimaryReferenceId,
                                        BaseCommandId = classStart.BaseCommandId,
                                        ProjectId = projectId
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMaster);

                                    #endregion


                                    for (int j = 0; j < lstFirstIndexWS[0] - 1; j++)
                                    {
                                        if (lstata[j].Trim() != "")
                                        {
                                            string stmt = "";
                                            try
                                            {
                                                if (lstata[j] != string.Empty && lstata[j].Trim().StartsWith("/"))
                                                {
                                                    stmt = lstata[j].Trim().Substring(2);
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
                                                                                FileId = file.FileId,
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

                                                                    #region Comment Region
                                                                    //#region Insert Only Method logic
                                                                    //var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                                    //if (child.Length > 0)
                                                                    //{
                                                                    //    if (child.Length > 0)
                                                                    //    {
                                                                    //        var stmtReferenceMaster = new StatementReferenceMaster
                                                                    //        {
                                                                    //            FileId = file.FileId,
                                                                    //            ResolvedStatement = child,
                                                                    //            OriginalStatement = child,
                                                                    //            ClassCalled = null,
                                                                    //            MethodName = null,
                                                                    //            DataOrObjectType = null,
                                                                    //            MethodCalled = null,
                                                                    //            VariableNameDeclared = null,
                                                                    //            BaseCommandId = methodStart.BaseCommandId,
                                                                    //            PrimaryCommandId = methodStart.PrimaryReferenceId,
                                                                    //            ProjectId = projectId
                                                                    //        };

                                                                    //        await
                                                                    //       _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                    //        continue;
                                                                    //    }
                                                                    //}
                                                                    //#endregion
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
                                    iIndex = file.FilePath.LastIndexOf("\\");
                                    if (iIndex > 0)
                                    {
                                        strOnlyFileName = file.FilePath.Substring(iIndex + 1);
                                        strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "").Replace(".JCL", "");
                                    }
                                    for (int iCnt = 0; iCnt < lstFirstIndexWS.Count; iCnt = iCnt + 1)
                                    {
                                        List<string> textLineWSNew = new List<string>();
                                        if (iCnt + 1 < lstFirstIndexWS.Count)
                                        {
                                            textLineWSNew =
                                                lstata.Skip(lstFirstIndexWS[iCnt])
                                                    .Take((lstFirstIndexWS[iCnt + 1]) - (lstFirstIndexWS[iCnt]))
                                                    .ToList<string>();
                                        }
                                        else if (iCnt + 1 == lstFirstIndexWS.Count)
                                        {
                                            textLineWSNew =
                                                lstata.Skip(lstFirstIndexWS[iCnt]).Take((lstFirstIndexWS[iCnt])).ToList<string>();
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
                                                                        FileId = file.FileId,
                                                                        ResolvedStatement = parent.Trim().Replace("//", ""),
                                                                        OriginalStatement = parent.Trim().Replace("//", ""),
                                                                        ClassCalled = null,
                                                                        MethodName = parent.Trim(),
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
                                                                        //FileId = file.FileId,
                                                                        //ResolvedStatement = parent,
                                                                        //OriginalStatement = parent,
                                                                        //ClassCalled = null,
                                                                        //MethodName = null,
                                                                        //DataOrObjectType = null,
                                                                        //MethodCalled = parent,
                                                                        //VariableNameDeclared = null,
                                                                        //BaseCommandId = 5,
                                                                        //PrimaryCommandId = 24,
                                                                        //ProjectId = projectId

                                                                        FileId = file.FileId,
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
                                                                            string[] stmtSplit = child.Trim().Split(' ');

                                                                            if (stmtSplit.Length > 0)
                                                                            {
                                                                                int fileExtension = 0;
                                                                                string ClassNamed = "";
                                                                                string checkSplitVal = "";

                                                                                if (stmtSplit.Length > 2)
                                                                                {
                                                                                    checkSplitVal = stmtSplit[2].Trim().ToString();
                                                                                }
                                                                                else
                                                                                {
                                                                                    checkSplitVal = stmtSplit[1].Trim().ToString();
                                                                                }

                                                                                if (checkSplitVal.Contains("PGM") || checkSplitVal.Contains("PROC"))
                                                                                {
                                                                                    string[] words = child.Split('=');
                                                                                    string program = words[0].Replace("EXEC", "");
                                                                                    string className = words[1].Split(',').First();

                                                                                    if (program.Trim() == "PGM")
                                                                                    {
                                                                                        fileExtension = 6;
                                                                                    }
                                                                                    else if (program.Trim() == "PROC")
                                                                                    {
                                                                                        fileExtension = 8;
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        fileExtension = 8;
                                                                                    }

                                                                                    var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                                    //if (callExtExpandedCode.Count == 0)
                                                                                    //{
                                                                                    //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                    //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                                    //    string strSplit = ClassNamed.Split('\\').Last();
                                                                                    //    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                    //}
                                                                                    //else
                                                                                    //{
                                                                                    //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                                    //}

                                                                                    if (callExtExpandedCode.Count == 0)
                                                                                    {
                                                                                        callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                        ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                        string strSplit = ClassNamed.Split('\\').Last();
                                                                                        ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                                    }

                                                                                    var stmtReferenceMaster = new StatementReferenceMaster
                                                                                    {
                                                                                        FileId = file.FileId,
                                                                                        ResolvedStatement = child,
                                                                                        OriginalStatement = child,
                                                                                        //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                        ClassCalled = ClassNamed,
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
                                                                                }
                                                                                else
                                                                                {
                                                                                    string[] words = child.Trim().Split(' ');
                                                                                    string program = words[0].Replace("EXEC", "");
                                                                                    string className = words[1].ToString();
                                                                                    fileExtension = 8;

                                                                                    var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                                    //if (callExtExpandedCode.Count == 0)
                                                                                    //{
                                                                                    //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                    //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                                    //    string strSplit = ClassNamed.Split('\\').Last();
                                                                                    //    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                    //}
                                                                                    //else
                                                                                    //{
                                                                                    //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                                    //}

                                                                                    if (callExtExpandedCode.Count == 0)
                                                                                    {
                                                                                        callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                        ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                        string strSplit = ClassNamed.Split('\\').Last();
                                                                                        ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                    }
                                                                                    else
                                                                                    {
                                                                                        ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                                    }


                                                                                    var stmtReferenceMaster = new StatementReferenceMaster
                                                                                    {
                                                                                        FileId = file.FileId,
                                                                                        ResolvedStatement = child,
                                                                                        OriginalStatement = child,
                                                                                        //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                        ClassCalled = ClassNamed,
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
                                                                                }

                                                                            }

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
                                                                                FileId = file.FileId,
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
                                                                                FileId = file.FileId,
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
                                                                                FileId = file.FileId,
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
                                                                            FileId = file.FileId,
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
                                                                            Statement = Statement.Replace("'", "''");
                                                                            if (Statement.EndsWith(","))
                                                                            {
                                                                                Statement = Statement.Substring(0, Statement.Length - 1);
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
                                                                                            string[] words = child.Split('=');
                                                                                            string program = words[0].Replace("EXEC", "");
                                                                                            string className = words[1].Split(',').First();
                                                                                            int fileExtension = 0;
                                                                                            string ClassNamed = "";

                                                                                            if (program.Trim() == "PGM")
                                                                                            {
                                                                                                fileExtension = 6;
                                                                                            }
                                                                                            else if (program.Trim() == "PROC")
                                                                                            {
                                                                                                fileExtension = 8;
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                                fileExtension = 8;
                                                                                            }


                                                                                            var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                                            //if (callExtExpandedCode.Count == 0)
                                                                                            //{
                                                                                            //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                            //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                                            //    string strSplit = ClassNamed.Split('\\').Last();
                                                                                            //    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                            //}
                                                                                            //else
                                                                                            //{
                                                                                            //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                                            //}

                                                                                            if (callExtExpandedCode.Count == 0)
                                                                                            {
                                                                                                callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                                ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                                string strSplit = ClassNamed.Split('\\').Last();
                                                                                                ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                                ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                                            }



                                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                                            {
                                                                                                FileId = file.FileId,
                                                                                                ResolvedStatement = child,
                                                                                                OriginalStatement = child,
                                                                                                //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                                ClassCalled = ClassNamed,
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
                                                                                                FileId = file.FileId,
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
                                                                                                FileId = file.FileId,
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
                                                                                                FileId = file.FileId,
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
                                                                                            FileId = file.FileId,
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
                                    FileId = file.FileId,
                                    ResolvedStatement = "END",
                                    OriginalStatement = "END",
                                    ClassCalled = null,
                                    MethodName = file.FileName,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = methodEnd.BaseCommandId,
                                    PrimaryCommandId = methodEnd.PrimaryReferenceId,
                                    ProjectId = projectId

                                    //FileId = file.FileId,
                                    //ResolvedStatement = "END",
                                    //OriginalStatement = "END",
                                    //ClassCalled = null,
                                    //MethodName = null,
                                    //DataOrObjectType = null,
                                    //MethodCalled = null,
                                    //VariableNameDeclared = null,
                                    //BaseCommandId = 9,
                                    //PrimaryCommandId = 31,
                                    //ProjectId = projectId
                                };

                                await
                          _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterEndMet);
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
                                    ProjectId = projectId
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
                    else if (file.FileTypeExtensionId == 8)
                    {
                        #region PROCS files processing Logic
                        string parent = string.Empty, child = string.Empty;
                        string RemainStat = string.Empty;
                        string JclProgname = string.Empty;
                        int methodProc = 0;
                        if (lstAllLines.Count > 0)
                        {
                            try
                            {
                                List<string> lstata = new List<string>();

                                lstata = File.ReadAllLines(file.FilePath).ToList();

                                List<int> lstFirstIndexWS =
                    lstata.Select((s, i) => new { Str = s, Index = i })
                        .Where(x => x.Str.Contains("exec".ToUpper())
                                && x.Str.StartsWith("//*") == false)
                        .Select(x => x.Index)
                        .ToList<int>();


                                if (lstFirstIndexWS.Count > 0)
                                {
                                    #region Insert Class logic
                                    var classStart = callClassIndicatorStart.Find(x => x.StartIndicator == null);
                                    var statementReferenceMasterClassStart = new StatementReferenceMaster
                                    {
                                        FileId = file.FileId,
                                        ResolvedStatement = file.FileName.Replace(".proc", "").ToUpper(),
                                        OriginalStatement = file.FileName.Replace(".proc", "").ToUpper(),
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
                                        ProjectId = projectId
                                    };
                                    await
                                        _codeVortoService.StatementReferenceMasterRepository.AddNewItem(statementReferenceMasterClassStart);

                                    #endregion

                                    for (int j = 0; j < lstFirstIndexWS[0] - 1; j++)
                                    {
                                        if (lstata[j] == "")
                                        {
                                            break;
                                        }

                                        string stmt = string.Empty;
                                        try
                                        {
                                            if (lstata[j].Length > 70)
                                            {
                                                stmt = lstata[j].Trim().Substring(2, 70);
                                            }
                                            else
                                            {
                                                stmt = lstata[j].Trim();
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
                                                                            FileId = file.FileId,
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
                                            catch (Exception ex)
                                            {
                                                Console.WriteLine(ex.InnerException);
                                            }
                                        }
                                    }

                                    #region INSERT THE STATEMENT IN STATEMENTREFERENCE TABLE

                                    string strOnlyFileName = "";
                                    int iIndex = 0;
                                    iIndex = file.FilePath.LastIndexOf("\\");
                                    if (iIndex > 0)
                                    {
                                        strOnlyFileName = file.FilePath.Substring(iIndex + 1);
                                        strOnlyFileName = strOnlyFileName.ToUpper().Replace(".TXT", "").Replace("PROC", "");
                                    }

                                    for (int iCnt = 0; iCnt < lstFirstIndexWS.Count; iCnt++)
                                    {
                                        //{
                                        List<string> textLineWSProc = new List<string>();
                                        if (iCnt < lstFirstIndexWS.Count - 1)
                                        {
                                            textLineWSProc =
                                                lstata.Skip(lstFirstIndexWS[iCnt])
                                                    .Take((lstFirstIndexWS[iCnt + 1]) - (lstFirstIndexWS[iCnt]))
                                                    .ToList<string>();
                                        }
                                        else
                                        {
                                            textLineWSProc =
                                                lstata.Skip(lstFirstIndexWS[iCnt])
                                                    .Take((lstata.Count) - (lstFirstIndexWS[iCnt]))
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
                                                    //if (parent.Trim().Length > 0)
                                                    //{
                                                    //    //parent = ;
                                                    //}

                                                    #region Insert Only Method logic
                                                    var methodStart = methodIndicationStart.Find(x => x.StartIndicator != null);
                                                    if (parent.Length > 0)
                                                    {
                                                        if (methodProc == 0)
                                                        {
                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                            {
                                                                FileId = file.FileId,
                                                                ResolvedStatement = parent.Replace("//", "").Trim(),
                                                                OriginalStatement = parent.Replace("//", "").Trim(),
                                                                ClassCalled = null,
                                                                MethodName = parent.Replace("//", "").Trim(),
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
                                                                //FileId = file.FileId,
                                                                //ResolvedStatement = parent,
                                                                //OriginalStatement = parent,
                                                                //ClassCalled = null,
                                                                //MethodName = null,
                                                                //DataOrObjectType = null,
                                                                //MethodCalled = parent,
                                                                //VariableNameDeclared = null,
                                                                //BaseCommandId = 5,
                                                                //PrimaryCommandId = 24,
                                                                //ProjectId = projectId

                                                                FileId = file.FileId,
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
                                                                    string[] stmtSplit = child.Trim().Split(' ');

                                                                    if (stmtSplit.Length > 0)
                                                                    {
                                                                        int fileExtension = 0;
                                                                        string ClassNamed = "";
                                                                        string checkSplitVal = "";
                                                                        if (stmtSplit.Length > 2)
                                                                        {
                                                                            checkSplitVal = stmtSplit[2].Trim().ToString();
                                                                        }
                                                                        else
                                                                        {
                                                                            checkSplitVal = stmtSplit[1].Trim().ToString();
                                                                        }

                                                                        if (checkSplitVal.Contains("PGM") || checkSplitVal.Contains("PROC"))
                                                                        {

                                                                            string[] words = child.Split('=');
                                                                            string program = words[0].Replace("EXEC", "");
                                                                            string className = words[1].Split(',').First();

                                                                            if (program.Trim() == "PGM")
                                                                            {
                                                                                fileExtension = 6;
                                                                            }
                                                                            else if (program.Trim() == "PROC")
                                                                            {
                                                                                fileExtension = 8;
                                                                            }
                                                                            else
                                                                            {
                                                                                fileExtension = 8;
                                                                            }

                                                                            var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                            //if (callExtExpandedCode.Count == 0)
                                                                            //{
                                                                            //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                            //    if (callExtExpandedCode.Count > 0)
                                                                            //    {
                                                                            //        ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                            //        string strSplit = ClassNamed.Split('\\').Last();
                                                                            //        ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                            //    }
                                                                            //    else
                                                                            //    {
                                                                            //        ClassNamed = null;
                                                                            //    }
                                                                            //}
                                                                            //else
                                                                            //{
                                                                            //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                            //}

                                                                            if (callExtExpandedCode.Count == 0)
                                                                            {
                                                                                callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                if (callExtExpandedCode.Count > 0)
                                                                                {
                                                                                    ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                    string strSplit = ClassNamed.Split('\\').Last();
                                                                                    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                }
                                                                                else
                                                                                {
                                                                                    ClassNamed = null;
                                                                                }
                                                                            }
                                                                            else
                                                                            {
                                                                                ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                            }


                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = file.FileId,
                                                                                ResolvedStatement = child,
                                                                                OriginalStatement = child,
                                                                                //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                ClassCalled = ClassNamed,
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

                                                                            #region Comment Region For PROCS 06-30-2016
                                                                            //    var stmtReferenceMaster = new StatementReferenceMaster
                                                                            //    {
                                                                            //        FileId = file.FileId,
                                                                            //        ResolvedStatement = child,
                                                                            //        OriginalStatement = child,
                                                                            //        ClassCalled = stmtSplit[1].ToString(),
                                                                            //        MethodName = null,
                                                                            //        DataOrObjectType = null,
                                                                            //        MethodCalled = null,
                                                                            //        VariableNameDeclared = null,
                                                                            //        BaseCommandId = execCicsJclStart.BaseCommandId,
                                                                            //        PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                            //        ProjectId = projectId
                                                                            //    };

                                                                            //    await
                                                                            //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                            //}
                                                                            //else
                                                                            //{
                                                                            //    var stmtReferenceMaster = new StatementReferenceMaster
                                                                            //    {
                                                                            //        FileId = file.FileId,
                                                                            //        ResolvedStatement = child,
                                                                            //        OriginalStatement = child,
                                                                            //        ClassCalled = null,
                                                                            //        MethodName = null,
                                                                            //        DataOrObjectType = null,
                                                                            //        MethodCalled = null,
                                                                            //        VariableNameDeclared = null,
                                                                            //        BaseCommandId = execCicsJclStart.BaseCommandId,
                                                                            //        PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                            //        ProjectId = projectId
                                                                            //    };

                                                                            //    await
                                                                            //   _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);
                                                                            //}
                                                                            #endregion
                                                                        }
                                                                        else
                                                                        {
                                                                            string[] words = child.Trim().Split(' ');
                                                                            string program = words[0].Replace("EXEC", "");
                                                                            string className = words[1].ToString();
                                                                            fileExtension = 8;

                                                                            var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                            //if (callExtExpandedCode.Count == 0)
                                                                            //{
                                                                            //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                            //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                            //    string strSplit = ClassNamed.Split('\\').Last();
                                                                            //    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                            //}
                                                                            //else
                                                                            //{
                                                                            //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                            //}

                                                                            if (callExtExpandedCode.Count == 0)
                                                                            {
                                                                                callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                string strSplit = ClassNamed.Split('\\').Last();
                                                                                ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();

                                                                            }
                                                                            else
                                                                            {
                                                                                ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                            }

                                                                            var stmtReferenceMaster = new StatementReferenceMaster
                                                                            {
                                                                                FileId = file.FileId,
                                                                                ResolvedStatement = child,
                                                                                OriginalStatement = child,
                                                                                //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                ClassCalled = ClassNamed,
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
                                                                        }
                                                                    }

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
                                                                        FileId = file.FileId,
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
                                                                        FileId = file.FileId,
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
                                                                        FileId = file.FileId,
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
                                                                    FileId = file.FileId,
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
                                                                    //if (stmtFlag > 0)
                                                                    //{
                                                                    //    stmtNew = stmtNew.ToString() + Statement;
                                                                    //}
                                                                    //else
                                                                    //{
                                                                    stmtNew = stmtNew.TrimStart().TrimEnd().ToString();
                                                                    //}
                                                                    stmtFlag = 1;
                                                                    //Statement = Statement.Substring(0, Statement.Length - 1);
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
                                                                //InsertIntoFD_Proc(ParentId, child, FileID, strProcSectionTypeId);

                                                                if (child.Length > 0)
                                                                {
                                                                    #region Comment region for PROCS On date 06-30-2016
                                                                    //#region Insert EXEC  statement logic
                                                                    //var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                                    //if (child.Length > 0)
                                                                    //{
                                                                    //    if (!child.StartsWith("*"))
                                                                    //    {

                                                                    //        if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                                    //        {
                                                                    //            var stmtReferenceMaster = new StatementReferenceMaster
                                                                    //            {
                                                                    //                FileId = file.FileId,
                                                                    //                ResolvedStatement = child,
                                                                    //                OriginalStatement = child,
                                                                    //                ClassCalled = null,
                                                                    //                MethodName = null,
                                                                    //                DataOrObjectType = null,
                                                                    //                MethodCalled = null,
                                                                    //                VariableNameDeclared = null,
                                                                    //                BaseCommandId = execCicsJclStart.BaseCommandId,
                                                                    //                PrimaryCommandId = execCicsJclStart.PrimaryReferenceId,
                                                                    //                ProjectId = projectId
                                                                    //            };

                                                                    //            await
                                                                    //           _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMaster);

                                                                    //            continue;
                                                                    //        }
                                                                    //    }
                                                                    //}

                                                                    //#endregion
                                                                    #endregion

                                                                    #region Insert EXEC  statement logic
                                                                    var execCicsJclStart = callExternalIndicationStart.Find(x => x.StartIndicator != null);
                                                                    if (child.Length > 0)
                                                                    {
                                                                        if (!child.StartsWith("*"))
                                                                        {

                                                                            if (child.StartsWith(execCicsJclStart.StartIndicator))
                                                                            {
                                                                                string[] words = child.Split('=');
                                                                                string program = words[0].Replace("EXEC", "");
                                                                                string className = words[1].Split(',').First();
                                                                                int fileExtension = 0;
                                                                                string ClassNamed = "";

                                                                                if (program.Trim() == "PGM")
                                                                                {
                                                                                    fileExtension = 6;
                                                                                }
                                                                                else if (program.Trim() == "PROC")
                                                                                {
                                                                                    fileExtension = 8;
                                                                                }
                                                                                else
                                                                                {
                                                                                    fileExtension = 8;
                                                                                }


                                                                                var callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 0);

                                                                                //if (callExtExpandedCode.Count == 0)
                                                                                //{
                                                                                //    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim();
                                                                                //    string strSplit = ClassNamed.Split('\\').Last();
                                                                                //    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();
                                                                                //}
                                                                                //else
                                                                                //{
                                                                                //    ClassNamed = callExtExpandedCode[0].FilePath.Substring(2, callExtExpandedCode[0].FilePath.Length - 2).Trim().Replace("\\", ".");
                                                                                //}

                                                                                if (callExtExpandedCode.Count == 0)
                                                                                {
                                                                                    callExtExpandedCode = GetFileCount(className, projectId, fileExtension, 1);
                                                                                    ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim();
                                                                                    string strSplit = ClassNamed.Split('\\').Last();
                                                                                    ClassNamed = ClassNamed.Replace(strSplit, "").Replace("\\", ".") + className + "." + program.Trim().Replace("PGM", "CBL").ToLower();

                                                                                }
                                                                                else
                                                                                {
                                                                                    ClassNamed = projectName + callExtExpandedCode[0].FilePath.Replace(projectPath, "").Trim().Replace("\\", ".");
                                                                                }

                                                                                var stmtReferenceMaster = new StatementReferenceMaster
                                                                                {
                                                                                    FileId = file.FileId,
                                                                                    ResolvedStatement = child,
                                                                                    OriginalStatement = child,
                                                                                    //ClassCalled = ClassNamed.Remove(ClassNamed.Remove(ClassNamed.Length - 1).LastIndexOf('.')),
                                                                                    ClassCalled = ClassNamed,
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
                                                                                    FileId = file.FileId,
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
                                                                                FileId = file.FileId,
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
                                        //}
                                    }
                                    #endregion

                                }

                                #region Insert Method End logic
                                var methodEnd = methodIndicationEnd.Find(x => x.StartIndicator == null);
                                var stmtReferenceMasterEndMet = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = "END",
                                    OriginalStatement = "END",
                                    ClassCalled = null,
                                    MethodName = file.FileName,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    BaseCommandId = methodEnd.BaseCommandId,
                                    PrimaryCommandId = methodEnd.PrimaryReferenceId,
                                    ProjectId = projectId

                                    //FileId = file.FileId,
                                    //ResolvedStatement = "END",
                                    //OriginalStatement = "END",
                                    //ClassCalled = null,
                                    //MethodName = null,
                                    //DataOrObjectType = null,
                                    //MethodCalled = null,
                                    //VariableNameDeclared = null,
                                    //BaseCommandId = 9,
                                    //PrimaryCommandId = 31,
                                    //ProjectId = projectId
                                };

                                await
                          _codeVortoService.StatementReferenceMasterRepository.AddNewItem(stmtReferenceMasterEndMet);
                                #endregion

                                #region Insert Class End logic
                                var classEnd = callClassIndicatorEnd.Find(x => x.StartIndicator == null);
                                var statementReferenceMasterEnd = new StatementReferenceMaster
                                {
                                    FileId = file.FileId,
                                    ResolvedStatement = "END " + file.FileName.Replace(".proc", "").ToUpper(),
                                    OriginalStatement = "END " + file.FileName.Replace(".proc", "").ToUpper(),
                                    ClassCalled = null,
                                    MethodName = null,
                                    DataOrObjectType = null,
                                    MethodCalled = null,
                                    VariableNameDeclared = null,
                                    PrimaryCommandId = classEnd.PrimaryReferenceId,
                                    BaseCommandId = classEnd.BaseCommandId,
                                    ProjectId = projectId
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
                            if (fileExtension == 7)
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
                                        ServiceContract = null
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
            
            return Ok("Done");
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
                                if (!s.ClassNameDeclared.Contains(".cbl"))
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
        public async Task<IHttpActionResult> GetWorkFlowWorkSpaceOLD(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                //var lstTreeViewData = new List<TreeViewData>();
                List<TreeView> lstTreeView = new List<TreeView>();
                List<TreeView> secondTab = new List<TreeView>();

                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                    .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                Expression<Func<StatementReferenceMaster, bool>> expression =
                    master => master.ProjectId == projectId && (master.ClassNameDeclared == clsName
                        || master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(expression, 1);
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
                    GraphName = "<span class='nodeToBold'>" + baseStatementMaster.OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster.BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = clsName,
                    ActualStatementId = "Actual_" + baseStatementMaster.StatementId,
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
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
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
                #region Extra blocks...
                var indexPosition = -1;
                lstTreeView = copyOfLstTreeView;
                int ifCounter = 0;
                string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                int tempId = 0;

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
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
                                        "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView,
                                colorArray[tempId]);
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
                        if (lstTreeView[i].BaseCommandId == 1)
                            ifCounter++;
                        if (lstTreeView[i].BaseCommandId == 2)
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
                    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 ||
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
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();
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

                int nodeId = 100;
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
                        Width = widthCnt.ToString()
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
                        Width = widthCnt.ToString()
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
                            ShapeId = "Decision",
                            Name = condition,
                            Color = "#ff6600"
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
                        var childItems = (from s in secondTab where s.ParentId == curItem.GraphId select s).ToList().Distinct();
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
                                Color = nodeColor
                            };
                        }
                        else
                        {
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = curItem.StatementReferenceMaster.ClassCalled,
                                Color = nodeColor
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
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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
                            nodeColor = "#f5bd6a";
                        nodeId++;
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = methodCalled[0].ClassNameDeclared,
                            Color = nodeColor
                        };
                        treeView.Nodes.Add(node);
                        string m = item.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First()); // RemoveNodes(treeView.Nodes, treeView.Links);
                #endregion

                //Expression<Func<ActionWorkflows, bool>> actionExpression = e => e.MethodStatementId == statementId &&
                //                                                          e.ProjectId == projectId;
                var actionWorkflow = await _codeVortoService.ActionWorkflowsRepository
                    .GetEntityData<ActionWorkflows>(s => s.MethodStatementId == statementId && s.ProjectId == projectId);
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
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetWorkFlowWorkSpace_07192016(int projectId, int stmtId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region Start processing

                //var lstTreeViewData = new List<TreeViewData>();
                List<TreeView> lstTreeView = new List<TreeView>();
                List<TreeView> secondTab = new List<TreeView>();

                var startClasses =
                    await _codeVortoService.ActionWorkflowsRepository
                        .GetAllItems(p => p.MethodStatementId != 0 && p.ProjectId == projectId);
                var clsName =
                    (from s in startClasses where s.MethodStatementId == stmtId select s.OriginObject).ToList().First();
                Expression<Func<StatementReferenceMaster, bool>> expression =
                    master =>
                        master.ProjectId == projectId &&
                        (master.ClassNameDeclared == clsName ||
                         master.ClassNameDeclared == clsName.Split('.').LastOrDefault());
                var baseStatementMaster = await
                    _codeVortoService.StatementReferenceMasterRepository.GetItem<StatementReferenceMaster>(expression, 1);
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
                    GraphName = "<span class='nodeToBold'>" + baseStatementMaster.OriginalStatement + "</span>",
                    HasChild = true,
                    ParentId = "-1",
                    BaseCommandId = baseStatementMaster.BaseCommandId,
                    StatementReferenceMaster = workflowRef[0],
                    SpriteCssClass = clsName,
                    ActualStatementId = "Actual_" + baseStatementMaster.StatementId,
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
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
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
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" +
                                                 treeItem.GraphName +
                                                 "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView,
                                colorArray[tempId]);
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
                        if (lstTreeView[i].BaseCommandId == 1)
                            ifCounter++;
                        if (lstTreeView[i].BaseCommandId == 2)
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
                    .Where(item => item.BaseCommandId == 6 || item.BaseCommandId == 2 ||
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
                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();
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

                int nodeId = 100;
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
                        Width = widthCnt.ToString()
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
                        Width = widthCnt.ToString()
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
                            ShapeId = "Decision",
                            Name = condition,
                            Color = "#ff6600"
                        };
                        treeView.Nodes.Add(node);
                        treeView.Links.Add(new Link
                        {
                            //Origin = nodeId - 1,
                            Origin = treeView.Nodes.First().Id,
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
                                strName = strSplit[strSplit.Length - 3] + "." + strSplit[strSplit.Length - 2];
                            }
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = strName.ToUpper(),
                                Color = nodeColor
                            };
                        }
                        else
                        {
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = curItem.StatementReferenceMaster.ClassCalled,
                                Color = nodeColor
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
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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
                            nodeColor = "#f5bd6a";
                        nodeId++;
                        var node = new Node
                        {
                            Id = nodeId,
                            ShapeId = "RoundRect",
                            Name = methodCalled[0].ClassNameDeclared,
                            Color = nodeColor
                        };
                        treeView.Nodes.Add(node);
                        string m = item.StatementReferenceMaster.MethodCalled;
                        if (m != null)
                        {
                            m = m + "()";
                            treeView.Links.Add(new Link
                            {
                                Origin = treeView.Nodes.First().Id,
                                Target = nodeId,
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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

                treeView.Links = RemoveMultipleLinks(treeView.Links, treeView.Nodes);
                treeView.Nodes = treeView.Nodes.RemoveHangingNodes(treeView.Links, treeView.Nodes.First());
                // RemoveNodes(treeView.Nodes, treeView.Links);

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
                    Color = node.Color
                }).ToList();
                var generalRepositoryNodeDetails =
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
                    //GraphName = sTab.GraphName,
                    GraphName = sTab.GraphName + "&nbsp;<img id='imgpseudo' src='../images/regex_icon.png' onclick='PseudoCodeDialog(" + sTab.ActualStatementId.Split('_')[1] + ")'/>",
                    HasChild = sTab.HasChild.ToString(),
                    MethodCalled = sTab.MethodCalled,
                    ParentId = sTab.ParentId,
                    PrimaryCommandId = sTab.PrimaryCommandId,
                    SpriteCssClass = sTab.SpriteCssClass,
                    WorkflowStartStatementId = actionWorkflow.First().MethodStatementId,
                    //StatementId = sTab.StatementReferenceMaster.StatementId.ToString()
                    StatementId = sTab.StatementReferenceMaster.StatementId
                }).ToList();
                var generalRepositoryWorkflowTreeviewSecondTabDetails =
                    new GeneralRepository<WorkflowTreeviewSecondTabDetails>(new AppDbContext());
                await generalRepositoryWorkflowTreeviewSecondTabDetails.BulkInsert(lstWorkflowTreeviewSecondTabDetails);

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
                lstData.Add(treeViewDataNew);
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */
                return Ok("Workflow data collected successfully");
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
                    copyOfLstTreeView = GetCallInternalDetails(treeItem.GraphId, treeItem, copyOfLstTreeView, projectId,
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
                //lstTreeView = copyOfLstTreeView.ToList();
                int ifCounter = 0;
                string[] colorArray = { "#2998fb", "#1dc5d8", "#860f0f", "#b80ee0" };
                int tempId = 0;

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
                            treeItem.GraphName = "<span class='nodeToBold' style='color: " + colorArray[tempId] + ";'>" + treeItem.GraphName +
                                        "</span>";
                            AssignColorsToChildNodes(child, ref copyOfLstTreeView,
                                colorArray[tempId]);
                            tempId++;
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
                        if (treeViewList[j].ParentId == prevParentId)
                            treeViewList[j].ParentId = graphId;
                    }
                }

                secondTab.Add(lstTreeView.ElementAt(0));
                secondTab.Add(lstTreeView.ElementAt(1));

                secondTab.AddRange(allSeqListItemsNew
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

                secondTab = secondTab.OrderBy(k => k.NodeId).ToList();
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

                int nodeId = 100;
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
                        Width = widthCnt.ToString()
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
                        Width = widthCnt.ToString()
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
                            ShapeId = "Decision",
                            Name = condition,
                            Color = "#ff6600"
                        };
                        treeView.Nodes.Add(node);
                        treeView.Links.Add(new Link
                        {
                            //Origin = nodeId - 1,
                            Origin = treeView.Nodes.First().Id,
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
                                Color = nodeColor
                            };
                        }
                        else
                        {
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = curItem.StatementReferenceMaster.ClassCalled,
                                Color = nodeColor
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
                                    Origin = treeView.Nodes.First().Id,
                                    Target = nodeId,
                                    LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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
                                Color = nodeColor
                            };
                        }
                        else
                        {
                            node = new Node
                            {
                                Id = nodeId,
                                ShapeId = "RoundRect",
                                Name = methodCalled[0].ClassNameDeclared,
                                Color = nodeColor
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
                                LinkText = "[" + linkSeqNumber + "] " + m.Substring(0, m.IndexOf('('))
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
                    Color = node.Color
                }).ToList();
                var generalRepositoryNodeDetails =
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
                lstData.Add(treeViewDataNew);
                lstData.Add(new TreeViewData(lstTreeView));
                return Ok(lstData);
                */
                return Ok("Workflow data collected successfully");
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
                    Color = "#ff6600"
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = treeView.StatementReferenceMaster.ClassCalled,
                        Color = nodeColor
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor
                    };

                }
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
                    Color = "#ff6600"
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = treeView.StatementReferenceMaster.ClassCalled,
                        Color = nodeColor
                    };

                }

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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor
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
                var childItems = (from s in secondTab where s.ParentId == treeView.GraphId select s).ToList().Distinct();
                foreach (var cItem in childItems)
                {
                    treeViewData = ProcessChildItemsCallInternal(projectId, secondTab, cItem, treeViewData, node, ref nodeId, ref linkSeqNumber);
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
                    ShapeId = "Decision",
                    Name = condition,
                    Color = "#ff6600",
                    Width = width,
                    Height = height
                };
                treeViewData.Nodes.Add(node);
                treeViewData.Links.Add(new Link
                {
                    Origin = lastNode.Id,
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = treeView.StatementReferenceMaster.ClassCalled,
                        Color = nodeColor
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor
                    };

                }
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
                        Color = nodeColor
                    };
                }
                else
                {
                    node = new Node
                    {
                        Id = nodeId,
                        ShapeId = "RoundRect",
                        Name = methodCalled[0].ClassNameDeclared,
                        Color = nodeColor
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

        protected void AssignColorsToChildNodes(TreeView treeView, ref List<TreeView> lstTreeView,
            string color)
        {
            treeView.GraphName = "<span class='nodeToBold' style='color: green;'>" + treeView.GraphName +
                                 "</span>";
            var childItems =
                (from s in lstTreeView where s.ParentId == treeView.GraphId select s).ToList();
            foreach (var child in childItems)
            {
                child.GraphName = "<span style='color: " + color + ";'>" + child.GraphName +
                                  "</span>";
            }
            //return lstTreeView;
        }

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
                            ActualStatementId = "Missing" + auto + "_99999999"
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
                                    lstTreeView, projectId, ref auto, ref treeNodeId);
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
            int projectId, ref int autoInt, ref int treeNodeId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                string methodName = treeView.MethodCalled;
                object[] parameters = {
                                          new MySqlParameter("@bCommandId", MySqlDbType.Int32){Value = 8},
                                          new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                                          new MySqlParameter("@methodNm", MySqlDbType.VarChar){Value = methodName},
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
                            ActualStatementId = "Missing" + autoInt + "_99999999"
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
                                ActualStatementId = "Missing" + autoInt + "_99999999"
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
                                    lstTreeView, projectId, ref autoInt, ref treeNodeId);
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


        private List<TreeView> GetCallExternalDetails_07192016(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
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
                                    lstTreeView, projectId, ref auto, ref treeNodeId);
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
                return lstTreeView;
            }
        }

        private List<TreeView> GetCallInternalDetails_07192016(string statememtId, TreeView treeView, List<TreeView> lstTreeView,
            int projectId, ref int autoInt, ref int treeNodeId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                if (string.IsNullOrEmpty(treeView.MethodCalled)) return lstTreeView;

                string methodName = treeView.MethodCalled;
                object[] parameters = {
                                          new MySqlParameter("@bCommandId", MySqlDbType.Int32){Value = 8},
                                          new MySqlParameter("@prjId", MySqlDbType.Int32){Value = projectId},
                                          new MySqlParameter("@methodNm", MySqlDbType.VarChar){Value = methodName},
                                      };
                var stmtMaster = _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpFindStatementForMethodName", parameters).Result;
                if (stmtMaster.Count == 0)
                {
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
                            ActualStatementId = "Missing_99999999"
                        });
                    }
                }
                else
                {
                    var callExtExpandedCode = GetGenericBlock(stmtMaster[0].StatementId, 8, 9);
                    if (callExtExpandedCode.Count == 0)
                    {
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
                                    lstTreeView, projectId, ref autoInt, ref treeNodeId);
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

        public static List<string> getLanguageKeyword()
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

    }
}
