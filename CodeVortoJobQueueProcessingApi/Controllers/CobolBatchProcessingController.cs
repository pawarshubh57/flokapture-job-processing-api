
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Formatting;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using Newtonsoft.Json;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class CobolBatchProcessingController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public CobolBatchProcessingController()
        {
            
        }

        public CobolBatchProcessingController(ICodeVortoService codeVortoService)
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
                    int solutionId = projectMaster.SolutionId ?? 0;
                    DateTime startDate = DateTime.Now;
                    string dashLine =
                        "=================================================================================\n";
                    string startStatus = "Started executing Method: StartProcessUbProject\nDate: " +
                                         DateTime.Now.ToString("g") + "\nTime: " +
                                         DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(dashLine);
                    Console.WriteLine(startStatus);
                    stringBuilder.AppendLine(dashLine).AppendLine(startStatus);

                    await StartProcessUbProject(projectMaster.ProjectId).ConfigureAwait(false); // Done

                    string endStatus = "Completed executing Method: StartProcessUbProject\nDate: " +
                                       DateTime.Now.ToString("g") + "\nTime: " +
                                       DateTime.Now.ToString("HH:m:s tt zzz");
                    Console.WriteLine(endStatus);
                    Console.WriteLine(dashLine);
                    stringBuilder.AppendLine(dashLine).AppendLine(endStatus);

                    // 


                    var projectJson = JsonConvert.SerializeObject(projectMaster, Formatting.Indented);
                    return Ok("Project processed successfully with all methods one by one. Project Details: " + projectJson);
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                    return InternalServerError(exception);
                }
            }

        }

        [HttpGet]
        public async Task<HttpResponseMessage> StartProcessUbProject(int projectId)
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
                var ignoredFile =
                    await projectMasterRepository.GetAllIgnoredFiles<IgnoreFiles>(projectDetails.LanguageId)
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
                            if (currentFile == null) continue;
                            var fileLines = File.ReadAllLines(currentFile).ToList();
                            var exist = fileLines.Any(f => regexPattern.IsMatch(f));
                            var fileType = exist ? "Online" : "Batch";
                            var lineCount = fileLines.Count(line => !string.IsNullOrWhiteSpace(line));
                            var fileName = Path.GetFileName(currentFile);
                            if (string.IsNullOrEmpty(fileName)) continue;
                            var extension = Path.GetExtension(currentFile);
                            var extensionId =
                                entensionList.First(e => e.FileExtension == extension).FileTypeExtensionId;
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
                        await
                            _codeVortoService.FileMasterRepository.BulkInsert(lstFileMasters).ConfigureAwait(false);
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
        public string RemoveSpacesBetweenWord()
        {
            string currentLine = "EXEC CICS RECEIVE          MAP('DTIN094') MAPSET('DTIN094') END-EXEC.";
            Regex regex = new Regex("[ ]{2,}", RegexOptions.None);
            currentLine = regex.Replace(currentLine, " ");
            return currentLine;
        }
    }
}
