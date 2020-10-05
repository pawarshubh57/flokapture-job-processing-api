using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.ExtensionLibrary;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class UniverseTestController : ApiController
    {
        [HttpGet]
        public IHttpActionResult ProcessOneFile(string filePath)
        {
            var copyOfLines = new List<string>();
            var lockedRegEx = new Regex(@"(.*\sLOCKED\s)(.*)", RegexOptions.IgnoreCase);
            var endWithLockedRegEx = new Regex(@"\s+LOCKED$", RegexOptions.IgnoreCase);
            var thenRegEx = new Regex(@"(.*\sTHEN)(.*)", RegexOptions.IgnoreCase);
            var allFileLines = File.ReadAllLines(filePath).ToList();
            var lstConvertedLines = new List<string>();
            foreach (var line in allFileLines)
            {
                string thisLine = line.Trim();
                if (thisLine.StartsWith("*") || string.IsNullOrEmpty(thisLine)) continue;

                if (endWithLockedRegEx.IsMatch(thisLine))
                {
                    copyOfLines.Add(thisLine);
                    copyOfLines.Add("IF NOT SUCCESS THEN");
                    continue;
                }
                if (thisLine.StartsWith("IF ", true, CultureInfo.CurrentCulture) &&
                    !thisLine.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                {
                    var matches = thenRegEx.Match(thisLine);
                    int loopCnt = -1;
                    foreach (Group group in matches.Groups)
                    {
                        loopCnt++;
                        if (loopCnt <= 0) continue;
                        if (string.IsNullOrEmpty(group.Value)) continue;
                        string groupValue = group.Value.Trim();
                        copyOfLines.Add(groupValue);
                    }
                    copyOfLines.Add("END");
                    continue;
                }

                if (!lockedRegEx.IsMatch(thisLine))
                {
                    copyOfLines.Add(thisLine);
                    continue;
                }
                string newLine = thisLine.CheckCommentInStatement();
                var newLines = newLine.CheckForLocked();
                copyOfLines.AddRange(newLines);
            }
            foreach (var line in copyOfLines)
            {
                string thisLine = line.Trim();
                if (thisLine.StartsWith("*")) continue;
                var lstLines = thisLine.CheckForConversion();
                lstConvertedLines.AddRange(lstLines);
            }

            int ifCount = lstConvertedLines.Count(a => a.StartsWith("IF "));
            int endIfCount = lstConvertedLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");
            if (ifCount == endIfCount) return Ok(lstConvertedLines);

            Console.WriteLine("=============================================================================");
            Console.WriteLine("IF Count: " + ifCount);
            Console.WriteLine("END IF Count: " + endIfCount);
            Console.WriteLine("File Name: " + Path.GetFileName(filePath));
            Console.WriteLine("File Path: " + filePath);
            Console.WriteLine("=============================================================================");
            return Ok(lstConvertedLines);
        }

        public List<string> ProcessFile(string filePath)
        {
            var copyOfLines = new List<string>();
            var lockedRegEx = new Regex(@"(.*\sLOCKED\s)(.*)", RegexOptions.IgnoreCase);
            var endWithLockedRegEx = new Regex(@"\s+LOCKED$", RegexOptions.IgnoreCase);
            var thenRegEx = new Regex(@"(.*\sTHEN)(.*)", RegexOptions.IgnoreCase);
            var allFileLines = File.ReadAllLines(filePath).ToList();
            allFileLines.RemoveAll(s => s.Length <= 0);
            allFileLines = allFileLines.Select(s => s.Trim()).ToList();
            allFileLines = allFileLines.CombineAllBrokenLines('_');
            var lstConvertedLines = new List<string>();
            foreach (var line in allFileLines)
            {
                string thisLine = line.Trim();
                if (thisLine.StartsWith("*") || string.IsNullOrEmpty(thisLine)) continue;
                string newLine = thisLine.CheckCommentInStatement();
                if (endWithLockedRegEx.IsMatch(newLine))
                {
                    copyOfLines.Add(newLine);
                    copyOfLines.Add("IF NOT SUCCESS THEN");
                    continue;
                }

                if (newLine.StartsWith("IF ", true, CultureInfo.CurrentCulture) &&
                    !newLine.EndsWith(" THEN", true, CultureInfo.CurrentCulture))
                {
                    if (newLine.StartsWith("IF ", true, CultureInfo.CurrentCulture) 
                        && newLine.EndsWith(" ELSE", true, CultureInfo.CurrentCulture))
                    {
                        string ifPart = newLine.Replace(" ELSE", " THEN");
                        copyOfLines.Add(ifPart);
                        copyOfLines.Add("END ELSE");
                        continue;
                    }

                    var matches = thenRegEx.Match(newLine);
                    int loopCnt = -1;
                    foreach (Group group in matches.Groups)
                    {
                        loopCnt++;
                        if (loopCnt <= 0) continue;
                        if (string.IsNullOrEmpty(@group.Value)) continue;
                        string groupValue = @group.Value.Trim();
                        copyOfLines.Add(groupValue);
                    }
                    copyOfLines.Add("END");
                    continue;
                }

                if (!lockedRegEx.IsMatch(newLine))
                {
                    copyOfLines.Add(newLine);
                    continue;
                }
                var newLines = thisLine.CheckForLocked();
                copyOfLines.AddRange(newLines);
            }
            foreach (var line in copyOfLines)
            {
                string thisLine = line.Trim();
                if (thisLine.StartsWith("*")) continue;
                var lstLines = thisLine.CheckForConversion();
                lstConvertedLines.AddRange(lstLines);
            }
            return lstConvertedLines;
        }

        [HttpGet]
        public IHttpActionResult ProcessDirectory(string directoryPath)
        {
            var allFiles = Directory.GetFiles(directoryPath, "*.*", SearchOption.AllDirectories).ToList();
            var unMatchedCount = new List<string>();
            foreach (var filePath in allFiles)
            {
                var lstConvertedLines = ProcessFile(filePath);
                int ifCount = lstConvertedLines.Count(a => a.StartsWith("IF "));
                int endIfCount = lstConvertedLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");

                Console.WriteLine("=============================================================================");
                Console.WriteLine("IF Count: " + ifCount);
                Console.WriteLine("END IF Count: " + endIfCount);
                Console.WriteLine("File Name: " + Path.GetFileName(filePath));
                Console.WriteLine("File Path: " + filePath);
                Console.WriteLine("=============================================================================");
                unMatchedCount.Add("=============================================================================");
                unMatchedCount.Add("File Name: " + Path.GetFileName(filePath));
                unMatchedCount.Add("File Path: " + filePath);
                unMatchedCount.Add("=============================================================================");
            }
            return Ok(unMatchedCount);
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessUnprocessedFiles(int projectId)
        {
            using (var codeVortoService = new CodeVortoServices.CodeVortoService())
            {
                var allFiles = await codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.Processed == 4).ConfigureAwait(false);
                var unMatchedCount = new List<string>();
                foreach (var fileMaster in allFiles)
                {
                    var lstConvertedLines = ProcessFile(fileMaster.FilePath);
                    int ifCount = lstConvertedLines.Count(a => a.StartsWith("IF ", true, CultureInfo.InvariantCulture));
                    if (fileMaster.FileTypeExtensionId == 9 || fileMaster.FileTypeExtensionId == 17)
                        ifCount = ifCount + 1;
                    int endIfCount = lstConvertedLines.Count(a => a == "END" || a == "END-IF" || a == "END IF");

                    Console.WriteLine("=============================================================================");
                    Console.WriteLine("IF Count: " + ifCount);
                    Console.WriteLine("END IF Count: " + endIfCount);
                    Console.WriteLine("File Name: " + Path.GetFileName(fileMaster.FilePath));
                    Console.WriteLine("File Path: " + fileMaster.FilePath);
                    Console.WriteLine("=============================================================================");
                    bool matchResult = ifCount == endIfCount;
                    if (matchResult) continue;
                    unMatchedCount.Add("=============================================================================");
                    unMatchedCount.Add("File Id: " + fileMaster.FileId);
                    unMatchedCount.Add("File Name: " + Path.GetFileName(fileMaster.FilePath));
                    unMatchedCount.Add("IF Count: " + ifCount);
                    unMatchedCount.Add("END IF Count: " + endIfCount);
                    unMatchedCount.Add("File Path: " + fileMaster.FilePath);
                    unMatchedCount.Add("=============================================================================");
                }
                return Ok(unMatchedCount);
            }
        }

    }
}

