using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.LogMessage;
using BusinessLayer.Models;
using BusinessLayer.UniverseBasic;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using DataAccessLayer;
using LumenWorks.Framework.IO.Csv;
using MySql.Data.MySqlClient;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Entity;
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

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class TestController : ApiController
    {
        private readonly UniverseBasicVersion1 _universeBasicVersion1 = new UniverseBasicVersion1();
        private AppDbContext _appDbContext;
        private ICodeVortoService _codeVortoService;
        // private static readonly ILog Log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        [HttpGet]
        public List<string> TestFunction(string input)
        {
            var input1 = _universeBasicVersion1.CheckIfSatetment(input);
            var searchResults = new List<string>(input1);
            return searchResults;
        }

        [HttpGet]
        public object ReadTextFile()
        {
            string[] contains = { ";*", "; *" };
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\Programs\Test1.txt").ToList();
            //methodBlockList.RemoveAll(l => l.Length <= 0);
            var methodBlockListMain = new List<string>();
            foreach (var str in methodBlockList)
                if (contains.Any(c => str.Contains(c)))
                {
                    var s = str.Substring(0, str.LastIndexOf(";", StringComparison.Ordinal));
                    methodBlockListMain.Add(s);
                }
                else
                    methodBlockListMain.Add(str);
            methodBlockListMain = methodBlockListMain.Select(x => x.Trim()).ToList();
            var ifCounter = 0;
            foreach (var statement in methodBlockListMain)
            {
                if (statement.StartsWith("IF "))
                    ifCounter++;

                if (statement.EndsWith("END") && statement.StartsWith("END"))
                    ifCounter--;
            }
            var msg = ifCounter != 0 ? "Something wrong" : "Ok";
            return msg;
        }

        [HttpGet]
        public object ReadTextFile(List<string> methodBlockList)
        {
            string[] contains = { ";*", "; *" };
            string msg;
            //List<string> methodBlockList = File.ReadAllLines(@"D:\Auctor\Programs\Test1.txt").ToList();
            //methodBlockList.RemoveAll(l => l.Length <= 0);
            var methodBlockListMain = new List<string>();
            foreach (var str in methodBlockList)
                if (contains.Any(c => str.Contains(c)))
                {
                    var s = str.Substring(0, str.LastIndexOf(";", StringComparison.Ordinal));
                    methodBlockListMain.Add(s);
                }
                else
                    methodBlockListMain.Add(str);
            methodBlockListMain = methodBlockListMain.Select(x => x.Trim()).ToList();
            var ifCounter = 0;
            foreach (var statement in methodBlockListMain)
            {
                if (statement.StartsWith("IF "))
                    ifCounter++;

                if (statement.EndsWith("END IF") && statement.EndsWith("END IF"))
                    ifCounter--;
            }
            msg = ifCounter != 0 ? "Something wrong" : "Ok";
            return msg;
        }

        [HttpGet]
        public async Task<IHttpActionResult> UploadDataDictionary(string directoryPath)
        {
            var allFiles = Directory.GetFiles(directoryPath, "*.txt");

            using (_appDbContext = new AppDbContext())
            {
                foreach (var file in allFiles)
                {
                    var methodBlockList = File.ReadAllLines(file).ToList();

                    methodBlockList = methodBlockList.Skip(1).ToList();
                    methodBlockList.RemoveAll(l => l.Length <= 0);
                    var indexPosition = 0;
                    var listItems = new List<DataDictionary>();
                    foreach (var str in methodBlockList)
                    {
                        var currentLine = str.Replace("\"", "").Split(',');
                        var isValidNumber = Regex.IsMatch(currentLine[1], @"^[0-9]+(\.[0-9]+)?$");
                        var replacementName = isValidNumber
                            ? "R." + currentLine[0] + "(" + currentLine[1] + ")"
                            : "K." + currentLine[0];
                        if (indexPosition == 0) replacementName = currentLine[0];
                        var cDate = currentLine[7];

                        listItems.Add(new DataDictionary
                        {
                            FileName = currentLine[0],
                            FieldNo = currentLine[1],
                            Description = currentLine[2],
                            FieldLabel = currentLine[3],
                            RptFieldLength = currentLine[4],
                            TypeOfData = currentLine[5],
                            SingleArray = currentLine[6],
                            DateOfCapture = cDate,
                            ReplacementName = replacementName
                        });
                        indexPosition++;
                    }
                    _appDbContext.DataDictionary.AddRange(listItems);
                    await _appDbContext.SaveChangesAsync();
                }
            }
            return Ok("Data dictionary data uploaded successfully");
        }

        [HttpGet]
        public IHttpActionResult ReadMenuCsv(string directoryPath)
        {
            var allFiles = Directory.GetFiles(directoryPath, "*.csv", SearchOption.TopDirectoryOnly);
            var stream = new StreamReader(allFiles[0]);
            var csvReader = new CsvReader(stream, true);
            int fieldCount = csvReader.FieldCount;
            while (csvReader.ReadNextRecord())
            {
                for (int i = 0; i < fieldCount; i++)
                    Console.Write("{0}", csvReader[i] == null ? "Missing" : csvReader[i]);
                Console.WriteLine("========================");
            }
            return Ok(csvReader.Columns);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllMenuItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var allItems = await _appDbContext.UniverseFileMenu.ToListAsync().ConfigureAwait(false);
                return Ok(allItems);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UploadFileMenuData(string directoryPath, int projectId)
        {
            var allFiles = Directory.GetFiles(directoryPath, "*.csv", SearchOption.TopDirectoryOnly);
            var listItems = new List<UniverseFileMenu>();
            using (_appDbContext = new AppDbContext())
            {
                foreach (var file in allFiles)
                {
                    var methodBlockList = File.ReadAllLines(file, Encoding.UTF7).ToList();

                    methodBlockList = methodBlockList.Skip(1).ToList();
                    methodBlockList.RemoveAll(l => l.Length <= 0);
                    var stream = new StreamReader(file);
                    var csvReader = new CsvReader(stream, true);
                    while (csvReader.ReadNextRecord())
                    {
                        listItems.Add(new UniverseFileMenu
                        {
                            ProjectId = projectId,
                            UserId = 1,
                            ActionExecuted = csvReader[3],
                            MenuId = csvReader[0],
                            MenuDescription = csvReader[2],
                            MenuTitle = csvReader[1],
                            UploadedOn = DateTime.Now
                        });
                    }
                    _appDbContext.UniverseFileMenu.AddRange(listItems);
                    await _appDbContext.SaveChangesAsync().ConfigureAwait(false);
                    return Ok("File Menu Data uploaded successfully");
                }
            }
            return Ok("File Menu Data uploaded successfully");
        }

        [HttpGet]
        public void TestRegEx(string input)
        {
            var match = input.IsValid();
            Console.Write(match);
        }

        [HttpGet]
        public object ReadSampleCaseCode()
        {
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\SampleCode\BP6000.txt").ToList();
            methodBlockList = methodBlockList.Select(x => x.Trim()).ToList();
            var methodBlockListMain = new List<string>();
            var methodBlockListFinal = new List<string>();
            foreach (var statement in methodBlockList)
            {
                var statement1 = statement.ToUpper();
                if (statement1.StartsWith("CASE") && !statement1.Contains("OTHERWISE"))
                {
                    statement1 = statement1.Replace("CASE", "IF");
                    methodBlockListMain.Add(statement1);
                }
                //else if (statement1.StartsWith("FUNCTION = 'AND'"))
                //{
                //    methodBlockListMain.Add(statement1);
                //    methodBlockListMain.Add("ELSE");
                //}
                else if (statement1.StartsWith("FUNCTION = 'OR'"))
                {
                    methodBlockListMain.Add(statement1);
                    methodBlockListMain.Add("END IF");
                }
                else if (statement1.Contains("OTHERWISE"))
                {
                    methodBlockListMain.Add("ELSE");
                }
                else if (statement1.StartsWith("END CASE"))
                {
                    methodBlockListMain.Add("END IF");
                }
                else if (statement1.StartsWith("BEGIN CASE"))
                {
                }
                else
                {
                    methodBlockListMain.Add(statement1);
                }
            }
            foreach (var statementMain in methodBlockListMain)
                if (statementMain.StartsWith("IF"))
                {
                    methodBlockListFinal.Add("ELSE");
                    methodBlockListFinal.Add(statementMain);
                }
                else
                {
                    methodBlockListFinal.Add(statementMain);
                }
            methodBlockListFinal = methodBlockListMain.Skip(1).ToList();
            return methodBlockListFinal;
        }

        [HttpGet]
        public object ReadMatreaduCode()
        {
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\SampleCode\BP6000.txt").ToList();
            methodBlockList = methodBlockList.Select(x => x.Trim()).ToList();
            var methodBlockListMain = new List<string>();
            foreach (var statement in methodBlockList)
                if (statement.EndsWith("LOCKED"))
                {
                    methodBlockListMain.Add(statement);
                    methodBlockListMain.Add("IF RECORD-ID-EXISTS");
                }
                else if (statement.EndsWith("ELSE"))
                {
                    var statement1 = statement.Replace("ELSE", "");
                    methodBlockListMain.Add(statement1);
                    methodBlockListMain.Add("IF NOT SUCCESS");
                }
                else if (statement.EndsWith("THEN"))
                {
                    var statement1 = statement.Replace("THEN", "");
                    methodBlockListMain.Add(statement1);
                    methodBlockListMain.Add("IF SUCCESS");
                }
                else if (statement.StartsWith("END ELSE"))
                {
                    methodBlockListMain.Add("ELSE");
                }
                else if (statement.StartsWith("END"))
                {
                    methodBlockListMain.Add("END-IF");
                }
                else
                {
                    methodBlockListMain.Add(statement);
                }

            return methodBlockListMain;
        }

        [HttpGet]
        public List<string> ReStateCaseBlock()
        {
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\SampleCode\BP6000.txt").ToList();
            methodBlockList = methodBlockList.Select(x => x.Trim()).ToList();
            methodBlockList =
                methodBlockList.Where(cLine => !cLine.StartsWith("BEGIN CASE"))
                    .Where(cLine => !cLine.StartsWith("END CASE"))
                    .ToList();
            methodBlockList = methodBlockList.Select(cLine => cLine.Replace("CASE", "IF")).ToList();
            var finalList = new List<string> { methodBlockList[0] };
            var ifCounter = 0;
            var flag = false;
            for (var i = 1; i < methodBlockList.Count; i++)
                if (methodBlockList[i].StartsWith("IF") && !methodBlockList[i].Contains("OTHERWISE"))
                {
                    if (ifCounter != 0)
                    {
                        finalList.Add("END IF");
                        finalList.Add("ELSE");
                        finalList.Add(methodBlockList[i]);
                    }
                    else
                    {
                        finalList.Add("ELSE");
                        finalList.Add(methodBlockList[i]);
                    }
                    ifCounter++;
                }
                else if (methodBlockList[i].Contains("OTHERWISE"))
                {
                    flag = true;
                    finalList.Add("END IF");
                    finalList.Add("ELSE");
                }
                else
                {
                    finalList.Add(methodBlockList[i]);
                }
            if (!flag) finalList.Add("END IF");

            finalList.Add("END IF");
            return finalList;
        }

        [HttpGet]
        public List<string> ReStateCaseBlock(List<string> caseBlockList)
        {
            var methodBlockList = caseBlockList.Select(cLine => cLine.Replace("CASE", "IF")).ToList();
            var finalList = new List<string> { methodBlockList[0] };
            var ifCounter = 0;
            var flag = false;
            for (var i = 1; i < methodBlockList.Count; i++)
                if (methodBlockList[i].StartsWith("IF") && !methodBlockList[i].Contains("OTHERWISE"))
                {
                    if (ifCounter != 0)
                    {
                        finalList.Add("END IF");
                        finalList.Add("ELSE");
                        finalList.Add(methodBlockList[i]);
                    }
                    else
                    {
                        finalList.Add("ELSE");
                        finalList.Add(methodBlockList[i]);
                    }
                    ifCounter++;
                }
                else if (methodBlockList[i].Contains("OTHERWISE"))
                {
                    flag = true;
                    finalList.Add("END IF");
                    finalList.Add("ELSE");
                }
                else
                {
                    finalList.Add(methodBlockList[i]);
                }
            if (!flag) finalList.Add("END IF");

            finalList.Add("END IF");


            var treeViewList = new List<string>();
            ifCounter = 0;
            foreach (var t in finalList)
            {
                treeViewList.Add(t);
                if (t.StartsWith("IF"))
                    ifCounter++;
                if (t.StartsWith("END IF"))
                    ifCounter--;
                if (ifCounter == 0)
                    break;
            }

            return treeViewList;
        }

        [HttpGet]
        public object ExtractCaseStatementBlock()
        {
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\SampleCode\BP6000.txt").ToList();
            var newDictionary = new Dictionary<int, string>();
            var blockList = methodBlockList.Select(x => x.Trim()).ToArray();
            var indexPosition = 0;
            foreach (var list in blockList)
            {
                indexPosition++;
                if (!list.StartsWith("BEGIN CASE")) continue;

                var caseBlockList = blockList.ToList().GetBlockOfLinesAsList(indexPosition, "END CASE");
                caseBlockList.RemoveAt(caseBlockList.Count - 1);
                //return caseBlockList;
                var modifiedCaseBlock = ReStateCaseBlockNew(caseBlockList.ToList());
                var check = modifiedCaseBlock.VerifyMethodBlockForIfWithMatchingEndIf();
                if (check)
                    return modifiedCaseBlock;
                return null;
            }
            return newDictionary;
        }

        [HttpGet]
        public List<string> ReStateCaseBlockNew(List<string> caseBlockList)
        {
            //var methodBlockList = caseBlockList.Select(cLine => cLine.Replace("CASE", "IF")).ToList();
            var finalList = new List<string>();
            var caseOtherwise = caseBlockList.Exists(s => s == "CASE OTHERWISE");
            var caseCounter = 0;
            foreach (var t in caseBlockList)
            {
                var sentense = t;
                if ((caseCounter == 0) && sentense.StartsWith("CASE "))
                {
                    sentense = t.Replace("CASE", "IF") + " THEN";
                    finalList.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE OTHERWISE"))
                {
                    finalList.Add("END IF");
                    finalList.Add("ELSE");
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE ") && (caseCounter >= 1))
                {
                    finalList.Add("END IF");
                    finalList.Add("ELSE");
                    sentense = t.Replace("CASE", "IF") + " THEN";
                    finalList.Add(sentense);
                    caseCounter++;
                    continue;
                }

                finalList.Add(sentense);
            }
            if (!caseOtherwise)
                finalList.Add("END IF");

            return finalList;
        }

        [HttpGet]
        public object ReadMatreaduCodeNew()
        {
            var methodBlockList = File.ReadAllLines(@"D:\Auctor\SampleCode\BP6000.txt").ToList();
            methodBlockList = methodBlockList.Select(x => x.Trim()).ToList();
            var methodBlockListMain = new List<string>();
            foreach (var statement in methodBlockList)
                if (statement.EndsWith("LOCKED"))
                {
                    methodBlockListMain.Add(statement);
                    methodBlockListMain.Add("IF RECORD-ID-EXISTS");
                }
                else if (statement.EndsWith("THEN"))
                {
                    var statement1 = statement.Replace("THEN", "");
                    methodBlockListMain.Add(statement1);
                    methodBlockListMain.Add("IF SUCCESS");
                }
                else if (statement.StartsWith("END ELSE"))
                {
                    methodBlockListMain.Add("ELSE");
                }
                else if (statement.StartsWith("END"))
                {
                    methodBlockListMain.Add("END-IF");
                }
                else
                {
                    methodBlockListMain.Add(statement);
                }

            return methodBlockListMain;
        }

        [HttpGet]
        public string CheckBeginEndCaseCount()
        {
            var allFiles = @"D:\Auctor\CodeVorto\UniverseBasic\RootDirectory\Programs\BP.BP\BP3001.pgm";
            //foreach (var file in allFiles)
            //{
            var allLines = File.ReadAllLines(allFiles).ToList();
            var check = allLines.CheckBeginAndEndCaseCounts();
            var match = check ? "Yes" : "No";
            Console.WriteLine("Matche? : " + match);
            Console.WriteLine(allFiles);
            Console.WriteLine("=====================================================");
            //}

            //foreach (var file in allFiles)
            //{
            var allLines123 = File.ReadAllLines(allFiles).ToList();
            var indexPosition = -1;
            allLines123.RemoveAll(s => s.Length <= 0);
            allLines123 = allLines123.Select(s => s.Trim()).ToList();
            allLines123 = allLines123.Where(s => !s.StartsWith("*")
                                                 && !s.StartsWith("!!") && !s.StartsWith(";*") && !s.StartsWith("; *"))
                .ToList();
            foreach (var line in allLines123)
            {
                indexPosition++;
                if (line != "BEGIN CASE") continue;

                var lstBlock = allLines123.GetBlockOfLinesAsList(indexPosition, "BEGIN CASE", "END CASE");

                Console.WriteLine("============================================================================");
                foreach (var block in lstBlock)
                    Console.WriteLine(block);
                Console.WriteLine("============================================================================");
            }
            //}

            return "Done";
        }

        [HttpGet]
        public IHttpActionResult ModifyCaseBlock()
        {
            var allFiles = @"D:\Auctor\CodeVorto\UniverseBasic\Test.txt";
            var allLines = File.ReadAllLines(allFiles).ToList();
            allLines = allLines.Select(s => s.Trim()).ToList();
            var list = _universeBasicVersion1.SimplifyCaseAndNestedCaseStatementsBlock(allLines);
            return Ok(list);
        }

        [HttpGet]
        public IHttpActionResult GetListFromCertainPoint()
        {
            var allFiles = @"D:\Auctor\CodeVorto\UniverseBasic\Test.txt";
            var allLines = File.ReadAllLines(allFiles).ToList();
            var extractedLines = new List<string>();
            allLines = allLines.Select(s => s.Trim()).ToList();
            allLines = allLines.Select(s => s.CheckCommentInStatement()).ToList();
            allLines = allLines.AdjustMatReadLockedLikeStatements();
            //string regEx = @"^[\%\/\\\&\?\,\'\;\:\!\-\*\#\$]+$";
            //allLines = allLines.Where(line => !Regex.IsMatch(line, regEx)).ToList();
            var lstAllGoSubs = allLines.GetAllGoSub("GOSUB");
            var indexPosition = -1;
            foreach (var line in allLines)
            {
                indexPosition++;

                if (line.StartsWith("STOP"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }
                if (line.StartsWith("* BEGIN MAINLINE"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }

                if (line.StartsWith("* MAINLINE"))
                {
                    var getRange = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                    extractedLines.AddRange(getRange);
                    //return Ok(extractedLines);
                    break;
                }
                if (!lstAllGoSubs.Any(l => line.StartsWith(l))) continue;

                var getRangeNew = allLines.GetRange(indexPosition, allLines.Count - indexPosition);
                extractedLines.AddRange(getRangeNew);
                //return Ok(extractedLines);
                break;
            }
            var dictionary = new Dictionary<string, List<string>>();

            foreach (var goSub in lstAllGoSubs.Distinct().ToList())
            {
                var methodBlock = PickUpAllMethodBlocks(extractedLines, goSub, lstAllGoSubs, "RETURN", "STOP");
                methodBlock.Insert(1, goSub);
                methodBlock.RemoveAll(s => s.Length <= 0);
                var caseStmtCount = methodBlock.Count(s => s == "BEGIN CASE");
                if (caseStmtCount >= 1)
                {
                    var simplifiedWithCaseBlocks = methodBlock.SimplifyCaseAndNestedCaseStatementsBlock();
                    dictionary.Add(goSub, simplifiedWithCaseBlocks);
                    continue;
                }

                dictionary.Add(goSub, methodBlock);
            }

            var ifCount = dictionary.Values.Sum(val => val.Count(v => v.StartsWith("IF ")));
            var endIfCount = dictionary.Values.Sum(val => val.Count(v => (v == "END") || (v == "END IF")));

            return Ok("If Cnt = " + ifCount + Environment.NewLine + " End If Cnt = " + endIfCount);
        }

        [HttpGet]
        public List<string> PickUpAllMethodBlocks(List<string> inputList, string startingPoint,
            List<string> allGoSub, params string[] otherEndPoints)
        {
            var outputList = new List<string>();
            var index = inputList.FindIndex(a => a.StartsWith(startingPoint));
            if (index == -1)
            {
                outputList.Add(null);
                return outputList;
            }

            index = index + 1;

            var goupTo4Lines = index - 4;
            var methodComments = string.Empty;
            for (var length = index; length >= 0; length--)
            {
                var line = inputList[length];
                if (length == goupTo4Lines)
                {
                    if (methodComments == string.Empty) methodComments = null;
                    break;
                }

                if (!line.StartsWith("* ")) continue;
                methodComments = line.Split('*').LastOrDefault();
                break;
            }
            outputList.Add(methodComments);
            /*
            string regExString = String.Empty;
            int start = 0;
            foreach (var endPoint in otherEndPoints)
            {
                if (start == 0)
                    regExString += "(" + endPoint + "\\s\\S+?[\\s])";
                else
                    regExString += "|(" + endPoint + "\\s\\S+?[\\s])";

                start++;
            }
            Regex regExPattern = new Regex(regExString, RegexOptions.IgnoreCase);
            */

            for (var length = index; length < inputList.Count; length++)
            {
                var line = inputList[length];
                if (line.StartsWith("*")) continue;

                outputList.Add(line);
                if (line.StartsWith("*")) break;
                if (otherEndPoints.Any(l => l == line)) break;
                if (!allGoSub.Any(l => line.StartsWith(l))) continue;

                outputList.Remove(line);
                break;
            }
            /*
            foreach (string line in inputList)
            {
                outputList.Add(line);
                if (otherEndPoints.Any(l => line.StartsWith(l))) break;
                if (allGoSub.Any(l => line.StartsWith(l))) break;
            }
            */
            return outputList;
        }

        [HttpPost]
        public IHttpActionResult TestIndexInList([FromBody] List<string> programLineList)
        {
            var stopLinesCount = programLineList.Count(l => l == "STOP");
            var lastIndexOfStop = 0;
            if (stopLinesCount > 0)
            {
                var indPos = -1;
                foreach (var line in programLineList)
                {
                    indPos++;
                    if (line == "STOP") stopLinesCount--;
                    if (stopLinesCount != 0) continue;

                    lastIndexOfStop = indPos;
                    break;
                }
            }
            return Ok(lastIndexOfStop);
        }

        [HttpGet]
        public IHttpActionResult ConvertCaseToIfElse()
        {
            var caseBlockList = File.ReadAllLines(@"D:\TestCase.txt").ToList();
            caseBlockList = caseBlockList.Select(s => s.Trim()).ToList();
            //var cBlock = caseBlockList.SimplifyCaseAndNestedCaseStatementsBlock();
            //if (cBlock != null) return Ok(cBlock);

            var copyOfCaseBlock = caseBlockList.ToList();
            int indexPos = -1;
            bool hasCaseOtherwise = false;
            int totalCaseBlocks = copyOfCaseBlock.Count(s => s.StartsWith("CASE "));
            foreach (var line in caseBlockList)
            {
                indexPos++;
                if (line == "BEGIN CASE")
                    copyOfCaseBlock[indexPos] = "";
                if (line == "END CASE")
                    copyOfCaseBlock[indexPos] = "END IF";

                if (line != "CASE OTHERWISE") continue;

                copyOfCaseBlock[indexPos] = "ELSE";
                hasCaseOtherwise = true;
            }
            copyOfCaseBlock = copyOfCaseBlock.FindAll(s => !string.IsNullOrEmpty(s));
            caseBlockList = copyOfCaseBlock.ToList();
            copyOfCaseBlock = new List<string>();
            int caseCounter = 0;
            indexPos = -1;
            foreach (var line in caseBlockList)
            {
                indexPos++;
                var sentense = line;
                if (sentense.StartsWith("CASE ") && caseCounter == 0)
                {
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter >= 2)
                {
                    if (copyOfCaseBlock[indexPos - 1] != "END" || copyOfCaseBlock[indexPos - 1] != "END IF")
                        copyOfCaseBlock.Add("END IF");
                    copyOfCaseBlock.Add("ELSE");
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter <= 1)
                {
                    //copyOfCaseBlock.Add("END IF");
                    copyOfCaseBlock.Add("ELSE");
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense == "ELSE" && caseCounter > 0)
                {
                    if (copyOfCaseBlock[indexPos - 1] != "END" && copyOfCaseBlock[indexPos - 1] != "END IF")
                        copyOfCaseBlock.Add("END IF");

                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                copyOfCaseBlock.Add(sentense);
            }
            if (!hasCaseOtherwise && totalCaseBlocks > 1)
                copyOfCaseBlock.Add("END IF");

            return Ok(copyOfCaseBlock);
        }

        [HttpGet]
        public IHttpActionResult ConvertCaseToIfElse(string filePath)
        {
            var caseBlockList = File.ReadAllLines(filePath).ToList();
            caseBlockList = caseBlockList.Select(s => s.Trim()).ToList();
            var copyOfCaseBlock = caseBlockList.ToList();
            int indexPos = -1;
            foreach (var line in caseBlockList)
            {
                indexPos++;
                if (line == "BEGIN CASE")
                    copyOfCaseBlock[indexPos] = "";
                if (line == "END CASE")
                    copyOfCaseBlock[indexPos] = "END IF";
            }
            copyOfCaseBlock = copyOfCaseBlock.FindAll(s => !string.IsNullOrEmpty(s));
            caseBlockList = copyOfCaseBlock.ToList();
            copyOfCaseBlock = new List<string>();
            indexPos = -1;
            List<string> caseConditions = new List<string>();
            int caseCounter = 0;
            foreach (var line in caseBlockList)
            {
                indexPos++;
                var sentense = line;
                if (sentense == "CASE OTHERWISE")
                {
                    string caseOtherwise = caseConditions
                        .Aggregate(string.Empty, (current, condition) => current + condition + " OR ");
                    caseOtherwise = caseOtherwise.Substring(0,
                        caseOtherwise.LastIndexOf("OR", StringComparison.CurrentCulture));
                    caseOtherwise = "IF NOT (" + caseOtherwise + ") THEN";
                    copyOfCaseBlock.Add("END IF");
                    copyOfCaseBlock.Add(caseOtherwise);
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter == 0)
                {
                    caseConditions.Add(sentense.Replace("CASE", ""));
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                if (sentense.StartsWith("CASE ") && caseCounter >= 1)
                {
                    caseConditions.Add(sentense.Replace("CASE", ""));
                    copyOfCaseBlock.Add("END IF");
                    sentense = sentense.Replace("CASE", "IF") + " THEN";
                    copyOfCaseBlock.Add(sentense);
                    caseCounter++;
                    continue;
                }
                copyOfCaseBlock.Add(sentense);
            }
            return Ok(copyOfCaseBlock);
        }

        [HttpGet]
        public async Task<IHttpActionResult> ViewSource(int solutionId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetAllItems(p => p.SolutionId == solutionId).ContinueWith(t =>
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
            return Ok("Processed");
        }

        [HttpGet]
        public IHttpActionResult ChangeTxtExtensionToQry(string directoryPath, string searchExt, string replaceExt)
        {
            var allFiles = Directory.GetFiles(directoryPath, searchExt, SearchOption.TopDirectoryOnly).ToList();

            foreach (var file in allFiles)
            {
                string newFileName = Path.GetFileNameWithoutExtension(file);
                if (newFileName == null) continue;
                int firstIndex = newFileName.IndexOf("_", StringComparison.Ordinal);
                string lastOrDefault = newFileName.Substring(firstIndex + 1);
                newFileName = lastOrDefault.Replace("~", "");
                var allLines = File.ReadAllLines(file);
                File.WriteAllLines(directoryPath + "\\" + newFileName + replaceExt, allLines);
            }

            return Ok("Done");
        }

        [HttpGet]
        public IHttpActionResult Start()
        {
            Task doTask = Task.Factory.StartNew(() => DoTask());
            var tokenSource = new CancellationTokenSource();
            Task task = LongRunningAsync(tokenSource.Token);
            tokenSource.Token.Register(async () => await LongRunningAsync(tokenSource.Token));

            //Task[] tasks = { doTask, task };
            Task thisTask = doTask.ContinueWith(t => task, tokenSource.Token);
            //CancelRequest();
            var x = Task.WaitAny(thisTask);
            Console.WriteLine("Task Started!");
            Thread.Sleep(1000);
            return Ok(tokenSource.Token);
        }

        private Task DoTask()
        {
            return new Task(EmptyTask);
        }

        private void EmptyTask()
        {
            Console.WriteLine("Task Started!");
        }

        [HttpPost]
        public void Stop([FromBody] CancellationTokenSource cancellationToken)
        {
            CancellationTokenSource tokenSource = cancellationToken;
            tokenSource.Cancel();
            // LongRunningAsync(tokenSource.Token);
        }

        private async Task LongRunningAsync(CancellationToken cancellationToken)
        {
            var taskCompletionSource = new TaskCompletionSource<IHttpActionResult>();
            await Task.Factory.StartNew(() =>
            {
                if (!cancellationToken.IsCancellationRequested)
                {
                    for (int i = 0; i < 99999999; i++)
                    {
                        if (cancellationToken.IsCancellationRequested)
                            cancellationToken.ThrowIfCancellationRequested();

                        Console.WriteLine("Iteration:- " + i);
                        Thread.Sleep(1500);
                    }
                    taskCompletionSource.SetResult(Ok("Process completed!"));
                    return taskCompletionSource.Task;
                }
                taskCompletionSource.SetException(new OperationCanceledException());

                return taskCompletionSource.Task;
            }, cancellationToken);
            //return taskCompletionSource.Task;
        }

        public string GetFormCaption(List<string> fileLines)
        {
            int index = fileLines.FindIndex(s => s.Contains("Begin Form"));
            string caption = string.Empty;
            for (int i = index + 1; i < fileLines.Count; i++)
            {
                string currentLine = fileLines[i];
                if (currentLine.StartsWith("Caption"))
                {
                    caption = currentLine.Substring(currentLine.IndexOf('=') + 1);
                    caption = caption.Replace("\"", "").Replace("&", "");
                    if (fileLines.Count < i + 1 && fileLines[i + 1].StartsWith("\""))
                    {
                        caption += fileLines[i + 1].Replace("\"", "").Replace("&", "");
                    }
                    break;
                }
                if (currentLine.StartsWith("Begin")) break;
            }
            return caption;
        }

        [HttpGet]
        public void FindCaption()
        {
            List<string> tempList = new List<string>
            {
                "ckSelect",
                "cmdClearAll",
                "cmdExit",
                "cmdPrintForms",
                "cmdPrintLetters"
            };
            var allLines =
                File.ReadAllLines(@"D:\Auctor\CodeVorto\ExportedApp_Main\Forms\frmAboutToExpire.frm").ToList();
            allLines = allLines.Select(l => l.Trim()).ToList();
            allLines = allLines.Where(l => !l.StartsWith("0x") && l != "" && !l.StartsWith("'")).ToList();
            foreach (var cStr in tempList)
            {
                if (!allLines.Any(l => l.Contains(cStr))) continue;

                int index = allLines.FindIndex(s => s.Contains(cStr));
                string caption = string.Empty;
                for (int i = index; i < allLines.Count; i++)
                {
                    string sss = allLines[i];
                    if (sss.StartsWith("Caption"))
                    {
                        caption = sss.Substring(sss.IndexOf('=') + 1);
                        caption = caption.Replace("\"", "").Replace("&", "");
                    }
                    if (sss == "End" || sss.StartsWith("Begin")) break;
                }
                if (string.IsNullOrEmpty(caption))
                    Console.WriteLine(cStr);
                Console.WriteLine(caption);
            }

            GetFormCaption(allLines);
        }

        [HttpGet]
        public async void UpdateProjectId()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository.GetAllItems().ConfigureAwait(false);
                foreach (var file in allFiles)
                {
                    var file1 = file;
                    var allStatements =
                        await _codeVortoService.StatementReferenceMasterRepository
                            .GetAllListItems(s => s.FileId == file1.FileId).ConfigureAwait(false);

                    var allStmts = allStatements.ToList();
                    foreach (var sMaster in allStmts)
                    {
                        sMaster.ProjectId = file.ProjectId;
                        await
                            _codeVortoService.StatementReferenceMasterRepository.UpdateItem(sMaster)
                                .ConfigureAwait(false);
                    }
                }
            }
        }

        [HttpGet]
        public string TestRemovePart()
        {
            var regexFir = new Regex("(.*)(\"(.*)\'(.*)\")");
            var regexSec = new Regex("\'(.*)");
            List<string> test = new List<string>
            {
                "sCriteria = \"[IndividualSSN] like '\" & sInput & \"'\"",
                "  Me.cmdFindPrevious.Enabled = True         '10/22/2002",
                " StatusBarText =\"Individual's Last Name\""
            };
            foreach (var str in test)
            {
                if (regexFir.IsMatch(str))
                {
                    Console.WriteLine(test);
                    continue;
                }
                if (regexSec.IsMatch(str))
                {
                    Console.WriteLine(str);
                }
            }
            return "Test";
        }

        [HttpGet]
        public IHttpActionResult FunctionConvertPseudoCode(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                int languageId = projectDetails.LanguageId;

                #region Apply PseudoCode Conversion for if statement

                string strQuery =
                 " SELECT S.StatementID, S.FileId, S.ProjectId, S.BaseCommandId,S.OriginalStatement, S.PrimaryCommandId " +
                 " FROM statementreferencemaster S  WHERE S.ProjectId = " + projectId + " AND S.BaseCommandID = 1 and S.AlternateName is null; ";
                var appBlock = new MySqlDbConnectionBaseClass();
                DataSet dsStatementRefer = appBlock.ExecuteNonQuery(strQuery, "");
                string strQuery1 = "SELECT * FROM regexpatternmaster where LanguageId =" + languageId + "; ";
                DataSet dsRegexData = appBlock.ExecuteNonQuery(strQuery1, "");
                if (dsStatementRefer.Tables.Count > 0 && dsStatementRefer.Tables[0].Rows.Count > 0)
                {
                    for (int iCnt = 0; iCnt < dsStatementRefer.Tables[0].Rows.Count; iCnt++)
                    {
                        try
                        {
                            var strStatement = dsStatementRefer.Tables[0].Rows[iCnt]["OriginalStatement"].ToString();
                            var iStatementId = Convert.ToInt32(dsStatementRefer.Tables[0].Rows[iCnt]["StatementId"]);
                            var iBaseCommandId = Convert.ToInt32(dsStatementRefer.Tables[0].Rows[iCnt]["BaseCommandId"]);
                            DataRow[] matchedRegex = dsRegexData.Tables[0].Select("BaseCommandId=" + iBaseCommandId);
                            foreach (var row in matchedRegex)
                            {
                                try
                                {
                                    var strPattern = row["RegexPattern"].ToString();
                                    Regex regex = new Regex(strPattern);
                                    var strAlternateCodeRepresent = row["AlternateCodeRepresentation"].ToString();
                                    string strAlternateStatement = "";
                                    if (regex.IsMatch(strStatement))
                                    {
                                        int index = 1;
                                        foreach (Match match in regex.Matches(strStatement))
                                        {
                                            var aaa = match.Groups[1].Value;
                                            strAlternateStatement =
                                                strAlternateCodeRepresent.Replace("<<" + index + ">>",
                                                    aaa);
                                            index++;
                                            break;

                                        }

                                        strAlternateStatement = strAlternateStatement.Replace(" >= ",
                                            " is greater than or equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" <= ",
                                            " is less than or equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" => ",
                                            " is equal to or greater than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" =< ",
                                            " is equal to or less than ");
                                        strAlternateStatement = strAlternateStatement.Replace(">= ",
                                            " is greater than or equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace("<= ",
                                            " is less than or equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace("=> ",
                                            " is equal to or greater than ");
                                        strAlternateStatement = strAlternateStatement.Replace("=< ",
                                            " is equal to or less than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" != ",
                                            " is not equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" Ne ",
                                            " is not equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" <> ",
                                            " is not equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" Gt ",
                                            " is greater than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" Lt ", " is less than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" > ", " is greater than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" < ", " is less than ");
                                        strAlternateStatement = strAlternateStatement.Replace(" = ", " is equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" Not= ",
                                            " is not equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(" NE ", " is not equal to ");
                                        strAlternateStatement = strAlternateStatement.Replace(")NE ", ") is not equal to ");


                                        /*                 List<string> actualString = new List<string>(new string[] { " >=", " <=", " = ", "=", " <> " });
                                        List<string> replaceString = new List<string>(new string[] { "is greater than or equal to", "is less than or equal to ", "is equal to", "is equal to", "is not equal to " });
                                                        var aaassds = actualString.FindIndex(x => x.Contains(strStatement));
                                                        var djhsdfj = aaassds;*/
                                        strQuery = " UPDATE statementreferencemaster "
                                                   + " SET AlterNatename = '" +
                                                   strAlternateStatement.Replace("\'", "\"").Replace("\"", "") + "' "
                                                   + " WHERE StatementId = " + iStatementId + " ";
                                        appBlock.ExecuteNonQuery(strQuery, "");

                                        strQuery = " UPDATE SecondTabProgramDetails "
                                                   + " SET AlterNatename = '" +
                                                   strAlternateStatement.Replace("\'", "\"") +
                                                   "' "
                                                   + " WHERE StatementId = " + iStatementId + " ";

                                        appBlock.ExecuteNonQuery(strQuery, "");

                                        strQuery = " UPDATE WorkflowTreeviewSecondTabDetails "
                                                   + " SET AlterNatename = '" +
                                                   strAlternateStatement.Replace("\'", "\"") +
                                                   "' "
                                                   + " WHERE StatementId = " + iStatementId + " ";

                                        appBlock.ExecuteNonQuery(strQuery, "");
                                        break;

                                    }
                                }
                                catch (Exception exception)
                                {
                                    LogMessage.WriteExceptionLogMessage(exception);
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
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> FunctionConvertSubRoutinePseudoCode(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectDetails = projectMasterRepository.GetItem(projectId);
                if (projectDetails == null) return Ok(projectId);
                #region Apply PseudoCode Conversion for if statement

                string strQuery =
                    "SELECT * FROM statementreferencemaster   WHERE ProjectId = " + projectId + " AND BaseCommandID = 1 ; ";
                var statementRef = await
                    _codeVortoService.StatementReferenceMasterRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                        strQuery).ConfigureAwait(false);
                var appBlock = new MySqlDbConnectionBaseClass();
                foreach (var statement in statementRef)
                {
                    string strAlternateStatement = string.Empty;
                    if (statement.AlternateName.Contains(" LE ") || statement.AlternateName.Contains(" Le "))
                    {
                        strAlternateStatement = statement.AlternateName.Replace(" LE ", " is less than or equal to ").Replace(" Le ", " is less than or equal to ");
                    }
                    if (statement.AlternateName.Contains(" GE ") || statement.AlternateName.Contains(" Ge "))
                    {
                        strAlternateStatement = statement.AlternateName.Replace(" GE ", " is greater than or equal to ").Replace(" Ge ", " is greater than or equal to ");
                    }
                    if (statement.AlternateName.Contains(" LT ") || statement.AlternateName.Contains(" Lt "))
                    {
                        strAlternateStatement = statement.AlternateName.Replace(" Lt ", " is less than ").Replace(" LT ", " is less than ");
                    }
                    if (statement.AlternateName.Contains(" GT ") || statement.AlternateName.Contains(" Gt "))
                    {
                        strAlternateStatement = statement.AlternateName.Replace(" GT ", " is greater than ").Replace(" Gt ", " is greater than ");
                    }
                    if (statement.AlternateName.Contains(" NE ") || statement.AlternateName.Contains(" Ne "))
                    {
                        strAlternateStatement = statement.AlternateName.Replace(" NE ", " is not equal to ").Replace(" Ne ", " is not equal to ");
                    }
                    if (string.IsNullOrWhiteSpace(strAlternateStatement)) continue;
                    strQuery = " UPDATE statementreferencemaster "
                               + " SET AlternateName = '" + strAlternateStatement.Replace("\'", "\"").Replace("\"", "") + "' "
                               + " WHERE StatementId = " + statement.StatementId + " ";
                    appBlock.ExecuteNonQuery(strQuery, "");

                    var actualStaete = "Actual_" + statement.StatementId;
                    strQuery = " UPDATE secondtabsubroutineprogramdetails "
                               + " SET AlternateName = '" + strAlternateStatement.Replace("\'", "\"").Replace("\"", "") + "' "
                               + " WHERE ActualStatementId = '" + actualStaete + "' ";
                    appBlock.ExecuteNonQuery(strQuery, "");

                }



                #endregion
            }
            return Ok("done");
        }

        [HttpGet]
        public IHttpActionResult LogFileTesting()
        {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("Testing");
            LogMessage.WriteLogMessage(stringBuilder);
            return Ok("ok");
        }

        [HttpGet]
        public async Task<IHttpActionResult> TestDataDepentency(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                #region region 8...
                var fileMasterData =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId)
                        .ContinueWith(t =>
                        {
                            var result = t.Result;
                            return result.ToList();
                        });
                object[] andParameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""}
                };
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", andParameters);
                var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;
                var regexSql = new Regex(@"[\""]SELECT \*");
                foreach (var constructor in fileMasterData)
                {
                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.Contains("\"Select") ||
                                                                               x.OriginalStatement.Contains("\"SELECT")))
                        .ToList();
                    string result = string.Empty;
                    var lstDataDependency = new List<DataDependency>();

                    if (!selectStatName.Any()) continue;

                    foreach (var itemSelect in selectStatName)
                    {
                        int aaa;
                        string bbb;
                        string attrStmt;
                        string attributes = null;

                        if (regexSql.IsMatch(itemSelect.OriginalStatement.ToUpper().TrimEnd()))
                        {
                            if (itemSelect.OriginalStatement.Contains("where") ||
                                itemSelect.OriginalStatement.Contains("WHERE"))
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("where", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(0, aaa).Trim();
                                bbb = RemoveCheckSpecialChars(bbb);
                                result = bbb.Trim().Split(' ').LastOrDefault();

                                attrStmt = itemSelect.OriginalStatement.Substring(aaa).Trim();
                                string[] value = attrStmt.Split(' ');
                                for (int i = 0; i < value.Length; i++)
                                {
                                    if (!value[i].StartsWith("=") && value[i].Contains("="))
                                    {
                                        attributes = attributes + "," + value[i].Split('=').FirstOrDefault();
                                    }
                                    else
                                    {
                                        if (value[i] == "=" || value[i] == "=\"")
                                        {
                                            attributes = attributes + "," + value[i - 1];
                                        }
                                    }
                                }

                                if (!string.IsNullOrEmpty(attributes))
                                {
                                    attributes = RemoveSpecialChars(attributes);
                                    attributes = attributes.TrimStart(',').Trim();
                                }
                            }
                            else
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("from", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim();
                                result = bbb.Split(' ').FirstOrDefault();
                                result = RemoveSpecialChars(result);
                            }
                        }
                        else
                        {
                            if (itemSelect.OriginalStatement.Contains("from") ||
                                itemSelect.OriginalStatement.Contains("FROM"))
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("from", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim().Replace("GROUP", "")
                                    .Replace("WHERE", "");
                                result = bbb.Split(' ').FirstOrDefault();
                                result = RemoveSpecialChars(result);

                                attrStmt = itemSelect.OriginalStatement.Substring(0, aaa).Split('=')
                                    .LastOrDefault().Trim().Replace("Select", "").Replace("SELECT", "")
                                    .Replace("DISTINCTROW", "").Trim();

                                if (attrStmt.Contains(","))
                                {
                                    string[] value = attrStmt.Split(',');

                                    attributes = value.Aggregate(attributes,
                                        (current, t) => current + "," + t.TrimStart().TrimEnd());
                                }
                                else
                                {
                                    attributes = attrStmt;
                                }
                                attributes = RemoveSpecialChars(attributes);
                                attributes = attributes.TrimStart(',').Trim();
                            }
                        }
                        //var dataDependencyRepository = new DataDependencyRepository(new AppDbContext());
                        DataDependency datadependency = new DataDependency
                        {
                            DataDepedencyId = 0,
                            ProjectId = projectId,
                            FileId = constructor.FileId,
                            Entity = result,
                            Attributes = attributes
                        };

                        List<int> iCnt = (from dd in lstDataDependency
                                          where dd.Entity == result && dd.FileId == constructor.FileId
                                          select dd.FileId).ToList();
                        if (iCnt.Count == 0)
                        {
                            lstDataDependency.Add(datadependency);
                        }
                    }

                    await _codeVortoService.DataDependencyRepository.BulkInsert(listOfEntities: lstDataDependency);
                }

                #endregion
            }
            return Ok("done");
        }

        public string RemoveSpecialChars(string input)
        {
            return Regex.Replace(input, @"[^a-zA-Z\._,]", string.Empty);
        }

        public string RemoveCheckSpecialChars(string input)
        {
            return Regex.Replace(input, @"[^0-9a-zA-Z\._,*=() ]", string.Empty);
        }

        [HttpGet]
        public async Task<IHttpActionResult> TestDataDependancyNew(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMasterData =
                    await _codeVortoService.FileMasterRepository.GetAllItems(p => p.ProjectId == projectId)
                        .ContinueWith(
                            t =>
                            {
                                var result = t.Result;
                                return result.ToList();
                            });

                object[] andParamters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""},
                };
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", andParamters);
                var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;


                #region region 8 ...

                var regexSql = new Regex(@"[\""]SELECT \*");
                foreach (var constructor in fileMasterData)
                {
                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.Contains("\"Select") ||
                                                                               x.OriginalStatement.Contains("\"SELECT")))
                        .ToList();
                    string result = string.Empty;
                    var lstDataDependency = new List<DataDependency>();

                    if (!selectStatName.Any()) continue;

                    foreach (var itemSelect in selectStatName)
                    {
                        int aaa;
                        string bbb;
                        string attrStmt;
                        string attributes = null;

                        if (regexSql.IsMatch(itemSelect.OriginalStatement.ToUpper().TrimEnd()))
                        {
                            if (itemSelect.OriginalStatement.Contains("where") ||
                                itemSelect.OriginalStatement.Contains("WHERE"))
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("where", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(0, aaa).Trim();
                                bbb = RemoveCheckSpecialChars(bbb);
                                result = bbb.Trim().Split(' ').LastOrDefault();
                                if (result != null && result.Contains(')'))
                                    result = result.Replace(")", "");

                                attrStmt = itemSelect.OriginalStatement.Substring(aaa).Trim();
                                string[] value = attrStmt.Split(' ');
                                for (int i = 0; i < value.Length; i++)
                                {
                                    if (!value[i].StartsWith("=") && value[i].Contains("="))
                                    {
                                        attributes = attributes + "," + value[i].Split('=').FirstOrDefault();
                                    }
                                    else
                                    {
                                        if (value[i] == "=" || value[i] == "=\"")
                                        {
                                            attributes = attributes + "," + value[i - 1];
                                        }
                                    }
                                }

                                if (!string.IsNullOrEmpty(attributes))
                                {
                                    attributes = RemoveSpecialChars(attributes);
                                    attributes = attributes.TrimStart(',').Trim();
                                }
                            }
                            else
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("from", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim();
                                result = bbb.Split(' ').FirstOrDefault();
                                result = RemoveSpecialChars(result);
                                if (result.Contains(')'))
                                {
                                    result = result.Replace(")", "");
                                }
                            }
                        }
                        else
                        {
                            if (itemSelect.OriginalStatement.Contains("from") ||
                                itemSelect.OriginalStatement.Contains("FROM"))
                            {
                                aaa = itemSelect.OriginalStatement.IndexOf("from", StringComparison.OrdinalIgnoreCase);
                                bbb = itemSelect.OriginalStatement.Substring(aaa + 4).Trim().Replace("GROUP", "")
                                    .Replace("WHERE", "");
                                result = bbb.Split(' ').FirstOrDefault();
                                result = RemoveSpecialChars(result);
                                if (result.Contains(')'))
                                {
                                    result = result.Replace(")", "");
                                }
                                attrStmt = itemSelect.OriginalStatement.Substring(0, aaa).Split('=')
                                    .LastOrDefault().Trim().Replace("Select", "").Replace("SELECT", "")
                                    .Replace("DISTINCTROW", "").Trim();

                                if (attrStmt != null && attrStmt.Contains(","))
                                {
                                    string[] value = attrStmt.Split(',');

                                    attributes = value.Aggregate((string)null,
                                        (current, t) => current + "," + t.TrimStart().TrimEnd());
                                }
                                else
                                {
                                    attributes = attrStmt;
                                }
                                attributes = RemoveSpecialChars(attributes);
                                attributes = attributes.TrimStart(',').Trim();
                            }
                        }
                        //var dataDependencyRepository = new DataDependencyRepository(new AppDbContext());
                        var datadependency = new DataDependency
                        {
                            DataDepedencyId = 0,
                            ProjectId = projectId,
                            FileId = constructor.FileId,
                            Entity = result,
                            Attributes = attributes,
                            EntityOld = result,
                            FileIdOld = constructor.FileId,
                            FileMaster = null
                        };

                        var iCnt = (from dd in lstDataDependency
                                    where result != null && (dd.Entity.ToLower() == result.ToLower() && dd.FileId == constructor.FileId)
                                    select dd.FileId).ToList();
                        if (iCnt.Count == 0)
                        {
                            lstDataDependency.Add(datadependency);
                        }
                    }
                    await _codeVortoService.DataDependencyRepository.BulkInsert(listOfEntities: lstDataDependency);
                }

                foreach (var constructor in fileMasterData)
                {
                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.StartsWith(
                                                                                   "Begin InputTables") ||
                                                                               x.OriginalStatement.StartsWith(
                                                                                   "\"Begin InputTables"))).ToList();

                    if (!selectStatName.Any()) continue;
                    var sqlstatementReferMast =
                        " select * from statementreferencemaster where StatementId Between(select statementId from statementreferencemaster " +
                        " where OriginalStatement ='Begin InputTables' and StatementId >= " +
                        selectStatName[0].StatementId +
                        " LIMIT 1) and(select StatementId From StatementReferenceMaster Where OriginalStatement ='End' AND StatementId >= " +
                        selectStatName[0].StatementId + " LIMIT 1) And StatementId >= " + selectStatName[0].StatementId +
                        "; ";
                    var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlstatementReferMast);

                    if (!selectStatName.Any()) continue;
                    foreach (var itemSelect in statementReference)
                    {
                        if (!itemSelect.OriginalStatement.StartsWith("Name")) continue;
                        var mainStatement = itemSelect.OriginalStatement.Between("\"", "\"");
                        var datadependency = new DataDependency
                        {
                            DataDepedencyId = 0,
                            ProjectId = projectId,
                            FileId = constructor.FileId,
                            Entity = mainStatement,
                            Attributes = null,
                            EntityOld = mainStatement,
                            FileIdOld = constructor.FileId,
                            FileMaster = null
                        };
                        await _codeVortoService.DataDependencyRepository.AddNewItem(datadependency);
                    }
                }
            }
            #endregion

            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateWorkflowNames(int solutionId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                // Get all projects within this solutionId...
                var allProjects = await _codeVortoService.ProjectMasterRepository
                    .GetAllListItems(e => e.SolutionId == solutionId).ConfigureAwait(false);

                using (var appDbContext = new AppDbContext())
                {
                    var allItems = await appDbContext.UniverseFileMenu.ToListAsync().ConfigureAwait(false);

                    foreach (var project in allProjects)
                    {
                        // Get all action workflows for current project...
                        var project1 = project;
                        var allActionWorkflows = await _codeVortoService.ActionWorkflowsRepository
                            .GetAllListItems(e => e.ProjectId == project1.ProjectId).ConfigureAwait(false);

                        foreach (var actionWorkflow in allActionWorkflows)
                        {
                            var fileName = Path.GetFileNameWithoutExtension(actionWorkflow.OriginFilePath);
                            if (string.IsNullOrEmpty(fileName)) continue;

                            if (fileName.Contains("."))
                            {
                                string beforeDot = fileName.Split('.')[1];
                                var hasValue = allItems.Any(s => beforeDot != null
                                    && (s.ActionExecuted.StartsWith(fileName) ||
                                    s.ActionExecuted.StartsWith(beforeDot)));

                                if (!hasValue) continue;

                                var fileMenu = allItems.Find(s => beforeDot != null
                                                                    && (s.ActionExecuted.StartsWith(fileName) ||
                                                                        s.ActionExecuted.StartsWith(beforeDot)));
                                actionWorkflow.WorkflowName = fileMenu.WorkflowMenuName;
                                // actionWorkflow.WorkflowBusinessName = fileMenu.WorkflowMenuName;

                                await _codeVortoService.ActionWorkflowsRepository
                                    .UpdateItem(actionWorkflow).ConfigureAwait(false);
                            }
                            else
                            {
                                var hasValue = allItems.Any(s => s.ActionExecuted.StartsWith(fileName));

                                if (!hasValue) continue;

                                var fileMenu = allItems.Find(s => s.ActionExecuted.StartsWith(fileName));
                                actionWorkflow.WorkflowName = fileMenu.WorkflowMenuName;
                                // actionWorkflow.WorkflowBusinessName = fileMenu.WorkflowMenuName;

                                await _codeVortoService.ActionWorkflowsRepository
                                    .UpdateItem(actionWorkflow).ConfigureAwait(false);
                            }
                        }
                    }
                }
                return Ok("Action workflows updated successfully");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> DataDepdancy(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMasterData = await _codeVortoService.FileMasterRepository.GetAllListItems(p => p.ProjectId == projectId);

                object[] andParameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                    new MySqlParameter("@andCondition", MySqlDbType.VarChar) {Value = ""}
                };
                var allStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllProjectItemsByCondition", andParameters);
                var statementReferenceMasters = (IList<StatementReferenceMaster>)allStatements;


                #region 10 for DataDependancy for RecordSource

                foreach (var constructor in fileMasterData)
                {

                    var selectStatName = statementReferenceMasters.Where(x => x.ProjectId == projectId
                                                                              && x.FileId == constructor.FileId &&
                                                                              (x.OriginalStatement.StartsWith("RecordSource") ||
                                                                               x.OriginalStatement.StartsWith("\"RecordSource"))).ToList();
                    if (!selectStatName.Any()) continue;
                    foreach (var statement in selectStatName)
                    {
                        var sqlQuery = "select * from statementreferencemaster where ClassNameDeclared='" +
                                       statement.ClassCalled + "';";
                        var statementReference = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                        if (statementReference.Count != 0) continue;
                        var mainStatement = statement.OriginalStatement.Between("\"", "\"");
                        if (mainStatement.StartsWith("Select") || mainStatement.StartsWith("SELECT")) continue;
                        var datadependency = new DataDependency
                        {
                            DataDepedencyId = 0,
                            ProjectId = projectId,
                            FileId = constructor.FileId,
                            Entity = mainStatement,
                            Attributes = null
                        };
                        await _codeVortoService.DataDependencyRepository.AddNewItem(datadependency);

                    }


                }

                #endregion

            }


            return StatusCode(HttpStatusCode.OK);
        }

        [HttpGet]
        public async Task<HttpResponseMessage> UpdateDataIoStatements(int projectId)
        {
            #region Update Data I/O Statements and all statements that using those variable, set BaseCommand Id = 45...

            using (_codeVortoService = new CodeVortoService())
            {
                string sqlOpenRecordset = " SELECT * FROM StatementReferenceMaster WHERE OriginalStatement " +
                    " LIKE '%.OpenRecordset%' AND ProjectId = " + projectId + "; ";
                var allRecordSetStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlOpenRecordset).ConfigureAwait(false);
                foreach (var statementRef in allRecordSetStatements)
                {
                    statementRef.BaseCommandId = 6;
                    statementRef.ClassCalled = "Database";
                    statementRef.MethodCalled = "OpenRecordset ( )";

                    await _codeVortoService.StatementReferenceMasterRepository
                        .UpdateItem(statementRef).ConfigureAwait(false);

                    var dataObjectType = statementRef.OriginalStatement.Replace("Set ", "");
                    string variableName = dataObjectType.Split(new[] { "=" }, StringSplitOptions.None)[0];

                    variableName = variableName.Trim();
                    string sqlDataIoStats =
                        " SELECT * FROM StatementReferenceMaster WHERE OriginalStatement LIKE '%" + variableName + ".%' " +
                        " OR OriginalStatement LIKE '%" + variableName + "!%' AND FileId = " + statementRef.FileId;
                    var allDataIoStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataIoStats).ConfigureAwait(false);

                    foreach (var dataIoState in allDataIoStatements)
                    {
                        if (dataIoState.BaseCommandId != 0) continue;

                        dataIoState.VariableNameDeclared = variableName;
                        dataIoState.BaseCommandId = 45;
                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(dataIoState).ConfigureAwait(false);
                    }
                }
            }

            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent("Process completed successfully")
            };
            #endregion
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateDataIoStatementForUniverse(int projectId)
        {

            #region Update field for basecommandId = 45

            //var sqlQuery = "SELECT * FROM DataDependency WHERE projectId != 1;";
            //var dataDependancy = await _codeVortoService.DataDependencyRepository
            //    .GetDataFromSqlQuery<DataDependency>(sqlQuery).ConfigureAwait(false);

            var sqlQuery = " SELECT  * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectId + "  GROUP BY FileName;";
            var universeBasicDataDicyionary =
                await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

            foreach (var dataDictionary in universeBasicDataDicyionary)
            {
                string tableName = dataDictionary.Description;

                string fileName = dataDictionary.FileName;
                string sqlQry = "SELECT * from statementreferencemaster WHERE projectId=" + projectId +
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


            #endregion

            return Ok("done");
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
                var sqlQuery = "SELECT * FROM DataDependency WHERE projectId != 1;";
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
                    string sqlQery = "select * from statementReferencemaster where OriginalStatement like '%" + entityName + "%' " +
                                     "AND FileId = " + dependancy.FileId + " AND ( ";
                    int i = 0;
                    foreach (var list in mainList)
                    {
                        i++;
                        sqlQery += i == 1
                            ? " OriginalStatement LIKE '%" + list + "%'"
                            : " OR OriginalStatement LIKE '%" + list + "%'";
                    }
                    sqlQery += ")";

                    var statementList =
                        await
                            _codeVortoService.StatementReferenceMasterRepository
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
                    var crud =
                        await
                            _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
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
        public async Task<IHttpActionResult> InsertCurdActivity()
        {
            using (var codeVortoService = new CodeVortoService())
            {
                var insertList = new List<string> {"MATWRITE"};
                var updateList = new List<string> {"MATREADU"};
                var deleteList = new List<string> {"DELETE"};
                var selectList = new List<string> {"MATREAD"};
                var sqlQuery = "SELECT * FROM DataDependency WHERE projectId != 1;";
                var dataDependancy = await _codeVortoService.DataDependencyRepository
                    .GetDataFromSqlQuery<DataDependency>(sqlQuery).ConfigureAwait(false);
                foreach (var dependancy in dataDependancy)
                {
                    string tableName = dependancy.Entity;
                    string sqlQry = "SELECT * from statementreferencemaster WHERE" +
                                    " AlteAlternateNamernate like '%" + tableName + "%' OR BusinessName '%" + tableName +
                                    "%';";
                    var statementRefMaster =
                        await
                            _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                    foreach (var statementRef in statementRefMaster)
                    {
                        #region Added into DbCrudActivityTable

                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "N";
                        var dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = tableName,
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
                        string sqlQery = "select * from statementReferencemaster where AlteAlternateNamernate like '%" + tableName + "%' " +
                                         "AND ( ";
                        int i = 0;
                        foreach (var list in mainList)
                        {
                            i++;
                            sqlQery += i == 1
                                ? " OriginalStatement LIKE '%" + list + "%'"
                                : " OR OriginalStatement LIKE '%" + list + "%'";
                        }
                        sqlQery += ")";

                        var statementList =
                            await
                                codeVortoService.StatementReferenceMasterRepository
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
                                EntityName = dataDictionary.Description,
                                SelectOrRead = select,
                                InsertOrCreate = iR,
                                Update = update,
                                Delete = delete,
                                FileId = fileId[0].FileId
                            };
                        }
                        string entity = dataDictionary.Description;
                        int fileIdNew = fileId[0].FileId;
                        Expression<Func<DbCrudActivity, bool>> expression =
                            x => x.EntityName == entity && x.FileId == fileIdNew;
                        var crud =
                            await
                                codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                                    .ConfigureAwait(false);
                        if (crud.Count != 0) continue;
                        await codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                        #endregion
                    }
                }
            }
        }
        */

        [HttpGet]
        public async Task<IHttpActionResult> InsertDataDependancyForUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    string sqlQry = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId= " + projectId + " AND GROUP BY FileName";

                    var universebasicdatadictionarywithFileName =
                        await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQry)
                            .ConfigureAwait(false);

                    string sqlQuery = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId= " + projectId + ";";

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
                        string sqlStmtMaster = "SELECT * FROM statementreferencemaster WHERE projectId =" + projectId + " " +
                                               "AND  OriginalStatement LIKE '%" + fileName + "%';";
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
        public async Task<IHttpActionResult> DataDependancyforUniverse(int projectId)
        {

            using (var codeVortoService = new CodeVortoService())
            {
                var insertList = new List<string> { "MATWRITE" };
                var updateList = new List<string> { "MATREADU" };
                var deleteList = new List<string> { "DELETE" };
                var selectList = new List<string> { "MATREAD" };
                string sqlQry = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId =" + projectId + "  GROUP BY FileName";

                var universebasicdatadictionarywithFileName =
                   await codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQry)
                        .ConfigureAwait(false);

                string sqlQuery = "SELECT * FROM UniverseBasicDataDictionary WHERE ProjectId = " + projectId + ";";
                var universebasicdatadictionary =
                    await codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

                var fileMaster =
                    await codeVortoService.FileMasterRepository.GetAllListItems(x => x.ProjectId == 2)
                            .ConfigureAwait(false);

                foreach (var dataDictionary in universebasicdatadictionarywithFileName)
                {
                    var fileId = (from f in fileMaster
                                  where Path.GetFileNameWithoutExtension(f.FilePath) == dataDictionary.FileName
                                  select f).ToList();

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
                    var dataDependancy = new DataDependency
                    {
                        FileId = fileId[0].FileId,
                        Entity = dataDictionary.Description,
                        Attributes = string.Join(",", attribute),
                        ProjectId = 2
                    };
                    await codeVortoService.DataDependencyRepository.AddNewItem(dataDependancy);

                    string sqlQury =
                        "SELECT * FROM statementreferencemaster WHERE OriginalStatement" +
                        " LIKE '%" + dataDictionary.FileName + "%' AND BaseCommandId = 45 AND projectId != 2;";
                    var statemenRefMaster =
                        await
                            codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQury).ConfigureAwait(false);
                    foreach (var statementRef in statemenRefMaster)
                    {
                        if (statementRef.BaseCommandId != 0) continue;
                        int statementId = statementRef.StatementId;
                        statementRef.BaseCommandId = 45;
                        await codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef);

                        var treeViewTabSecond = await codeVortoService.WorkflowTreeviewSecondTabDetailsRepository
                            .GetAllListItems(s => s.StatementId == statementId).ConfigureAwait(false);

                        foreach (var sTab in treeViewTabSecond)
                        {
                            if (sTab.BaseCommandId != 0) continue;
                            sTab.BaseCommandId = 45;
                            sTab.ProgramId = fileId[0].FileId;
                            await codeVortoService.WorkflowTreeviewSecondTabDetailsRepository.UpdateItem(sTab);
                        }
                        var treeViewSecondTabProgram = await codeVortoService.SecondTabProgramDetailsRepository
                            .GetAllListItems(s => s.StatementId == statementId).ConfigureAwait(false);

                        foreach (var sTab in treeViewSecondTabProgram)
                        {
                            if (sTab.BaseCommandId != 0) continue;
                            sTab.BaseCommandId = 45;
                            sTab.ProgramId = fileId[0].FileId;
                            await codeVortoService.SecondTabProgramDetailsRepository.UpdateItem(sTab);
                        }
                    }
                    #region Added into DbCrudActivityTable

                    string iR = "N";
                    string update = "N";
                    string delete = "N";
                    string select = "N";
                    var dbCurdActivity = new DbCrudActivity
                    {
                        EntityName = dataDictionary.Description,
                        SelectOrRead = select,
                        InsertOrCreate = iR,
                        Update = update,
                        Delete = delete,
                        FileId = fileId[0].FileId
                    };
                    var mainList = new List<string>();
                    mainList.AddRange(insertList);
                    mainList.AddRange(deleteList);
                    mainList.AddRange(updateList);
                    mainList.AddRange(selectList);
                    string sqlQery = "select * from statementReferencemaster where OriginalStatement like '%" +
                                     dataDictionary.FileName + "%' " +
                                     "AND ( ";
                    int i = 0;
                    foreach (var list in mainList)
                    {
                        i++;
                        sqlQery += i == 1 ? " OriginalStatement LIKE '%" + list + "%'" : " OR OriginalStatement LIKE '%" + list + "%'";
                    }
                    sqlQery += ")";

                    var statementList =
                        await
                        codeVortoService.StatementReferenceMasterRepository.GetDataFromSqlQuery<StatementReferenceMaster>(sqlQery)
                        .ConfigureAwait(false);

                    if (statementList.Any())
                    {
                        foreach (string sss in statementList.Select(s => s.OriginalStatement))
                        {
                            if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                            if (updateList.Any(c => sss.Contains(c))) update = "Y";
                            if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                            if (selectList.Any(c => sss.Contains(c))) select = "Y";
                            //if (sss.Contains("MATREADU")) update = "Y";
                            //if (sss.Contains("DELETE")) delete = "Y";
                            //if (sss.Contains("MATWRITE")) iR = "Y";
                            //if (sss.Contains("MATREAD")) select = "Y";
                        }
                        dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = dataDictionary.Description,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = fileId[0].FileId
                        };
                    }
                    string entity = dataDictionary.Description;
                    int fileIdNew = fileId[0].FileId;
                    Expression<Func<DbCrudActivity, bool>> expression = x => x.EntityName == entity && x.FileId == fileIdNew;
                    var crud =
                        await
                            codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                                .ConfigureAwait(false);
                    if (crud.Count != 0) continue;
                    await codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                    #endregion

                }
                return Ok(universebasicdatadictionarywithFileName);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateDataDependancyTable(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);

                var allDataDependancyEntries = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(d => d.ProjectId == projectId).ConfigureAwait(false);

                var dictionary = new Dictionary<int, List<string>>();
                foreach (var file in allFiles)
                {
                    var allCurrentEntities = (from entry in allDataDependancyEntries
                                              where entry.FileIdOld == file.FileId
                                              select entry).ToList();

                    var allEntities = new List<string>();
                    foreach (var currentEntity in allCurrentEntities)
                    {
                        var cEntity = currentEntity.EntityOld;
                        if (!allFiles.Any(f => f.FileName.StartsWith(cEntity)))
                        {
                            allEntities.Add(cEntity);
                            continue;
                        }
                        var fileMaster
                            = allFiles.Find(f => f.FileName.StartsWith(cEntity));
                        if (fileMaster == null) continue;
                        GetEntitiesForFile(fileMaster, allFiles, allDataDependancyEntries, ref allEntities);
                    }
                    if (!allEntities.Any()) continue;

                    dictionary.Add(file.FileId, allEntities.Distinct().ToList());
                    Console.WriteLine("===============================================");
                    allEntities.ForEach(Console.WriteLine);
                    Console.WriteLine("===============================================");
                    var addedEntities = new List<string>();
                    foreach (var entity in allEntities)
                    {
                        if (string.IsNullOrEmpty(entity)) continue;
                        string newEntity = entity.Split(new[] { "015" }, StringSplitOptions.None).First();
                        if (addedEntities.Any(e => e == newEntity)) continue;
                        addedEntities.Add(newEntity);

                        var datadependency = new DataDependency
                        {
                            DataDepedencyId = 0,
                            ProjectId = projectId,
                            FileId = file.FileId,
                            Entity = newEntity,
                            Attributes = null,
                            FileMaster = null,
                            EntityOld = newEntity,
                            FileIdOld = file.FileId
                        };
                        await _codeVortoService.DataDependencyRepository.AddNewItem(datadependency);

                    }

                }

                return Ok(dictionary);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> DeleteMultipleEntriesFromDataDependency(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allDataDependancyEntries = await _codeVortoService.DataDependencyRepository
                    .GetAllListItems(d => d.ProjectId == projectId).ConfigureAwait(false);
                foreach (var dataDependency in allDataDependancyEntries)
                {
                    var allItems = await _codeVortoService.DataDependencyRepository
                        .GetAllListItems(d => d.FileId == dataDependency.FileId
                                              && d.Entity == dataDependency.Entity &&
                                              d.ProjectId == dataDependency.ProjectId).ConfigureAwait(false);
                    if (allItems.Count <= 1) continue;

                    int loop = -1;
                    foreach (var item in allItems)
                    {
                        loop++;
                        if (loop == 0) continue;
                        await _codeVortoService.DataDependencyRepository.DeleteItem(item.DataDepedencyId)
                            .ConfigureAwait(false);
                    }
                }
                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> DeleteMultipleEntriesFromCrudActivity(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(d => d.ProjectId == projectId).ConfigureAwait(false);
                var fileIds = (from f in allFiles select f.FileId).ToList();

                string sqlQuery = "SELECT * FROM DbCrudActivities WHERE FileId IN (" + string.Join(",", fileIds) + ");";
                var allCrudActivities = await _codeVortoService.DbCrudActivityRepository
                    .GetDataFromSqlQuery<DbCrudActivity>(sqlQuery).ConfigureAwait(false);

                foreach (var crudActivity in allCrudActivities)
                {
                    var allItems = allCrudActivities.Where(d => d.FileId == crudActivity.FileId
                    && d.InsertOrCreate == crudActivity.InsertOrCreate && d.Delete == crudActivity.Delete &&
                    d.EntityName == crudActivity.EntityName && d.SelectOrRead == crudActivity.SelectOrRead &&
                    d.Update == crudActivity.Update).ToList();

                    if (allItems.Count <= 1) continue;

                    int loop = -1;
                    foreach (var item in allItems)
                    {
                        loop++;
                        if (loop == 0) continue;
                        await _codeVortoService.DataDependencyRepository.DeleteItem(item.ActivityId)
                            .ConfigureAwait(false);
                    }
                }
                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateCrudActivity(int projectId)
        {
            #region Insert data into DbCrudActivity

            using (_codeVortoService = new CodeVortoService())
            {
                var dataDependancyList = await _codeVortoService.DataDependencyRepository
                        .GetAllListItems(p => p.ProjectId == projectId).ConfigureAwait(false);
                foreach (var dataList in dataDependancyList)
                {
                    string sqlQry = "select * from statementreferencemaster where projectId=" + projectId +
                                    " and FileId =" + dataList.FileId + " AND OriginalStatement like '%" +
                                    dataList.Entity + "%';";
                    var statementList = await _codeVortoService.StatementReferenceMasterRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);

                    string iR = "N";
                    string update = "N";
                    string delete = "N";
                    string select = "Y";

                    var dbCurdActivity = new DbCrudActivity
                    {
                        EntityName = dataList.Entity,
                        SelectOrRead = select,
                        InsertOrCreate = iR,
                        Update = update,
                        Delete = delete,
                        FileId = dataList.FileId
                    };

                    foreach (var statementRef in statementList)
                    {
                        var dataObjectType = statementRef.OriginalStatement.Replace("Set ", "");
                        string variableName = dataObjectType.Split(new[] { "=" }, StringSplitOptions.None)[0];
                        variableName = variableName.Trim().Replace("'", "");
                        string sqlDataIoStats =
                            " SELECT * FROM StatementReferenceMaster WHERE  FileId = " + statementRef.FileId +
                            " AND ProjectId = " + projectId + " AND ( OriginalStatement LIKE '%" + variableName +
                            ".AddNew%' " +
                            " OR OriginalStatement LIKE '%" + variableName + ".Update%' " +
                            " OR OriginalStatement LIKE '%" + variableName + ".Delete%');";
                        var allDataIoStatements = await _codeVortoService.StatementReferenceMasterRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataIoStats).ConfigureAwait(false);

                        foreach (string sss in allDataIoStatements.Select(s => s.OriginalStatement))
                        {
                            if (sss.Contains(".Update")) update = "Y";
                            if (sss.Contains(".Delete")) delete = "Y";
                            if (sss.Contains(".AddNew")) iR = "Y";
                        }
                        dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = dataList.Entity,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = dataList.FileId
                        };
                    }
                    string entity = dataList.Entity;
                    int fileId = dataList.FileId;
                    Expression<Func<DbCrudActivity, bool>> expression =
                        x => x.EntityName == entity && x.FileId == fileId;
                    var crud = await _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                        .ConfigureAwait(false);
                    if (crud.Count != 0) continue;
                    await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);
                }
                return Ok("Done Process");
            }


            #endregion

        }

        private void GetEntitiesForFile(FileMaster file, List<FileMaster> lstFileMaster,
            List<DataDependency> allDataDependancyEntries, ref List<string> entityList)
        {
            var allCurrentEntities = (from entry in allDataDependancyEntries
                                      where entry.FileId == file.FileId
                                      select entry.Entity).ToList();

            foreach (var currentEntity in allCurrentEntities)
            {
                if (currentEntity == "tblOrganizations") entityList.Add(currentEntity);
                if (entityList.Any(e => e == currentEntity)) continue;
                if (!lstFileMaster.Any(f => f.FileName.StartsWith(currentEntity))) { entityList.Add(currentEntity); continue; }
                var fileMaster = lstFileMaster.Find(f => f.FileName.StartsWith(currentEntity));
                if (fileMaster == null) continue;
                GetEntitiesForFile(fileMaster, lstFileMaster, allDataDependancyEntries, ref entityList);
            }
        }

        [HttpGet]
        public IHttpActionResult ExtractEntityDependency(int projectId, string filePath)
        {
            var processedFileLines = new List<string>();
            var fileLines = File.ReadAllLines(filePath).ToList();
            fileLines = fileLines.Select(s => s.Trim()).ToList();
            var mergedLines = MergeAllLines(fileLines);
            processedFileLines.AddRange(mergedLines);
            var lstTables = new List<string>();
            Console.WriteLine("======================================================\n");
            foreach (var pLine in processedFileLines)
            {
                string inputString = Regex.Replace(pLine, @"\s+", " ");
                var regex = new Regex("\"SELECT", RegexOptions.IgnoreCase);
                if (!regex.IsMatch(inputString)) continue;
                var tableRegEx = new Regex(@"(from|join)\s+(?<table>\S+)", RegexOptions.IgnoreCase);
                if (!tableRegEx.IsMatch(inputString)) continue;

                foreach (Match match in tableRegEx.Matches(inputString))
                {
                    if (match.Groups["table"] != null)
                        lstTables.Add(match.Groups["table"].Value);
                }
            }
            // lstTables.Distinct().ToList().ForEach(Console.WriteLine);
            lstTables.ForEach(Console.WriteLine);
            Console.WriteLine("======================================================\n");
            return Ok(processedFileLines);
        }

        // Extraction of Entity Dependency for VBA...
        [HttpGet]
        public async Task<IHttpActionResult> ExtractEntityDependency(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allFiles = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);
                var entityDictionary = new Dictionary<string, List<string>>();
                var regularEx = new Regex("^\'", RegexOptions.Singleline);
                int fCount = -1;
                foreach (var fileMaster in allFiles)
                {
                    fCount++;
                    var processedFileLines = new List<string>();
                    var fileLines = File.ReadAllLines(fileMaster.FilePath).ToList();
                    fileLines = fileLines.Select(s => s.Trim()).ToList();
                    fileLines = fileLines.Where(l => !l.StartsWith("'") || !regularEx.IsMatch(l)).ToList();
                    var mergedLines = MergeAllLines(fileLines);
                    processedFileLines.AddRange(mergedLines);
                    var lstTables = new List<string>();
                    Console.WriteLine("\n======================================================\n");
                    foreach (var pLine in processedFileLines)
                    {
                        string inputString = Regex.Replace(pLine, @"\s+", " ");
                        var regex = new Regex("\"SELECT\\s", RegexOptions.IgnoreCase);
                        if (!regex.IsMatch(inputString)) continue;
                        var tableRegEx = new Regex(@"(from|join)\s+(?<table>\S+)", RegexOptions.IgnoreCase);
                        if (!tableRegEx.IsMatch(inputString)) continue;
                        foreach (Match match in tableRegEx.Matches(inputString))
                        {
                            if (match.Groups["table"] == null) continue;
                            string tble = match.Groups["table"].Value.Split(new[] { '\'', '"', '(', ')' }, StringSplitOptions.None).First();
                            string tblName = Regex.Replace(tble, @"[^0-9a-zA-Z]+", "");
                            lstTables.Add(tblName);
                        }
                    }
                    for (int lnCnt = 0; lnCnt < processedFileLines.Count; lnCnt++)
                    {
                        string line = processedFileLines[lnCnt];
                        if (!line.StartsWith("Begin InputTables")) continue;
                        for (int kIndex = lnCnt; kIndex < processedFileLines.Count; kIndex++)
                        {
                            string tName = processedFileLines[kIndex];
                            if (tName == "End") break;
                            var regEx = new Regex("\"(.*)\"");
                            if (!regEx.IsMatch(tName)) continue;
                            var match = regEx.Match(tName).Value;
                            lstTables.Add(match.Replace("\"", ""));
                        }
                        break;
                    }
                    foreach (var pLine in processedFileLines)
                    {
                        string line = pLine;
                        if (!line.StartsWith("RecordSource")) continue;
                        var regEx = new Regex("\"(.*)\"");
                        if (!regEx.IsMatch(line)) continue;
                        var match = regEx.Match(line).Value;
                        if (match.Count(char.IsWhiteSpace) <= 0)
                            lstTables.Add(match.Replace("\"", ""));
                        break;
                    }

                    var distinctList = lstTables.Distinct().ToList();
                    entityDictionary.Add(fileMaster.FilePath, distinctList);
                    distinctList.ForEach(Console.WriteLine);
                    Console.WriteLine("\n======================================================\n");
                    var addedEntities = new List<string>();
                    foreach (var tbl in distinctList)
                    {
                        if (string.IsNullOrEmpty(tbl)) continue;
                        if (string.IsNullOrEmpty(tbl)) continue;
                        string newEntity = tbl.Split(new[] { "015" }, StringSplitOptions.None).First();
                        if (addedEntities.Any(e => e == newEntity)) continue;
                        addedEntities.Add(newEntity);
                        var dataDependency = new DataDependency
                        {
                            FileMaster = null,
                            Attributes = null,
                            ProjectId = projectId,
                            FileId = fileMaster.FileId,
                            Entity = newEntity,
                            EntityOld = newEntity,
                            FileIdOld = fileMaster.FileId,
                            DataDepedencyId = 0
                        };
                        await _codeVortoService.DataDependencyRepository.AddNewItem(dataDependency)
                            .ConfigureAwait(false);
                    }

                    Console.WriteLine("File Processed: " + fCount);
                }
                return Ok(entityDictionary);
            }
        }

        private static IEnumerable<string> MergeAllLines(IReadOnlyList<string> allLines)
        {
            var mergedLine = new List<string>();
            for (int lnIndex = 0; lnIndex < allLines.Count; lnIndex++)
            {
                string line = allLines[lnIndex];
                if (!line.StartsWith("\"")) { mergedLine.Add(line); continue; }
                string previousLine = allLines[lnIndex - 1].TrimEnd('"');
                int lastIndex = mergedLine.Count - 1;
                for (; lnIndex < allLines.Count; lnIndex++)
                {
                    if (allLines[lnIndex].StartsWith("\""))
                        previousLine += allLines[lnIndex].TrimStart('"').TrimEnd('"');
                    else
                    {
                        mergedLine[lastIndex] = previousLine + "\"";
                        lnIndex--;
                        break;
                    }
                }
            }
            return mergedLine;
        }

        [HttpGet]
        public IHttpActionResult Change(string rootPath, string extension, string dToSkip, string directoryName)
        {
            var dirNames = directoryName.Split(',').ToArray();
            return ChangeExtensions(rootPath, extension, dToSkip, dirNames);
        }

        [HttpGet]
        public IHttpActionResult ChangeExtensions(string rootPath, string extension, string dToSkip,
          params string[] directoryName)
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

            int totalFiles = 0;
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
                        totalFiles++;
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
                    totalFiles++;
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
                totalFiles++;
                var newFile = File.ReadAllLines(file);
                var newFileWithExtension = file + extension;
                if (File.Exists(newFileWithExtension)) File.Delete(newFileWithExtension);
                File.WriteAllLines(newFileWithExtension, newFile);
                File.Delete(file);
            }

            return Ok("File extensions has been changed. Total Files: " + totalFiles);
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateExtensionForSubroutine()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string strQry = "select * from filemaster where ProjectId = 2 and FileTypeExtensionId=17;";

                var fileMasters =
                    await
                        _codeVortoService.FileMasterRepository.GetDataFromSqlQuery<FileMaster>(strQry).ConfigureAwait(false);
                foreach (var file in fileMasters)
                {
                    var fileName = file.FileName.Replace("pgm", "sbr");
                    file.FileName = fileName;
                    var filePath = file.FilePath.Replace("pgm", "sbr");
                    file.FilePath = filePath;
                    file.ProjectMaster = null;
                    file.FileTypeExtensionReference = null;
                    await _codeVortoService.FileMasterRepository.UpdateItem(file);
                }
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdatePseudoCodeForIcdFile(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    string sqlQy =
                        "select * from statementreferencemaster where fileId >= 327 and fileId <= 856 and basecommandId = 1;";
                    var statementReferenceMasterData = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQy).ConfigureAwait(false);
                    string sqlSubRoutine =
                        "select * from secondtabsubroutineprogramdetails where ProgramId >= 327 and ProgramId <= 856 and basecommandId = 1  and (Alternatename is  null or Alternatename = '');";
                    var secondTabSubroutine =
                        await
                            _codeVortoService.SecondTabSubRoutineProgramRepository
                                .GetDataFromSqlQuery<SecondTabSubRoutineProgramDetails>(sqlSubRoutine)
                                .ConfigureAwait(false);
                    int index = -1;
                    foreach (var secondTab in secondTabSubroutine)
                    {
                        index++;
                        Console.WriteLine(index + "  " + secondTab.ActualStatementId);
                        var actualStatement = secondTab.ActualStatementId.Split('_')[1];
                        int statementId = Convert.ToInt32(actualStatement);
                        var statementRef =
                        (from s in statementReferenceMasterData
                         where s.StatementId == statementId && s.FileId == secondTab.ProgramId
                         select s).ToList();
                        foreach (var statement in statementRef)
                        {
                            var alternateName = statement.AlternateName;
                            secondTab.AlternateName = alternateName;
                            await _codeVortoService.SecondTabSubRoutineProgramRepository.UpdateItem(secondTab);
                        }

                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
            return Ok("Done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateFileIdInActionsWorkflowTable(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allActionsWorkflows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);
                var fileMasters = await _codeVortoService.FileMasterRepository
                        .GetAllListItems(f => f.ProjectId == projectId).ConfigureAwait(false);
                int workflowCount = 0;
                foreach (var actionWorkflow in allActionsWorkflows)
                {
                    string filePath = actionWorkflow.OriginFilePath;
                    var fileMaster = (from f in fileMasters where f.FilePath == filePath select f).ToList().First();
                    if (fileMaster == null) continue;
                    workflowCount++;
                    actionWorkflow.FileId = fileMaster.FileId;
                    actionWorkflow.FileMaster = null;
                    actionWorkflow.ProjectMaster = null;

                    await _codeVortoService.ActionWorkflowsRepository.UpdateItem(actionWorkflow).ConfigureAwait(false);
                }

                return Ok("Action Workflows Updated: " + workflowCount);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateDataInputOutputStatement(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                string sqlOpenRecordset = " SELECT * FROM StatementReferenceMaster" +
                                          " WHERE  ProjectId = " + projectId + " AND " +
                                          "(OriginalStatement  LIKE '%.OpenRecordset%'  OR" +
                                          " OriginalStatement like '%.Execute%'  OR " +
                                          " OriginalStatement like '%.dbs.Close%' OR" +
                                          " OriginalStatement like '% DLookup%') ; ";
                var allRecordSetStatements = await _codeVortoService.StatementReferenceMasterRepository
                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlOpenRecordset).ConfigureAwait(false);
                foreach (var statementRef in allRecordSetStatements)
                {
                    string oStatement = statementRef.OriginalStatement.ToLower();
                    string cCalled = oStatement.Contains("openrecordset")
                        ? "Database"
                        : oStatement.Contains("dlookup") ? "UnKnown" : "Database";
                    string mCalled = oStatement.Contains("openrecordset")
                        ? "OpenRecordset ( )"
                        : oStatement.Contains("dlookup")
                            ? "DLookup ( )"
                            : oStatement.Contains(".execute") ? "Execute ( )" : "Close ( )";

                    statementRef.BaseCommandId = 6;
                    statementRef.ClassCalled = cCalled;
                    statementRef.MethodCalled = mCalled;

                    await _codeVortoService.StatementReferenceMasterRepository
                        .UpdateItem(statementRef).ConfigureAwait(false);

                    var dataObjectType = statementRef.OriginalStatement.Replace("Set ", "");
                    string variableName = dataObjectType.Split(new[] { "=" }, StringSplitOptions.None)[0];

                    variableName = variableName.Trim().Replace("'", "");
                    string sqlDataIoStats =
                        " SELECT * FROM StatementReferenceMaster WHERE FileId = " + statementRef.FileId +
                        " AND (OriginalStatement LIKE '%" + variableName + ".%' OR OriginalStatement LIKE '%" +
                        variableName + "!%'); ";
                    var allDataIoStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataIoStats).ConfigureAwait(false);

                    foreach (var dataIoState in allDataIoStatements)
                    {
                        if (dataIoState.BaseCommandId != 0) continue;

                        dataIoState.VariableNameDeclared = variableName;
                        dataIoState.BaseCommandId = 45;
                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(dataIoState).ConfigureAwait(false);
                    }
                    string sqlDataOtherStatemt =
                        " SELECT * FROM StatementReferenceMaster WHERE  OriginalStatement LIKE '%.orderby%' " +
                        " OR OriginalStatement LIKE '%.groupby%' " +
                        " OR OriginalStatement LIKE '%.filter%'" +
                        " OR OriginalStatement LIKE '%.AllowAdditions%'" +
                        " OR OriginalStatement LIKE '%.AllowFilters%' " +
                        " AND ProjectId = " + projectId + "; ";
                    var allDataOtherStatements = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlDataOtherStatemt).ConfigureAwait(false);
                    foreach (var dataOtherStatemt in allDataOtherStatements)
                    {
                        if (dataOtherStatemt.BaseCommandId != 0) continue;

                        dataOtherStatemt.VariableNameDeclared = variableName;
                        dataOtherStatemt.BaseCommandId = 45;
                        await _codeVortoService.StatementReferenceMasterRepository
                            .UpdateItem(dataOtherStatemt).ConfigureAwait(false);
                    }
                }
            }
            return Ok("done");
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateToBaseCommandId45(int solutionId, int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var sqlQuery = "Select FileName, RowId, FieldNo, Description, FieldLabel, RptFieldLength, TypeofData, " +
                               " SingleArray, DateOfCapture, ReplacementName FROM UniverseBasicDataDictionary  WHERE ProjectId =" + projectId + "  GROUP BY  FileName;";
                var universeBasicDataDicyionary =
                    await _codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);
                foreach (var dataDictionary in universeBasicDataDicyionary)
                {
                    string tableName = dataDictionary.Description;
                    string fileName = dataDictionary.FileName;
                    if (string.IsNullOrEmpty(fileName)) continue;
                    string sqlQry = " SELECT * FROM StatementReferenceMaster WHERE SolutionId = " + solutionId +
                                    " AND OriginalStatement LIKE '%" + fileName + "%';";
                    var statementRefMaster = await _codeVortoService.StatementReferenceMasterRepository
                           .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQry).ConfigureAwait(false);
                    foreach (var statementRef in statementRefMaster)
                    {
                        var newStatement = Regex.Replace(!string.IsNullOrEmpty(statementRef.AlternateName)
                            ? statementRef.AlternateName :
                            statementRef.OriginalStatement, fileName, tableName, RegexOptions.IgnoreCase);
                        newStatement = newStatement.Length > 1451 ? newStatement.Substring(0, 1450) : newStatement;

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

                return Ok("Done");
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> InsertSystemLevelCrudActivityForUniverse(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
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
                var regexClear = new Regex(@"(^EXECUTE\s+""CLEAR.FILE)|(^EXECUTE\s+'CLEAR.FILE)|(^CLEAR.FILE\s+)|(^EXECUTE\s+CLEAR.FILE)");
                var regexEd = new Regex(@"^ED\s+");
                var sqlQry = " SELECT * FROM FileMaster WHERE ProjectId = " + projectId + " AND FileTypeExtensionId = 11;";
                var fileMaster = await _codeVortoService.FileMasterRepository
                    .GetDataFromSqlQuery<FileMaster>(sqlQry).ConfigureAwait(false);
                foreach (var fMaster in fileMaster)
                {
                    var entityName = Path.GetFileNameWithoutExtension(fMaster.FileName);
                    var sqlStatementQry =
                        "SELECT * FROM statementreferencemaster where OriginalStatement like '%" + entityName + "%';";
                    var statementRefenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlStatementQry).ConfigureAwait(false);
                    if (!statementRefenceMaster.Any()) continue;
                    var fileIds = (from s in statementRefenceMaster select s.FileId).ToList();
                    var distinctFileIds = fileIds.Distinct().ToList();
                    foreach (var fId in distinctFileIds)
                    {
                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "N";
                        var currentStatememts = statementRefenceMaster.Where(x => x.FileId == fId).ToList();
                        if (!currentStatememts.Any()) continue;
                        foreach (string sss in currentStatememts.Select(s => s.OriginalStatement))
                        {
                            if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                            if (updateList.Any(c => sss.Contains(c))) update = "Y";
                            if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                            if (selectList.Any(c => sss.Contains(c))) select = "Y";
                            if (regex.IsMatch(sss)) select = "Y";
                            if (regexClear.IsMatch(sss)) delete = "Y";
                            if (regexEd.IsMatch(sss)) update = "Y";
                            //string[] strExcute = {"EXECUTE"};
                            //if (sss.StartsWith("EXECUTE"))
                            //{
                            //    string[] strStatement = sss.Split(strExcute, StringSplitOptions.None);
                            //    var nStatement = strStatement[1].TrimStart().Replace("'", "").Replace("\"", "");
                            //    if (regexClear.IsMatch(nStatement)) delete = "Y";
                            //}

                        }
                        if (iR == "N" && update == "N" && delete == "N" && select == "N") continue;
                        var dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = entityName,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = fId,
                            ProjectId = projectId,
                            FileMaster = null
                        };
                        string entity = entityName;
                        int fileIdNew = fId;
                        Expression<Func<DbCrudActivity, bool>> expression =
                            x => x.EntityName == entity && x.FileId == fileIdNew;
                        var crud = await _codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                            .ConfigureAwait(false);
                        if (crud.Any()) continue;
                        await _codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);
                    }
                }
            }
            return Ok("done");
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

                stringBuilder.AppendLine("================================================================");
                stringBuilder.AppendLine("\n" + "Started process for project: " + projectId + " project name: " +
                                         projectDetails.ProjectName + " and project physical path is: " +
                                         projectDetails.PhysicalPath);

                var extensionList = await _codeVortoService.FileTypeExtensionRepository
                    .GetAllItems(p => p.LanguageId == projectDetails.LanguageId).ConfigureAwait(true);
                var strExtensions = new List<string>();
                var fileTypeExtensionReferences = extensionList as IList<FileTypeExtensionReference> ??
                                                  extensionList.ToList();
                strExtensions.AddRange(fileTypeExtensionReferences.Select(extension => extension.FileExtension));
                stringBuilder.AppendLine("==================================================================");
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
                        var allFiles = Directory.GetFiles(directory, "*.TXT", SearchOption.AllDirectories).ToList();
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
                            var extension = Path.GetExtension(currentFile).ToLower();
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
                        stringBuilder.AppendLine("============================================================");
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
                            Content = new StringContent("Files extracted for Common Library type project for Universe Basic is completed.")
                        };
                }

                return new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent("Done") };
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> MissingEntitiesAndObjectsReport(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var missingEnitiesList = new List<MissingObjects>();
                var fileMasters = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(x => (x.FileTypeExtensionId == 9 || x.FileTypeExtensionId == 17 ||
                                           x.FileTypeExtensionId == 12) && x.ProjectId == projectId).ConfigureAwait(false);
                var entitiesData = await _codeVortoService.FileMasterRepository.GetAllListItems(
                            x => x.FileTypeExtensionId == 11 && x.ProjectId == projectId);

                var keywordList = new List<string>{
                    "MATWRITE ","MATWRITEL ","MATWRITEU ","WRITEVU ","WRITE ","WRITEU ","WRITEV ","WRITET ","WRITEBLK ",
                    "WRITESEQF ", "WRITESEQ ","DELETE ","DELETELIST ", "DELETEU " ,"MATREAD ","MATREADL ","MATREADU ",
                    "OPENCHECK ","OPENDEV ","OPENPATH ","OPENSEQ ","OPEN ","LOCATE ","READ ", "READL ","READU ","READV ",
                    "REAFV ","READSEQ ","READVL ","READVU ","READBLK ","READLIST ","READNEXT ","READT ","SSELECT ","SELECT"
                };
                /*
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

                 keywordList.AddRange(insertList);
                 keywordList.AddRange(updateList);
                 keywordList.AddRange(deleteList);
                 keywordList.AddRange(selectList);
                 */
                var regexOn = new Regex(@"ON\s+([\w\d\.]+)");

                foreach (var fileMaster in fileMasters)
                {
                    var entityMissingFileWise = new List<MissingObjects>();
                    var onContainsStatements = new List<string>();
                    var currentFileId = fileMaster.FileId;
                    var viewSource = await _codeVortoService.ViewSourceMasterRepository
                        .GetItem<ViewSourceMaster>(x => x.FileId == currentFileId && x.ProjectId == projectId)
                        .ConfigureAwait(false);
                    if (viewSource == null) return NotFound();
                    string[] sourceData = viewSource.SourceData.Split('\n');

                    foreach (var line in sourceData)
                    {
                        var currentLine = line.Trim();
                        var check = keywordList.Any(x => currentLine.StartsWith(x));
                        if (!check) continue;
                        if (!currentLine.Contains(" ON ")) continue;
                        onContainsStatements.Add(currentLine);
                    }
                    if (!onContainsStatements.Any()) continue;
                    foreach (var statement in onContainsStatements)
                    {
                        if (!regexOn.IsMatch(statement)) continue;
                        var onContent = regexOn.Match(statement).Groups[1].Value;
                        if (string.IsNullOrEmpty(onContent)) continue;
                        var regexPattern = new Regex(@"R{1,}[.]([\w\d\.]+)|K{1,}[.]([\w\d\.]+)");
                        if (!regexPattern.IsMatch(statement)) continue;
                        var matches = regexPattern.Matches(statement);
                        foreach (var match in matches)
                        {
                            var matchWord = match.ToString().Trim();
                            var rValue = new Regex(@"R.([\w\d\.]+)");
                            var kValue = new Regex(@"K.([\w\d\.]+)");
                            if (!rValue.IsMatch(matchWord) && !kValue.IsMatch(matchWord)) continue;
                            string finalValue = string.Empty;
                            if (rValue.IsMatch(matchWord))
                            {
                                var rVal = rValue.Match(matchWord).Groups[1].Value;
                                if (!string.IsNullOrEmpty(rVal))
                                    finalValue = rVal;
                            }
                            else if (kValue.IsMatch(matchWord))
                            {
                                var kVal = kValue.Match(matchWord).Groups[1].Value;
                                if (!string.IsNullOrEmpty(kVal))
                                    finalValue = kVal;
                            }

                            entitiesData.RemoveAll(e => e == null);
                            var verifyEntityCheck = (from e in entitiesData
                                                     let f = Path.GetFileName(e.FileName)
                                                     where finalValue.StartsWith(f)
                                                     select e).ToList();

                            if (verifyEntityCheck.Any()) continue;
                            var existEntity = entityMissingFileWise
                                .Where(x => x.FileId == currentFileId && x.CalledObjectName == onContent).ToList();
                            if (existEntity.Any()) continue;

                            entityMissingFileWise.Add(new MissingObjects
                            {
                                FileId = currentFileId,
                                ProjectId = projectId,
                                CalledObjectName = onContent,
                                Type = "Entity",
                                Statement = statement,
                                FromObject = fileMaster.FileName,
                                FileMaster = null,
                                MissingObjectId = 0
                            });
                        }
                    }
                    missingEnitiesList.AddRange(entityMissingFileWise);
                }
                await _codeVortoService.MissingObjectsRepository.BulkInsert(missingEnitiesList).ConfigureAwait(false);

                missingEnitiesList = new List<MissingObjects>();
                var statementReferenceMaster = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(x => x.BaseCommandId == 6 && x.ReferenceFileId == 0 && x.ProjectId == projectId)
                    .ConfigureAwait(false);
                var regEx = new Regex(@"^EXECUTE\s|^PH\s|^PHANTOM\s"); //|^RUN\s

                foreach (var statementReference in statementReferenceMaster)
                {
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

                return Ok(missingEnitiesList);
            }
        }

        [HttpGet]
        public List<string> ConversionOfExecStatements()
        {
            var allLines =
                File.ReadAllLines(@"C:\inetpub\wwwroot\flokapture\ExtractedProjects\COBOL_Demo\COBOL\dtfm164.cbl")
                    .ToList();
            var mainBlock = new List<string>();
            if (allLines.Count <= 0) return mainBlock;
            var regex = @"^EXEC CICS RETURN|^EXEC CICS XCTL";
            foreach (string currentLine in allLines)
            {

                if (currentLine.StartsWith("EXEC CICS HANDLE AID"))
                {
                    string[] toStr = { "AID" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1];
                    var finalStatement = moveState.Split(' ');
                    foreach (var statement in finalStatement)
                    {
                        if (statement == "END-EXEC." || statement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(statement)) continue;
                        string[] state = statement.Split('(');
                        var mainLine = "IF " + state[0] + " IS PRESSED";
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall);
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }

                if (currentLine.StartsWith("EXEC CICS HANDLE CONDITION"))
                {
                    string[] toStr = { "CONDITION" };
                    string[] moveStatement = currentLine.Split(toStr, StringSplitOptions.None);
                    var moveState = moveStatement[1].Trim();
                    var finalStatement = moveState.Split(')');
                    int finalStatCount = finalStatement.Length - 1;
                    int index = 0;
                    foreach (var statement in finalStatement)
                    {
                        var newStatement = statement.Trim();
                        index++;
                        if (newStatement == "END-EXEC." || newStatement == "END-EXEC") continue;
                        if (string.IsNullOrWhiteSpace(newStatement)) continue;
                        var ifState = index == 1 ? "IF " : "ELSE-IF ";
                        if (finalStatCount > 1)
                        {
                            if (index == finalStatCount)
                            {
                                ifState = "ELSE ";
                            }
                        }
                        string[] state = newStatement.Split('(');
                        var mainLine = ifState + "CONDITION IS " + state[0];
                        mainBlock.Add(mainLine);
                        var internalcall = "PERFORM " + state[1].Replace("(", "").Replace(")", "");
                        mainBlock.Add(internalcall.Trim());
                        mainBlock.Add("END-IF");
                    }
                    continue;
                }

                if (Regex.IsMatch(currentLine, regex))
                {
                    var regexPattern = "@EXEC CICS\\s+[A-z0-9\\-]\\S+(\\s+[A-z0-9\\-]\\S+)";
                    var matches = Regex.Match(currentLine, regexPattern, RegexOptions.IgnoreCase);
                    var statementMatch = matches.Groups[1].Value;
                    Console.WriteLine(statementMatch);

                }
                mainBlock.Add(currentLine);

            }
            return mainBlock;
        }

        [HttpGet]
        public IHttpActionResult ModifyCobolFolder()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                try
                {
                    var path = @"D:\Flokapture-Cobol-Latest-19-feb-2020\COBOLSamples";

                    var allRootFolder = Directory.GetDirectories(path);
                  
                    foreach (var rootFolder in allRootFolder)
                    {
                        var subDirectories = Directory.GetDirectories(rootFolder);
                        foreach (var subDir in subDirectories)
                        {
                            Console.WriteLine(subDirectories);
                            var dirInfo = new DirectoryInfo(rootFolder);
                            dirInfo.DeleteEmptyDirs();

                            foreach (var sourceSubDirPath in Directory.EnumerateDirectories(subDir, "*",
                                SearchOption.AllDirectories))
                                Directory.CreateDirectory(sourceSubDirPath.Replace(sourceSubDirPath, rootFolder));

                            var rootDir = new DirectoryInfo(subDir);
                            rootDir.MoveTo(Path.Combine(rootDir.Parent.FullName, "test"));
                            var getChildDir = Directory.GetDirectories(Path.Combine(rootDir.Parent.FullName, "test"));
                            foreach (var childDir in getChildDir)
                            {
                                var dir = new DirectoryInfo(childDir);
                                string newPath = Path.Combine(rootFolder, dir.Name);
                                Directory.Move(childDir, newPath);
                            }
                            Directory.Delete(Path.Combine(rootDir.Parent.FullName, "test"));
                        }
                       
                        //var allDirectories = Directory.GetDirectories(rootFolder).ToList();
                        //foreach (var directory in allDirectories)
                        //{
                        //   var isDirectory =  IsDirectoryEmpty(directory);
                        //    if (!isDirectory)
                        //        await GetSubDirectories(directory, false);
                        //    else
                        //       Directory.Delete(directory);

                        //}

                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    return InternalServerError(exception);
                }
            }
            return Ok("Done");
        }

        protected virtual async Task<IHttpActionResult> GetSubDirectories(string path, bool subDirectory)
        {
            var rootDirectories = Directory.GetDirectories(path);
            var allRootFiles = Directory.GetFiles(path);
            if (!rootDirectories.Any() && !allRootFiles.Any() && subDirectory) Directory.Delete(path);
            foreach (var directory in rootDirectories)
            {
                var allFolder = Directory.GetDirectories(directory);
                var allFiles = Directory.GetFiles(directory);
                if (!allFolder.Any() && !allFiles.Any())
                {
                    Directory.Delete(directory);
                    continue;
                }
                foreach (var folder in allFolder)
                {
                    await GetSubDirectories(folder, true);
                }

            }
            return Ok("Done");
        }
        public bool IsDirectoryEmpty(string path)
        {
            return !Directory.EnumerateFileSystemEntries(path).Any();
        }

        [HttpGet]
        public async Task<IHttpActionResult> UploadFileMenuDataRevised(int projectId )
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
                var projectMaster = projectMasterRepository.GetItem(projectId);
                string directoryPath = projectMaster.PhysicalPath;
                directoryPath = Path.Combine(directoryPath, "Menu");

                var allFiles = Directory.GetFiles(directoryPath, "*.csv", SearchOption.TopDirectoryOnly);
                var listItems = new List<UniverseFileMenu>();
                using (var appDbContext = new AppDbContext())
                {
                    foreach (var file in allFiles)
                    {
                        var methodBlockList = File.ReadAllLines(file, Encoding.UTF7).ToList();

                        methodBlockList = methodBlockList.Skip(1).ToList();
                        methodBlockList.RemoveAll(l => l.Length <= 0);
                        var stream = new StreamReader(file);
                        var csvReader = new CsvReader(stream, true);
                        while (csvReader.ReadNextRecord())
                        {
                            listItems.Add(new UniverseFileMenu
                            {
                                ProjectId = projectMaster.ProjectId,
                                UserId = 1,
                                ActionExecuted = csvReader[3],
                                MenuId = csvReader[0],
                                MenuDescription = csvReader[2] ?? "",
                                MenuTitle = csvReader[1],
                                UploadedOn = DateTime.Now
                            });
                        }
                        // appDbContext.UniverseFileMenu.AddRange(listItems);
                        // await appDbContext.SaveChangesAsync().ConfigureAwait(false);
                    }

                    Console.WriteLine("======================================================================================\n");
                    Console.WriteLine("File Menu Data uploaded successfully\n");
                    Console.WriteLine("======================================================================================\n");

                    return Ok("File Menu Data uploaded successfully");
                }
            }
           
        }


        /*
        public async Task<IHttpActionResult> CombineAllEXEXLine()
        {
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    var allLines = new List<string>();
                    var fileLines = File.ReadAllLines(@"D:\Auctor\CodeVorto\COBOLSamplesTesting\ACA\COBOL\COBOL\ACAC100.cbl").ToList();
                    int indexCnt = -1;
                    foreach (var line in fileLines)
                    {
                        indexCnt++;
                        var nLine = line;
                        if (!nLine.StartsWith("EXEC CICS"))
                        {
                            allLines.Add(nLine);
                            continue;
                        }
                        var mainLine
                        for (int i = indexCnt; i < fileLines.Count; i++)
                        {

                        }
                    }
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.InnerException);
            }
            return Ok("done");
        }
         */
    }
}

/*
 #region Update field for basecommandId = 45
                stringBuilder.AppendLine(
 "========================================================================================");
                stringBuilder.AppendLine("Started update field for basecommandId = 45 for solution: " + solutionId + "");

                var sqlQuery = "SELECT * FROM DataDependency WHERE projectId != 1;";
                var dataDependancy = await _codeVortoService.DataDependencyRepository
                    .GetDataFromSqlQuery<DataDependency>(sqlQuery).ConfigureAwait(false);
                foreach (var dependancy in dataDependancy)
                {
                    string tableName = dependancy.Entity;
                    string fileName = Path.GetFileNameWithoutExtension(dependancy.FileMaster.FilePath);
                    string sqlQry = "SELECT * from statementreferencemaster WHERE projectId=" + projectId +
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
                stringBuilder.AppendLine("Ended update field for basecommandId = 45 for solution: " + solutionId + "");

                #endregion
 */
