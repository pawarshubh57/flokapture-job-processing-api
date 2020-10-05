using System.Collections.Generic;
using System.IO;
using System.Linq;
using BusinessLayer.DbEntities;
using BusinessLayer.ExtensionLibrary;
using BusinessLayer.Models;

namespace BusinessLayer.VisualBasicVba
{
    public class VisualBasicVba1
    {
        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator, int pMethodStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);

            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = null,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = fileName, //Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                    PrimaryCommandId = pClassStartIndicator,
                    BaseCommandId = 19,
                    ProjectId = fileMaster.ProjectId
                },
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = fileName,
                    OriginalStatement = fileName,
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pMethodStartIndicator,
                    BaseCommandId = 8,
                    ProjectId = fileMaster.ProjectId
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterEnd(FileMaster fileMaster,
            int pClassEndIndicator, int pMethodEndIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = "End",
                    OriginalStatement = "End",
                    ClassCalled = null,
                    MethodName = fileName,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pMethodEndIndicator,
                    BaseCommandId = 9,
                    ProjectId = fileMaster.ProjectId
                },
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = "End",
                    OriginalStatement = "End",
                    ClassCalled = null,
                    MethodName = null,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pClassEndIndicator,
                    BaseCommandId = 20,
                    ProjectId = fileMaster.ProjectId
                }
            };
            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterStart(FileMaster fileMaster,
            int pClassStartIndicator)
        {
            var fileName = Path.GetFileNameWithoutExtension(fileMaster.FilePath);
            List<StatementReferenceMaster> lstStatementReferenceMaster;
            if (fileMaster.FileTypeExtensionId == 14)
            {
                lstStatementReferenceMaster = new List<StatementReferenceMaster>
                {
                    new StatementReferenceMaster
                    {
                        FileId = fileMaster.FileId,
                        ResolvedStatement = fileName,
                        OriginalStatement = fileName,
                        ClassCalled = null,
                        MethodName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        ClassNameDeclared = fileName,
                        PrimaryCommandId = pClassStartIndicator,
                        BaseCommandId = 19,
                        ProjectId = fileMaster.ProjectId
                    },
                    new StatementReferenceMaster
                    {
                        FileId = fileMaster.FileId,
                        ResolvedStatement = "Form_Source()",
                        OriginalStatement = "Form_Source()",
                        ClassCalled = null,
                        MethodName = "Form_Source()",
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        ClassNameDeclared = fileName,
                        PrimaryCommandId = pClassStartIndicator,
                        BaseCommandId = 8,
                        ProjectId = fileMaster.ProjectId
                    }
                };
            }
            else
            {
                lstStatementReferenceMaster = new List<StatementReferenceMaster>
                {
                    new StatementReferenceMaster
                    {
                        FileId = fileMaster.FileId,
                        ResolvedStatement = fileName,
                        OriginalStatement = fileName,
                        ClassCalled = null,
                        MethodName = null,
                        DataOrObjectType = null,
                        MethodCalled = null,
                        VariableNameDeclared = null,
                        ClassNameDeclared = fileName,
                        PrimaryCommandId = pClassStartIndicator,
                        BaseCommandId = 19,
                        ProjectId = fileMaster.ProjectId
                    }
                };
            }

            return lstStatementReferenceMaster;
        }

        public List<StatementReferenceMaster> PrepareStatementReferenceMasterEnd(FileMaster fileMaster,
            int pClassEndIndicator)
        {
            var lstStatementReferenceMaster = new List<StatementReferenceMaster>
            {
                new StatementReferenceMaster
                {
                    FileId = fileMaster.FileId,
                    ResolvedStatement = "End",
                    OriginalStatement = "End",
                    ClassCalled = null,
                    MethodName = null,
                    DataOrObjectType = null,
                    MethodCalled = null,
                    VariableNameDeclared = null,
                    ClassNameDeclared = null,
                    PrimaryCommandId = pClassEndIndicator,
                    BaseCommandId = 20,
                    ProjectId = fileMaster.ProjectId
                }
            };
            return lstStatementReferenceMaster;
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
                    caption = caption.Replace("\"", "").Replace("&", "").Trim();
                    if (fileLines.Count > i + 1 && fileLines[i + 1].StartsWith("\""))
                    {
                        caption += fileLines[i + 1].Trim().Replace("\"", "").Replace("&", "");
                    }
                    break;
                }
                if (currentLine.StartsWith("Begin")) break;
            }
            return caption;
        }

        public string FindCaption(string input, List<string> allLines)
        {
            string caption = string.Empty;
            if (!allLines.Any(l => l.Contains(input))) return caption;

            int index = allLines.FindIndex(s => s.Contains(input));
            for (int i = index; i < allLines.Count; i++)
            {
                string sss = allLines[i];
                if (sss.StartsWith("Caption"))
                {
                    caption = sss.Substring(sss.IndexOf('=') + 1);
                    caption = caption.Replace("\"", "").Replace("&", "");
                    break;
                }
                if (sss == "End" || sss.StartsWith("Begin")) break;
            }
            // return string.IsNullOrEmpty(caption) ? input : caption;
            return caption;
        }

        public List<string> GetSubForms(List<string> fileLines)
        {
            var subFormCounts = fileLines.FindSubFormIndexes(c => c == "Begin Subform");
            var subFormNames = new List<string>();
            if (!subFormCounts.Any()) return subFormNames;
            foreach (var index in subFormCounts)
            {
                for (int cnt = index + 1; cnt < fileLines.Count; cnt++)
                {
                    if (fileLines[cnt].StartsWith("Begin") || fileLines[cnt] == "End") break;
                    if (!fileLines[cnt].StartsWith("SourceObject")) continue;
                    var lastOrDefault = fileLines[cnt].Split('=').LastOrDefault();
                    if (lastOrDefault != null)
                        subFormNames.Add(lastOrDefault.Replace("\"", ""));
                }
            }
            return subFormNames;
        }

        public static Node GetNodeDetails(int nodeId, string shapeId, string color, string name, TreeView curItem)
        {
            var charCountOfText = name.Length + name.Count(s => s == '\n');
            var width = CalculateWidth(charCountOfText);
            var height = CalculateHeight(charCountOfText);
            name = name.Replace("Then", "").Replace("THEN", "");
            var node = new Node
            {
                Id = nodeId,
                ShapeId = shapeId,
                Name = name,
                Color = color,
                Width = width,
                Height = height,
                StatementId = int.Parse(curItem.ActualStatementId.Split('_')[1]),
                GroupName = curItem.GroupName,
                GroupId = curItem.GroupId,
                ProgramId = curItem.ProgramId,
                BaseCommandId = curItem.BaseCommandId
            };
            return node;
        }

        public static Link GetLinkDetails(int origin, int target, string linkSeqNumber, TreeView curItem)
        {
            var link = new Link
            {
                Origin = origin,//treeView.Nodes.First().Id,
                Target = target,
                LinkText = linkSeqNumber,
                ProgramId = curItem.ProgramId,
                StatementId = curItem.StatementReferenceMaster.StatementId
            };
            return link;
        }

        public static string CalculateWidth(int textLength)
        {
            int widthCalculation = (textLength * 3 + 10);
            var width = widthCalculation.ToString();
            return width;
        }

        public static string CalculateHeight(int charCountOfText)
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