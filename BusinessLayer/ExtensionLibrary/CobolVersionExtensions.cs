using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace BusinessLayer.ExtensionLibrary
{
    public static class CobolVersionExtensions
    {
        /// <summary>
        /// Remove start end character from string
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="startCount"></param>
        /// <param name="endCount"></param>
        /// <returns></returns>

        public static List<string> RemoveStartEndCharacters(this List<string> allLines, int startCount, int endCount)
        {
            var mainBlock = new List<string>();
            foreach (var line in allLines)
            {
                if (string.IsNullOrWhiteSpace(line)) continue;
                var startLine = line.Substring(startCount);
                if (string.IsNullOrWhiteSpace(startLine)) continue;
                if (startLine.Length <= endCount || endCount == 0)
                {
                    mainBlock.Add(startLine);
                    continue;
                }
                string endLine = startLine.Substring(0, endCount);
                if (string.IsNullOrEmpty(endLine)) continue;
                mainBlock.Add(endLine);
            }
            return mainBlock;
        }

        /// <summary>
        /// Remove all commented lines
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="comments"></param>
        /// <returns></returns>
        public static List<string> RemoveCommentedLines(this List<string> allLines, string[] comments)
        {
            var mainBlock = new List<string>();
            foreach (var line in allLines)
            {
                var isPresent = comments.Any(x => line.StartsWith(x));
                if (isPresent) continue;
                mainBlock.Add(line);
            }
            return mainBlock;
        }

        public static List<string> RemoveStartCharacters(this List<string> allLines, int count)
        {
            var mainBlock = new List<string>();
            foreach (var line in allLines)
            {
                if (String.IsNullOrEmpty(line)) continue;
                var newLine = line.Substring(count);
                mainBlock.Add(newLine);
            }
            return mainBlock;
        }

        public static List<string> RemoveEndCharacters(this List<string> allLines, int count)
        {
            var mainList = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line;
                if (string.IsNullOrWhiteSpace(currentLine)) continue;
                if (currentLine.Length <= count)
                {
                    mainList.Add(currentLine);
                    continue;
                }
                string str = currentLine.Substring(0, count);
                mainList.Add(str);
            }
            return mainList;
        }
        /// <summary>
        /// Remove all null or empty lines form list
        /// </summary>
        /// <param name="allLines"></param>
        /// <returns></returns>
        public static List<string> RemoveAllEmptyLines(this List<string> allLines)
        {
            var mainBlockList = new List<string>();
            foreach (var line in allLines)
            {
                if (String.IsNullOrWhiteSpace(line)) continue;
                mainBlockList.Add(line);
            }
            return mainBlockList;
        }

        /// <summary>
        /// Remove all commented lines
        /// </summary>
        /// <param name="allLines"></param>
        /// <param name="comment"></param>
        /// <returns></returns>
        public static List<string> RemoveAllCommentedLines(this List<string> allLines, string comment)
        {
            var mainBlock = new List<string>();
            foreach (var line in allLines)
            {
                if (line.StartsWith(comment)) continue;
                //mainBlock.Add(line.Trim());
                mainBlock.Add(line);
            }
            return mainBlock;
        }

        public static List<string> RemoveSpacesBetweenWords(this List<string> allLines)
        {
            var regex = new Regex(@"[\s]{2,}", RegexOptions.None);
            allLines = allLines.Select(x => x.TrimStart()).ToList();
            return allLines.Select(currentLine => regex.Replace(currentLine, " ")).ToList();
        }

        public static List<string> ReplaceStatement(this List<string> allLines, string replaceWith, string replaceTo)
        {
            var mainListBlock = new List<string>();
            foreach (var line in allLines)
            {
                var currentLine = line;
                if (currentLine.StartsWith(replaceWith))
                {
                    currentLine = currentLine.Replace(replaceWith, replaceTo);
                    mainListBlock.Add(currentLine);
                    continue;
                }
                mainListBlock.Add(currentLine);
            }
            return mainListBlock;
        }

        public static bool VerifyMethodBlockForIfWithMatchingEndIfCount(this List<string> lstMethodBlock)
        {
            var ifCount = lstMethodBlock.Count(s => !string.IsNullOrEmpty(s) && s.StartsWith("IF "));
            var endIfCount = lstMethodBlock.Count(s => "END-IF" == s.Trim());
            Console.WriteLine("=============================================================================");
            Console.WriteLine("IF Count: " + ifCount);
            Console.WriteLine("END IF Count: " + endIfCount);
            var check = ifCount == endIfCount;
            return check;
        }
        public static void DeleteEmptyDirs(this DirectoryInfo dir)
        {
            foreach (DirectoryInfo d in dir.GetDirectories())
                d.DeleteEmptyDirs();

            try { dir.Delete(); }
            catch (IOException) { }
            catch (UnauthorizedAccessException) { }
        }
    }
}
