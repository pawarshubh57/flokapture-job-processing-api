using System;
using System.Linq;
using BusinessLayer.Models;

namespace BusinessLayer.UniverseBasic
{
    public class ClsUniverseBasic
    {
        public Node GetGraphTreeNode(int nodeId, string shape, string name, string color, string width, string height, string actualStatementId)
        {
            var node = new Node
            {
                Id = nodeId,
                ShapeId = shape,
                Name = name,
                Color = color,
                Width = width,
                Height = height,
                StatementId = Int32.Parse(actualStatementId.Split('_')[1])
            };
            return node;
        }

        public Link GetGraphTreeLink(int origin, int target, string linkText, int? statementId)
        {
            if (statementId.HasValue)
            {
                var link = new Link
                {
                    Origin = origin,
                    Target = target,
                    LinkText = linkText,
                    StatementId = statementId.Value
                };
                return link;
            }
            else
            {
                var link = new Link
                {
                    Origin = origin,
                    Target = target,
                    LinkText = linkText,
                };
                return link;
            }
        }

        public  string CalculateWidth(int textLength)
        {
            int widthCalculation = (textLength * 2) + 20;
            if (widthCalculation > 200 || textLength > 100)
            {
            }
            else
            {
                widthCalculation = widthCalculation * 1;
            }

            var width = widthCalculation.ToString();
            return width;
        }

        public  string CalculateHeight(int charCountOfText)
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

        public  Node GetNodeDetails(int nodeId, string shapeId, string color, string name, TreeView curItem)
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
                StatementReferenceMaster = curItem.StatementReferenceMaster,
                BaseCommandId = curItem.BaseCommandId,
                NodeParentId = curItem.ParentId,
                GraphId = curItem.GraphId
            };
            return node;
        }

        public  Link GetLinkDetails(int origin, int target, string linkSeqNumber, TreeView curItem)
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

      
    }
}
