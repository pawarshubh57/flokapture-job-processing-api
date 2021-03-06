﻿using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.Models
{
    [Table("SecondTabProgramDetails")]
    public class SecondTabProgramDetails
    {
        private string _hasChild;

        [Key]
        public int RowId { get; set; }

        public int SecondTabRowId { get; set; }

        public string HasChild
        {
            get { return _hasChild; }
            set { _hasChild = value == "1" ? "true" : "false"; }
        }

        public string GraphId { get; set; }
        public string GraphName { get; set; }
        public string ParentId { get; set; }
        public string SpriteCssClass { get; set; }
        public int BaseCommandId { get; set; }
        public int PrimaryCommandId { get; set; }
        public string ClassCalled { get; set; }
        public string MethodCalled { get; set; }
        public string ActualStatementId { get; set; }
        public int ProjectId { get; set; }
        public int ActionWorkflowId { get; set; }
        public int WorkflowStartStatementId { get; set; }
        public int StatementId { get; set; }
        public int IndentLevel { get; set; }
        public int? ProgramId { get; set; }
        public string AlternateName { get; set; }
        public string AnnotateStatement { get; set; }
        public string StatementComment { get; set; }
    }
}
