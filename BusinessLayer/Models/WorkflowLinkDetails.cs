using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.Models
{
    [Table("WorkflowLinkDetails")]
    public class WorkflowLinkDetails
    {
        [Key]
        public int RowId { get; set; }
        public int Origin { get; set; }
        public int Target { get; set; }
        public string LinkText { get; set; }
        public int StatementId { get; set; }
        public int BaseCommandId { get; set; }
        public string BusinessName { get; set; }
        public string BusinessDescription { get; set; }
        public int ProjectId { get; set; }
        public int ActionWorkflowId { get; set; }
        public int WorkflowStartStatementId { get; set; }
        public int? ProgramId { get; set; }
    }
}
