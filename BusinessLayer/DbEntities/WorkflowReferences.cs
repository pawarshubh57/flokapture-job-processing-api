using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("workflowreferences")]
    public class WorkflowReferences
    {
        [Key]
        public int RowId { get; set; }

        public int ActionWorkflowId { get; set; }
        public int ProgramId { get; set; }
        public int StartStatementId { get; set; }
    }
}