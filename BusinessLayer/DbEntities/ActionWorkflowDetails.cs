using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("ActionWorkflowDetails")]
    public class ActionWorkflowDetails
    {
        [Key]
        public int ActionWorkflowDetailsId { get; set; }

        public int ActionWorkflowId { get; set; }
        public int DecisionCount { get; set; }
        public string ExtrernalCalls { get; set; }
        public string InternalCalls { get; set; }
        public string View { get; set; }
        public string ProjectName { get; set; }
        public string OriginObject { get; set; }
        public string WorkflowName { get; set; }
        public string Disable { get; set; }
        public int IsDeleted { get; set; }
        public string ShortDetails { get; set; }
        public int ProjectId { get; set; }
        public string GraphId { get; set; }
        public string ParentId { get; set; }
        public string ObjectType { get; set; }
        public int FileMenuId { get; set; }

        //[ForeignKey("ActionWorkflowId")]
        //public virtual ActionWorkflows ActionWorkflows { get; set; }
    }
}
