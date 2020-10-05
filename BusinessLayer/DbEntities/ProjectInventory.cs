using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("Project-Inventory")]
    public class ProjectInventory
    {
        [Key]
        public int InventoryRowId { get; set; }
        public int FileId { get; set; }
        public int SolutionId { get; set; }
        public int ProjectId { get; set; }
        public string ObjectName { get; set; }
        public int LoC { get; set; }
        public int Complexity { get; set; }
        public string ExternalCall { get; set; }
        public int InternalCall { get; set; }
        public string ExtenstionType { get; set; }
        public string UsesEntities { get; set; }
        public string UsesQueries { get; set; }
        public string UsesReports { get; set; }
        public string UsesObjects { get; set; }
        public string ParticipateInWorkflow { get; set; }
        public string Description { get; set; }
        public string CalledFrom { get; set; }
        public string CallingTo { get; set; }
        public string ObjectsWithCurrentFile { get; set; }
        public string ParticipateInWorkflowWithBusinessName { get; set; }
        public string UsesQueriesWithCurrentFile { get; set; }
        public string UsesReportsWithCurrentFile { get; set; }

        [ForeignKey("ProjectId")]
        public virtual ProjectMaster ProjectMaster { get; set; }

        [ForeignKey("SolutionId")]
        public virtual SolutionMaster SolutionMaster { get; set; }
    }
}