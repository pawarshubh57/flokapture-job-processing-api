using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("ProjectProcessingStep")]
    public class ProjectProcessingStep
    {
        [Key]
        public int ProcessingStepId { get; set; }
        public int ProjectId { get; set; }
        public string ProcessStep { get; set; }
        public bool Status { get; set; }
        public DateTime? StartDate { get; set; }
        public DateTime? EndDate { get; set; } 
    }
}
