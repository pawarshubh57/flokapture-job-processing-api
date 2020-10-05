
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("ProjectDocuments")]
   public class ProjectDocuments
    {
        [Key]
        public int ProjectDocumentId { get; set; }

        public int ProjectId { get; set; }
        public string DocumentTitle { get; set; }
        public string DocumentName { get; set; }
        public string DocumentPath { get; set; }
        [DataType(DataType.Date)]
        public DateTime? UploadedOn { get; set; }

        public int? UserId { get; set; }

        [ForeignKey("ProjectId")]
        public virtual ProjectMaster ProjectMaster { get; set; }
    }
}
