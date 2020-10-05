using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("MissingObjects")]
    public class MissingObjects
    {
        [Key]
        public int MissingObjectId { get; set; }
        public int ProjectId { get; set; }
        public int FileId { get; set; }
        public string CalledObjectName { get; set; }
        public string EntityName { get; set; }
        public string FromObject { get; set; }
        public string Type { get; set; }
        public string Statement { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }
    }
}

