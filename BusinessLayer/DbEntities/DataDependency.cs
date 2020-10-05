using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;


namespace BusinessLayer.DbEntities
{
    [Table("DataDependency")]
    public class DataDependency
    {
        [Key]
        public int DataDepedencyId { get; set; }

        public int ProjectId { get; set; }
        public string Entity { get; set; }
        public string Attributes { get; set; }
        public int FileId { get; set; }
        public string EntityOld { get; set; }
        public int FileIdOld { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }
    }
}
