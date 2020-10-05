using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("EntitiesToExclude")]
    public class EntitiesToExclude
    {
        [Key]
        public int RowId { get; set; }

        public int ProjectId { get; set; }
        public int FileId { get; set; }
        public string FileName { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }
    }
}
