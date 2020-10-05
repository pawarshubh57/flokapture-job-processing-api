using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("IgnoreFiles")]
    public class IgnoreFiles
    {
        [Key]
        public int RowId { get; set; }

        public int LanguageId { get; set; }
        public string FileName { get; set; }
        public string FileExtension { get; set; }
    }
}
