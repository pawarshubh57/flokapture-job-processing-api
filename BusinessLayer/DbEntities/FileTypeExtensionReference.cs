using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("FileTypeExtensionReference")]
    public class FileTypeExtensionReference
    {
        [Key]
        public int FileTypeExtensionId { get; set; }
        public int LanguageId { get; set; }
        public string FileExtension { get; set; }
        public int FileTypeId { get; set; }
        public string FileTypeName { get; set; }
        public int ProjectId { get; set; }

        [ForeignKey("FileTypeId")]
        public virtual FileTypeReference FileTypeReference { get; set; }

        [ForeignKey("LanguageId")]
        public virtual LanguageMaster LanguageMaster { get; set; }
    }
}
