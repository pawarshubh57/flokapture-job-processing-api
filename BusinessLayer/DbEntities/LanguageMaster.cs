using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("LanguageMaster")]
    public class LanguageMaster
    {
        [Key]
        public int LanguageId { get; set; }

        public string LanguageName { get; set; }
    }
}