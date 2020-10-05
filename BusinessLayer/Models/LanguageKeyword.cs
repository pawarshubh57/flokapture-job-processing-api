
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
namespace BusinessLayer.Models
{
    [Table("languagekeywords")]
    public class LanguageKeyword
    {
        [Key]
        public int langKeyId { get; set; }
        public string KeywordName { get; set; }
        public int languageId { get; set; }
    }
}
