using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("RegexPatternMaster")]
    public class RegexPatternMaster
    {
        [Key]
        public int RegexPatternId { get; set; }
        public string RegexPattern { get; set; }
        public string AlternateCodeRepresentation { get; set; }
        public int BaseCommandId { get; set; }
        public int? TokenCount { get; set; }
        public int? LanguageId { get; set; }
    }

}
