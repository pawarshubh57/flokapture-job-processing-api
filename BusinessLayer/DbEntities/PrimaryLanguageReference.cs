using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("PrimaryLanguageReference")]
    public class PrimaryLanguageReference
    {
        [Key]
        public int PrimaryReferenceId { get; set; }
        public int LanguageId { get; set; }
        public int BaseCommandId { get; set; }
        public string PrimaryCommand { get; set; }
        public string StartIndicator { get; set; }
        public string EndIndicator { get; set; }
        public string LineContainsIndicator { get; set; }
        public string CreatedOn { get; set; }


        [ForeignKey("LanguageId")]
        public LanguageMaster LanguageMaster { get; set; }

        [ForeignKey("BaseCommandId")]
        public BaseCommandReference BaseCommandReference { get; set; }
    }
}