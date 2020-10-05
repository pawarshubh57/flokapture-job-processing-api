using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;


namespace BusinessLayer.DbEntities
{
    [Table("DeadDataTypes")]
    public class DeadDataTypes
    {
        [Key]
        public int KeywordId { get; set; }

        public int LanguageId { get; set; }
        public string KaywordName { get; set; }
    }
}
