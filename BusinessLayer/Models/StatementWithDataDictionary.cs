using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.Models
{
    [Table("statementwithdatadictionary")]
    public class StatementWithDataDictionary
    {
        [Key]
        public int StatementWithDataDictionaryId { get; set; }

        public int StatementId { get; set; }
        public string OrignalStatement { get; set; }
        public string ReplacedStatement { get; set; }
    }
}