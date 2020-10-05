using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("DecisionChart")]
    public class DecisionChart
    {
        [Key]
        public int DecId { get; set; }

        public int ProjectId { get; set; }
        public int StatementId { get; set; }
        public string DecisionHtml { get; set; }
        public string DecisionXml { get; set; }
    }
}
