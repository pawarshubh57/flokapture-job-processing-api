using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("StatementBusinessDetails")]
    public class StatementBusinessDetails
    {
        [Key]
        public int BusinessDetailsId { get; set; }

        public int StatementId { get; set; }
        public string ObjectType { get; set; }
        public string OriginalName { get; set; }
        public string BusinessName { get; set; }
        public string Description { get; set; }
    }
}
