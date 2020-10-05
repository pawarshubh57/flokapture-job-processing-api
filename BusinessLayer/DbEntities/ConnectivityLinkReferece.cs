using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("connectivitylinkreference")]
    public class ConnectivityLinkReferece
    {
        [Key]
        public int? Id { get; set; }

        public int Origin { get; set; }
        public int Target { get; set; }
        public string LinkText { get; set; }
        public int StatementId { get; set; }
        public int ProjectId { get; set; }
    }
}


