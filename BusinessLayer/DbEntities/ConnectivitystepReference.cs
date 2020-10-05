using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("connectivitystepreference")]
    public class ConnectivityStepReference
    {
        [Key]
        public int Id { get; set; }

        public int NodeId { get; set; }
        public string Name { get; set; }
        public string Shape { get; set; }
        public string Color { get; set; }
        public int ParentId { get; set; }
        public int StatementId { get; set; }
        public int ProjectId { get; set; }
    }
}



