
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("DataInventory")]
    public class DataInventory
    {
        [Key]
        public int DataInventoryId { get; set; }
        public string ObjectName { get; set; }
        public string ObjectType { get; set; }
        public string ExternalCall { get; set; }
        public string CalledFrom { get; set; }
        public string UsesEntities { get; set; }
        public string ObjectDescription { get; set; }
        public int ProjectId { get; set; }
        public string StatementInList { get; set; }
    }
}
