using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.Models
{
    [Table("CatalogMaster")]
    public class CatalogMaster
    {
        [Key]
        public int CatalogId { get; set; }

        public string CatalogName { get; set; }
        public int CreatedBy { get; set; }
        [DataType(DataType.Date)]
        public DateTime? CreatedOn { get; set; }
    }
}
