using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("AlwaysConsiderClasses")]
    public class AlwaysConsiderClasses
    {
        [Key]
        public int ClassId { get; set; }

        public string ClassName { get; set; }
        public string Assembly { get; set; }
    }
}
