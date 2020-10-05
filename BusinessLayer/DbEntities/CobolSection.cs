
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("CobolSection")]
    public class CobolSection
    {
        [Key]
        public  int SectionId { get; set; }
        public string SectionName { get; set; }
    }
}
