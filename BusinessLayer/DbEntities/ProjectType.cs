using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("ProjectType")]
    public class ProjectType
    {
        [Key]
        public int ProjectTypeId { get; set; }

        public int LanguageId { get; set; }
        public string ProjectTypeName { get; set; }
    }
}
