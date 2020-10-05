using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("solutionmaster")]
    public class SolutionMaster
    {
        [Key]
        public int SolutionId { get; set; }

        public string SolutionType { get; set; }
    }
}