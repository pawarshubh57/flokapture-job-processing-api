using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("viewsourcemaster")]
    public class ViewSourceMaster
    {
        [Key]
        public int ViewSourceId { get; set; }

        public int FileId { get; set; }
        public string SourceData { get; set; }
        public string SourceWithoutComments { get; set; }
        public int ProjectId { get; set; }
    }
}