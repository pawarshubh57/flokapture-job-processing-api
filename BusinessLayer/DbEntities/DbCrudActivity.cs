using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
namespace BusinessLayer.DbEntities
{
    [Table("DbCrudActivities")]
    public class DbCrudActivity
    {
        [Key]
        public int ActivityId { get; set; }

        public string EntityName { get; set; }
        public string ReferenceObjectName { get; set; }
        public string SelectOrRead { get; set; }
        public string InsertOrCreate { set; get; }
        public string Update { set; get; }
        public string Delete { set; get; }
        public int FileId { get; set; }
        public int ProjectId { get;set; }
        public bool IsOpen { get; set; }
        public string OpenStatement { get; set; }
        public bool HasIndicator { get; set; }

        // public string FilePath { get; set; }
        // public string FileName { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }
    }
}
