using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
    
namespace BusinessLayer.DbEntities
{
    [Table("CrudActivityReport")]
    public class CrudActivityReport 
    {
        [Key]
        public int CrudActivityReportId { get; set; }
        public string EntityName { get; set; }
        public string AttributeNumber { get; set; }
        public string AttributeName { get; set; }
        public string ObjectName { get; set; }
        public string ObjectType { get; set; }
        public int ProjectId { get; set; }
        public bool HasIndicator { get; set; }
        public bool IsOpen { get; set; }
        public string AttributeDescription { get; set; }
        public string OpenStatement { get; set; }
    }
}
