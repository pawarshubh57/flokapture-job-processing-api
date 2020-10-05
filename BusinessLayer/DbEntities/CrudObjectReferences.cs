using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("CrudObjectReferences")]
    public class CrudObjectReferences
    {
        [Key]
        public int CrudObjectReferenceId { get; set; }

        public string ObjectName { get; set; }
        public int? ObjectFileId { get; set; }
        public string ObjectType { get; set; }
        public string UsedBy { get; set; }
        public int? UsedByFileId { get; set; }
        public string Statement { get; set; }
        public string PhysicalEntityName { get; set; }
        public int? EntityFileId { get; set; }
        public string LogicalEntityName { get; set; }

        [ForeignKey("ObjectFileId")]
        public FileMaster ObjectFileMaster { get; set; }

        [ForeignKey("UsedByFileId")]
        public FileMaster UsedByFileMaster { get; set; }

        [ForeignKey("EntityFileId")]
        public FileMaster EntityFileMaster { get; set; }
        public int ProjectId { get; set; }  
    }
}
