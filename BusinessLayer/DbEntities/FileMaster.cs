using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

// ReSharper disable ClassWithVirtualMembersNeverInherited.Global
namespace BusinessLayer.DbEntities
{
    [Table("FileMaster")]
    public class FileMaster 
    {
        [Key]
        public int FileId { get; set; }

        public int ProjectId { get; set; }
        public string FileName { get; set; }
        public string FilePath { get; set; }
        public int Processed { get; set; }
        public int FileTypeExtensionId { get; set; }
        public int? SolutionId { get; set; }
        public string WorkFlowStatus { get; set; }
        public int DoneParsing { get; set; }
        public int IsNewVersion { get; set; } // 1 for new version, 0 for old / first uploaded version
        public int LinesCount { get; set; }
        public string FileType { get; set; }

        [ForeignKey("ProjectId")]
        public virtual ProjectMaster ProjectMaster { get; set; }

        [ForeignKey("FileTypeExtensionId")]
        public virtual FileTypeExtensionReference FileTypeExtensionReference { get; set; }
    }
}
