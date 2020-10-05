using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Diagnostics.CodeAnalysis;

namespace BusinessLayer.DbEntities
{
    [Table("ProjectMaster")]
    [SuppressMessage("ReSharper", "ClassWithVirtualMembersNeverInherited.Global")]
    public class ProjectMaster
    {
        [Key]
        public int ProjectId { get; set; }
        public int LanguageId { get; set; }

        public int? Active { get; set; }
        public string ProjectName { get; set; }
        public int ProjectConfigType { get; set; }
        public string PhysicalPath { get; set; }
        public int? TotalFiles { get; set; }
        [DataType(DataType.Date)]
        public DateTime? UploadedDate { get; set; }
        public DateTime? UploadedTime { get; set; }
        [DataType(DataType.Date)]
        public DateTime? ProcessedDate { get; set; }
        public DateTime? ProcessedTime { get; set; }
        public int? NumberOfClasses { get; set; }
        public int? NumberOfScreens { get; set; }
        public int? BusinessRulesCollected { get; set; }
        public int? SolutionId { get; set; }
        public string ProjectDescription { get; set; }
        public string UploadedTm;
        public string ProcessedTm;
        public int OrgnizationId { get; set; }
        public int Processed { get; set; }
        public bool IsCtCode { get; set; }
        [ForeignKey("LanguageId")]
        public virtual LanguageMaster LanguageMaster { get; set; }

        [ForeignKey("ProjectConfigType")]
        public virtual ProjectType ProjectType { get; set; }
    }
}