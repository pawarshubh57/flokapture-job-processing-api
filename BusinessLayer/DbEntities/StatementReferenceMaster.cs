using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("StatementReferenceMaster")]
    public class StatementReferenceMaster
    {
        public int GroupId;
        public string GroupName;

        [Key]
        public int StatementId { get; set; }
        public int FileId { get; set; }
        public int BaseCommandId { get; set; }
        public int? OtherBaseCommandId { get; set; }
        public int ProjectId { get; set; }
        public int PrimaryCommandId { get; set; }
        public string ResolvedStatement { get; set; }
        public string OriginalStatement { get; set; }
        public string MethodName { get; set; }
        public string VariableNameDeclared { get; set; }
        public string DataOrObjectType { get; set; }
        public string ClassCalled { get; set; }
        public string MethodCalled { get; set; }
        public string ClassNameDeclared { get; set; }
        public int? AssociatedToParent { get; set; }
        public string ParsedOrNot { get; set; }
        public string WorkFlowRelevant { get; set; }
        public string BusinessName { get; set; }
        public string BusinessDescription { get; set; }
        public string AlternateName { get; set; }
        public string StatementComment { get; set; }
        public int ReferenceFileId { get; set; }
        public int SolutionId { get; set; }
        public string AnnotateStatement { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }

        [ForeignKey("ReferenceFileId")]
        public virtual FileMaster ReferenceFileMaster { get; set; }
    }
}