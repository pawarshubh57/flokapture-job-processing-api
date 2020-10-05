

using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("CobolVariable")]
   public class CobolVariable
    {
        [Key]
        public int VariableId { get; set; }

        public int SectionId { get; set; }
        public string SectionName { get; set; }
        public string VariableName { get; set; }
        public string VariableLevel { get; set; }
        public string DataTypeField { get; set; }
        public string DefaultValue { get; set; }
        public int FileId { get; set; }
        public int ProjectId { get; set; }
        public string PictureClause { get; set; }
        public string ComputationOrBinary { get; set; }
        public string Lenght { get; set; }
        public string DataType { get; set; }
        public string ParentId { get; set; }
        public string GraphId { get; set; }
    }
}
