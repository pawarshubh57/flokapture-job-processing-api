using BusinessLayer.DbEntities;

namespace BusinessLayer.Models
{
    public class Node
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string ShapeId { get; set; }
        public string Height { get; set; }
        public string Width { get; set; }
        public string Color { get; set; }
        public int ParentId { get; set; }
        public int ChildId { get; set; }
        public int StatementTypeId { get; set; }
        public int FileId { get; set; }
        public int StatementId { get; set; }
        public int BaseCommandId { get; set; }
        public string BusinessName { get; set; }
        public string BusinessDescription { get; set; }
        public string GroupName { get; set; }
        public int GroupId { get; set; }
        public int? ProgramId { get; set; }
        public string OriginalClassName;
        public StatementReferenceMaster StatementReferenceMaster;
        public string NodeParentId { get; set; }
        public string GraphId { get; set; }
    }
}