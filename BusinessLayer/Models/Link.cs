namespace BusinessLayer.Models
{
    public class Link
    {
        public int Origin { get; set; }
        public int Target { get; set; }
        public string LinkText { get; set; }
        public int StatementId { get; set; }
        public int BaseCommandId { get; set; }
        public string BusinessName { get; set; }
        public string BusinessDescription { get; set; }
        public int? ProgramId { get; set; }
    }
}