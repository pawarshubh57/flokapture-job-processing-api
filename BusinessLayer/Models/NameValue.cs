namespace BusinessLayer.Models
{
    public class NameValue
    {
        public int Value { get; set; }
        public string Name { get; set; }
        public string MethodName { get; set; }
        public int ProgramId { get; set; }
        public string CalledObjectName { get; set; }
        public string CalledObjectType { get; set; }
    }
}