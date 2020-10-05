using System.Collections.Generic;

namespace BusinessLayer.Models
{
    public class FlowChart
    {
        public List<Node> Nodes { get; set; }
        public List<Link> Links { get; set; }
        public List<string> Classes { get; set; }
        public List<string> Methods { get; set; }
        public List<string> CombineList { get; set; }
    }
}
