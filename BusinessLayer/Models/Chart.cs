using System.Collections.Generic;

namespace BusinessLayer.Models
{
    public class Chart
    {
        public List<ChartItem> Charts { get; set; }
    }

    public class ChartItem
    {
        public string label { get; set; }
        public double data { get; set; }
        public string color { get; set; }
    }

}
