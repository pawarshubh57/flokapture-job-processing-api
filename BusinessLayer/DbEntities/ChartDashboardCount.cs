namespace BusinessLayer.DbEntities
{

    public class ChartDashboardCount
    {
        public int RowId { get; set; }
        public int NoOfVb { get; set; }
        public int NoOfDesigner { get; set; }
        public int NoOfConfig { get; set; }
        public int NoOfSVC { get; set; }
        public int NoOfCbl { get; set; }
        public int NoOfJcl { get; set; }
        public int NoOfProc { get; set; }
    }

    public class ChartDashboardCountNew
    {

        public string Extension { get; set; }
        public string Count { get; set; }
    }

    public class ChartDashboardActionWorkFlow
    {
        public string Project { get; set; }
        public string Count { get; set; }
    }


}
