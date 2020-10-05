using System.Collections.Generic;
using BusinessLayer.DbEntities;

namespace BusinessLayer.Models
{
    public class TreeViewData
    {
        public TreeViewData() { }

        public TreeViewData(List<TreeView> treeViewData)
        {
            TreeViewList = treeViewData;
        }

        public ActionWorkflows ActionWorkflows { get; set; }
        public List<TreeView> TreeViewList { get; set; }
        public List<Link> Links { get; set; }
        public List<Node> Nodes { get; set; }
    }

    public class WorkFlowData
    {
        public ActionWorkflows ActionWorkflows { get; set; }
        public IEnumerable<WorkflowTreeviewSecondTabDetails> TreeViewListSecondTab { get; set; }
        public IEnumerable<WorkflowTreeviewTabFirstDetails> TreeViewListFirstTab { get; set; }
        public IEnumerable<WorkflowLinkDetails> Links { get; set; }
        public IEnumerable<WorkflowNodeDetails> Nodes { get; set; }
    }
}