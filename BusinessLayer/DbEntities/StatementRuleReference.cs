using System;
using System.ComponentModel.DataAnnotations;

namespace BusinessLayer.DbEntities
{
    public class StatementRuleReference
    {
        private DateTime? _createdOn;
        [Key]
        public int RowId { get; set; }

        public int RuleId { get; set; }
        public int StatementIdFrom { get; set; }
        public int StatementIdTo { get; set; }
        public string StatementNotes { get; set; }
        public int CreatedBy { get; set; }
        [DataType(DataType.Date)]
        public DateTime? CreatedOn
        {
            get { return _createdOn; }
            set
            {
                if (value != null)
                    _createdOn = value;
                _createdOn = DateTime.Now;
            }
        }
        public RuleSummaryAndAssociations RuleSummaryAndAssociations { get; set; }
    }

    //public class RuleSummaryAndAssociations
    //{
    //    public int RuleCatalogId { get; set; }
    //    public string RuleName { get; set; }
    //    public string RuleSummary { get; set; }
    //}

    //public class StatementRuleReference
    //{
    //    private DateTime? _createdOn;
    //    [Key]
    //    public int RowId { get; set; }

    //    public int RuleId { get; set; }
    //    public int StatementIdFrom { get; set; }
    //    public int StatementIdTo { get; set; }
    //    public string StatementNotes { get; set; }
    //    public int CreatedBy { get; set; }
    //    [DataType(DataType.Date)]
    //    public DateTime? CreatedOn
    //    {
    //        get { return _createdOn; }
    //        set
    //        {
    //            if (value != null)
    //                _createdOn = value;
    //            _createdOn = DateTime.Now;
    //        }
    //    }

    //    [ForeignKey("RuleId")]
    //    public virtual RuleSummaryAndAssociations RuleSummaryAndAssociations { get; set; }
    //}

}