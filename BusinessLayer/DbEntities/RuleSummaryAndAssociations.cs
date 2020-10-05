using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using BusinessLayer.Models;

namespace BusinessLayer.DbEntities
{
    [Table("RuleSummaryAndAssociations")]
    public class RuleSummaryAndAssociations
    {
        private DateTime? _createdOn;
        [Key]
        public int RuleId { get; set; }
        public int RuleCatalogId { get; set; }
        public string RuleName { get; set; }
        public string RuleSummary { get; set; }
        public int CreatedBy { get; set; }
        [DataType(DataType.Date)]
        public DateTime? CreatedOn {
            get { return _createdOn; }
            set
            {
                if (value != null)
                    _createdOn = value;
                _createdOn = DateTime.Now;
            }
        }

        [ForeignKey("RuleCatalogId")]
        public virtual CatalogMaster CatalogMaster { get; set; }

        public ICollection<StatementRuleReference> StatementRuleReference { get; set; }
    }
}