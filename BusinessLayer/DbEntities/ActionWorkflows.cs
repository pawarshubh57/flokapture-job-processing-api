using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Text.RegularExpressions;
using BusinessLayer.ExtensionLibrary;

namespace BusinessLayer.DbEntities
{
    [Table("ActionWorkflows")]
    public class ActionWorkflows
    {
        private string _originEventMethod = string.Empty;
        private string _technicalAndBusinessName = string.Empty;
        private string _workflowBusinessName = string.Empty;

        [Key]
        public int ActionWorkflowId { get; set; }

        public int? ProjectId { get; set; }
        public string WorkflowName { get; set; }
        public string OriginObject { get; set; }
        public string OriginFileName { get; set; }
        public string OriginFilePath { get; set; }

        public string OriginEventMethod
        {
            get
            {
                var split = Regex.Replace(_originEventMethod, "(?<=[a-z])([A-Z])", " $1", RegexOptions.Compiled);
                // split = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(split.ToLower());
                // split = split.Replace('_', ' ');
                return split;
            }
            set { _originEventMethod = !string.IsNullOrEmpty(value) ? value : string.Empty; }
        }

        public int MethodStatementId { get; set; }
        public string RelatedScreenControl { get; set; }
        public string ServiceBaseAddress { get; set; }
        public string ServiceContract { get; set; }
        public int FileId { get; set; }

        [DataType(DataType.Date)]
        public DateTime? CreatedOn { get; set; }

        public int? CreatedBy { get; set; }
        public string EndPointOrService { get; set; }

        public string WorkflowBusinessName
        {
            get
            {
                var split = Regex.Replace(_workflowBusinessName, "(?<=[a-z])([A-Z])", " $1", RegexOptions.Compiled);
                // split = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(split.ToLower());
                split = split.Replace('_', ' ');
                return split;
            }
            set { _workflowBusinessName = !string.IsNullOrEmpty(value) ? value : string.Empty; }
        }
        public string TechnicalAndBusinessName
        {
            get { return _technicalAndBusinessName; }
            set
            {
                _originEventMethod = _originEventMethod.Trim();
                if (string.IsNullOrEmpty(_originEventMethod) || _originEventMethod.ContainsAll("(", ")") ||
                    _originEventMethod.Contains("Private Sub"))
                    value = WorkflowName;
                else
                    value = WorkflowName + " - " + _originEventMethod;

                _technicalAndBusinessName = value;
            }
        }

        public string WorkflowBusinessDescription { get; set; }
        public int? Processed { get; set; }
        public int IsDeleted { get; set; }
        public string ReasonAboutDisable { get; set; }
        public int FileMenuId { get; set; }

        [ForeignKey("FileId")]
        public virtual FileMaster FileMaster { get; set; }

        [ForeignKey("ProjectId")]
        public virtual ProjectMaster ProjectMaster { get; set; }
    }
}