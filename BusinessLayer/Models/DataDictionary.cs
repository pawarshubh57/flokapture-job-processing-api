using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Text.RegularExpressions;

namespace BusinessLayer.Models
{
    [Table("UniverseBasicDataDictionary")]
    public class DataDictionary
    {
        [Key]
        public int RowId { get; set; }

        public string FileName { get; set; }
        public string FieldNo { get; set; }
        public string Description { get; set; }
        public string FieldLabel { get; set; }
        public string RptFieldLength { get; set; }
        public string TypeOfData { get; set; }
        public string SingleArray { get; set; }
        public string DateOfCapture { get; set; }
        public string ReplacementName { get; set; }
        public int ProjectId { get; set; }
    }

    [Table("UniverseBasicFileMenuRevised")]
    public class UniverseFileMenu
    {
        private string _menuTitle;
        private string _menuDescription;

        [Key]
        public int FileMenuId { get; set; }

        public string WorkflowMenuName
        {
            get
            {
                if (string.IsNullOrEmpty(MenuDescription))
                    return MenuTitle;

                if (MenuDescription.StartsWith("-") || MenuDescription.StartsWith(" -"))
                    return string.Format("{0} {1}", MenuTitle, MenuDescription);

                return string.Format("{0} - {1}", MenuTitle, MenuDescription);
            }
        }

        public string MenuId { get; set; }

        public string MenuTitle
        {
            get { return _menuTitle; }
            set
            {
                if (string.IsNullOrEmpty(value)) return;

                string titleValue = value; //.ToLower();
                titleValue = Regex.Replace(titleValue, @"[ ]{2,}", " - ");
                _menuTitle = titleValue; // CultureInfo.CurrentCulture.TextInfo.ToTitleCase(titleValue);
            }
        }

        public string MenuDescription
        {
            get { return _menuDescription; }
            set
            {
                if (string.IsNullOrEmpty(value)) return;

                string titleValue = value; //.ToLower();
                var regex = new Regex(@"@\(\d+\,\d+\)", RegexOptions.IgnoreCase);
                if (regex.IsMatch(titleValue))
                {
                    foreach (Group group in regex.Match(titleValue).Groups)
                    {
                        var grpValue = titleValue.Substring(group.Value.Length).Trim();
                        _menuDescription = grpValue; // CultureInfo.CurrentCulture.TextInfo.ToTitleCase(grpValue);
                        break;
                    }
                }
                else
                    _menuDescription = titleValue; // CultureInfo.CurrentCulture.TextInfo.ToTitleCase(titleValue);
            }
        }

        public string ActionExecuted { get; set; }
        public int UserId { get; set; }

        [DataType(DataType.Date)]
        public DateTime UploadedOn { get; set; }

        public int ProjectId { get; set; }
    }
}