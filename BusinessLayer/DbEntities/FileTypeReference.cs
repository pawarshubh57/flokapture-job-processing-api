using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Globalization;

namespace BusinessLayer.DbEntities
{
    [Table("FileTypeReference")]
    public class FileTypeReference
    {
        private readonly CultureInfo _usa = new CultureInfo("en-US");
        private DateTime _dateTime;
        [Key]
        public int FileTypeId { get; set; }
        public string FileType { get; set; }

        public DateTime DateAdded
        {
            get { return _dateTime; }
            set { _dateTime = FormatDate(value.ToString(CultureInfo.InvariantCulture)); }
        }

        //private readonly CultureInfo _uk = new CultureInfo("en-GB");
        //string shortUkDateFormatString = _uk.DateTimeFormat.ShortDatePattern;
        //string shortUkTimeFormatString = _uk.DateTimeFormat.ShortTimePattern;

        public DateTime FormatDate(string date)
        {
            DateTime usDate = DateTime.Parse(date, _usa);
            return usDate;
        }
    }
}