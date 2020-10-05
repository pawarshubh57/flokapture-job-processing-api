using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("UserDetails")]
    public class UserDetails
    {
        [Key]
        [Column("UserDetailsId")]
        public int? UserDetailsId { get; set; }

        //  public string RoleName { get; set; }
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public string ContactNumber { get; set; }
        public string EmailId { get; set; }
        public string Address { get; set; }
        public int? UserId { get; set; }
        public int Role { get; set; }
        public string Status { get; set; }
        public string UserName {
            get { return string.Format("{0} {1}", FirstName, LastName); }
        }

        [DataType(DataType.Date)]
        public DateTime CreatedDate { get; set; }

        [ForeignKey("UserId")]
        public virtual UserMaster UserMaster { get; set; }

        public override string ToString()
        {
            return string.Format("{0} {1}", FirstName, LastName);
        }
    }
}