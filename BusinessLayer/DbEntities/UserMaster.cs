using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("UserMaster")]
    public class UserMaster : EntityBase
    {
        [Key]
        public int? UserId { get; set; }

        public string UserName { get; set; }
        public string Password { get; set; }

        public ICollection<UserDetails> UserDetails { get; set; }
    }
}