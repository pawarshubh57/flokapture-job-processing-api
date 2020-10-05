 using System.Collections.Generic;
 using System.ComponentModel.DataAnnotations;
 using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
 {
     [Table("BaseCommandReference")]
     public class BaseCommandReference
     {
         [Key]
         public int BaseCommandId { get; set; }
         public string BaseCommand { get; set; }

         public ICollection<PrimaryLanguageReference> PrimaryLanguageReference { get; set; }
     }
 }
