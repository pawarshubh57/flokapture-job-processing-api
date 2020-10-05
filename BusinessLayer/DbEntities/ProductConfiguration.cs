using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
   [Table("productconfiguration")]
   public class ProductConfiguration
    {
       [Key]
       public int ProductConfigurationId { get; set; }
       public string PropertyName { get; set; }
       public string PropertyValue { get; set; }
        
       [DataType(DataType.Date)]
       public DateTime CreatedDate { get; set; }

       [DataType(DataType.Date)]
       public DateTime UpdatedDate { get; set; }

       public int UserId { get; set; }
       public int IsDeleted { get; set; }

       public List<ProductConfiguration> ProductConfiguration1 { get; set; }
    }
}
