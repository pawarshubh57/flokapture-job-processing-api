using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ProductConfigurationRepository :BaseRepository<ProductConfiguration>
    {
        public ProductConfigurationRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
