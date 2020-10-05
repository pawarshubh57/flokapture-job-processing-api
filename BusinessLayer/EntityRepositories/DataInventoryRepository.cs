

using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
   public class DataInventoryRepository :BaseRepository<DataInventory>
    {
        public DataInventoryRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
