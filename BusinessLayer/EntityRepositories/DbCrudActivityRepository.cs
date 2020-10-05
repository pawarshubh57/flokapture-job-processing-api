
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class DbCrudActivityRepository : BaseRepository<DbCrudActivity>
    {
        public DbCrudActivityRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
