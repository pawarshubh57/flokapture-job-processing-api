using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class DataDependencyRepository : BaseRepository<DataDependency>
    {
        public DataDependencyRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
