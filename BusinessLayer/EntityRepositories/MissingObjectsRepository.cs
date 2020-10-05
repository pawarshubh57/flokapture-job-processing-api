
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class MissingObjectsRepository : BaseRepository<MissingObjects>
    {
        public MissingObjectsRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
