using BusinessLayer.DatabaseContext;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class AlwaysConsiderClassesRepository : BaseRepository<AlwaysConsiderClasses>
    {
        public AlwaysConsiderClassesRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
