using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ConnectivityStepRefereceRepository : BaseRepository<ConnectivityStepReference>
    {
        public ConnectivityStepRefereceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
