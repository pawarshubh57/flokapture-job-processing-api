using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ConnectivityLinkRefereceRepository : BaseRepository<ConnectivityLinkReferece>
    {
        public ConnectivityLinkRefereceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
