using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ViewSourceMasterRepository : BaseRepository<ViewSourceMaster>
    {
        public ViewSourceMasterRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
