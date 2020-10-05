using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ActionWorkflowDetailsRepository : BaseRepository<ActionWorkflowDetails>
    {
        public ActionWorkflowDetailsRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
