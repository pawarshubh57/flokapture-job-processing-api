using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class ActionWorkflowsRepository : BaseRepository<ActionWorkflows>
    {
        public ActionWorkflowsRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
