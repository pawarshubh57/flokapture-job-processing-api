using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
  public  class WorkflowTreeviewTabFirstDetailsRepository : BaseRepository<WorkflowTreeviewTabFirstDetails>
    {
        public WorkflowTreeviewTabFirstDetailsRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
