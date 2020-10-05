using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
    public class WorkflowTreeviewSecondTabDetailsRepository : BaseRepository<WorkflowTreeviewSecondTabDetails>
    {
        public WorkflowTreeviewSecondTabDetailsRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}