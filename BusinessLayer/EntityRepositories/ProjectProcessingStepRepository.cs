

using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
   public class ProjectProcessingStepRepository : BaseRepository<ProjectProcessingStep>
    {
        public ProjectProcessingStepRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
