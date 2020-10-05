using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;


namespace BusinessLayer.EntityRepositories
{
    public class WorkflowReferencesRepository : BaseRepository<WorkflowReferences>
    {
        public WorkflowReferencesRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}