
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
  public  class SolutionMasterRepository : BaseRepository<SolutionMaster>
    {
      public SolutionMasterRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}
