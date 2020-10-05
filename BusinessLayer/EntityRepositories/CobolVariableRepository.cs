
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
   public class CobolVariableRepository : BaseRepository<CobolVariable>
    {
       public CobolVariableRepository(AppDbContext appDbContext) : base(appDbContext)
       {
       }
    }
}
