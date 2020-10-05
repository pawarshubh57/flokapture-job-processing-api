
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
   public class CobolSectionRepository :BaseRepository<CobolSection>
    {
        public CobolSectionRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }
    }
}
