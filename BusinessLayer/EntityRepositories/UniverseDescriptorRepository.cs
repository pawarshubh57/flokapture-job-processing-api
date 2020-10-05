using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
   public class UniverseDescriptorRepository : BaseRepository<UniverseDescriptor>
    {
        public UniverseDescriptorRepository(AppDbContext appDbContext) : base(appDbContext)
        {

        }
    }
}
