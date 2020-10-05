using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class PrimaryLanguageReferenceRepository : BaseRepository<PrimaryLanguageReference>
    {
        public PrimaryLanguageReferenceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}