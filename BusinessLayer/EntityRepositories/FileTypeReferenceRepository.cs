using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class FileTypeReferenceRepository : BaseRepository<FileTypeReference>
    {
        public FileTypeReferenceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}