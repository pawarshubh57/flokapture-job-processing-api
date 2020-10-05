using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class FileObjectReferenceRepository : BaseRepository<FileObjectReference>
    {
        public FileObjectReferenceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}