using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class FileObjectMethodReferenceRepository : BaseRepository<FileObjectMethodReference>
    {
        public FileObjectMethodReferenceRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}