using System.Data.Entity;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class FileTypeExtensionReferenceRepository : BaseRepository<FileTypeExtensionReference>
    {
        private AppDbContext _appDbContext;
        public FileTypeExtensionReferenceRepository(AppDbContext appDbContext) : base(appDbContext) { }
        public override async Task<FileTypeExtensionReference> DeleteItem(FileTypeExtensionReference itemSource, int id)
        {
            using (_appDbContext = new AppDbContext())
            {
                if (id != itemSource.FileTypeExtensionId) return null;
                _appDbContext.Set<FileTypeExtensionReference>().Remove(itemSource);
                await _appDbContext.SaveChangesAsync();
                return itemSource;
            }
        }
        public override async Task<FileTypeExtensionReference> AddNewItem(FileTypeExtensionReference itemSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                var project = await _appDbContext.FileTypeExtensionReference
                    .FirstOrDefaultAsync(p => p.FileExtension == itemSource.FileExtension);
                if (project != null) return null;

                var projectMaster = await base.AddNewItem(itemSource);
                return projectMaster;
            }
        }
    }
}