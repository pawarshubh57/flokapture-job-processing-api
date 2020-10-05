using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
    public class FileMasterRepository : BaseRepository<FileMaster>
    {
        private AppDbContext _appDbContext;
        public FileMasterRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }

        public override async Task<IEnumerable<FileMaster>> GetAllItems(Predicate<FileMaster> predicate )
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstFileMaster = await _appDbContext.FileMaster.ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result.FindAll(predicate);
                    return result;
                });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<FileMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstFileMaster, settings);
                var currentFileMasterDetails = JsonConvert.DeserializeObject<List<FileMaster>>(json);
                return currentFileMasterDetails;
            }
        }

        public override async Task<List<FileMaster>> GetAllListItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<FileMaster>().Include("ProjectMaster").Include("FileTypeExtensionReference")
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<FileMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<List<FileMaster>>(json);
                return listData;
            }
        }

        public override async Task<List<FileMaster>> GetAllListItems(Expression<Func<FileMaster, bool>> expression)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<FileMaster>().AsQueryable().Where(expression)
                    .Include("ProjectMaster").Include("FileTypeExtensionReference")
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<FileMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<List<FileMaster>>(json);
                return listData;
            }
        }
    }
}