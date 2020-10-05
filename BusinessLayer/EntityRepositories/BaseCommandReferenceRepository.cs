using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DbEntities;
using BusinessLayer.DatabaseContext;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
    public class BaseCommandReferenceRepository : BaseRepository<BaseCommandReference>
    {
        private AppDbContext _appDbContext;

        public BaseCommandReferenceRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }

        public override async Task<IEnumerable<BaseCommandReference>> GetAllItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<BaseCommandReference>()
                    .Include("PrimaryLanguageReference").ToListAsync();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<BaseCommandReference>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var baseCommandReference = JsonConvert.DeserializeObject<List<BaseCommandReference>>(json);
                return baseCommandReference;
            }
        }

        public override async Task<List<BaseCommandReference>> GetAllListItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<BaseCommandReference>().Include("PrimaryLanguageReference")
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<BaseCommandReference>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<List<BaseCommandReference>>(json);
                return listData;
            }
        }

        public new async Task<List<T>> GetEntityData<T>(Predicate<T> predicate) where T : class 
        {
            using (_appDbContext = new AppDbContext())
            {
                var entityList = await _appDbContext.Set<T>().ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    var data = result.FindAll(predicate);
                    return data;
                });
                return entityList;
            }
        }

        public new async Task<List<T>> GetDataFromSqlQuery<T>(string mySqlQuery) where T : class 
        {
            using (_appDbContext = new AppDbContext())
            {
                var listData = await _appDbContext.Set<T>().SqlQuery(mySqlQuery).ToListAsync();
                return listData;
            }
        }
    }
}