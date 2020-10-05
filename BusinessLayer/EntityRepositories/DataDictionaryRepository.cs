using System.Collections.Generic;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
   public  class DataDictionaryRepository :BaseRepository<DataDictionary>
    {
       private AppDbContext _appDbContext;
       public DataDictionaryRepository(AppDbContext appDbContext) : base(appDbContext)
       {
       }

        public override async Task<List<DataDictionary>> GetDataFromSqlQuery<T>(string mySqlQuery)
        {
            using (_appDbContext = new AppDbContext())
            {
                var listData = await _appDbContext.Set<DataDictionary>().SqlQuery(mySqlQuery).ToListAsync();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<DataDictionary>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(listData, settings);
                var data = JsonConvert.DeserializeObject<List<DataDictionary>>(json);
                return data;
            }
        }
    }
}
