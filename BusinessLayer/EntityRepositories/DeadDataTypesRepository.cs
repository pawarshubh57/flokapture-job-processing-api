using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
    public class DeadDataTypesRepository : BaseRepository<DeadDataTypes>
    {
        private AppDbContext _appDbContext;
        public DeadDataTypesRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }

        public override async Task<IEnumerable<DeadDataTypes>> GetAllItems(Predicate<DeadDataTypes> predicate )
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstDeatDataTypes = await _appDbContext.DeadDataTypes.ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result.FindAll(predicate);
                    return result;
                });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<DeadDataTypes>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstDeatDataTypes, settings);
                var deadDataTypes = JsonConvert.DeserializeObject<List<DeadDataTypes>>(json);
                return deadDataTypes;
            }
        }
    }
}