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
    public class UserMasterRepository : BaseRepository<UserMaster>
    {
        private AppDbContext _appDbContext;
        public UserMasterRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }

        public override async Task<UserMaster> GetItem<T>(Expression<Func<UserMaster, bool>> expression, int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                var quarable = _appDbContext.Set<UserMaster>().Include("UserDetails");
                var entitySet = await quarable.Where(expression).ToListAsync();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<UserMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(entitySet, settings);
                var userMasters = JsonConvert.DeserializeObject<List<UserMaster>>(json);
                var userMaster = userMasters.FirstOrDefault();
                return userMaster;
            }
        }
    }
}