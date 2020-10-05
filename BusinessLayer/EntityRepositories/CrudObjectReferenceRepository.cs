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
    public class CrudObjectReferenceRepository : BaseRepository<CrudObjectReferences>
    {
        private AppDbContext _appDbContext;
        public CrudObjectReferenceRepository(AppDbContext appDbContext) : base(appDbContext)
        {

        }

        public override async Task<List<CrudObjectReferences>> GetAllListItems(Expression<Func<CrudObjectReferences, bool>> expression)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstCrudReferences = await _appDbContext.CrudObjectReference.AsQueryable().Where(expression)
                    .Include("ObjectFileMaster").ToListAsync();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstCrudReferences, settings);
                var crudObject = JsonConvert.DeserializeObject<List<CrudObjectReferences>>(json);
                return crudObject;
            }
        }
    }
}
