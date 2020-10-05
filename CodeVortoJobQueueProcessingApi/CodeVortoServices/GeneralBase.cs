using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace CodeVortoJobQueueProcessingApi.CodeVortoServices
{
    public abstract class GeneralBase : ApiController //, IGeneralRepository<T> where T : class
    {
        public DbSet GetType(string t)
        {
            var location = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);
            var assembly = Assembly.LoadFile(location + "\\BusinessLayer.dll");
            var dbEntities = new AppDbContext();
            return assembly.GetExportedTypes()
                .Where(type => type.Name == t)
                .Select(type => dbEntities.Set(type)).FirstOrDefault();
        }

        public virtual T SetType<T>() where T : class
        {
            using (var dbEntities = new AppDbContext())
            {
                var entity = dbEntities.Set<T>();
                return entity as T;
            }
        }

        public Task<List<NameValue>> GetNameValue()
        {
            var lstNameValue = new List<NameValue>();
            var data = new NameValue
            {
                Value = 0,
                Name = "Select"
            };
            lstNameValue.Add(data);
            return Task.FromResult(lstNameValue);
        }
    }
}