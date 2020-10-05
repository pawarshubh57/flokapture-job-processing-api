using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.ExtensionLibrary;
using Newtonsoft.Json;

namespace CodeVortoJobQueueProcessingApi.CodeVortoServices
{
    public sealed class GeneralRepository<TSource> : IBasicRepository<TSource> where TSource : class
    {
        private AppDbContext _appDbContext;

        public GeneralRepository()
        {
        }

        public GeneralRepository(AppDbContext appDbContext)
        {
            _appDbContext = appDbContext;
        }

        public AppDbContext GetAppDbContext()
        {
            _appDbContext = new AppDbContext();
            return _appDbContext;
        }

        public DbSet<T> GetDbSet<T>() where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var dbSet = _appDbContext.Set<T>();
                return dbSet;
            }
        }

        public DbSet<TSource> DbSet
        {
            get
            {
                var dbContext = new AppDbContext().Set<TSource>();
                return dbContext;
            }
        }

        public Type GetType(TSource tSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                return _appDbContext.Set<TSource>().GetType();
            }
        }

        public async Task<IEnumerable<TSource>> GetAllItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<TSource>().ToListAsync();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var lstData = JsonConvert.DeserializeObject<List<TSource>>(json);
                return lstData;
            }
        }

        public async Task<IEnumerable<TSource>> GetAllItems(Predicate<TSource> predicate)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<TSource>()
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        var itemList = result.FindAll(predicate);
                        return itemList;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var lstData = JsonConvert.DeserializeObject<List<TSource>>(json);
                return lstData;
            }
        }

        public async Task<List<TSource>> GetAllListItems(Expression<Func<TSource, bool>> expression)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<TSource>().AsQueryable().Where(expression)
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<TSource>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<List<TSource>>(json);
                return listData;
            }
        }

        public async Task<List<TSource>> GetAllListItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<TSource>().AsQueryable()
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<TSource>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<List<TSource>>(json);
                return listData;
            }
        }

        public async Task<IEnumerable<T>> GetAllItems<T>(Expression<Func<T, bool>> expression) where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<T>().AsQueryable().Where(expression)
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result;
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<T>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<IEnumerable<T>>(json);
                return listData;
            }
        }

        public async Task<TSource> AddNewItem(TSource itemSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                _appDbContext.Set<TSource>().Add(itemSource);
                await _appDbContext.SaveChangesAsync();
                return itemSource;
            }
        }

        public async Task<TSource> UpdateItem(TSource itemSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                _appDbContext.Set<TSource>().Attach(itemSource);
                _appDbContext.Entry(itemSource).State = EntityState.Modified;
                await _appDbContext.SaveChangesAsync();
                return itemSource;
            }
        }

        public async Task<int> BulkInsert(List<TSource> listOfEntities)
        {
            using (_appDbContext = new AppDbContext())
            {
                _appDbContext.Set<TSource>();
                /*
                var options = new BulkInsertOptions
                {
                    EnableStreaming = true,
                };
                _appDbContext.BulkInsert(listOfEntities, options);
                */
                _appDbContext.Set<TSource>().AddRange(listOfEntities);
                var retVal = await _appDbContext.SaveChangesAsync();
                return retVal;
            }
        }

        public async Task<int> DeleteItem(int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                var item = _appDbContext.Set<TSource>().Find(tKey);
                if (item != null)
                {
                    _appDbContext.Set<TSource>().Remove(item);
                    var records = await _appDbContext.SaveChangesAsync();
                    return records;
                }
                return 0;
            }
        }

        public Task<int> DeleteItem<T>(Predicate<TSource> predicate, int tKey)
        {
            throw new NotImplementedException();
        }

        public TSource GetItem(int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                var item = _appDbContext.Set<TSource>().Find(tKey);
                return item;
            }
        }

        public async Task<TSource> GetItem<T>(Expression<Func<TSource, bool>> expression, int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                var item = await _appDbContext.Set<TSource>().AsQueryable()
                     .Where(expression).ToListAsync().ContinueWith(t =>
                     {
                         var listItem = t.Result;
                         var tItem = listItem.FirstOrDefault();
                         return tItem;
                     });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<TSource>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(item, settings);
                var listData = JsonConvert.DeserializeObject<TSource>(json);
                return listData;
            }
        }

        public async Task<T> GetItem<T>(Expression<Func<T, bool>> expression) where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstItems = await _appDbContext.Set<T>().AsQueryable().Where(expression)
                    .ToListAsync().ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.FirstOrDefault();
                    });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<T>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstItems, settings);
                var listData = JsonConvert.DeserializeObject<T>(json);
                return listData;
            }
        }

        public IHttpActionResult UpdateItems(IEnumerable<TSource> updateSource)
        {
            throw new NotImplementedException();
        }

        public async Task<TSource> FindItem<T>(Predicate<TSource> predicate)
        {
            using (_appDbContext = new AppDbContext())
            {
                var entity = await _appDbContext.Set<TSource>().ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    var data = result.Find(predicate);
                    return data;
                });
                return entity;
            }
        }

        public async Task<T> FindItem<T>(Expression<Func<T, bool>> expression) where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var entity = await _appDbContext.Set<T>().AsQueryable().Where(expression)
                    .ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    var data = result.FirstOrDefault();
                    return data;
                });
                return entity;
            }
        }

        public async Task<List<TSource>> FindAllItems<T>(Predicate<TSource> predicate)
        {
            using (_appDbContext = new AppDbContext())
            {
                var entityList = await _appDbContext.Set<TSource>().ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    var data = result.FindAll(predicate);
                    return data;
                });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<TSource>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(entityList, settings);
                var lstData = JsonConvert.DeserializeObject<List<TSource>>(json);
                return lstData;
            }
        }

        public async Task<List<T>> FindAllItems<T>(Expression<Func<T, bool>> expression) where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var entityList = await _appDbContext.Set<T>()
                    .AsQueryable().Where(expression)
                    .ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    return result;
                });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<T>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(entityList, settings);
                var lstData = JsonConvert.DeserializeObject<List<T>>(json);
                return lstData;
            }
        }

        public async Task<List<TSource>> GetDataFromSqlQuery<T>(string mySqlQuery)
        {
            using (_appDbContext = new AppDbContext())
            {
                var listData = await _appDbContext.Set<TSource>().SqlQuery(mySqlQuery).ToListAsync();
                return listData;
            }
        }

        public async Task<List<T>> GetEntityData<T>(Predicate<T> predicate) where T : class
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

        public async Task<List<T>> ExecuteStoreProcedure<T>(string storeProcName, params object[] parameters)
            where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstData = await _appDbContext.ExecuteStoredProcedure<T>(storeProcName, parameters)
                    .ContinueWith(t =>
                    {
                        var result = t.Result;
                        return result.ToList();
                    });
                return lstData;
            }
        }

        public async Task<int> ExecuteStoreProcedure(string storeProcName, params object[] parameters)
        {
            using (_appDbContext = new AppDbContext())
            {
                int retVal = await _appDbContext.ExecuteStoredProcedure(storeProcName, parameters);
                return retVal;
            }
        }

        public DbSet<TSource> GetDbSet()
        {
            using (_appDbContext = new AppDbContext())
            {
                var dbSet = _appDbContext.Set<TSource>();
                return dbSet;
            }
        }

        public IQueryable<TSource> GetQueryable()
        {
            using (_appDbContext = new AppDbContext())
            {
                var quearableCollection = _appDbContext.Set<TSource>().AsQueryable();
                return quearableCollection;
            }
        }

        public async Task<TSource> DeleteItem(TSource itemSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                _appDbContext.Set<TSource>().Remove(itemSource);
                await _appDbContext.SaveChangesAsync();
                return itemSource;
            }
        }
    }
}