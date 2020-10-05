using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using MySql.Data.MySqlClient;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
    public class ProjectMasterRepository : BaseRepository<ProjectMaster>
    {
        private AppDbContext _appDbContext;

        public ProjectMasterRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }

        public override async Task<ProjectMaster> DeleteItem(ProjectMaster itemSource, int id)
        {
            using (_appDbContext = new AppDbContext())
            {
                //itemSource.ProjectConfigType = id;
                if (id != itemSource.ProjectId) return null;
                _appDbContext.Set<ProjectMaster>().Remove(itemSource);
                await _appDbContext.SaveChangesAsync();
                return itemSource;
            }
        }

        public override async Task<ProjectMaster> AddNewItem(ProjectMaster itemSource)
        {
            using (_appDbContext = new AppDbContext())
            {
                var project = await _appDbContext.ProjectMaster
                    .FirstOrDefaultAsync(p => p.ProjectName == itemSource.ProjectName);
                if (project != null) return null;

                var projectMaster = await base.AddNewItem(itemSource);
                return projectMaster;
            }
        }

        public override async Task<IEnumerable<ProjectMaster>> GetAllItems()
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstData =
                    await
                        _appDbContext.Set<ProjectMaster>()
                            .AsQueryable()
                            .ToListAsync();
                
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstData, settings);
                var currentProjects = JsonConvert.DeserializeObject<List<ProjectMaster>>(json);
                return  currentProjects;
            }
        }

        public override ProjectMaster GetItem(int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                var projectDetails =
                    _appDbContext.ProjectMaster
                        .Where(p => p.ProjectId == tKey)
                        .ToList();
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(projectDetails, settings);
                var currentCaseMasterDetails = JsonConvert.DeserializeObject<List<ProjectMaster>>(json);
                return currentCaseMasterDetails.FirstOrDefault();
            }
        }

        public List<ProjectMaster> GetProjectsItems(int tKey)
        {
            using (_appDbContext = new AppDbContext())
            {
                if (tKey != 0)
                {
                    var projectDetails =
                        _appDbContext.ProjectMaster.Include("FileTypeExtensionReference")
                            .Where(p => p.ProjectId == tKey)
                            .ToList();

                    var settings = new JsonSerializerSettings
                    {
                        ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                        PreserveReferencesHandling = PreserveReferencesHandling.None,
                        ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                        Formatting = Formatting.Indented
                    };
                    var json = JsonConvert.SerializeObject(projectDetails, settings);
                    var currentCaseMasterDetails = JsonConvert.DeserializeObject<List<ProjectMaster>>(json);
                    return currentCaseMasterDetails;
                }
                else
                {
                    var projectDetails =
                        _appDbContext.ProjectMaster.Include("FileTypeExtensionReference")
                         .Include("LanguageMaster").ToList();

                    var settings = new JsonSerializerSettings
                    {
                        ContractResolver = new ReferenceLoopResolver<ProjectMaster>(),
                        PreserveReferencesHandling = PreserveReferencesHandling.None,
                        ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                        Formatting = Formatting.Indented
                    };
                    var json = JsonConvert.SerializeObject(projectDetails, settings);
                    var currentCaseMasterDetails = JsonConvert.DeserializeObject<List<ProjectMaster>>(json);
                    return currentCaseMasterDetails;
                }
            }
        }

        public IEnumerable<string> ProcessAllFilesFromDirectory(string directoryPath, List<string> extensionsList)
        {
            var allFiles = Directory.GetFiles(directoryPath, "*.*", SearchOption.AllDirectories)
                .Where(s => extensionsList.Any(s.EndsWith));
            return allFiles;
        }

        public new T GetEntityData<T>() where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var entity = _appDbContext.Set<T>().ToListAsync();
                return entity as T;
            }
        }

        public async Task<List<T>> GetAllIgnoredFiles<T>(int languageId) // where T : class
        {
            using (_appDbContext = new AppDbContext())
            {
                var parameter = new MySqlParameter
                {
                    ParameterName = "@lId",
                    Value = languageId
                };
                var ignoreFiles =
                    await
                        _appDbContext.Database
                            .SqlQuery<T>("Call SpGetAllIngoredFiles(@lId)", parameter).ToListAsync()
                            .ContinueWith(t =>
                            {
                                var result = t.Result;
                                return result;
                            });
                return ignoreFiles;
            }
        }
    }
}