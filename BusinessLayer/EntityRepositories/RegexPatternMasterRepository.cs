using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using Newtonsoft.Json;


/*
 * Created by: Yogesh Sonawane
 * Created date:  10 Jan. 2014
 * Updated on: 23 Aug. 2015
 * Purpose: Communication channel between MySQL Database and Web Api
 * Salus-U Inc. All rights reserved.
*/
namespace BusinessLayer.EntityRepositories
{
    public sealed class RegexPatternMasterRepository : BaseRepository<RegexPatternMaster>
    {
        private AppDbContext _appDbContext;
        public RegexPatternMasterRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }

        public override async Task<IEnumerable<RegexPatternMaster>> GetAllItems(Predicate<RegexPatternMaster> predicate)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstRegexMaster = await _appDbContext.RegexPatternMaster.ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result.FindAll(predicate);
                    return result;
                }).ConfigureAwait(false);
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<RegexPatternMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstRegexMaster, settings);
                var currentRegexMasterDetails = JsonConvert.DeserializeObject<List<RegexPatternMaster>>(json);
                return currentRegexMasterDetails;
            }
        }
    }
}
