using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Threading.Tasks;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using Newtonsoft.Json;

namespace BusinessLayer.EntityRepositories
{
    public class StatementReferenceRepository : BaseRepository<StatementReferenceMaster>
    {
        private AppDbContext _appDbContext;
        public StatementReferenceRepository(AppDbContext appDbContext) : base(appDbContext)
        {
        }

        public override async Task<IEnumerable<StatementReferenceMaster>>
            GetAllItems(Predicate<StatementReferenceMaster> predicate)
        {
            using (_appDbContext = new AppDbContext())
            {
                var lstStatementReferenceMaster = await _appDbContext.StatementReferenceMaster.ToListAsync().ContinueWith(t =>
                {
                    var result = t.Result;
                    //return result.FindAll(s=>s.FileId != tKey);
                    var itemList = result.FindAll(predicate);
                    return itemList;
                });
                var settings = new JsonSerializerSettings
                {
                    ContractResolver = new ReferenceLoopResolver<FileMaster>(),
                    PreserveReferencesHandling = PreserveReferencesHandling.None,
                    ReferenceLoopHandling = ReferenceLoopHandling.Ignore,
                    Formatting = Formatting.Indented
                };
                var json = JsonConvert.SerializeObject(lstStatementReferenceMaster, settings);
                var currentStatementReferenceMaster = JsonConvert.DeserializeObject<List<StatementReferenceMaster>>(json);
                return currentStatementReferenceMaster;
            }
        }

        public override async Task<StatementReferenceMaster> AddNewItem(StatementReferenceMaster itemSource)
        {
            // if (itemSource.BaseCommandId == 0) return await base.AddNewItem(itemSource).ConfigureAwait(false);
            var crudListUniVerse = new List<string> { "MATWRITE ", "MATWRITEU ", "MATWRITEL ", "WRITEVU ", "WRITE ", "WRITEU ",
                "WRITEV ", "WRITET ", "WRITEBLK ", "WRITESEQF ", "WRITESEQ ","MATWRITE ", "MATWRITEU ", "MATWRITEL ",
                "WRITEVU ", "WRITE ", "WRITEU ", "WRITEV ", "WRITET ", "WRITEBLK ", "WRITESEQF ", "WRITESEQ ","DELETE ",
                "DELETELIST ", "DELETEU ", "MATREAD ","MATREADL ","MATREADU ", "OPENCHECK ","OPENDEV ","OPENPATH ",
                "OPENSEQ ","OPEN ","LOCATE ", "READ ", "READL ", "READU ", "READV ","REAFV ","READSEQ ","READVL ",
                "READVU ","READBLK ","READLIST ","READNEXT ","READT ", "SSELECT ", "SELECT ", "List " };

            if (crudListUniVerse.Any(itemSource.OriginalStatement.StartsWith)) itemSource.BaseCommandId = 45;

            var crudListCobol = new List<string> { "EXEC SQL ", "REWRITE ", "WRITEQ ", "CLOSE ", "START " };

            if (crudListCobol.Any(itemSource.OriginalStatement.StartsWith)) itemSource.BaseCommandId = 45;

            return await base.AddNewItem(itemSource).ConfigureAwait(false);
        }

        public override async Task<int> BulkInsert(List<StatementReferenceMaster> listOfEntities)
        {
            int insertedCount = 0;
            foreach (var entity in listOfEntities)
            {
                await AddNewItem(entity).ConfigureAwait(false);
                insertedCount++;
            }
            return insertedCount;
        }
    }
}