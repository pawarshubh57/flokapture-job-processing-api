

using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
   public  class StatementWithDataDictionaryRepository :BaseRepository<StatementWithDataDictionary>
    {
       public StatementWithDataDictionaryRepository(AppDbContext appDbContext) : base(appDbContext)
       {
       }
    }
}
