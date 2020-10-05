

using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
   public class LanguageKeywordRepository : BaseRepository<LanguageKeyword>
    {
       public LanguageKeywordRepository(AppDbContext appDbContext) : base(appDbContext)
       {
       }
    }
}
