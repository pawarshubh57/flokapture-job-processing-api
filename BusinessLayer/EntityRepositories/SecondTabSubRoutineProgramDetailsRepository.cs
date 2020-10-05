using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
    public class SecondTabSubRoutineProgramDetailsRepository : BaseRepository<SecondTabSubRoutineProgramDetails>
    {
       public SecondTabSubRoutineProgramDetailsRepository(AppDbContext appDbContext) : base(appDbContext)
       {
       }
    }
}
