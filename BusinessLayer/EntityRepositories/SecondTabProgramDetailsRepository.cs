using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
    public class SecondTabProgramDetailsRepository : BaseRepository<SecondTabProgramDetails>
    {
        public SecondTabProgramDetailsRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}