using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.Models;

namespace BusinessLayer.EntityRepositories
{
    public class FirstTabProgramDetailsRepository : BaseRepository<FirstTabProgramDetails>
    {
        public FirstTabProgramDetailsRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}