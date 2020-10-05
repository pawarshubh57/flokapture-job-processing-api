using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;

namespace BusinessLayer.EntityRepositories
{
    public class DecisionChartRepository : BaseRepository<DecisionChart>
    {
        public DecisionChartRepository(AppDbContext appDbContext)
            : base(appDbContext)
        {
        }
    }
}