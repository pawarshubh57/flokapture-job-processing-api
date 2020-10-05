using System.Data;
using System.Threading.Tasks;
using DataAccessLayer;
using MySql.Data.MySqlClient;

namespace BusinessLayer.EntityClasses
{
    public class ClsWorkflowWorkspaceData
    {
        private readonly MySqlDbConnectionBaseClass _appBlock = new MySqlDbConnectionBaseClass();

        public async Task<DataSet> GetWorkspaceWorkflowData(int projectId, int stmtId)
        {
            MySqlParameter[] parameters =
            {
                new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId},
                new MySqlParameter("@stmtId", MySqlDbType.Int32) {Value = stmtId}
            };
            DataSet dataSet = await _appBlock.ExecuteProcedureForSelect("SpGetWorkFlowWorkspaceData", parameters);
            return dataSet;
        }
    }
}
