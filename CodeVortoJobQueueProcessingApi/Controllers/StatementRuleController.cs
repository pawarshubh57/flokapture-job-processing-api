using System.Web.Http;
using BusinessLayer.DbEntities;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class StatementRuleController : ApiController
    {
        [HttpPost]
        public IHttpActionResult AddStatementRules([FromBody] StatementRuleReference statementRuleReference)
        {
            if (!ModelState.IsValid) return BadRequest();
            var ss = statementRuleReference.RuleSummaryAndAssociations;
            var s = ss.RuleSummary;
            return Ok();
        }
    }
}
