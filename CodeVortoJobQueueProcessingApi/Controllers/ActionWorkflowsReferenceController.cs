using System.Threading.Tasks;
using System.Web.Http;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class ActionWorkflowsReferenceController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public ActionWorkflowsReferenceController()
        {
        }

        public ActionWorkflowsReferenceController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public IHttpActionResult Get()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allActionWorkflows = _codeVortoService.ActionWorkflowsRepository.GetAllItems();
                return Ok(allActionWorkflows);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateWorkflowName(int workflowId, string workflowName)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var item = _codeVortoService.ActionWorkflowsRepository
                    .GetItem(workflowId);
                item.WorkflowBusinessName = workflowName;

                var data = await _codeVortoService.ActionWorkflowsRepository
                    .UpdateItem(item);
                return Ok(data);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateWorkflowDesc(int workflowId, string workflowDesc)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var item = _codeVortoService.ActionWorkflowsRepository
                    .GetItem(workflowId);
                item.WorkflowBusinessDescription = workflowDesc;
                var data = await _codeVortoService.ActionWorkflowsRepository
                    .UpdateItem(item);
                return Ok(data);
            }
        }
    }
}
