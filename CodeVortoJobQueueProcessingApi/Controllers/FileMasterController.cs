using System.Threading.Tasks;
using System.Web.Http;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class FileMasterController : ApiController
    {
        private ICodeVortoService _codeVortoService;
        public FileMasterController()
        {
        }

        public FileMasterController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> Get()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileMasterList = await _codeVortoService.FileMasterRepository.GetAllItems();
                return Ok(fileMasterList);
            }
        }
        public int GetLeadingWhitespaceLength(string s)
        {
            var whiteSpaceCount = 0;
            while (char.IsWhiteSpace(s[whiteSpaceCount]))
                whiteSpaceCount++;

            return whiteSpaceCount;
        }
    }
}