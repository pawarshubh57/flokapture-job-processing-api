using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class FileTypeReferenceController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public FileTypeReferenceController()
        {
        }

        public FileTypeReferenceController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpPost]
        public async void Post([FromBody] FileTypeReference fileTypeReference)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                await _codeVortoService.FileTypeReferenceRepository.AddNewItem(fileTypeReference);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> Get()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var data = await _codeVortoService.FileTypeReferenceRepository.GetAllItems();
                return Ok(data);
            }
        }
    }
}
