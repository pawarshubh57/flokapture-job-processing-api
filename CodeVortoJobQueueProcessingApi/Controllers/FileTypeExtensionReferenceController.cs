using System.Collections.Generic;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class FileTypeExtensionReferenceController : ApiController 
    {
        private ICodeVortoService _codeVortoService;
        
        public FileTypeExtensionReferenceController()
        {
        }

        public FileTypeExtensionReferenceController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> Get()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var fileTypeExtensionList = await _codeVortoService.FileTypeExtensionRepository.GetAllItems();
                return Ok(fileTypeExtensionList);
            }
        }

        [HttpPost]
        public async Task<IHttpActionResult> Post([FromBody] List<FileTypeExtensionReference> lstFileTypeExtensionReferences )
        {
            if (!ModelState.IsValid) return BadRequest("Request is invalid");

            using (_codeVortoService = new CodeVortoService())
            {
                foreach (var fileType in lstFileTypeExtensionReferences)
                {
                    await _codeVortoService.FileTypeExtensionRepository.AddNewItem(fileType);
                }
                return Ok(lstFileTypeExtensionReferences);
            }

            
        }
    }
}
