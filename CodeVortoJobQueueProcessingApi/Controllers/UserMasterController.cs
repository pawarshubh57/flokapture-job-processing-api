using System;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityClasses;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class UserMasterController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public UserMasterController()
        {
            //_codeVortoService = new CodeVortoService();
        }

        public UserMasterController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> Get()
        {
            _codeVortoService = new CodeVortoService();
            var usersList = await _codeVortoService.UserMasterRepository.GetAllItems();
            return Ok(usersList);
        }

        [HttpPost]
        public async Task<IHttpActionResult> UserLogin([FromBody] UserMaster user)
        {
            if (!ModelState.IsValid)
                return BadRequest("Username or password is invalid. Try again");
            try
            {
                using (_codeVortoService = new CodeVortoService())
                {
                    string password = user.Password;
                    string encrptedPass = ClsUserMaster.EncryptPassword(password);
                    /*
                    string decrptedPass = ClsUserMaster.DecryptPassword(encrptedPass);
                    string d = decrptedPass;
                    string e = encrptedPass;
                    */
                    Expression<Func<UserMaster, bool>> expression = e => e.UserName == user.UserName &&
                                                                         e.Password == encrptedPass;
                    var userMaster = await _codeVortoService.UserMasterRepository
                                .GetItem<UserMaster>(expression, 1);
                    if (userMaster == null) return BadRequest("User not found");
                  
                    var userDetails = userMaster.UserDetails.First();
                    return Ok(userDetails);
                }
            }
            catch (Exception exception)
            {
                return InternalServerError(exception);
            }
        }
    }
}
