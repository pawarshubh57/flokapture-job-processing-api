using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class StatementReferenceController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public StatementReferenceController()
        {
        }

        public StatementReferenceController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        public IHttpActionResult GetAll()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                Expression<Func<StatementReferenceMaster, bool>> expression = master => master.BaseCommandId == 10;
                //Predicate<StatementReferenceMaster> predicate = p => p.BaseCommandId == 10;
                var allData = _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(expression);
                return Ok(allData);
            }
        }

        public async Task<IHttpActionResult> Get(int bCommandId1, int bCommandId2)
        {
            Predicate<StatementReferenceMaster> predicate = p =>
                p.BaseCommandId == bCommandId1 ||
                p.BaseCommandId == bCommandId2;

            using (_codeVortoService = new CodeVortoService())
            {
                var statementReference =
                    await _codeVortoService.StatementReferenceMasterRepository.GetAllItems(predicate);
                NameValueData lstNameValueData = new NameValueData();
                List<NameValue> lstAllClasses = new List<NameValue>();
                List<NameValue> lstAllMethods = new List<NameValue>();
                var statementReferenceMasters = statementReference as StatementReferenceMaster[] ??
                                                statementReference.ToArray();
                var allClasses = (from stmt in statementReferenceMasters
                    where stmt.BaseCommandId == bCommandId2
                    select stmt).ToList();
                var allMethods = (from stmt in statementReferenceMasters
                    where stmt.BaseCommandId == bCommandId1
                    select stmt).ToList();
                lstNameValueData.Classes = new List<NameValue>();
                lstAllClasses.AddRange(
                    allClasses.Select(master =>
                        new NameValue
                        {
                            Value = master.StatementId,
                            Name = master.ClassNameDeclared
                        }));
                lstNameValueData.Classes.AddRange(lstAllClasses);
                lstNameValueData.Methods = new List<NameValue>();
                lstAllMethods.AddRange(
                    allMethods.Select(master =>
                        new NameValue
                        {
                            Value = master.StatementId,
                            Name = master.MethodName
                        }));
                lstNameValueData.Methods.AddRange(lstAllMethods);
                return Ok(lstNameValueData);
            }
        }
    }

    public class NameValueData
    {
        public List<NameValue> Classes { get; set; }

        public List<NameValue> Methods { get; set; }
    }
}