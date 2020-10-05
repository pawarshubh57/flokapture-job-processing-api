
namespace CodeVortoJobQueueProcessingApi.ControllerHelperClasses
{
    public class StatementReferenceMasterHelper
    {
        /*
        private async Task<string> DataDependancyWithCrudActivityUniverseProject()
        {
            List<string> insertList = new List<string> { "MATWRITE" };
            List<string> updateList = new List<string> { "MATREADU" };
            List<string> deleteList = new List<string> { "DELETE" };
            List<string> selectList = new List<string> { "MATREAD" };

            using (var codeVortoService = new CodeVortoService())
            {
                try
                {
                    string sqlQry = "select * from universebasicdatadictionary group by FileName";

                    var universebasicdatadictionarywithFileName =
                        await codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQry)
                            .ConfigureAwait(false);

                    string sqlQuery = "select * from universebasicdatadictionary;";
                    var universebasicdatadictionary =
                        await codeVortoService.DataDictionaryRepository.GetDataFromSqlQuery<DataDictionary>(sqlQuery);

                    var fileMaster =
                        await codeVortoService.FileMasterRepository.GetAllListItems(x => x.ProjectId == 2)
                            .ConfigureAwait(false);

                    foreach (var dataDictionary in universebasicdatadictionarywithFileName)
                    {
                        var fileId = (from f in fileMaster
                            where Path.GetFileNameWithoutExtension(f.FilePath) == dataDictionary.FileName
                            select f).ToList();

                        List<string> attribute = new List<string>();
                        var dataAttributeList =
                            (from d in universebasicdatadictionary where d.FileName == dataDictionary.FileName select d)
                                .ToList();

                        int index = -1;
                        foreach (var attList in dataAttributeList)
                        {
                            index++;
                            if (index == 0) continue;
                            string attr = attList.Description;
                            if (!string.IsNullOrEmpty(attr)) attribute.Add(attr);
                        }
                        var dataDependancy = new DataDependency
                        {
                            FileId = fileId[0].FileId,
                            Entity = dataDictionary.Description,
                            Attributes = string.Join(",", attribute),
                            ProjectId = 2
                        };
                        await codeVortoService.DataDependencyRepository.AddNewItem(dataDependancy);
                        string sqlQury =
                            "SELECT * FROM statementreferencemaster WHERE OriginalStatement" +
                            " LIKE '%" + dataDictionary.FileName + "%' AND BaseCommandId = 0 AND projectId != 2;";
                        var statemenRefMaster =
                            await
                                codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQury).ConfigureAwait(false);
                        foreach (var statementRef in statemenRefMaster)
                        {
                            statementRef.BaseCommandId = 45;
                            await codeVortoService.StatementReferenceMasterRepository.UpdateItem(statementRef);
                        }

                        #region Added into DbCrudActivityTable

                        string iR = "N";
                        string update = "N";
                        string delete = "N";
                        string select = "N";
                        var dbCurdActivity = new DbCrudActivity
                        {
                            EntityName = dataDictionary.Description,
                            SelectOrRead = select,
                            InsertOrCreate = iR,
                            Update = update,
                            Delete = delete,
                            FileId = fileId[0].FileId
                        };
                        var mainList = new List<string>();
                        mainList.AddRange(insertList);
                        mainList.AddRange(deleteList);
                        mainList.AddRange(updateList);
                        mainList.AddRange(selectList);
                        string sqlQery = "select * from statementReferencemaster where OriginalStatement like '%" +
                                         dataDictionary.FileName + "%' " +
                                         "AND ( ";
                        int i = 0;
                        foreach (var list in mainList)
                        {
                            i++;
                            sqlQery += i == 1
                                ? " OriginalStatement LIKE '%" + list + "%'"
                                : " OR OriginalStatement LIKE '%" + list + "%'";
                        }
                        sqlQery += ")";

                        var statementList =
                            await
                                codeVortoService.StatementReferenceMasterRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQery)
                                    .ConfigureAwait(false);

                        if (statementList.Any())
                        {
                            foreach (string sss in statementList.Select(s => s.OriginalStatement))
                            {
                                if (insertList.Any(c => sss.Contains(c))) iR = "Y";
                                if (updateList.Any(c => sss.Contains(c))) update = "Y";
                                if (deleteList.Any(c => sss.Contains(c))) delete = "Y";
                                if (selectList.Any(c => sss.Contains(c))) select = "Y";
                                //if (sss.Contains("MATREADU")) update = "Y";
                                //if (sss.Contains("DELETE")) delete = "Y";
                                //if (sss.Contains("MATWRITE")) iR = "Y";
                                //if (sss.Contains("MATREAD")) select = "Y";
                            }
                            dbCurdActivity = new DbCrudActivity
                            {
                                EntityName = dataDictionary.Description,
                                SelectOrRead = select,
                                InsertOrCreate = iR,
                                Update = update,
                                Delete = delete,
                                FileId = fileId[0].FileId
                            };
                        }
                        string entity = dataDictionary.Description;
                        int fileIdNew = fileId[0].FileId;
                        Expression<Func<DbCrudActivity, bool>> expression =
                            x => x.EntityName == entity && x.FileId == fileIdNew;
                        var crud =
                            await
                                codeVortoService.DbCrudActivityRepository.GetAllListItems(expression)
                                    .ConfigureAwait(false);
                        if (crud.Count != 0) continue;
                        await codeVortoService.DbCrudActivityRepository.AddNewItem(dbCurdActivity);

                        #endregion
                    }
                    //return new OkResult(universebasicdatadictionarywithFileName);
                    return "done";
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.InnerException);
                }
            }
           
        }
        */
    }
}