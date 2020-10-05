using System.Collections.Generic;
using System.Net.Http;
using System.Threading.Tasks;
using BusinessLayer.DbEntities;

namespace CodeVortoWinFormApplication
{
    public class ClsProjectMaster : RestServiceApiClient
    {
        public async Task<List<ProjectMaster>> GetAllProjects()
        {
            var result = await ExecuteAsync<ProjectMaster>("/api/ProjectMaster/Get").ConfigureAwait(true);
            return result;
        }

        public async Task<List<LanguageMaster>> GetAllLanguages()
        {
            var result = await ExecuteRestSharpClientAsync<LanguageMaster>("/api/ProjectMaster/GetAllLenguages")
                .ConfigureAwait(true);
            return result;
        }

        public async Task<List<SolutionMaster>> GetAllSolutions()
        {
            var result = await ExecuteRestSharpClientAsync<SolutionMaster>("/api/ProjectMaster/GetAllSolutions")
                .ConfigureAwait(true);
            return result;
        }

        public async Task<List<ProjectType>> GetAllProjectTypes(int solutionId)
        {
            var result = await ExecuteRestSharpClientAsync<ProjectType>
                ("/api/ProjectMaster/GetAllProjectTypes?solutionId=" + solutionId).ConfigureAwait(true);
            return result;
        }

        public async Task<int> UploadProject<T>(T tSource) where T : class
        {
            var result = await ExecuteRestPost("/api/ProjectMaster/Post", tSource).ConfigureAwait(true);
            return result;
        }

        public async Task<HttpResponseMessage> StartProjectProcessing(int projectId)
        {
            var processResult = await ExecuteProjectProcessAsync
                ("/api/UniverseBasicFinalVersionController/StartProcessUbProject?projectId=" + projectId)
                .ConfigureAwait(true);
            return processResult;
        }
    }
}