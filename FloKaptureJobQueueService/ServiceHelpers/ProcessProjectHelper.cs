using System;
using System.Configuration;
using System.Net.Http;
using System.Threading.Tasks;
using BusinessLayer.DbEntities;

namespace FloKaptureJobQueueService.ServiceHelpers
{
    public class ProcessProjectHelper : HttpClient
    {
        private string _baseAddress;

        private string ApiBaseAddress
        {
            get
            {
                _baseAddress = ConfigurationManager.AppSettings["ApiBaseAddress"];
                return _baseAddress;
            }
        }

        public Uri Uri
        {
            get { return !string.IsNullOrEmpty(ApiBaseAddress) ? new Uri(ApiBaseAddress) : new Uri(_baseAddress); }
        }

        public async Task<string> StartProjectProcessingAsync(ProjectMaster projectMaster)
        {
            using (var httpClient = new HttpClient())
            {
                httpClient.BaseAddress = Uri;

                httpClient.DefaultRequestHeaders.Accept.Clear();
                try
                {
                    var response = await httpClient.GetAsync(Uri).ConfigureAwait(true);
                    if (response.IsSuccessStatusCode)
                    {
                        await response.Content.ReadAsStringAsync().ConfigureAwait(true);
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                    return exception.Message;
                }

                string status = string.Format("Project {0} has been Processed Successfully.", projectMaster.ProjectName);
                return status;
            }
        }
    }
}
