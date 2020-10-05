using System.Web.Http;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class HomeController : ApiController
    {
        [HttpGet]
        public string GetStatus()
        {
            // var stringBuilder = new StringBuilder();
            // stringBuilder.AppendLine("Testing log message from Yogesh's machine...");
            // LogMessage.WriteLogMessage(stringBuilder);
            return "API is up and running!";
        }
    }
}