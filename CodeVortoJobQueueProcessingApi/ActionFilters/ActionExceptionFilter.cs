using System;
using System.Reflection;
using System.Text;
using System.Web.Http;
using System.Web.Http.Filters;
using log4net;

namespace CodeVortoJobQueueProcessingApi.ActionFilters
{
    public class ActionExceptionFilter : ExceptionFilterAttribute
    {
        private static readonly ILog Log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        public override void OnException(HttpActionExecutedContext actionExecutedContext)
        {
            var routeData = actionExecutedContext.ActionContext.RequestContext.RouteData;
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine(
                "==================================================================================================");
            stringBuilder.AppendLine("Controller: " + routeData.Values["controller"]);
            stringBuilder.AppendLine("Action: " + routeData.Values["action"]);
            stringBuilder.AppendLine("Exception Message: " + actionExecutedContext.Exception.Message);
            stringBuilder.AppendLine("Source Exception: " + actionExecutedContext.Exception.Source);

            string stackTrace = actionExecutedContext.Exception.StackTrace;

            int index = stackTrace.IndexOf("from", StringComparison.Ordinal);
            stackTrace = stackTrace.Substring(0, index);
            var abc = stackTrace;
            stringBuilder.AppendLine("Inner Exception:" + abc);
            stringBuilder.AppendLine("Request Path: " + actionExecutedContext.Request.RequestUri.PathAndQuery);
            stringBuilder.AppendLine("Request Uri: " +
                                     ((ApiController)actionExecutedContext.ActionContext.ControllerContext.Controller)
                                         .Request.RequestUri);
            stringBuilder.AppendLine("==================================================================================================");
            Log.Debug(stringBuilder);
        }
    }
}