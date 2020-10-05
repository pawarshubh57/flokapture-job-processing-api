using System.Reflection;
using System.Text;
using System.Web.Http;
using System.Web.Http.Controllers;
using System.Web.Http.Filters;
using log4net;

namespace CodeVortoJobQueueProcessingApi.ActionFilters
{
    public class LogActionFilter
    {
        private static readonly ILog Log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        public void  OnActionExecuting(HttpActionContext filterContext)
        {
            var routeData = filterContext.RequestContext.RouteData;
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("==================================================================================================");
            stringBuilder.AppendLine("Controller: " + routeData.Values["controller"]);
            stringBuilder.AppendLine("Action: " + routeData.Values["action"]);
            stringBuilder.AppendLine("Request Path: " + filterContext.Request.RequestUri.PathAndQuery);
            stringBuilder.AppendLine("Request Uri: " + ((ApiController)filterContext.ControllerContext.Controller).Request.RequestUri);
            stringBuilder.AppendLine("==================================================================================================");

            Log.Debug(stringBuilder);
            Log.Info(filterContext.RequestContext);
        }

        public void OnActionExecuted(HttpActionExecutedContext filterContext)
        {
            var routeData = filterContext.ActionContext.RequestContext.RouteData;
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("==================================================================================================");
            stringBuilder.AppendLine("Controller: " + routeData.Values["controller"]);
            stringBuilder.AppendLine("Action: " + routeData.Values["action"]);
            stringBuilder.AppendLine("Request Path: " + filterContext.Request.RequestUri.PathAndQuery);
            stringBuilder.AppendLine("Request Uri: " +
                                     ((ApiController) filterContext.ActionContext.ControllerContext.Controller).Request
                                         .RequestUri);
            stringBuilder.AppendLine("==================================================================================================");
            Log.Debug(stringBuilder);
            Log.Info(filterContext.Response);
        }
    }
}