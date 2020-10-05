using System;
using System.Configuration;
using System.Globalization;
using System.Threading.Tasks;
using System.Web.Http;
using System.Web.Http.SelfHost;
using CodeVortoJobQueueProcessingApi.Formatters;
using CodeVortoJobQueueProcessingApi.Handlers;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace CodeVortoJobQueueProcessingApi
{
    public static class WebApiConfig
    {
        public static async Task Register(HttpSelfHostConfiguration httpSelfHostConfiguration)
        {
            string dateFormat = ConfigurationManager.AppSettings["DateFormat"];
            httpSelfHostConfiguration.MapHttpAttributeRoutes();
            httpSelfHostConfiguration.Routes.MapHttpRoute("DefaultApi", "api/{controller}/{action}/{id}",
                new { id = RouteParameter.Optional, action = RouteParameter.Optional });
            httpSelfHostConfiguration.Formatters.JsonFormatter.SerializerSettings.TypeNameHandling =
                TypeNameHandling.Auto;
            var formatters = httpSelfHostConfiguration.Formatters;
            var jsonFormatter = formatters.JsonFormatter;
            var settings = jsonFormatter.SerializerSettings;
            settings.Culture = new CultureInfo("en-US", true);
           
            var cultureInfo = new CultureInfo("en-US")
            {
                DateTimeFormat = new DateTimeFormatInfo
                {
                    ShortDatePattern = dateFormat
                }
            };
            settings.Converters.Add(new IsoDateTimeConverter
            {
                DateTimeFormat = dateFormat,
                Culture = cultureInfo
            });
            httpSelfHostConfiguration.Formatters.Add(jsonFormatter);
            httpSelfHostConfiguration.Formatters.JsonFormatter
                .SerializerSettings.ReferenceLoopHandling = ReferenceLoopHandling.Ignore;
            jsonFormatter.SerializerSettings.Formatting = Formatting.Indented;
            httpSelfHostConfiguration.Formatters.Add(jsonFormatter);
            httpSelfHostConfiguration.MaxReceivedMessageSize = 2147483600;
            httpSelfHostConfiguration.MessageHandlers.Add(new CorsHandler());
            httpSelfHostConfiguration.MessageHandlers.Add(new NullValueHandler());
            httpSelfHostConfiguration.Formatters.Add(new BrowserJsonFormatter());
            var server = new HttpSelfHostServer(httpSelfHostConfiguration);
            Console.WriteLine("========================================================================\n");
            Console.WriteLine(string.Concat("Opening server at: ", httpSelfHostConfiguration.BaseAddress.AbsoluteUri));
            Console.WriteLine("\n========================================================================");
            Console.WriteLine();
            Console.WriteLine("========================================================================");
            Console.WriteLine(Environment.NewLine + "floKapture Agent (Job Processing) Api Started successfully on port " +
                              httpSelfHostConfiguration.BaseAddress.Port);
            Console.WriteLine(Environment.NewLine +
                              "========================================================================");
            await server.OpenAsync().ConfigureAwait(false);
            Console.WriteLine(Environment.NewLine + "Press Enter to quit.");
            Console.WriteLine();
        }
    }
}