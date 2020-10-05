using System;
using System.Configuration;
using System.ServiceProcess;
using System.Threading.Tasks;
using System.Web.Http.SelfHost;
using CodeVortoJobQueueProcessingApi;
using FloKaptureJobQueueService.ServiceHelpers;

namespace FloKaptureJobQueueService
{
    public partial class FloKaptureJobQueueService : ServiceBase
    {
        private static readonly Func<Task> TaskFunction = StartApi;

        private static Task StartApi()
        {
            var apiBaseAddress = ConfigurationManager.AppSettings["ApiBaseAddress"];
            var httpSelfHostConfiguration = new HttpSelfHostConfiguration(apiBaseAddress);
            var selfHostApiTask = WebApiConfig.Register(httpSelfHostConfiguration);
            return selfHostApiTask;
        }

        public void StopService()
        {
            OnStop();
        }

        public void StartService()
        {
            try
            {
                Console.WriteLine(Environment.NewLine+ @"==========================================================="+ Environment.NewLine);

                switch (IntPtr.Size)
                {
                    case 8:
                        Console.WriteLine(@"Running on 64 bit operating system");
                        break;
                    case 4:
                        Console.WriteLine(@"Running on 32 bit operating system");
                        break;
                }
                Console.WriteLine(Environment.NewLine + @"===========================================================");
                Console.WriteLine();

                Task.Factory.StartNew(async () => await Task.Run(TaskFunction).ConfigureAwait(false));

                var serviceTimer = new ServiceTimerHelper();
                serviceTimer.StartBackGroundWorker();

                Console.ReadLine();
            }
            catch (ObjectDisposedException objectDisposedException)
            {
                Console.WriteLine(objectDisposedException.Message);
                Console.WriteLine(Environment.NewLine + @"Press Enter to quit.");
                Console.ReadLine();
            }
            catch (AggregateException aggregateException)
            {
                if (aggregateException.InnerException != null)
                    Console.WriteLine(aggregateException.InnerException.Message);
                Console.WriteLine(Environment.NewLine + @"Press Enter to quit.");
                Console.ReadLine();
            }
        }

        protected override void OnStart(string[] args)
        {
            StartService();
        }

        protected override void OnStop()
        {
            Stop();
        }
    }
}
