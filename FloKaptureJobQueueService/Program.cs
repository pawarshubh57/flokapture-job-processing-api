using System;
// using Topshelf;

namespace FloKaptureJobQueueService
{
    internal static class Program
    {
        private static void Main()
        {
            ConfigureService.Configure();

            /*
            var serviceBase = new FloKaptureJobQueueService();
            ServiceBase.Run(serviceBase);
            */
        }
    }

    internal static class ConfigureService
    {
        internal static void Configure()
        {
            /*
            Console.WriteLine(@"==========================================================="+Environment.NewLine);
            HostFactory.Run(configure =>
            {
                configure.Service<FloKaptureJobQueueService>(service =>
                {
                    service.ConstructUsing(s => new FloKaptureJobQueueService());
                    service.WhenStarted(s => s.StartService());
                    service.WhenStopped(s => s.StopService());
                });
                configure.RunAsLocalSystem();
            });
            Console.WriteLine(@"===========================================================" + Environment.NewLine);
            */
        }
    }
}
