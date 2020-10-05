using System;
using System.Configuration;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Web.Http.SelfHost;
using CodeVortoJobQueueProcessingApi;
using log4net.Config;

[assembly: XmlConfigurator(Watch = true)]

namespace CodeVortoJobProcessingApp
{
    internal class Program
    {
        private static readonly Func<Task> TaskFunction = StartApi;

        private const int SwMinimize = 6;

        [DllImport("Kernel32.dll", CallingConvention = CallingConvention.StdCall, SetLastError = true)]
        private static extern IntPtr GetConsoleWindow();

        [DllImport("User32.dll", CallingConvention = CallingConvention.StdCall, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool ShowWindow([In] IntPtr hWnd, [In] int nCmdShow);

        private static void Main()
        {
            try
            {
                Console.WriteLine("===========================================================");
                switch (IntPtr.Size)
                {
                    case 8:
                        Console.WriteLine("Running on 64 bit operating system");
                        break;
                    case 4:
                        Console.WriteLine("Running on 32 bit operating system");
                        break;
                }
                Console.WriteLine("===========================================================");
                Console.WriteLine();

                Task.Factory.StartNew(async () => await Task.Run(TaskFunction).ConfigureAwait(false));
                Console.ReadLine();
            }
            catch (ObjectDisposedException objectDisposedException)
            {
                Console.WriteLine(objectDisposedException.Message);
                Console.WriteLine(Environment.NewLine + "Press Enter to quit.");
                Console.ReadLine();
            }
            catch (AggregateException aggregateException)
            {
                if (aggregateException.InnerException != null)
                    Console.WriteLine(aggregateException.InnerException.Message);
                Console.WriteLine(Environment.NewLine + "Press Enter to quit.");
                Console.ReadLine();
            }
        }

        private static Task StartApi()
        {
            var apiBaseAddress = ConfigurationManager.AppSettings["ApiBaseAddress"];
            var httpSelfHostConfiguration = new HttpSelfHostConfiguration(apiBaseAddress);
            var selfHostApiTask = WebApiConfig.Register(httpSelfHostConfiguration);

            var hWndConsole = GetConsoleWindow();
            ShowWindow(hWndConsole, SwMinimize);

            return selfHostApiTask;
        }
    }
}