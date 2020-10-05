using System;
using System.ComponentModel;
using System.Threading;
using System.Timers;
using Timer = System.Timers.Timer;

namespace FloKaptureJobQueueService.ServiceHelpers
{
    public class ServiceTimerHelper
    {
        public BackgroundWorker BackGroundWorker = new BackgroundWorker();
        public Timer GlobalTimer = new Timer();

        public ServiceTimerHelper()
        {
            GlobalTimer.Enabled = true; // Interval = 7.2e+7  // -- 20 Hours
            GlobalTimer.Interval = 30000;
            GlobalTimer.Elapsed += GlobalTimer_Elapsed;
            GlobalTimer.Start();
            // StartBackGroundWorker();
        }

        private static void BackGroundWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            Console.WriteLine(e.ProgressPercentage);
        }

        private void BackGroundWorker_DoWork(object sender, DoWorkEventArgs e)
        {
            if (BackGroundWorker.IsBusy) return;

            for (int i = 0; i < 1500; i++)
            {
                Console.WriteLine(@"Counter: " + i);
                Thread.Sleep(1000);
            }
        }

        private void GlobalTimer_Elapsed(object sender, ElapsedEventArgs e)
        {
            StartBackGroundWorker();
        }

        public void StartBackGroundWorker()
        {
            BackGroundWorker.WorkerReportsProgress = true;
            BackGroundWorker.WorkerSupportsCancellation = true;
            BackGroundWorker.DoWork += BackGroundWorker_DoWork;
            BackGroundWorker.ProgressChanged += BackGroundWorker_ProgressChanged;
            BackGroundWorker.RunWorkerAsync();
        }
    }
}
