using System;
using System.Windows.Forms;


namespace CodeVortoWinFormApplication
{
    internal static class Program
    {
        [STAThread]
        private static void Main()
        {
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new FloKaptureProcessingAgent());
                // CodeVortoAgentApiProcess.StopAgentApiProcess();
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
                CodeVortoAgentApiProcess.StopAgentApiProcess();
            }
        }
    }
}