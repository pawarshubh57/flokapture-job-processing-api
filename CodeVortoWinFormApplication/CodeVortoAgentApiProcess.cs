using System;
using System.Configuration;
using System.Diagnostics;

namespace CodeVortoWinFormApplication
{
    public static class CodeVortoAgentApiProcess
    {
        private static readonly string ApiExePath = ConfigurationManager.AppSettings["CodeVortoAgentApiExePath"];
        private static Process _agentApiProcess = new Process();

        public static void StartAgentApi()
        {
            var processStartInfo =
                new ProcessStartInfo(ApiExePath)
                {
                    WindowStyle = ProcessWindowStyle.Minimized,
                    CreateNoWindow = true
                };
            _agentApiProcess = new Process {StartInfo = processStartInfo};
            _agentApiProcess.Start();
        }

        public static void StopAgentApiProcess()
        {
            try
            {
                if (_agentApiProcess.HasExited) return;
                _agentApiProcess.Kill();
                _agentApiProcess.WaitForExit();
                _agentApiProcess.CloseMainWindow();
            }
            catch (InvalidOperationException)
            {
                // _agentApiProcess.CloseMainWindow();
                _agentApiProcess.Kill();
                _agentApiProcess.WaitForExit();
            }
        }
    }
}