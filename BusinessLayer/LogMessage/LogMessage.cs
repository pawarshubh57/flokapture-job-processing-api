using System;
using System.Reflection;
using System.Text;
using log4net;

namespace BusinessLayer.LogMessage
{
    public static class LogMessage
    {
        private static readonly ILog Log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);
        public static void WriteLogMessage(StringBuilder stringBuilder)
        {
            Log.Info(stringBuilder);
        }
        public static void WriteExceptionLogMessage(Exception exception)
        {
            var stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("===========================================================================");
            stringBuilder.AppendLine("Exception: " + exception.Message);
            stringBuilder.AppendLine("Inner Exception: " + exception.InnerException);
            stringBuilder.AppendLine("Exception Source: " + exception.Source);
            stringBuilder.AppendLine("StackTrace: " + exception.StackTrace);
            stringBuilder.AppendLine("===========================================================================");

            Log.Debug(stringBuilder);
        }
        public static void LogDebug(StringBuilder stringBuilder)
        {
            Log.Debug(stringBuilder);
        }
        public static void LogFatalLevelMessage(string message)
        {
            Log.Fatal(message);
        }
        public static void LogFatalLevelMessage(StringBuilder stringBuilder, Exception exception)
        {
            Log.Fatal(stringBuilder, exception);
        }
    }
}