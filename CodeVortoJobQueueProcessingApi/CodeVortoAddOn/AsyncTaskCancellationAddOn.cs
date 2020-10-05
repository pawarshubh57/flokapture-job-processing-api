using System.Threading;

namespace CodeVortoJobQueueProcessingApi.CodeVortoAddOn
{
    public static class AsyncTaskCancellationAddOn
    {
        public static CancellationToken CancellationToken;
        public static CancellationTokenSource CancellationTokenSource = new CancellationTokenSource();

        static AsyncTaskCancellationAddOn()
        {
            CancellationToken = CancellationTokenSource.Token;
        }
    }
}