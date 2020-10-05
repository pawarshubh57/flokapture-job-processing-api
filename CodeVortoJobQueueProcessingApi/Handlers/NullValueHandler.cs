using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Formatting;
using System.Threading;
using System.Threading.Tasks;

namespace CodeVortoJobQueueProcessingApi.Handlers
{
    public class NullValueHandler : DelegatingHandler
    {
        protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request,
            CancellationToken cancellationToken)
        {
            var response = await base.SendAsync(request, cancellationToken).ConfigureAwait(false);
            if (response.Content == null)
            {
                response.Content = new ObjectContent<List<EmptyList>>(new List<EmptyList>(),
                    new JsonMediaTypeFormatter());
            }
            else if (response.Content is ObjectContent)
            {
                var objectContent = (ObjectContent)response.Content;
                if (objectContent.Value == null)
                {
                    response.Content = new ObjectContent<List<EmptyList>>(new List<EmptyList>(),
                        new JsonMediaTypeFormatter());
                }
            }
            return response;
        }
    }

    public class EmptyList
    {
    }
}
