using System;
using System.Collections.Generic;
using System.Configuration;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;
using Newtonsoft.Json;
using RestSharp;

namespace CodeVortoWinFormApplication
{
    public abstract class RestServiceApiClient : HttpClient
    {
        private string _baseAddress;

        private new string BaseAddress
        {
            get
            {
                _baseAddress = ConfigurationManager.AppSettings["ApiBaseAddress"];
                return _baseAddress;
            }
        }

        public Uri Uri
        {
            get
            {
                return !string.IsNullOrEmpty(BaseAddress)
                    ? new Uri(BaseAddress)
                    : new Uri(_baseAddress);
            }
        }

        protected async Task<List<T>> ExecuteAsync<T>(string endPoint) where T : class, new()
        {
            using (var client = new HttpClient())
            {
                client.BaseAddress = new Uri(BaseAddress);
                client.DefaultRequestHeaders.Accept.Clear();
                try
                {
                    var response = await client.GetAsync(endPoint).ConfigureAwait(true);
                    if (response.IsSuccessStatusCode)
                    {
                        var outputString
                            = await response.Content.ReadAsStringAsync().ConfigureAwait(true);

                        var lstT =
                            JsonConvert.DeserializeObject<List<T>>(
                                outputString);
                        return lstT;
                    }
                }
                catch (Exception exception)
                {
                    Console.WriteLine(exception.Message);
                }
            }
            return new List<T>();
        }

        protected async Task<List<T>> ExecuteRestSharpClientAsync<T>(string endPoint) where T : class, new()
        {
            var client = new RestClient {BaseUrl = new Uri(BaseAddress)};
            var restRequest = new RestRequest(endPoint);

            try
            {
                var response = await client.ExecuteTaskAsync<T>(restRequest).ConfigureAwait(true);
                if (response.StatusCode == HttpStatusCode.OK)
                {
                    var outputString = response.Content;
                    var lstT = JsonConvert.DeserializeObject<List<T>>(outputString);
                    return lstT;
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }

            return new List<T>();
        }

        protected async Task<HttpResponseMessage> ExecuteProjectProcessAsync(string endPoint)
        {
            var client = new RestClient { BaseUrl = new Uri(BaseAddress) };
            var restRequest = new RestRequest(endPoint);

            try
            {
                var response = await client.ExecuteTaskAsync(restRequest).ConfigureAwait(true);
                if (response.StatusCode == HttpStatusCode.OK)
                {
                    var responseMessage = new HttpResponseMessage(HttpStatusCode.OK);
                    return responseMessage;
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
            }

            return new HttpResponseMessage(HttpStatusCode.OK);
        }

        protected async Task<int> ExecuteRestPost<T>(string endPoint, T tSource) where T : class
        {
            var client = new RestClient {BaseUrl = new Uri(BaseAddress)};
            var restRequest = new RestRequest(endPoint, Method.POST);
            restRequest.AddJsonBody(tSource);
            try
            {
                var response =
                    await Task.Factory.StartNew(() => client.ExecuteAsPost(restRequest, "POST")).ConfigureAwait(false);
                if (response.StatusCode == HttpStatusCode.OK)
                    return 1;
            }
            catch (Exception)
            {
                return -1;
            }
            return 0;
        }
    }
}