using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json.Serialization;

namespace CodeVortoJobQueueProcessingApi.Formatters
{
    internal class DictionaryAsArrayResolver : DefaultContractResolver
    {
        protected override JsonContract CreateContract(Type objectType)
        {
            if (objectType.GetInterfaces().Any(i => i == typeof (IDictionary) ||
                                                    (i.IsGenericType &&
                                                     i.GetGenericTypeDefinition() == typeof (IDictionary<,>))))
            {
                return CreateArrayContract(objectType);
            }

            return base.CreateContract(objectType);
        }
    }
}