using System;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Serialization;

namespace BusinessLayer.EntityRepositories
{
    public class ReferenceLoopResolver<T> : DefaultContractResolver where T : class
    {
        private readonly ClassConverter _defaultConverter = new ClassConverter();

        protected override JsonContract CreateContract(Type objectType)
        {
            var contract = base.CreateContract(objectType);
            if (objectType.IsPrimitive || objectType == typeof(DateTime) || objectType == typeof(string)
                || objectType.Name.Contains("List") || objectType == typeof(T))
            {
                contract.Converter = base.CreateContract(objectType).Converter;
            }
            else
            {
                contract.Converter = _defaultConverter;
            }
            return contract;
        }
    }



    public class ClassConverter : CustomCreationConverter<object>
    {
        public override object Create(Type objectType)
        {
            return null;
        }
    }
}