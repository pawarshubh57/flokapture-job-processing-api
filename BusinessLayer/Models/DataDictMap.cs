using CsvHelper.Configuration;

namespace BusinessLayer.Models
{
    public sealed class DataDictMap : ClassMap<DataDict>
    {
        public DataDictMap()
        {
            Map(m => m.FileName).Name("\"FILE NAME\"", "\"FILE_NAME\"", "\"FILE\"");
            Map(m => m.FieldNo).Name("\"FIELD NO\"", "\"FIELD_NO\"", "\"FIELD\"");
            Map(m => m.Description).Name("\"DESCRIPTION\"");
            Map(m => m.FieldLabel).Name("\"FIELD LABEL\"", "\"FIELD_LABEL\"");
            Map(m => m.RptFieldLength).Name("\"RPT FIELD LENGTH\"", "\"RPT_FIELD_LENGTH\"");
            Map(m => m.TypeOfData).Name("\"TYPE OF DATA\"", "\"TYPE_OF_DATA\"");
            Map(m => m.SingleArray).Name("\"SINGLE/ ARRAY\"", "\"SINGLE_ARRAY\"");
            Map(m => m.DateOfCapture).Name("\"DATE OF CAPTURE\"", "\"DATE_OF_CAPTURE\"");
        }
    }
}
