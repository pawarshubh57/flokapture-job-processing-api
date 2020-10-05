using System;
using System.Configuration;
using System.Data;
using System.Data.OleDb;
using System.Data.SqlClient;
using System.Globalization;
using System.Runtime.InteropServices;

/*
 * Created by: Yogesh Sonawane
 * Created date:  10 Jan. 2014
 * Updated on: 23 Aug. 2015
 * Purpose: Communication channel between Sql Server Database and Web Api
 * NjSoft Inc. All rights reserved.
*/

namespace DataAccessLayer
{
    public sealed class SqlDbConnectionBaseClass : IDisposable
    {
        private SqlConnection _connection;
        private DataSet _dataSet;
        private DataTable _dtTable;
        private OleDbConnection _oleDbConn = new OleDbConnection();
        private int _result;
        private SqlDataAdapter _sqlDataAdapter;
        private SqlTransaction _trx;

        public SqlDbConnectionBaseClass()
        {
            _connection = new SqlConnection();
        }

        public string ConString { get; set; }

        public void Dispose()
        {
            GC.SuppressFinalize(this);
        }

        private string GetSqlConnectionString()
        {
            var constr = ConfigurationManager.AppSettings["ConnectionString"].ToString(CultureInfo.InvariantCulture);
            /*
            if (constr == "NotDefined")
            {
                // Try to find out connection string in XML file which is located at bin/Debug or bin/Release directory
                var filePath = Application.StartupPath + "\\" +
                               ConfigurationManager.AppSettings["ConnectionStringXml"].ToString(
                                   CultureInfo.InvariantCulture);
                if (File.Exists(path: filePath))
                {
                    var xmlConnStringdoc = new XmlDocument();
                    using (var connString = new FileStream(filePath, FileMode.Open, FileAccess.Read))
                    {
                        xmlConnStringdoc.Load(connString);
                        var xmlConnStringnode = xmlConnStringdoc.GetElementsByTagName("Values");

                        foreach (XmlNode xmlnode in xmlConnStringnode)
                        {
                            constr = xmlnode.InnerText;
                        }
                    }
                }
            }
            */
            return constr;
        }

        private SqlConnection OpenConnection()
        {
            if (_connection == null)
                _connection = new SqlConnection();
            if (_connection.State == ConnectionState.Closed)
            {
                _connection.ConnectionString = GetSqlConnectionString();
                _connection.Open();
            }
            return _connection;
        }

        private OleDbConnection OpenConnectionOleDb()
        {
            if (_oleDbConn.State == ConnectionState.Closed)
            {
                _oleDbConn.ConnectionString =
                    ConfigurationManager.ConnectionStrings["ConnectionStringOleDb"].ToString();
                _oleDbConn.Open();
            }
            return _oleDbConn;
        }

        public DataSet ExecuteStoreProcedureWithParameter(string[] parameterArray, string[] valueArray)
        {
            using (var dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    var objSqlTransaction = _connection.BeginTransaction();
                    using (var command = new SqlCommand("GetPanAndDomainDetails", _connection, objSqlTransaction)
                    {
                        CommandType = CommandType.StoredProcedure
                    })
                    {
                        command.Parameters.AddWithValue("@panDescription",
                            valueArray[0].ToString(CultureInfo.InvariantCulture));
                        command.Parameters.AddWithValue("@domainDescription",
                            valueArray[1].ToString(CultureInfo.InvariantCulture));

                        using (var da = new SqlDataAdapter(command))
                        {
                            da.Fill(dataSet);
                        }
                    }
                    objSqlTransaction.Commit();
                }
                return dataSet;
            }
        }

        public DataTable ExecuteDatasetSp(string spName, SqlParameter[] paramArray, string opt)
        {
            try
            {
                _connection = null;
                using (_connection = new SqlConnection(opt))
                {
                    _connection.Open();
                    using (var sqlCommand = new SqlCommand
                    {
                        Connection = _connection,
                        CommandText = " Exec (' use " + paramArray[1].Value + "  Select * from " + paramArray[0].Value +
                                      " ')"
                    })
                    {
                        DataTable schemaTable;
                        using (IDataReader dr2 = sqlCommand.ExecuteReader(CommandBehavior.KeyInfo))
                        {
                            schemaTable = dr2.GetSchemaTable();
                            dr2.Close();
                        }
                        return schemaTable;
                    }
                }
            }
            finally
            {
                if (_connection != null) _connection.Close();
            }
        }

        public DataTable ExecuteProcedureForSelect(string spName)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (
                        var sqlCommand = new SqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure}
                        )
                    {
                        using (var reader = sqlCommand.ExecuteReader())
                        {
                            _dtTable.Load(reader);
                            return _dtTable;
                        }
                    }
                }
            }
        }

        public DataTable ExecuteProcedureForSelect(string spName, SqlParameter[] param, string optional)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var cmd = new SqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure})
                    {
                        foreach (var p in param)
                        {
                            cmd.Parameters.Add(p);
                        }
                        using (var reader = cmd.ExecuteReader())
                        {
                            _dtTable.Load(reader);
                            _connection.Close();
                            return _dtTable;
                        }
                    }
                }
            }
        }

        public DataSet ExecuteProcedureForSelect(string spName, SqlParameter[] param)
        {
            using (_dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    using (var cmd = new SqlCommand
                    {
                        Connection = _connection,
                        CommandType = CommandType.StoredProcedure,
                        CommandText = spName
                    })
                    {
                        if (param != null)
                        {
                            foreach (var p in param)
                            {
                                cmd.Parameters.Add(p);
                            }
                        }
                        using (var da = new SqlDataAdapter(cmd))
                        {
                            da.Fill(_dataSet);
                            _connection.Close();
                            return _dataSet;
                        }
                    }
                }
            }
        }

        public void ExecuteInsertSp(string spName, SqlParameter[] paramArray)
        {
            using (_connection = OpenConnection())
            {
                using (var cmd = new SqlCommand
                {
                    Connection = _connection,
                    CommandType = CommandType.StoredProcedure,
                    CommandText = spName
                })
                {
                    foreach (var p in paramArray)
                    {
                        cmd.Parameters.Add(p);
                    }
                    cmd.ExecuteNonQuery();
                }
            }
        }

        //required for clsAutoDiscovery(added by sonal)
        public int ExecuteNonQueryAgent(string sqlQuery, int option)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var sqlCommand = new SqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        var retVal = sqlCommand.ExecuteNonQuery();
                        return retVal;
                    }
                }
            }
        }

        public void UpdateStatus(string strGuid, bool boolStatus)
        {
            try
            {
                var log = boolStatus ? "Status updated for agent as active" : "Status updated for agent as inactive";
                int boolFlag;
                if (!string.IsNullOrEmpty(strGuid))
                {
                    var strQuery = " update tbl_agentmaster set agentstatus='" + boolStatus +
                                   "' where str_GUIDAssigned='" +
                                   strGuid.Trim() + "'";

                    boolFlag = ExecuteNonQueryAgent(strQuery, 1);
                }
                else
                {
                    boolFlag = 0;
                    log = "Status updated for agent as inactive";
                }

                if (boolFlag != 0)
                {
                    InsertLogDetails(log, "UpdateStatus", "Success");
                }
                else
                {
                    InsertLogDetails(log, "UpdateStatus", "Success");
                }
            }
            catch (Exception ex)
            {
                InsertExceptionDetails(ex.StackTrace, "UpdateStatus", ex.Message);
            }
        }

        /// <summary>
        ///     InsertLogDetails(int int_FileId, string str_LogDetails, string str_ComponentName, string str_LogType) accepts 4
        ///     arguments.
        ///     It writes log into database table.
        /// </summary>
        /// <param name="strLogDetails">string</param>
        /// <param name="strMethodName"></param>
        /// <param name="strLogType">string</param>
        public void InsertLogDetails(string strLogDetails, string strMethodName, string strLogType)
        {
            try
            {
                var strQuery = "INSERT INTO tbl_LogDetails ( "
                               + "  LogDetails, MethodName, LogType, CreatedOn, UpdatedOn "
                               + ") VALUES ( "
                               + " '" + strLogDetails + "', '" + strMethodName + "', '" + strLogType + "', '"
                               + DateTime.Now + "', '" + DateTime.Now
                               + "') ";

                ExecuteNonQuery(strQuery);
            }
            catch (Exception ex)
            {
                /*To insert exception into table. */
                InsertExceptionDetails(ex.Message.ToString(CultureInfo.InvariantCulture), "InsertLogDetails",
                    ex.StackTrace);
            }
            finally
            {
                GC.Collect();
            }
        }

        public void InsertExceptionDetails(string strExceptionStackTrace, string strMethodName,
            string strExceptionMessage)
        {
            var strQuery = " INSERT INTO tbl_ExceptionDetails ( "
                           + " ExceptionStackTrace, MethodName, ExceptionMessage, CreatedOn "
                           + ") VALUES ( "
                           + " '" + strExceptionStackTrace + "', '" + strMethodName + "', '" + strExceptionMessage +
                           "', '" + DateTime.Now
                           + "') ";
            ExecuteNonQuery(strQuery);
        }

        ~SqlDbConnectionBaseClass()
        {
            Dispose();
        }

        #region Query Analyzer Component...

        public DataTable ExecuteNonQueryOleDb(string sqlQuery, string entity)
        {
            if (entity == null) throw new ArgumentNullException("entity");
            using (_dtTable = new DataTable())
            {
                using (_oleDbConn = OpenConnectionOleDb())
                {
                    _dtTable = _oleDbConn.GetOleDbSchemaTable(OleDbSchemaGuid.Primary_Keys,
                        new object[] {null, null, entity});
                    _oleDbConn.Close();
                    return _dtTable;
                }
            }
        }

        /// <summary>
        ///     for insert,update and delete execution command.
        /// </summary>
        /// <param name="cmd">conatins the sqlce command</param>
        /// <param name="strConnectionString"></param>
        /// <returns></returns>
        public int ExecuteCmd(SqlCommand cmd, [Optional] string strConnectionString)
        {
            try
            {
                ConString = string.IsNullOrEmpty(strConnectionString) ? GetSqlConnectionString() : strConnectionString;
                _connection = new SqlConnection(ConString);
                if (_connection.State == ConnectionState.Closed)
                {
                    _connection.Open();
                }
                cmd.Connection = _connection;
                _trx = _connection.BeginTransaction();
                cmd.Transaction = _trx;
                _result = cmd.ExecuteNonQuery();
                _trx.Commit();
                _connection.Close();
            }
            catch (Exception ex)
            {
                _trx.Rollback();
                if (ex.StackTrace != null)
                {
                }
            }
            finally
            {
                _connection = null;
            }
            return _result;
        }

        /// <summary>
        ///     For Retrieve scalar value
        /// </summary>
        /// <param name="cmd">conatins the sqlce command</param>
        /// <param name="strConnectionString"></param>
        /// <returns></returns>
        public string ExecuteScalar(SqlCommand cmd, [Optional] string strConnectionString)
        {
            var resultScalar = "";
            ConString = string.Empty;
            try
            {
                ConString = string.IsNullOrEmpty(strConnectionString) ? GetSqlConnectionString() : strConnectionString;
                _connection = new SqlConnection(ConString);
                if (_connection.State == ConnectionState.Closed)
                {
                    _connection.Open();
                    cmd.Connection = _connection;
                    _trx = _connection.BeginTransaction();
                    cmd.Transaction = _trx;
                    resultScalar = Convert.ToString(cmd.ExecuteScalar());
                    _trx.Commit();
                    _connection.Close();
                }
            }
            catch (Exception ex)
            {
                _trx.Rollback();
                if (ex.StackTrace != null)
                {
                }
            }
            finally
            {
                _connection = null;
            }
            return resultScalar;
        }

        /// <summary>
        ///     for getting the datatable value.
        /// </summary>
        /// <param name="cmd">conatins the sqlce command</param>
        /// <param name="strConnectionString"></param>
        /// <returns></returns>
        public DataTable ExecuteDatatable(SqlCommand cmd, [Optional] string strConnectionString)
        {
            try
            {
                ConString = string.IsNullOrEmpty(strConnectionString) ? GetSqlConnectionString() : strConnectionString;
                //ConString = GetConnectionString();
                _connection = new SqlConnection(ConString);
                if (_connection.State == ConnectionState.Closed)
                {
                    _connection.Open();
                }
                cmd.Connection = _connection;
                _sqlDataAdapter = new SqlDataAdapter(cmd);

                _dataSet = new DataSet();
                _sqlDataAdapter.Fill(_dataSet);
                _connection.Close();

                if (_dataSet.Tables.Count == 0 || _dataSet.Tables[0].Rows.Count == 0)
                {
                    _dtTable = null;
                }
                else
                {
                    _dtTable = _dataSet.Tables[0];
                }
            }
            catch (Exception ex)
            {
                if (ex.StackTrace != null)
                {
                }
            }
            finally
            {
                _connection = null;
                _sqlDataAdapter = null;
            }
            return _dtTable;
        }

        /// <summary>
        ///     for getting the dataset value.
        /// </summary>
        /// <param name="cmd">conatins the sqlce command</param>
        /// <param name="strConnectionString"></param>
        /// <returns></returns>
        public DataSet ExecuteDataset(SqlCommand cmd, [Optional] string strConnectionString)
        {
            try
            {
                ConString = string.IsNullOrEmpty(strConnectionString) ? GetSqlConnectionString() : strConnectionString;
                using (_connection = new SqlConnection(ConString))
                {
                    if (_connection.State == ConnectionState.Closed)
                    {
                        _connection.Open();
                    }
                    cmd.Connection = _connection;
                    using (_sqlDataAdapter = new SqlDataAdapter(cmd))
                    {
                        using (_dataSet = new DataSet())
                        {
                            _sqlDataAdapter.Fill(_dataSet);
                            if (_dataSet.Tables.Count == 0 || _dataSet.Tables[0].Rows.Count == 0)
                            {
                                _dataSet = null;
                            }
                            else
                            {
                                return _dataSet;
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                if (ex.StackTrace != null)
                {
                }
            }
            finally
            {
                _connection = null;
                _sqlDataAdapter = null;
            }
            return _dataSet;
        }


        public DataTable ExecuteNonQuery(string sqlQuery)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var selectCommand = new SqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        using (var adp = new SqlDataAdapter(selectCommand))
                        {
                            adp.Fill(_dtTable);
                            return _dtTable;
                        }
                    }
                }
            }
        }

        public DataTable ExecuteNonQueryTaskStatus(string sqlQuery)
        {
            using (var dtStatus = new DataTable())
            {
                using (var sqlConnection = new SqlConnection {ConnectionString = GetSqlConnectionString()})
                {
                    if (sqlConnection.State == ConnectionState.Closed)
                        sqlConnection.Open();
                    using (var selectCommand = new SqlCommand {CommandText = sqlQuery, Connection = sqlConnection})
                    {
                        using (var adp = new SqlDataAdapter(selectCommand))
                        {
                            adp.Fill(dtStatus);
                            return dtStatus;
                        }
                    }
                }
            }
        }

        public int ExecuteNonQuery(string sqlQuery, int optVar)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var sqlCommand = new SqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        var retVal = sqlCommand.ExecuteNonQuery();
                        return retVal;
                    }
                }
            }
        }

        public DataSet ExecuteNonQuery(string sqlQuery, string opt)
        {
            using (_dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    using (var sqlCommand = new SqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        using (var adp = new SqlDataAdapter(sqlCommand))
                        {
                            adp.Fill(_dataSet);
                            return _dataSet;
                        }
                    }
                }
            }
        }

        #endregion
    }
}