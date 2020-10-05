using System;
using System.Configuration;
using System.Data;
using System.Globalization;
using System.Threading.Tasks;
using MySql.Data.MySqlClient;

/*
 * Created by: Yogesh Sonawane
 * Created date:  10 Jan. 2014
 * Updated on: 23 Aug. 2015
 * Purpose: Communication channel between MySQL Database and Web Api
 * NjSoft Inc. All rights reserved.
*/

namespace DataAccessLayer
{
    public sealed class MySqlDbConnectionBaseClass : IDisposable
    {
        private MySqlConnection _connection;
        private DataSet _dataSet;
        private DataTable _dtTable;
        private MySqlDataAdapter _sqlDataAdapter;

        public MySqlDbConnectionBaseClass()
        {
            _connection = new MySqlConnection();
        }
        public void Dispose()
        {
            GC.SuppressFinalize(this);
        }

        private string GetMySqlConnectionString()
        {
            var constr =
                ConfigurationManager.AppSettings["ConnectionStringMySql"].ToString(CultureInfo.InvariantCulture);
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

        private MySqlConnection OpenConnection()
        {
            if (_connection == null)
                _connection = new MySqlConnection();
            if (_connection.State == ConnectionState.Closed)
            {
                try
                {
                    _connection.ConnectionString = GetMySqlConnectionString();
                    _connection.Open();
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.InnerException);
                    return _connection;
                }
            }
            return _connection;
        }

        public DataSet ExecuteStoreProcedureWithParameter(string[] parameterArray, string[] valueArray)
        {
            using (var dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    var objSqlTransaction = _connection.BeginTransaction();
                    using (var command = new MySqlCommand("GetPanAndDomainDetails", _connection, objSqlTransaction)
                    {
                        CommandType = CommandType.StoredProcedure
                    })
                    {
                        command.Parameters.AddWithValue("@panDescription",
                            valueArray[0].ToString(CultureInfo.InvariantCulture));
                        command.Parameters.AddWithValue("@domainDescription",
                            valueArray[1].ToString(CultureInfo.InvariantCulture));

                        using (var da = new MySqlDataAdapter(command))
                        {
                            da.Fill(dataSet);
                        }
                    }
                    objSqlTransaction.Commit();
                }
                return dataSet;
            }
        }

        /// <summary>
        /// </summary>
        /// <param name="spName"></param>
        /// <param name="paramArray"></param>
        /// <param name="opt"></param>
        /// <returns></returns>
        public DataTable ExecuteDatasetSp(string spName, MySqlParameter[] paramArray, string opt)
        {
            try
            {
                _connection = null;
                using (_connection = new MySqlConnection(opt))
                {
                    _connection.Open();
                    using (var sqlCommand = new MySqlCommand
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
                        var sqlCommand = new MySqlCommand(spName, _connection)
                        {
                            CommandType = CommandType.StoredProcedure
                        }
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

        public async Task<DataTable> ExecuteProcedureForSelect(string spName, MySqlParameter[] param,
            string optional)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var cmd = new MySqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure})
                    {
                        foreach (var p in param)
                        {
                            cmd.Parameters.AddWithValue(p.ParameterName, p.Value);
                        }
                        using (_sqlDataAdapter = new MySqlDataAdapter(cmd))
                        {
                            await _sqlDataAdapter.FillAsync(_dtTable);
                            return _dtTable;
                        }
                    }
                }
            }
        }

        public async Task<DataSet> ExecuteProcedureForSelect(string spName, MySqlParameter[] param)
        {
            using (_dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    using (var cmd = new MySqlCommand
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
                        using (var da = new MySqlDataAdapter(cmd))
                        {
                            await da.FillAsync(_dataSet);
                            _connection.Close();
                            return _dataSet;
                        }
                    }
                }
            }
        }

        public async Task<int> ExecuteInsertStoreProcedure(string spName, MySqlParameter[] paramArray)
        {
            var retVal = 0;
            using (_connection = OpenConnection())
            {
                using (var cmd = new MySqlCommand
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
                    using (var reader = await cmd.ExecuteReaderAsync())
                    {
                        if (reader.Read())
                        {
                            retVal = reader.GetInt32(0);
                        }
                    }
                }
            }
            return retVal;
        }

        ~MySqlDbConnectionBaseClass()
        {
            Dispose();
        }

        public async Task<DataTable> ExecuteNonQueryAsync(string sqlQuery)
        {
            await Task.Run(async () =>
            {
                using (_dtTable = new DataTable())
                {
                    using (_connection = OpenConnection())
                    {
                        using (var selectCommand = new MySqlCommand {CommandText = sqlQuery, Connection = _connection})
                        {
                            using (var adp = new MySqlDataAdapter(selectCommand))
                            {
                                await adp.FillAsync(_dtTable);
                            }
                        }
                    }
                }
            });
            return _dtTable;
        }

        public DataTable ExecuteNonQueryTaskStatus(string sqlQuery)
        {
            using (var dtStatus = new DataTable())
            {
                using (var mySqlConnection = new MySqlConnection {ConnectionString = GetMySqlConnectionString()})
                {
                    if (mySqlConnection.State == ConnectionState.Closed)
                        mySqlConnection.Open();
                    using (var selectCommand = new MySqlCommand {CommandText = sqlQuery, Connection = mySqlConnection})
                    {
                        using (var adp = new MySqlDataAdapter(selectCommand))
                        {
                            adp.Fill(dtStatus);
                            return dtStatus;
                        }
                    }
                }
            }
        }

        public async Task<int> ExecuteNonQueryAsync(string sqlQuery, int optVar)
        {
            using (_dtTable = new DataTable())
            {
                using (_connection = OpenConnection())
                {
                    using (var mySqlCommand = new MySqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        var retVal = await mySqlCommand.ExecuteNonQueryAsync();
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
                    using (var mySqlCommand = new MySqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        using (var adp = new MySqlDataAdapter(mySqlCommand))
                        {
                            adp.Fill(_dataSet);
                            return _dataSet;
                        }
                    }
                }
            }
        }

        public async Task<DataSet> ExecuteNonQueryAsync(string sqlQuery, string opt)
        {
            using (_dataSet = new DataSet())
            {
                using (_connection = OpenConnection())
                {
                    using (var mySqlCommand = new MySqlCommand {CommandText = sqlQuery, Connection = _connection})
                    {
                        using (var adp = new MySqlDataAdapter(mySqlCommand))
                        {
                            await adp.FillAsync(_dataSet);
                            return _dataSet;
                        }
                    }
                }
            }
        }

        public async Task<int> ExecuteStoreProcedureAsync(string spName, string[,] param)
        {
            var retVal = 0;
            using (_connection = OpenConnection())
            {
                using (var cmd = new MySqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure})
                {
                    var name = param[0, 0];
                    var value = param[0, 1];
                    var parameter = new MySqlParameter
                    {
                        Direction = ParameterDirection.Input,
                        DbType = DbType.String,
                        ParameterName = name,
                        Value = value
                    };
                    cmd.Parameters.Add(parameter);

                    using (var reader = await cmd.ExecuteReaderAsync())
                    {
                        if (reader.Read())
                        {
                            retVal = reader.GetInt32(0);
                        }
                    }
                }
            }
            return retVal;
        }

        public async Task<string> ExecuteStoreProcedureAsync(string spName, MySqlParameter[] param)
        {
            var retVal = string.Empty;
            try
            {
                using (_connection = OpenConnection())
                {
                    using (var cmd = new MySqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure})
                    {
                        foreach (var p in param)
                        {
                            cmd.Parameters.Add(p);
                        }
                        using (var reader = await cmd.ExecuteReaderAsync())
                        {
                            try
                            {
                                while (reader.Read())
                                {
                                    var outPut = reader.GetString(0);
                                    switch (outPut)
                                    {
                                        case "0":
                                            retVal = "Added";
                                            break;
                                        case "1":
                                            retVal = "Exists";
                                            break;
                                        default:
                                            retVal = outPut;
                                            break;
                                    }
                                }
                                if (!reader.IsClosed)
                                    reader.Close();
                            }
                            catch (MySqlException exception)
                            {
                                Console.WriteLine(exception.Message);
                            }
                            catch (Exception ex)
                            {
                                Console.WriteLine(ex.Message);
                            }
                        }
                    }
                }
                return retVal;
            }
            finally
            {
                if (_connection.State == ConnectionState.Open)
                    _connection.Close();
            }
        }

        public async Task<int> ExecuteSpWithOutParameter(string spName, MySqlParameter[] parameters)
        {
            var vNumber = 0;
            using (_connection = OpenConnection())
            {
                using (
                    var mySqlCommand = new MySqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure}
                    )
                {
                    foreach (var p in parameters)
                    {
                        mySqlCommand.Parameters.Add(p);
                    }
                    var task = await mySqlCommand.ExecuteNonQueryAsync();
                    if (task >= 0)
                    {
                        vNumber = (int) mySqlCommand.Parameters["@versionNo"].Value;
                        return vNumber;
                    }
                }
            }
            return vNumber;
        }

        public async Task<int> ExecuteSpWithOutParameter(string spName, MySqlParameter[] parameters,
            string outParameterName)
        {
            var vNumber = 0;
            using (_connection = OpenConnection())
            {
                using (
                    var mySqlCommand = new MySqlCommand(spName, _connection) {CommandType = CommandType.StoredProcedure}
                    )
                {
                    foreach (var p in parameters)
                    {
                        mySqlCommand.Parameters.Add(p);
                    }
                    var task = await mySqlCommand.ExecuteNonQueryAsync();
                    if (task >= 0)
                    {
                        vNumber = (int) mySqlCommand.Parameters[outParameterName].Value;
                        return vNumber;
                    }
                }
            }
            return vNumber;
        }
    }
}