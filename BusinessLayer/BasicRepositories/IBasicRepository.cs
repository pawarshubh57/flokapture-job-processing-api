using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq.Expressions;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;

namespace BusinessLayer.BasicRepositories
{
    public interface IBasicRepository<TSource> where TSource : class
    {
        /// <summary>
        /// Get main AppDbContext class instance direclty
        /// </summary>
        /// <returns></returns>
        AppDbContext GetAppDbContext();
        /// <summary>
        /// Returns the DbSet TSource which is helpful to get data directly 
        /// </summary>
        /// <returns></returns>
        DbSet<T> GetDbSet<T>() where T : class;

        /// <summary>
        /// 
        /// </summary>
        DbSet<TSource> DbSet { get; }
        /// <summary>
        ///     Returns the Type of TSource
        /// </summary>
        /// <param name="tSource"></param>
        /// <returns></returns>
        Type GetType(TSource tSource);

        /// <summary>
        ///     Returns the IEnumerable of TSource which is Generic input type
        /// </summary>
        /// <returns></returns>
        Task<IEnumerable<TSource>> GetAllItems();

        /// <summary>
        ///     Returns the IEnumerable of TSource which is Generic input type
        /// </summary>
        /// <returns></returns>
        Task<IEnumerable<TSource>> GetAllItems(Predicate<TSource> predicate);

        /// <summary>
        ///     Returns the IEnumerable of TSource which is Generic input type
        /// </summary>
        /// <returns></returns>
        Task<List<TSource>> GetAllListItems(Expression<Func<TSource, bool>> expression);// where T : class;

        /// <summary>
        /// Return List of TSource without expression... Use this when small amount of data.
        /// </summary>
        /// <returns></returns>
        Task<List<TSource>> GetAllListItems();

        /// <summary>
        ///     Returns the IEnumerable of TSource which is Generic input type
        /// </summary>
        /// <returns></returns>
        Task<IEnumerable<T>> GetAllItems<T>(Expression<Func<T, bool>> expression) where T : class;

        /// <summary>
        ///     Returns the int
        /// </summary>
        /// <param name="itemSource"></param>
        /// <returns></returns>
        Task<TSource> AddNewItem(TSource itemSource);

        /// <summary>
        /// </summary>
        /// <param name="itemSource"></param>
        Task<TSource> UpdateItem(TSource itemSource);

        /// <summary>
        /// Returns int
        /// </summary>
        /// <param name="listOfEntities"></param>
        /// <returns></returns>
        Task<int> BulkInsert(List<TSource> listOfEntities);

        /// <summary>
        /// </summary>
        /// <param name="tKey"></param>
        /// <returns></returns>
        Task<int> DeleteItem(int tKey);

        /// <summary>
        /// </summary>
        /// <param name="predicate"></param>
        /// <param name="tKey"></param>
        /// <returns></returns>
        Task<int> DeleteItem<T>(Predicate<TSource> predicate, int tKey);

        /// <summary>
        /// </summary>
        /// <param name="tKey"></param>
        /// <returns></returns>
        TSource GetItem(int tKey);

        /// <summary>
        /// </summary>
        /// <param name="expression"></param>
        /// <param name="tKey"></param>
        /// <returns></returns>
        Task<TSource> GetItem<T>(Expression<Func<TSource, bool>> expression, int tKey);

        /// <summary>
        /// </summary>
        /// <param name="expression"></param>
        /// <returns></returns>
        Task<T> GetItem<T>(Expression<Func<T, bool>> expression) where T : class;

        /// <summary>
        /// </summary>
        /// <param name="updateSource"></param>
        IHttpActionResult UpdateItems(IEnumerable<TSource> updateSource);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="predicate"></param>
        /// <returns></returns>
        Task<TSource> FindItem<T>(Predicate<TSource> predicate);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="predicate"></param>
        /// <returns></returns>
        Task<List<TSource>> FindAllItems<T>(Predicate<TSource> predicate);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="mySqlQuery"></param>
        /// <returns></returns>
        Task<List<TSource>> GetDataFromSqlQuery<T>(string mySqlQuery);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="predicate"></param>
        /// <returns></returns>
        Task<List<T>> GetEntityData<T>(Predicate<T> predicate) where T : class;

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="storeProcName"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        Task<List<T>> ExecuteStoreProcedure<T>(string storeProcName, params object[] parameters) where T : class;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="storeProcName"></param>
        /// <param name="parameters"></param>
        /// <returns></returns>
        Task<int> ExecuteStoreProcedure(string storeProcName, params object[] parameters);
    }
}