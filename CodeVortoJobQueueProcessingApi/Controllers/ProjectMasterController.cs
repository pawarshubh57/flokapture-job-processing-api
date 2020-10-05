using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class ProjectMasterController : ApiController
    {
        private ICodeVortoService _codeVortoService;

        public ProjectMasterController()
        {
        }

        public ProjectMasterController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }

        [HttpGet]
        public async Task<IHttpActionResult> Get()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectList = await _codeVortoService.ProjectMasterRepository.GetAllItems();
                var projectMasters = projectList as IList<ProjectMaster> ?? projectList.ToList();
                foreach (var project in projectMasters)
                {
                    if (project.UploadedTime != null)
                        project.UploadedTm = project.UploadedTime.Value.ToString("HH:mm tt");
                    if (project.ProcessedTime != null)
                        project.ProcessedTm = project.ProcessedTime.Value.ToString("HH:mm tt");
                }
                return Ok(projectMasters);
            }
        }

        [HttpGet]
        public IHttpActionResult Get(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                return Ok(projectMaster);
            }
        }

        [HttpPost]
        public async Task<IHttpActionResult> Post([FromBody] ProjectMaster projectMaster)
        {
            if (!ModelState.IsValid)
                return ResponseMessage(Request.CreateResponse(HttpStatusCode.InternalServerError, ModelState));

            using (_codeVortoService = new CodeVortoService())
            {
                var totalFiles =
                    Directory.GetFiles(projectMaster.PhysicalPath, "*.*", SearchOption.AllDirectories).Length;
                projectMaster.TotalFiles = totalFiles;
                projectMaster.UploadedDate = DateTime.Now;
                projectMaster.UploadedTime = DateTime.Now;
                projectMaster.SolutionId = projectMaster.LanguageId;
                var project = await _codeVortoService.ProjectMasterRepository
                    .AddNewItem(projectMaster).ConfigureAwait(false);
                if (project == null) return BadRequest("Project name already exist");

                return Ok(project);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetProjectWorkSpaces()
        {
            var projectMasterRepository = new ProjectMasterRepository(new AppDbContext());
            var projects = await projectMasterRepository.GetAllItems();
            return Ok(projects);
        }

        [HttpGet]
        public async Task<IHttpActionResult> SearchProjects(string fromDate, string toDate)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var frmDate = Convert.ToDateTime(fromDate).ToShortDateString();
                var tDate = Convert.ToDateTime(toDate).ToShortDateString();
                var sqlQuery = "Select * from ProjectMaster where UploadedDate BETWEEN '" + frmDate + "' AND '" +
                               tDate + "'; ";
                var projectsList = await _codeVortoService.ProjectMasterRepository
                    .GetDataFromSqlQuery<ProjectMaster>(sqlQuery);
                return Ok(projectsList);
            }
        }

        [HttpPost]
        public async Task<IHttpActionResult> UploadProjectDocuments([FromBody] ProjectDocuments projectDocuments)
        {
            if (!ModelState.IsValid) return BadRequest();

            var generalRepository = new GeneralRepository<ProjectDocuments>(new AppDbContext());
            var documentDetail = await generalRepository.AddNewItem(projectDocuments);
            if (documentDetail == null) return InternalServerError();

            var documentData = await generalRepository.GetAllItems(p => p.ProjectId == documentDetail.ProjectId);
            return Ok(documentData);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetProjectDocuments(int projectId)
        {
            var generalRepository = new GeneralRepository<ProjectDocuments>(new AppDbContext());
            var documentData = await generalRepository.GetAllItems(p => p.ProjectId == projectId);
            return Ok(documentData);
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetApplicationPieChart(int projectId)
        // To Get all the application Pie Chart
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var objChart = new Chart { Charts = new List<ChartItem>() };
                try
                {
                    try
                    {
                        var generalRepository =
                            new GeneralRepository<ChartDashboardCount>(new AppDbContext());
                        object[] parameters =
                        {
                            new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
                        };
                        var chartData = await generalRepository
                            .ExecuteStoreProcedure<ChartDashboardCountNew>("GetApplicationChartData", parameters);

                        if ((chartData.Count > 0) && (chartData.Count > 0))
                        {
                            #region Added Chart data

                            if (chartData.Count > 0)
                                for (var i = 0; i < chartData.Count; i++)
                                {
                                    string newCol;
                                    if (chartData[i].Extension.Replace(".", "") == "vb")
                                        newCol = "#8669CC";
                                    else if (chartData[i].Extension.Replace(".", "") == "config")
                                        newCol = "#a6c600";
                                    else if (chartData[i].Extension.Replace(".", "") == "svc")
                                        newCol = "#177bbb";
                                    else if (chartData[i].Extension.Replace(".", "") == "designer")
                                        newCol = "#f84f9a";
                                    else if (chartData[i].Extension.Replace(".", "") == "cbl")
                                        newCol = "#FF0000";
                                    else if (chartData[i].Extension.Replace(".", "") == "jcl")
                                        newCol = "#FFA500";
                                    else if (chartData[i].Extension.Replace(".", "") == "proc")
                                        newCol = "#469496";
                                    else
                                        newCol = "#247A00";
                                    objChart.Charts.Add(new ChartItem
                                    {
                                        label = chartData[i].Extension.Replace(".", ""),
                                        data = Convert.ToInt32(chartData[i].Count),
                                        color = newCol
                                    });
                                }

                            #endregion
                        }
                    }
                    catch (Exception exception)
                    {
                        return InternalServerError(exception);
                    }
                }
                catch (Exception exception)
                {
                    return InternalServerError(exception);
                }
                return Ok(objChart);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetApplicationChart(int projectId) // To Get all the application Chart
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var objChart = new Chart { Charts = new List<ChartItem>() };

                try
                {
                    var generalRepository =
                        new GeneralRepository<ChartDashboardCount>(new AppDbContext());
                    object[] parameters =
                    {
                        new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var chartData = await generalRepository
                        .ExecuteStoreProcedure<ChartDashboardCountNew>("GetApplicationChartData", parameters);

                    if ((chartData.Count > 0) && (chartData.Count > 0))
                    {
                        #region Added Chart data

                        if (chartData.Count > 0)
                            for (var i = 0; i < chartData.Count; i++)
                                objChart.Charts.Add(new ChartItem
                                {
                                    label = chartData[i].Extension.Replace(".", ""),
                                    data = Convert.ToInt32(chartData[i].Count),
                                    color = "#8669CC"
                                });

                        #endregion
                    }
                }
                catch (Exception exception)
                {
                    return InternalServerError(exception);
                }
                return Ok(objChart);
            }
        }

        //[HttpGet]
        //public async Task<Chart> GetApplicationLOCChartOld(int projectId) // To Get all the application LOC Chart
        //{
        //    using (_codeVortoService = new CodeVortoService())
        //    {
        //        var objChart = new Chart();
        //        objChart.Charts = new List<ChartItem>();
        //        //var projectId = 0;
        //        try
        //        {


        //            GeneralRepository<ChartDashboardCount> generalRepository =
        //       new GeneralRepository<ChartDashboardCount>(new AppDbContext());
        //            object[] parameters =
        //    {
        //        new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
        //    };
        //            var chartData = await generalRepository
        //                .ExecuteStoreProcedure<ChartDashboardCount>("SpChartLOCDashboard", parameters);

        //            if (chartData.Count > 0 && chartData.Count > 0)
        //            {
        //                #region Added Chart data
        //                if (chartData.Count > 0)
        //                {
        //                    foreach (var chartContain in chartData)
        //                    {
        //                        chartData.ToArray();
        //                        if (chartData.ToArray()[0].NoOfVb != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "vb",
        //                                data = chartData.ToArray()[0].NoOfVb,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfDesigner != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "designer",
        //                                data = chartData.ToArray()[0].NoOfDesigner,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfConfig != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "config",
        //                                data = chartData.ToArray()[0].NoOfConfig,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfSVC != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "svc",
        //                                data = chartData.ToArray()[0].NoOfSVC,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfCbl != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "cbl",
        //                                data = chartData.ToArray()[0].NoOfCbl,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfJcl != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "jcl",
        //                                data = chartData.ToArray()[0].NoOfJcl,

        //                            });
        //                        }

        //                        if (chartData.ToArray()[0].NoOfProc != 0)
        //                        {
        //                            objChart.Charts.Add(new ChartItem
        //                            {
        //                                label = "procs",
        //                                data = chartData.ToArray()[0].NoOfProc,

        //                            });
        //                        }
        //                    }
        //                }
        //                #endregion
        //            }
        //        }
        //        catch (Exception)
        //        {

        //        }

        //        return objChart;
        //    }

        //}

        [HttpGet]
        public async Task<IHttpActionResult> GetApplicationLocChart(int projectId)
        // To Get all the application LOC Chart
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var objChart = new Chart { Charts = new List<ChartItem>() };
                //var projectId = 0;
                try
                {
                    var generalRepository =
                        new GeneralRepository<ChartDashboardCount>(new AppDbContext());
                    object[] parameters =
                    {
                        new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var chartData = await generalRepository
                        .ExecuteStoreProcedure<ChartDashboardCountNew>("SpChartLOCDashboard", parameters);

                    if ((chartData.Count > 0) && (chartData.Count > 0))
                    {
                        #region Added Chart data

                        if (chartData.Count > 0)
                            for (var i = 0; i < chartData.Count; i++)
                                if (Convert.ToInt32(chartData[i].Count) != 0)
                                    objChart.Charts.Add(new ChartItem
                                    {
                                        label = chartData[i].Extension.Replace(".", ""),
                                        data = Convert.ToInt32(chartData[i].Count),
                                        color = "#8669CC"
                                    });

                        #endregion
                    }
                }
                catch (Exception exception)
                {
                    return InternalServerError(exception);
                }

                return Ok(objChart);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetApplicationActionWorkflowChart(int projectId)
        // To Get all the application workflow data.
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var objChart = new Chart { Charts = new List<ChartItem>() };
                try
                {
                    var generalRepository =
                        new GeneralRepository<ChartDashboardCount>(new AppDbContext());
                    object[] parameters =
                    {
                        new MySqlParameter("prjId", MySqlDbType.Int32) {Value = projectId}
                    };
                    var chartData = await generalRepository
                        .ExecuteStoreProcedure<ChartDashboardActionWorkFlow>("GetActionWorkFlowChartData", parameters);

                    if ((chartData.Count > 0) && (chartData.Count > 0))
                    {
                        #region Added Chart data

                        if (chartData.Count > 0)
                            for (var i = 0; i < chartData.Count; i++)
                                objChart.Charts.Add(new ChartItem
                                {
                                    label = chartData[i].Project.Replace(".", ""),
                                    data = Convert.ToInt32(chartData[i].Count),
                                    color = "#8669CC"
                                });

                        #endregion
                    }
                }
                catch (Exception exception)
                {
                    return InternalServerError(exception);
                }
                return Ok(objChart);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllLenguages()
        {
            using (var appDbContext = new AppDbContext())
            {
                var lstLanguages = await appDbContext.LanguageMaster.ToListAsync().ConfigureAwait(true);
                return Ok(lstLanguages);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllSolutions()
        {
            using (var appDbContext = new AppDbContext())
            {
                var lstSolutions = await appDbContext.SolutionMaster.ToListAsync().ConfigureAwait(true);
                return Ok(lstSolutions);
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> GetAllProjectTypes(int solutionId)
        {
            using (var appDbContext = new AppDbContext())
            {
                var lstProjectTypes = await appDbContext.ProjectType.ToListAsync().ConfigureAwait(true);
                lstProjectTypes = lstProjectTypes.Where(type => type.LanguageId == solutionId).ToList();
                return Ok(lstProjectTypes);
            }
        }

        [HttpGet]
        public async Task<ProjectMaster> GetUnProcessedProject()
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = await _codeVortoService.ProjectMasterRepository.GetItem<ProjectMaster>(x => x.Processed == 0);
                // projectMaster = projectMaster ?? new ProjectMaster();
                return projectMaster;
            }
        }

        [HttpGet]
        public async Task<IHttpActionResult> UpdateProjectStatus(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var projectMaster = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                if (projectMaster == null) return BadRequest("Project not found");

                projectMaster.Processed = 1;
                projectMaster.ProcessedDate = DateTime.Now;
                projectMaster.ProcessedTime = DateTime.Now;
                await _codeVortoService.ProjectMasterRepository.UpdateItem(projectMaster);

                return Ok(projectMaster);
            }
        }
    }
}