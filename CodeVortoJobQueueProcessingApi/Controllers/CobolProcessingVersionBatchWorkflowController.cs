using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DbEntities;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MySql.Data.MySqlClient;

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public partial class CobolProcessingVersionBatchController
    {
        public ActionWorkflowDetails GiveMeActionWorkFlowDetail(List<ActionWorkflows> workflowRef,
            List<StatementReferenceMaster> lstStateRef, ActionWorkflows workflow, int parentRowId, int projectId, int flag)
        {
            var enable = workflowRef.Count(x => x.IsDeleted == 0);
            var hidden = workflowRef.Count(x => x.IsDeleted == 1);
            var count = "<b style='color: #7030a0;'>" + " (Total Workflows: " + workflowRef.Count +
                        " | Enabled: " + enable + " | Hidden: " + hidden + ")" + "</b>";
            var graphId = "graph_" + parentRowId;

            /*
            var objConnect = "btnObj_" + workflow.ActionWorkflowId;
            var download = "btnDowload_" + workflow.ActionWorkflowId;
            var fileId = workflow.FileId;
            var callExternal = lstStateRef
                .Count(d => (d.BaseCommandId == 6) && (d.FileId == workflow.FileId));
            var callInternal = lstStateRef
                .Count(d => (d.BaseCommandId == 5) && (d.FileId == workflow.FileId));

            var decisionCount = lstStateRef.Count(d => (d.BaseCommandId == 1) &&
                                                       (d.FileId == workflow.FileId)) + 1;
            */
            string disabled = string.Empty;
            string btnStyle = "style='margin-top :5px;height: 31px;'";
            string pageUrl = "workflow_workspace.html?prjId=" + workflow.ProjectId + "&stId=" +
                             workflow.MethodStatementId + "&aId=" + workflow.ActionWorkflowId + "";
            /*
            string downloadReqDoc = "onclick = downloadRequirementDoc(" + workflow.ActionWorkflowId + ");";
            string onClickUploadFile = "onclick=uploadFilePopupShow('" +
                                       workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") +
                                       "'," + workflow.ActionWorkflowId + ");";
            string onClickRename = "onclick=funWorkflowRename('" +
                                   workflow.OriginObject.Replace("'", "&apos;").Replace(" ", "&nbsp;") + "'," +
                                   workflow.ActionWorkflowId + ");";

            var disable = "<button id=" + workflow.ActionWorkflowId +
                          " style='margin-top: 5px;' class='btn btn-info btn-icon icon-lg fa fa-trash' onclick='workFlowDisable(" +
                         workflow.ActionWorkflowId + ")'></button>";
            */
            var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                       "<button " + btnStyle + " " + disabled +
                       " class='btn btn-mint'>View</button> </a>";
            /*
            var view = "<a " + disabled + " href='" + pageUrl + "'>" +
                       "<button " + btnStyle + " " + disabled +
                       " class='btn btn-mint'>View</button> </a>" +
                       " &nbsp;<a href='javascript:void(0);'><button style='margin-top : 5px;height: 31px;' class='btn btn-mint' " +
                       onClickRename + " >Rename</button> </a>&nbsp;<button id=" +
                       workflow.ActionWorkflowId + " style='margin-top : 4px;' " +
                       "class='btn btn-mint btn-icon icon-lg fa fa-upload' " + onClickUploadFile +
                       " title='Upload image/file(s)'></button>&nbsp;" +
                       "<button id=" + download +
                       " style='margin-bottom: -8px; margin-top: -3px;height: 31px;'" +
                       " class='btn btn-primary btn-icon icon-lg fa fa-download' " + downloadReqDoc +
                       " title='Download Requirement Specification Document(.docx)' ></button>&nbsp;" +
                       "<button id=" + objConnect +
                       " style='margin-bottom: -8px; margin-top: -3px;height: 31px;' class='btn btn-success btn-icon icon-lg fa fa-sitemap' " +
                       "onclick='downloadObjectConnectivityIndividualFlowchartUniverse(" +
                       fileId + ", " + workflow.ActionWorkflowId +
                       ");' title='Download object connectivity.'></button>";
            */
            var workflowDetails = new ActionWorkflowDetails
            {
                DecisionCount = 0, //decisionCount,
                ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                View = view,
                OriginObject = workflow.OriginObject.ToUpper(),
                WorkflowName = workflow.WorkflowName.ToUpper(),
                ProjectName = workflow.ProjectMaster.ProjectName,
                ActionWorkflowId = workflow.ActionWorkflowId,
                Disable = "-",
                IsDeleted = workflow.IsDeleted,
                ShortDetails = count,
                GraphId = graphId,
                ParentId = "-1",
                ProjectId = projectId
            };
            return workflowDetails;
        }

        [HttpGet]
        public async Task<IHttpActionResult> ProcessActionWorkflowDetails(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allActionWorkFlows = await _codeVortoService.ActionWorkflowsRepository
                    .GetAllListItems(a => a.ProjectId == projectId).ConfigureAwait(false);
                var projectDetails = _codeVortoService.ProjectMasterRepository.GetItem(projectId);
                int solutionId = projectDetails.SolutionId ?? 0;

                object[] parameters =
                {
                    new MySqlParameter("@prjId", MySqlDbType.Int32) {Value = projectId}
                };
                var allClassNameDeclared =
                    await _codeVortoService.StatementReferenceMasterRepository
                        .ExecuteStoreProcedure<StatementReferenceMaster>("SpGetAllStatementForWorkFlowProcess", parameters)
                        .ConfigureAwait(false);

                var fileMasterData = await _codeVortoService.FileMasterRepository
                    .GetAllListItems(f => f.SolutionId == solutionId && f.ProjectId == projectDetails.ProjectId)
                    .ConfigureAwait(false);
                int graphId = 0;
                int childsGraphId = 0;

                Console.WriteLine("=============================================================");
                Console.WriteLine("Total Action Workflows to process: " + allActionWorkFlows.Count);
                Console.WriteLine("=============================================================");
                foreach (var actionWorkflow in allActionWorkFlows)
                {
                    graphId++;
                    Console.WriteLine("Action Workflows Remaining: " + (allActionWorkFlows.Count - graphId));
                    Console.WriteLine("=============================================================");

                    childsGraphId++;
                    Thread.Sleep(100);
                    int fileMenuId = actionWorkflow.FileMenuId;
                    if (actionWorkflow.FileId == 0)
                    {
                        var emptyActionDetails = GiveMeActionWorkFlowDetail(allActionWorkFlows, allClassNameDeclared,
                            actionWorkflow, graphId, projectId, 0);
                        emptyActionDetails.GraphId = "EmptyGraph_" + graphId;
                        emptyActionDetails.ParentId = "-1";
                        emptyActionDetails.ObjectType = "Menu";
                        emptyActionDetails.FileMenuId = fileMenuId;
                        await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(emptyActionDetails)
                            .ConfigureAwait(false);
                        Thread.Sleep(10);
                        continue;
                    }
                    int fileId = actionWorkflow.FileId;
                    var currentJclFile = fileMasterData.First(f => f.FileId == fileId);
                    // main workflow
                    var aDetails = GiveMeActionWorkFlowDetail(allActionWorkFlows, allClassNameDeclared, actionWorkflow,
                        graphId, projectId, 1);
                    aDetails.GraphId = "Graph_" + graphId;
                    aDetails.ParentId = "-1";
                    aDetails.ObjectType = "JCL"; // objectType;
                    aDetails.WorkflowName = !string.IsNullOrEmpty(actionWorkflow.OriginEventMethod)
                        ? actionWorkflow.OriginEventMethod
                        : actionWorkflow.WorkflowName;
                    aDetails.FileMenuId = fileMenuId;
                    await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(aDetails).ConfigureAwait(false);
                    Thread.Sleep(100);

                    // main jcl
                    string disabled = string.Empty;
                    var jclGraphId = "JclGraphId_" + graphId;
                    string btnStyle = "style='margin-top :5px;height: 31px;'";

                    var allCallExt = allClassNameDeclared
                        .Where(s => s.FileId == currentJclFile.FileId && s.BaseCommandId == 6)
                        .OrderBy(s => s.StatementId).ToList();

                    foreach (var file in allCallExt)
                    {
                        Thread.Sleep(500);
                        if (fileMasterData.All(f => f.FileId != file.FileId)) continue;
                        if (file.ReferenceFileId == 0) continue;
                        var fileMaster = fileMasterData.First(f => f.FileId == file.ReferenceFileId);
                        if (fileMaster == null) continue;

                        var fileStatements1 = File.ReadAllLines(fileMaster.FilePath).ToList();
                        fileStatements1.RemoveAll(s => s.Length <= 0);
                        fileStatements1 = fileStatements1.Select(s => s.Trim()).ToList();
                        var fileBusinessName1 = fileStatements1.First().TrimStart('/').Trim('*').StartsWith("PA ")
                            ? fileStatements1.First().TrimStart('/').Trim('*').Replace("PA ", "").Trim()
                            : fileStatements1.First().TrimStart('/').Trim('*').Trim();

                        if (fileMaster.FileTypeExtensionId == 8)
                        {
                            // add new actionWorkflowDetails...
                            string pageUrl1 = "customview.html?prjId=" + projectId + "&fileId=" +
                                              fileMaster.FileId + "";
                            var view1 = "<a " + disabled + " href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " " + disabled +
                                        " class='btn btn-mint'>View</button>";

                            string objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0, //decisionCount,
                                ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                                InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                ProjectName = fileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = aDetails.GraphId,
                                GraphId = "ProgramGraphId_" + graphId + fileMaster.FileId,
                                ProjectId = projectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("Program");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);
                            Thread.Sleep(1000);

                            childsGraphId = await FaktJclToProgramChaCode(fileMaster, fileMasterData, allClassNameDeclared,
                                workflowDetails1.GraphId, childsGraphId, fileMenuId).ConfigureAwait(false);
                            childsGraphId = childsGraphId + 1;

                            continue;
                        } // Program
                        if (fileMaster.FileTypeExtensionId == 6)
                        {
                            // add new actionWorkflowDetails...
                            string objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            string jclFileGraphId = "NewJclGraph_" + graphId + fileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + projectId + "&fileId=" +
                                              fileMaster.FileId + "";
                            var view1 = "<a " + disabled + " href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " " + disabled +
                                        " class='btn btn-mint'>View</button>";

                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0, //decisionCount,
                                ExtrernalCalls = "-", // callExternal > 0 ? "Yes" : "No",
                                InternalCalls = "-", // callInternal > 0 ? "Yes" : "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(fileMaster.FilePath),
                                ProjectName = fileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = jclGraphId,
                                GraphId = jclFileGraphId,
                                ProjectId = projectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("JCL");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);

                            if (fileMaster.FileId == file.FileId) continue;

                            Thread.Sleep(600);
                            childsGraphId = await FaktJclToProgramChaCode(fileMaster, fileMasterData, allClassNameDeclared,
                                jclFileGraphId, childsGraphId, fileMenuId).ConfigureAwait(false);
                            childsGraphId = childsGraphId + 1;
                        } // JCL
                    }
                }

                return Ok("All action workflow details processed successfully.");
            }
        }

        private async Task<int> FaktJclToProgramChaCode(FileMaster fileMaster, List<FileMaster> fileMasterData,
            List<StatementReferenceMaster> allClassNameDeclared, string parentId, int childsGraphId, int fileMenuId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var allCallExt = await _codeVortoService.StatementReferenceMasterRepository
                    .GetAllListItems(s => s.FileId == fileMaster.FileId && s.BaseCommandId == 6 && s.ReferenceFileId != 0)
                    .ConfigureAwait(false);
                allCallExt = allCallExt.OrderBy(s => s.StatementId).ToList();
                string btnStyle = "style='margin-top :5px;height: 31px;'";
                var allClasses = (from a in allCallExt
                                  where !string.IsNullOrEmpty(a.ClassCalled)
                                  select a.ClassCalled).ToList();

                foreach (var aClass in allClasses)
                {
                    Thread.Sleep(100);
                    var allFiles = (from s in allClassNameDeclared
                                    where aClass == s.ClassNameDeclared
                                    select s).ToList();

                    foreach (var file in allFiles)
                    {
                        Thread.Sleep(200);
                        if (fileMasterData.All(f => f.FileId != file.FileId)) continue;
                        var thisFileMaster = fileMasterData.First(f => f.FileId == file.FileId);

                        var fileStatements1 = File.ReadAllLines(thisFileMaster.FilePath).ToList();
                        fileStatements1.RemoveAll(s => s.Length <= 0);
                        fileStatements1 = fileStatements1.Select(s => s.Trim()).ToList();
                        var fileBusinessName1 = fileStatements1.First().TrimStart('/').Trim('*').StartsWith("PA ")
                            ? fileStatements1.First().TrimStart('/').Trim('*').Replace("PA ", "").Trim()
                            : fileStatements1.First().TrimStart('/').Trim('*').Trim();

                        if (thisFileMaster.FileTypeExtensionId == 8)
                        {
                            // add new actionWorkflowDetails...
                            string pilluChaGraphId = "PgmGraphId_" + ++childsGraphId + thisFileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + thisFileMaster.ProjectId + "&fileId=" +
                                              thisFileMaster.FileId + "";
                            var view1 = "<a href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " class='btn btn-mint'>View</button>";
                            string objectType = fileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0,
                                ExtrernalCalls = "No",
                                InternalCalls = "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                ProjectName = thisFileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = parentId,
                                GraphId = pilluChaGraphId,
                                ProjectId = thisFileMaster.ProjectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("Program");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);
                            Thread.Sleep(200);
                            continue;
                        } // Proc
                        if (thisFileMaster.FileTypeExtensionId == 19)
                        {
                            // add new actionWorkflowDetails...
                            string jclFileGraphId = "PilluJclGraph_" + ++childsGraphId + thisFileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + thisFileMaster.ProjectId + "&fileId=" +
                                              thisFileMaster.FileId + "";
                            var view1 = "<a  href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " class='btn btn-mint'>View</button>";
                            string objectType = thisFileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0,
                                ExtrernalCalls = "No",
                                InternalCalls = "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                ProjectName = thisFileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = parentId,
                                GraphId = jclFileGraphId,
                                ProjectId = thisFileMaster.ProjectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("JCL");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);

                            // if (thisFileMaster.FileId == file.FileId) break;

                            Thread.Sleep(500);
                            await FaktJclToProgramChaCode(thisFileMaster, fileMasterData, allClassNameDeclared,
                                jclFileGraphId, childsGraphId, fileMenuId);
                            Console.WriteLine("InputLib");
                        } // InputLib
                        if (thisFileMaster.FileTypeExtensionId == 6)
                        {
                            // add new actionWorkflowDetails...
                            string jclFileGraphId = "PilluJclGraph_" + ++childsGraphId + thisFileMaster.FileId;
                            string pageUrl1 = "customview.html?prjId=" + thisFileMaster.ProjectId + "&fileId=" +
                                              thisFileMaster.FileId + "";
                            var view1 = "<a  href=javascript:window.open('" + pageUrl1 + "')>" +
                                        "<button " + btnStyle + " class='btn btn-mint'>View</button>";
                            string objectType = thisFileMaster.FileTypeExtensionReference.FileTypeName;
                            var workflowDetails1 = new ActionWorkflowDetails
                            {
                                DecisionCount = 0,
                                ExtrernalCalls = "No",
                                InternalCalls = "No",
                                View = view1,
                                OriginObject = Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                WorkflowName = fileBusinessName1, // Path.GetFileNameWithoutExtension(thisFileMaster.FilePath),
                                ProjectName = thisFileMaster.ProjectMaster.ProjectName,
                                ActionWorkflowId = 0,
                                ShortDetails = "0",
                                ParentId = parentId,
                                GraphId = jclFileGraphId,
                                ProjectId = thisFileMaster.ProjectId,
                                ObjectType = objectType,
                                FileMenuId = fileMenuId
                            };
                            // Console.WriteLine("JCL");
                            await _codeVortoService.ActionWorkflowDetailsRepository.AddNewItem(workflowDetails1)
                                .ConfigureAwait(false);

                            if (thisFileMaster.FileId == file.FileId) break;

                            Thread.Sleep(500);
                            await FaktJclToProgramChaCode(thisFileMaster, fileMasterData, allClassNameDeclared,
                                jclFileGraphId, childsGraphId, fileMenuId);
                            Console.WriteLine("Cobol / Program");
                        } // Cobol / Program
                    }
                }
                return childsGraphId;
            }
        }
    }
}
