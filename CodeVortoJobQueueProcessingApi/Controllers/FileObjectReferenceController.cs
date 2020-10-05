using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Web.Http;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.GraphML;
using BusinessLayer.Models;
using CodeVortoJobQueueProcessingApi.CodeVortoServices;
using MindFusion.Diagramming;
using MindFusion.Diagramming.Layout;
using MindFusion.Diagramming.WebForms;
/* Added for VISIO Export */

namespace CodeVortoJobQueueProcessingApi.Controllers
{
    public class FileObjectReferenceController : ApiController
    {
        private ICodeVortoService _codeVortoService;
        private readonly DiagramView _diagramView = new DiagramView();

        public FileObjectReferenceController()
        {
        }

        public FileObjectReferenceController(ICodeVortoService codeVortoService)
        {
            _codeVortoService = codeVortoService;
        }
      
        [HttpGet]
        public async Task<FlowChart> GetObjectReference(string projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {

                var statementReferenceRepository = new StatementReferenceRepository(new AppDbContext());
                //var fileMasterRepository = new FileMasterRepository(new AppDbContext());
                var generalRepository = new GeneralRepository<DeadDataTypes>(new AppDbContext());
                List<Node> lstNodes = new List<Node>();
                var objFlowChart = new FlowChart();

                #region Using DataBase entries and generated the flowchart
                var connectivityStepRef = await _codeVortoService.ConnectivityStepReferenceRepository
                    .GetDataFromSqlQuery<ConnectivityStepReference>(
                    "select * from ConnectivityStepReference where ProjectId=" + projectId + "");

                var connectivitylinkRef = await _codeVortoService.ConnectivityLinkRefereceRepository
                    .GetDataFromSqlQuery<ConnectivityLinkReferece>(
                    "select * from connectivitylinkreference where ProjectId=" + projectId + "");



                if (connectivityStepRef.Count > 0 && connectivitylinkRef.Count > 0)
                {
                    #region Added Nodes
                    if (connectivityStepRef.Count > 0)
                    {
                        foreach (var stepNodes in connectivityStepRef)
                        {
                            Node nodeObject = new Node();

                            nodeObject.Id = stepNodes.StatementId;
                            nodeObject.Name = stepNodes.Name;
                            nodeObject.ShapeId = stepNodes.Shape;
                            nodeObject.Height = "15";
                            nodeObject.Width = "100";
                            nodeObject.Color = stepNodes.Color;
                            nodeObject.StatementId = stepNodes.StatementId;
                            nodeObject.BaseCommandId = 0;
                            lstNodes.Add(nodeObject);
                        }

                        objFlowChart.Nodes = lstNodes;
                    }
                    #endregion

                    #region Added Links
                    if (connectivitylinkRef.Count > 0)
                    {
                        var lstLinks = new List<Link>();

                        foreach (var stepLinks in connectivitylinkRef)
                        {
                            var linkObject = new Link
                            {
                                Origin = stepLinks.Origin,
                                Target = stepLinks.Target,
                                BaseCommandId = 0,
                                StatementId = stepLinks.StatementId,
                                LinkText = stepLinks.LinkText
                            };

                            lstLinks.Add(linkObject);
                        }

                        objFlowChart.Links = lstLinks;
                    }
                    #endregion
                }
                #endregion

                #region Runtime Created the flowchart
                else
                {
                    try
                    {
                        #region Add Nodes Logic
                        var allClassData =
                            await statementReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                                "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and ClassNameDeclared != 'null';");
                        foreach (var s in allClassData)
                        {
                            Node nodeObject = new Node
                            {
                                Id = s.StatementId,
                                Name = s.ClassNameDeclared,
                                ShapeId = "RoundRect",
                                Height = "15",
                                Width = "100",
                                Color = "#F5BD6A",
                                StatementId = s.StatementId,
                                BaseCommandId = s.BaseCommandId
                            };

                            lstNodes.Add(nodeObject);
                            var fileNewId = s.FileId;

                            //  Get the all Classes to file wise.
                            var allCallingClassData = await statementReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "select * from statementreferencemaster where BaseCommandId=7" +
                                    " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                    fileNewId + "';");

                            foreach (var c in allCallingClassData)
                            {
                                var datatype = c.DataOrObjectType;
                                var allDeadDatatypes = await generalRepository.GetDataFromSqlQuery<DeadDataTypes>(
                                    "SELECT * FROM deaddatatypes");
                                int j = 0;
                                for (int i = 0; i < allDeadDatatypes.Count; i++)
                                {
                                    var keywordName = allDeadDatatypes[i].KaywordName;
                                    if (datatype.Contains(keywordName))
                                    {
                                        j++;
                                    }
                                }

                                if (j == 0)
                                {
                                    // Check the class the present or not.
                                    // Class is present to add the color : Yellow
                                    // Class is not present to add the color : Gray 

                                    var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                    var checkClassPresentOrNot = await statementReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "SELECT * from statementreferencemaster where FileId In (" +
                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                            "') and classNameDeclared like '%" + dataObject + "%';");
                                    if (checkClassPresentOrNot.Count > 0)
                                    {
                                        Node nodeObject1 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#F5BD6A",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,

                                        };


                                        List<int> iCnt = (from dd in lstNodes
                                                          where dd.Name == nodeObject1.Name
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject1);
                                        }
                                    }
                                    else
                                    {
                                        Node nodeObject2 = new Node
                                        {
                                            Id = c.StatementId,
                                            Name = c.DataOrObjectType,
                                            ShapeId = "RoundRect",
                                            Height = "15",
                                            Width = "100",
                                            Color = "#C0C0C0",
                                            StatementId = c.StatementId,
                                            BaseCommandId = c.BaseCommandId,
                                            FileId = c.FileId
                                        };

                                        List<int> iCnt = (from dd in lstNodes
                                                          where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                                          select dd.Id).ToList();
                                        if (iCnt.Count == 0)
                                        {
                                            lstNodes.Add(nodeObject2);
                                        }

                                    }
                                }
                            }
                        }
                        objFlowChart.Nodes = lstNodes;

                        #endregion

                        if (lstNodes.Count > 0)
                        {
                            #region Add Links

                            #region links variables

                            var lstLinks = new List<Link>();

                            #endregion

                            var allClassDataLinks = await statementReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and ClassNameDeclared!='null'");

                            foreach (var s in allClassDataLinks)
                            {
                                var iDofNode = s.StatementId;
                                var fileId = s.FileId;
                                // Get the file wise "Dim" started classes
                                var allCallingClassDataLinks = await statementReferenceRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                        "select * from statementreferencemaster where BaseCommandId=7" +
                                        " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                        fileId + "';");

                                foreach (var c in allCallingClassDataLinks)
                                {
                                    var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                    var calledDataObject = c.DataOrObjectType;
                                    var statementId = c.StatementId;
                                    var allDeadDatatypesLinks = await generalRepository
                                        .GetDataFromSqlQuery<DeadDataTypes>(
                                            "SELECT * FROM deaddatatypes");

                                    // Check the dead datatype in classes or Not

                                    int j = 0;
                                    for (int i = 0; i < allDeadDatatypesLinks.Count; i++)
                                    {
                                        var keywordName = allDeadDatatypesLinks[i].KaywordName;
                                        if (dataObject != null && dataObject.Contains(keywordName))
                                        {
                                            j++;
                                        }

                                    }
                                    if (j == 0)
                                    {
                                        // Check the Class id called or not.
                                        var checkClassPresentOrNotLink = await statementReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "SELECT * from statementreferencemaster where FileId In (" +
                                                " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                "') and classNameDeclared like '%" + dataObject + "%';");

                                        if (checkClassPresentOrNotLink.Count > 0)
                                        {
                                            foreach (var f in checkClassPresentOrNotLink)
                                            {

                                                var checkClassCallMethod = await statementReferenceRepository
                                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                        "SELECT * from statementreferencemaster where FileId In (" +
                                                        " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                        "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
                                                        "';");

                                                if (checkClassCallMethod.Count > 0)
                                                {
                                                    string mulMethodName = "";
                                                    var linkObject = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = f.StatementId,
                                                        BaseCommandId = f.BaseCommandId,
                                                        StatementId = f.StatementId
                                                    };
                                                    for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled != null)
                                                        {
                                                            if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                                checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                            {
                                                                if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                    !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                                {
                                                                    int indexMethod =
                                                                        checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                            StringComparison.Ordinal);
                                                                    if (indexMethod > 0)
                                                                    {
                                                                        if (
                                                                            !mulMethodName.Contains(
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled.Substring(0,
                                                                                        indexMethod)))
                                                                        {

                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled.Substring(0,
                                                                                                    indexMethod);
                                                                            linkObject.LinkText =
                                                                                mulMethodName.Substring(1,
                                                                                    mulMethodName.Length - 1)
                                                                                    .Replace("))", "");
                                                                        }
                                                                    }
                                                                    else
                                                                    {
                                                                        if (
                                                                            !mulMethodName.Contains(
                                                                                checkClassCallMethod[e]
                                                                                    .MethodCalled))
                                                                        {

                                                                            mulMethodName = mulMethodName + ", " +
                                                                                            checkClassCallMethod[e]
                                                                                                .MethodCalled;
                                                                            linkObject.LinkText =
                                                                                mulMethodName.Substring(1,
                                                                                    mulMethodName.Length - 1)
                                                                                    .Replace("))", "");
                                                                        }
                                                                    }


                                                                    if (linkObject.Origin != linkObject.Target)
                                                                    {
                                                                        if (e == checkClassCallMethod.Count - 1)
                                                                        {
                                                                            lstLinks.Add(linkObject);
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        else
                                        {
                                            #region Class Is not Calling to add the Class as Misssing Element

                                            var checkClassCallMethod = await statementReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "select * from statementreferencemaster where fileid= '" + fileId +
                                                    "' and statementId > '" + statementId + "' and" +
                                                    " statementId < (select statementId from statementreferencemaster where fileid= '" +
                                                    fileId + "' and statementId > '" + statementId + "'" +
                                                    " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                    " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                    "') and MethodCalled is not null;");


                                            if (checkClassCallMethod.Count == 0) // For Global variable
                                            {

                                                checkClassCallMethod = await statementReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "select * from statementreferencemaster where fileid= '" + fileId +
                                                    "' and statementId > '" + statementId + "' and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                    " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                    "') and MethodCalled is not null;");
                                            }

                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var l = (from d in lstNodes where d.Name == c.ClassNameDeclared select d).ToList();
                                                Link linkObject1 = new Link();
                                                if (l.Count > 0)
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = l.First().StatementId
                                                    };
                                                }
                                                else
                                                {
                                                    linkObject1 = new Link
                                                    {
                                                        Origin = iDofNode,
                                                        Target = statementId
                                                    };
                                                }
                                                //linkObject.target = c.StatementId;
                                                //foreach (var e in checkClassCallMethod)
                                                for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled != null)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                            checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                        {
                                                            if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                            {
                                                                int indexMethod =
                                                                    checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                        StringComparison.Ordinal);
                                                                if (indexMethod > 0)
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled.Substring(
                                                                                0, indexMethod)))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e].MethodCalled
                                                                                            .Substring(0, indexMethod);
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1).Replace("))", "");
                                                                    }

                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e].MethodCalled))
                                                                    {
                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e].MethodCalled;
                                                                        linkObject1.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1).Replace("))", "");
                                                                    }
                                                                }


                                                                if (linkObject1.Origin != linkObject1.Target)
                                                                {
                                                                    if (e == checkClassCallMethod.Count - 1)
                                                                    {
                                                                        lstLinks.Add(linkObject1);
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            #endregion
                                        }
                                    }
                                }
                            }

                            objFlowChart.Links = lstLinks;

                            #region Code to remove Missing Origin / missing target links

                            List<int> missingTargets =
                                lstLinks.Select(x => x.Target)
                                    .ToList()
                                    .Except(lstNodes.Select(y => y.Id))
                                    .ToList();
                            if (missingTargets.Count > 0)
                            {
                                for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets.Count; iNotlnkCnt++)
                                {
                                    lstLinks.RemoveAll(x => x.Target == missingTargets[iNotlnkCnt]);
                                }
                            }

                            List<int> missingOrigins =
                                lstLinks.Select(x => x.Origin)
                                    .ToList()
                                    .Except(lstNodes.Select(y => y.Id))
                                    .ToList();
                            if (missingOrigins.Count > 0)
                            {
                                for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins.Count; iNotlnkCnt++)
                                {
                                    lstLinks.RemoveAll(x => x.Origin == missingOrigins[iNotlnkCnt]);
                                }
                            }

                            List<int> missingTargets1 =
                                lstLinks.Select(x => x.Target)
                                    .ToList()
                                    .Except(lstNodes.Select(y => y.Id))
                                    .ToList();
                            if (missingTargets1.Count > 0)
                            {
                                for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets1.Count; iNotlnkCnt++)
                                {
                                    lstLinks.RemoveAll(x => x.Target == missingTargets1[iNotlnkCnt]);
                                }
                            }

                            List<int> missingOrigins1 =
                                lstLinks.Select(x => x.Origin)
                                    .ToList()
                                    .Except(lstNodes.Select(y => y.Id))
                                    .ToList();
                            if (missingOrigins1.Count > 0)
                            {
                                for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins1.Count; iNotlnkCnt++)
                                {
                                    lstLinks.RemoveAll(x => x.Origin == missingOrigins1[iNotlnkCnt]);
                                }
                            }

                            #endregion
                            #region Code to remove no links to the nodes

                            List<int> missingNodes =
                                lstNodes.Select(x => x.Id)
                                    .ToList()
                                    .Except(lstLinks.Select(y => y.Origin))
                                    .ToList();
                            if (missingNodes.Count > 0)
                            {
                                for (int iNotlnkCnt = 0; iNotlnkCnt < missingNodes.Count; iNotlnkCnt++)
                                {
                                    List<int> iCnt = (from dd in lstLinks
                                                      where dd.Target == missingNodes[iNotlnkCnt]
                                                      select dd.Target).ToList();
                                    if (iCnt.Count == 0)
                                    {
                                        lstNodes.RemoveAll(x => x.Id == missingNodes[iNotlnkCnt]);
                                    }
                                }
                            }
                            #endregion

                            #endregion
                        }
                        var startNodes = await _codeVortoService.ActionWorkflowsRepository
                           .GetDataFromSqlQuery<ActionWorkflows>(
                               " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is null;");
                        int tempI = 1;
                        var list =
                            startNodes.FindAll(
                                s => s.EndPointOrService != "Service");
                        foreach (var node in list)
                        {
                            Node nodeObject = new Node
                            {
                                StatementId = 99998 + tempI,
                                Id = 99998 + tempI,
                                Name = node.WorkflowName,
                                ShapeId = "RoundRect",
                                Height = "15",
                                Width = "100",
                                //Color = "#a9d18e"
                                Color = "#28C965"
                            };
                            lstNodes.Add(nodeObject);
                            tempI++;
                        }
                        //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
                        var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
                        //var enumerable = tempNodes.ToList();
                        foreach (var tNode in tempNodes)
                        {
                            foreach (var allNode in objFlowChart.Nodes)
                            {
                                var checkClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                       .GetDataFromSqlQuery<ActionWorkflows>(
                           " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is not null group by OriginFileName;");

                                foreach (var chkNodes in checkClassInNodes)
                                {
                                    if (allNode.Name == chkNodes.OriginFileName.Replace(".vb", ""))
                                    {
                                        int kk = allNode.StatementId;
                                        objFlowChart.Links.Add(new Link
                                        {
                                            Origin = tNode.Id,
                                            Target = kk
                                        });
                                    }
                                }
                            }
                        }

                        #region  Insert the ConnectivityStepReference & ConnectivityLinkReference
                        if (objFlowChart.Nodes.Count > 0)
                        {
                            var nodes = objFlowChart.Nodes.ToList();
                            foreach (var mNodes in nodes)
                            {
                                var nodesData = new ConnectivityStepReference
                                {
                                    Name = mNodes.Name,
                                    Shape = mNodes.ShapeId,
                                    Color = mNodes.Color,
                                    ParentId = mNodes.ParentId,
                                    StatementId = mNodes.StatementId,
                                    ProjectId = Convert.ToInt32(projectId)
                                };
                                await _codeVortoService.ConnectivityStepReferenceRepository.AddNewItem(nodesData);
                            }
                        }

                        if (objFlowChart.Links.Count > 0)
                        {
                            var links = objFlowChart.Links.ToList();
                            foreach (var mLinks in links)
                            {
                                    var liksData = new ConnectivityLinkReferece
                                    {
                                        Origin = mLinks.Origin,
                                        Target = mLinks.Target,
                                        LinkText = mLinks.LinkText,
                                        StatementId = mLinks.StatementId,
                                        ProjectId = Convert.ToInt32(projectId)

                                    };
                                    await _codeVortoService.ConnectivityLinkRefereceRepository.AddNewItem(liksData);
                            }
                        }
                        #endregion
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(ex.Message);
                    }
                }
                #endregion

                return objFlowChart;
            }
        }

        [HttpGet]
        public async Task<FlowChart> GetObjectReference(string projectId, string oDictionary)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var deadDatatypesRepository = new DeadDataTypesRepository(new AppDbContext());
                var filemasterRepository = new FileMasterRepository(new AppDbContext());
                List<Node> lstNodes = new List<Node>();
                var objFlowChart = new FlowChart();
                try
                {
                    #region Add Nodes Logic
                    // Added b4 changes...
                    var allClassData =
                        await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                            "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and ClassNameDeclared != 'null';");
                    foreach (var s in allClassData)
                    {
                        Node nodeObject = new Node();
                        //var chkFileSvcOrNot =
                        //    await
                        //        filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                        //            "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                        //            s.ClassNameDeclared.Trim() + ".svc';");
                        //var color = chkFileSvcOrNot.Count > 0 ? "#28C965" : "#F5BD6A";
                        //string clsName = s.ClassNameDeclared;
                        //if (color == "#28C965")
                        //    clsName = clsName + " WCF-SVC";

                        nodeObject.Id = s.StatementId;
                        //nodeObject.Name = clsName;
                        nodeObject.Name = s.ClassNameDeclared;
                        nodeObject.ShapeId = "RoundRect";
                        nodeObject.Height = "15";
                        nodeObject.Width = "100";
                        //nodeObject.Color = color;
                        nodeObject.Color = "#F5BD6A";
                        nodeObject.StatementId = s.StatementId;
                        nodeObject.BaseCommandId = s.BaseCommandId;
                        nodeObject.BusinessName = s.BusinessName;
                        lstNodes.Add(nodeObject);

                        var fileNewId = s.FileId;

                        //  Get the all Classes to file wise.
                        var allCallingClassData = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                "select * from statementreferencemaster where BaseCommandId=7" +
                                " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                fileNewId + "';");

                        foreach (var c in allCallingClassData)
                        {
                            var datatype = c.DataOrObjectType;
                            var allDeadDatatypes = await deadDatatypesRepository.GetDataFromSqlQuery<DeadDataTypes>(
                                "SELECT * FROM deaddatatypes");
                            int j = 0;
                            for (int i = 0; i < allDeadDatatypes.Count; i++)
                            {
                                var keywordName = allDeadDatatypes[i].KaywordName;
                                if (datatype.Contains(keywordName))
                                {
                                    j++;
                                }
                            }

                            if (j == 0)
                            {
                                // Check the class the present or not.
                                // Class is present to add the color : Yellow
                                // Class is not present to add the color : Gray 

                                var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                var checkClassPresentOrNot = await baseCommandReferenceRepository
                                    .GetDataFromSqlQuery<StatementReferenceMaster>(
                                        "SELECT * from statementreferencemaster where FileId In (" +
                                        " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                        "') and classNameDeclared like '%" + dataObject + "%';");
                                if (checkClassPresentOrNot.Count > 0)
                                {
                                    Node nodeObject1 = new Node
                                    {
                                        Id = c.StatementId,
                                        Name = c.DataOrObjectType,
                                        ShapeId = "RoundRect",
                                        Height = "15",
                                        Width = "100",
                                        Color = "#F5BD6A",
                                        StatementId = c.StatementId,
                                        BaseCommandId = c.BaseCommandId,
                                        BusinessName = c.BusinessName
                                    };


                                    List<int> iCnt = (from dd in lstNodes
                                                      where dd.Name == nodeObject1.Name
                                                      select dd.Id).ToList();
                                    if (iCnt.Count == 0)
                                    {
                                        lstNodes.Add(nodeObject1);
                                    }
                                }
                                else
                                {
                                    Node nodeObject2 = new Node
                                    {
                                        Id = c.StatementId,
                                        Name = c.DataOrObjectType,
                                        ShapeId = "RoundRect",
                                        Height = "15",
                                        Width = "100",
                                        Color = "#C0C0C0",
                                        StatementId = c.StatementId,
                                        BaseCommandId = c.BaseCommandId,
                                        BusinessName = c.BusinessName,
                                        FileId = c.FileId
                                    };

                                    List<int> iCnt = (from dd in lstNodes
                                                      where dd.Name == nodeObject2.Name && dd.FileId == nodeObject2.FileId
                                                      select dd.Id).ToList();
                                    if (iCnt.Count == 0)
                                    {
                                        lstNodes.Add(nodeObject2);
                                    }

                                }
                            }
                        }
                    }
                    objFlowChart.Nodes = lstNodes;

                    #endregion

                    if (lstNodes.Count > 0)
                    {
                        #region Add Links

                        #region links variables

                        var lstLinks = new List<Link>();

                        #endregion

                        var allClassDataLinks = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                "SELECT * FROM statementreferencemaster where ProjectId='" + projectId + "' and ClassNameDeclared!='null'");

                        foreach (var s in allClassDataLinks)
                        {
                            var iDofNode = s.StatementId;
                            var fileId = s.FileId;
                            var bName = s.BusinessName;
                            // Get the file wise "Dim" started classes
                            var allCallingClassDataLinks = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "select * from statementreferencemaster where BaseCommandId=7" +
                                    " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                    fileId + "';");

                            foreach (var c in allCallingClassDataLinks)
                            {
                                var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                var calledDataObject = c.DataOrObjectType;
                                var statementId = c.StatementId;
                                var allDeadDatatypesLinks = await deadDatatypesRepository
                                    .GetDataFromSqlQuery<DeadDataTypes>(
                                        "SELECT * FROM deaddatatypes");

                                // Check the dead datatype in classes or Not

                                int j = 0;
                                for (int i = 0; i < allDeadDatatypesLinks.Count; i++)
                                {
                                    var keywordName = allDeadDatatypesLinks[i].KaywordName;
                                    if (dataObject != null && dataObject.Contains(keywordName))
                                    {
                                        j++;
                                    }

                                }
                                if (j == 0)
                                {
                                    // Check the Class id called or not.
                                    var checkClassPresentOrNotLink = await baseCommandReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "SELECT * from statementreferencemaster where FileId In (" +
                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                            "') and classNameDeclared like '%" + dataObject + "%';");

                                    if (checkClassPresentOrNotLink.Count > 0)
                                    {
                                        foreach (var f in checkClassPresentOrNotLink)
                                        {

                                            var checkClassCallMethod = await baseCommandReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "SELECT * from statementreferencemaster where FileId In (" +
                                                    " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                    "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
                                                    "';");

                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var linkObject = new Link
                                                {
                                                    Origin = iDofNode,
                                                    Target = f.StatementId,
                                                    BaseCommandId = f.BaseCommandId,
                                                    StatementId = f.StatementId,
                                                    BusinessName = f.BusinessName
                                                };
                                                for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled != null)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                            checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                        {
                                                            if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                            {
                                                                int indexMethod =
                                                                    checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                        StringComparison.Ordinal);
                                                                if (indexMethod > 0)
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e]
                                                                                .MethodCalled.Substring(0,
                                                                                    indexMethod)))
                                                                    {

                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled.Substring(0,
                                                                                                indexMethod);
                                                                        linkObject.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e]
                                                                                .MethodCalled))
                                                                    {

                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled;
                                                                        linkObject.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }


                                                                if (linkObject.Origin != linkObject.Target)
                                                                {
                                                                    if (e == checkClassCallMethod.Count - 1)
                                                                    {
                                                                        lstLinks.Add(linkObject);
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        #region Class Is not Calling to add the Class as Misssing Element

                                        var checkClassCallMethod = await baseCommandReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "select * from statementreferencemaster where fileid= '" + fileId +
                                                "' and statementId > '" + statementId + "' and" +
                                                " statementId < (select statementId from statementreferencemaster where fileid= '" +
                                                fileId + "' and statementId > '" + statementId + "'" +
                                                " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                "') and MethodCalled is not null;");

                                        if (checkClassCallMethod.Count > 0)
                                        {
                                            string mulMethodName = "";
                                            Link linkObject1 = new Link
                                            {
                                                Origin = iDofNode,
                                                Target = statementId,
                                                BusinessName = bName
                                            };
                                            //linkObject.target = c.StatementId;
                                            //foreach (var e in checkClassCallMethod)
                                            for (int e = 0; e < checkClassCallMethod.Count; e++)
                                            {

                                                if (checkClassCallMethod[e].MethodCalled != null)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                        checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                    {
                                                        if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                            !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                        {
                                                            int indexMethod =
                                                                checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                    StringComparison.Ordinal);
                                                            if (indexMethod > 0)
                                                            {

                                                                //linkObject1.linkText = e.MethodCalled.Substring(0, indexMethod);
                                                                if (
                                                                    !mulMethodName.Contains(
                                                                        checkClassCallMethod[e].MethodCalled.Substring(
                                                                            0, indexMethod)))
                                                                {
                                                                    mulMethodName = mulMethodName + ", " +
                                                                                    checkClassCallMethod[e].MethodCalled
                                                                                        .Substring(0, indexMethod);
                                                                    linkObject1.LinkText =
                                                                        mulMethodName.Substring(1,
                                                                            mulMethodName.Length - 1).Replace("))", "");
                                                                }

                                                            }
                                                            else
                                                            {

                                                                //linkObject1.linkText = e.MethodCalled.ToString();
                                                                if (
                                                                    !mulMethodName.Contains(
                                                                        checkClassCallMethod[e].MethodCalled))
                                                                {
                                                                    mulMethodName = mulMethodName + ", " +
                                                                                    checkClassCallMethod[e].MethodCalled;
                                                                    linkObject1.LinkText =
                                                                        mulMethodName.Substring(1,
                                                                            mulMethodName.Length - 1).Replace("))", "");
                                                                }
                                                            }


                                                            if (linkObject1.Origin != linkObject1.Target)
                                                            {
                                                                if (e == checkClassCallMethod.Count - 1)
                                                                {
                                                                    lstLinks.Add(linkObject1);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        #endregion
                                    }
                                }
                            }
                        }

                        objFlowChart.Links = lstLinks;

                        #region Code to remove Missing Origin / missing target links

                        List<int> missingTargets =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Target == missingTargets[iNotlnkCnt]);
                            }
                        }

                        List<int> missingOrigins =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Origin == missingOrigins[iNotlnkCnt]);
                            }
                        }

                        List<int> missingTargets1 =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets1.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingTargets1.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Target == missingTargets1[iNotlnkCnt]);
                            }
                        }

                        List<int> missingOrigins1 =
                            lstLinks.Select(x => x.Origin)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins1.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingOrigins1.Count; iNotlnkCnt++)
                            {
                                lstLinks.RemoveAll(x => x.Origin == missingOrigins1[iNotlnkCnt]);
                            }
                        }

                        #endregion

                        #region Code to remove no links to the nodes

                        List<int> missingNodes =
                            lstNodes.Select(x => x.Id)
                                .ToList()
                                .Except(lstLinks.Select(y => y.Origin))
                                .ToList();
                        if (missingNodes.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingNodes.Count; iNotlnkCnt++)
                            {
                                List<int> iCnt = (from dd in lstLinks
                                                  where dd.Target == missingNodes[iNotlnkCnt]
                                                  select dd.Target).ToList();
                                if (iCnt.Count == 0)
                                {
                                    lstNodes.RemoveAll(x => x.Id == missingNodes[iNotlnkCnt]);
                                }
                            }
                        }

                        #endregion

                        #endregion
                    }

                    #region Commentted Old Code
                    //var startNodes = await baseCommandReferenceRepository
                    //   .GetDataFromSqlQuery<ActionWorkflows>(
                    //       " SELECT * FROM actionworkflows group by OriginObject, EndPointOrService; ");
                    //int tempI = 1;
                    //var list =
                    //    startNodes.FindAll(
                    //        s => s.EndPointOrService != "Service");
                    //foreach (var node in list)
                    //{
                    //    Node nodeObject = new Node
                    //    {
                    //        StatementId = 99998 + tempI,
                    //        Id = 99998 + tempI,
                    //        Name = node.EndPointOrService,
                    //        ShapeId = "RoundRect",
                    //        Height = "15",
                    //        Width = "100",
                    //        Color = "#a9d18e"
                    //    };
                    //    lstNodes.Add(nodeObject);
                    //    tempI++;
                    //}
                    //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
                    //var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
                    ////var enumerable = tempNodes.ToList();
                    //foreach (var tNode in tempNodes)
                    //{
                    //    foreach (var rNodes in remainingNodes)
                    //    {
                    //        foreach (var allNode in objFlowChart.Nodes)
                    //        {
                    //            string oName = rNodes.OriginObject.Split('.').LastOrDefault();
                    //            if (oName != allNode.Name || allNode.Name == "RetrieveReport") continue;
                    //            int kk = allNode.StatementId;
                    //            objFlowChart.Links.Add(new Link
                    //            {
                    //                Origin = tNode.Id,
                    //                Target = kk
                    //            });
                    //        }
                    //    }
                    //}
                    #endregion

                    var startNodes = await _codeVortoService.ActionWorkflowsRepository
                          .GetDataFromSqlQuery<ActionWorkflows>(
                              " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is null;");
                    int tempI = 1;
                    var list =
                        startNodes.FindAll(
                            s => s.EndPointOrService != "Service");
                    foreach (var node in list)
                    {
                        Node nodeObject = new Node
                        {
                            StatementId = 99998 + tempI,
                            Id = 99998 + tempI,
                            Name = node.WorkflowName,
                            ShapeId = "RoundRect",
                            Height = "15",
                            Width = "100",
                            //Color = "#a9d18e"
                            Color = "#28C965"
                        };
                        lstNodes.Add(nodeObject);
                        tempI++;
                    }
                    //var remainingNodes = startNodes.FindAll(s => s.EndPointOrService == "Service");
                    var tempNodes = objFlowChart.Nodes.FindAll(n => n.StatementId >= 99999);
                    //var enumerable = tempNodes.ToList();
                    foreach (var tNode in tempNodes)
                    {
                        //foreach (var rNodes in remainingNodes)
                        //{
                        foreach (var allNode in objFlowChart.Nodes)
                        {

                            var CheckClassInNodes = await _codeVortoService.ActionWorkflowsRepository
                   .GetDataFromSqlQuery<ActionWorkflows>(
                       " select * from actionworkFlows where ProjectId=" + projectId + " and EndPointOrService is not null group by OriginFileName;");

                            foreach (var chkNodes in CheckClassInNodes)
                            {
                                //string oName = rNodes.OriginObject.Split('.').LastOrDefault();
                                if (allNode.Name == chkNodes.OriginFileName.Replace(".vb", ""))
                                {
                                    int kk = allNode.StatementId;
                                    objFlowChart.Links.Add(new Link
                                    {
                                        Origin = tNode.Id,
                                        Target = kk
                                    });
                                }
                            }
                        }
                        //}
                    }

                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }


                return objFlowChart;
            }
        }

        [HttpGet]
        public async Task<FlowChart> GetObjectDictionaryData(int projectId)
        {
            using (_codeVortoService = new CodeVortoService())
            {
                var baseCommandReferenceRepository = new BaseCommandReferenceRepository(new AppDbContext());
                var deadDatatypesRepository = new DeadDataTypesRepository(new AppDbContext());
                var filemasterRepository = new FileMasterRepository(new AppDbContext());
                List<Node> lstNodes = new List<Node>();
                var objFlowChart = new FlowChart();
                try
                {
                    #region Add Nodes Logic
                    var allClassData =
                        await baseCommandReferenceRepository.GetDataFromSqlQuery<StatementReferenceMaster>(
                            "SELECT * FROM statementreferencemaster where ClassNameDeclared != 'null';");
                    foreach (var s in allClassData)
                    {
                        Node nodeObject = new Node();
                        var chkFileSvcOrNot =
                            await
                                filemasterRepository.GetDataFromSqlQuery<FileMaster>(
                                    "select * from filemaster where ProjectId= " + projectId + " and FileName='" +
                                    s.ClassNameDeclared.Trim() + ".svc';");
                        var color = chkFileSvcOrNot.Count > 0 ? "#28C965" : "#F5BD6A";

                        string clsName = s.ClassNameDeclared;
                        if (color == "#28C965")
                            clsName = clsName + " WCF-SVC";

                        nodeObject.Id = s.StatementId;
                        nodeObject.Name = clsName;
                        nodeObject.ShapeId = "RoundRect";
                        nodeObject.Height = "15";
                        nodeObject.Width = "100";
                        nodeObject.Color = color;
                        nodeObject.StatementId = s.StatementId;
                        nodeObject.BaseCommandId = s.BaseCommandId;
                        lstNodes.Add(nodeObject);
                        var fileNewId = s.FileId;

                        //  Get the all Classes to file wise.
                        var allCallingClassData = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                "select * from statementreferencemaster where BaseCommandId=7" +
                                " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                fileNewId + "';");

                        foreach (var c in allCallingClassData)
                        {
                            var datatype = c.DataOrObjectType;
                            var allDeadDatatypes = await deadDatatypesRepository.GetDataFromSqlQuery<DeadDataTypes>(
                                "SELECT * FROM deaddatatypes");
                            int j = 0;
                            for (int i = 0; i < allDeadDatatypes.Count; i++)
                            {
                                var keywordName = allDeadDatatypes[i].KaywordName;
                                if (datatype.Contains(keywordName))
                                {
                                    j++;
                                }
                            }

                            if (j != 0) continue;
                            // Check the class the present or not.
                            // Class is present to add the color : Yellow
                            // Class is not present to add the color : Gray 

                            var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                            var checkClassPresentOrNot = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "SELECT * from statementreferencemaster where FileId In (" +
                                    " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                    "') and classNameDeclared like '%" + dataObject + "%';");
                            if (checkClassPresentOrNot.Count > 0)
                            {
                                Node nodeObject1 = new Node
                                {
                                    Id = c.StatementId,
                                    Name = c.DataOrObjectType,
                                    ShapeId = "RoundRect",
                                    Height = "15",
                                    Width = "100",
                                    Color = "#F5BD6A",
                                    StatementId = c.StatementId,
                                    BaseCommandId = c.BaseCommandId
                                };


                                List<int> iCnt = (from dd in lstNodes
                                                  where dd.Name == nodeObject1.Name
                                                  select dd.Id).ToList();
                                if (iCnt.Count == 0)
                                {
                                    lstNodes.Add(nodeObject1);
                                }
                            }
                            else
                            {
                                Node nodeObject2 = new Node
                                {
                                    Id = c.StatementId,
                                    Name = c.DataOrObjectType,
                                    ShapeId = "RoundRect",
                                    Height = "15",
                                    Width = "100",
                                    Color = "#C0C0C0",
                                    StatementId = c.StatementId,
                                    BaseCommandId = c.BaseCommandId
                                };

                                List<int> iCnt = (from dd in lstNodes
                                                  where dd.Name == nodeObject2.Name
                                                  select dd.Id).ToList();
                                if (iCnt.Count == 0)
                                {
                                    lstNodes.Add(nodeObject2);
                                }

                            }
                        }
                    }
                    objFlowChart.Nodes = lstNodes;

                    #endregion

                    if (lstNodes.Count > 0)
                    {
                        #region Add Links

                        #region links variables

                        var lstLinks = new List<Link>();

                        #endregion

                        var allClassDataLinks = await baseCommandReferenceRepository
                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                "SELECT * FROM statementreferencemaster where ClassNameDeclared!='null'");

                        foreach (var s in allClassDataLinks)
                        {
                            var iDofNode = s.StatementId;
                            var fileId = s.FileId;
                            // Get the file wise "Dim" started classes
                            var allCallingClassDataLinks = await baseCommandReferenceRepository
                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                    "select * from statementreferencemaster where BaseCommandId=7" +
                                    " and DataOrObjectType Not In (SELECT KaywordName from deaddatatypes) and FileId='" +
                                    fileId + "';");

                            foreach (var c in allCallingClassDataLinks)
                            {
                                var dataObject = c.DataOrObjectType.Split('.').LastOrDefault();
                                var calledDataObject = c.DataOrObjectType;
                                var statementId = c.StatementId;
                                var allDeadDatatypesLinks = await deadDatatypesRepository
                                    .GetDataFromSqlQuery<DeadDataTypes>(
                                        "SELECT * FROM deaddatatypes");

                                // Check the dead datatype in classes or Not

                                int j = 0;
                                for (int i = 0; i < allDeadDatatypesLinks.Count; i++)
                                {
                                    var keywordName = allDeadDatatypesLinks[i].KaywordName;
                                    if (dataObject != null && dataObject.Contains(keywordName))
                                    {
                                        j++;
                                    }

                                }
                                if (j == 0)
                                {
                                    // Check the Class id called or not.
                                    var checkClassPresentOrNotLink = await baseCommandReferenceRepository
                                        .GetDataFromSqlQuery<StatementReferenceMaster>(
                                            "SELECT * from statementreferencemaster where FileId In (" +
                                            " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                            "') and classNameDeclared like '%" + dataObject + "%';");

                                    if (checkClassPresentOrNotLink.Count > 0)
                                    {
                                        foreach (var f in checkClassPresentOrNotLink)
                                        {

                                            var checkClassCallMethod = await baseCommandReferenceRepository
                                                .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                    "SELECT * from statementreferencemaster where FileId In (" +
                                                    " SELECT FileId FROM filemaster where ProjectId='" + projectId +
                                                    "') and BasecommandId= 6 and ClassCalled='" + calledDataObject +
                                                    "';");

                                            if (checkClassCallMethod.Count > 0)
                                            {
                                                string mulMethodName = "";
                                                var linkObject = new Link
                                                {
                                                    Origin = iDofNode,
                                                    Target = f.StatementId,
                                                    BaseCommandId = f.BaseCommandId,
                                                    StatementId = f.StatementId
                                                };
                                                for (int e = 0; e < checkClassCallMethod.Count; e++)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled != null)
                                                    {
                                                        if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                            checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                        {
                                                            if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                                !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                            {
                                                                int indexMethod =
                                                                    checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                        StringComparison.Ordinal);
                                                                if (indexMethod > 0)
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e]
                                                                                .MethodCalled.Substring(0,
                                                                                    indexMethod)))
                                                                    {

                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled.Substring(0,
                                                                                                indexMethod);
                                                                        linkObject.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    if (
                                                                        !mulMethodName.Contains(
                                                                            checkClassCallMethod[e]
                                                                                .MethodCalled))
                                                                    {

                                                                        mulMethodName = mulMethodName + ", " +
                                                                                        checkClassCallMethod[e]
                                                                                            .MethodCalled;
                                                                        linkObject.LinkText =
                                                                            mulMethodName.Substring(1,
                                                                                mulMethodName.Length - 1)
                                                                                .Replace("))", "");
                                                                    }
                                                                }


                                                                if (linkObject.Origin != linkObject.Target)
                                                                {
                                                                    if (e == checkClassCallMethod.Count - 1)
                                                                    {
                                                                        lstLinks.Add(linkObject);
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        #region Class Is not Calling to add the Class as Misssing Element

                                        var checkClassCallMethod = await baseCommandReferenceRepository
                                            .GetDataFromSqlQuery<StatementReferenceMaster>(
                                                "select * from statementreferencemaster where fileid= '" + fileId +
                                                "' and statementId > '" + statementId + "' and" +
                                                " statementId < (select statementId from statementreferencemaster where fileid= '" +
                                                fileId + "' and statementId > '" + statementId + "'" +
                                                " and BasecommandId=9 limit 1) and VariableNameDeclared In (select VariableNameDeclared from statementreferencemaster" +
                                                " where fileid= '" + fileId + "' and statementId ='" + statementId +
                                                "') and MethodCalled is not null;");

                                        if (checkClassCallMethod.Count > 0)
                                        {
                                            string mulMethodName = "";
                                            Link linkObject1 = new Link
                                            {
                                                Origin = iDofNode,
                                                Target = statementId
                                            };
                                            //linkObject.target = c.StatementId;
                                            //foreach (var e in checkClassCallMethod)
                                            for (int e = 0; e < checkClassCallMethod.Count; e++)
                                            {

                                                if (checkClassCallMethod[e].MethodCalled != null)
                                                {
                                                    if (checkClassCallMethod[e].MethodCalled.Trim().Contains("(") &&
                                                        checkClassCallMethod[e].MethodCalled.Trim().EndsWith(")"))
                                                    {
                                                        if (!checkClassCallMethod[e].MethodCalled.Contains("=") &&
                                                            !checkClassCallMethod[e].MethodCalled.StartsWith("<"))
                                                        {
                                                            int indexMethod =
                                                                checkClassCallMethod[e].MethodCalled.IndexOf("(",
                                                                    StringComparison.Ordinal);
                                                            if (indexMethod > 0)
                                                            {

                                                                //linkObject1.linkText = e.MethodCalled.Substring(0, indexMethod);
                                                                if (
                                                                    !mulMethodName.Contains(
                                                                        checkClassCallMethod[e].MethodCalled.Substring(
                                                                            0, indexMethod)))
                                                                {
                                                                    mulMethodName = mulMethodName + ", " +
                                                                                    checkClassCallMethod[e].MethodCalled
                                                                                        .Substring(0, indexMethod);
                                                                    linkObject1.LinkText =
                                                                        mulMethodName.Substring(1,
                                                                            mulMethodName.Length - 1).Replace("))", "");
                                                                }

                                                            }
                                                            else
                                                            {

                                                                //linkObject1.linkText = e.MethodCalled.ToString();
                                                                if (
                                                                    !mulMethodName.Contains(
                                                                        checkClassCallMethod[e].MethodCalled))
                                                                {
                                                                    mulMethodName = mulMethodName + ", " +
                                                                                    checkClassCallMethod[e].MethodCalled;
                                                                    linkObject1.LinkText =
                                                                        mulMethodName.Substring(1,
                                                                            mulMethodName.Length - 1).Replace("))", "");
                                                                }
                                                            }


                                                            if (linkObject1.Origin != linkObject1.Target)
                                                            {
                                                                if (e == checkClassCallMethod.Count - 1)
                                                                {
                                                                    lstLinks.Add(linkObject1);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        #endregion
                                    }
                                }
                            }
                        }

                        objFlowChart.Links = lstLinks;

                        #region Code to remove Missing Origin / missing target links

                        List<int> missingTargets =
                            lstLinks.Select(x => x.Target)
                                .ToList()
                                .Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets.Count > 0)
                        {
                            foreach (int t in missingTargets)
                            {
                                lstLinks.RemoveAll(x => x.Target == t);
                            }
                        }

                        List<int> missingOrigins =
                            lstLinks.Select(x => x.Origin)
                                .ToList().Except(lstNodes.Select(y => y.Id)).ToList();
                        if (missingOrigins.Count > 0)
                        {
                            foreach (int t in missingOrigins)
                            {
                                lstLinks.RemoveAll(x => x.Origin == t);
                            }
                        }

                        List<int> missingTargets1 =
                            lstLinks.Select(x => x.Target)
                                .ToList().Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingTargets1.Count > 0)
                        {
                            foreach (int t in missingTargets1)
                            {
                                lstLinks.RemoveAll(x => x.Target == t);
                            }
                        }

                        List<int> missingOrigins1 =
                            lstLinks.Select(x => x.Origin)
                                .ToList().Except(lstNodes.Select(y => y.Id))
                                .ToList();
                        if (missingOrigins1.Count > 0)
                        {
                            foreach (int t in missingOrigins1)
                            {
                                lstLinks.RemoveAll(x => x.Origin == t);
                            }
                        }

                        #endregion

                        #region Code to remove no links to the nodes

                        List<int> missingNodes =
                            lstNodes.Select(x => x.Id)
                                .ToList()
                                .Except(lstLinks.Select(y => y.Origin))
                                .ToList();
                        if (missingNodes.Count > 0)
                        {
                            for (int iNotlnkCnt = 0; iNotlnkCnt < missingNodes.Count; iNotlnkCnt++)
                            {
                                List<int> iCnt = (from dd in lstLinks
                                                  where dd.Target == missingNodes[iNotlnkCnt]
                                                  select dd.Target).ToList();
                                if (iCnt.Count == 0)
                                {
                                    lstNodes.RemoveAll(x => x.Id == missingNodes[iNotlnkCnt]);
                                }
                            }
                        }
                        #endregion

                        #endregion
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
                List<string> lstClasses = new List<string>();
                List<string> lstMethods = new List<string>();
                List<string> combineList = new List<string>();
                foreach (var link in objFlowChart.Links)
                {
                    int cls = link.Target;
                    string className = string.Empty;
                    Node node = (from n in objFlowChart.Nodes where n.Id == cls select n).ToList().First();
                    if (node != null)
                        className = node.Name;
                    if (className.Contains(" WCF-SVC"))
                        className = className.Replace(" WCF-SVC", "");
                    string[] method = link.LinkText.Split(',');
                    lstClasses.Add(className.Trim());
                    lstMethods.AddRange(method.Select(m => m.Trim()));
                    combineList.AddRange(method.Select(m => className.Trim() + "~" + m.Trim()));
                }
                foreach (var node in objFlowChart.Nodes)
                {
                    var k = (from f in lstClasses where f == node.Name select f).ToList();
                    if (k.Count == 0)
                    {
                        lstClasses.Add(node.Name);
                        combineList.Add(node.Name);
                    }
                }

                objFlowChart.Classes = lstClasses.Distinct().ToList();
                objFlowChart.Methods = lstMethods.Distinct().ToList();
                objFlowChart.CombineList = combineList.Distinct().ToList();

                return objFlowChart;
            }
        }

        [HttpPost]
        public string GetLinkToDownloadFlowChart(FlowChart flowchartObject, int flag, string fileNameToCreate)
        {
            string datetime = DateTime.Now.ToString("MM-dd-yyyy_HH-mm-ss");
            string FileNameNew = flowchartObject.Nodes[0].Name;
            string strGlobalPath = string.Empty;
            string UserName = string.Empty;
            string fileName = string.Empty;
            string applicationName = string.Empty;
            string sourceFileName = string.Empty;

            //string strQry = "select LookupValue from LookupReference where LookupIndexType='DOCITEMS' and LookupName = 'Reports' ";
            //string strQryUserName = "select UserName from UserMaster where UserId=" + UserId + "";

            //strGlobalPath = objDataBaseService.ReadScalarValueSql(strQry);
            //UserName = objDataBaseService.ReadScalarValueSql(strQryUserName);


            //logForNet.Debug("Reports direcotry path in the database is - " + strGlobalPath);

            //strGlobalPath = strGlobalPath + UserName + "\\XML\\";

            // strGlobalPath = strGlobalPath + "" + saveUrlPath + "";

            strGlobalPath = "C:\\inetpub\\wwwroot\\VERTO\\Reports\\XML\\";

            if (!Directory.Exists(strGlobalPath))
            {
                Directory.CreateDirectory(strGlobalPath);
            }

            #region GetApplicationName

            //string strQryApplicationName = "select ApplicationName from ApplicationMaster where ApplicationID='" +
            //applicationName = objDataBaseService.ReadScalarValueSql(strQryApplicationName);

            //if (applicationName == "0")
            //{
            //    applicationName = "_";
            //}
            //else
            //{
            //    applicationName = applicationName + "_";
            //}

            #endregion
            if ((fileNameToCreate == "ObjectConnectivity_"))
            {
                sourceFileName = "";
            }
            else
            {

                //sourceFileName = saveUrlPath + "_";
            }

            fileName = FileNameNew;


            string linkToReturn = string.Empty;
            if (flag == 1)
            {
                /* Download Visio & PDF */
                //linkToReturn = GeneratePdfForFlowchart(flowchartObject, FileNameToCreate, datetime, strGlobalPath);
            }
            else
            {

                ClsGraphMLEngine objGraphMlEngine = null;
                // add logic to call GraphMLEngine class for all the nodes
                // save the file
                // return the link
                if (fileName != null && flowchartObject != null)
                {
                    objGraphMlEngine = new ClsGraphMLEngine();
                    string nodeText = string.Empty;
                    string linkText = string.Empty;

                    // 1.---- Call GetHeader()
                    string headerText = objGraphMlEngine.getHeader(fileName);

                    // 2.---- Call getFooter()
                    string footerText = objGraphMlEngine.getFooter();

                    // 3.---- Call getNodeStream() for each Nodes
                    for (int iObjFC = 0; iObjFC < flowchartObject.Nodes.Count; iObjFC++)
                    {
                        //typeID, sNodeLabel, sNodeIDText, width, height
                        string sNodeLabel = flowchartObject.Nodes[iObjFC].Name.Replace("'", "&apos;").Replace(">", "&gt;").Replace("<", "&lt;").Replace("\"", "&quot;").Replace("&", "&amp;");
                        string sNodeIDText = flowchartObject.Nodes[iObjFC].Id.ToString().Replace("'", "&apos;").Replace(">", "&gt;").Replace("<", "&lt;").Replace("\"", "&quot;").Replace("&", "&amp;");
                        /* calculate width n height dynamically - as the values from passed object doesnt fit the items in yEd_Graph_Editor */
                        string width = string.Empty;
                        string height = string.Empty;
                        string colorOfNode = flowchartObject.Nodes[iObjFC].Color;
                        if (sNodeLabel != "" && sNodeLabel != string.Empty)
                        {
                            int charCountOfText = sNodeLabel.Length;
                            if (charCountOfText > 0)
                            {
                                if (flowchartObject.Nodes[iObjFC].Height == null)
                                {
                                    flowchartObject.Nodes[iObjFC].Height = "15";
                                }

                                int widthCalculation = (charCountOfText * 10);
                                width = widthCalculation.ToString();
                                int heightCalculation = int.Parse(flowchartObject.Nodes[iObjFC].Height);
                                heightCalculation = heightCalculation + 10;
                                height = heightCalculation.ToString();
                            }
                        }
                        //width = flowchartObject.nodes[iObjFC].width.ToString();
                        //height = flowchartObject.nodes[iObjFC].height.ToString();

                        string nodetype = flowchartObject.Nodes[iObjFC].ShapeId;
                        string nodeTypeToPass = string.Empty;

                        if (iObjFC == 0)
                        {
                            nodeText += objGraphMlEngine.getNodeStream(ClsGraphMLEngine.NODE_TYPE.START, sNodeLabel, sNodeIDText, int.Parse(width), int.Parse(height), colorOfNode);
                        }
                        else if ((iObjFC) == (flowchartObject.Nodes.Count - 1))
                        {
                            nodeText += objGraphMlEngine.getNodeStream(ClsGraphMLEngine.NODE_TYPE.END, sNodeLabel, sNodeIDText, int.Parse(width), int.Parse(height), colorOfNode);
                        }
                        else if (nodetype == "Rhombus" || nodetype == "Decision")
                        {
                            int charCountOfText = sNodeLabel.Length;
                            if (charCountOfText > 0)
                            {
                                int widthCalculation = (charCountOfText * 2);
                                if (widthCalculation > 200 || charCountOfText > 100)
                                {
                                    widthCalculation = widthCalculation * 4;
                                }
                                else
                                {
                                    widthCalculation = widthCalculation * 4;
                                }

                                if (widthCalculation < 200)
                                {
                                    widthCalculation = 300;
                                }

                                width = widthCalculation.ToString();
                                int heightCalculation = 0;
                                if (charCountOfText > 100)
                                {
                                    heightCalculation = charCountOfText - 40;
                                }
                                else
                                {
                                    heightCalculation = charCountOfText;
                                }

                                if (heightCalculation < 30)
                                {
                                    heightCalculation = 35;
                                }
                                height = heightCalculation.ToString();
                            }

                            nodeText += objGraphMlEngine.getNodeStream(ClsGraphMLEngine.NODE_TYPE.DECISON, sNodeLabel, sNodeIDText, int.Parse(width), int.Parse(height), colorOfNode);
                        }
                        else
                        {
                            nodeText += objGraphMlEngine.getNodeStream(ClsGraphMLEngine.NODE_TYPE.RECTANGLE, sNodeLabel, sNodeIDText, int.Parse(width), int.Parse(height), colorOfNode);
                        }

                    }

                    // 4.---- Call returnLinkConstruct() for each link
                    for (int iObjFC = 0; iObjFC < flowchartObject.Links.Count; iObjFC++)
                    {
                        //orginID, destID, sLinkLabel, sLinkIDText, sLinkColor
                        string orginID = flowchartObject.Links[iObjFC].Origin.ToString();
                        string destID = flowchartObject.Links[iObjFC].Target.ToString();
                        string sLinkLabel;
                        try
                        {
                            sLinkLabel = flowchartObject.Links[iObjFC].LinkText;
                        }
                        catch (Exception ex)
                        {
                            sLinkLabel = "NONE";
                            Console.WriteLine(ex.InnerException);
                        }

                        string sLinkIdText = iObjFC.ToString();
                        string sLinkColor = "000000";
                        if (!string.IsNullOrEmpty(sLinkLabel) && !string.IsNullOrEmpty(sLinkIdText))
                        {
                            linkText += objGraphMlEngine.returnLinkConstruct(orginID, destID, sLinkLabel.Trim(),
                                sLinkIdText, sLinkColor);
                        }
                        else
                        {
                            linkText += objGraphMlEngine.returnLinkConstruct(orginID, destID, "",
                                sLinkIdText, sLinkColor);
                        }
                    }

                    // 5.---- Create a file on server with all the Above data and save that file to the location
                    //        return that location+FileName in linkToReturn

                    string dataToWriteInFile = headerText + nodeText + linkText + footerText;
                    string tmpFN = string.Empty;
                    string finalFileName = string.Empty;
                    try
                    {
                        //Flowchart_<<program name>>_<<para name>>_<<userid>>_<<sessionid>>.graphml
                        // tmpFN = "Flowchart_" + fileName + "_" + ".graphml";
                        tmpFN = fileNameToCreate + applicationName + sourceFileName + datetime + ".graphml";
                        finalFileName = strGlobalPath + tmpFN;

                        //logForNet.Debug("Generating " + FileNameToCreate + " under directory " + finalFileName);

                        FileStream fileGraphML = new FileStream(finalFileName, FileMode.Create, FileAccess.ReadWrite, FileShare.Write);
                        fileGraphML.Close();
                        linkToReturn = finalFileName;
                        //fileGraphML.Flush();
                        fileGraphML.Dispose();
                        fileGraphML = null;
                    }
                    catch (Exception ee)
                    {
                        Console.WriteLine(ee.InnerException);
                    }

                    using (StreamWriter stremWrite = new StreamWriter(finalFileName))
                    {
                        stremWrite.WriteLine(dataToWriteInFile);
                    }


                    #region-- getting ImagePath --
                    //string strQryLP = "select LookupValue from LookupReference where LookupIndexType='DOCITEMS' and LookupName = 'IISReportPath' ";
                    string strLocalPath = string.Empty;


                    //strLocalPath = objDataBaseService.ReadScalarValueSql(strQryLP);

                    #region Server Path
                    strLocalPath = "http:\\\\ec2-52-39-126-42.us-west-2.compute.amazonaws.com\\VERTO\\Reports\\XML\\";
                    #endregion

                    #region LocalPath Path
                    //strLocalPath = "http:\\\\localhost\\VERTO\\Reports\\XML\\";
                    #endregion

                    strLocalPath = strLocalPath + UserName;

                    #endregion

                    linkToReturn = linkToReturn.Replace(strGlobalPath, strLocalPath);
                    //linkToReturn = linkToReturn + strLocalPath;
                }
            }


            return linkToReturn;
        }

        [HttpPost]
        public async Task<IHttpActionResult> PostObjectDictionaryData([FromBody] DictionaryData dictionaryData)
        {
            if (dictionaryData == null)
                return BadRequest("Request is invalid");

            using (_codeVortoService = new CodeVortoService())
            {
                List<string> classes = dictionaryData.Classes;
                List<string> methods = dictionaryData.Methods;
                foreach (var pClass in classes)
                {
                    string[] className = pClass.Split('~');
                    string sqlQuery = "SELECT * FROM statementreferencemaster where ClassNameDeclared = '" +
                                      className[0] + "' AND BaseCommandId = 19; ";
                    var statmentMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                    if (statmentMaster.Count <= 0) continue;
                    foreach (var stmtMaster in statmentMaster)
                    {
                        stmtMaster.BusinessName = className[1];
                        stmtMaster.BusinessDescription = className[2];
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stmtMaster);
                    }
                }
                foreach (var pMethod in methods)
                {
                    string[] methodName = pMethod.Split('~');
                    string sqlQuery = "SELECT * FROM statementreferencemaster where MethodName = '" + methodName[0] +
                                      "' AND BaseCommandId = 8;";
                    var statmentMaster = await _codeVortoService.StatementReferenceMasterRepository
                        .GetDataFromSqlQuery<StatementReferenceMaster>(sqlQuery);
                    if (statmentMaster.Count <= 0) continue;
                    foreach (var stmtMaster in statmentMaster)
                    {
                        stmtMaster.BusinessName = methodName[1];
                        stmtMaster.BusinessDescription = methodName[2];
                        await _codeVortoService.StatementReferenceMasterRepository.UpdateItem(stmtMaster);
                    }
                }
            }
            return Ok("Dictionary data updated successfully");
        }
    }
}
