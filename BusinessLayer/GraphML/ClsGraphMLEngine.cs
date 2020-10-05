namespace BusinessLayer.GraphML
{
    public class ClsGraphMLEngine
    {
        // Class provides GraphML generation based on nodes and links information passed

        public enum NODE_TYPE
        {
            RECTANGLE,
            START,
            END,
            DECISON
        }

        // returns the header for filename
        public string getHeader(string sProcessName)
        {
            string sReturn;

            sReturn =
                "<graphml xmlns='http://graphml.graphdrawing.org/xmlns/graphml' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' ";
            sReturn = sReturn +
                      " xsi:schemaLocation = 'http://graphml.graphdrawing.org/xmlns/graphml http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd'";
            sReturn = sReturn + " xmlns:y='http://www.yworks.com/xml/graphml'>";
            sReturn = sReturn +
                      " <key id='d0' for='node' yfiles.type='nodegraphics'/> <key id='d1' for='edge' yfiles.type='edgegraphics'/>";
            sReturn = sReturn +
                      " <key id='d2' for='node' attr.name='url' attr.type='string'/> <key id='d3' for='node' attr.name='description' attr.type='string'/>";
            sReturn = sReturn + " <graph id='" + sProcessName + "' edgedefault='directed'>";

            return sReturn;
        }

        // returns footer for file
        public string getFooter()
        {
            return " </graph> </graphml>";
        }

        //return node construct
        public string getNodeStream(NODE_TYPE typeID, string sNodeLabel, string sNodeIDText, int width, int height,
            string color)
        {
            if (color.ToLower() == "lightgray")
                color = "#d3d3d3";
            else if (color.ToLower() == "lightgreen")
                color = "#90ee90";
            else if (color.ToLower() == "lightblue")
                color = "#add8e6";
            else if (color.ToLower() == "lightcyan")
                color = "#e0ffff";
            else if (color.ToLower() == "lightpink")
                color = "#FFB6C1";
            else if (color.ToLower().Contains("light"))
                color = "#c0c0c0";
            var sReturn = "";
            switch ((int) typeID)
            {
                case 0:
                    sReturn = "<node id='" + sNodeIDText +
                              "' statementnumber='96'> <data key='d0'> <y:ShapeNode> <y:NodeLabel fontSize='14' fontFamily='Dialog'>";
                    sReturn = sReturn + sNodeLabel + "</y:NodeLabel> ";
                    sReturn = sReturn + " <y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height +
                              "' /> <y:Shape type='roundrectangle'/> <y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/>";
                    sReturn = sReturn + "<y:Fill  color='" + color + "' transparent='false' color2='" + color +
                              "'/> <y:BorderStyle type='line' width='1.0'  color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 1:
                    sReturn = "<node id='" + sNodeIDText + "' statementnumber='96'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn + "<y:Shape type='roundrectangle'/><y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/><y:Fill  color='" + color + "' transparent='false' color2='" +
                              color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 2:
                    sReturn = "<node id='" + sNodeIDText + "' statementnumber='96'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn + "<y:Shape type='roundrectangle'/><y:DropShadow Color = '" + color +
                              "' offsetX='6' offsetY='8'/><y:Fill  color='" + color + "' transparent='false' color2='" +
                              color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                case 3:
                    sReturn = "<node id='" + sNodeIDText + "' statementnumber='99'> <data key='d0'> <y:ShapeNode>";
                    sReturn = sReturn + "<y:NodeLabel fontSize='14' fontFamily='Dialog'>" + sNodeLabel +
                              "</y:NodeLabel>";
                    sReturn = sReturn + "<y:Geometry x='0.0' y='0.0' width='" + width + "' height='" + height + "' />";
                    sReturn = sReturn +
                              "<y:Shape type='diamond'/> <y:DropShadow color='#c0c0c0' offsetX='6' offsetY='8'/><y:Fill   color='" +
                              color + "' transparent='false' color2='" + color + "'/>";
                    sReturn = sReturn + " <y:BorderStyle type='line' width='1.0' color='" + color +
                              "'/> </y:ShapeNode> </data> </node>";
                    break;
                default:
                    break;
            }

            return sReturn;
        }

        // pass lable as NONE is nothign is to be printed on the link
        //link color is in RRGGBB format
        public string returnLinkConstruct(string orginID, string destID, string sLinkLabel, string sLinkIDText = "NONE",
            string sLinkColor = "ffffff")
        {
            var sReturn = "";

            if (sLinkLabel.Trim().ToUpper() == "NONE")
            {
                // no node label provided so no need to place node text
                sReturn = "<edge id='" + sLinkIDText + "' source='" + orginID + "' target='" + destID + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn + "<y:LineStyle type='line' width='2' color='#" + sLinkColor +
                          "'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            else if (sLinkLabel.Trim().ToUpper() == "")
            {
                // no node label provided so no need to place node text
                sReturn = "<edge id='" + sLinkIDText + "' source='" + orginID + "' target='" + destID + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn +
                          "<y:LineStyle type='line' width='2' color='#000000'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            else
            {
                sReturn = "<edge id='" + sLinkIDText + "' source='" + orginID + "' target='" + destID + "'>";
                sReturn = sReturn +
                          " <data key='d1'> <y:PolyLineEdge> <y:Path sx='0.0' sy='0.0' tx='2.0' ty='10.0'> <y:Point x='0.0' y='0.0'/> </y:Path>";
                sReturn = sReturn +
                          "<y:LineStyle type='line' width='2' color='#000000'/> <y:Arrows source='none' target='delta'/> <y:BendStyle smoothed='true'/>";
                //sReturn = sReturn + "<y:EdgeLabel backgroundColor='#000000' textColor='#ffffff' fontSize='12' fontFamily='Dialog' modelName='three_center' modelPosition='center'> " + sLinkLabel + "</y:EdgeLabel>";
                sReturn = sReturn +
                          "<y:EdgeLabel backgroundColor='#ffffff' textColor='#000000' fontSize='12' fontFamily='Dialog' modelName='three_center' modelPosition='center'> " +
                          sLinkLabel + "</y:EdgeLabel>";
                sReturn = sReturn + "</y:PolyLineEdge> </data> </edge>";
            }
            return sReturn;
        }
    }
}