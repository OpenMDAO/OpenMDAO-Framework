/**
 *  VariableFigure: an object representing a component in an openmdao workflow
 *
 *  A VariableFigure consists of a rectangular box containing the
 *  variable name and type.
 *
 *  Arguments:
 *      elm:      jQuery element which will contain the VariableFigure
 *      model:    object that provides access to the openmdao model
 *      pathname: the pathname of the component
 *      variable: a dictionary of the variable's attributes
 *      inout:    'input' or 'output' for an input or output variable respectively
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.VariableFigure=function(model, pathname, variable, inout) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'VariableFigure-'+pathname.replace(/\./g,'-'),
        name = openmdao.Util.getName(pathname),
        parent = openmdao.Util.getPath(pathname),
        parentName = openmdao.Util.getName(parent),
        svg = jQuery('<svg height="35" width="150">'
                   + '    <rect x="0" y="5" height="30" width="150" rx="5" ry="5";" />'
                   + '    <text id="name" x="75" y="17" text-anchor="middle">Name</text>'
                   + '    <text id="units" x="75" y="30" font-style="italic" text-anchor="middle">Units</text>'
                   + '</svg>'),
        fig = jQuery('<div class="VariableFigure" style="width:100px;height:35px;float:left;padding:5px;position:absolute" />')
            .append(svg),
        rectCSS = {'stroke-width':2, 'stroke':'#0b93d5', 'fill':'#999999'},
        contextMenu = jQuery("<ul class='context-menu'>")
            .appendTo(fig),
        tok,
        units;

//    elm.append(fig);

//    if (inout === 'input') {
//        this.outputPort=null;
//        this.inputPort=new draw2d.InputPort();
//        if (this.variable.connected) {
//            this.inputPort.setBackgroundColor(new draw2d.Color(255,0,0));
//        }
//    }
//    else {
//        this.outputPort=new draw2d.OutputPort();
//        this.inputPort=null;
//        if (this.variable.connected) {
//            this.outputPort.setBackgroundColor(new draw2d.Color(255,0,0));
//        }
//    }

    tok = variable.type.split('.');
    if (tok.length > 1) {
        units = variable.units + ' (' + tok[tok.length-1] + ')';
    }
    else {
        units = variable.units + ' (' + tok + ')';
    }

    // set name, id, tooltip and width
    fig.attr('id',id);
    svg.uniqueId();
    svg.find('#name').text(name);
    svg.find('#units').text(units);
    fig.find('rect').css(rectCSS);  // defaults
    contextMenu.uniqueId();

    // create context menu
    contextMenu.append(jQuery('<li><b>'+name+'</b></li>'));
    if (inout === 'output') {
        contextMenu.append(jQuery('<li>Create Passthrough</li>').click(function(e) {
            var parentAssm = openmdao.Util.getPath(parent),
                cmd = parentAssm+".create_passthrough('"+parentName+"."+name+"')";
            model.issueCommand(cmd);
        }));
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get SVG element */
    this.getSVGElement = function() {
        return svg;
    };

    /** get element */
    this.getElement = function() {
        return fig;
    };

    /** get pathname */
    this.getPathname = function() {
        return pathname;
    };

    /** get width */
    this.getWidth = function() {
        return fig.width();
    };

    /** get height */
    this.getHeight = function() {
        return fig.height();
    };

    /** get position relative to parent div */
    this.getPosition = function() {
        return fig.position();
    };

    /** get position relative to parent div */
    this.setPosition = function(x, y) {
        return fig.css({ left: x, top: y });
    };

};

//openmdao.VariableFigure.prototype.onDragstart=function(x,y){
//    var dragStarted=draw2d.Node.prototype.onDragstart.call(this,x,y);
//    if (this.header===null){
//        return false;
//    }
//    if (this.originalHeight===-1) {
//        if (this.canDrag === true &&
//            x < parseInt(this.header.style.width,10) &&
//            y < parseInt(this.header.style.height,10)) {
//            return true;
//        }
//    }
//    else {
//        return dragStarted;
//    }
//};

//openmdao.VariableFigure.prototype.setCanDrag=function(flag){
//    draw2d.Node.prototype.setCanDrag.call(this,flag);
//    this.html.style.cursor="";
//    if(this.header===null){
//        return;
//    }
//    if(flag){
//        this.header.style.cursor="move";
//    }else{
//        this.header.style.cursor="";
//    }
//};

//openmdao.VariableFigure.prototype.setWorkflow=function(wkflw){
//    draw2d.Node.prototype.setWorkflow.call(this,wkflw);
//    if (wkflw !== null) {
//        if (this.inputPort!==null) {
//            this.inputPort.setWorkflow(wkflw);
//            this.inputPort.setName("input");
//            this.addPort(this.inputPort,0,this.height/2);
//        }
//        if (this.outputPort!==null) {
//            this.outputPort.setWorkflow(wkflw);
//            this.outputPort.setName("output");
//            this.addPort(this.outputPort,this.width+5,this.height/2);
//            var oThis=this;
//            this.outputPort.createCommand = function(request) {
//                if(request.getPolicy() === draw2d.EditPolicy.CONNECT) {
//                    if( request.source.parentNode.id === request.target.parentNode.id) {
//                        return null;
//                    }
//                    if (request.source instanceof draw2d.InputPort) {
//                        var srcName       = openmdao.Util.getName(oThis.pathname),
//                            srcParent     = openmdao.Util.getPath(oThis.pathname),
//                            srcParentName = openmdao.Util.getName(srcParent),
//                            srcParentPath = openmdao.Util.getPath(srcParent),
//                            dstFigure     = request.source.getParent(),
//                            dstName       = openmdao.Util.getName(dstFigure.pathname),
//                            dstParent     = openmdao.Util.getPath(dstFigure.pathname),
//                            dstParentName = openmdao.Util.getName(dstParent),
//                            dstParentPath = openmdao.Util.getPath(dstParent),
//                            asm = null,
//                            src = null,
//                            dst = null;

//                        if (srcParentPath === dstParentPath) {
//                            // both vars are in components of a common assembly
//                            asm = srcParentPath;
//                            src = srcParentName + "." + srcName;
//                            dst = dstParent + "." + dstName;
//                        }
//                        else if (srcParent === dstParentPath) {
//                            // this is an assembly var, connecting to a comp var
//                            asm = srcParent;
//                            src = srcName;
//                            dst = dstParentName + "." + dstName;
//                        }
//                        else if (srcParentPath === dstParent) {
//                            // this is a comp var, connecting to a assembly var
//                            asm = srcParentPath;
//                            src = srcParentName + "." + srcName;
//                            dst = dstName;
//                        }
//                        else  {
//                            alert("Can't connect",oThis.pathname,'to',dstFigure.pathname);
//                            return false;
//                        }
//                        oThis.model.issueCommand(asm+".connect('"+src+"','"+dst+"')");
//                    }
//                    return true;
//                }
//            };
//        }
//    }
//};

