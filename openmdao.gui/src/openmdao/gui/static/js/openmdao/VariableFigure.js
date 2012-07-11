var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.VariableFigure=function(myModel,pathname,variable,inout){
    this.myModel = myModel;
    this.pathname = pathname;
    this.variable = variable;
    this.inout = inout;
    if (inout === 'input') {
        this.outputPort=null;
        this.inputPort=new draw2d.InputPort();
        if (this.variable.connected) {
            this.inputPort.setBackgroundColor(new draw2d.Color(255,0,0));
        }
    }
    else {
        this.outputPort=new draw2d.OutputPort();
        this.inputPort=null;
        if (this.variable.connected) {
            this.outputPort.setBackgroundColor(new draw2d.Color(255,0,0));
        }
    }
    draw2d.Node.call(this);
    this.setDimension(100,30);
    this.originalHeight=-1;

    var tok = pathname.split('.');
    if (tok.length > 1) {
        this.name = tok[tok.length-1];
        if (this.name === 'driver') {
            this.name = tok[tok.length-2] + '.' + this.name;
        }
    }
    else {
        this.name = pathname;
    }
    this.setTitle(this.name);

    tok = variable.type.split('.');
    if (tok.length > 1) {
        this.setContent('<center><i>' + variable.units +
                        ' (' + tok[tok.length-1] + ') </i></center>');
    }
    else {
        this.setContent('<center><i>' + variable.units +
                        ' (' + tok + ') </i></center>');
    }

    this.setCanDrag(false);
};

openmdao.VariableFigure.prototype=new draw2d.Node();

openmdao.VariableFigure.prototype.type="VariableFigure";

openmdao.VariableFigure.prototype.createHTMLElement=function(){
    var item=document.createElement("div");
    item.id=this.id;
    item.style.color="black";
    item.style.position="absolute";
    item.style.left=this.x+"px";
    item.style.top=this.y+"px";
    item.style.height=this.width+"px";
    item.style.width=this.height+"px";
    item.style.margin="0px";
    item.style.padding="0px";
    item.style.outline="none";
    item.style.zIndex=String(draw2d.Figure.ZOrderBaseIndex);

    this.header=document.createElement("div");
    this.header.style.position="absolute";
    this.header.style.left="0px";
    this.header.style.top="0px";
    this.header.style.height="15px";
    this.header.style.backgroundColor="#CCCCCC";
    this.header.style.borderTop="3px solid #666666";
    this.header.style.borderLeft="1px solid #666666";
    this.header.style.borderRight="1px solid #666666";
    this.header.style.fontSize="9px";
    this.header.style.textAlign="center";
    this.disableTextSelection(this.header);
    this.header.className="VariableFigureHeader";

    this.textarea=document.createElement("div");
    this.textarea.style.position="absolute";
    this.textarea.style.left="0px";
    this.textarea.style.top="15px";
    this.textarea.style.backgroundColor="white";
    this.textarea.style.borderTop="2px solid #666666";
    this.textarea.style.borderLeft="1px solid #666666";
    this.textarea.style.borderRight="1px solid #666666";
    this.textarea.style.overflow="hidden";
    this.textarea.style.fontSize="9pt";
    this.disableTextSelection(this.textarea);

    item.appendChild(this.header);
    item.appendChild(this.textarea);
    return item;
};

openmdao.VariableFigure.prototype.setDimension=function(w,h){
    draw2d.Node.prototype.setDimension.call(this,w,h);
    if(this.top_left!==null){
        this.textarea.style.width=(this.width)+"px";
        this.textarea.style.height=(this.height-this.header.height)+"px";
        this.header.style.width=(this.width)+"px";
    }
    if(this.outputPort!==null){
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if(this.inputPort!==null){
        this.inputPort.setPosition(0,this.height/2);
    }
};

openmdao.VariableFigure.prototype.setTitle=function(title){
    this.header.innerHTML=title;
};

openmdao.VariableFigure.prototype.setContent=function(html){
    this.textarea.innerHTML=html;
};

openmdao.VariableFigure.prototype.onDragstart=function(x,y){
    var dragStarted=draw2d.Node.prototype.onDragstart.call(this,x,y);
    if (this.header===null){
        return false;
    }
    if (this.originalHeight===-1) {
        if (this.canDrag === true &&
            x < parseInt(this.header.style.width,10) &&
            y < parseInt(this.header.style.height,10)) {
            return true;
        }
    }
    else {
        return dragStarted;
    }
};

openmdao.VariableFigure.prototype.setCanDrag=function(flag){
    draw2d.Node.prototype.setCanDrag.call(this,flag);
    this.html.style.cursor="";
    if(this.header===null){
        return;
    }
    if(flag){
        this.header.style.cursor="move";
    }else{
        this.header.style.cursor="";
    }
};

openmdao.VariableFigure.prototype.setWorkflow=function(wkflw){
    draw2d.Node.prototype.setWorkflow.call(this,wkflw);
    if (wkflw !== null) {
        if (this.inputPort!==null) {
            this.inputPort.setWorkflow(wkflw);
            this.inputPort.setName("input");
            this.addPort(this.inputPort,0,this.height/2);
        }
        if (this.outputPort!==null) {
            this.outputPort.setWorkflow(wkflw);
            this.outputPort.setName("output");
            this.addPort(this.outputPort,this.width+5,this.height/2);
            var oThis=this;
            this.outputPort.createCommand = function(request) {
                if(request.getPolicy() === draw2d.EditPolicy.CONNECT) {
                    if( request.source.parentNode.id === request.target.parentNode.id) {
                        return null;
                    }
                    if (request.source instanceof draw2d.InputPort) {
                        var srcName       = openmdao.Util.getName(oThis.pathname),
                            srcParent     = openmdao.Util.getPath(oThis.pathname),
                            srcParentName = openmdao.Util.getName(srcParent),
                            srcParentPath = openmdao.Util.getPath(srcParent),
                            dstFigure     = request.source.getParent(),
                            dstName       = openmdao.Util.getName(dstFigure.pathname),
                            dstParent     = openmdao.Util.getPath(dstFigure.pathname),
                            dstParentName = openmdao.Util.getName(dstParent),
                            dstParentPath = openmdao.Util.getPath(dstParent),
                            asm = null,
                            src = null,
                            dst = null;

                        if (srcParentPath === dstParentPath) {
                            // both vars are in components of a common assembly
                            asm = srcParentPath;
                            src = srcParentName + "." + srcName;
                            dst = dstParent + "." + dstName;
                        }
                        else if (srcParent === dstParentPath) {
                            // this is an assembly var, connecting to a comp var
                            asm = srcParent;
                            src = srcName;
                            dst = dstParentName + "." + dstName;
                        }
                        else if (srcParentPath === dstParent) {
                            // this is a comp var, connecting to a assembly var
                            asm = srcParentPath;
                            src = srcParentName + "." + srcName;
                            dst = dstName;
                        }
                        else  {
                            alert("Can't connect",oThis.pathname,'to',dstFigure.pathname);
                            return false;
                        }
                        oThis.myModel.issueCommand(asm+".connect('"+src+"','"+dst+"')");
                    }
                    return true;
                }
            };
        }
    }
};

openmdao.VariableFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var oThis=this;
    if (oThis.inout === 'output') {
        menu.appendMenuItem(new draw2d.MenuItem("Create Passthrough",null,function(){
            var parent     = openmdao.Util.getPath(oThis.pathname),
                parentName = openmdao.Util.getName(parent),
                parentAssm = openmdao.Util.getPath(parent),
                cmd = parentAssm+".create_passthrough('"+parentName+"."+oThis.name+"')";
            oThis.myModel.issueCommand(cmd);
        }));
    }
    return menu;
};

// openmdao.VariableFigure.prototype.onMouseEnter=function(){
    // this.getWorkflow().showTooltip(new openmdao.Tooltip(this.name),true);
// };

openmdao.VariableFigure.prototype.onDoubleClick=function(){
    // nada ATM
};

openmdao.VariableFigure.prototype.onMouseEnter=function(){
    this.setColor(new draw2d.Color(0,255,0));
};
openmdao.VariableFigure.prototype.onMouseLeave=function(){
    this.setColor(null);
};
