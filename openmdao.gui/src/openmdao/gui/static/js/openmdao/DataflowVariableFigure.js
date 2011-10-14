var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.DataflowVariableFigure=function(myModel,pathname,type){
    this.myModel = myModel;
    this.pathname = pathname;
    this.type = type;
    this.cornerWidth=15;
    this.cornerHeight=15;
    if (type == 'input') {
        this.outputPort=null;
        this.inputPort=new draw2d.InputPort();
    }
    else {
        this.outputPort=new draw2d.OutputPort();
        this.inputPort=null;
    }
    draw2d.Node.call(this);
    this.setDimension(100,50);
    this.originalHeight=-1;
    
    var tok = pathname.split('.')
    if (tok.length > 1) {
        this.name = tok[tok.length-1];
        if (this.name === 'driver') {
            this.name = tok[tok.length-2] + '.' + this.name
        }
    }
    else
        this.name = pathname
    this.setTitle(this.name)
    
    var tok = type.split('.')
    if (tok.length > 1)
        this.setContent('<center><i>'+tok[tok.length-1]+'</i></center>')
    else
        this.setContent('<center><i>'+type+''+'</i></center>')
        
    this.toggle();
    this.setCanDrag(false);
};

openmdao.DataflowVariableFigure.prototype=new draw2d.Node();

openmdao.DataflowVariableFigure.prototype.type="DataflowVariableFigure";

openmdao.DataflowVariableFigure.prototype.createHTMLElement=function(){
    var circleIMG = "url(/static/images/circle.png)";
    
    var item=document.createElement("div");    
    item.id=this.id;
    item.style.position="absolute";
    item.style.left=this.x+"px";
    item.style.top=this.y+"px";
    item.style.height=this.width+"px";
    item.style.width=this.height+"px";
    item.style.margin="0px";
    item.style.padding="0px";
    item.style.outline="none";
    item.style.zIndex=""+draw2d.Figure.ZOrderBaseIndex;
    
    this.top_left=document.createElement("div");
    this.top_left.style.background=circleIMG+" no-repeat top left";
    this.top_left.style.position="absolute";
    this.top_left.style.width=this.cornerWidth+"px";
    this.top_left.style.height=this.cornerHeight+"px";
    this.top_left.style.left="0px";
    this.top_left.style.top="0px";
    this.top_left.style.fontSize="2px";
    this.top_right=document.createElement("div");
    this.top_right.style.background=circleIMG+" no-repeat top right";
    this.top_right.style.position="absolute";
    this.top_right.style.width=this.cornerWidth+"px";
    this.top_right.style.height=this.cornerHeight+"px";
    this.top_right.style.left="0px";
    this.top_right.style.top="0px";
    this.top_right.style.fontSize="2px";
    
    this.bottom_left=document.createElement("div");
    this.bottom_left.style.background=circleIMG+" no-repeat bottom left";
    this.bottom_left.style.position="absolute";
    this.bottom_left.style.width=this.cornerWidth+"px";
    this.bottom_left.style.height=this.cornerHeight+"px";
    this.bottom_left.style.left="0px";
    this.bottom_left.style.top="0px";
    this.bottom_left.style.fontSize="2px";
    this.bottom_right=document.createElement("div");
    this.bottom_right.style.background=circleIMG+" no-repeat bottom right";
    this.bottom_right.style.position="absolute";
    this.bottom_right.style.width=this.cornerWidth+"px";
    this.bottom_right.style.height=this.cornerHeight+"px";
    this.bottom_right.style.left="0px";
    this.bottom_right.style.top="0px";
    this.bottom_right.style.fontSize="2px";
    
    this.header=document.createElement("div");
    this.header.style.position="absolute";
    this.header.style.left=this.cornerWidth+"px";
    this.header.style.top="0px";
    this.header.style.height=(this.cornerHeight)+"px";
    this.header.style.backgroundColor="#CCCCFF";
    this.header.style.borderTop="3px solid #666666";
    this.header.style.fontSize="9px";
    this.header.style.textAlign="center";
    this.disableTextSelection(this.header);
    
    this.footer=document.createElement("div");
    this.footer.style.position="absolute";
    this.footer.style.left=this.cornerWidth+"px";
    this.footer.style.top="0px";
    this.footer.style.height=(this.cornerHeight-1)+"px";
    this.footer.style.backgroundColor="white";
    this.footer.style.borderBottom="1px solid #666666";
    this.footer.style.fontSize="2px";
    
    this.textarea=document.createElement("div");
    this.textarea.style.position="absolute";
    this.textarea.style.left="0px";
    this.textarea.style.top=this.cornerHeight+"px";
    this.textarea.style.backgroundColor="white";
    this.textarea.style.borderTop="2px solid #666666";
    this.textarea.style.borderLeft="1px solid #666666";
    this.textarea.style.borderRight="1px solid #666666";
    this.textarea.style.overflow="auto";
    this.textarea.style.fontSize="9pt";
    this.disableTextSelection(this.textarea);
    
    item.appendChild(this.top_left);
    item.appendChild(this.header);
    item.appendChild(this.top_right);
    item.appendChild(this.textarea);
    item.appendChild(this.bottom_left);
    item.appendChild(this.footer);
    item.appendChild(this.bottom_right);
    return item;
};

openmdao.DataflowVariableFigure.prototype.setDimension=function(w,h){
    draw2d.Node.prototype.setDimension.call(this,w,h);
    if(this.top_left!==null){
        this.top_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.top=(this.height-this.cornerHeight)+"px";
        this.bottom_left.style.top=(this.height-this.cornerHeight)+"px";
        this.textarea.style.width=(this.width-2)+"px";
        this.textarea.style.height=(this.height-this.cornerHeight*2)+"px";
        this.header.style.width=(this.width-this.cornerWidth*2)+"px";
        this.footer.style.width=(this.width-this.cornerWidth*2)+"px";
        this.footer.style.top=(this.height-this.cornerHeight)+"px";
    }
    if(this.outputPort!==null){
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if(this.inputPort!==null){
        this.inputPort.setPosition(0,this.height/2);
    }
};

openmdao.DataflowVariableFigure.prototype.setTitle=function(title){
    this.header.innerHTML=title;
};

openmdao.DataflowVariableFigure.prototype.setContent=function(_5014){
    this.textarea.innerHTML=_5014;
};

openmdao.DataflowVariableFigure.prototype.onDragstart=function(x,y){
    var _5017=draw2d.Node.prototype.onDragstart.call(this,x,y);
    if(this.header===null){
        return false;
    }
    if(y<this.cornerHeight&&x<this.width&&x>(this.width-this.cornerWidth)){
        this.toggle();
        return false;
    }
    if(this.originalHeight==-1){
        if(this.canDrag===true&&x<parseInt(this.header.style.width)&&y<parseInt(this.header.style.height)){
            return true;
        }
    }else{
        return _5017;
    }
};

openmdao.DataflowVariableFigure.prototype.setCanDrag=function(flag){
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

openmdao.DataflowVariableFigure.prototype.setWorkflow=function(wkflw){
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
                if(request.getPolicy() ==draw2d.EditPolicy.CONNECT) {
                    if( request.source.parentNode.id == request.target.parentNode.id) {
                        return null;
                    }
                    if (request.source instanceof draw2d.InputPort) {
                        var parentPath = openmdao.Util.getParentPath(oThis.pathname),
                            parentAssm = openmdao.Util.getParentPath(parentPath),
                            parentName = openmdao.Util.getName(parentPath),
                            dstName    = request.source.getParent().name,
                            dstPath    = openmdao.Util.getParentPath(request.source.getParent().pathname),
                            dstParent  = openmdao.Util.getName(dstPath),
                            src = parentName + "." + oThis.name,
                            dst = dstParent + "." + dstName,
                            cmd = "top."+parentAssm+".connect('"+src+"','"+dst+"')";
                        oThis.myModel.issueCommand(cmd);
                    };                
                    return null;
                }
            }
        }
    }
};

openmdao.DataflowVariableFigure.prototype.toggle=function(){
    if(this.originalHeight==-1){
        this.originalHeight=this.height;
        this.setDimension(this.width,this.cornerHeight*2);
        this.setResizeable(false);
    }else{
        this.setDimension(this.width,this.originalHeight);
        this.originalHeight=-1;
        this.setResizeable(true);
    }
};

openmdao.DataflowVariableFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var oThis=this;
    if (oThis.type == 'output') {
        menu.appendMenuItem(new draw2d.MenuItem("Create Passthrough",null,function(){
            var parentPath = openmdao.Util.getParentPath(oThis.pathname),
                parentAssm = openmdao.Util.getParentPath(parentPath),
                parentName = openmdao.Util.getName(parentPath),
                cmd = "top."+parentAssm+".create_passthrough('"+parentName+"."+oThis.name+"')";
            oThis.myModel.issueCommand(cmd);
        }));
    }
    return menu;
};

// openmdao.DataflowVariableFigure.prototype.onMouseEnter=function(){
    // this.getWorkflow().showTooltip(new openmdao.Tooltip(this.name),true);
// };

openmdao.DataflowVariableFigure.prototype.onDoubleClick=function(){
    // nada ATM
};

openmdao.DataflowVariableFigure.prototype.onMouseEnter=function(){
    this.setColor(new draw2d.Color(0,255,0));
};
openmdao.DataflowVariableFigure.prototype.onMouseLeave=function(){
    this.setColor(null);
};
