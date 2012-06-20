
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowComponentFigure=function(model,pathname,type, valid) {
    this.openmdao_model = model;
    this.pathname = pathname;
    this.type = type;
    this.valid = valid;
    this.cornerWidth=15;
    this.cornerHeight=15;
    this.outputPort=null;
    this.inputPort=null;
    draw2d.Node.call(this);
    this.setDimension(100,50);
    this.originalHeight=-1;

    // get name for this figure and set title appropriately
    this.name = openmdao.Util.getName(pathname);
    if (this.name === 'driver') {
        var parent = openmdao.Util.getPath(pathname),
            parentName = openmdao.Util.getName(parent);
        this.name = parentName + '.driver';
    }
    this.setTitle(this.name);

    // set the content text to be the type name (in italics)
    var tok = type.split('.');
    if (tok.length > 1) {
        this.setContent('<center><i>'+tok[tok.length-1]+'</i></center>');
    }
    else {
        this.setContent('<center><i>'+type+'</i></center>');
    }

    // do not allow moving or resizing
    this.setCanDrag(false);
    this.setResizeable(false);

    // set color based on valid status
    if (this.valid) {
        this.setColor(new draw2d.Color(0,255,0));
    }
    else {
        this.setColor(new draw2d.Color(255,0,0));
    }

    // change color based on execution status
    topic = pathname+'.exec_state';
    model.addListener(topic, this.setExecState.bind(this));
};

openmdao.WorkflowComponentFigure.prototype=new draw2d.Node();

openmdao.WorkflowComponentFigure.prototype.type="WorkflowComponentFigure";

openmdao.WorkflowComponentFigure.prototype.createHTMLElement=function(){
    var circleIMG = "url(/static/images/circle.png)";

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
    this.header.style.backgroundColor="#CCCCCC";
    this.header.style.borderTop="3px solid #666666";
    this.header.style.fontSize="9px";
    this.header.style.textAlign="center";
    this.disableTextSelection(this.header);

    this.footer=document.createElement("div");
    this.footer.style.position="absolute";
    this.footer.style.left=this.cornerWidth+"px";
    this.footer.style.top="0px";
    this.footer.style.height=(this.cornerHeight)+"px";
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
    this.textarea.style.overflow="hidden";
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

openmdao.WorkflowComponentFigure.prototype.setDimension=function(w,h){
    draw2d.Node.prototype.setDimension.call(this,w,h);
    if(this.top_left!==null){
        this.top_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.top=(this.height-this.cornerHeight)+"px";
        this.bottom_left.style.top=(this.height-this.cornerHeight)+"px";
        this.textarea.style.width=(this.width-2)+"px";
        this.textarea.style.height=(this.height-this.cornerHeight*2)+"px";
        this.header.style.width=(this.width-this.cornerWidth*2+1)+"px";
        this.footer.style.width=(this.width-this.cornerWidth*2+1)+"px";
        this.footer.style.top=(this.height-this.cornerHeight-1)+"px";
    }
    if(this.outputPort!==null){
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if(this.inputPort!==null){
        this.inputPort.setPosition(-5,this.height/2);
    }
};

openmdao.WorkflowComponentFigure.prototype.setTitle=function(title){
    this.header.innerHTML=title;
};

openmdao.WorkflowComponentFigure.prototype.setContent=function(_5014){
    this.textarea.innerHTML=_5014;
};

openmdao.WorkflowComponentFigure.prototype.onDragstart=function(x,y){
    var _5017=draw2d.Node.prototype.onDragstart.call(this,x,y);
    if(this.header===null){
        return false;
    }
    if(y<this.cornerHeight&&x<this.width&&x>(this.width-this.cornerWidth)){
        this.toggle();
        return false;
    }
    if(this.originalHeight===-1){
        if(this.canDrag===true &&
           x<parseInt(this.header.style.width,10) &&
           y<parseInt(this.header.style.height,10)) {
            return true;
        }
    }else{
        return _5017;
    }
};

openmdao.WorkflowComponentFigure.prototype.setCanDrag=function(flag){
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

openmdao.WorkflowComponentFigure.prototype.setWorkflow=function(_5019){
    draw2d.Node.prototype.setWorkflow.call(this,_5019);
};

openmdao.WorkflowComponentFigure.prototype.toggle=function(){
    if(this.originalHeight===-1){
        this.originalHeight=this.height;
        this.setDimension(this.width,this.cornerHeight*2);
        this.setResizeable(false);
    }else{
        this.setDimension(this.width,this.originalHeight);
        this.originalHeight=-1;
        this.setResizeable(true);
    }
};

openmdao.WorkflowComponentFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu(),
        model = this.openmdao_model,
        pathname = this.pathname,
        name = this.name,
        parent = this.getParent();

    // menu header
    menu.appendMenuItem(new draw2d.MenuItem("<b>"+name+"</b>",null,function(){
    }));

    // properties
    menu.appendMenuItem(new draw2d.MenuItem("Edit",null,function(){
        cf = new openmdao.ComponentFrame(model,pathname);
    }));

    // properties
    menu.appendMenuItem(new draw2d.MenuItem("Properties",null,function(){
        var id = (pathname+'-properties').replace(/\./g,'-');
        editor = new openmdao.PropertiesFrame(id,model).editObject(pathname);
    }));

    // run
    menu.appendMenuItem(new draw2d.MenuItem("Run",null,function(){
        var cmd = pathname + '.run();';
        model.issueCommand(cmd);
    }));

    // remove
    menu.appendMenuItem(new draw2d.MenuItem("Remove from Workflow",null,function(){
        if (parent) {
            var cmd = parent.pathname+".workflow.remove('";
            if (/.driver$/.test(name)) {
                cmd = cmd + name.replace(/.driver/g,'') + "')";
            }
            else {
                cmd = cmd + name + "')";
            }
            model.issueCommand(cmd);
        }
    }));

    menu.setZOrder(999999);
    return menu;
};

openmdao.WorkflowComponentFigure.prototype.onDoubleClick=function(){
    editor = new openmdao.ComponentFrame(this.openmdao_model,this.pathname);
};

/**
openmdao.WorkflowComponentFigure.prototype.onMouseEnter=function(){
    this.setColor(new draw2d.Color(0,255,0));
    // this.getWorkflow().showTooltip(new openmdao.Tooltip(this.name),true);
};

openmdao.WorkflowComponentFigure.prototype.onMouseLeave=function(){
    this.setColor(null);
};
**/

openmdao.WorkflowComponentFigure.prototype.setExecState=function(message){
    var state = message[1];
    //debug.info('WorkflowComponentFigure',this.pathname,state);
    if (state === "VALID") {
        this.setColor(new draw2d.Color(0,255,0));
    }
    else if (state === "INVALID") {
        this.setColor(new draw2d.Color(255,0,0));
    }
    else if (state === "RUNNING") {
        this.setColor(new draw2d.Color(0,0,255));
    }
};
