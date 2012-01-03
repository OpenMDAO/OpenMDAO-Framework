openmdao.WorkflowFigure=function(myModel,flowpath,pathname,driver){
    this.myModel = myModel;
    this.flowpath = flowpath;
    this.pathname = pathname;
    this.driver = driver;
    this.horizontal = true;
    // if my name is 'driver', then use my parent's (assembly) name
    var tok = flowpath.split('.')    
    if (tok.length > 1) {
        this.name = tok[tok.length-1];
        if (tok.length > 2 && this.name === 'driver') {
            this.name = tok[tok.length-2];
        }
    }
    else
        this.name = flowpath;
    this.title = this.name
    this.defaultBackgroundColor=new draw2d.Color(255,255,255);
    this.highlightBackgroundColor=new draw2d.Color(250,250,200);
    draw2d.CompartmentFigure.call(this);
    this.setBackgroundColor(this.defaultBackgroundColor);
    this.setDimension(110,60);    

    this.setCanDrag(false);
};
openmdao.WorkflowFigure.prototype=new draw2d.CompartmentFigure();
openmdao.WorkflowFigure.prototype.createHTMLElement=function(){
    var item=draw2d.CompartmentFigure.prototype.createHTMLElement.call(this);
    item.style.margin="0px";
    item.style.padding="0px";
    item.style.border="1px solid black";
    item.style.cursor=null;
    this.titlebar=document.createElement("div");
    this.titlebar.style.color="white";
    this.titlebar.style.position="absolute";
    this.titlebar.style.left="0px";
    this.titlebar.style.top="0px";
    this.titlebar.style.width=(this.getWidth()-5)+"px";
    this.titlebar.style.height="15px";
    this.titlebar.style.margin="0px";
    this.titlebar.style.padding="0px";
    this.titlebar.style.font="normal 10px verdana";
    this.titlebar.style.backgroundColor="gray";
    this.titlebar.style.borderBottom="1px solid gray";
    this.titlebar.style.borderLeft="5px solid transparent";
    this.titlebar.style.whiteSpace="nowrap";
    this.titlebar.style.textAlign="left";
    //this.titlebar.style.backgroundImage="url(window_toolbar.png)";
    this.textNode=document.createTextNode(this.title);
    this.titlebar.appendChild(this.textNode);
    item.appendChild(this.titlebar);
 
    // set up for dropping objects from jstree
    var elm = jQuery(item);
    elm.addClass("WorkflowFigure");
    elm.data('name',this.name);
    elm.data('pathname',this.pathname);
    elm.data('flowpath',this.flowpath);
    
    return item;
};
openmdao.WorkflowFigure.prototype.onFigureEnter=function(_4a1c){
    if(this.children[_4a1c.id]===null){
        this.setBackgroundColor(this.highlightBackgroundColor);
    }
    draw2d.CompartmentFigure.prototype.onFigureEnter.call(this,_4a1c);
};
openmdao.WorkflowFigure.prototype.onFigureLeave=function(_4a1d){
    draw2d.CompartmentFigure.prototype.onFigureLeave.call(this,_4a1d);
    this.setBackgroundColor(this.defaultBackgroundColor);
};
openmdao.WorkflowFigure.prototype.onFigureDrop=function(_4a1e){
    draw2d.CompartmentFigure.prototype.onFigureDrop.call(this,_4a1e);
    this.setBackgroundColor(this.defaultBackgroundColor);
};
openmdao.WorkflowFigure.prototype.setDimension=function(w,h){
    draw2d.CompartmentFigure.prototype.setDimension.call(this,w,h);
    if(this.titlebar!==null){
        this.titlebar.style.width=(this.getWidth()-5)+"px";
    }
};
openmdao.WorkflowFigure.prototype.setTitle=function(title){
    this.title=title;
};
openmdao.WorkflowFigure.prototype.getMinWidth=function(){
    return 50;
};
openmdao.WorkflowFigure.prototype.getMinHeight=function(){
    return 50;
};
openmdao.WorkflowFigure.prototype.setBackgroundColor=function(color){
    this.bgColor=color;
    if(this.bgColor!==null){
        this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
    }
    else{
        this.html.style.backgroundColor="transparent";
    }
};
openmdao.WorkflowFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var oThis=this;
    menu.appendMenuItem(new draw2d.MenuItem("Flip Workflow",null,function(){
        oThis.horizontal = !oThis.horizontal;
        oThis.redraw();
    }));
    menu.appendMenuItem(new draw2d.MenuItem("Clear Workflow",null,function(){
        var asm = 'top.'+oThis.pathname,
            cmd = asm + '.workflow.clear();' + asm + '.config_changed();';
        oThis.myModel.issueCommand(cmd);
    }));
    menu.setZOrder(999999);
    return menu;
};

/** add a component figure to this workflow (container) figure */
openmdao.WorkflowFigure.prototype.addComponentFigure=function(comp_fig){
    //debug.info("flow:",this.name,"children:",this.getChildren(),'horizontal',this.horizontal)
    var count = this.getChildren().size;
    if (this.horizontal) {
        //x = this.getAbsoluteX()+getFlowWidth(this);
        x = this.getAbsoluteX()+comp_fig.getWidth()*count*1.5;
        y = 20+this.getAbsoluteY();
    }
    else {
        x = this.getAbsoluteX();
        //y = 20+this.getAbsoluteY()+getFlowHeight(this);
        y = 20+this.getAbsoluteY()+comp_fig.getHeight()*count*1.5;
    }                                            
    this.addChild(comp_fig);
    this.getWorkflow().addFigure(comp_fig,x,y);
};

/** resize workflow (container) figure to contain all it's children */
openmdao.WorkflowFigure.prototype.resize=function(){
    var i=0, 
        xmin=999999, xmax=0, 
        ymin=999999, ymax=0,
        width = 100, height = 50,
        children = this.getChildren();
    for (i=0;i<children.size;i++) {
        child = children.get(i);
        if (child instanceof openmdao.WorkflowComponentFigure) {
            x = child.getAbsoluteX();
            if (x < xmin) {
                xmin = x;
            };
            if (x > xmax) {
                xmax = x;
                width = child.getWidth();
            };
            y = child.getAbsoluteY();
            if (y < ymin) {
                ymin = y;
            };
            if (y > ymax) {
                ymax = y;
                height = child.getHeight();
            };
        };
    };
    width = xmax+width-xmin;
    height = ymax+height-ymin+20;
    this.setDimension(width,height);
    
    // if we outgrew the workflow, grow it a bit
    // var workflow = this.getWorkflow();
    // debug.info('width',width,'workflow',workflow.getWidth());
    // if (workflow.getWidth() < width) {
        // workflow.setWidth(width+100);
    // }
    // debug.info('width',width,'workflow',workflow.getWidth());
    // debug.info('height',height,'workflow',workflow.getHeight());
    // if (workflow.getHeight() < height) {
        // workflow.setHeight(height+100);
    // }
    // debug.info('height',height,'workflow',workflow.getHeight());
    // workflow.setBackgroundImage( "/static/images/grid_10.png", true);    
};

/** redraw workflow (container) figure */
openmdao.WorkflowFigure.prototype.redraw=function(){
    var children = this.getChildren();
    for (i=0;i<children.size;i++) {
        child = children.get(i);                
        if (child instanceof openmdao.WorkflowFigure) {
            x = child.driver.getAbsoluteX()+child.driver.getWidth()-20;
            y = child.driver.getAbsoluteY()+child.driver.getHeight()-10;            
        }
        else {
            if (this.horizontal) {
                x = this.getX()+child.getWidth()*i*1.5;
                y = 20+this.getY();
            }
            else {
                x = this.getAbsoluteX();
                y = 20+this.getY()+child.getHeight()*i*1.5;
            }
        }
        child.setPosition(x,y);
    };
    this.resize();
};
