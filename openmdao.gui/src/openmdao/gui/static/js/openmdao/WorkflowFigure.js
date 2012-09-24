
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFigure=function(model,flowpath,pathname,driver){
    this.openmdao_model = model;
    this.flowpath = flowpath;
    this.pathname = pathname;
    this.driver = driver;

    // if my name is 'driver', then use my parent's (assembly) name
    var tok = flowpath.split('.');
    if (tok.length > 1) {
        this.name = tok[tok.length-1];
        if (tok.length > 2 && this.name === 'driver') {
            this.name = tok[tok.length-2];
        }
    }
    else {
        this.name = flowpath;
    }

    draw2d.CompartmentFigure.call(this);

    this.defaultBackgroundColor=new draw2d.Color(255,255,255);
    this.highlightBackgroundColor=new draw2d.Color(250,250,200);
    this.dropHighlightBackgroundColor=new draw2d.Color(207,214,254);
    this.setBackgroundColor(this.defaultBackgroundColor);
    this.setDimension(110,60);

    this.horizontal = true;

    // do not allow moving or resizing
    this.setCanDrag(false);
    this.setResizeable(false);
};

openmdao.WorkflowFigure.prototype=new draw2d.CompartmentFigure();
openmdao.WorkflowFigure.prototype.createHTMLElement=function(){
    var item=draw2d.CompartmentFigure.prototype.createHTMLElement.call(this);

    item.className = "WorkflowFigure";

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
    this.titlebar.style.fontSize="10px";
    this.titlebar.style.backgroundColor="gray";
    this.titlebar.style.borderBottom="1px solid gray";
    this.titlebar.style.borderLeft="5px solid transparent";
    this.titlebar.style.whiteSpace="nowrap";
    this.titlebar.style.textAlign="left";
    this.titlebar.className="WorkflowFigureTitleBar";
    //this.titlebar.style.backgroundImage="url(window_toolbar.png)";
    this.titlebar.innerHTML= this.name;
    item.appendChild(this.titlebar);

    // set up for dropping components to add to workflow
    var self = this,
        model = this.openmdao_model,
        elm = jQuery(item);
    elm.data('name',this.name);
    elm.data('pathname',this.pathname);

    elm.highlightAsDropTarget = function(){ self.highlightAsDropTarget(); };
    elm.unhighlightAsDropTarget = function(){ self.unhighlightAsDropTarget(); };

    elm.droppable ({
        accept: '.component,.IComponent',
            out: function(ev,ui) {
                openmdao.drag_and_drop_manager.draggableOut(elm);
            },
            over: function(ev,ui) {
                // only allow drops of components in same assembly as driver
                var target_pathname = elm.data('pathname'),
                    target_parent = openmdao.Util.getPath(target_pathname),
                    dragged_object = jQuery(ui.draggable).clone(),
                    dragged_pathname,
                    dragged_parent;
                if (dragged_object.hasClass('component')) {
                    dragged_pathname = jQuery(ui.draggable ).parent().attr("path");
                    dragged_parent = openmdao.Util.getPath(dragged_pathname);
                    if (dragged_parent === target_parent) {
                        openmdao.drag_and_drop_manager.draggableOver(elm);
                    }
                }
                else if (dragged_object.hasClass('IComponent')) {
                    openmdao.drag_and_drop_manager.draggableOver(elm);
                }
            },
            drop: function(ev,ui) {
                top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
                if (top_div) {
                    var drop_function = top_div.droppable('option', 'actualDropHandler');
                    drop_function(ev, ui);
                }
            },
            actualDropHandler: function(ev,ui) {
                openmdao.drag_and_drop_manager.clearHighlightingDroppables();
                var target_pathname = elm.data('pathname'),
                    target_parent = openmdao.Util.getPath(target_pathname),
                    dropped_object = jQuery(ui.draggable).clone(),
                    dropped_pathname,
                    dropped_parent,
                    dropped_name,
                    prompt;

                if (dropped_object.hasClass('component')) {
                    // dropped from component tree, component must be in same assembly as the driver
                    dropped_pathname = jQuery(ui.draggable).parent().attr("path");
                    dropped_parent = openmdao.Util.getPath(dropped_pathname);
                    if (dropped_parent === target_parent) {
                        dropped_name = openmdao.Util.getName(dropped_pathname);
                        cmd = target_pathname + '.workflow.add("' + dropped_name + '")';
                        model.issueCommand(cmd);
                    }
                }
                else if (dropped_object.hasClass('IComponent')) {
                    // dropped from library, create new component in same assembly as the driver
                    dropped_pathname = dropped_object.attr("modpath");
                    dropped_name = dropped_object.text();
                    prompt = 'Specify a name for the new '+dropped_name+'<br>'+
                             '(It will be added to '+target_parent +' and to <br>'+
                             'the workflow of '+ target_pathname+')';
                    openmdao.Util.promptForValue(prompt, function(name) {
                            model.addComponent(dropped_pathname,name,target_parent, function() {
                                // if successful, then add to workflow as well
                                cmd = target_pathname+'.workflow.add("'+name+'")';
                                model.issueCommand(cmd);
                            });
                        }
                    );
                }
            }
        });

    return item;
};


/** Highlight this figure when it the cursor is over it and it can accept a drop */
openmdao.WorkflowFigure.prototype.highlightAsDropTarget=function(){
    this.setBackgroundColor(this.dropHighlightBackgroundColor);
};

/** Turn off highlighting of this figure when it can no longer accept a drop
    because the cursor is not over it or another drop target is over it */
openmdao.WorkflowFigure.prototype.unhighlightAsDropTarget=function(){
    this.setBackgroundColor(this.defaultBackgroundColor);
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
    this.titlebar.innerHTML= title;
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
    var self=this;
    menu.appendMenuItem(new draw2d.MenuItem("Flip Workflow",null,function(){
        self.horizontal = !self.horizontal;
        self.redraw();
    }));
    menu.appendMenuItem(new draw2d.MenuItem("Clear Workflow",null,function(){
        var asm = self.pathname,
            cmd = asm + '.workflow.clear();' + asm + '.config_changed();';
        self.openmdao_model.issueCommand(cmd);
    }));
    menu.setZOrder(999999);
    return menu;
};

/** add a component figure to this workflow (container) figure */
openmdao.WorkflowFigure.prototype.addComponentFigure=function(comp_fig){
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
            }
            if (x > xmax) {
                xmax = x;
                width = child.getWidth();
            }
            y = child.getAbsoluteY();
            if (y < ymin) {
                ymin = y;
            }
            if (y > ymax) {
                ymax = y;
                height = child.getHeight();
            }
        }
    }
    width = xmax+width-xmin;
    height = ymax+height-ymin+20;
    this.setDimension(width,height);
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
    }
    this.resize();
};
