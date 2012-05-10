openmdao.DataflowFigure=function(model,pathname){
    draw2d.CompartmentFigure.call(this);

    this.model = model;
    this.pathname = pathname;
    this.name = openmdao.Util.getName(pathname);
    this.title = this.name;

    this.defaultBackgroundColor=new draw2d.Color(255,255,255);
    this.highlightBackgroundColor=new draw2d.Color(250,250,200);
    this.setBackgroundColor(this.defaultBackgroundColor);
    this.setDimension(110,60);

    this.horizontal = true;

    // do not allow moving or resizing
    this.setCanDrag(true);
    this.setResizeable(true);

    this.figures = {};

    debug.info('DataflowFigure',model,pathname,this);
};
openmdao.DataflowFigure.prototype=new draw2d.CompartmentFigure();
openmdao.DataflowFigure.prototype.createHTMLElement=function(){
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
    elm.addClass("DataflowFigure");
    elm.data('name',this.name);
    elm.data('pathname',this.pathname);

    return item;
};
openmdao.DataflowFigure.prototype.onFigureEnter=function(_4a1c){
    if(this.children[_4a1c.id]===null){
        this.setBackgroundColor(this.highlightBackgroundColor);
    }
    draw2d.CompartmentFigure.prototype.onFigureEnter.call(this,_4a1c);
};
openmdao.DataflowFigure.prototype.onFigureLeave=function(_4a1d){
    draw2d.CompartmentFigure.prototype.onFigureLeave.call(this,_4a1d);
    this.setBackgroundColor(this.defaultBackgroundColor);
};
openmdao.DataflowFigure.prototype.onFigureDrop=function(_4a1e){
    debug.info("DataflowFigure.onFigureDrop",_4a1e)
    draw2d.CompartmentFigure.prototype.onFigureDrop.call(this,_4a1e);
    this.setBackgroundColor(this.defaultBackgroundColor);
};
openmdao.DataflowFigure.prototype.setDimension=function(w,h){
    draw2d.CompartmentFigure.prototype.setDimension.call(this,w,h);
    if(this.titlebar!==null){
        this.titlebar.style.width=(this.getWidth()-5)+"px";
    }
};
openmdao.DataflowFigure.prototype.setTitle=function(title){
    this.title=title;
};
openmdao.DataflowFigure.prototype.getMinWidth=function(){
    return 50;
};
openmdao.DataflowFigure.prototype.getMinHeight=function(){
    return 50;
};
openmdao.DataflowFigure.prototype.setBackgroundColor=function(color){
    this.bgColor=color;
    if(this.bgColor!==null){
        this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
    }
    else{
        this.html.style.backgroundColor="transparent";
    }
};
openmdao.DataflowFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var self=this;
    // nothing for now
    return menu;
};

/** update dataflow by recreating figures from JSON dataflow data
 */
openmdao.DataflowFigure.prototype.updateFigures=function(json) {
    var self=this;
    debug.info('DataflowFigure.updateFigures',this,self,json)

    jQuery.each(json['components'],function(idx,comp) {
        var name = comp['name'],
            type = comp['type'],
            valid = comp['valid'],
            fig = self.figures[name];

        if (!fig) {
            if (self.pathname) {
                var fig = new openmdao.DataflowComponentFigure(self.model,self.pathname+'.'+name,type,valid);
            }
            else {
                var fig = new openmdao.DataflowComponentFigure(self.model,name,type,valid);
            }
            fig.setTitle(name);
            self.figures[name] = fig;
        };

        fig.setContent('<center>(('+type+'))'+'</center>');

        self.addComponentFigure(fig);
    })

    jQuery.each(json['connections'],function(idx,conn) {
        // internal connections only
        if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
            var src_name = conn[0].split('.')[0],
                dst_name = conn[1].split('.')[0],
                src_fig = self.figures[src_name],
                dst_fig = self.figures[dst_name];
                c = new draw2d.Connection();
            // TODO: only create new connection if one doesn't already exist
            c.setSource(src_fig.getPort("output"));
            c.setTarget(dst_fig.getPort("input"));
            c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
            c.onDoubleClick = function() {
                debug.info('DataflowFigure.connections dblclick',self,src_name,dst_name)
                new openmdao.ConnectionFrame(self.model,self.pathname,src_name,dst_name);
            };
            self.getWorkflow().addFigure(c);
        }
    })

    this.layout();
}

/** add a component figure to this dataflow (container) figure */
openmdao.DataflowFigure.prototype.addComponentFigure=function(comp_fig){
    //debug.info("flow:",this.name,"children:",this.getChildren(),'horizontal',this.horizontal)
    var count = this.getChildren().size;
    if (this.horizontal) {
        //x = this.getAbsoluteX()+getFlowWidth(this);
        x = this.getAbsoluteX()+comp_fig.getWidth()*count*1.5;
        y = 25+this.getAbsoluteY();
    }
    else {
        x = this.getAbsoluteX();
        //y = 20+this.getAbsoluteY()+getFlowHeight(this);
        y = 25+this.getAbsoluteY()+comp_fig.getHeight()*count*1.5;
    }
    this.addChild(comp_fig);
    this.getWorkflow().addFigure(comp_fig,x,y);
};

/** layout component figures */
openmdao.DataflowFigure.prototype.layout=function() {
    var connected = [],
        unconnected = [],
        i=0, x=25, y=25+this.getAbsoluteY();

    jQuery.each(this.figures, function(idx,fig) {
        if (fig.isConnected()) {
            connected.push(fig);
        }
        else {
            unconnected.push(fig);
        }
    });

    // unconnected components are laid out in rows
    var row = 0,
        row_start = 0,
        max_width = this.getWorkflow().getWidth();

    jQuery.each(unconnected,function(idx,fig) {
        x = (idx-row_start)*(fig.getWidth()+20) + 20;
        if ((x + fig.getWidth()) > max_width) {
            row = row + 1;
            row_start = idx;
            x = (idx-row_start)*(fig.getWidth()+20) + 20;
            y = y + row*(fig.getHeight()+20);
        }
        fig.setPosition(x,y);
    });

    // connected components are laid out diagonally
    // (top left to bottom right)
    x = 0;
    jQuery.each(connected,function(idx,fig) {
        x = idx*(fig.getWidth()+20) + 20;
        y = y + (fig.getHeight()+20) + 20;
        fig.setPosition(x,y);
    });

    this.resize();
};

/** resize workflow (container) figure to contain all it's children */
openmdao.DataflowFigure.prototype.resize=function(){
    var i=0,
        xmin=999999, xmax=0,
        ymin=999999, ymax=0,
        width = 100, height = 50,
        children = this.getChildren();
    for (i=0;i<children.size;i++) {
        child = children.get(i);
        if (child instanceof openmdao.DataflowComponentFigure) {
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
    width = xmax+width-xmin+5;
    height = ymax+height-ymin+25;
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
openmdao.DataflowFigure.prototype.redraw=function(){
    var children = this.getChildren();
    for (i=0;i<children.size;i++) {
        child = children.get(i);
        if (child instanceof openmdao.DataflowFigure) {
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
