
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowFigure=function(model,pathname,type,valid){
    this.openmdao_model = model;
    this.pathname = pathname;
    this.name = openmdao.Util.getName(pathname);
    this.type = type || '';
    this.valid = valid;

    this.cornerWidth=15;
    this.cornerHeight=15;

    this.outputPort=null;
    this.inputPort=null;

    draw2d.CompartmentFigure.call(this);

    this.setTitle(this.name);

    var tok = this.type.split('.');
    if (tok.length > 1) {
        this.contentHTML = '<center><i>'+tok[tok.length-1]+'</i></center>';
    }
    else {
        this.contentHTML = '<center><i>'+this.type+'</i></center>';
    }
    this.setContent(this.contentHTML);

    //this.setBorder(new draw2d.LineBorder(1));

    this.defaultBackgroundColor=new draw2d.Color(255,255,255);
    this.highlightBackgroundColor=new draw2d.Color(250,250,200);
    this.setBackgroundColor(this.defaultBackgroundColor);
    this.setDimension(this.getMinWidth(),this.getMinHeight());
    this.horizontal = true;

    // do not allow moving or resizing
    this.setCanDrag(false);
    this.setResizeable(false);

    this.figures = {};
    this.connections = {};
    this.margin = 20;
};
openmdao.DataflowFigure.prototype=new draw2d.CompartmentFigure();
openmdao.DataflowFigure.prototype.createHTMLElement=function(){
    var item=draw2d.CompartmentFigure.prototype.createHTMLElement.call(this);

    var circleIMG = "url(/static/images/circle.png)";

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

    this.contentArea=document.createElement("div");
    this.contentArea.style.position="absolute";
    this.contentArea.style.left="0px";
    this.contentArea.style.top=this.cornerHeight+"px";
    this.contentArea.style.backgroundColor="white";
    this.contentArea.style.borderTop="2px solid #666666";
    this.contentArea.style.borderLeft="1px solid #666666";
    this.contentArea.style.borderRight="1px solid #666666";
    this.contentArea.style.overflow="hidden";
    this.contentArea.style.fontSize="9pt";
    this.disableTextSelection(this.contentArea);

    // set up for dropping objects from jstree
    var elm = jQuery(item);
    elm.addClass("DataflowFigure");
    elm.data('name',this.name);
    elm.data('pathname',this.pathname);

    item.appendChild(this.top_left);
    item.appendChild(this.header);
    item.appendChild(this.top_right);
    item.appendChild(this.contentArea);
    item.appendChild(this.bottom_left);
    item.appendChild(this.footer);
    item.appendChild(this.bottom_right);
    return item;
};

openmdao.DataflowFigure.prototype.setWorkflow=function(wkflw){
    draw2d.Node.prototype.setWorkflow.call(this,wkflw);
    if(wkflw!==null && this.inputPort===null){
        this.inputPort=new draw2d.InputPort();
        this.inputPort.setWorkflow(wkflw);
        this.inputPort.setName("input");
        this.addPort(this.inputPort,this.width/2,0);

        this.outputPort=new draw2d.OutputPort();
        this.outputPort.setWorkflow(wkflw);
        this.outputPort.setName("output");

        var model = this.openmdao_model,
            name = this.name,
            pathname = this.pathname;
        this.outputPort.createCommand = function(request) {
            if(request.getPolicy() === draw2d.EditPolicy.CONNECT) {
                if(request.source.parentNode.id===request.target.parentNode.id){
                    return null;
                }
                if (request.source instanceof draw2d.InputPort) {
                    var path = openmdao.Util.getPath(pathname),
                        src  = name,
                        dst  = request.source.getParent().name;
                    eitor = new openmdao.ConnectionFrame(model,path,src,dst);
                }
                return null;
            }
        };
        this.addPort(this.outputPort,this.width+5,this.height/2);
    }
};

openmdao.DataflowFigure.prototype.isConnected=function(){
    return ((this.outputPort.getConnections().size > 0) ||
            (this.inputPort.getConnections().size > 0));
};

openmdao.DataflowFigure.prototype.onFigureEnter=function(figure){
    if(this.children[figure.id]===null){
        this.setBackgroundColor(this.highlightBackgroundColor);
    }
    draw2d.CompartmentFigure.prototype.onFigureEnter.call(this,figure);
};

openmdao.DataflowFigure.prototype.onFigureLeave=function(figure){
    draw2d.CompartmentFigure.prototype.onFigureLeave.call(this,figure);
    this.setBackgroundColor(this.defaultBackgroundColor);
};

openmdao.DataflowFigure.prototype.onDragstart=function(x,y){
    var dragStarted = draw2d.Node.prototype.onDragstart.call(this,x,y);
    if (y<this.cornerHeight && x<this.width && x>(this.width-this.cornerWidth)){
        this.toggle();
        return false;
    }
    else {
        return dragStarted;
    }
};

openmdao.DataflowFigure.prototype.onFigureDrop=function(_4a1e){
    debug.info("DataflowFigure.onFigureDrop",_4a1e);
    draw2d.CompartmentFigure.prototype.onFigureDrop.call(this,_4a1e);
    this.setBackgroundColor(this.defaultBackgroundColor);
};

openmdao.DataflowFigure.prototype.setDimension=function(w,h){
    draw2d.CompartmentFigure.prototype.setDimension.call(this,w,h);
    if(this.top_left!==null){
        this.top_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.left=(this.width-this.cornerWidth)+"px";
        this.bottom_right.style.top=(this.height-this.cornerHeight)+"px";
        this.bottom_left.style.top=(this.height-this.cornerHeight)+"px";
        this.contentArea.style.width=(this.width-2)+"px";
        this.contentArea.style.height=(this.height-this.cornerHeight*2)+"px";
        this.header.style.width=(this.width-this.cornerWidth*2+1)+"px";
        this.footer.style.width=(this.width-this.cornerWidth*2+1)+"px";
        this.footer.style.top=(this.height-this.cornerHeight-1)+"px";
    }
    if (this.outputPort!==null) {
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if (this.inputPort!==null) {
        this.inputPort.setPosition(this.width/2,0);
    }
};
openmdao.DataflowFigure.prototype.setTitle=function(title){
    this.header.innerHTML= title;
};

openmdao.DataflowFigure.prototype.setContent=function(content){
    this.contentArea.innerHTML=content;
};

openmdao.DataflowFigure.prototype.getMinWidth=function(){
    return 110;
};
openmdao.DataflowFigure.prototype.getMinHeight=function(){
    return 60;
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
    // nothing for now
    return menu;
};

openmdao.DataflowFigure.prototype.toggle=function(){
    if (this.height > this.getMinHeight()) {
        this.minimize();
    }
    else{
        this.maximize();
    }
};

openmdao.DataflowFigure.prototype.minimize=function(){
    debug.info('DataflowFigure.minimize ',this.name,this);
    // remove all child figures
    var self = this,
        workflow = this.getWorkflow();

    jQuery.each(this.figures,function(name,fig) {
        debug.info('DataflowFigure.minimize removing',name,fig);
        fig.minimize();
        self.removeChild(fig);
        workflow.removeFigure(fig);
    });
    this.figures = {};

    jQuery.each(this.connections,function(name,conn) {
        debug.info('DataflowFigure.minimize removing',name,conn);
        workflow.removeFigure(conn);
    });
    this.connections = {};

    this.setContent(this.contentHTML);
    this.setDimension(this.getMinWidth(),this.getMinHeight());

    var parent = this.getParent();
    if (parent instanceof openmdao.DataflowFigure) {
        parent.layout();
    }
};

openmdao.DataflowFigure.prototype.maximize=function(){
    // get child data from model and redraw with child figures
    this.openmdao_model.getDataflow(this.pathname,
        this.updateDataflow.bind(this),
        function(jqXHR, textStatus, errorThrown) {
            debug.error('Error getting dataflow for',this,jqXHR);
        }.bind(this)
    );
};

/** update dataflow by recreating figures from JSON dataflow data
 */
openmdao.DataflowFigure.prototype.updateDataflow=function(json) {
    var self=this,
        workflow = this.getWorkflow();

    debug.info('updatedataflow',this.name,this);

    this.setContent('');

    jQuery.each(json.components,function(idx,comp) {
        var name = comp.name,
            type = comp.type,
            valid = comp.valid,
            fig = self.figures[name];

        if (!fig) {
            if (self.pathname) {
                figname = self.pathname+'.'+name;
            }
            else {
                figname = name;
            }
            fig = new openmdao.DataflowFigure(self.openmdao_model,
                                              figname,type,valid);
            self.figures[name] = fig;
        }

        self.addChild(fig);
        workflow.addFigure(fig,0,0);
    });

    jQuery.each(json.connections,function(idx,conn) {
        // internal connections only
        if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
            var src_name = conn[0].split('.')[0],
                dst_name = conn[1].split('.')[0],
                con_name = src_name+'-'+dst_name,
                src_fig = self.figures[src_name],
                dst_fig = self.figures[dst_name],
                c = self.connections[con_name];

            if (!c) {
                c = new draw2d.Connection();
                c.setSource(src_fig.getPort("output"));
                c.setTarget(dst_fig.getPort("input"));
                c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                c.onDoubleClick = function() {
                    var editor = new openmdao.ConnectionFrame(self.openmdao_model,
                                                self.pathname,src_name,dst_name);
                }
                workflow.addFigure(c);
                self.connections[con_name] = c;
            }
        }
    });

    this.layout();
};

/** layout component figures */
openmdao.DataflowFigure.prototype.layout=function() {
    var connected = [],
        unconnected = [],
        x0=this.getAbsoluteX(),
        y0=this.getAbsoluteY();

//    debug.info('DataflowFigure.layout',this,x0,y0);

    jQuery.each(this.figures, function(idx,fig) {
        if (fig.isConnected()) {
            connected.push(fig);
        }
        else {
            unconnected.push(fig);
        }
    });

    // unconnected components are laid out in rows
    var margin = this.margin,
        x = x0 + margin,
        y = y0 + margin,
        row_height = 0,
        canvas_width = this.getWorkflow().getWidth();

    jQuery.each(unconnected,function(idx,fig) {
//        debug.info('DataflowFigure.layout unconnected',idx,fig,x,y);
        fig.setPosition(x,y);
        x = x + fig.getWidth() + margin;
        fig_height = fig.getHeight();
        if (fig_height > row_height) {
            row_height = fig_height;
        }
        if (x > canvas_width) {
            x = x0 + margin;
            y = y + row_height + margin;
            row_height = 0;
        }
    });

    // connected components are laid out diagonally
    // (top left to bottom right)
    x = x0 + margin;
    y = y + row_height + margin;
    jQuery.each(connected,function(idx,fig) {
//        debug.info('DataflowFigure.layout connected',idx,fig,x,y);
        fig.setPosition(x,y);
        x = x + fig.getWidth()+margin;
        y = y + fig.getHeight()+margin;
    });

    this.resize();

    var parent = this.getParent();
    if (parent instanceof openmdao.DataflowFigure) {
        parent.layout();
    }
};

/** resize figure to contain all it's children */
openmdao.DataflowFigure.prototype.resize=function(){
    var i=0,
        xmin=999999, xmax=0,
        ymin=999999, ymax=0,
        width = this.getMinWidth(),
        height = this.getMinHeight(),
        children = this.getChildren();

    for (i=0;i<children.size;i++) {
        child = children.get(i);
        if (child instanceof openmdao.DataflowFigure) {
            x = child.getAbsoluteX();
            if (x < xmin) {
                xmin = x;
            }
            x = x + child.getWidth();
            if (x > xmax) {
                xmax = x;
            }
            y = child.getAbsoluteY()+child.getHeight();
            if (y < ymin) {
                ymin = y;
            }
            y = y + child.getHeight();
            if (y > ymax) {
                ymax = y;
            }
        }
    }
    width = xmax-xmin+this.margin*2;
    height = ymax-ymin+this.margin*2;
    debug.info('DataflowFigure.resize',xmin,xmax,width,ymin,ymax,height);
    this.setDimension(width,height);
};

