var openmdao = ( openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowFigure=function(project, pathname, prop_fn, type, valid, interfaces) {
    this.project = project;
    this.pathname = pathname;
    this.prop_fn = prop_fn;
    this.name = openmdao.Util.getName(pathname);
    this.type = type || '';
    this.valid = valid;
    this.interfaces = interfaces;

    if (arguments.length < 5) { // Refresh doesn't pass all arguments.
        this.baseType = pathname === '' ? 'Component' : 'Assembly';
    }
    else {
        if (interfaces.indexOf('IAssembly') >= 0) {
            this.baseType = 'Assembly';
        }
        else if (interfaces.indexOf('IDriver') >= 0) {
            this.baseType = 'Driver';
        }
        else {
            this.baseType = 'Component';
        }
    }
    this.maxmin = this.baseType === 'Assembly' ? '+' : '';

    this.cornerWidth=15;
    this.cornerHeight=15;

    this.outputPort=null;
    this.inputPort=null;
    this.fbOutputPort=null;
    this.fbInputPort=null;

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

    this.defaultBackgroundColor=new draw2d.Color(255,255,255);
    this.highlightBackgroundColor=new draw2d.Color(250,250,200);

    this.setDimension(this.getMinWidth(),this.getMinHeight());

    // do not allow moving, resizing or deleting
    this.setCanDrag(false);
    this.setResizeable(false);
    this.setDeleteable(false);

    // allow selection?
    this.setSelectable(true);

    this.inputsFigure = null;
    this.outputsFigure = null;

    this.figures = {};
    this.connections = {};
    this.margin = 20;
    this.drawDataFlows = true;
    this.drawDriverFlows = true;

    if (this.pathname === '') {
        // global dataflow figure is transparent with no border
        this.html.style.border = 'none';
    }
    else {
        // set background color
        this.setBackgroundColor(this.defaultBackgroundColor);

        // set initial color based on valid status
        if (this.valid === true) {
            this.setColor(new draw2d.Color(0,255,0));
        }
        else if (this.valid === false) {
            this.setColor(new draw2d.Color(255,0,0));
        }

        // listen for changes to valid status due to execution
        topic = pathname+'.exec_state';
        this.project.addListener(topic, this.setExecState.bind(this));
    }
};

openmdao.DataflowFigure.prototype=new draw2d.CompartmentFigure();

/** clean up (to be called after being removed from workflow/canvas) */
openmdao.DataflowFigure.prototype.destroy=function(){
    // remove listener
    topic = this.pathname+'.exec_state';
    this.project.removeListener(topic,this.setExecState.bind(this));
};

/** Remove component figure. */
openmdao.DataflowFigure.prototype.removeComponent = function(name_or_fig) {
    var fig = null;
    if (name_or_fig instanceof openmdao.DataflowFigure) {
        fig = name_or_fig;
    }
    else {
        fig = this.figures[name_or_fig];
        delete this.figures[name_or_fig];
    }
    fig.minimize();
    this.getWorkflow().removeFigure(fig);
    this.removeChild(fig);
    fig.destroy();
};

openmdao.DataflowFigure.prototype.createHTMLElement=function(){
    var item=draw2d.CompartmentFigure.prototype.createHTMLElement.call(this);

    item.id=this.id;
    item.className = "DataflowFigure";

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
    item.setAttribute('pathname', this.pathname);

    if (this.pathname !== '') {
        var circleIMG;
        if (this.maxmin === '+') {
           circleIMG = "url(/static/images/circle-plus.png)";
        }
        else if (this.maxmin === '-') {
           circleIMG = "url(/static/images/circle-minus.png)";
        }
        else {
           circleIMG = "url(/static/images/circle.png)";
        }

        this.top_left=document.createElement("div");
        this.top_left.style.background=circleIMG+" no-repeat top left";
        this.top_left.style.position="absolute";
        this.top_left.style.width=this.cornerWidth+"px";
        this.top_left.style.height=this.cornerHeight+"px";
        this.top_left.style.left="0px";
        this.top_left.style.top="0px";
        this.top_left.style.fontSize="2px";
        this.top_left.className="DataflowFigureTopLeft";

        this.top_right=document.createElement("div");
        this.top_right.style.background=circleIMG+" no-repeat top right";
        this.top_right.style.position="absolute";
        this.top_right.style.width=this.cornerWidth+"px";
        this.top_right.style.height=this.cornerHeight+"px";
        this.top_right.style.left="0px";
        this.top_right.style.top="0px";
        this.top_right.style.fontSize="2px";
        this.top_right.className="DataflowFigureTopRight";

        this.bottom_left=document.createElement("div");
        this.bottom_left.style.background=circleIMG+" no-repeat bottom left";
        this.bottom_left.style.position="absolute";
        this.bottom_left.style.width=this.cornerWidth+"px";
        this.bottom_left.style.height=this.cornerHeight+"px";
        this.bottom_left.style.left="0px";
        this.bottom_left.style.top="0px";
        this.bottom_left.style.fontSize="2px";
        this.bottom_left.className="DataflowFigureBottomLeft";

        this.bottom_right=document.createElement("div");
        this.bottom_right.style.background=circleIMG+" no-repeat bottom right";
        this.bottom_right.style.position="absolute";
        this.bottom_right.style.width=this.cornerWidth+"px";
        this.bottom_right.style.height=this.cornerHeight+"px";
        this.bottom_right.style.left="0px";
        this.bottom_right.style.top="0px";
        this.bottom_right.style.fontSize="2px";
        this.bottom_right.className="DataflowFigureBottomRight";

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
        this.header.className="DataflowFigureHeader";

        this.footer=document.createElement("div");
        this.footer.style.position="absolute";
        this.footer.style.left=this.cornerWidth+"px";
        this.footer.style.top="0px";
        this.footer.style.height=(this.cornerHeight)+"px";
        this.footer.style.backgroundColor="white";
        this.footer.style.borderBottom="1px solid #666666";
        this.footer.style.fontSize="2px";
        this.footer.className="DataflowFigureFooter";

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
        this.contentArea.className="DataflowFigureContentArea";
        this.disableTextSelection(this.contentArea);

        item.appendChild(this.top_left);
        item.appendChild(this.header);
        item.appendChild(this.top_right);
        item.appendChild(this.contentArea);
        item.appendChild(this.bottom_left);
        item.appendChild(this.footer);
        item.appendChild(this.bottom_right);

        /* Handle drag and drop from the Library. Use the code that
           deals with the layering problem of drag and drop where
           you can drop something that appears to be on top
           but it ends up in a layer below it
        */
        var self = this,
            maxmin = this.maxmin,
            elm = jQuery(item);

        elm.data('name', this.name);
        elm.data('pathname', this.pathname);
        elm.highlightAsDropTarget = function(){ self.highlightAsDropTarget(); };
        elm.unhighlightAsDropTarget = function(){ self.unhighlightAsDropTarget(); };

        // Boxes can be dragged into workflow list.
        elm.draggable({
            appendTo: 'body',
            helper: 'clone',
            opacity: 0.15
        });

        // Component names can be dropped into the diagram.
        elm.droppable({
            accept: '.IComponent',
            tolerance: 'pointer',
            // greedy: true,
            out: function(ev,ui){
                openmdao.drag_and_drop_manager.draggableOut(elm);
            },
            over: function(ev,ui){
                openmdao.drag_and_drop_manager.draggableOver(elm);
            },
            drop: function(ev,ui) {
                var dropTarget = openmdao.drag_and_drop_manager.getDropTarget(ev, ui);
                if (dropTarget) {
                    dropTarget.droppable('option', 'dropHandler')(ev, ui);
                }
            },
            dropHandler: function(ev,ui) {
                // could get same event multiple times if drop triggers for sibling targets
                if (this.dropEvent && this.dropEvent === ev.originalEvent) {
                    return;  // already handled this drop event
                }
                this.dropEvent = ev.originalEvent;

                var droppedObject = jQuery(ui.draggable).clone(),
                    droppedName = droppedObject.text(),
                    droppedPath = droppedObject.attr("modpath");

                openmdao.drag_and_drop_manager.reset();

                if (maxmin !== '') {
                    openmdao.project.addObject(droppedPath, droppedName,
                                             elm.data("pathname"));
                }
                else {
                    openmdao.project.replaceObject(droppedPath, droppedName,
                                                 elm.data("pathname"));
                }
            }
        });
    }

    return item;
};

/** Highlight this figure when the cursor is over it and it can accept a drop */
openmdao.DataflowFigure.prototype.highlightAsDropTarget=function(){
    var circleIMG = "url(/static/images/circle-plus-drop-zone.png)";
    this.bottom_right.style.backgroundImage=circleIMG ;
    this.bottom_left.style.backgroundImage=circleIMG ;
    this.contentArea.style.backgroundColor="#CFD6FE";
    this.footer.style.backgroundColor="#CFD6FE";
};

/** Turn off highlighting of this figure when it can no longer accept a drop */
openmdao.DataflowFigure.prototype.unhighlightAsDropTarget=function(){
    var circleIMG ;
    if (this.maxmin === '+') {
        circleIMG = "url(/static/images/circle-plus.png)";
    }
    else if (this.maxmin === '-') {
        circleIMG = "url(/static/images/circle-minus.png)";
    }
    else {
        circleIMG = "url(/static/images/circle.png)";
    }
    this.bottom_right.style.backgroundImage=circleIMG ;
    this.bottom_left.style.backgroundImage=circleIMG ;
    this.contentArea.style.backgroundColor="white";
    this.footer.style.backgroundColor="white";
};

/** double clicking on figure brings up a component editor on the component */
openmdao.DataflowFigure.prototype.onDoubleClick=function(){
    new openmdao.ObjectFrame(this.project, this.pathname);
};

/** hook into setWorkflow to add input & ouput ports */
openmdao.DataflowFigure.prototype.setWorkflow=function(wkflw){
    draw2d.Node.prototype.setWorkflow.call(this,wkflw);
    if (wkflw !== null && this.pathname !=='' && this.inputPort === null) {
        this.inputPort=new draw2d.InputPort();
        this.inputPort.setWorkflow(wkflw);
        this.inputPort.setName("input");
        this.inputPort.setCanDrag(false);
        this.inputPort.setId(this.pathname+"-input");

        this.outputPort=new draw2d.OutputPort();
        this.outputPort.setWorkflow(wkflw);
        this.outputPort.setName("output");
        this.outputPort.setCanDrag(true);
        this.outputPort.setId(this.pathname+"-output");

        this.fbOutputPort=new draw2d.OutputPort();
        this.fbOutputPort.setWorkflow(wkflw);
        this.fbOutputPort.setName("fbOutput");
        this.fbOutputPort.setCanDrag(true);
        this.fbOutputPort.setId(this.pathname+"-fbOutput");

        if (this.baseType === 'Driver') {
            this.fbInputPort=new draw2d.InputPort();
            this.fbInputPort.setWorkflow(wkflw);
            this.fbInputPort.setName("fbInput");
            this.fbInputPort.setCanDrag(false);
            this.fbInputPort.setId(this.pathname+"-fbInput");
        }

        var name = this.name,
            pathname = this.pathname,
            project = this.project;

        var onDragstart = function(x, y) {
            var dragStarted = draw2d.OutputPort.prototype.onDragstart.call(this, x, y);
            // Fix connectionLine's z-index.
            if (dragStarted) {
                this.parentNode.workflow.connectionLine.setZOrder(this.getZOrder());
            }
            return dragStarted;
        };
        this.outputPort.onDragstart = onDragstart;
        this.fbOutputPort.onDragstart = onDragstart;

        var onDragEnter = function(port) {
            draw2d.InputPort.prototype.onDragEnter.call(this, port);
            // Fix corona's z-index.
            if (this.corona) {
                this.corona.setZOrder(this.getZOrder());
            }
        };
        this.inputPort.onDragEnter = onDragEnter;
        if (this.baseType === 'Driver') {
            this.fbInputPort.onDragEnter = onDragEnter;
        }

        var createCommand = function(request) {
            if(request.getPolicy() === draw2d.EditPolicy.CONNECT) {
                if(request.source.parentNode.id===request.target.parentNode.id){
                    return null;
                }
                if (request.source instanceof draw2d.InputPort) {
                    var path = openmdao.Util.getPath(pathname),
                        src  = name,
                        dst  = request.source.getParent().name;
                    var f = new openmdao.ConnectionsFrame(project,path,src,dst);
                }
                return null;
            }
        };
        this.outputPort.createCommand = createCommand;
        this.fbOutputPort.createCommand = createCommand;

        this.addPort(this.inputPort,this.width/2,0);
        this.addPort(this.outputPort,this.width+5,this.height/2);
        this.addPort(this.fbOutputPort,-5,this.height/2);
        if (this.baseType === 'Driver') {
            this.addPort(this.fbInputPort,this.width/2,this.height);
        }
    }
};

/** determine if either the input or output port is connected to anything */
openmdao.DataflowFigure.prototype.isConnected=function(){
    return ((this.outputPort.getConnections().size > 0) ||
            (this.inputPort.getConnections().size > 0) ||
            (this.fbOutputPort.getConnections().size > 0) ||
            (this.fbInputPort && this.fbInputPort.getConnections().size > 0));
};

/** hook into drag start to detect click on max/min button */
openmdao.DataflowFigure.prototype.onDragstart=function(x,y){
    var dragStarted = draw2d.Node.prototype.onDragstart.call(this,x,y);
    if ((this.maxmin === '+' || this.maxmin === '-') &&
        y<this.cornerHeight && x<this.width && x>(this.width-this.cornerWidth)){
        this.toggle();
        return false;
    }
    else {
        // Display properties for this item in the properties tab.
        if (this.prop_fn) {
            this.prop_fn(this.pathname);
        }
        return dragStarted;
    }
};

/** TODO: enable moving a component into another dataflow */
openmdao.DataflowFigure.prototype.onFigureDrop=function(figure){
    draw2d.CompartmentFigure.prototype.onFigureDrop.call(this,figure);
    this.setBackgroundColor(this.defaultBackgroundColor);
};

/** set dimensions and relocate ports accordingly */
openmdao.DataflowFigure.prototype.setDimension=function(w,h){
    draw2d.CompartmentFigure.prototype.setDimension.call(this,w,h);
    if(this.hasOwnProperty('top_left') && this.top_left!==null){
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
    if (this.hasOwnProperty('outputPort') && this.outputPort!==null) {
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if (this.hasOwnProperty('inputPort') && this.inputPort!==null) {
        this.inputPort.setPosition(this.width/2,0);
    }
    if (this.hasOwnProperty('fbOutputPort') && this.fbOutputPort!==null) {
        this.fbOutputPort.setPosition(-5,this.height/2);
    }
    if (this.hasOwnProperty('fbInputPort') && this.fbInputPort!==null) {
        this.fbInputPort.setPosition(this.width/2,this.height);
    }
};

openmdao.DataflowFigure.prototype.setTitle=function(title){
    if (title.length > 0 && this.hasOwnProperty('header')) {
        this.header.innerHTML= title;
    }
};

openmdao.DataflowFigure.prototype.setContent=function(content){
    if (this.hasOwnProperty('contentArea')) {
        this.contentArea.innerHTML=content;
    }
};

openmdao.DataflowFigure.prototype.getMinWidth=function(){
    return 100;
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

/** context menu */
openmdao.DataflowFigure.prototype.getContextMenu=function(){
    var self = this,
        menu = new draw2d.Menu(),
        pathname = this.pathname,
        name = this.name,
        connections = this.connections,
        asm, txt;

    if (name.length > 0) {
        // menu header
        menu.appendMenuItem(new draw2d.MenuItem("<b>"+name+"</b>", null, function() {
        }));

        // edit
        menu.appendMenuItem(new draw2d.MenuItem("Edit", null, function(){
            cf = new openmdao.ObjectFrame(self.project, pathname);
        }));

        // properties
        menu.appendMenuItem(new draw2d.MenuItem("Properties", null, function() {
            var id = (pathname+'-properties').replace(/\./g,'-');
            f = new openmdao.PropertiesFrame(id, self.project).editObject(pathname);
        }));

        // evaluate, for ImplicitComponent only
        if (jQuery.inArray('IImplicitComponent', this.interfaces) >= 0) {
            menu.appendMenuItem(new draw2d.MenuItem("Evaluate", null, function() {
                var cmd = pathname + '.evaluate()';
                self.project.issueCommand(cmd);
            }));
        }

        // run
        menu.appendMenuItem(new draw2d.MenuItem("Run", null, function() {
            self.project.runComponent(pathname);
        }));

        // menu spacer
        menu.appendMenuItem(new draw2d.MenuItem("-", null, function() {
        }));

        // if maximized, add menu items for editing/toggling connections
        if (this.maxmin === '-') {
            menu.appendMenuItem(new draw2d.MenuItem("Edit Data Connections", null, function() {
                var f = new openmdao.ConnectionsFrame(self.project, pathname);
            }));

            if (this.drawDataFlows) {
                txt = 'Hide Data Connections';
            }
            else {
                txt = 'Show Data Connections';
            }
            menu.appendMenuItem(new draw2d.MenuItem(txt, null, function() {
                self.drawDataFlows = !self.drawDataFlows;
                self.maximize();
            }));

            if (this.drawDriverFlows) {
                txt = 'Hide Driver Connections';
            }
            else {
                txt = 'Show Driver Connections';
            }
            menu.appendMenuItem(new draw2d.MenuItem(txt, null, function() {
                self.drawDriverFlows = !self.drawDriverFlows;
                self.maximize();
            }));

            // menu spacer
            menu.appendMenuItem(new draw2d.MenuItem("-", null, function() {
            }));

            // Edit passthroughs
            menu.appendMenuItem(new draw2d.MenuItem("Edit Passthroughs", null, function() {
                var f = new openmdao.PassthroughsFrame(self.project, pathname);

            }));

            // menu spacer
            menu.appendMenuItem(new draw2d.MenuItem("-", null, function() {
            }));
        }

        // if not maximized and in an assembly, add menu item to disconnect
        asm = openmdao.Util.getPath(pathname);
        if ((this.maxmin !== '-') && (asm.length > 0)) {
            menu.appendMenuItem(new draw2d.MenuItem("Disconnect", null, function() {
                var cmd = asm + '.disconnect("'+name+'")';
                self.project.issueCommand(cmd);
            }));
        }

        // remove
        menu.appendMenuItem(new draw2d.MenuItem("Remove", null, function() {
            self.project.removeObject(pathname);
        }));

        menu.setZOrder(999999);
    }

    return menu;
};

openmdao.DataflowFigure.prototype.toggle = function() {
    if (this.maxmin === '+') {
        this.maximize();
    }
    else{
        this.minimize();
    }
};

/* show the minimized version of the figure, just a box with the type name */
openmdao.DataflowFigure.prototype.minimize=function(force){
    // remove all child figures
    var self = this,
        workflow = this.getWorkflow();

    if (self.maxmin === '-' || force) {
        self.maxmin = '+';
        if (self.hasOwnProperty('top_right')) {
            var circleIMG = "url(/static/images/circle-plus.png)";
            self.top_right.style.background=circleIMG+" no-repeat top right";
        }

        jQuery.each(self.figures,function(name,fig) {
            this.removeComponent(fig);
        });
        self.figures = {};

        jQuery.each(self.connections,function(name,conn) {
            workflow.removeFigure(conn);
        });
        self.connections = {};

        if (self.inputsFigure) {
            workflow.removeFigure(self.inputsFigure);
            self.removeChild(self.inputsFigure);
            self.inputsFigure = null;
        }

        if (self.outputsFigure) {
            workflow.removeFigure(self.outputsFigure);
            self.removeChild(self.outputsFigure);
            self.outputsFigure = null;
        }

        self.setContent(self.contentHTML);
        self.setDimension(self.getMinWidth(),self.getMinHeight());

        var parent = self.getParent();
        if (parent instanceof openmdao.DataflowFigure) {
            parent.layout();
        }
    }
};

/* show the maximized version of the figure, with subcomponents & connections */
openmdao.DataflowFigure.prototype.maximize=function() {
    // ensure the maxmin button is set to maximized
    if (this.maxmin === '+') {
        this.maxmin = '-';
        var circleIMG = "url(/static/images/circle-minus.png)";
        this.top_right.style.background=circleIMG+" no-repeat top right";
    }

    // get child data from project and redraw with child figures
    var self = this;
    self.project.getDataflow(self.pathname)
        .done(self.updateDataflow.bind(self))
        .fail(function(jqXHR, textStatus, errorThrown) {
            debug.error('Error getting dataflow for', self,
                        jqXHR, textStatus, errorThrown);
        });
};

/** update dataflow by recreating figures from JSON dataflow data */
openmdao.DataflowFigure.prototype.updateDataflow=function(json) {
    if (!json.hasOwnProperty('components')) {
        return;
    }

    var self=this,
        workflow = this.getWorkflow(),
        src_port, dst_port;

    if (! workflow) {
        // this can happen if the figure was deleted from the canvas
        // while we were waiting for the json data
        return;
    }

    this.setContent('');

    var flows = [];
    flows = flows.concat(json.connections);
    flows = flows.concat(json.parameters);
    flows = flows.concat(json.constraints);
    flows = flows.concat(json.objectives);
    flows = flows.concat(json.responses);

    jQuery.each(json.components,function(idx, comp) {
        var name = comp.name,
            type = comp.type,
            valid = comp.valid,
            interfaces = comp.interfaces,
            precedence = idx,  // Used to maintain original ordering.
            fig = self.figures[name];

        if (fig) {
            if (fig.pythonID === comp.python_id) {
                // Update precedence.
                fig.precedence = precedence;
            }
            else {
                // Set up for replacement.
                prededence = fig.precedence;
                self.removeComponent(name);
                fig = null;
                jQuery.each(flows, function(idx, conn) {
                    var src_name = conn[0].indexOf('.') < 0 ? '' : conn[0].split('.')[0],
                        dst_name = conn[1].indexOf('.') < 0 ? '' : conn[1].split('.')[0],
                        con_name = src_name+'-'+dst_name;
                        con = self.connections[con_name];
                    if (con && (name === src_name || name === dst_name)) {
                        workflow.removeFigure(self.connections[con_name]);
                        delete self.connections[con_name];
                    }
                });
            }
        }

        if (!fig) {
            if (self.pathname) {
                figname = self.pathname+'.'+name;
            }
            else {
                figname = name;
            }
            fig = new openmdao.DataflowFigure(self.project, figname, self.prop_fn,
                                              type, valid, interfaces);
            fig.pythonID = comp.python_id;
            fig.precedence = precedence;
            self.figures[name] = fig;
            self.addChild(fig);
            workflow.addFigure(fig,0,0);
        }

        if (fig.maxmin === '-' || self.pathname === '') {
            fig.maximize();
        }
    });

    if (!self.inputsFigure) {
        self.inputsFigure = new draw2d.Node();
        self.inputsFigure.html.style.border = 'none';
        self.addChild(self.inputsFigure);
        workflow.addFigure(self.inputsFigure,0,0);
    }

    if (!self.outputsFigure) {
        self.outputsFigure = new draw2d.Node();
        self.outputsFigure.html.style.border = 'none';
        self.addChild(this.outputsFigure);
        workflow.addFigure(self.outputsFigure,0,0);
    }

    var flow_colors = {
        data:       new draw2d.Color(100, 100, 100),
        parameter:  new draw2d.Color(  0,   0, 200),
        constraint: new draw2d.Color(  0,   0, 200),
        objective:  new draw2d.Color(  0,   0, 200),
        response:   new draw2d.Color(  0,   0, 200)
    };

    var displayFlow = function(conn, type, tabName) {
        var src_name = conn[0].indexOf('.') < 0 ? '' : conn[0].split('.')[0],
            dst_name = conn[1].indexOf('.') < 0 ? '' : conn[1].split('.')[0],
            src_fig = self.figures[src_name],
            dst_fig = self.figures[dst_name],
            con_name, con;

        if (!src_fig && conn[0].indexOf('.')>0) {
            // src_name is not a component, so the dotted path must indicate a vartree
            src_name = '';
            src_fig = self.figures[src_name];
        }

        if (!dst_fig && conn[1].indexOf('.')>0) {
            // dst_name is not a component, so the dotted path must indicate a vartree
            dst_name = '';
            dst_fig = self.figures[dst_name];
        }

        con_name = src_name+'-'+dst_name;
        con = self.connections[con_name];

        if (!con) {
            con = new draw2d.Connection();
            con.setLineWidth(2);
            con.setCoronaWidth(10);
            con.setColor(flow_colors[type]);

            if (src_name.length > 0) {
                if (src_fig) {
                    if (type === 'data' || type === 'parameter') {
                        con.setSource(src_fig.getPort("output"));
                    } else {
                        con.setSource(src_fig.getPort("fbOutput"));
                    }
                    con.setZOrder(self.getZOrder()+2);
                }
                else {
                    debug.error('DataflowFigure.displayFlow()',self.pathname,'cannot find source component:',src_name);
                    return;
                }
            }
            else {
                src_port = new draw2d.OutputPort();
                src_port.setWorkflow(workflow);
                src_port.setName(con_name);
                src_port.setCanDrag(false);
                src_port.setId(con_name);
                self.inputsFigure.addPort(src_port,0,0);
                con.setSource(src_port);
                con.setRouter(null);
                con.setZOrder(self.getZOrder()+1);
            }

            if (dst_name.length > 0) {
                if (dst_fig) {
                    if (type === 'data' || type === 'parameter') {
                        con.setTarget(dst_fig.getPort("input"));
                    } else {
                        con.setTarget(dst_fig.getPort("fbInput"));
                    }
                    con.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                    con.setZOrder(self.getZOrder()+2);
                }
                else {
                    debug.error('DataflowFigure.displayFlow()',self.pathname,'cannot find destination component:',dst_name);
                    return;
                }
            }
            else {
                dst_port = new draw2d.InputPort();
                dst_port.setWorkflow(workflow);
                dst_port.setName(con_name);
                dst_port.setCanDrag(false);
                dst_port.setId(con_name);
                self.outputsFigure.addPort(dst_port,0,0);
                con.setTarget(dst_port);
                con.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                con.setRouter(null);
                con.setZOrder(self.getZOrder()+1);
            }

            // context menu
            con.getContextMenu=function(){
                var menu=new draw2d.Menu();
                if (type === 'data') {
                    menu.appendMenuItem(new draw2d.MenuItem("Edit Connections",null,
                        function(){
                            var f = new openmdao.ConnectionsFrame(self.project,
                                                 self.pathname,src_name,dst_name);
                        })
                    );
                }
                else {
                    var driver = (type === 'parameter') ? src_name : dst_name;
                    menu.appendMenuItem(new draw2d.MenuItem("Edit Driver", null,
                        function() {
                            var f = new openmdao.ObjectFrame(self.project,
                                                             self.pathname+'.'+driver,
                                                             tabName);
                        })
                    );
                }
                menu.setZOrder(self.getZOrder()+3);
                return menu;
            };

            // double click brings up connection frame if between two components
            // FIXME: usually doesn't work... DataflowFigure steals the clicks
            if ((src_name.length > 0) && (dst_name.length > 0)) {
                if (type === 'data') {
                    con.onDoubleClick = function() {
                        var f = new openmdao.ConnectionsFrame(self.project, self.pathname,
                                                              src_name, dst_name);
                    };
                }
                else {
                    var driver = (type === 'parameter') ? src_name : dst_name;
                    con.onDoubleClick = function() {
                        var f = new openmdao.ObjectFrame(self.project, self.pathname+'.'+driver);
                    };
                }
            }

            workflow.addFigure(con);
            self.connections[con_name] = con;
        }
    };

    flows = [];
    if (this.drawDataFlows) {
        jQuery.each(json.connections, function(idx, flow) {
            displayFlow(flow, 'data', 'Inputs'); });
        flows = flows.concat(json.connections);
    }
    if (this.drawDriverFlows) {
        jQuery.each(json.parameters, function(idx, flow) {
            displayFlow(flow, 'parameter', 'Parameters');
        });
        flows = flows.concat(json.parameters);
        jQuery.each(json.constraints, function(idx, flow) {
            displayFlow(flow, 'constraint', 'Constraints');
        });
        flows = flows.concat(json.constraints);
        jQuery.each(json.objectives, function(idx, flow) {
            displayFlow(flow, 'objective', 'Objectives');
        });
        flows = flows.concat(json.objectives);
        jQuery.each(json.responses, function(idx, flow) {
            displayFlow(flow, 'response', 'Responses');
        });
        flows = flows.concat(json.responses);
    }

    // remove any component figures that are not in the updated data
    jQuery.each(self.figures,function(name,fig){
        var found = false;
        jQuery.each(json.components, function(idx,comp) {
            if (comp.name === name) {
                found = true;
            }
        });
        if (!found) {
            self.removeComponent(name);
        }
    });

    jQuery.each(self.connections,function(name,fig){
        var found = false;
        jQuery.each(flows, function(idx,conn) {
            var src_name = conn[0].indexOf('.') < 0 ? '' : conn[0].split('.')[0],
                dst_name = conn[1].indexOf('.') < 0 ? '' : conn[1].split('.')[0],
                con_name = src_name+'-'+dst_name;
            if (con_name === name) {
                found = true;
            }
        });
        if (!found) {
            workflow.removeFigure(fig);
            delete self.connections[name];
        }
    });

    this.layout();
};

/** layout component figures */
openmdao.DataflowFigure.prototype.layout=function() {
    var self = this,
        connected = [],
        unconnected = [],
        x0 = this.getAbsoluteX(),
        y0 = this.getAbsoluteY(),
        margin = this.margin,
        x = x0 + margin,
        y = y0 + margin + this.cornerHeight,
        row_height = 0,
        canvas_width = this.getWorkflow().getWidth(),
        compare = function(fig1, fig2) { return fig1.precedence - fig2.precedence; };

    jQuery.each(self.figures, function(idx,fig) {
        if (fig.isConnected()) {
            connected.push(fig);
        }
        else {
            unconnected.push(fig);
        }
    });

    // Maintain original order in spite of replacements.
    connected.sort(compare);
    unconnected.sort(compare);

    // Add some room for incoming arrowheads.
    if (connected.length > 0) {
        var first = connected[0];
        if (first.inputPort.getConnections().size > 0) {
            y += this.cornerHeight;
        }
        if (first.fbOutputPort.getConnections().size > 0) {
            x  += this.cornerWidth;
            x0 += this.cornerWidth;
        }
    }

    // connected components are laid out diagonally (top left to bottom right)
    jQuery.each(connected,function(idx,fig) {
        fig.setPosition(x,y);
        x = x + fig.getWidth()  + margin;
        y = y + fig.getHeight() + margin;
    });

    // Add some room for incoming arrowheads to a trailing driver.
    if (connected.length > 0) {
        var last = connected[connected.length-1];
        if (last.fbInputPort && last.fbInputPort.getConnections().size > 0) {
            y += this.cornerHeight;
        }
    }

    // unconnected components are laid out in rows
    x = x0 + margin;
    jQuery.each(unconnected,function(idx,fig) {
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

    // resize
    self.resize();

    // line up assembly inputs and outputs with their component ports
    jQuery.each(self.connections, function(name,conn) {
        var src_port = conn.getSource(),
            dst_port = conn.getTarget();

        if (src_port.getName() === name) {
            // source port is on the assembly
            var dstX = dst_port.getAbsoluteX(),
                X0 = self.inputsFigure.getAbsoluteX();
            src_port.setPosition(dstX-X0,0);
        }

        if (dst_port.getName() === name) {
            // destination port is on the assembly
            var srcY = src_port.getAbsoluteY(),
                Y0 = self.outputsFigure.getAbsoluteY();
            dst_port.setPosition(0,srcY-Y0);
        }
    });

    // layout parent to accomodate new size
    var parent = self.getParent();
    if (parent instanceof openmdao.DataflowFigure) {
        parent.layout();
    }
};

/** resize figure to contain all it's children */
openmdao.DataflowFigure.prototype.resize=function(){
    var width, height, i,
        x0=this.getAbsoluteX(),
        y0=this.getAbsoluteY(),
        xmin=999999, xmax=0,
        ymin=999999, ymax=0,
        children = this.getChildren(),
        inport = null,  inportX = null,
        outport = null, outportY = null;

    for (i=0; i<children.size; i++) {
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
            y = child.getAbsoluteY();
            if (y < ymin) {
                ymin = y;
            }
            y = y + child.getHeight();
            if (y > ymax) {
                ymax = y;
            }
        }
    }

    width  = xmax-xmin+this.margin*2;
    height = ymax-ymin+this.margin*2 + this.cornerHeight;

    // Adjust for connections.
    var connected = [],
        unconnected = 0,
        compare = function(fig1, fig2) { return fig1.precedence - fig2.precedence; };

    jQuery.each(this.figures, function(idx,fig) {
        if (fig.isConnected()) {
            connected.push(fig);
        }
        else {
            unconnected += 1;
        }
    });
    connected.sort(compare);
    if (connected.length > 0) {
        var first = connected[0];
        if (first.inputPort.getConnections().size > 0) {
            height += this.cornerHeight;
        }
        if (first.fbOutputPort.getConnections().size > 0) {
            width += this.cornerWidth;
        }
        var last = connected[connected.length-1];
        if (last.outputPort.getConnections().size > 0) {
            width += this.cornerWidth;
        }
        if (unconnected === 0) {
            if (last.fbInputPort && last.fbInputPort.getConnections().size > 0) {
                height += this.cornerHeight;
            }
        }
    }

    this.setDimension(width,height);

    if (this.inputsFigure) {
        this.inputsFigure.setDimension(width - 2*this.margin, 1);
        this.inputsFigure.setPosition(x0 + this.margin,
                                      y0 + this.cornerHeight + 2);
    }

    if (this.outputsFigure) {
        this.outputsFigure.setDimension(1, height - this.cornerHeight - this.margin);
        this.outputsFigure.setPosition(x0 + width - 2,
                                       y0 + this.cornerHeight);
    }
};

openmdao.DataflowFigure.prototype.setExecState=function(message){
    var state = message[1];
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
