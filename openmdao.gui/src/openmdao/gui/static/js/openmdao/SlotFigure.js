
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotFigure=function(myModel,pathname,type,filled){
    this.myModel = myModel;

    self.myModel = myModel; // TODO : Need this?


    this.pathname = pathname;
    this.type = type;
    this.filled = filled;
    this.cornerWidth=15;
    this.cornerHeight=15;
    this.outputPort=null;
    this.inputPort=null;
    draw2d.Node.call(this);
    this.setDimension(100,50);
    this.originalHeight=-1;

    // get name for this figure and set title appropriately
    this.name = openmdao.Util.getName(pathname);
    this.setTitle(this.name);

    // set the content text to be the type name (in italics)
    var tok = type.split('.');
    if (tok.length > 1) {
        this.setContent('<center><i>'+tok[tok.length-1]+'</i></center>');
    }
    else {
        this.setContent('<center><i>'+String(type)+'</i></center>');
    }

    // do not allow moving (TODO: allow moving)
    this.setCanDrag(false);

    if (! this.filled) {
        //this.setColor(new draw2d.Color(0,255,0));
        this.header.style.color="#CC0000";
        this.textarea.style.color="#CC0000";
    }
};

openmdao.SlotFigure.prototype=new draw2d.Node();

openmdao.SlotFigure.prototype.type="SlotFigure";

openmdao.SlotFigure.prototype.createHTMLElement=function(){
    var circleIMG = "url(/static/images/circle.png)";
    var self = this;

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

    // handle the drag and drop to the slots 
    //   using the drag and drop manager
    var elm = jQuery(item);
    openmdao.drag_and_drop_manager.addDroppable( elm ) ;
    elm.addClass("SlotFigure");

    elm.data('corresponding_openmdao_object',this);
    elm.droppable ({
        accept: "." + this.type,
        out: function(ev,ui){
            var o = elm.data('corresponding_openmdao_object');
            o.unhighlightAsDropTarget() ;
            openmdao.drag_and_drop_manager.draggableOut( elm ) ;

            calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( elm ) ;
            topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( elm ) ;

            debug.info( "out of slot", elm[0].id, o.name ) ;
            //debug.info ("over", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )

        },
        over: function(ev,ui){
            var o = elm.data('corresponding_openmdao_object');
            openmdao.drag_and_drop_manager.draggableOver( elm ) ;

            calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( elm ) ;
            topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( elm ) ;

            debug.info( "over slot", elm[0].id, o.name ) ;

            if ( o.name === "warm_start_data" ) {
                debug.info( "over slot for warm start data" ) ;
            }

            //debug.info ("over", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )
        },

        drop: function(ev,ui) { 
            /* divs could be in front of divs and the div that gets the drop
               event might not be the one that is in front visibly and therefore
               is not the div the user wants the drop to occur on
            */

            var o = elm.data('corresponding_openmdao_object');
            debug.info( "dropping on slot", elm[0].id, o.name ) ;


            top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent_ver2( ev, ui ) ;
            /* call the method on the correct div to handle the drop */
            var drop_function = top_div.droppable( 'option', 'actualDropHandler');
            drop_function( ev, ui ) ;
        }, 

        actualDropHandler: function(ev,ui) { 
            
            var o = elm.data('corresponding_openmdao_object');
            debug.info( "actual drop on slot", elm[0].id, o.name ) ;

            /* new way */
            var droppedObject = jQuery(ui.draggable).clone(),
            droppedName = droppedObject.text(),
            droppedPath = droppedObject.attr("modpath"),
            module = openmdao.Util.getPath(droppedPath),
            klass = openmdao.Util.getName(droppedPath);
            
            cmd = 'from '+module+' import '+klass+'\n'
                +  self.pathname+'='+klass+'()';
            self.myModel.issueCommand(cmd);
            
            /* old way */
            // var droppedObject = jQuery(ui.draggable).clone(),
            // droppedName = droppedObject.text(),
            // droppedPath = droppedObject.attr("modpath"),
            // off = elm.parent().offset(),
            // x = Math.round(ui.offset.left - off.left),
            // y = Math.round(ui.offset.top - off.top),
            // o = elm.data('corresponding_openmdao_object'),
            // model = o.myModel ;
            // model.addComponent(droppedPath,o.name,o.pathname);

            openmdao.drag_and_drop_manager.clearHighlightingDroppables() ;
            openmdao.drag_and_drop_manager.clearDroppables() ;
        }

    }
                  ) ;



    return item;
};


openmdao.SlotFigure.prototype.highlightAsDropTarget=function(){
    var circleIMG = "url(/static/images/circle-plus-drop-zone.png)";
    this.bottom_right.style.backgroundImage=circleIMG ;
    this.bottom_left.style.backgroundImage=circleIMG ;
    this.textarea.style.backgroundColor="#CFD6FE";
    this.footer.style.backgroundColor="#CFD6FE";

    debug.info( "highlighting slot", this.id ) ;

};

openmdao.SlotFigure.prototype.unhighlightAsDropTarget=function(){
    var circleIMG ;
    if (this.maxmin === '+') {
        circleIMG = "url(/static/images/circle-plus.png)";
    } else if (this.maxmin === '-') {
        circleIMG = "url(/static/images/circle-minus.png)";
    } else {
        circleIMG = "url(/static/images/circle.png)";
    }
    this.bottom_right.style.backgroundImage=circleIMG ;
    this.bottom_left.style.backgroundImage=circleIMG ;
    this.textarea.style.backgroundColor="white";
    this.footer.style.backgroundColor="white";
};




openmdao.SlotFigure.prototype.setDimension=function(w,h){
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
    if (this.outputPort!==null) {
        this.outputPort.setPosition(this.width+5,this.height/2);
    }
    if (this.inputPort!==null) {
        this.inputPort.setPosition(this.width/2,0);
    }
};

openmdao.SlotFigure.prototype.setTitle=function(title){
    this.header.innerHTML=title;
};

openmdao.SlotFigure.prototype.setContent=function(_5014){
    this.textarea.innerHTML=_5014;
};

openmdao.SlotFigure.prototype.onDragstart=function(x,y){
    var _5017=draw2d.Node.prototype.onDragstart.call(this,x,y);
    if(this.header===null){
        return false;
    }
    if(y<this.cornerHeight&&x<this.width&&x>(this.width-this.cornerWidth)){
        this.toggle();
        return false;
    }
    if(this.originalHeight===-1){
        if(this.canDrag===true && x<parseInt(this.header.style.width,10) &&
                                  y<parseInt(this.header.style.height,10)) {
            return true;
        }
    }else{
        return _5017;
    }
};

openmdao.SlotFigure.prototype.setCanDrag=function(flag){
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

openmdao.SlotFigure.prototype.setWorkflow=function(wkflw){
    draw2d.Node.prototype.setWorkflow.call(this,wkflw);
    if(wkflw!==null && this.inputPort===null){
        this.inputPort=new draw2d.InputPort();
        this.inputPort.setWorkflow(wkflw);
        this.inputPort.setName("input");
        this.addPort(this.inputPort,this.width/2,0);

        this.outputPort=new draw2d.OutputPort();
        this.outputPort.setWorkflow(wkflw);
        this.outputPort.setName("output");
        var oThis=this;
        this.outputPort.createCommand = function(request) {
            if(request.getPolicy() === draw2d.EditPolicy.CONNECT) {
                if( request.source.parentNode.id === request.target.parentNode.id) {
                    return null;
                }
                if (request.source instanceof draw2d.InputPort) {
                    var path = openmdao.Util.getPath(oThis.pathname),
                        src  = oThis.name,
                        dst  = request.source.getParent().name,
                        f = openmdao.ConnectionsFrame(oThis.myModel,path,src,dst);
                }
                return null;
            }
        };
        this.addPort(this.outputPort,this.width+5,this.height/2);
    }
};

openmdao.SlotFigure.prototype.toggle=function(){
    if(this.originalHeight === -1){
        this.originalHeight=this.height;
        this.setDimension(this.width,this.cornerHeight*2);
        this.setResizeable(false);
    }else{
        this.setDimension(this.width,this.originalHeight);
        this.originalHeight=-1;
        this.setResizeable(true);
    }
};

openmdao.SlotFigure.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var oThis=this;
    // TODO: what context menu options make sense here?
    menu.setZOrder(999999);
    return menu;
};

openmdao.SlotFigure.prototype.onDoubleClick=function(){
    if (this.filled) {
        var f = new openmdao.ComponentFrame(this.myModel,this.pathname);
    }
};

openmdao.SlotFigure.prototype.onMouseEnter=function(){
    this.setColor(new draw2d.Color(0,255,0));
    //this.getWorkflow().showTooltip(new openmdao.Tooltip(this.pathname),true);
};

openmdao.SlotFigure.prototype.onMouseLeave=function(){
    this.setColor(null);
};
