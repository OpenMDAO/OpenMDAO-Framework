
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        slotsID = "#"+pathname.replace(/\./g,'-')+"-slots",
        slotsCSS = 'height:'+(screen.height-100)+'px;'+
                   'width:'+(screen.width-100)+'px;'+
                   'position:relative; background-color:black;',
        slotsDiv = jQuery('<div id='+slotsID+' style="'+slotsCSS+'">').appendTo(elm),
        slots = new draw2d.Workflow(slotsID);

    var true_dropdiv = null ; 


    self.pathname = pathname;

    slots.setBackgroundImage( null, true);
    elm.css({ 'overflow':'auto' });
    slots.setViewPort(elm.attr('id'));

        true_dropdiv = slotsDiv.parent() ;
        true_dropdiv.data('corresponding_openmdao_object',this);
        openmdao.drag_and_drop_manager.addDroppable( true_dropdiv ) ;

        /* dataflowDiv.droppable ({ */
        true_dropdiv.droppable ({
            accept: '.objtype',
            
            out: function(ev,ui){
                
                var o = true_dropdiv.data('corresponding_openmdao_object');
                //o.unhighlightAsDropTarget() ; /* TODO: need this ?? */
                openmdao.drag_and_drop_manager.draggableOut( true_dropdiv ) ;
                
                debug.info( "out of slots pane true drop div" ) ;

                /* Just used for debugging */
                calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( true_dropdiv ) ;
                topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( true_dropdiv ) ;
                
                //debug.info ("out", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )
                
            },
            over: function(ev,ui){
                openmdao.drag_and_drop_manager.draggableOver( true_dropdiv ) ;
                
                debug.info( "over slots pane true drop div" ) ;


                /* calculated_zindex = openmdao.drag_and_drop_manager.computeCalculatedZindex( elm ) ;
                   topmost_zindex = openmdao.drag_and_drop_manager.computeTopmostZindex( elm ) ;
                   debug.info ("over", elm.find(".DataflowFigureHeader")[0].innerHTML, calculated_zindex, topmost_zindex )
                */
            },

        drop: function(ev,ui) { 
            /* divs could be in front of divs and the div that gets the drop
               event might not be the one that is in front visibly and therefore
               is not the div the user wants the drop to occur on
            */

            var o = elm.data('corresponding_openmdao_object');
            debug.info( "dropping on slots pane" ) ;


            top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent_ver2( ev, ui ) ;
            /* call the method on the correct div to handle the drop */
            if ( top_div ) {
                var drop_function = top_div.droppable( 'option', 'actualDropHandler');
                drop_function( ev, ui ) ;
            }
        }, 


            
        }
                              ) ;

    // TODO: Do I really need these ?
    this.highlightAsDropTarget=function(){
        debug.info ("highlight slotspane", slotsDiv[0].id ) ;
    };
    
    this.unhighlightAsDropTarget=function(){
        debug.info ("unhighlight slotspane", slotsDiv[0].id ) ;
    };


    // // make the slots pane droppable
    // slotsDiv.droppable ({
    //     accept: '.objtype',
    //     drop: function(ev,ui) {
    //         // get the object that was dropped and where it was dropped
    //         var droppedObject = jQuery(ui.draggable).clone(),
    //             droppedName = droppedObject.text(),
    //             droppedPath = droppedObject.attr("path"),
    //             off = slotsDiv.parent().offset(),
    //             x = Math.round(ui.offset.left - off.left),
    //             y = Math.round(ui.offset.top - off.top);
    //         var elem = slotsDiv[0];
    //         var zindex = document.defaultView.getComputedStyle(elem,null).getPropertyValue("z-index");
    //         debug.info(droppedName,'dropped on slots:',self.pathname,'z-index',slotsDiv.css('z-index'),'zIndex',slotsDiv.css('zIndex'));
    //         if (droppedObject.hasClass('objtype')) {
    //             openmdao.Util.promptForValue('Specify a name for the new '+droppedName,function(name) {
    //                 model.addComponent(droppedPath,name,self.pathname);
    //             });
    //         }
    //         else if (droppedObject.hasClass('obj')) {
    //             var cmd = self.pathname+'='+droppedPath;
    //             debug.info('SlotsPane:',cmd);
    //             model.issueCommand(cmd);
    //         }
    //     }
    // });

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            var name = slot.name,
                type = slot.klass,
                filled = slot.filled,
                fig;

            if (self.pathname) {
                fig = new openmdao.SlotFigure(model,self.pathname+'.'+name,type,filled);
            }
            else {
                fig = new openmdao.SlotFigure(model,name,type,filled);
            }

            fig.setTitle(name);
            figures[name] = fig;
            fig.setContent('<center>(('+type+'))'+'</center>');
            // TODO: flexible grid layout (adjusting for size)
            var count = Object.keys(figures).length,
                x = (count-1)*(fig.getWidth()+20)  + 20,
                y = 20;
            //debug.info('count=',count,'x=',x,'y=',y)
            slots.addFigure(fig,x,y);
        });
    }

    /***********************************************************************
     *  protected
     ***********************************************************************/

    /** update slots diagram */
    this.loadData = function(json) {
        slots.clear();
        figures = {};
        if (Object.keys(json).length > 0) {
            updateFigures(json,false);
        }
    };

};
