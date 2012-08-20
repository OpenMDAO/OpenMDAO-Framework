
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        slotsID = pathname.replace(/\./g,'-')+"-slots",
        slotsDiv = jQuery('<div style="position:relative; background-color:black;">')
            .appendTo(elm),
        slotHTML = '<div height="50" width="100" style="margin:10px;float:left;">'
                 + '<svg height="50" width="100">'
                 + '    <rect height="50" width="100" rx="15" ry="15" '
                 + '          style="stroke-dasharray:3; stroke:red; stroke-width:2; fill:white" />'
                 + '    <text id="name" x="50" y="20" text-anchor="middle">Name</text>'
                 + '    <text id="klass" x="50" y="40" font-style="italic" text-anchor="middle">Klass</text>'
                 + '</svg>'
                 + '</div>';

    self.pathname = pathname;

    elm.css({'overflow':'auto'});

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            var name = slot.name,
                type = slot.klass,
                filled = slot.filled,
                color = filled ? 'green' : 'red',
                fig = jQuery(slotHTML).draggable();

            figures[name] = fig;

            fig.attr('id','slot-'+self.pathname+'.'+name);
            fig.attr('title',name);
            fig.find('rect').css({'stroke': color});
            fig.find('#name').css({'fill': color}).text(name);
            fig.find('#klass').css({'fill': color}).text(type);
            if (self.pathname) {
                fig.pathname = self.pathname+'.'+name;
            }
            else {
                fig.pathname = name;
            }
            fig.model = model;
            fig.klass = slot.klass;
            fig.filled = slot.filled;
            slotsDiv.append(fig);

            openmdao.drag_and_drop_manager.addDroppable(fig);
            fig.addClass("SlotFigure");
            fig.data('corresponding_openmdao_object',fig);
            fig.droppable ({
                accept: '.'+slot.klass,
                out: function(ev,ui){
                    fig.unhighlightAsDropTarget() ;
                    openmdao.drag_and_drop_manager.draggableOut(fig);
                },
                over: function(ev,ui){
                    openmdao.drag_and_drop_manager.draggableOver(fig);
                },
                drop: function(ev,ui) {
                    top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev, ui);
                    var drop_function = top_div.droppable( 'option', 'actualDropHandler');
                    drop_function(ev, ui);
                },
                actualDropHandler: function(ev,ui) {
                    var droppedObject = jQuery(ui.draggable).clone(),
                        droppedName = droppedObject.text(),
                        droppedPath = droppedObject.attr("modpath"),
                        module = openmdao.Util.getPath(droppedPath),
                        klass = openmdao.Util.getName(droppedPath);
                        cmd = 'from '+module+' import '+klass+';\n'
                            +  self.pathname+'='+klass+'()';
                    model.issueCommand(cmd);
                    openmdao.drag_and_drop_manager.clearHighlightingDroppables();
                }
            });

            /** Highlight figure when cursor is over it and it can accept a drop */
            fig.highlightAsDropTarget=function(){
                fig.find('rect').css({'fill': 'gray'});
            };

            /** Unhighlight figure when it can no longer accept a drop because
                the cursor is not over it or another drop target is over it */
            fig.unhighlightAsDropTarget=function(){
                fig.find('rect').css({'fill': 'white'});
            };
        });
    }

    /***********************************************************************
     *  protected
     ***********************************************************************/

    /** update slots diagram */
    this.loadData = function(json) {
        slotsDiv.html('');
        figures = {};
        if (Object.keys(json).length > 0) {
            updateFigures(json,false);
        }
    };

};
