
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.SlotsPane = function(elm,model,pathname,name,editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        slotsID = pathname.replace(/\./g,'-')+"-slots",
        slotsDiv = jQuery('<svg style="position:relative; background-color:black;">')
            .appendTo(elm),
        slotHTML = '<svg>'
                 + '    <rect height="50" width="100" rx="15" ry="15" '
                 + '          style="stroke-dasharray:5; stroke:red; stroke-width:2; fill:gray" />'
                 + '    <text x="5" y="30" font-style="italic">Slot</text>'
                 + '</svg>',
        fig = slotsDiv.parent();

    self.pathname = pathname;

    elm.css({'overflow':'auto'});

    function getInteresectedElements(evt) {
      var rpos = svgroot.createSVGRect();
      rpos.x = evt.clientX;
      rpos.y = evt.clientY;
      rpos.width = rpos.height = 1;
      return root.getIntersectionList(rpos, null);
    }

    /** update slots by recreating figures from JSON slots data
     *  TODO: prob just want to iterate through & update existing figures
     */
    function updateFigures(json) {
        jQuery.each(json, function(idx,slot) {
            var name = slot.name,
                type = slot.klass,
                filled = slot.filled,
                fig = jQuery(slotHTML);

            figures[name] = fig;

            fig.attr('title',name);
            fig.find('text').text(name);
            if (self.pathname) {
                fig.pathname = self.pathname+'.'+name;
            }
            else {
                fig.pathname = name;
            }
            fig.model = model;
            fig.type = slot.klass;
            fig.filled = slot.filled;
            // TODO: flexible grid layout (adjusting for size)
            fig.attr('x', idx*120 + 20);
            fig.attr('y', 20);
            slotsDiv.append(fig);

//            fig.droppable ({
//                accept: '.objtype',
//                over: function(ev,ui){
//                    var droppedObject = jQuery(ui.draggable).clone(),
//                        droppedName = droppedObject.text(),
//                        droppedPath = droppedObject.attr("modpath");
//                    debug.info('DnD over ',fig.pathname,droppedPath);
//                },
//                out: function(ev,ui) {
//                    var droppedObject = jQuery(ui.draggable).clone(),
//                        droppedName = droppedObject.text(),
//                        droppedPath = droppedObject.attr("modpath");
//                    debug.info('DnD out ',fig.pathname,droppedPath);
//                },
//                drop: function(ev,ui) {
//                    var droppedObject = jQuery(ui.draggable).clone(),
//                        droppedName = droppedObject.text(),
//                        droppedPath = droppedObject.attr("modpath");
//                    debug.info('DnD drop ',fig.pathname,droppedPath);
//                    model.issueCommand(fig.pathname+'='+droppedPath+'()');
//                }
//            });

            true_dropdiv = slotsDiv.parent() ;
            true_dropdiv.data('corresponding_openmdao_object',this);
            openmdao.drag_and_drop_manager.addDroppable(true_dropdiv);

            /* slotsDiv.droppable ({ */
            true_dropdiv.droppable ({
                accept: '.IComponent',

                out: function(ev,ui){
                    openmdao.drag_and_drop_manager.draggableOut( true_dropdiv ) ;
                },
                over: function(ev,ui){
                    openmdao.drag_and_drop_manager.draggableOver( true_dropdiv ) ;
                },
                drop: function(ev,ui) {
                    top_div = openmdao.drag_and_drop_manager.getTopDroppableForDropEvent(ev,ui);
                    if (top_div) {
                        var drop_function = top_div.droppable('option','actualDropHandler');
                        drop_function(ev,ui);
                    }
                },
                actualDropHandler: function(ev,ui) {
                    var droppedObject = jQuery(ui.draggable).clone(),
                    droppedName = droppedObject.text(),
                    droppedPath = droppedObject.attr("modpath"),
                    model = true_dropdiv.data("corresponding_openmdao_object").openmdao_model;

                    openmdao.drag_and_drop_manager.clearHighlightingDroppables() ;
                    openmdao.drag_and_drop_manager.clearDroppables() ;

                    openmdao.Util.promptForValue('Enter name for new '+ droppedName, function(name) {
                         model.addComponent(droppedPath,name,self.pathname);
                     });
                }
            });
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
