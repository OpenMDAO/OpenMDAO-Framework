
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowDiagram=function(elm, model, json){
    var self = this,
        pathname = json.pathname,
        id = 'WorkflowDiagram-'+pathname.replace(/\./g,'-'),
        diagram = jQuery('<div class="WorkflowDiagram" id="'+id+'" style="position:absolute" />'),
        driver = new openmdao.WorkflowComponentFigure(diagram, model,
                                json.pathname, json.type, json.valid),
        flow_css = 'border-style:solid;border-color:black;border-width:thin;background-color:white;',
        flow_fig = jQuery('<div class="WorkflowFigure" style="'+flow_css+'" />'),
        comp_figs = {},
        defaultBackgroundColor = '#FFFFFF',
        highlightBackgroundColor = '#FAFAC8',
        dropHighlightBackgroundColor = '#CFD6FE',
        horizontal = true,
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(diagram);

    // add diagram to parent element, add driver & flow figs to diagram
    elm.append(diagram);
    diagram.append(driver);
    diagram.append(flow_fig);

    // position flow fig to overlap bottom right corner of driver fig
    flow_fig.css({ 'position': 'absolute',
                   'left': driver.getWidth()  - 10 + 'px',
                   'top':  driver.getHeight() - 10 + 'px' });

    // set up flow_fig as a drop target for components to add to workflow
//    flow_fig.data('name',name);
    flow_fig.data('pathname',pathname);

    flow_fig.highlightAsDropTarget = function() {
        diagram.find('rect').css({ 'fill': dropHighlightBackgroundColor });
    };

    flow_fig.unhighlightAsDropTarget = function() {
        diagram.find('rect').css({ 'fill': defaultBackgroundColor });
    };

    flow_fig.droppable ({
        accept: '.component, .IComponent',
            out: function(ev,ui) {
                openmdao.drag_and_drop_manager.draggableOut(diagram);
            },
            over: function(ev,ui) {
                // only allow drops of components in same assembly as driver
                var target_pathname = diagram.data('pathname'),
                    target_parent = openmdao.Util.getPath(target_pathname),
                    dragged_object = jQuery(ui.draggable).clone(),
                    dragged_pathname,
                    dragged_parent;
                if (dragged_object.hasClass('component')) {
                    dragged_pathname = jQuery(ui.draggable ).parent().attr("path");
                    dragged_parent = openmdao.Util.getPath(dragged_pathname);
                    if (dragged_parent === target_parent) {
                        openmdao.drag_and_drop_manager.draggableOver(diagram);
                    }
                }
                else if (dragged_object.hasClass('IComponent')) {
                    openmdao.drag_and_drop_manager.draggableOver(diagram);
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
                var target_pathname = diagram.data('pathname'),
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

    // create context menu
    contextMenu.append(jQuery('<li><b>'+self.name+'</b></li>'));
    contextMenu.append(jQuery('<li>Flip Workflow</li>').click(function(e) {
        horizontal = !horizontal;
        redraw();
    }));
    contextMenu.append(jQuery('<li>Clear Workflow</li>').click(function(e) {
        var cmd = pathname + '.workflow.clear();';
        model.issueCommand(cmd);
    }));
    ContextMenu.set(contextMenu.attr('id'), diagram.attr('id'));

    /** redraw flow fig */
//    function redraw() {
//        var i = 0,
//            x = flow_fig.position().left + 10,
//            y = flow_fig.position().top  + 10;
//        debug.info('WorkflowDiagram.redraw() comp_figs:',comp_figs,'x:',x,'y:',y);
//        if (comp_figs.size > 0) {
//            for (i=0; i<comp_figs.size; i++) {
//                debug.info('WorkflowDiagram.redraw() i:',i,'comp_fig:',comp_figs[i]);
//                comp_figs[i].setPosition(x,y);
//                if (horizontal) {
//                    x = x + comp_figs[i].getWidth() + 10;
//                }
//                else {
//                    y = y + comp_figs[i].getHeight() + 10;
//                }
//            }
//            debug.info('WorkflowDiagram.redraw() i:',i,'comp_fig:',comp_figs[i]);
//            flow_fig.css({'width':  x = x + comp_figs[i].getWidth()  + 10,
//                          'height': y = y + comp_figs[i].getHeight() + 10 });
//        }
//    }

    /** update workflow figure from JSON workflow data */
    function updateWorkflow(json) {
        var comp_fig;

        jQuery.each(json, function(idx, comp) {
            if (comp_figs.hasOwnProperty(comp.pathname)) {
                comp_fig = comp_figs[comp.pathname];
                if (comp.workflow) {
                    if (comp_fig instanceof WorkflowDiagram) {
                        comp_fig.update(workflow);
                    }
                    else {
                        debug.error('WorkflowDiagram.updateWorkflow - comp has workflow');
                    }
                }
                else {
                    comp_fig.setType(comp.type);
                    comp_fig.setValid(comp.valid);
                }
            }
            else {
                comp_fig = new openmdao.WorkflowComponentFigure(flow_fig, model,
                                comp.pathname, comp.type, comp.valid);
            }
        });
        
        debug.info('flowfig width:',flow_fig.css('width'),'flow_fig height:',flow_fig.css('height'));
        debug.info('width',  (driver.getWidth()  + flow_fig.width()));
        debug.info('height', (driver.getHeight() + flow_fig.height()));
        diagram.css({ 'width':  (driver.getWidth()  + flow_fig.width()),
                      'height': (driver.getHeight() + flow_fig.height()) });
    }

    // populate flow fig with component figures
    updateWorkflow(json.workflow);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** provide access to context menu (for use after diagram is in the DOM */
    this.getContextMenu = function() {
        return contextMenu;
    };

    /** get width */
    this.getWidth = function(x, y) {
//        debug.info('WorkflowDiagram.getWidth()',name,diagram,diagram.width());
        return diagram.width();
    };

    /** get height */
    this.getHeight = function(x, y) {
//        debug.info('WorkflowDiagram.getHeight()',name,diagram,diagram.height());
        return diagram.height();
    };

    /** set position relative to parent div */
    this.getPosition = function() {
//        debug.info('WorkflowDiagram.getPosition()',name,diagram,diagram.position().left,diagram.position().top);
        return diagram.position();
    };

    /** set position relative to parent div */
    this.setPosition = function(x, y) {
        debug.info('WorkflowDiagram.setPosition()',name,diagram,x,y);
        diagram.css({ 'position': 'absolute', 'left': x+'px', 'top': y+'px' });
    };

    /** update workflow diagram from JSON data */
    this.update = function(json) {
        driver.setType(json.type);
        driver.setValid(json.valid);
        updateWorkflow(json.workflow);
    };

};
