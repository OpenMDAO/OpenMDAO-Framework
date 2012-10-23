
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFigure = function(elm, model, json) {
    var self = this,
        pathname = json.pathname,
        name = openmdao.Util.getName(pathname),
        id = elm.attr('id')+'-'+pathname.replace(/\./g,'-')+'-WorkflowFigure',
        fig = jQuery('<div class="WorkflowFigure" id='+id+' style="float:left;position:relative;left:0px" />')
            .appendTo(elm),
        driver = new openmdao.WorkflowComponentFigure(fig,model,json.pathname,json.type,json.valid),
        flow_css = 'border-style:solid;border-color:black;border-width:thin;background-color:white;',
        flow_div = jQuery('<div style="'+flow_css+'" id='+id.replace(/WorkflowFigure/g,'flow')+' />')
            .appendTo(fig),
        contextMenu = jQuery("<ul id="+id.replace(/WorkflowFigure/g,'menu')+" class='context-menu'>")
            .appendTo(fig),
        comp_figs = {},
        defaultBackgroundColor = '#FFFFFF',
        highlightBackgroundColor = '#FAFAC8',
        dropHighlightBackgroundColor = '#CFD6FE',
        horizontal = true;

    // if name is 'driver', then prepend parent assembly name
    if (name === 'driver') {
        name = openmdao.Util.getName(openmdao.Util.getPath(pathname))+'.driver';
    }

    // position flow fig to overlap bottom right corner of driver fig
    flow_div.css({ 'position': 'absolute',
                   'left': driver.getWidth()  - 15,
                   'top':  driver.getHeight() - 15 });

    /** arrange component figures and resize flow div to contain them*/
    function layout() {
        debug.info('WorkflowFigure.layout()',self,pathname,'horizontal =',horizontal);
        var i = 0,
            flow_height = 0,
            comp_height = 0,
            flow_width = 0,
            comp_width = 0,
            children;

        jQuery.each(comp_figs, function(path, comp_fig) {
            if (comp_figs.hasOwnProperty(path)) {
                comp_width = comp_fig.getWidth();
                comp_height = comp_fig.getHeight();
                if (horizontal) {
                    flow_width = flow_width + comp_width;
                    flow_height = comp_height > flow_height ? comp_height : flow_height;
                }
                else {
                    flow_height = flow_height + comp_height;
                    flow_width = comp_width > flow_width ? comp_width : flow_width;
                }
            }
        });

        children = flow_div.children('.WorkflowComponentFigure, .WorkflowFigure');
        debug.info('WorkflowFigure.layout()',self,pathname,'children =',children);
        if (children.length > 0) {
            flow_div.css({ 'width': flow_width, 'height': flow_height });
            fig.css({ 'width':  flow_width  + driver.getWidth(),
                      'height': flow_height + driver.getHeight() });
            if (horizontal) {
                children.css({ 'clear': 'none' });
            }
            else {
                children.css({ 'clear': 'both' });
            }
        }
        else {
            flow_width = 100;
            flow_height = 60;
            flow_div.css({ 'width': flow_width, 'height': flow_height });
            fig.css({ 'width':  flow_width  + driver.getWidth(),
                      'height': flow_height + driver.getHeight() });

        }

        // tell the nearest parent workflow figure to update it's layout to
        // accomodate the new dimensions of this workflow figure
        fig.parents('.WorkflowFigure').first().trigger('layout');
    }

    // set up flow_div as a drop target for components to add to workflow
    flow_div.data('name',name);
    flow_div.data('pathname',pathname);

    flow_div.highlightAsDropTarget = function() {
        fig.find('rect').css({ 'fill': dropHighlightBackgroundColor });
    };

    flow_div.unhighlightAsDropTarget = function() {
        fig.find('rect').css({ 'fill': defaultBackgroundColor });
    };

    flow_div.droppable ({
        accept: '.component, .IComponent',
            out: function(ev,ui) {
                openmdao.drag_and_drop_manager.draggableOut(fig);
            },
            over: function(ev,ui) {
                // only allow drops of components in same assembly as driver
                var target_pathname = fig.data('pathname'),
                    target_parent = openmdao.Util.getPath(target_pathname),
                    dragged_object = jQuery(ui.draggable).clone(),
                    dragged_pathname,
                    dragged_parent;
                if (dragged_object.hasClass('component')) {
                    dragged_pathname = jQuery(ui.draggable ).parent().attr("path");
                    dragged_parent = openmdao.Util.getPath(dragged_pathname);
                    if (dragged_parent === target_parent) {
                        openmdao.drag_and_drop_manager.draggableOver(fig);
                    }
                }
                else if (dragged_object.hasClass('IComponent')) {
                    openmdao.drag_and_drop_manager.draggableOver(fig);
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
                var target_pathname = fig.data('pathname'),
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

    fig.bind('layout', function() {
        layout();
    });

    // create context menu
    contextMenu.append(jQuery('<li><b>'+name+'</b></li>'));
    contextMenu.append(jQuery('<li>Flip Workflow</li>').click(function(e) {
        horizontal = !horizontal;
        layout();
    }));
    contextMenu.append(jQuery('<li>Clear Workflow</li>').click(function(e) {
        var cmd = pathname + '.workflow.clear();' +
                  pathname + '.config_changed();';
        model.issueCommand(cmd);
    }));
    ContextMenu.set(contextMenu.attr('id'), flow_div.attr('id'));

    /** update workflow from JSON workflow data */
    function updateWorkflow(json) {
        var layout_triggered = false;
        debug.info('WorkflowFigure.updateWorkflow()',self,pathname,'horizontal=',horizontal,'json =',json);
        flow_div.html('');

        // delete figures for components that are no longer in workflow
        jQuery.each(comp_figs, function(idx, comp) {
            var comp_found = false;
            jQuery.each(json, function(idx, json_comp) {
                if (json_comp.pathname === comp.pathname) {
                    comp_found = true;
                }
            });
            if (! comp_found) {
                delete comp_figs[comp.pathname];
            }
        });

        // update figures for components that are in updated workflow
        jQuery.each(json, function(idx, comp) {
            var comp_fig;
            if (comp_figs.hasOwnProperty(comp.pathname)) {
                comp_fig = comp_figs[comp.pathname];
                if (comp.workflow) {
                    if (comp_fig instanceof WorkflowFigure) {
                        comp_fig.update(workflow);
                        layout_triggered = true;
                    }
                    else {
                        debug.error('WorkflowFigure.updateWorkflow() - ' +
                                    'component figure has workflow');
                    }
                }
                else {
                    comp_fig.setType(comp.type);
                    comp_fig.setValid(comp.valid);
                }
            }
            else if (comp.hasOwnProperty('workflow')) {
                // comp is a driver with it's own workflow
                comp_fig = new openmdao.WorkflowFigure(flow_div, model, comp);
                comp_figs[comp.pathname] = comp_fig;
            }
            else if (comp.hasOwnProperty('driver')) {
                // comp is an assembly with a driver that has it's own workflow
                comp_fig = new openmdao.WorkflowFigure(flow_div, model, comp.driver);
                comp_figs[comp.pathname] = comp_fig;
            }
            else {
                comp_fig = new openmdao.WorkflowComponentFigure(flow_div, model,
                                comp.pathname, comp.type, comp.valid);
                comp_figs[comp.pathname] = comp_fig;
            }
        });

        if (! layout_triggered) {
            // will still get extra layouts if there are multiple child workflows
            layout();
        }
    }

    // populate flow fig with component figures
    updateWorkflow(json.workflow);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get width */
    this.getWidth = function(x, y) {
        return fig.width();
    };

    /** get height */
    this.getHeight = function(x, y) {
        return fig.height();
    };

    /** update workflow fig from JSON data */
    this.update = function(json) {
        driver.setType(json.type);
        driver.setValid(json.valid);
        updateWorkflow(json.workflow);
    };

};
