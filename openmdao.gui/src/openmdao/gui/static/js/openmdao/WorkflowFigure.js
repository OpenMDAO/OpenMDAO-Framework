/**
 *  WorkflowFigure: an object representing an openmdao workflow
 *
 *  A workflowFigure consists of a driver component figure and an offset box
 *  containing figures for each of the components in the driver's workflow
 *
 *  Arguments:
 *      elm:    jQuery element which will contain the WorkflowFigure
 *      model:  object that provides access to the openmdao model
 *      driver: pathname of the driver of the parent workflow, if any
 *      json:   json representation of the workflow, per the openmdao server API
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFigure = function(elm, model, driver, json) {
    var self = this,
        pathname = json.pathname,
        name = openmdao.Util.getName(pathname),
        id = elm.attr('id')+'-'+pathname.replace(/\./g,'-')+'-WorkflowFigure',
        fig = jQuery('<div class="WorkflowFigure" id='+id+' style="float:left;position:relative;left:0px" />')
            .appendTo(elm),
        drvr_fig = new openmdao.WorkflowComponentFigure(fig,model,driver,json.pathname,json.type,json.valid),
        flow_css = 'background-repeat:no-repeat', //;border-style:solid;border-color:yellow;border-width:thin;',
        flow_div = jQuery('<div style="'+flow_css+'" id='+id.replace(/WorkflowFigure/g,'flow')+'/>')
            .appendTo(fig),
        maxmin_css = 'position:absolute;top:0;left:0;width:16px;height:16px;z-index:9000',
        maxmin_div = jQuery('<div style="'+maxmin_css+'"></div>')
            .appendTo(flow_div),
        contextMenu = jQuery("<ul id="+id.replace(/WorkflowFigure/g,'menu')+" class='context-menu'>")
            .appendTo(fig),
        comp_figs = {},
        horizontal = true,
        minimized = false;

    // store the pathname on the element for child component figures to access
    fig.data('pathname', pathname);

    // if name is 'driver', then prepend parent assembly name
    if (name === 'driver') {
        name = openmdao.Util.getName(openmdao.Util.getPath(pathname))+'.driver';
    }

    // position flow fig to overlap bottom right corner of driver fig
    flow_div.css({ 'position': 'absolute',
                   'left': drvr_fig.getWidth()  - 16,
                   'top':  drvr_fig.getHeight() - 16,
                   'background-image': 'url("/static/images/workflow_bg.png")' });

    /** set the size and shape of the flow div background image */
    function setBackground() {
        // get position and dimension of last child
        // if the last child is a WorkflowFigure, then use it's driver
        var children = flow_div.children('.WorkflowComponentFigure, .WorkflowFigure'),
            last_child = children.last();
        if (last_child.hasClass('WorkflowFigure')) {
            last_child = last_child.children('.WorkflowComponentFigure').first();
        }

        debug.info('WorkflowFigure.setBackground()',pathname,
                   'children:',children,'last_child:',last_child.attr('id'));

        // set size of background image to extend to last child
        if (minimized) {
            bg_height = 0;
            bg_width = 0;
        }
        else if (children.length > 0) {
            if (horizontal) {
                bg_width = last_child.offset().left - flow_div.offset().left
                         + last_child.outerWidth();
                bg_height = 70;
                debug.info('last_child left:',last_child.offset().left,
                           'flow_div left:',flow_div.offset().left,
                           'last_child width:',last_child.outerWidth());
                debug.info('WorkflowFigure.setBackground()', pathname,
                           'HORZ','bg_width:',bg_width,'bg_height:',bg_height);
            }
            else {
                bg_width = 110;
                bg_height = last_child.offset().top - flow_div.offset().top
                          + last_child.outerHeight();
                debug.info('last_child top:',last_child.offset().top,
                           'flow_div top:',flow_div.offset().top,
                           'last_child height',last_child.outerHeight());
                debug.info('WorkflowFigure.setBackground()', pathname,
                           'VERT','bg_width:',bg_width,'bg_height:',bg_height);

            }
        }
        else {
            bg_height = 70;
            bg_width = 110;
            debug.info('WorkflowFigure.setBackground()', pathname,'no children',
                       'bg_width:',bg_width,'bg_height:',bg_height);
        }

        flow_div.css({ 'background-size': bg_width + 'px ' + bg_height + 'px' });
    }

    /** arrange component figures and resize flow div to contain them */
    function layout() {
        var comp_height = 0,
            comp_width = 0,
            flow_height = 0,
            flow_width = 0,
            children = flow_div.children('.WorkflowComponentFigure, .WorkflowFigure');

        if (minimized) {
            maxmin_div.removeClass('ui-icon-maximized');
            maxmin_div.addClass('ui-icon-minimized');
            children.hide();
            flow_width = 15;
            flow_height = 15;
            flow_div.css({ 'width': flow_width,
                           'height': flow_height });
            fig.css({ 'width':  flow_width  + drvr_fig.getWidth(),
                      'height': flow_height + drvr_fig.getHeight() });
        }
        else {
            maxmin_div.removeClass('ui-icon-minimized');
            maxmin_div.addClass('ui-icon-maximized');
            children.show();
            debug.info('WorkflowFigure.layout()',pathname,'horz:',horizontal,'children:',children);
            // set the children to horizontal or vertical layout per the flag
            if (horizontal) {
                children.css({ 'clear': 'none' });
            }
            else {
                children.css({ 'clear': 'both' });
            }

            // determine width & height of flow div needed to contain all components
            if (children.length > 0) {
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
            }
            else {
                flow_width  = 100;
                flow_height = 60;
            }

            flow_div.css({ 'width': flow_width,
                           'height': flow_height });
            fig.css({ 'width':  flow_width  + drvr_fig.getWidth(),
                      'height': flow_height + drvr_fig.getHeight() });

            // tell the nearest parent workflow figure to update it's layout to
            // accomodate the new dimensions of this workflow figure
            var parent_workflow = fig.parents('.WorkflowFigure').first();
            if (parent_workflow.length > 0) {
                debug.info('***',pathname,'TRIGGERING LAYOUT on',parent_workflow.attr('id'),'***');
                parent_workflow.trigger('layout');
            }
        }

        setBackground();
    }

    // add click handler to maxmin div
    maxmin_div.click(function() {
        minimized = !minimized;
        layout();
    });

    // set up flow_div as a drop target for components to add to workflow
    flow_div.data('name',name);
    flow_div.data('pathname',pathname);

    flow_div.highlightAsDropTarget = function() {
        flow_div.css({ 'background-image': 'url("/static/images/highlight_bg.png")' });
    };

    flow_div.unhighlightAsDropTarget = function() {
        flow_div.css({ 'background-image': 'url("/static/images/workflow_bg.png")' });
    };

    flow_div.droppable ({
        accept: '.component, .IComponent',
            out: function(ev,ui) {
                openmdao.drag_and_drop_manager.draggableOut(flow_div);
            },
            over: function(ev,ui) {
                // only allow drops of components in same assembly as driver
                var target_pathname = flow_div.data('pathname'),
                    target_parent = openmdao.Util.getPath(target_pathname),
                    dragged_object = jQuery(ui.draggable).clone(),
                    dragged_pathname,
                    dragged_parent;
                if (dragged_object.hasClass('component')) {
                    dragged_pathname = jQuery(ui.draggable ).parent().attr("path");
                    dragged_parent = openmdao.Util.getPath(dragged_pathname);
                    if (dragged_parent === target_parent) {
                        openmdao.drag_and_drop_manager.draggableOver(flow_div);
                    }
                }
                else if (dragged_object.hasClass('IComponent')) {
                    openmdao.drag_and_drop_manager.draggableOver(flow_div);
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
                var target_pathname = flow_div.data('pathname'),
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
                             'the workflow of '+target_pathname+')';
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

    fig.on('layout', function(e) {
        debug.info('*** WorkflowFigure layout() triggered ***',pathname,e);
        e.stopPropagation();
        layout();
    });

    fig.on('setBackground', function(e) {
        debug.info('*** WorkflowFigure setBackground() triggered ***',pathname,e);
        e.stopPropagation();
        setBackground();
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
        var layout_triggered = false,
            deleted_comps = [];
        debug.info('WorkflowFigure.updateWorkflow()',pathname,'json:',json);

        // delete figures for components that are no longer in the workflow
        jQuery.each(comp_figs, function(idx, comp_fig) {
            var comp_pathname = comp_fig.getPathname(),
                comp_found = false;
            jQuery.each(json, function(idx, comp) {
                if (comp.driver && comp.driver.pathname === comp_pathname) {
                    // comp is an assembly (figure is a WorkflowFigure)
                    comp_found = true;
                }
                else if (comp.workflow && comp.pathname === comp_pathname) {
                    // comp is an driver (figure is a WorkflowFigure)
                    comp_found = true;
                }
                else if (comp.pathname === comp_pathname) {
                    // comp is just a component (figure is a WorkflowComponentFigure)
                    comp_found = true;
                }
            });
            if (! comp_found) {
                deleted_comps.push(comp_pathname);
            }
        });
        jQuery.each(deleted_comps, function(idx, comp_pathname) {
            var comp_fig = comp_figs[comp_pathname];
            delete comp_figs[comp_pathname];
            comp_fig.destroy();
        });

        // update figures for components that are in the updated workflow
        jQuery.each(json, function(idx, comp) {
            var comp_key = comp.hasOwnProperty('driver') ? comp.driver.pathname : comp.pathname,
                comp_fig;
            if (comp_figs.hasOwnProperty(comp_key)) {
                comp_fig = comp_figs[comp_key];
                if (comp_fig instanceof openmdao.WorkflowFigure) {
                    if (comp.workflow) {
                        // comp is a driver
                        comp_fig.update(comp);
                        layout_triggered = true;
                    }
                    else if (comp.driver) {
                        // comp is an assembly
                        comp_fig.update(comp.driver);
                        layout_triggered = true;
                    }
                    else {
                        debug.error('WorkflowFigure.updateWorkflow() - ' +
                                    'workflow figure update has no workflow data');
                    }
                }
                else {
                    comp_fig.setType(comp.type);
                    comp_fig.setValid(comp.valid);
                }
            }
            else if (comp.hasOwnProperty('workflow')) {
                // new comp is a driver with it's own workflow
                comp_fig = new openmdao.WorkflowFigure(flow_div, model, pathname, comp);
                comp_figs[comp_key] = comp_fig;
            }
            else if (comp.hasOwnProperty('driver')) {
                // new comp is an assembly with a driver that has it's own workflow
                comp_fig = new openmdao.WorkflowFigure(flow_div, model, pathname, comp.driver);
                comp_figs[comp_key] = comp_fig;
            }
            else {
                comp_fig = new openmdao.WorkflowComponentFigure(flow_div, model,
                                pathname, comp.pathname, comp.type, comp.valid);
                comp_figs[comp.pathname] = comp_fig;
            }
        });

        // if any children are a WorkflowFigure, they will trigger the layout,
        // otherwise we do it here
        // note: will still get extra layouts if there are multiple child workflows
        if (! layout_triggered) {
            debug.info('*** DOING LAYOUT ***',pathname);
            layout();
        }
    }

    // populate flow fig with component figures
    updateWorkflow(json.workflow);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get element */
    this.getElement = function() {
        return fig;
    };

    /** get pathname */
    this.getPathname = function() {
        return pathname;
    };

    /** get width */
    this.getWidth = function(x, y) {
        return fig.width();
    };

    /** get height */
    this.getHeight = function(x, y) {
        return fig.height();
    };

    /** get position relative to parent div */
    this.getPosition = function() {
        return fig.position();
    };

    /** update workflow fig from JSON data */
    this.update = function(json) {
        debug.info('WorkflowFigure.update()',pathname,json);
        if (json.workflow) {
            drvr_fig.setType(json.type);
            drvr_fig.setValid(json.valid);
            updateWorkflow(json.workflow);
        }
        else {
            debug.info('WorkflowFigure.update() invalid data:',pathname,json);
        }
    };

    /** clean up listener */
    this.destroy = function() {
        jQuery.each(comp_figs, function(path, comp_fig) {
            comp_fig.destroy();
        });
        fig.remove();
    };

};
