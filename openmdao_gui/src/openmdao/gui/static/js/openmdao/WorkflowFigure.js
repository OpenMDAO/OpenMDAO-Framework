/**
 *  WorkflowFigure: an object representing an openmdao workflow
 *
 *  A workflowFigure consists of a driver component figure and an offset box
 *  containing figures for each of the components in the driver's workflow
 *
 *  Arguments:
 *      elm:      jQuery element which will contain the WorkflowFigure
 *      project:  object that provides access to the openmdao project
 *      driver:   pathname of the driver of the parent workflow, if any
 *      json:     json representation of the workflow, per the openmdao server API
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFigure = function(elm, project, driver, json) {
    var self = this,
        pathname = json.pathname,
        name = openmdao.Util.getName(pathname),
        id = elm.attr('id')+'-'+pathname.replace(/\./g,'-')+'-WorkflowFigure',
        fig = jQuery('<div class="WorkflowFigure" id='+id+' style="float:left;position:relative;left:0px" />')
            .appendTo(elm),
        drvr_fig = new openmdao.WorkflowComponentFigure(fig, project, driver, json),
        flow_css = 'background-repeat:no-repeat', //;border-style:solid;border-color:yellow;border-width:thin;',
        flow_div = jQuery('<div style="'+flow_css+'" id='+id.replace(/WorkflowFigure/g,'flow')+'/>')
            .appendTo(fig),
        maxmin_css = 'position:absolute;top:0;left:0;width:16px;height:16px;z-index:9000',
        maxmin_div = jQuery('<div style="'+maxmin_css+'"></div>')
            .appendTo(flow_div),
        contextMenu = jQuery("<ul id="+id.replace(/WorkflowFigure/g,'menu')+" class='context-menu'>")
            .appendTo(fig),
        comp_figs = [],
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
            }
            else {
                bg_width = 110;
                bg_height = last_child.offset().top - flow_div.offset().top
                          + last_child.outerHeight();
            }
        }
        else {
            bg_height = 70;
            bg_width = 110;
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

            // set the children to horizontal or vertical layout per the flag
            if (horizontal) {
                children.css({ 'clear': 'none' });
            }
            else {
                children.css({ 'clear': 'both' });
            }

            // determine width & height of flow div needed to contain all components
            if (children.length > 0) {
                jQuery.each(comp_figs, function(idx, comp_fig) {
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
        }

        // give browser a few ms to reflow everything then resize background
        setTimeout(function() {
            setBackground();
        }, 200);
    }

    /** layout all child workflow figures, then this one, then the parents */
    function layoutAll() {
        flow_div.children('.WorkflowFigure').trigger('layoutAll');
        layout();
        var parent_workflow = fig.parents('.WorkflowFigure').first();
        while (parent_workflow.length > 0) {
            if (parent_workflow.length > 0) {
                parent_workflow.trigger('layout');
            }
            parent_workflow = parent_workflow.parents('.WorkflowFigure').first();
        }
    }

    // add click handler to maxmin div
    maxmin_div.click(function() {
        minimized = !minimized;
        layoutAll();
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
        accept: '.component, .IComponent, .DataflowFigure',
        tolerance: 'pointer',
        greedy: true,
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

            if (dragged_object.hasClass('DataflowFigure')) {
                dragged_pathname = jQuery(ui.draggable).attr('pathname');
                dragged_parent = openmdao.Util.getPath(dragged_pathname);
                if (dragged_parent === target_parent) {
                    openmdao.drag_and_drop_manager.draggableOver(flow_div);
                }
            }
            else if (dragged_object.hasClass('component')) {
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

            openmdao.drag_and_drop_manager.reset();

            var target_pathname = flow_div.data('pathname'),
                target_parent = openmdao.Util.getPath(target_pathname),
                dropped_object = jQuery(ui.draggable).clone(),
                dropped_pathname,
                dropped_parent,
                dropped_name,
                prompt;

            if (dropped_object.hasClass('DataflowFigure')) {
                dropped_pathname = jQuery(ui.draggable).attr('pathname'),
                dropped_parent = openmdao.Util.getPath(dropped_pathname);
                if (dropped_parent === target_parent) {
                    dropped_name = openmdao.Util.getName(dropped_pathname);
                    cmd = target_pathname + '.workflow.add("' + dropped_name + '")';
                    project.issueCommand(cmd);
                }
            }
            else if (dropped_object.hasClass('component')) {
                // dropped from component tree, component must be in same assembly as the driver
                dropped_pathname = jQuery(ui.draggable).parent().attr("path");
                dropped_parent = openmdao.Util.getPath(dropped_pathname);
                if (dropped_parent === target_parent) {
                    dropped_name = openmdao.Util.getName(dropped_pathname);
                    cmd = target_pathname + '.workflow.add("' + dropped_name + '")';
                    project.issueCommand(cmd);
                }
            }
            else if (dropped_object.hasClass('IComponent')) {
                // dropped from library, create new component in same assembly as the driver
                dropped_pathname = dropped_object.attr("modpath");
                dropped_name = dropped_object.text();
                prompt = 'Specify a name for the new '+dropped_name+'<br>'+
                         '(It will be added to '+target_parent +' and to <br>'+
                         'the workflow of '+ target_pathname+')';
                openmdao.project.addObject(dropped_pathname, dropped_name,
                                         target_parent, prompt, function(name) {
                    // If successful, then add to workflow as well.
                    cmd = target_pathname+'.workflow.add("'+name+'")';
                    project.issueCommand(cmd);
                });
            }
        }
    });

    // make the layout() function triggerable
    fig.on('layout', function(e) {
        e.stopPropagation();
        layout();
    });

    // make the layoutAll() function triggerable
    fig.on('layoutAll', function(e) {
        e.stopPropagation();
        layoutAll();
    });

    // make the setBackground() function triggerable
    fig.on('setBackground', function(e) {
        e.stopPropagation();
        setBackground();
    });

    // create context menu
    contextMenu.append(jQuery('<li><b>'+name+'</b></li>'));
    contextMenu.append(jQuery('<li>Flip Workflow</li>').click(function(e) {
        horizontal = !horizontal;
        layoutAll();
    }));
    contextMenu.append(jQuery('<li>Clear Workflow</li>').click(function(e) {
        var cmd = pathname + '.workflow.clear() ; ' +
                  pathname + '.config_changed()';
        project.issueCommand(cmd);
    }));
    ContextMenu.set(contextMenu.attr('id'), flow_div.attr('id'));

    /** update workflow from JSON workflow data */
    function updateWorkflow(json) {

        // Traverse server workflow checking for a match in figure list.
        jQuery.each(json, function(idx, comp) {
            var match = false,
                remove = 0,
                comp_fig,
                comp_pathname;

            if (comp_figs.length > idx) {
                comp_fig = comp_figs[idx];
                comp_pathname = comp_fig.getPathname();
                if (comp.driver && comp.driver.pathname === comp_pathname) {
                    // comp is an assembly (figure is a WorkflowFigure)
                    comp_fig.update(comp.driver);
                    match = true;
                }
                else if (comp.workflow && comp.pathname === comp_pathname) {
                    // comp is an driver (figure is a WorkflowFigure)
                    comp_fig.update(comp);
                    match = true;
                }
                else if (comp.pathname === comp_pathname) {
                    // comp is just a component (figure is a WorkflowComponentFigure)
                    comp_fig.setType(comp.type);
                    comp_fig.setValid(comp.valid);
                    match = true;
                }
                else {
                    remove = 1;
                }
            }

            // Append/insert new figure into list.
            if (!match) {
                if (comp.hasOwnProperty('workflow')) {
                    // new comp is a driver with it's own workflow
                    comp_fig = new openmdao.WorkflowFigure(flow_div, project,
                                                           pathname, comp);
                }
                else if (comp.hasOwnProperty('driver')) {
                    // new comp is an assembly with a driver that has it's own workflow
                    comp_fig = new openmdao.WorkflowFigure(flow_div, project,
                                                           pathname, comp.driver);
                }
                else {
                    comp_fig = new openmdao.WorkflowComponentFigure(flow_div,
                                               project, pathname, comp);
                }
                var removed = comp_figs.splice(idx, remove, comp_fig);
                if (removed.length) {
                    removed[0].destroy();
                }
            }
        });

        // Delete any extra figures at end of list.
        for (var i = comp_figs.length-1 ; i >= json.length ; --i) {
            comp_figs.splice(i, 1)[0].destroy();
        }
    }

    // populate flow fig with component figures
    updateWorkflow(json.workflow);

    // perform layout from the bottom up
    layoutAll();

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
    this.getWidth = function() {
        return fig.width();
    };

    /** get height */
    this.getHeight = function() {
        return fig.height();
    };

    /** get position relative to parent div */
    this.getPosition = function() {
        return fig.position();
    };

    /** update workflow fig from JSON data */
    this.update = function(json) {
        if (json.workflow) {
            drvr_fig.setType(json.type);
            drvr_fig.setValid(json.valid);
            updateWorkflow(json.workflow);
            layoutAll();
        }
        else {
            debug.error('WorkflowFigure.update() invalid workflow data for',
                        pathname,':',json);
        }
    };

    /** clean up listener */
    this.destroy = function() {
        jQuery.each(comp_figs, function(idx, comp_fig) {
            comp_fig.destroy();
        });
        fig.remove();
    };

};
