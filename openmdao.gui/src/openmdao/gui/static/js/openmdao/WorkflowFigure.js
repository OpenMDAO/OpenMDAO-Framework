
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFigure=function(model,workflow,flowpath,pathname,driver){
    var self = this,
        id = 'WorkflowFigure-'+pathname.replace(/\./g,'-'),
        name = flowpath,
        parent = openmdao.Util.getPath(pathname),
        parentName = openmdao.Util.getName(parent),
        children = [],
        svg = '<svg height="60" width="100">'
            + '    <rect x="0" y="5" height="50" width="100" rx="15" ry="15";" />'
            + '    <text id="name" x="50" y="25" text-anchor="middle">Name</text>'
            + '    <text id="klass" x="50" y="45" font-style="italic" text-anchor="middle">Klass</text>'
            + '</svg>',
        fig = jQuery('<div class="WorkflowFigure"  style="width:100px;height:60px" />')
            .append(svg),
        defaultBackgroundColor = '#FFFFFF',
        highlightBackgroundColor = '#FAFAC8',
        dropHighlightBackgroundColor = '#CFD6FE',
        filledRectCSS = {'stroke-width':2, 'stroke':'#0b93d5', 'fill': 'white'},
        filledTextCSS = {'fill': defaultBackgroundColor},
        horizontal = true,
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    workflow.append(fig);

    // if my name is 'driver', then use my parent's (assembly) name
    var tok = flowpath.split('.');
    if (tok.length > 1) {
        name = tok[tok.length-1];
        if (tok.length > 2 && name === 'driver') {
            name = tok[tok.length-2];
        }
    }

    /** redraw workflow (container) figure */
    function redraw() {
        for (i=0;i<children.size;i++) {
            child = children[i];
            if (child instanceof openmdao.WorkflowFigure) {
                x = child.driver.getPosition().left + child.driver.getWidth()-20;
                y = child.driver.getPosition().top + child.driver.getHeight()-10;
            }
            else {
                if (this.horizontal) {
                    x = this.getX()+child.getWidth()*i*1.5;
                    y = 20+this.getY();
                }
                else {
                    x = this.position().left;
                    y = 20+this.getY()+child.getHeight()*i*1.5;
                }
            }
            child.setPosition(x,y);
        }
        self.resize();
    }

    // set up for dropping components to add to workflow
    fig.data('name',name);
    fig.data('pathname',pathname);

    fig.highlightAsDropTarget = function() {
        fig.find('rect').css({ 'fill': dropHighlightBackgroundColor });
    };

    fig.unhighlightAsDropTarget = function() {
        fig.find('rect').css({ 'fill': defaultBackgroundColor });
    };

    fig.droppable ({
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

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** provide access to context menu (for use after fig is in the DOM */
    this.getContextMenu = function() {
        return contextMenu;
    };

    /** get width */
    this.getWidth = function(x, y) {
//        debug.info('WorkflowFigure.getWidth()',name,fig,fig.width());
        return fig.width();
    };

    /** get height */
    this.getHeight = function(x, y) {
//        debug.info('WorkflowFigure.getHeight()',name,fig,fig.height());
        return fig.height();
    };

    /** set position relative to parent div */
    this.getPosition = function() {
//        debug.info('WorkflowFigure.getPosition()',name,fig,fig.position().left,fig.position().top);
        return fig.position();
    };

    /** set position relative to parent div */
    this.setPosition = function(x, y) {
        debug.info('WorkflowFigure.setPosition()',name,fig,x,y);
        fig.css({ 'position': 'relative', 'left': x+'px', 'top': y+'px' });
    };

    /** add a component figure to this workflow (container) figure */
    this.addComponentFigure = function(comp_fig){
        var count = children.length;
        debug.info('WorkflowFigure.addComponentFigure()',name,comp_fig,count);
        if (horizontal) {
            //x = this.getAbsoluteX()+getFlowWidth(this);
            x = this.getPosition().left + comp_fig.getWidth()*count*1.5;
            y = 20;
        }
        else {
            x = 0;
            //y = 20+this.getAbsoluteY()+getFlowHeight(this);
            y = 20 + this.getPosition().top + comp_fig.getHeight()*count*1.5;
        }
        children.push(comp_fig);
        debug.info('WorkflowFigure adding comp_fig:',comp_fig,'to',fig);
        fig.append(comp_fig.getElement());
        debug.info('WorkflowFigure.addComponentFigure()',fig,comp_fig,x,y);
        comp_fig.setPosition(x,y);
    };

    /** resize workflow (container) figure to contain all it's children */
    this.resize=function(){
        var i=0,
            xmin=999999, xmax=0,
            ymin=999999, ymax=0,
            width = 100, height = 50;
        for (i=0;i<children.size;i++) {
            child = children.get(i);
            if (child instanceof openmdao.WorkflowComponentFigure) {
                x = child.position().left;
                if (x < xmin) {
                    xmin = x;
                }
                if (x > xmax) {
                    xmax = x;
                    width = child.getWidth();
                }
                y = child.position().top;
                if (y < ymin) {
                    ymin = y;
                }
                if (y > ymax) {
                    ymax = y;
                    height = child.getHeight();
                }
            }
        }
        width = xmax+width-xmin;
        height = ymax+height-ymin+20;
        fig.css({ 'width':width, 'height':height });
    };

};
