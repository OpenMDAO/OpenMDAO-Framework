/**
 *  WorkflowComponentFigure: an object representing a component in an openmdao workflow
 *
 *  A WorkflowComponentFigure consists of a rectangular box containing the
 *  component name and type.  The rectangle is rendered with a red outline if
 *  if the component's state is valid or green if it is invalid.
 *
 *  Arguments:
 *      elm:      jQuery element which will contain the WorkflowComponentFigure
 *      project:  object that provides access to the openmdao project
 *      driver:   pathname of the driver of the parent workflow, if any
 *      comp:     object representing the component, has the following attributes
 *          pathname:   the pathname of the component
 *          type:       the type of the component
 *          interfaces: the interfaces implemented by the component
 *          valid:      the initial valid state of the component
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowComponentFigure=function(elm, project, driver, comp) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'WorkflowComponentFigure-'+comp.pathname.replace(/\./g,'-'),
        name = openmdao.Util.getName(comp.pathname),
        parent = openmdao.Util.getPath(comp.pathname),
        parentName = openmdao.Util.getName(parent),
        svg = jQuery('<svg height="60" width="100">'
                   + '    <rect x="0" y="5" height="50" width="100" rx="15" ry="15";" />'
                   + '    <text id="name" x="50" y="25" text-anchor="middle">Name</text>'
                   + '    <text id="klass" x="50" y="45" font-style="italic" text-anchor="middle">Klass</text>'
                   + '</svg>'),
        fig = jQuery('<div class="WorkflowComponentFigure" style="width:100px;height:60px;float:left;padding:5px" />')
            .append(svg),
        rectCSS = {'stroke-width':2, 'stroke':'#0b93d5', 'fill':'#999999'},
        contextMenu = jQuery("<ul class='context-menu'>")
            .appendTo(fig);

    elm.append(fig);

    // if my name is just 'driver', qualify with parent (assembly) name
    if (name === 'driver') {
        name = parentName + '.driver';
    }

    // remove path from type name
    var tok = comp.type.split('.');
    if (tok.length > 1) {
        comp.type = tok[tok.length-1];
    }

    // set the content text to be the name and type
    svg.find('#name').text(name);
    svg.find('#klass').text(comp.type);

    // set ids
    fig.attr('id',id);
    svg.uniqueId();
    contextMenu.uniqueId();

    // create context menu
    contextMenu.append(jQuery('<li><b>'+name+'</b></li>'));
    contextMenu.append(jQuery('<li>Edit</li>').click(function(e) {
        var frame = new openmdao.ObjectFrame(project, comp.pathname);
    }));
    contextMenu.append(jQuery('<li>Properties</li>').click(function(e) {
        var id = (comp.pathname+'-properties').replace(/\./g,'-'),
            frame = new openmdao.PropertiesFrame(id,project).editObject(comp.pathname);
    }));
    if (jQuery.inArray('IImplicitComponent', comp.interfaces) >= 0) {
        contextMenu.append(jQuery('<li>Evaluate</li>').click(function(e) {
            var cmd = comp.pathname + '.evaluate()';
            project.issueCommand(cmd);
        }));
    }
    contextMenu.append(jQuery('<li>Run</li>').click(function(e) {
        project.runComponent(comp.pathname);
    }));
    if (driver.length > 0) {
        contextMenu.append(jQuery('<li>Remove from Workflow</li>').click(function(e) {
            var cmd = driver+".workflow.remove('";
            if (/.driver$/.test(name)) {
                cmd = cmd + name.replace(/.driver/g,'') + "')";
            }
            else {
                cmd = cmd + name + "')";
            }
            project.issueCommand(cmd);
        }));
    }
    ContextMenu.set(contextMenu.attr('id'), svg.attr('id'));

    /** open object editor on double click */
    fig.dblclick(function(e) {
        frame = new openmdao.ObjectFrame(project, comp.pathname);
    });

    // set rectangle color based on state
    function setState(state) {
        fig.find('rect').css(rectCSS);  // defaults
        if (state === "VALID") {
            fig.find('rect').css({ 'stroke': '#00FF00' });  // green
        }
        else if (state === "INVALID") {
            fig.find('rect').css({ 'stroke': '#FF0000' });  // red
        }
        else if (state === "RUNNING") {
            fig.find('rect').css({ 'stroke': '#0000FF' });  // blue
        }
    }

    // set initial state & add listener for updates
    if (comp.valid) {
        setState('VALID');
    }
    else {
        setState('INVALID');
    }

    // handle exec_state messages
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== comp.pathname+'.exec_state') {
            debug.warn('Invalid exec_state data for:', comp.pathname, message);
        }
        else {
            setState(message[1]);
        }
    }

    project.addListener(comp.pathname+'.exec_state', handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get element */
    this.getElement = function() {
        return fig;
    };

    /** get pathname */
    this.getPathname = function() {
        return comp.pathname;
    };

    /** get width */
    this.getWidth = function(x, y) {
        return fig.outerWidth();
    };

    /** get height */
    this.getHeight = function(x, y) {
        return fig.outerHeight();
    };

    /** set type */
    this.setType = function(new_type) {
        comp.type = new_type;
    };

    /** set valid flag */
    this.setValid = function(new_valid) {
        comp.valid = new_valid;
        if (comp.valid) {
            setState('VALID');
        }
        else {
            setState('INVALID');
        }
    };

    /** clean up listener */
    this.destroy = function() {
        project.removeListener(comp.pathname+'.exec_state', handleMessage);
        fig.remove();
    };
};

