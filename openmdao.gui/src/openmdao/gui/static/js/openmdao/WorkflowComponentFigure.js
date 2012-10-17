
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;


var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowComponentFigure=function(elm, model, pathname, type, valid) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        id = 'WorkflowComponentFigure-'+pathname.replace(/\./g,'-'),
        name = openmdao.Util.getName(pathname),
        parent = openmdao.Util.getPath(pathname),
        parentName = openmdao.Util.getName(parent),
        svg = '<svg height="60" width="100">'
            + '    <rect x="0" y="5" height="50" width="100" rx="15" ry="15";" />'
            + '    <text id="name" x="50" y="25" text-anchor="middle">Name</text>'
            + '    <text id="klass" x="50" y="45" font-style="italic" text-anchor="middle">Klass</text>'
            + '</svg>',
        fig = jQuery('<div class="WorkflowComponentFigure" style="width:100px;height:60px;float:left;padding:5px" />')
            .append(svg),
        rectCSS = {'stroke-width':2, 'stroke':'#0b93d5', 'fill': 'white'},
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(fig);

    elm.append(fig);

    // get name for this figure and set title appropriately
    if (name === 'driver') {
        name = parentName + '.driver';
    }

    // set the content text to be the type name (in italics)
    var tok = type.split('.');
    if (tok.length > 1) {
        type = tok[tok.length-1];
    }

    // set name, id, tooltip and width
    fig.find('#name').text(name);
    fig.find('#klass').text(type);
    fig.attr('id',id);

    // create context menu
    contextMenu.append(jQuery('<li><b>'+name+'</b></li>'));
    contextMenu.append(jQuery('<li>Edit</li>').click(function(e) {
        var frame = new openmdao.ObjectFrame(model,pathname);
    }));
    contextMenu.append(jQuery('<li>Properties</li>').click(function(e) {
        var id = (pathname+'-properties').replace(/\./g,'-'),
            frame = new openmdao.PropertiesFrame(id,model).editObject(pathname);
    }));
    contextMenu.append(jQuery('<li>Run</li>').click(function(e) {
        var cmd = pathname + '.run();';
        model.issueCommand(cmd);
    }));
    contextMenu.append(jQuery('<li>Remove from Workflow</li>').click(function(e) {
        if (parent) {
            var cmd = parent.pathname+".workflow.remove('";
            if (/.driver$/.test(name)) {
                cmd = cmd + name.replace(/.driver/g,'') + "')";
            }
            else {
                cmd = cmd + name + "')";
            }
            model.issueCommand(cmd);
        }
    }));

    /** provide access to fig's context menu (for use after fig is in the DOM */
    fig.getContextMenu = function() {
        return contextMenu;
    };

    /** open object editor on double click */
    fig.dblclick(function(e) {
        frame = new openmdao.ObjectFrame(model, pathname);
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
    if (valid) {
        setState('VALID');
    }
    else {
        setState('INVALID');
    }

    model.addListener(pathname+'.exec_state', function(message) {
        setState(message[1]);
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get element */
    this.getElement = function() {
        return fig;
    };

    /** get width */
    this.getWidth = function(x, y) {
//        debug.info('WorkflowComponentFigure.getWidth()',name,fig,fig.width());
        return fig.outerWidth();
    };

    /** get height */
    this.getHeight = function(x, y) {
//        debug.info('WorkflowComponentFigure.getHeight()',name,fig,fig.height());
        return fig.outerHeight();
    };

    /** set position relative to parent div */
    this.getPosition = function() {
//        debug.info('WorkflowComponentFigure.getPosition()',name,fig,fig.position().left,fig.position().top);
        return fig.position();
    };

    /** set position relative to parent div */
    this.setPosition = function(x, y) {
        debug.info('WorkflowComponentFigure.setPosition()',name,fig,x,y);
        fig.css({ 'position': 'absolute', 'left': x+'px', 'top': y+'px' });
    };

    /** set type */
    this.setType = function(new_type) {
        type = new_type;
        valid = new_valid;
    };

    /** set valid flag */
    this.setValid = function(new_valid) {
        valid = new_valid;
        if (valid) {
            setState('VALID');
        }
        else {
            setState('INVALID');
        }
    };

};

