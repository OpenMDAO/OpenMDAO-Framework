
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;


var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowComponentFigure=function(model, workflow, pathname, type, valid) {
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
        elm = jQuery('<div class="WorkflowComponentFigure" style="width:100px;height:60px" />')
            .append(svg),
        rectCSS = {'stroke-width':2, 'stroke':'#0b93d5', 'fill': 'white'},
        textCSS = {'fill': 'black'},
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>")
            .appendTo(elm);

    workflow.append(elm);

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
    elm.find('#name').text(name);
    elm.find('#klass').text(type);
    elm.attr('id',id);

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
    elm.getContextMenu = function() {
        return contextMenu;
    };

    /** open object editor on double click */
    elm.dblclick(function(e) {
        frame = new openmdao.ObjectFrame(model, pathname);
    });

    // set rectangle color based on state
    function setState(state) {
        elm.find('rect').css(rectCSS);  // defaults
        if (state === "VALID") {
            elm.find('rect').css({ 'stroke': '#00FF00' });  // green
        }
        else if (state === "INVALID") {
            elm.find('rect').css({ 'stroke': '#FF0000' });  // red
        }
        else if (state === "RUNNING") {
            elm.find('rect').css({ 'stroke': '#0000FF' });  // blue
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
        return elm;
    };

    /** get width */
    this.getWidth = function(x, y) {
//        debug.info('WorkflowComponentFigure.getWidth()',name,elm,elm.width());
        return elm.width();
    };

    /** get height */
    this.getHeight = function(x, y) {
//        debug.info('WorkflowComponentFigure.getHeight()',name,elm,elm.height());
        return elm.height();
    };

    /** set position relative to parent div */
    this.getPosition = function() {
//        debug.info('WorkflowComponentFigure.getPosition()',name,elm,elm.position().left,elm.position().top);
        return elm.position();
    };

    /** set position relative to parent div */
    this.setPosition = function(x, y) {
        debug.info('WorkflowComponentFigure.setPosition()',name,elm,x,y);
        elm.css({ 'position': 'absolute', 'left': x+'px', 'top': y+'px' });
    };

};

