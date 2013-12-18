/***********************************************************************
 *  PassthroughsFrame: A frame that shows all the inputs and outputs of
 *                     of an assembly and enables creating and removing
 *                     of passthroughs.
 *
 *  There is an Inputs tree and an Outputs tree, with a checkbox next
 *  to each variable. Toggling the checkbox will create of remove that
 *  variable as a passthrough variable.
 *
 *  Arguments:
 *      project:    object that provides access to the openmdao project
 *      pathname: the pathname of the assembly
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(project, pathname) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Passthroughs: ' + pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/

    var _self = this,
        _table_id_input  = id+'-inputs',
        _table_id_output = id+'-outputs',
        _passthroughHTML = '<div id='+id+'-passthroughdiv style="overflow:auto;height:100%;background:gray">'
                         + '  <table><tr>'
                         + '    <td valign="top">INPUTS:<div id='+_table_id_input+'></div></td>'
                         + '    <td valign="top">OUTPUTS:<div id='+_table_id_output+'></div></td>'
                         + '  </tr></table>'
                         + '</div>',
        _passthroughDiv = jQuery(_passthroughHTML)
            .appendTo(_self.elm),
        _div_input = _passthroughDiv.find("#"+_table_id_input),
        _div_output = _passthroughDiv.find("#"+_table_id_output);

    /** create passthrough */
    function createPassthrough(path) {
        var parts = path.split("."),
            assembly_idx = parts.indexOf(pathname.split(".").slice(-1)[0]),
            comp_path = parts.slice(assembly_idx + 1).join("."),
            cmd = "_ = " + pathname +".create_passthrough('"+comp_path+"')";

        project.issueCommand(cmd);
    }

    /** remove passthrough */
    function removePassthrough(path) {
        var parts = path.split("."),
            assembly = parts[0],
            vname = parts[parts.length - 1],
            cmd = "_ = " + pathname +".remove('"+vname+"')";

        project.issueCommand(cmd);
    }

    /** create or remove passthrough when checkbox is clicked */
    handleCbClick = function(e, d) {
        var tagName = d.args[0].tagName,
            refreshing = d.inst.data.core.refreshing;

        if ((tagName == "A" || tagName == "INS") &&
            (refreshing !== true && refreshing !== "undefined")) {

            var cobj = jQuery(d.rslt[0]),
                this_path = cobj.attr("path"),
                status = cobj.attr("class").split(' ').pop();

            if (status == "jstree-checked") {
                createPassthrough(this_path);
            }
            else {
                removePassthrough(this_path);
            }
        }
    };

    /** create jstree on the target element with the given json data */
    function makeTree(name, target, jsonData) {
        var tree_div = jQuery('<div>')
            .appendTo(target);

        tree_div.jstree({
            "core" : {
                "animation" : false
            },
            "plugins":   [ "json_data", "sort", "themes", "types", "ui", "crrm", "checkbox" ],
            "json_data": { "data" : jsonData },
            "themes":    { "theme": "openmdao", "icons": false },
            "checkbox":  { real_checkboxes: true },
            "types" : {
                "types": {
                    "disabled" : {
                        "check_node" : false,
                        "uncheck_node" : false
                    },
                    "enabled" : {
                        "check_node" : true,
                        "uncheck_node" : true
                    }
                }
             }
        });

        tree_div.bind("change_state.jstree", handleCbClick);
    }

    /** (re)create the input and output passthrough trees with the given passthrough data */
    function updateTrees(passthroughs) {
        var treeData = {},
            openNodes = [];

        // update inputs
        _div_input.find("li.jstree-open").each(function () {
            openNodes.push(this.id);
        });
        _div_input.empty();
        jQuery.each(passthroughs.inputs, function(compName, compVars) {
            treeData.data = compName;
            treeData.attr = { 'id': compName, 'rel': 'disabled' };
            treeData.state = openNodes.indexOf(compName) >= 0 ? 'open' : 'closed';
            treeData.children = [];
            jQuery.each(compVars, function(varName, alias) {
                treeData.children.push({
                    'data': varName,
                    'attr': {
                        'id': varName,
                        'path': compName + '.' + varName,
                        'class': alias ? 'jstree-checked' : 'jstree-unchecked'
                    }
                });
            });
            makeTree(compName+"-input", _div_input, treeData);
        });

        treeData = {};
        openNodes = [];

        // update outputs
        _div_output.find("li.jstree-open").each(function () {
            openNodes.push(this.id);
        });
        _div_output.empty();
        jQuery.each(passthroughs.outputs, function(compName, compVars) {
            treeData.data = compName;
            treeData.attr = { 'id': compName, 'rel': 'disabled' };
            treeData.state = openNodes.indexOf(compName) >= 0 ? 'open' : 'closed';
            treeData.children = [];
            jQuery.each(compVars, function(varName, alias) {
                treeData.children.push({
                    'data': varName,
                    'attr': {
                        'id': varName,
                        'path': compName + '.' + varName,
                        'class': alias ? 'jstree-checked' : 'jstree-unchecked'
                    }
                });
            });
            makeTree(compName+"-output", _div_output, treeData);
        });
    }

    /** get passthrough data and update the input and output passthrough trees */
    function update() {
        project.getPassthroughs(pathname)
            .done(updateTrees)
            .fail(function(jqXHR, textStatus, errorThrown) {
                debug.error('Error getting passthoughs for', pathname,
                            jqXHR, textStatus, errorThrown);
            });
    }

    /** update when notified of change in assembly */
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== pathname) {
            debug.warn('Invalid object data for:', pathname, message);
            debug.warn('message length', message.length, 'topic', message[0]);
        }
        else {
            if (Object.keys(message[1]).length > 0) {
                update();
            }
            else {
                _self.close();  // no data means the assembly was deleted
            }
        }
    }

    /***********************************************************************
     *  protected
     ***********************************************************************/

    // cancel subscriptions before you die
    this.destructor = function() {
        if (pathname && pathname.length>0) {
            project.removeListener(pathname, update);
        }
    };

    // populate the inputs/outputs trees
    update();

    // listen for changes to the assembly
    project.addListener(pathname, handleMessage);
};

/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;