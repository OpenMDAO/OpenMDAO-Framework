/***********************************************************************
 *  PassthroughsFrame: A frame that shows all the inputs and outputs of
 *                     of an assembly andenables creating and removing
 *                     of passthroughs.
 *
 *  There is an Inputs tree and an Outputs tree, with a checkbox next
 *  to each variable. Toggling the checkbox will create of remove that
 *  variable as a passthrough variable.
 *
 *  Arguments:
 *      pathname: the pathname of the assembly
 ***********************************************************************/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model, pathname) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Edit passthroughs: ' + openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        table_id_input  = id+'-inputs',
        table_id_output = id+'-outputs',
        passthroughHTML = '<div id='+id+'-passthroughdiv style = "overflow:auto;height:100%;background:gray">'
                        + '  <table><tr>'
                        + '    <td valign = "top">INPUTS:<div id='+table_id_input+'></div></td>'
                        + '    <td valign = "top">OUTPUTS:<div id='+table_id_output+'></div></td>'
                        + '  </tr></table>'
                        + '</div>',
        passthroughDiv = jQuery(passthroughHTML)
            .appendTo(self.elm),
        div_input = passthroughDiv.find("#"+table_id_input),
        div_output = passthroughDiv.find("#"+table_id_output);

    /** create passthrough */
    function createPassthrough(path) {
        var parts = path.split("."),
            assembly_idx = parts.indexOf(pathname.split(".").slice(-1)[0]),
            comp_path = parts.slice(assembly_idx + 1).join("."),
            cmd = "_ = " + pathname +".create_passthrough('"+comp_path+"')";

        model.issueCommand(cmd);
    }

    /** remove passthrough */
    function removePassthrough(path) {
        var parts = path.split("."),
            assembly = parts[0],
            vname = parts[parts.length - 1],
            cmd = "_ = " + pathname +".remove('"+vname+"')";

        model.issueCommand(cmd);
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

    /** create jstree on the target element with the given json data*/
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

    // (re)create the inputs & outputs trees
    function update() {
        model.getAllAttributes(pathname, function(attributes,e) {
            var openNodes = [];
            div_input.find("li.jstree-open").each(function () {
                openNodes.push(this.id);
            });
            div_input.empty();
            jQuery.each(attributes.inputs, function(idx, component) {
                if (openNodes.indexOf(component.data) >= 0) {
                    component.state = 'open';
                }
                makeTree(component.data+"-input", div_input, component);

            });

            openNodes = [];
            div_output.find("li.jstree-open").each(function () {
                openNodes.push(this.id);
            });
            div_output.empty();
            jQuery.each(attributes.outputs, function(idx, component) {
                if (openNodes.indexOf(component.data) >= 0) {
                    component.state = 'open';
                }
                makeTree(component.data+"-output", div_output, component);
            });

        });
    }

    /***********************************************************************
     *  protected
     ***********************************************************************/

    this.destructor = function() {
        if (pathname && pathname.length>0) {
            model.removeListener(pathname, update);
        }
    };

    // populate the inputs/outputs trees
    update();

    // update when there is a change to the assembly
    model.addListener(pathname, update);
};

/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;