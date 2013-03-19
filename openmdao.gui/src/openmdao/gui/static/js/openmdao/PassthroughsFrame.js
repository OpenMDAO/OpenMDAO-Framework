var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    var table_id_input = id+'-passthrough-input-table';
    var tree_dep = {};
    var pathname = pathname;
    var table_id_output = id+'-passthrough-output-table';
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Edit passthroughs: '+openmdao.Util.getName(pathname));
    var self = this;
        // component selectors
    var modified = "";
    /***********************************************************************
     *  private
     ***********************************************************************/
    
    
    this.handleCbClick = function(e, d) { 
        var tagName = d.args[0].tagName;
        var refreshing = d.inst.data.core.refreshing;
        if ((tagName == "A" || tagName == "INS") &&
          (refreshing != true && refreshing != "undefined")) {
        
        cobj = jQuery(d.rslt[0])
        var this_path = cobj.attr("path");
        var status = cobj.attr("class").split(' ').pop();
        
        if (status == "jstree-checked") {
            self.makePassthrough(this_path);
            }
        else {
            self.removePassthrough(this_path);
             }
        
        
        }}

    this.successHandler = function(e, u) {
        self.makeTables();
    }
    
    this.doneHandler = function(e, u) {
        //self.makeTables();
    }
    
    this.errorHandler = function(jqXHR, textStatus, errorThrown) {

    }
    
    this.makePassthrough = function(path) {
        var parts = path.split(".");
        var assembly_idx = parts.indexOf(pathname.split(".").slice(-1)[0]);
        var comp_path = parts.slice(assembly_idx + 1).join(".");
        var cmd = "_ = " + pathname +".create_passthrough('"+comp_path+"')";

        model.issueCommand(cmd, self.successHandler, self.errorHandler, self.doneHandler);
    }
        
    this.removePassthrough = function(path) {

        var parts = path.split(".");
        var assembly = parts[0];
        var vname = parts[parts.length - 1];

        var cmd = "_ = " + pathname +".remove('"+vname+"')";

        model.issueCommand(cmd, self.successHandler, self.errorHandler, self.doneHandler);
    }    
    
    var pathname = pathname;
    var input_data = {};
    var output_data = {};
    
    tree_dep = {};

    /** handle message about the assembly */
    function handleMessage(message) {

    }
    
    
    
    function makeTree(name, target, jsonData) {        
        this_name = name+'-tree_container';
        treeHTML = '<div id = '+ this_name + '/>'
        jQuery(treeHTML).appendTo(target);
        
        tree_input = jQuery("#" + this_name); 
        tree_input.jstree({
            "core" : { 
                    "animation" : false 
            }, 
            "plugins" :     [ "json_data", "sort", "themes", "types", "ui","crrm","checkbox" ],
            "json_data"   : { "data" : jsonData },
            "themes"      : { "theme":  "openmdao", "icons" : false  },
            "checkbox": {real_checkboxes :true},
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
        })
        
        tree_input.bind("change_state.jstree", self.handleCbClick);
        
        
        }
    this.makeTables = function() {
        jQuery("#"+id+'-passthroughdiv').empty().remove();

        passthroughHTML = '<div id = '+id+'-passthroughdiv style = "overflow:auto;overflow-y:auto;background:gray"><table><tr><td valign = "top">INPUTS:<div id = '+table_id_input+'-div>'
                       + '</div></td>'
                       +   '<td valign = "top">OUTPUTS:<div id = '+table_id_output+'-div>'
                       + '</div></table>'      
        passthroughDiv = jQuery(passthroughHTML).appendTo(self.elm);
        div_input = jQuery("#" + table_id_input+'-div');
        div_output = jQuery("#" + table_id_output+'-div');  
        
        var all_inputs = [];
        var all_outputs = []; 
        var top_inputs = [];
        var top_outputs = [];
        
        var input_targets = [];
        var output_targets = [];

        model.getAllAttributes(pathname, function(attributes,e) { //assembly-level: collect existing input passthroughs       
            inputs = attributes.inputs
            outputs = attributes.outputs
            jQuery.each(inputs, function(idx,component) {
                makeTree(component.data + "input", div_input, component)

            })
            jQuery.each(outputs, function(idx,component) {
                makeTree(component.data + "output", div_output, component)

            })
                
        }); //end getComponent assembly-level call
        
    }       

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

        self.makeTables();
        model.addListener(pathname, handleMessage);
}
/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;