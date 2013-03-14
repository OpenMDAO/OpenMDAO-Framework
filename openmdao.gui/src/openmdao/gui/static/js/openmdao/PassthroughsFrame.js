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
    
    this.travel_tree = function(d, name) {
        parent_ = d[name];
        parents = [parent_];
        up_ = d[parent_];
        while (up_) {
            parents = parents.concat(up_)
            up_ = d[up_]
        }
        return parents
        
    }
    
    this.get_halfchecked = function(tree_id) {
        checked_ids = [];
        jQuery(tree_id).find(".jstree-undetermined").each(function(i,element){    
            elem_id = jQuery(element).find("a").attr("id")
            checked_ids.push(elem_id);
        })
        return checked_ids;
    }
    
    this.disable_halfchecked = function()   {
        halfchecked_input = self.get_halfchecked("#" + table_id_input+'-div');
        halfchecked_output = self.get_halfchecked("#" + table_id_output+'-div');
        halfchecked = halfchecked_input.concat(halfchecked_output);
        
        jQuery.each(halfchecked, function(idx,an_id) {
            //div_input.jstree("set_type", "disabled", jQuery('#'+an_id));
            //div_output.jstree("set_type", "disabled", jQuery('#'+an_id));
            })
        }
    
    this.enable_parents = function(d, an_id) {
        parents = self.travel_tree(d, an_id);
        jQuery.each(parents, function(idx,this_id) {
            //div_input.jstree("set_type", "enabled", jQuery('#'+this_id));
            //div_output.jstree("set_type", "enabled", jQuery('#'+this_id));
            })
        }
    
    
    this.handleCbClick = function(e, d) { 
        var tagName = d.args[0].tagName;
        var refreshing = d.inst.data.core.refreshing;
        if ((tagName == "A" || tagName == "INS") &&
          (refreshing != true && refreshing != "undefined")) {
        c_data = d.rslt.obj[0].innerHTML;
        cobj = jQuery('<div>').html(c_data).find('a');
        
        var dom_id = cobj.attr('id');
        var name = cobj.attr('vname');
        modified = cobj.attr('component');
        var itype = cobj.attr('itype');
        var status = e.type;
        
        
        if (cobj.attr('parent') != "") {
            var this_path = modified + "." + cobj.attr('parent') + "." + name;
        }
        else {
             var this_path = modified + "." + name;
        
        }
        if (status == "check_node") {
            self.makePassthrough(this_path);
            self.disable_halfchecked();
            }
        else {
            self.removePassthrough(this_path, itype);
            self.enable_parents(tree_dep, dom_id);
            self.disable_halfchecked();
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
    
    this.check_passthrough_input = function(comp_path, input, connected_to, implicit_con, top_inputs)    {
        var ctl = 0;
        if (connected_to) {
            ctl = connected_to.length; 
            for (var i = 0; i < ctl; i++) {
                if (top_inputs.contains(connected_to[i])) {
                    checked="checked"; disabled = "";
                     return [checked, disabled]
                    }
                }
            }
        if (top_inputs.contains(pathname + "."+ input.name) || ctl > 0 || implicit_con) {
            checked = ""; disabled = "disabled";}
        else {checked = ""; disabled = "";}
        return [checked, disabled]
    }
    
    this.removePassthrough = function(path, itype) {

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
        console.log(jsonData)
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
        
        tree_input.bind("change_state.jstree", function (e, d) {
                if ((d.args[0].tagName == "A" || d.args[0].tagName == "INS") &&
                    (d.inst.data.core.refreshing != true && d.inst.data.core.refreshing != "undefined")) 
                {
                    //if a checkbox or it's text was clicked, 
                    //and this is not due to a refresh or initial load, run this code . . .
                    alert("list id: " +d.rslt.attr("id"));
                    alert("is item checked?" +"***TODO***"); 
                }
            });
        
        
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
            console.log(attributes)            
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