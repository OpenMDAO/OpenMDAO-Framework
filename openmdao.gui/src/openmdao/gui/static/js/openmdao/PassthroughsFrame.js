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
            div_input.jstree("set_type", "disabled", jQuery('#'+an_id));
            div_output.jstree("set_type", "disabled", jQuery('#'+an_id));
            })
        }
    
    this.enable_parents = function(d, an_id) {
        parents = self.travel_tree(d, an_id);
        jQuery.each(parents, function(idx,this_id) {
            div_input.jstree("set_type", "enabled", jQuery('#'+this_id));
            div_output.jstree("set_type", "enabled", jQuery('#'+this_id));
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
    
    
        jQuery("#"+id+'-passthroughdiv').empty().remove();

        componentsHTML = '<div id = '+id+'-passthroughdiv style = "overflow:auto;overflow-y:auto;background:gray"><table><tr><td valign = "top">INPUTS:<div id = '+table_id_input+'-div>'
                       + '</div></td>'
                       +   '<td valign = "top">OUTPUTS:<div id = '+table_id_output+'-div>'
                       + '</div></table>'      
        componentsDiv = jQuery(componentsHTML).appendTo(self.elm);
        
        div_input = jQuery("#" + table_id_input+'-div');
        div_output = jQuery("#" + table_id_output+'-div');               
        
        div_input.jstree({
            "core" : { 
                    "animation" : false 
            }, 
            "plugins" :     [ "html_data", "sort", "themes", "types", "ui","crrm","checkbox" ],
            "themes" :      { "theme":  "classic", "icons" : false },
            "checkbox": {real_checkboxes :true,
                        real_checkboxes_names : function(n) {return [n[0].lastChild.id+"chb",1]},
            }, 
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
        div_output.jstree({
            "core" : { 
                    "animation" : false 
            }, 
            "plugins" :     [ "html_data", "sort", "themes", "types", "ui", "crrm","checkbox" ],
            "themes" :      { "theme":  "classic", "icons" : false },
            "checkbox": {real_checkboxes :true,
                        real_checkboxes_names : function(n) {return [n[0].lastChild.id+"chb",1]},
            },  
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
        
    this.makeTables = function() {
        var all_inputs = [];
        var all_outputs = []; 
        var top_inputs = [];
        var top_outputs = [];
        
        var input_targets = [];
        var output_targets = [];

        model.getComponent(pathname, function(asm,e) {
            jQuery.each(asm.Inputs, function(idx,input) {
                top_inputs.push(pathname + "." + input.name);
                if (input.target) {
                    input_targets.push(pathname + "."+input.target);
                }
            })
            jQuery.each(asm.Outputs, function(idx,output) {
                top_outputs.push(pathname + "." + output.name);

                if (output.target) {
                    output_targets.push(pathname + "."+output.target);
                }
            })    
            jQuery.each(asm.Dataflow.components, function(idx,comp) {
                var comp_path = comp.pathname;
                this_id = comp_path.split(".").join("-");

                if (jQuery("#input-" + this_id).length == 0) {    
                    obj = {data : {"attr" : {"id" : 'input-'+ this_id}, "title" : comp_path, state : "closed"}}; 
                    div_input.jstree("create", -1, false,obj, false, true);
                    div_output.jstree("set_type", "disabled", jQuery('#input-'+this_id));
                    }
                    
                if (jQuery("#output-" + this_id).length == 0) {    
                    obj = {data : {"attr" : {"id" : 'output-'+this_id}, "title" : comp_path, state : "closed"}}; 
                    div_output.jstree("create", -1, false,obj, false, true);
                    div_output.jstree("set_type", "disabled", jQuery('#output-'+this_id));
                    }
                
                model.getComponent(comp.pathname, function (comp_data, e) {
                    jQuery.each(comp_data.Inputs, function(idx,input) {
                        implicit_con = "";
                        if (input.implicit) { implicit_con = eval(input.implicit.replace("parent",pathname));}
                        connected_to = eval(input.connected.replace("parent",pathname));
                        cd_array = self.check_passthrough_input(comp_path, input, connected_to, implicit_con, top_inputs);
                        
                        checked = cd_array[0];
                        disabled = cd_array[1];
                        pid = "";
                        if (input.parent) {
                            std_name = comp_path+ "." + input.parent + "." + input.name;
                            this_id = (comp_path + "-" + input.parent + "." + input.name).split(".").join("-")+'input-cb';
                            this_comp_id = (comp_path + "-" + input.parent).split(".").join("-")+'input-cb';
                            parent_state = jQuery.jstree._reference("#" + table_id_input+'-div').is_checked ("#"+this_comp_id);
                            pid = input.parent;
                            if (parent_state || jQuery("#"+this_comp_id).length == 0) {connected_to = "0"}

                        //tree_dep[comp_path + "." + input.parent + "." + input.name] = comp_path + "."+input.parent;
                        tree_dep[this_id] = this_comp_id;
                    
                        }
                        else {
                            std_name = comp_path+ "." + input.name;
                            this_id = (comp_path + "-" + input.name).split(".").join("-")+'input-cb';
                            this_comp_id = "input-" + comp_path.split(".").join("-");
                        }     
                        if (input_targets.indexOf(std_name) >= 0) { // passthrough exists?

                                checked = "checked";
                                disabled = "";
                        }
                        else if (connected_to) { // no passthrough - any other connections? if yes - disable toggle
                                checked = "";
                                disabled = "disabled";
                        }
                        else {checked = ""; disabled = "";}
                        if (jQuery("#" + this_id).length == 0 && disabled != "disabled") {    //create node

                            
                            obj = {data : {"attr" : {"id" : this_id, "itype":0, 
                                "component": comp_path,
                                "parent" : pid,
                                "vname" : input.name},
                                "title" : input.name, state : "closed"}}; 
                            
                            div_input.jstree("create", jQuery("#"+this_comp_id), false,obj, false, true);
                            if (checked == "checked") {


                                jQuery.jstree._reference("#" + table_id_input+'-div').check_node("#"+this_id);
                                }
                            else
                                {
                                jQuery.jstree._reference("#" + table_id_input+'-div').uncheck_node("#"+this_id)
                                }
                            
                            }
                        else if (jQuery("#"+this_id).length != 0 && disabled == "disabled") {
                        
                            div_input.jstree("remove", jQuery("#"+this_id));
                        }
                        if (modified != comp_path ) {
                           div_input.jstree("close_node", jQuery("#"+this_comp_id));
                           }
                    })
                    
                    jQuery.each(comp_data.Outputs, function(idx,output) {
                       connected_to = eval(output.connected.replace("parent",pathname));
                       
                        pid = "";
                        if (output.parent) { //variable tree element - nest under parent
                            std_name = comp_path+ "." + output.parent + "." + output.name;
                            this_id = (comp_path + "-" + 
                                output.parent + "." + output.name).split(".").join("-")+'output-cb';
                            this_comp_id = (comp_path + "-" + output.parent).split(".").join("-")+'output-cb';
                            parent_state = jQuery.jstree._reference("#" + table_id_output+'-div').is_checked ("#"+this_comp_id);
                            if (parent_state || jQuery("#"+this_comp_id).length == 0) {connected_to = "0"}
                            pid = output.parent;
                            tree_dep[this_id] = this_comp_id;
                        }

                        else { //all others
                            std_name = comp_path+ "." + output.name;
                            this_id = (comp_path + "-" + output.name).split(".").join("-")+'output-cb';
                            this_comp_id = "output-" + comp_path.split(".").join("-");
                        }    
                       
                       
                       //output_pass = false;
                       //if (connected_to) {
                       //     for (var i = 0; i < connected_to.length; i++) {
                       //         if (top_outputs.contains(connected_to[i])) {
                       //             output_pass = true;
                       //             break;
                       //         }
                       //     }
                       //}
                       
                        if (output_targets.indexOf(std_name) >= 0) { // passthrough exists?
                                checked = "checked";disabled = "";
                        }

                        else if (top_outputs.contains(pathname + "."+ output.name)) 
                            {checked = ""; disabled = "disabled";}

                        else {checked = ""; disabled = "";}
         
                        if (jQuery("#"+this_id).length == 0 && disabled != "disabled") {     
                            obj = {data : {"attr" : {"id" : this_id, "itype":1, 
                            "component": comp_path,
                            "parent" : pid,
                            "vname" :  output.name}, 
                            "title" : output.name, state : "closed"}}; 
                            
                            div_output.jstree("create", jQuery("#"+this_comp_id), false,obj, false, true);
                            if (checked == "checked") {
                                jQuery.jstree._reference("#" + table_id_output+'-div').check_node("#"+this_id);
                                }
                           
                            }
                        else if (jQuery("#"+this_id).length != 0 && disabled == "disabled") {
                        
                            div_output.jstree("remove", jQuery("#"+this_id));
                        }
                        if (modified != comp_path ) {
                           div_output.jstree("close_node", jQuery("#"+this_comp_id));
                           }
                    })
                });
            });
        });
    }       


div_input.bind("check_node.jstree", self.handleCbClick );
div_input.bind("uncheck_node.jstree", self.handleCbClick );
div_output.bind("check_node.jstree", self.handleCbClick );
div_output.bind("uncheck_node.jstree", self.handleCbClick );

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