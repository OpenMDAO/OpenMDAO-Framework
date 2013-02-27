var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    var table_id_input = id+'-passthrough-input-table';
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
        c_data = d.rslt.obj[0].innerHTML;
        cobj = jQuery('<div>').html(c_data).find('a');
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
            }
        else {
            self.removePassthrough(this_path, itype);
            
             }
        
        }}

    this.successHandler = function(e, u) {
        self.makeTables();
    }
    
    this.doneHandler = function(e, u) {
        //self.makeTables();
    }
    
    this.errorHandler = function(jqXHR, textStatus, errorThrown) {
        //console.log(textStatus);
    }
    
    this.makePassthrough = function(path) {
        var parts = path.split(".");
        var assembly_idx = parts.indexOf(pathname.split(".").slice(-1)[0]);
        var comp_path = parts.slice(assembly_idx + 1).join(".");
        var cmd = "_ = " + pathname +".create_passthrough('"+comp_path+"')";
        console.log("create");
        console.log(cmd);
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
        if (top_inputs.contains(pathname + "."+ input.name) || ctl > 0 || implicit_con || input.parent) {
            checked = ""; disabled = "disabled";}
        else {checked = ""; disabled = "";}
        return [checked, disabled]
    }
    
    this.removePassthrough = function(path, itype) {
        // console.log(parts);
        var parts = path.split(".");
        var assembly = parts[0];
        var vname = parts[parts.length - 1];

        var cmd = "_ = " + pathname +".remove('"+vname+"')";
        console.log("remove:")
        console.log(cmd);
        model.issueCommand(cmd, self.successHandler, self.errorHandler, self.doneHandler);
    }    
    
    var pathname = pathname;
    var input_data = {};
    var output_data = {};

    /** handle message about the assembly */
    function handleMessage(message) {
        //console.log(message);
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
                     } 
                 }
             }
        })
        
    this.makeTables = function() {
        var all_inputs = [];
        var all_outputs = []; 
        var top_inputs = [];
        var top_outputs = [];
        
        //div_input.empty()
        //div_output.empty()
        model.getComponent(pathname, function(asm,e) {
            jQuery.each(asm.Inputs, function(idx,input) {
                top_inputs.push(pathname + "." + input.name);
            })
            jQuery.each(asm.Outputs, function(idx,output) {
                top_outputs.push(pathname + "." + output.name);
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
                        console.log("in:"+comp_path+input.name);
                        cd_array = self.check_passthrough_input(comp_path, input, connected_to, implicit_con, top_inputs);

                        checked = cd_array[0];
                        disabled = cd_array[1];
                        this_id = (comp_path + "-" + input.name).split(".").join("-")+'input-cb';
                        pid = "";
                        if (input.parent) {
                        this_comp_id = (comp_path + "-" + input.parent).split(".").join("-")+'input-cb';
                        pid = input.parent;
                        }
                        else {
                        this_comp_id = "input-" + comp_path.split(".").join("-");
                        }     

                        if (jQuery("#" + this_id).length == 0 && disabled != "disabled") {    

                            
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
                        console.log("out:"+comp_path+output.name);
                       output_pass = false;
                       if (connected_to) {
                            for (var i = 0; i < connected_to.length; i++) {
                                if (top_outputs.contains(connected_to[i])) {
                                    output_pass = true;
                                    break;
                                }
                            }
                       }
                        if (output_pass) {checked="checked"; disabled = "";} 
                        else if (top_outputs.contains(pathname + "."+ output.name)) 
                            {checked = ""; disabled = "disabled";}

                        else {checked = ""; disabled = "";}
                        this_id = (comp_path + "-" + output.name).split(".").join("-")+'output-cb';
                        pid = "";
                        if (output.parent) {
                        this_comp_id = (comp_path + "-" + output.parent).split(".").join("-")+'output-cb';
                        pid = output.parent;
                        }
                        else {
                        this_comp_id = "output-" + comp_path.split(".").join("-");
                        }             
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