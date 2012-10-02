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

    /***********************************************************************
     *  private
     ***********************************************************************/
     
    this.handleCbClick = function(e) { 
        var this_path = e.target.name;
        var itype = jQuery(e.target).data("itype");
        var status = e.srcElement.checked;
        if (status) {
            self.makePassthrough(this_path);
        }
        else {
            self.removePassthrough(this_path);
            
        }
    }     

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
        var assembly = parts[0];
        var vname = parts[parts.length - 1];
        var comp_path = parts.slice(1).join(".");
        var cmd = assembly +".create_passthrough('"+comp_path+"')";
        model.issueCommand(cmd, self.successHandler, self.errorHandler, self.doneHandler);
    }
    
    this.removePassthrough = function(path) {
        var parts = path.split(".");
        var assembly = parts[0];
        var vname = parts[parts.length - 1];
        var cmd1 = assembly +".disconnect('"+vname+"')";
        var cmd2 = assembly +".remove_trait('"+vname+"')";
        model.issueCommand(cmd1, function() {
            model.issueCommand(cmd2, self.successHandler, self.errorHandler, self.doneHandler);
        });
    }    
    
    var pathname = pathname;

    
    this.makeTables = function() {
        var all_inputs = [];
        var all_outputs = []; 
        var top_inputs = [];
        var top_outputs = [];
        jQuery("#"+id+'-passthroughdiv').empty().remove();
        componentsHTML = '<div id = '+id+'-passthroughdiv style = "overflow:scroll;overflow-y:scroll;background:gray"><table><tr><td><table id = '+table_id_input+'>'
                       +        '<tr><td><u>Input variables</u></td>'
                       +            '<td><u>Passthrough</u></td>'
                       +        '</tr>'
                       //+        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></td>'
                       +   '<td valign = "top"><table id = '+table_id_output+'>'
                       +        '<td><u>Output variables</u></td>'
                       +            '<td><u>Passthrough</u></td>'
                       +        '</tr>'
                       //+        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></td></tr></table></tr></div>'      
        componentsDiv = jQuery(componentsHTML).appendTo(self.elm);
        table_input = jQuery("#" + table_id_input);
        table_output = jQuery("#" + table_id_output);
        
        model.getComponent(pathname, function(asm,e) {
            jQuery.each(asm.Inputs, function(idx,input) {
                top_inputs.push(pathname + "." + input.name);
            })
            jQuery.each(asm.Outputs, function(idx,output) {
                top_outputs.push(pathname + "." + output.name);
            }    )    
            jQuery.each(asm.Dataflow.components, function(idx,comp) {
                var comp_path = comp.pathname;
                model.getComponent(comp.pathname, function (comp_data, e) {
                    jQuery.each(comp_data.Inputs, function(idx,input) {
                        connected_to = eval(input.connected.replace("parent",pathname));
                        ctl = 0;
                        if (connected_to) {ctl = connected_to.length; connected_to = connected_to[0];}
                        //all_inputs.push([comp_path + "." + input.name, top_inputs.contains(connected_to)]);
                        if (top_inputs.contains(connected_to)) {checked="checked"; disabled = "";}
                        else if (top_inputs.contains(pathname + "."+ input.name) || ctl > 0) {checked = ""; disabled = "disabled = 'disabled'";}
                        else {checked = ""; disabled = "";}
                        this_id = (comp_path + "-" + input.name).replace(".","-");
                        cb = '<center><input type="checkbox" data-itype = 0 '+disabled+' id = '
                            +this_id+'-cb name = '+comp_path + "." + input.name+' '+checked+'/></center>'
                        this_input = "<tr><td>"+comp_path + "." + input.name+"</td>  <td>"+cb+"</td></tr>";
                        jQuery(this_input).appendTo(table_input);
                        jQuery("#" + this_id+'-cb').click(self.handleCbClick);
                    })
                    jQuery.each(comp_data.Outputs, function(idx,output) {
                       connected_to = eval(output.connected.replace("parent",pathname));
                       output_pass = false;
                       if (connected_to) {
                            for (var i = 0; i < connected_to.length; i++) {
                                if (top_outputs.contains(connected_to[i])) {
                                    output_pass = true;
                                    break;
                                }
                            }
                       }
                       //all_outputs.push([comp_path + "." + output.name, output_pass]);
                        if (output_pass) {checked="checked"; disabled = "";} 
                        else if (top_outputs.contains(pathname + "."+ output.name)) {checked = ""; disabled = "disabled = 'disabled'";}
                        else {checked = ""; disabled = "";}
                        this_id = (comp_path + "-" + output.name).replace(".","-");
                        cb = '<center><input type="checkbox" data-itype = 1 '+disabled+' id = '
                         +this_id+'-cb name = '+comp_path + "." + output.name+' '+checked+'/></center>'
                        this_output = "<tr><td>"+comp_path + "." + output.name+"</td>  <td>"+cb+"</td></tr>";
                        jQuery(this_output).appendTo(table_output);
                        this_id = (comp_path + "-" + output.name).replace(".","-");
                        jQuery("#" + this_id+'-cb').click(self.handleCbClick);
                    })
                });
            });
        });
    }       
    
    self.makeTables();

}
/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;