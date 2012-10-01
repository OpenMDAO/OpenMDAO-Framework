
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    var table_id_input = id+'-passthrough-input-table';
    var table_id_output = id+'-passthrough-output-table';
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Edit passthroughs: '+openmdao.Util.getName(pathname));

    var self = this,
        // component selectors
        componentsHTML = '<div style="width:100%;background:grey"><table id = '+table_id_input+'>'
                       +        '<tr><td>Input Name:</td>'
                       +            '<td>Passthrough:</td>'
                       +        '</tr>'
                       //+        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></div><br><div style="width:100%;background:grey">'
                        +   '<table id = '+table_id_output+'>'
                       +        '<td>Output Name:</td>'
                       +            '<td>Passthrough:</td>'
                       +        '</tr>'
                       //+        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></div>'      
        componentsDiv = jQuery(componentsHTML)
            .appendTo(self.elm);

    /***********************************************************************
     *  private
     ***********************************************************************/
     
    this.handleCbClick = function(e) { 
        var this_path = e.target.name;
        var itype = jQuery(e.target).data("itype");
        var status = e.srcElement.checked;
        if (status) {
            self.makePassthrough(this_path);
            // model.issueCommand(cmd);
        }
        else {
            self.removePassthrough(this_path);
            
        }
    }     
     
    this.makePassthrough = function(path) {
        var parts = path.split(".");
        var assembly = parts[0];
        var vname = parts[parts.length - 1];
        var comp_path = parts.slice(1).join(".");
        var cmd = assembly +".create_passthrough('"+comp_path+"')";
        //console.log(cmd);
        model.issueCommand(cmd);
    }
    
    this.removePassthrough = function(path) {
        var parts = path.split(".");
        var assembly = parts[0];
        var vname = parts[parts.length - 1];
        var cmd1 = assembly +".disconnect('"+vname+"')";
        var cmd2 = assembly +".remove_trait('"+vname+"')";
        //console.log(cmd1);
        //console.log(cmd2);
        model.issueCommand(cmd1);
        model.issueCommand(cmd2);
    }    
    
    var table_input = jQuery("#" + table_id_input);
    var table_output = jQuery("#" + table_id_output);
    var pathname = pathname;
   
    var all_inputs = [];
    var all_outputs = []; 
    var top_inputs = [];
    var top_outputs = [];
    model.getComponent("top", function(asm,e) {
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
                    if (connected_to) {connected_to = connected_to[0];}
                    //all_inputs.push([comp_path + "." + input.name, top_inputs.contains(connected_to)]);
                    if (top_inputs.contains(connected_to)) {checked="checked";} else {checked = "";}
                    this_id = (comp_path + "-" + input.name).replace(".","-");
                    cb = '<input type="checkbox" data-itype = 0 id = '+this_id+'-cb name = '+comp_path + "." + input.name+' '+checked+'/>'
                    this_input = "<tr><td>"+input.name+"</td>  <td>"+cb+"</td></tr>";
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
                    if (output_pass) {checked="checked";} else {checked = "";}
                    cb = '<input type="checkbox" data-itype = 1 id = '+this_id+'-cb name = '+comp_path + "." + output.name+' '+checked+'/>'
                    this_output = "<tr><td>"+output.name+"</td>  <td>"+cb+"</td></tr>";
                    jQuery(this_output).appendTo(table_output);                   
                })
            });
        });
    });
        
    
    // model.issueCommand(cmd);

}
/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;
