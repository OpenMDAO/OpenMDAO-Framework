
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Passthroughs: '+openmdao.Util.getName(pathname));



    /***********************************************************************
     *  private
     ***********************************************************************/
     
     
    var pathname = pathname;
    //console.log(model);
   
    this.getAllVariables = function() {
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
                        all_inputs.push([comp_path + "." + input.name, top_inputs.contains(connected_to)]);
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
                       all_outputs.push([comp_path + "." + output.name, output_pass]);
                    })
                });
            });
        });
        
    return [all_inputs, all_outputs]
    }
    
    // model.issueCommand(cmd);
    // initialize private variables
    var self = this,
        // component selectors
        componentsHTML = '<div style="width:100%;background:grey"><table>'
                       +        '<tr><td>Input Name:</td>'
                       +            '<td>Passthrough:</td>'
                       +        '<td>Output Name:</td>'
                       +            '<td>Passthrough:</td>'
                       +        '</tr>'
                       //+        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></div>',
        componentsDiv = jQuery(componentsHTML)
            .appendTo(self.elm);
            
        data = this.getAllVariables();
        console.log(data[0].length);
}
/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;
