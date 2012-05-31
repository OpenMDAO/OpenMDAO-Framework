



var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ConnectionEditor = function(model,pathname,src_comp,dst_comp) {
    var id = ('DCE-'+pathname+'-'+src_comp+'-'+dst_comp).replace(/\./g,'-');
    openmdao.ConnectionEditor.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname)+' '+src_comp+' to '+dst_comp);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        dataflowID = "#"+id+"-connections",
        dataflowDiv = jQuery('<div id='+dataflowID+'>').appendTo('<div style="background:white">').appendTo('#'+id),
        dataflow = new draw2d.Workflow(dataflowID),
        output_selector = jQuery("<input id='output_list' class='combobox' />"),
        input_selector = jQuery("<input id='input_list' class='combobox' />"),
        connect_button = jQuery("<button id='connect_button'>Connect</button>")
                            .click(function() {
                                var src = output_selector.val();
                                var dst = input_selector.val();
                                model.issueCommand(self.pathname+".connect('"+src+"','"+dst+"')");
                            });        

    self.elm.append(output_selector);
    self.elm.append(input_selector);
    self.elm.append(connect_button);
    
    self.pathname = pathname;
    self.src_comp = src_comp;
    self.dst_comp = dst_comp;
    
    //dataflow.setBackgroundImage( "/static/images/grid_10.png", true);

    model.addListener('',update)
    
     


    function loadData(data) {
        if (!data || !data['outputs'] || !data['inputs']) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            dataflow.clear();
            figures = {};
            var x = 20, y = 10;
	    console.log(src_comp);
            conn_list=jQuery.map( data["connections"], function(n){   return n;});
	    out_list=jQuery.map( data["outputs"], function(n){   return src_comp+'.'+n.name;});
	    in_list=jQuery.map( data["inputs"], function(n){   return dst_comp+'.'+n.name;});

	    console.log(out_list);
	    console.log(in_list);
//	    jQuery( "#output_list" ).autocomplete({source: out_list,minLength:0});
//	    jQuery( "#input_list" ).autocomplete({source: in_list,minLength:0});

            for (var i = 0; i <conn_list.length; i++) {conn_list[i]=conn_list[i].split('.')[1];}            
            jQuery.each(data['outputs'], function(idx,outvar) {
                if (conn_list.contains(outvar['name'])) {
                var src_name = src_comp+'.'+outvar['name'],
                    src_path = self.pathname+'.'+src_name,
                    fig = new openmdao.VariableFigure(model,src_path,outvar,'output');
                dataflow.addFigure(fig,x,y);
                figures[src_name] = fig
                y = y + fig.height + 10;
                }
            });
            x = 250, y = 10;
            jQuery.each(data['inputs'], function(idx,invar) {
                if (conn_list.contains(invar['name'])) {
                var dst_name = dst_comp+'.'+invar['name'],
                    dst_path = self.pathname+'.'+dst_name,
                    fig = new openmdao.VariableFigure(model,dst_path,invar,'input');
                dataflow.addFigure(fig,x,y);
                figures[dst_name] = fig
                y = y + fig.height + 10;
                }
            });
            dataflowDiv.css({'height':y+'px','width': x+100+'px'});

            jQuery.each(data['connections'],function(idx,conn) {
                // internal connections
                if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                    var src_name = conn[0],
                        dst_name = conn[1],
                        src_fig = figures[src_name],
                        dst_fig = figures[dst_name],
                        src_port = src_fig.getPort("output"),
                        dst_port = dst_fig.getPort("input");
                    c = new draw2d.Connection()
                    c.setSource(src_port);
                    c.setTarget(dst_port);
                    c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                    c.setRouter(new draw2d.BezierConnectionRouter());
                    c.getContextMenu=function(){
                        var menu=new draw2d.Menu();
                        var oThis=this;
                        menu.appendMenuItem(new draw2d.MenuItem("Disconnect",null,function(){
                                var asm = self.pathname,
                                    cmd = asm + '.disconnect("'+src_name+'","'+dst_name+'");'
                                        + asm + '.config_changed(update_parent=True);';
                                model.issueCommand(cmd);
                            })
                        );
                        return menu;
                    };
                    dataflow.addFigure(c);
                    src_port.setBackgroundColor(new draw2d.Color(0,0,0));
                    dst_port.setBackgroundColor(new draw2d.Color(0,0,0));
                }
                // TODO: handle connections to parent assembly vars (e.g. Vehicle.velocity)
                // TODO: show passthroughs somehow
            })
            
            // update the output & input slectors to current outputs & inputs
            output_selector.html('');

            //jQuery.each(out_list,function (idx,name) {
            //    output_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
            output_selector.autocomplete({ source: out_list ,minLength:0});

            input_selector.html('');
            //jQuery.each(in_list,function (idx,name) {
            //    input_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
            input_selector.autocomplete({ source: in_list ,minLength:0});
            
        }
    }

    /** if there is something loaded, update it from the model */
    function update() {
        self.editConnections(self.pathname,self.src_comp,self.dst_comp);
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** edit connectiosn between the source and destination objects in the assembly */
    this.editConnections = function(pathname, src_comp, dst_comp) {        
        self.pathname = pathname;
        self.src_comp = src_comp;
        self.dst_comp = dst_comp;

        var callback = loadData;
        model.getConnections(pathname, src_comp, dst_comp, callback,
            function(jqXHR, textStatus, errorThrown) {
                debug.error(jqXHR,textStatus,errorThrown);
                self.close();
            }
        )
        return this;
    }
    //jQuery("#"+id).html("TEST");
    update();


}

/** set prototype */
openmdao.ConnectionEditor.prototype = new openmdao.BaseFrame();
openmdao.ConnectionEditor.prototype.constructor = openmdao.ConnectionEditor;
