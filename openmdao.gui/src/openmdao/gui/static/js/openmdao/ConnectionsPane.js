
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectionsPane = function(elm,model,pathname,src_comp,dst_comp) {
    var id = ('DCE-'+pathname+'-'+src_comp+'-'+dst_comp).replace(/\./g,'-');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        figures = {},
        dataflowID = id+"-connections",
        output_selector = jQuery("<input id='output_list' class='combobox' />"),
        input_selector = jQuery("<input id='input_list' class='combobox' />"),
        connect_button = jQuery("<button id='connect_button'>Connect</button>")
                        .click(function() {
                            var src = output_selector.val();
                            var dst = input_selector.val();
                            model.issueCommand(self.pathname+".connect('"+src+"','"+dst+"')");
                        }),
        dataflowCSS = 'overflow:auto; background:grey;',
        dataflowDiv = jQuery('<div id='+dataflowID+' style="'+dataflowCSS+'">')
                        .appendTo(elm),
        dataflow = new draw2d.Workflow(dataflowID);

    elm.append(output_selector);
    elm.append(input_selector);
    elm.append(connect_button);

    self.pathname = pathname;
    self.src_comp = src_comp;
    self.dst_comp = dst_comp;

    self.showAllVariables = false;  // only show connected variables by default

    // create context menu for toggling the showAllVariables option
    dataflow.getContextMenu=function(){
        var menu=new draw2d.Menu();
        if (self.showAllVariables) {
            menu.appendMenuItem(new draw2d.MenuItem("Show Connections Only",null,
                function(){
                    self.showAllVariables = false;
                    self.update();
                })
            );
        }
        else {
            menu.appendMenuItem(new draw2d.MenuItem("Show All Variables",null,
                function(){
                    self.showAllVariables = true;
                    self.update();
                })
            );
        }
        return menu;
    };

    // plain grey background
    dataflow.setBackgroundImage(null);
    dataflowDiv.css({'background-color':'grey'});

    function loadData(data) {
        if (!data || !data.outputs || !data.inputs) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            dataflow.clear();
            figures = {};
            var i = 0,
                x = 20,
                y = 10,
                conn_list = jQuery.map(data.connections, function(n){return n;}),
                out_list  = jQuery.map(data.outputs, function(n){return self.src_comp+'.'+n.name;}),
                in_list   = jQuery.map(data.inputs, function(n){return self.dst_comp+'.'+n.name;});

            for (i = 0; i <conn_list.length; i++) {
                conn_list[i]=conn_list[i].split('.')[1];
            }
            jQuery.each(data.outputs, function(idx,outvar) {
                if (self.showAllVariables || conn_list.contains(outvar.name)) {
                    var src_name = self.src_comp+'.'+outvar.name,
                        src_path = self.pathname+'.'+src_name,
                        fig = new openmdao.VariableFigure(model,src_path,outvar,'output');
                    dataflow.addFigure(fig,x,y);
                    figures[src_name] = fig;
                    y = y + fig.height + 10;
                }
            });

            x = 250;
            y = 10;
            jQuery.each(data.inputs, function(idx,invar) {
                if (self.showAllVariables || conn_list.contains(invar.name)) {
                    var dst_name = self.dst_comp+'.'+invar.name,
                        dst_path = self.pathname+'.'+dst_name,
                        fig = new openmdao.VariableFigure(model,dst_path,invar,'input');
                    dataflow.addFigure(fig,x,y);
                    figures[dst_name] = fig;
                    y = y + fig.height + 10;
                }
            });
            dataflowDiv.css({'height':y+'px','width': x+100+'px'});

            jQuery.each(data.connections,function(idx,conn) {
                // internal connections
                if ((conn[0].indexOf('.') > 0) && (conn[1].indexOf('.') > 0)) {
                    var src_name = conn[0],
                        dst_name = conn[1],
                        src_fig = figures[src_name],
                        dst_fig = figures[dst_name],
                        src_port = src_fig.getPort("output"),
                        dst_port = dst_fig.getPort("input");
                    c = new draw2d.Connection();
                    c.setSource(src_port);
                    c.setTarget(dst_port);
                    c.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
                    c.setRouter(new draw2d.BezierConnectionRouter());
                    c.setCoronaWidth(10);
                    c.getContextMenu=function(){
                        var menu=new draw2d.Menu();
                        var oThis=this;
                        menu.appendMenuItem(new draw2d.MenuItem("Disconnect",null,function(){
                                var asm = self.pathname,
                                    cmd = asm + '.disconnect("'+src_name+'","'+dst_name+'")';
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
            });

            // update the output & input selectors to current outputs & inputs
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

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update from the model */
    this.update = function() {
        self.editConnections(self.pathname,self.src_comp,self.dst_comp);
    };

    /** edit connectiosn between the source and destination objects in the assembly */
    this.editConnections = function(pathname, src_comp, dst_comp) {
        self.pathname = pathname;
        self.src_comp = src_comp;
        self.dst_comp = dst_comp;

        model.getConnections(pathname, src_comp, dst_comp, loadData,
            function(jqXHR, textStatus, errorThrown) {
                debug.error(jqXHR,textStatus,errorThrown);
                self.close();
            }
        );
    };
    
    this.editConnections(pathname, src_comp, dst_comp);
};

/** set prototype */
openmdao.ConnectionsPane.prototype = new openmdao.BaseFrame();
openmdao.ConnectionsPane.prototype.constructor = openmdao.ConnectionsPane;
