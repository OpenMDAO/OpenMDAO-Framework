
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectionsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('connections-'+pathname).replace(/\./g,'-');
    openmdao.ConnectionsFrame.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        components_css = 'overflow:auto; background:grey;',
        components_div = jQuery('<div id='+id+' style="'+components_css+'">')
                        .appendTo(self.elm),
        src_selector = jQuery("<input id='srccmp_list' class='combobox' />")
                        .appendTo(components_div),
        dst_selector = jQuery("<input id='dstcmp_list' class='combobox' />")
                        .appendTo(components_div),
        connections_div = null,
        connections_pane = null;

    self.pathname = null;
    self.src_comp = src_comp;
    self.dst_comp = dst_comp;

    function editConnections(pathname, src_comp, dst_comp) {
        if (src_comp && dst_comp) {
            if (connections_pane === null) {
                connections_div = jQuery('<div style="overflow:auto; background:grey;">');
                self.elm.append(connections_div);
                connections_pane = new openmdao.ConnectionsPane(connections_div,
                                           model, pathname, src_comp, dst_comp);
                components_div.append(connections_pane);
            }
            else {
                connections_pane.editConnections(pathname, src_comp, dst_comp);
            }
        }
        else {
            if (connections_pane !== null) {
                connections_pane = null;
                connections_div.remove();
            }
        }
    }

    src_selector.autocomplete({
       select: function(event, ui) {
           src_selector.value = ui.item.value;
           ent = jQuery.Event('keypress.enterkey');
           ent.target = src_selector;
           ent.which = 13;
           src_selector.trigger(ent);
       },
       delay: 0,
       minLength: 0
    });
    src_selector.bind('keypress.enterkey', function(e) {
        if (e.which === 13) {
            src_selector.autocomplete('close');
            self.src_comp = e.target.value;
            editConnections(self.pathname, self.src_comp, self.dst_comp);
        }
    });

    dst_selector.autocomplete({
       select: function(event, ui) {
           dst_selector.value = ui.item.value;
           ent = jQuery.Event('keypress.enterkey');
           ent.target = dst_selector;
           ent.which = 13;
           dst_selector.trigger(ent);
       },
       delay: 0,
       minLength: 0
    });
    dst_selector.bind('keypress.enterkey', function(e) {
        if (e.which === 13) {
            dst_selector.autocomplete('close');
            self.dst_comp = e.target.value;
            editConnections(self.pathname, self.src_comp, self.dst_comp);
        }
    });

    function loadData(data) {
        if (!data || !data.Dataflow || !data.Dataflow.components) {
            // don't have what we need, probably something got deleted
            self.close();
        }
        else {
            comp_list = jQuery.map(data.Dataflow.components,
                                   function(comp,idx){ return comp.name; });

            // update the output & input selectors to current outputs & inputs
            src_selector.html('');

            //jQuery.each(out_list,function (idx,name) {
            //    src_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
            src_selector.autocomplete({source: comp_list});

            dst_selector.html('');
            //jQuery.each(in_list,function (idx,name) {
            //    dst_selector.append('<option value="'+name+'">'+name+'</option>');
            //});
            dst_selector.autocomplete({source: comp_list});
        }

        if (self.src_comp) {
            src_selector.val(self.src_comp);
        }
        if (self.dst_comp) {
            dst_selector.val(self.dst_comp);
        }
        editConnections(self.pathname, self.src_comp, self.dst_comp);
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            loadData(message[1]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if there is an object loaded, update it from the model */
    this.update = function() {
        // TODO: should just update existing panes rather than recreate them
        if (self.pathname && self.pathname.length>0) {
            self.editObject(self.pathname);
        }
    };

    /** get the specified object from model, load properties into tabs */
    this.editObject = function(path) {
        if (self.pathname !== path) {
           if (self.pathname !== null) {
                model.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            model.addListener(self.pathname, handleMessage);
        }

        model.getComponent(path, loadData,
            function(jqXHR, textStatus, errorThrown) {
                debug.warn('ConnectionsFrame.editObject() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            }
        );
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

    this.editObject(pathname);

    //FIXME: use data in message instead of brute force update
    model.addListener(pathname,this.update);
};

/** set prototype */
openmdao.ConnectionsFrame.prototype = new openmdao.BaseFrame();
openmdao.ConnectionsFrame.prototype.constructor = openmdao.ConnectionsFrame;
