
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ComponentEditor = function(model,pathname) {
    openmdao.ComponentEditor.prototype.init.call(this,null,'Component');
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this
        
    model.addListener(update)
          
    /** load the table with the given properties */
    function loadTabs(properties) {
        var paneID = id_+'_tabs';
        self.elm.html('<dl id="'+paneID+'"></dl>');
        if (properties['parameters']) {
            self.elm.append('<dt id="'+id_+'_parameters_tab">Parameters</dt>');
            self.elm.append('<dd id="'+id_+'_parameters_pane"><div id="'+id_+'parameters"></div></dd>');
        }
        if (properties['objectives']) {
            self.elm.append('<dt id="'+id_+'_objectives_tab">Objectives</dt>');
            self.elm.append('<dd id="'+id_+'_parameters_pane"><div id="'+id_+'objectives"></div></dd>');
        }
        if (properties['eqconstraints']) {
            self.elm.append('<dt id="'+id_+'_eqconstraints_tab">EqConstraints</dt>');
            self.elm.append('<dd id="'+id_+'_parameters_pane"><div id="'+id_+'eqconstraints"></div></dd>');
        }
        if (properties['ineqconstraints']) {
            self.elm.append('<dt id="'+id_+'_ineqconstraints_tab">IneqConstraints</dt>');
            self.elm.append('<dd id="'+id_+'_parameters_pane"><div id="'+id_+'ineqconstraints"></div></dd>');
        }
        new openmdao.TabbedPane(paneID);
        debug.info('ComponentEditor',self.elm)
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get the specified object from model, load properties into tabs */
    this.editObject = function(path) {
        debug.info('ComponentEditor',self.elm)
        if (self.pathname !== path)
            self.pathname = path
        model.getComponent(path, loadTabs,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error getting properties for "+self.pathname+" (status="+jqXHR.status+"): "+jqXHR.statusText)
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting properties',600,400)
                debug.error(jqXHR,textStatus,errorThrown)
            }
        )
        return this
    }
    
}

/** set prototype */
openmdao.ComponentEditor.prototype = new openmdao.BasePane();
openmdao.ComponentEditor.prototype.constructor = openmdao.ComponentEditor;