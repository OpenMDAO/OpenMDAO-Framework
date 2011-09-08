
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ComponentEditor = function(model,pathname) {
    // TODO: hack alert... mangling pathname
    openmdao.ComponentEditor.prototype.init.call(this,'CE-'+pathname.replace(/\./g,'-'),'Component: '+pathname);
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        panes = {};
        
    model.addListener(update)
          
    /** load the table with the given properties */
    function loadTabs(properties) {
        var style = 'style="padding:5px;background-color:#6a6a6a;border:1px solid #fff;"',
            dl = jQuery('<dl id="'+self.id+'_tabs"></dl>');
            
        self.elm.html("");
        self.elm.append(dl);
        
        var tabcount = 0;
        
        jQuery.each(properties,function (name,val) {
            if (name !== 'type') {
                tabcount = tabcount + 1;
                
                if (name.length > 10) {
                    tabname = name.substr(0,10);
                }
                else {
                    tabname = name;
                }
                
                var contentID = self.id+'_'+name,
                    tabID = contentID+'_tab',
                    targetID = contentID+'_pane',                    
                    dt = jQuery('<dt id="'+tabID+'" target="'+targetID+'">'+tabname+'</dt>'),
                    dd = jQuery('<dd id="'+targetID+'"></dd>'),
                    contentPane = jQuery('<div id="'+contentID+'" '+style+'></div>');
                                    
                dl.append(dt);
                dl.append(dd);
                dd.append(contentPane)
                
                getContent(contentPane,name,val)
            }
        });
        
        self.elm.width((tabcount+1)*75);

        openmdao.TabbedPane(self.id);
    }
    
    /** populate content pane appropriately for the content */
    function getContent(contentPane,name,val) {
        // TODO: get content pane type more dynamically (a look up table maybe?)
        if (name == 'Inputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane,model,pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name == 'Outputs') {
            panes[name] = new openmdao.PropertiesPane(contentPane,model,pathname,name,false);
            panes[name].loadData(val);
        }
        else if (name == 'Objectives') {
            panes[name] = new openmdao.ObjectivesPane(contentPane,model,pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name == 'Parameters') {
            panes[name] = new openmdao.ParametersPane(contentPane,model,pathname,name,true);
            panes[name].loadData(val);
        }
        else if ((name == 'EqConstraints') || (name == 'IneqConstraints')) {
            panes[name] = new openmdao.ConstraintsPane(contentPane,model,pathname,name,true);
            panes[name].loadData(val);
        }
        else if (name == 'Workflow') {
            panes[name] = new openmdao.WorkflowPane(contentPane,model,pathname,name,false);
            panes[name].loadData(val);
        }
        else {
            panes[name] = new openmdao.PropertiesPane(contentPane,model,pathname,name,false);
            panes[name].loadData(val);
        }
    }

    function loadData(properties) {
        jQuery.each(properties,function (name,val) {
            if (panes[name]) {
                panes[name].loadData(val);
            }
            else if (name !== 'type') {
                debug.warn("ComponentEditor: Unexpected object",pathname,name,val)
            }
        })
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        // TODO: should just update existing panes rather than recreate them
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get the specified object from model, load properties into tabs */
    this.editObject = function(path) {        
        var callback = loadData;
        if (self.pathname !== path) {
            // if not already editing this object, create the tabbed panes
            self.pathname = path;
            callback = loadTabs;
        }
        model.getComponent(path, callback,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error getting properties for "+self.pathname+" (status="+jqXHR.status+"): "+jqXHR.statusText)
                openmdao.Util.htmlWindow(jqXHR.responseText,'Error getting properties',600,400)
                debug.error(jqXHR,textStatus,errorThrown)
            }
        )
        return this;
    }

    if (pathname) {
        this.editObject(pathname);
    }

}

/** set prototype */
openmdao.ComponentEditor.prototype = new openmdao.BasePane();
openmdao.ComponentEditor.prototype.constructor = openmdao.ComponentEditor;