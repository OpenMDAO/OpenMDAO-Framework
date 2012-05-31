
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesFrame = function(id,model) {
    openmdao.PropertiesFrame.prototype.init.call(this,id,'Properties');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pathname,
        inputs,
        outputs,
        nameHeader = jQuery("<h3>"),
        inputsHeader = jQuery("<h3>Inputs</h3>"),
        outputsHeader = jQuery("<h3>Outputs</h3>"),
        inputsDiv = jQuery("<div id='inputs'>"),
        outputsDiv = jQuery("<div id='outputs'>");

    this.elm.append(nameHeader);
    this.elm.append('<p>');
    this.elm.append(inputsHeader);
    this.elm.append(inputsDiv);
    this.elm.append('<p>');
    this.elm.append(outputsHeader);
    this.elm.append(outputsDiv);
    this.elm.width(200);

    inputs = new openmdao.PropertiesPane(inputsDiv,model,self.pathname,'Inputs',true);
    inputsHeader.click(function () {
        inputsDiv.toggle("normal");
        return false;
    });

    outputs = new openmdao.PropertiesPane(outputsDiv,model,self.pathname,'Outputs',false);
    outputsHeader.click(function () {
        outputsDiv.toggle("normal");
        return false;
    });

    /** load the tables with the given properties */
    function loadTables(properties) {
        if (properties && properties.type) {
            nameHeader.html(properties.type+': '+self.pathname);
            inputs.loadData(properties.Inputs);
            outputs.loadData(properties.Outputs);
        }
        else {
            nameHeader.html(self.pathname);
            inputs.loadData([]);
            outputs.loadData([]);
        }
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid properties data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            properties = message[1];
            loadTables(properties);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get the specified object from model, load properties into table */
    this.editObject = function(path) {
        if (self.pathname !== path) {
           if (self.pathname) {
                model.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            inputs.pathname = path;
            outputs.pathname = path;
            // listen for messages and update component properties accordingly
            model.addListener(self.pathname, handleMessage);
        }
        model.getComponent(path, loadTables,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = '';
                if (self.elm.parent().hasClass('ui-dialog')) {
                    self.close();
                }
                else {
                    loadTables({});
                }
            }
        );
        return this;
    };

    /** if there is an object loaded, update it from the model */
    this.update = function() {
        if (self.pathname && self.pathname.length>0) {
            self.editObject(self.pathname);
        }
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

};

/** set prototype */
openmdao.PropertiesFrame.prototype = new openmdao.BaseFrame();
openmdao.PropertiesFrame.prototype.constructor = openmdao.PropertiesFrame;

