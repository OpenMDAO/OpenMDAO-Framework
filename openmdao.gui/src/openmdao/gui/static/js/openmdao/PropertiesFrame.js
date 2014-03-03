
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesFrame = function(id, project) {
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

    self.elm.append(nameHeader);
    self.elm.append('<p>');
    self.elm.append(inputsHeader);
    self.elm.append(inputsDiv);
    self.elm.append('<p>');
    self.elm.append(outputsHeader);
    self.elm.append(outputsDiv);
    self.elm.width(200);

    inputs = new openmdao.PropertiesPane(inputsDiv,project,self.pathname,'Inputs',true);
    inputsHeader.click(function () {
        inputsDiv.toggle("normal");
        return false;
    });

    outputs = new openmdao.PropertiesPane(outputsDiv,project,self.pathname,'Outputs',false);
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
            // Assume component was deleted.
            if (self.elm.parent().hasClass('ui-dialog')) {
                self.close();
            }
            else {
                nameHeader.html('');
                inputs.loadData([]);
                outputs.loadData([]);
            }
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

    /** resize contents */
    this.resize_contents = function() {
        self.elm.find('.slickgrid').trigger('resizeCanvas');
    };

    /** get the specified object from project, load properties into table */
    this.editObject = function(path) {
        if (self.pathname !== path) {
           if (self.pathname) {
                project.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            inputs.pathname = path;
            outputs.pathname = path;
            // listen for messages and update component properties accordingly
            project.addListener(self.pathname, handleMessage);

            project.getObject(path)
                .done(loadTables)
                .fail(function(jqXHR, textStatus, errorThrown) {
                    self.pathname = '';
                    if (self.elm.parent().hasClass('ui-dialog')) {
                        self.close();
                    }
                    else {
                        loadTables({});
                    }
                });
        }
        return this;
    };

    /** if there is an object loaded, update it from the project */
    this.update = function() {
        if (self.pathname && self.pathname.length>0) {
            self.editObject(self.pathname);
        }
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            project.removeListener(self.pathname, handleMessage);
        }
    };

};

/** set prototype */
openmdao.PropertiesFrame.prototype = new openmdao.BaseFrame();
openmdao.PropertiesFrame.prototype.constructor = openmdao.PropertiesFrame;

