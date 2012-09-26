
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ParametersPane = function(elm,model,pathname,name,editable) {
    var parms,
        parmsDiv = jQuery("<div id='"+name+"_parms'>"),
        buttonSpec = "class='button' style='text-align:center; margin-top:1em;'",
        addButton = jQuery("<div "+buttonSpec +">Add Parameter</div>"),
        clrButton = jQuery("<div "+buttonSpec +">Clear Parameters</div>"),
        columns = [
            {id:"target",  name:"Target",  field:"target",  width:140},
            {id:"low",     name:"Low",     field:"low",     width:70},
            {id:"high",    name:"High",    field:"high",    width:70},
            {id:"scaler",  name:"Scaler",  field:"scaler",  width:60},
            {id:"adder",   name:"Adder",   field:"adder",   width:50},
            {id:"fd_step", name:"fd_step", field:"fd_step", width:60},
            {id:"name",    name:"Name",    field:"name",    width:50}
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false
        };

    elm.append(parmsDiv);

    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    elm.append(table);

    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    parms = new Slick.Grid(parmsDiv, [], columns, options);
    if (editable) {
        parms.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            // TODO: check type and behave appropriately (quotes around strings?)
            cmd = pathname+'.'+args.item.name+'='+args.item.value;
            model.issueCommand(cmd);
        });
   }

    /** add a new parameter */
    function addParameter(target,low,high,scaler,adder,name) {
    
        // Supports parameter groups
        var targets = target.split(",");
        if (targets.length>1) {
            cmd = pathname+".add_parameter((";
            for(var i = 0; i < targets.length; i++) {
                cmd = cmd + "'" + jQuery.trim(targets[i]) + "',";
            }
            cmd = cmd + ")";
        }
        else {
            cmd = pathname+".add_parameter('"+target+"'";
        }
        
        if (low) {
            cmd = cmd + ",low="+low;
        }
        if (high) {
            cmd = cmd + ",high="+high;
        }
        if (scaler) {
            cmd = cmd + ",scaler="+scaler;
        }
        if (adder) {
            cmd = cmd + ",adder="+adder;
        }
        if (name) {
            cmd = cmd + ",name='"+name+"'";
        }
        cmd = cmd + ");";
        console.log(cmd)
        model.issueCommand(cmd);
    }

    /** prompt new parameter */
    function promptForParameter(callback) {
        // Build dialog markup
        var win = jQuery('<div id="parameter-dialog"></div>'),
            target = jQuery('<input id="parameter-target" type="text" style="width:100%"></input>'),
            low    = jQuery('<input id="parameter-low" type="text" style="width:50%"></input>'),
            high   = jQuery('<input id="parameter-high" type="text" style="width:50%"></input>'),
            scaler = jQuery('<input id="parameter-scaler" type="text" style="width:50%"></input>'),
            adder  = jQuery('<input id="parameter-adder" type="text" style="width:50%"></input>'),
            name   = jQuery('<input id="parameter-name" type="text" style="width:50%"></input>');

        win.append(jQuery('<div>Target: </div>').append(target));

        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Low: </div>').append(low)))
                            .append(jQuery('<td>').append(jQuery('<div>High: </div>').append(high)));
        table.append(row);
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Scaler: </div>').append(scaler)))
                            .append(jQuery('<td>').append(jQuery('<div>Adder: </div>').append(adder)));
        table.append(row);
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)));
        table.append(row);
        win.append(table);


        // Display dialog
        jQuery(win).dialog({
            modal: true,
            title: 'New Parameter',
            buttons: [
                {
                    text: 'Ok',
                    id: 'parameter-ok',
                    click: function() {
                        jQuery(this).dialog('close');
                        callback(target.val(),low.val(),high.val(),
                                 scaler.val(),adder.val(),name.val());
                        // remove from DOM
                        win.remove();
                    }
                },
                {
                    text: 'Cancel',
                    id: 'parameter-cancel',
                    click: function() {
                        jQuery(this).dialog('close');
                        // remove from DOM
                        win.remove();
                    }
                }
            ]
        });
    }

    /** clear all parameters */
    function clearParameters() {
        cmd = pathname+".clear_parameters();";
        model.issueCommand(cmd);
    }

    addButton.click(function() { promptForParameter(addParameter); });
    clrButton.click(function() { clearParameters(); });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            parms.setData(properties);
        }
        else {
            parms.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
            debug.info(properties);
        }
        parms.updateRowCount();
        parms.render();
    };
};
