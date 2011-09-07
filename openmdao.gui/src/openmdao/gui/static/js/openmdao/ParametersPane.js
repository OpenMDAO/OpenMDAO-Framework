
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ParametersPane = function(elm,model,pathname,name,editable) {
    var parms,
        parmsDiv = jQuery("<div id='"+name+"_parms'>"),
        addButton = jQuery("<div>Add Parameter</div>"),
        clrButton = jQuery("<div>Clear Parameters</div>"),
        columns = [
            {id:"target",  name:"Target",  field:"target", editor:TextCellEditor},
            {id:"low", name:"Low", field:"low", editor:TextCellEditor},
            {id:"high", name:"High", field:"high", editor:TextCellEditor},
            {id:"scaler", name:"Scaler", field:"scaler", editor:TextCellEditor},
            {id:"adder", name:"Adder", field:"adder", editor:TextCellEditor},
            {id:"fd_step", name:"fd_step", field:"fd_step", editor:TextCellEditor},
            {id:"scope", name:"Scope", field:"scope", editor:TextCellEditor},
            {id:"name",  name:"Name",  field:"name", editor:TextCellEditor},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
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
    };

    parms = new Slick.Grid(parmsDiv, [], columns, options)
    if (editable) {
        parms.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   };
    
    /** add a new parameter */
    function addParameter(target,low,high,scaler,adder) {
        cmd = "top."+pathname+".add_parameter('"+target+"'";
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
        cmd = cmd + ");"
        model.issueCommand(cmd);
    };
   
    /** prompt new parameter */
    function promptForParameter(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            target = jQuery('<input type="text" style="width:100%"></input>'),
            low    = jQuery('<input type="text" style="width:50%"></input>'),
            high   = jQuery('<input type="text" style="width:50%"></input>'),
            scaler = jQuery('<input type="text" style="width:50%"></input>'),
            adder  = jQuery('<input type="text" style="width:50%"></input>');
        
        win.append(jQuery('<div>Target: </div>').append(target));
        
        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Low: </div>').append(low)))
                            .append(jQuery('<td>').append(jQuery('<div>High: </div>').append(high)));
        table.append(row);        
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Scaler: </div>').append(scaler)))
                            .append(jQuery('<td>').append(jQuery('<div>Adder: </div>').append(adder)));                            
        table.append(row);
        win.append(table);

        
        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Parameter',
            'buttons': {
                'Ok': function() {                    
                    jQuery(this).dialog('close');
                    callback(target.val(),low.val(),high.val(),scaler.val(),adder.val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    };
    
    /** clear all parameters */
    function clearParameters() {
        cmd = "top."+pathname+".clear_parameters();"
        model.issueCommand(cmd);        
    }
    
    addButton.click(function() { promptForParameter(addParameter) });
    clrButton.click(function() { clearParameters() });

    /** load the table with the given properties */
    this.loadTable = function(properties) {
        if (properties) {
            parms.setData(properties)
        }
        else {
            parms.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        parms.updateRowCount()
        parms.render()
    }
        
}