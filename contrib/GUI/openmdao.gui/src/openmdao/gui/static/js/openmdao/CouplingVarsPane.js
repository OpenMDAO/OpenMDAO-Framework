
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.CouplingVarsPane = function(elm, project, pathname, name, editable) {
    var couplingVars,
        couplingVarsDiv = jQuery("<div id='"+name+"_CouplingVars' >"),
        addButton = jQuery("<button>Add Coupling Variables</button>").button(),
        clrButton = jQuery("<button>Clear Coupling Variables</button>").button(),
        columns = [
            {id:"independent", name:"Independent",  field:"independent",  width:180},
            {id:"dependent",   name:"Dependent",    field:"dependent", width:180}
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false
        };

    elm.append(couplingVarsDiv).width('100%');

    var table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    elm.append(table);

    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    couplingVars = new Slick.Grid(couplingVarsDiv, [], columns, options);
    if (editable) {
        couplingVars.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. project.setProperty(path,name,value)
            var cmd = pathname+'.'+args.item.name+' = '+args.item.value;
            project.issueCommand(cmd);
        });
   }

    /** add a new objective */
    function addCouplingVars(indep,dep,scope) {
        var cmd = pathname+".add_coupling_var('"+indep+"', '"+dep+"')";
        project.issueCommand(cmd);
    }

    /** prompt for new objective */
    function promptForCouplingVars(callback) {
        // Build dialog markup
        var win = jQuery('<div></div>'),
            indep = jQuery('<input type="text" style="width:100%"></input>'),
            dep   = jQuery('<input type="text" style="width:100%"></input>');

        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Independent: </div>').append(indep)))
                            .append(jQuery('<td>').append(jQuery('<div>Dependent:   </div>').append(dep)));
        table.append(row);
        win.append(table);

        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Coupling Variables',
            'buttons': {
                'Ok': function() {
                    jQuery(this).dialog('close');
                    callback(indep.val(),dep.val());
                    // remove from DOM
                    win.remove();
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                    // remove from DOM
                    win.remove();
                }
            }
        });
    }

    /** clear all CouplingVars */
    function clearCouplingVars() {
        var cmd = pathname+".clear_coupling_vars()";
        project.issueCommand(cmd);
    }

    addButton.click(function() { promptForCouplingVars(addCouplingVars); });
    clrButton.click(function() { clearCouplingVars(); });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            couplingVars.setData(properties);
        }
        else {
            couplingVars.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
        }
        couplingVars.updateRowCount();
        couplingVars.render();
    };
};
