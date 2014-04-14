
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ResponsesPane = function(elm, project, pathname, name, editable) {
    var responses,
        responsesDiv = jQuery("<div id='"+name+"_responses' class='slickgrid' style='overflow:none; height:320px; width:620px'>"),
        addButton = jQuery("<button>Add Response</button>").button(),
        clrButton = jQuery("<button>Clear Responses</button>").button(),
        columns = [
            {id:"del",   name:"",            field:"del",   width:25, formatter:buttonFormatter},
            {id:"expr",  name:"Expression",  field:"expr",  width:180},
            {id:"name",  name:"Name",        field:"name",  width:50}
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: false,
            autoEdit: false
        };

    function buttonFormatter(row, cell, value, columnDef, dataContext) {
        return '<div class="ui-icon-trash"></div>';
    }
    elm.append(responsesDiv).width('100%');

    var tabdiv = jQuery('<div class="post_slick" style="height:40px;">'),
        table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    tabdiv.append(table);
    elm.append(tabdiv);

    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    responses = new Slick.Grid(responsesDiv, [], columns, options);
    if (editable) {
        responses.onCellChange.subscribe(function(e, args) {
            // TODO: better way to do this (e.g. project.setProperty(path,name,value)
            var cmd = pathname+'.'+args.item.name+' = '+args.item.value;
            project.issueCommand(cmd);
        });
   }
    responses.onClick.subscribe(function (e) {
        var cell = responses.getCellFromEvent(e);
        if (cell.cell === 0) {
            var delname = responses.getData()[cell.row].name;
            var cmd = pathname+'.remove_response("'+delname+'")';
            project.issueCommand(cmd);
        }
    });

    /** add a new response */
    function addResponse(expr, name) {
        var cmd = pathname+".add_response('"+expr+"'";
        if (name) {
            cmd = cmd + ", name='"+name+"'";
        }
        cmd = cmd + ")";
        project.issueCommand(cmd);
    }

    responsesDiv.bind('resizeCanvas', function() {
        responses.resizeCanvas();
    });

    /** prompt for new response */
    function promptForResponse(callback) {
        // Build dialog markup
        var win = jQuery('<div id="response-dialog"></div>'),
            expr = jQuery('<input id="response-expr" type="text" style="width:100%"></input>'),
            name = jQuery('<input id="response-name" type="text" style="width:75%"></input>');

        win.append(jQuery('<div>Expression: </div>').append(expr));

        var table = jQuery('<table>');
        var row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)));
        table.append(row);
        win.append(table);

        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Response',
            'buttons': [
                {
                    text: 'Ok',
                    id: 'response-ok',
                    click: function() {
                        jQuery(this).dialog('close');
                        callback(expr.val(),name.val());
                        // remove from DOM
                        win.remove();
                    }
                },
                {
                    text: 'Cancel',
                    id: 'response-cancel',
                    click: function() {
                        jQuery(this).dialog('close');
                        // remove from DOM
                        win.remove();
                    }
                }
            ]
        });
    }

    /** clear all responses */
    function clearResponses() {
        var cmd = pathname+".clear_responses()";
        project.issueCommand(cmd);
    }

    addButton.click(function() { promptForResponse(addResponse); });
    clrButton.click(function() { clearResponses(); });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            responses.setData(properties);
        }
        else {
            responses.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
        }
        responses.updateRowCount();
        responses.render();
    };
};

