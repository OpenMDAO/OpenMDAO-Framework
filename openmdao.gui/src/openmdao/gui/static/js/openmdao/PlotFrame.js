
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// requires flot.js

openmdao.PlotFrame = function(id, project, pathname) {
    openmdao.PlotFrame.prototype.init.call(this,id,'Plot: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        plot = null,
        options = {
            series: { shadowSize: 0 }, // drawing is faster without shadows
            xaxis: { show: false }
        },
        data = {},
        interval = 30,
        timer = null,
        maxPoints = 300,
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>"),
        variables = [];

    // create plot in a div inside the element
    plot = jQuery('<div style="height:350px;width:600px;padding:5px;">')
           .appendTo(this.elm);
    plot = jQuery.plot(plot, [ data ], options);

    /** add new values to the data set, capping the number of points */
    function updateData(newValues) {
        if (!newValues) {
            debug.error('PlotFrame received bad data:',newValues);
            return;
        }
        //newValues = jQuery.parseJSON(newValues);
        jQuery.each(newValues,function (name,val) {
            if (! data[name]) {
                data[name] = [];
            }
            // don't exceed max number of points
            if (data[name].length > maxPoints) {
                data[name] = data[name].slice(1);
            }
            val = parseFloat(val);
            data[name].push(val);
        });
    }

    /** update the plot with current data */
    function updatePlot() {
        var plotdata = [];
        jQuery.each(data,function (name,vals) {
            // generate index values for x-axis, zip with y values
            var x = 0,
                xydata = [];
            for (x = 0; x < vals.length; ++x) {
                xydata.push([x, vals[x]]);
            }
            plotdata.push({ 'data':xydata, 'label':name });
        });
        plot.setData(plotdata);
        plot.resize();          // in case the frame was resized
        plot.setupGrid();
        plot.draw();
    }

    function handleMessage(message) {
        if (message.length === 2) {
            var newdata = {};
            newdata[message[0]] = message[1];
            updateData(newdata);
        }
        updatePlot();
    }

    // subscribe to project for data
    function plotVariable(pathname) {
        variables.push(pathname);
        project.addListener(pathname, handleMessage);
    }

    // prompt for a new variable to add to the plot
    function addVariable() {
        openmdao.Util.promptForValue('Enter pathname of variable to plot:',
            function(pathname) {
                plotVariable(pathname);
            }
        );
    }

    // create context menu
    contextMenu.append(jQuery('<li>Add Variable...</li>').click(function(ev) {
        addVariable();
    }));
    contextMenu.appendTo(this.elm);
    ContextMenu.set(contextMenu.attr('id'), id);

    if (pathname) {
        plotVariable(pathname);
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        var i =0;
        for (i = 0 ; i < variables.length ; ++i) {
            project.removeListener(variables[i], handleMessage);
        }
    };

    /** nothing to see here, we get our data elsewhere */
    this.update = function() {};

};

/** set prototype */
openmdao.PlotFrame.prototype = new openmdao.BaseFrame();

openmdao.PlotFrame.prototype.chooseVariable = function() {
    openmdao.Util.promptForValue('Enter pathname of variable to plot:',
        function(pathname) {
            p=new openmdao.PlotFrame('plot-'+pathname,openmdao.project,pathname);
        }
    );
};

