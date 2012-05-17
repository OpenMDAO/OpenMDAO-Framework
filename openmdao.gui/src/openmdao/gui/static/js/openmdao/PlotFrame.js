
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// requires flot.js

openmdao.PlotFrame = function(id,model,pathname) {
    openmdao.PlotFrame.prototype.init.call(this,id,'Plot: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        plot = null,
        options = {
            series: { shadowSize: 0 }, // drawing is faster without shadows
            yaxis: { min: 0, max: 150 },
            xaxis: { show: false }
        },
        data = {},
        interval = 30,
        timer = null,
        maxPoints = 300;

    self.pathname = pathname;

    // create plot in a div inside the element
    plot = jQuery('<div style="height:350px;width:600px;padding:5px;">')
           .appendTo(this.elm);
    plot = jQuery.plot(plot, [ data ], options);

    /** set the plot to continuously update after specified ms */
    function setRefresh(interval, func) {
        if (timer !== 'undefined') {
            clearInterval(timer);
        }
        timer = setInterval(func,interval);
    }

    // subscribe to model for data (or set a timer to poll for data)
    if (pathname) {
        model.addListener(pathname,function(message) {
            if (message.length === 2) {
                var newdata = {};
                newdata[message[0]] = message[1];
                updateData(newdata);
            }
            updatePlot();
        });
    }
    else {
        setRefresh(interval,function() {
            updateData(getRandomValue());
            updatePlot();
        });
    }

    /** return a new data value, randomly generated from last value */
    function getRandomValue() {
        var prev = data.length > 0 ? data[data.length - 1] : 50;
        var y = prev + Math.random() * 10 - 5;
        if (y < 0) {
            y = 0;
        }
        if (y > 100) {
            y = 100;
        }
        return y;
    }

    /** add new values to the data set, capping the number of points */
    function updateData(newValues) {
        if (!newValues) {
            debug.error('PlotFrame received bad data for',pathname,newValues);
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

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** nothing to see here, we get our data elsewhere */
    this.update = function() {};

};

/** set prototype */
openmdao.PlotFrame.prototype = new openmdao.BaseFrame();

openmdao.PlotFrame.prototype.chooseVariable = function() {
    openmdao.Util.promptForValue('Enter pathname of variable to plot:',
        function(pathname) {
            p=new openmdao.PlotFrame('plot-'+pathname,openmdao.model,pathname);
        }
    );
};

