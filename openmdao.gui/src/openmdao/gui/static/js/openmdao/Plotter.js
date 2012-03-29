
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

// requires flot.js

openmdao.Plotter = function(id,model,port) {
    openmdao.Plotter.prototype.init.call(this,id,'Plotter');
    
    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var plot = null,
        options = {
            series: { shadowSize: 0 }, // drawing is faster without shadows
            yaxis: { min: 0, max: 5 },
            xaxis: { show: false }
        },
        data = {},
        interval = 30,
        timer = null,
        maxPoints = 300;

    
    // create plot in a div inside the element
    plot = jQuery('<div style="height:350px;width:600px;padding:5px;">').appendTo(this.elm)
    plot = jQuery.plot(plot, [ data ], options)

    // connect to websocket for data (or set a timer to poll for data)
    if (true) {
        /** connect to a websocket */
        debug.info('connecting to plotWS')
        var sck = new WebSocket('ws://localhost:9000/workspace/plotWS');
        sck.onopen = function (e) {
            debug.info('plotWS socket opened',e);
        };
        sck.onclose = function (e) {
            debug.info('plotWS socket closed',e);
        };
        sck.onmessage = function(e) {
            debug.info('plotWS socket message:',e);
            updateData(e.data);
            updatePlot();                    
        };  
    }
    else {
        setRefresh(interval);
    };

    /** set the plot to continuously update after specified ms */
    function setRefresh(interval) {
        if (timer != 'undefined')
            clearInterval(timer);
        timer = setInterval(update,interval);
    }
    
    
    /** return a new data value, randomly generated from last value */
    function getRandomValue() {
        var prev = data.length > 0 ? data[data.length - 1] : 50;
        var y = prev + Math.random() * 10 - 5;
        if (y < 0)
            y = 0;
        if (y > 100)
            y = 100;
        return y;
    }
    
    /** add new values to the data set, capping the number of points */
    function updateData(newValues) {
        newValues = jQuery.parseJSON(newValues);
        jQuery.each(newValues,function (name,val) {
            if (! data[name]) {
                data[name] = []
            }
            // don't exceed max number of points
            if (data[name].length > maxPoints)
                data[name] = data[name].slice(1);
            data[name].push(val);
        });
    };
    
    /** update the plot with current data */
    function updatePlot() {
        var plotdata = []
        jQuery.each(data,function (name,vals) {
            // generate index values for x-axis, zip with y values
            var xydata = [];
            for (var x = 0; x < vals.length; ++x) {
                xydata.push([x, vals[x]]);
            };
            plotdata.push({ 'data':xydata, 'label':name });
        });
        plot.setData(plotdata);
        plot.resize();          // in case the popup was resized
        plot.setupGrid();
        plot.draw();
    }
    
    /** update the plot with random data */
    function update() {
        updateData(getRandomValue());
        updatePlot();
    }

};

/** set prototype */
openmdao.Plotter.prototype = new openmdao.BaseFrame();
openmdao.Plotter.prototype.constructor = openmdao.Plotter;
