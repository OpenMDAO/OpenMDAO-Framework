
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
        data = [],
        interval = 30,
        timer = null
    
    // create plot in a div inside the element
    plot = jQuery('<div style="height:350px;width:600px;padding:5px;">').appendTo(this.elm)
    plot = jQuery.plot(plot, [ data ], options)

    // if a port was specified then listen for updated values, otherwise generate random updates
    if (true) {
        /** TESTING WEBSOCKET STUFF */
        debug.info('making ajax call to get plot WS...');
        var varname = 'prob.dis1.y1'
        jQuery.ajax({
            type: 'GET',
            url:  'plotWS',
            success: function(port) {
                debug.info('got plot port:' + port);
                var url = 'ws://localhost:'+port+'/ws';
                sck = new WebSocket(url);
                debug.info("opened plot socket at",url,sck);
                sck.onopen = function (e) {
                    debug.info('plot socket opened',e);
                };
                sck.onclose = function (e) {
                    debug.info('plot socket closed',e);
                };
                sck.onmessage = function(e) {
                    debug.info('plot socket message:',e);
                    updateData(e.data);
                    updatePlot();                    
                };            
            },
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.error("Error getting console WS (status="+jqXHR.status+"): "+jqXHR.statusText)
                       debug.error(jqXHR,textStatus,errorThrown)
           }
        })
    }
    else {
        setRefresh(interval);
    }

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
    
    /** add a value to the data set, capping the number of points */
    function updateData(newValue) {
        // don't exceed max number of points
        var maxPoints = 300;        
        if (data.length > maxPoints)
            data = data.slice(1);

        // add the new value
        data.push(newValue);
    }
    
    /** update the plot with random data */
    function updatePlot() {
        // zip the generated y values with the x values
        var xydata = [];
        for (var i = 0; i < data.length; ++i)
            xydata.push([i, data[i]]);
        plot.setData([ xydata ]);
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
