/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.Plotter = function(id,model) {    // requires flot.js
    /***********************************************************************
     *  private
     ***********************************************************************/
    var self = this,
        elm = jQuery("#"+id),
        options = {
            series: { shadowSize: 0 }, // drawing is faster without shadows
            yaxis: { min: 0, max: 100 },
            xaxis: { show: false }
        },
        data = [],
        interval = 30,
        timer
    
    // if the elm doesn't exist, create a popup 
    if (elm.length === 0) {
        elm = jQuery('<div id='+id+'></div>')        
        elm.dialog({
            'modal': false,
            'title': 'Plot: '+id,
            'close': function(ev, ui) { clearInterval(timer); elm.remove(); },
            width: 600, 
            height: 350 
        })
    }
    else {
        elm.html("")
    }

    // create plot in a div inside the element
    plot = jQuery.plot(jQuery('<div style="padding:5px; height:100%">').appendTo(elm), [ getRandomData() ], options)
    
    // continuously update
    setRefresh(interval)

    /** set the plot to continuously update after specified ms */
    function setRefresh(interval) {
        self.interval = interval
        if (timer != 'undefined')
            clearInterval(timer)
        timer = setInterval(update,interval)    
    }
    
    /** return [x,y] data set, randomly updated with each call */
    function getRandomData() {
        var totalPoints = 300;
        
        if (data.length > 0)
            data = data.slice(1);

        // do a random walk
        while (data.length < totalPoints) {
            var prev = data.length > 0 ? data[data.length - 1] : 50;
            var y = prev + Math.random() * 10 - 5;
            if (y < 0)
                y = 0;
            if (y > 100)
                y = 100;
            data.push(y);
        }

        // zip the generated y values with the x values
        var res = [];
        for (var i = 0; i < data.length; ++i)
            res.push([i, data[i]])
        return res;
    }

    /** update the plot with random data */
    function update() {
        plot.setData([ getRandomData() ])
        plot.resize() // in case the popup was resized
        plot.setupGrid()
        plot.draw()
    }
    
    /** websocket example * /
    var url = "ws://localhost:9877/",
        s = new WebSocket(url),
        data = {};

 	s.onmessage = function(e) {
        //alert('got ' + e.data);
        var lines = e.data.split('\n');
        for (var i = 0; i < lines.length - 1; i++) {
            var parts = lines[i].split(' ');
            var d = parts[0], x = parseFloat(parts[1]), y = parseFloat(parts[2]);
            if (!(d in data)) data[d] = [];
                data[d].push([x,y]);
        }
 	
        var plots = [];
        for (var d in data) 
            plots.push( { data: data[d].slice(data[d].length - 200) } );
        jQuery.plot( elm, plots,{
            series: {
                lines: { show: true, fill: true },
                //points: { show: true },
            },
            //xaxis: { min: 0, max: 10 },
            //yaxis: { min: 0, max: 10 },
            xaxis: { min: 0 },
            yaxis: { min: 0 },
        });
        
        s.send('');
 	};
    /**/
}; 