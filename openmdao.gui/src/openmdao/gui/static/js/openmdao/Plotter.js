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
    var elm = jQuery("#"+id)
    
    /** example using inline data source, data should be fetched from a server */
    var data = [], 
    totalPoints = 300;
    
    function getRandomData() {
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

    var updateInterval = 30;
    
    /** setup control widget * /
    jQuery("#updateInterval").val(updateInterval).change(function () {
        var v = jQuery(this).val();
        if (v && !isNaN(+v)) {
            updateInterval = +v;
            if (updateInterval < 1)
                updateInterval = 1;
            if (updateInterval > 2000)
                updateInterval = 2000;
            jQuery(this).val("" + updateInterval);
        }
    });
    /**/

    // setup plot
    var options = {
        series: { shadowSize: 0 }, // drawing is faster without shadows
        yaxis: { min: 0, max: 100 },
        xaxis: { show: false }
    };
    var plot = jQuery.plot(elm, [ getRandomData() ], options);

    function update() {
        plot.setData([ getRandomData() ]);
        // since the axes don't change, we don't need to call plot.setupGrid()
        plot.draw();
        
        setTimeout(update, updateInterval);
    }

    update();
    
    
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