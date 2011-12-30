.. index:: DOE
.. _`DOE_paraboloid`:

Building a Model - Executing a Design of Experiment (DOE)
=========================================================

Lets say you're not interesting in optimization, instead you're much more interested 
in design space exploration. In that case you would want to use some kind of a Design 
Of Experiment (DOE). There are few different kinds of DOE's out there. Some of the most
popular are: 

  #. Full Factorial 
  #. Random Uniform
  #. Latin Hypercube
  #. Central Composite
  
OpenMDAO provides options to use all of these in our :ref:`standard library 
<openmdao.lib.doegenerators.api>`. If none of those meet your needs, you can also 
write your own DOEgenerator class to expand OpenMDAO capabilities (We'll leave that for a different
tutorial). For now, lets assume you wanted to use a Full Factorial DOE, with 10 levels for each 
variable. Create a file called *doe.py* and copy the following into it: 

.. testcode:: simple_model_doe

    from openmdao.main.api import Assembly, set_as_top
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial
    from openmdao.lib.casehandlers.api import ListCaseRecorder
    
    from openmdao.examples.simple.paraboloid import Paraboloid
    
    class Analysis(Assembly): 
    
        def __init__(self): 
            super(Analysis,self).__init__()
            
            self.add('paraboloid',Paraboloid())
            
            self.add('driver',DOEdriver())
            #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
            self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
            
            #DOEdriver will automatically record the values of any parameters for each case
            self.driver.add_parameter('paraboloid.x',low=-50,high=50)
            self.driver.add_parameter('paraboloid.y',low=-50,high=50)
            #tell the DOEdriver to also record any other variables you want to know for each case
            self.driver.case_outputs = ['paraboloid.f_xy',]
            
            #Simple recorder which stores the cases in memory. 
            self.driver.recorders = [ListCaseRecorder(),]
            
            self.driver.workflow.add('paraboloid')
            
            
To run a DOE we use the :ref:`DOEdriver <DOEdriver.py>`, which serves as the 
driver any time you want to run any kind of DOE. To specify the specific type of DOE you set the *DOEgenerator* 
atrribute. In this case we used :ref:`FullFactorial <FullFactorial.py>`, but any of the DOEgenerators 
would work. 

You can see that this code does not look a whole lot different than the code from the previous tutorials 
on :ref:`unconstrained <using-CONMIN>` and :ref:`constrained <constrained-optimization>`. We're still using 
the same Paraboloid component as before. Also, just like before, we use the *add_parameter* method to specify 
what inputs should be varied the by the DOE. Since we specifed the low and high to be -50 and 50 respectively, 
with 10 levels, the FullFactorial DOE generator will divide each parameter into 10 evenly spaced bins and then 
generate the full set of combinations possible (100 cases in total).

One new thing in this example is the use of a case recorder. Each case in a given DOE results in a set of inputs being
set into your model, then the model gets run, and some outputs are calculated. Obviously you want to record the results
of this process for each case in the DOE. You use a :ref:`CaseRecorder <openmdao.lib.casehandler.api.py>` for that. 
The CaseRecorder's job is to store the information from each case in some fassion. In this example 
we used a :ref:`ListCaseRecorder <openmdao.lib.casehandlers.listcaseiter.py>` which just stored them in memory. There are other kinds though
which are more permenant, for example the :ref:`DBcaseRecorder <openmdao.lib.casehandlers.db.py>` which 
saves all your cases to a SQLite database to be reviewed later. 

All CaseRecorders have the same interface, and can be all be used interchangably. In fact, 
if you notice that we specified a ListCasRecorder as part of a list. 

.. testsetup:: simple_model_doe_pieces
    
    from openmdao.main.api import Assembly, set_as_top
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial
    from openmdao.lib.casehandlers.api import ListCaseRecorder
    
    from openmdao.examples.simple.paraboloid import Paraboloid
    
    class Analysis(Assembly): 
    
        def __init__(self): 
            super(Analysis,self).__init__()
            
            self.add('paraboloid',Paraboloid())
            
            self.add('driver',DOEdriver())
            #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
            self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
            
            #DOEdriver will automatically record the values of any parameters for each case
            self.driver.add_parameter('paraboloid.x',low=-50,high=50)
            self.driver.add_parameter('paraboloid.y',low=-50,high=50)
            #tell the DOEdriver to also record any other variables you want to know for each case
            self.driver.case_outputs = ['paraboloid.f_xy',]
    
    self = Analysis()
   
.. testcode:: simple_model_doe_pieces
    
            #Simple recorder which stores the cases in memory. 
            self.driver.recorders = [ListCaseRecorder(),]

You can add as many CaseRecorders to that list as you want, and each one will record every case separately. That enables you 
to save information to more than one place at the same time.

The last new thing to look at is where we specify some extra variables to be saved off for each case. The DOEdriver 
automatically saves all the variables that were specified as parameters in every case. That way, you will always
know exactly what variable values were used for each case. But, of course, the inputs are just half the story. You will 
also want to store relevant outputs from each case. This is what the *case_outputs* attribute is for, on the DOEdriver. 
You would put any variables you wanted to track into this list, but here we only have the one output from 
paraboloid. 

.. testcode:: simple_model_doe_pieces

           self.driver.case_outputs = ['paraboloid.f_xy',]
           
           

To run this analysis, you would do the following: 

.. testsetup:: simple_model_doe_run

    from openmdao.main.api import Assembly, set_as_top
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial
    from openmdao.lib.casehandlers.api import ListCaseRecorder
    
    from openmdao.examples.simple.paraboloid import Paraboloid
    
    
    class Analysis(Assembly): 
        
        def __init__(self): 
            super(Analysis,self).__init__()
            
            self.add('paraboloid',Paraboloid())
            
            self.add('driver',DOEdriver())
            #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
            self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
            
            #DOEdriver will automatically record the values of any parameters for each case
            self.driver.add_parameter('paraboloid.x',low=-50,high=50)
            self.driver.add_parameter('paraboloid.y',low=-50,high=50)
            #tell the DOEdriver to also record any other variables you want to know for each case
            self.driver.case_outputs = ['paraboloid.f_xy',]
            
            #Simple recorder which stores the cases in memory. 
            self.driver.recorders = [ListCaseRecorder(),]
            
            self.driver.workflow.add('paraboloid')
                
.. testcode:: simple_model_doe_run

    if __name__ == "__main__":    

        import time
        
        analysis = Analysis()
        set_as_top(analysis)
    
        tt = time.time()
        analysis.run() 
        
        print "Elapsed time: ", time.time()-tt, "seconds"
        
        #write the case output to the screen
        for c in analysis.driver.recorders[0].get_iterator():
            print "x: %f, y: %f, z: %f"%(c['paraboloid.x'],c['paraboloid.y'],c['paraboloid.f_xy'])
            
The only new stuff here is the bit at the end where we loop over all the cases that were run. To keep
things simple, we just spit the data out to the screen. But the key thing to recognize here is  how you
work with cases. You can loop through each case by calling the *get_iterator()*  method 
on any case recorder. Then for each case you just adress the names of the variables like you would 
when working with a Python dictionary. You can put the data into any format you want from a loop like
this one. 

For instance, here is some code that uses matplotlib to generate a surface plot of the data from this run.

.. code-block:: python

    if __name__ == "__main__":    

        import time
        from matplotlib import pylab as p
        from matplotlib import cm
        import mpl_toolkits.mplot3d.axes3d as p3
        from numpy import array  
        
        analysis = Analysis()
        set_as_top(analysis)
    
        tt = time.time()
        analysis.run() 
        
        print "Elapsed time: ", time.time()-tt, "seconds"          
        
        raw_data = {}
        X=set()
        Y=set()
        for c in analysis.driver.recorders[0].get_iterator():
            raw_data[(c['paraboloid.x'],c['paraboloid.y'])] = c['paraboloid.f_xy']
            X.add(c['paraboloid.x'])
            Y.add(c['paraboloid.y'])
            
        X = sorted(list(X))
        Y = sorted(list(Y))
        
        xi,yi = p.meshgrid(X,Y)
        zi = []
        
        for x in X: 
            row = []
            for y in Y: 
                row.append(raw_data[(x,y)])
            zi.append(row)
        zi = array(zi)
        
        fig=p.figure()
        ax = p3.Axes3D(fig)
        ax.plot_surface(xi,yi,zi,rstride=1,cstride=1,cmap=cm.jet,linewidth=0)
        
        p.show()


.. figure:: doe.png
   :align: center
 
   A graph of the output from the execution of the DOE. 

   
If you would like to try this yourself, you can 
download the whole file :download:`here </../examples/openmdao.examples.simple/openmdao/examples/simple/doe.py>`.    



