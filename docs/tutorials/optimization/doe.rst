.. index:: DOE
.. _`DOE_paraboloid`:

Building a Model - Executing a Design of Experiment (DOE)
=========================================================

Let's say you're not interested in optimization, but instead you're much more interested
in design space exploration. In that case you would want to use some kind of a Design
Of Experiment (DOE). There are few different kinds of DOEs out there. Some of the most
popular are:

  #. Full Factorial 
  #. Random Uniform
  #. Latin Hypercube
  #. Central Composite
  #. CSV

OpenMDAO provides options to use all of these in our :ref:`standard library
<openmdao.lib.doegenerators.api>`. If 1-4 don't meet your needs, you can input your own using
the `CSVFile` generator, which we cover at the end of the tutorial. 
For now, let's assume you wanted to use a Uniform DOE, with 1000 uniformly distributed
data points. Create a file called ``doe.py`` and copy the following into it:

.. testcode:: simple_model_doe

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):
            self.add('paraboloid', Paraboloid)

            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(1000)

            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)

            self.driver.add_response('paraboloid.f_xy')

Or download our version of the file
:download:`here </../examples/openmdao.examples.simple/openmdao/examples/simple/doe.py>`.

To run a DOE we use the :ref:`DOEdriver <DOEdriver.py>`. To specify the particular type of DOE, you set the ``DOEgenerator`` attribute. In this case we used :ref:`Uniform <openmdao.lib.doegenerators.uniform.py>`, but any of the DOEgenerators would work.

You can see that this code does not look a whole lot different from the code in the previous
tutorials  on :ref:`unconstrained <using-CONMIN>` and :ref:`constrained <constrained-optimization>`
optimizations. We're still using  the same Paraboloid component as before. Also, just like before,
we use the ``add_parameter`` method to specify what inputs should be varied by the DOE. Using
the low and high to be -50 and 50 respectively, the Uniform DOE
generator will choose 1000 random, uniformly distributed points between -50 and 50.

In the example above, using `add_parameter` and `add_response` not only tells the `DOEdriver` what variables
to vary, but also tells the driver what information to store in the `case_input` and `case_output`
variable trees. 

For example, to run this analysis and use the `case_input` and `case_output` variable trees to print the recorded values, you would do the following:

.. testsetup:: simple_model_doe_run

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial, Uniform
    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):

            self.add('paraboloid', Paraboloid())

            self.add('driver', DOEdriver())
            #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
            #self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
            self.driver.DOEgenerator = Uniform(1000) 

            #DOEdriver will automatically record the values of any parameters for each case
            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)
            #tell the DOEdriver to also record any other variables you want to know for each case
            self.driver.add_response('paraboloid.f_xy')

            self.recorders = [JSONCaseRecorder('doe.json'), BSONCaseRecorder('doe.bson')]

.. testcode:: simple_model_doe_run

    if __name__ == "__main__":

        import time

        analysis = Analysis()

        tt = time.time()
        analysis.run()

        print "Elapsed time: ", time.time()-tt, "seconds"

        x = analysis.driver.case_inputs.paraboloid.x
        y = analysis.driver.case_inputs.paraboloid.y
        f_xy = analysis.driver.case_outputs.paraboloid.f_xy

        for i in range(0, len(x)):
            print "x: {} y: {} f(x, y): {}".format(x[i], y[i], f_xy[i])

Alternatively, the `case_input` and `case_output` variable trees can be used to generate an interactive, 3D surface plot with matplotlib:

.. testcode:: simple_model_doe_run

    if __name__ == "__main__":
        import time

        from mpl_toolkits.mplot3d import Axes3D
        from matplotlib import cm
        from matplotlib import pyplot as p

        analysis = Analysis()

        analysis.run()

        x = analysis.driver.case_inputs.paraboloid.x
        y = analysis.driver.case_inputs.paraboloid.y
        f_xy = analysis.driver.case_outputs.paraboloid.f_xy

        p.ion()
        fig = p.figure()
        ax = Axes3D(fig)
        #ax = p.gca()

        slices = range(3,len(X))[::10]
        every_10 = range(3,len(x))[::10]


        for i in every_10: 
            ax.clear()
            ax.set_xlim(-60,60)
            ax.set_ylim(-60,60)
            ax.set_zlim(-1000,6000)
            ax.grid(False)

            #3d surface plot
            ax.plot_trisurf(x[:i],y[:i],f_xy[:i], cmap=cm.jet, linewidth=0.2)

            p.draw()
            time.sleep(.005) #slow it down so you can see the changes

        p.ioff()
        

.. raw:: html

   <video controls>
      <source src="../../_downloads/doe_parab.webm" type="video/mp4">
    Your browser does not support the video tag.
    </video>

You can download this movie :download:`here <doe_parab.webm>`.


At times it's necessary to rerun an analysis. This can be a problem if the
DOE generator used has a random component. To handle this, DOEdriver records
the normalized DOE values to a CSV file. This file can be read in later by
a :ref:`CSVFile <openmdao.lib.doegenerators.csvfile.py>` DOE generator.
The DOEdriver can then be configured to use this CSVFile generator to rerun
the cases previously generated.

.. testcode:: simple_model_doe_rerun

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import CSVFile, Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid


    class Analysis(Assembly):

        def configure(self):
            self.add('paraboloid', Paraboloid())
            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(num_samples=1000)
            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)
            self.driver.add_response('paraboloid.f_xy')
            self.driver.workflow.add('paraboloid')


    if __name__ == '__main__':

        analysis = Analysis()

        # Run original analysis.
        analysis.run()

        # Reconfigure driver to rerun previously generated cases.
        analysis.driver.DOEgenerator = CSVFile(analysis.driver.doe_filename)
        # Note that analysis.driver.doe_filename will give you the name of
        #   the csv file saved by the DOE driver.

        # No need to re-record cases (and it avoids overwriting them).
        analysis.driver.record_doe = False

        # Rerun analysis.
        analysis.run()

Since DOEdriver is derived from :ref:`CaseIteratorDriver <caseiterdriver.py>`,
it's possible to run the various cases concurrently.  If evaluating a case
takes considerable time and you have a multiprocessor machine, setting
``analysis.driver.sequential`` to False will cause the cases to be evaluated
concurrently, based on available resources, which will usually be quicker.

