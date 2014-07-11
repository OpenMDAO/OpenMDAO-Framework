.. index:: DOE
.. _`DOE_paraboloid`:

Building a Model - Executing a Design of Experiment (DOE)
=========================================================

Let's say you're not interested in optimization, but instead you're much more interested
in design space exploration. In that case you would want to use some kind of a Design
Of Experiment (DOE). There are few different kinds of DOEs out there. Some of the most
popular are:

  .. Need hyperlinks for explanations of each DOE
  #. Full Factorial 
  #. Random Uniform
  #. Latin Hypercube
  #. Central Composite

OpenMDAO provides options to use all of these in our :ref:`standard library
<openmdao.lib.doegenerators.api>`. If none of those meet your needs, you can also
write your own DOEgenerator class to expand OpenMDAO capabilities. (We'll leave that for a different
tutorial). For now, let's assume you wanted to use a Full Factorial DOE, with 10 levels for each
variable. Create a file called ``doe.py`` and copy the following into it:

.. testcode:: simple_model_doe

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):
            self.add('parabloid', Paraboloid)

            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(1000)

            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)

            self.driver.add_response('paraboloid.f_xy')

            self.recorders = [JSONCaseRecorder('doe.json')]

Or download our version of the file
:download:`here </../examples/openmdao.examples.simple/openmdao/examples/simple/doe.py>`.

To run a DOE we use the :ref:`DOEdriver <DOEdriver.py>`, which serves as the
driver any time you want to run any kind of DOE. To specify the particular type of DOE, you set the ``DOEgenerator``
attribute. In this case we used :ref:`Uniform <Uniform.py>`, but any of the DOEgenerators
would work.

You can see that this code does not look a whole lot different from the code in the previous
tutorials  on :ref:`unconstrained <using-CONMIN>` and :ref:`constrained <constrained-optimization>`
optimizations. We're still using  the same Paraboloid component as before. Also, just like before,
we use the ``add_parameter`` method to specify what inputs should be varied by the DOE. Since we
specified the low and high to be -50 and 50 respectively, the Uniform DOE
generator will choose 1000 uniformly spaced points. 

One new thing in this example is the use of a case recorder. Each case in a given DOE results in a set of
inputs being set into your model; then the model gets run, and some outputs are calculated. Obviously you
want to record the results of this process for each case in the DOE. You use a :ref:`CaseRecorder
<openmdao.lib.casehandler.api.py>` for that.  The CaseRecorder's job is to store the information from each
case in some fashion. In this example  we used a :ref:`JSONCaseRecorder
<openmdao.lib.casehandlers.JSONCaseRecorder.py>` which is written to a file.

.. testsetup:: simple_model_doe_pieces

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):
            self.add('parabloid', Paraboloid)

            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(1000)

            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)

            self.driver.add_response('paraboloid.f_xy')


    self = Analysis()

.. testcode:: simple_model_doe_pieces

            #Simple recorder which stores the cases in a JSON file
            self.recorders = [JSONCaseRecorder('doe.json'), ]

You can add as many CaseRecorders to that list as you want, and each one will record every case separately. This
enables you to save information to more than one place at the same time.

The last new thing to look at is where we specify some extra variables to be saved off for each case. The DOEdriver
automatically saves all the variables that were specified as parameters in every case. That way, you will always
know exactly what variable values were used for each case. But, of course, the inputs are just half the story. You will
also want to store relevant outputs from each case. This is what the ``add_response`` method is for, on the DOEdriver.
You could add any variables you want to track as responses, but here we have only the one output from
paraboloid.

.. ::

           self.driver.add_response('paraboloid.f_xy')



To run this analysis, you would do the following:

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

        cds = CaseDataset('doe.json', 'json')
        data = cds.data.by_variable().fetch()

        x, y, f_xy = np.array(data['paraboloid.x']), np.array(data['paraboloid.y']), np.array(data['paraboloid.f_xy'])

        for i in range(0, len(x)):
            print "x: {} y: {} f(x, y): {}".format(x[i], y[i], f_xy[i])

The only new stuff here is the bit at the end where we loop over all the cases that were run. To keep
things simple, we just spit out the data to the screen. But the key thing to recognize here is  how you
work with cases. Creating a `CaseDataset` object allows you to open the data from the 'doe.json' file. The `data` attribute of `CaseDataset` objects can be used to specify what information to read from the open file. Using this feature, you could read in a specific case, a range of cases, specific varaibles, combinations of the above, and more. In the above example, we use `by_variable()` to only read the recorded parameters and responses and `fetch()` to return the data.  

Documentation for the CaseDataset query API can be read here: <../srcdocs/packages/openmdao.lib.html#openmdao.lib.casehandlers.query.CaseDataset>

..postprocessing section

Most often, you'll want to do some postprocessing using the data read using the `CaseDataset` object like plotting a graph, writing the data to a CSV file or just printing the data to the console. 

Below is an example that uses matplotlib and the parameters and responses read from a `CaseDataset` to generate a interactive, 3D surface plot.

.. code-block:: python

    import time

    import numpy as np 

    from mpl_toolkits.mplot3d import Axes3D
    from matplotlib import cm
    from matplotlib import pyplot as p

    from openmdao.lib.casehandlers.api import CaseDataset

    cds = CaseDataset('doe.json', 'json')
    #cds = CaseDataset('doe.bson', 'bson')

    data = cds.data.by_variable().fetch()

    X,Y,Z = np.array(data['paraboloid.x']), np.array(data['paraboloid.y']), np.array(data['paraboloid.f_xy'])

    p.ion()
    fig = p.figure()
    ax = Axes3D(fig)
    #ax = p.gca()

    slices = range(3,len(X))[::10]

    freq = 10/float(len(slices))

    for i in slices: 
        ax.clear()
        ax.set_xlim(-60,60)
        ax.set_ylim(-60,60)
        ax.set_zlim(-1000,6000)
        ax.grid(False)

        #3d surface plot
        ax.plot_trisurf(X[:i],Y[:i],Z[:i], cmap=cm.jet, linewidth=0.2)

        #ax.tricontourf(X[:i],Y[:i],Z[:i], np.linspace(-1000,6000,100), cmap=cm.jet, linewidth=0.2, )

        p.draw()
        time.sleep(freq)


    p.ioff()

.. figure:: doe.png
   :align: center
   :alt: Multi-colored graph of output from the execution of the DOE


   A Graph of the Output from the Execution of the DOE

For writing data to a CSV file, OpenMDAO provides a built in `CSVPostProcessor` that takes the data returned by a `CaseDataset` and writes it to a CSV file. 

.. code-block python

    from openmdao.lib.casehandlers.api import CaseDataSet
    from openmdao.lib.postprocessors.api import CSVPostProcessor

    cds = CaseDataset('doe.json', 'json')
    data = cds.by_variable().fetch()
    CSVPostProcessor(data, 'doe.csv')

OpenMDAO also offers a `DumpCasePostProcessor` that prints the data to the console. Using this is almost identical to the `CSVPostProcessor`.

.. code-block python

    from openmdao.lib.casehandlers.api import CaseDataSet
    from openmdao.lib.postprocessors.api import DumpCasePostProcessor

    cds = CaseDataset('doe.json', 'json')
    data = cds.by_variable().fetch()
    DumpCasePostProcessor(data)

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


..

  Since DOEdriver is derived from :ref:`CaseIteratorDriver <caseiterdriver.py>`,
  it's possible to run the various cases concurrently.  If evaluating a case
  takes considerable time and you have a multiprocessor machine, setting
  ``analysis.driver.sequential`` to False will cause the cases to be evaluated
  concurrently, based on available resources, which will usually be quicker.

