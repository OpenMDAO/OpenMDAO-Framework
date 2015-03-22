.. index:: case_recorder case_iterator CSV db

Recording Your Inputs and Outputs
=====================================

In the previous example, we showed how to use a `DOEdriver` to vary parameters and plot the values from the DOE using the `case_input` and `case_output` variable trees. Another way to work with the DOE data is to record it to a file, and then make your plots in a
post processing step.

Let's re-run the DOE example from the previous tutorial: a Unifrom DOE of a Paraboloid component. The
only difference from before is that we will specify a :ref:`JSONCaseRecorder
<openmdao.lib.casehandlers.jsoncase.py>`, which will save off all the data from our run into a
file, named by the `filename` argument. The following `script </../examples/openmdao.examples.simple/openmdao/examples/simple/case_recorders.py>` will run the DOE for you, and should result
in a file being created called `doe.json`.

.. note::

    OpenMDAO also constains a :ref:`BSONCaseRecorder <openmdao.lib.casehandlers.jsoncase.py>` recorder
    which records the data in a more compact format.

.. testcode:: case_recorders

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):
            self.add('paraboloid', Paraboloid())

            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(1000)

            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)

            self.driver.add_response('paraboloid.f_xy')

            self.recorders = [JSONCaseRecorder(out='doe.json')]

    if __name__ == "__main__":
        #-----------------------------
        # Run analysis
        #-----------------------------
        import os
        if os.path.exists('doe.json'):
            os.remove('doe.json')

        analysis = Analysis()

        analysis.run()


Once you have the `doe.json` file, then you can start to set up some post processing scripts to
let you interpret your data. The next script just prints the data to your screen.

.. testsetup:: case_recorders_post_processing

    from openmdao.main.api import Assembly, Component
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import Uniform

    from openmdao.examples.simple.paraboloid import Paraboloid

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    class Analysis(Assembly):

        def configure(self):
            self.add('paraboloid', Paraboloid())

            self.add('driver', DOEdriver())
            self.driver.DOEgenerator = Uniform(1000)

            self.driver.add_parameter('paraboloid.x', low=-50, high=50)
            self.driver.add_parameter('paraboloid.y', low=-50, high=50)

            self.driver.add_response('paraboloid.f_xy')

            self.recorders = [JSONCaseRecorder(filename='doe.json')]

    if __name__ == "__main__":
        #-----------------------------
        # Run analysis
        #-----------------------------
        import os
        if os.path.exists('doe.json'):
            os.remove('doe.json')

        analysis = Analysis()

        analysis.run()

::

    from openmdao.lib.casehandlers.api import CaseDataset

    #----------------------------------------------------
    # Print out history of our objective for inspection
    #----------------------------------------------------
    case_dataset = CaseDataset('doe.json', 'json')
    data = case_dataset.data.by_case().fetch()

    for case in data:
        print "x: %f, y:%f, f_xy:%s"%(case['paraboloid.x'], case['paraboloid.y'], case['paraboloid.f_xy'])

To open the data file, we use a :ref:`CaseDataset <openmdao.lib.casehandlers.query.py>` object. The arguments
for creating a `CaseDataset` object are the file name and file type. `CaseDataset` objects have a variety
of methods for controlling what information is read from the file. In this example, we use `by_case`
to specify that the data should be returned as a list of dictionaries, where each item of the list is a single case and variables can be accessed by row. The `fetch` executes the query to read the data into memory.

In the DOE tutorial, we showed how to generate a 3D surface plot using the `case_input` and `case_output` variable trees of a `DOEdriver`. Below is an example of generating the same plot using `CaseDataset` objects.
Retrieve the data using a `CaseDataset` object. We use `by_variable` to arrange the data by variable, rather than
by case order. Notice that we're using the exact same data file, without re-running to get it again.

::

    import time

    from mpl_toolkits.mplot3d import Axes3D
    from matplotlib import cm
    from matplotlib import pyplot as p

    from openmdao.lib.casehandlers.api import CaseDataset

    case_dataset = CaseDataset('doe.json', 'json')
    data = case_dataset.data.by_variable().fetch()
    x    = data['paraboloid.x']
    y    = data['paraboloid.y']
    f_xy    = data['paraboloid.f_xy']

    p.ion()
    fig = p.figure()
    ax = Axes3D(fig)

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
        time.sleep(.005) #slow things down so you can see the changes

    p.ioff()

OpenMDAO has convenience functions for two common post processing steps: writing data to a CSV file or printing out detailed information about cases. These functions, :ref:`caseset_query_to_csv <openmdao.lib.casehandlers.csv_post_processor.py>` and :ref:`caseset_query_dump <openmdao.lib.casehandlers.dump_post_processor.py>`, both require the data returned from executing a query. `caseset_query_to_csv` allows for an additional argument to specify the name of the CSV file to be created.

::

    from openmdao.lib.casehandlers.api import CaseDataset
    from openmdao.lib.casehandlers.api import caseset_query_to_csv
    from openmdao.lib.casehandlers.api import caseset_query_dump

    case_dataset = CaseDataset('doe.json', 'json')
    data = case_dataset.data.by_case().fetch()

    caseset_query_to_csv(data, filename='doe.csv')
    caseset_query_dump(data)


By default OpenMDAO will record all variables in the model.  This can get to be a lot
of data and the associated file can be quite large.  You can change the default behavior
by modifying the ``recording_options`` variable tree in the top level assembly.  There
are three options:

============================  =======   ===============================================
Option                        Default   Description
============================  =======   ===============================================
``save_problem_formulation``  True      Save parameters, objectives, constraints, etc.
``includes``                  ['*']     Variables to include
``excludes``                  [ ]       Variables to exclude (processed after includes)
============================  =======   ===============================================

Also, if you want to reduce
the data processed for a specific post processing scenario you can write out
a new file based on cases and/or variables specified in a query by replacing
`fetch()` with `write(filename)`.  You can optionally specify a format for the
new file (``json`` or ``bson``), so this facility can also be used for changing
the format of an existing case dataset file.

