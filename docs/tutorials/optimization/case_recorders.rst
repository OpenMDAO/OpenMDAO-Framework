.. index:: case_recorder case_iterator CSV db

Recording Your Inputs and Outputs
=====================================

In the previous example, we showed how to use a `DOEdriver` to vary parameters and reference the recorder values for parameters and responses using the `case_input` and `case_output` variable trees. Another way to record information is with `CaseRecorders`. By default, `CaseRecorders` will record as much information about a case as possible.

OpenMDAO contains the following case recorders:

==================== ====================================================================
Name                  Output Type
==================== ====================================================================
``JSONCaseRecorder``  JSON file, defaults to cases.json
-------------------- --------------------------------------------------------------------
``BSONCaseRecorder``  BSON file, defaults to cases.bson
==================== ====================================================================

The recorders are interchangeable, so you can use any of them in the top-level
assembly's ``recorders`` list. Since ``recoders`` is a list, it allows for recording
cases in multiple formats at once. All assemblies contain a ``recorders`` variable,
however only the top-level assembly's recorders are used. 

Although a `CaseRecorder` will record as much information by default, the
top-level assembly's ``includes`` and ``excludes`` variables can be used to 
limit which output varaibles are recorded. These variables hold lists of
patterns of variables to be included or excluded from the data recorded.
By default ``includes`` is ``['*']``, including everything.
By default ``excludes`` is ``[]``, excluding nothing.

Of these recorders, the JSONCaseRecorder and BSONCaseRecorder are the most useful
for passing data to other applications, such as an external post-processing
tool. The BSONCaseRecorder will record everything JSONCaseRecorder does, but in a
more compact form. To perform JSON and BSON case recorder post-processing,
:ref:`CaseDataset <openmdao.lib.casehandlers.query.py>` is typically used. We'll 
introduce `CaseDataset` later in this tutorial.

At the end of the top-level assembly's ``run()``, all case recorders are closed.
Each type of recorder defines its own implementation of ``close()``,
but the general idea is to specify that the recording process is complete.
For example, the JSONCaseRecorder will close the file being written so that
other applications can use it. Note that in some cases you cannot record to
a closed recorder.

Let's consider our simple unconstrained optimization of the Paraboloid component with SLSQP. We would like to print out the convergence history of the variables, objective, and constraint into a JSON file.

.. literalinclude:: ../../../examples/openmdao.examples.simple/openmdao/examples/simple/case_recorders.py

Here, we set ``opt_problem.recorders`` to be a list that contains the JSON recorder. The JSONCaseRecorder takes a filename as an argument.
These files will be written in the directory where you execute this Python file. To open the file, we use a `CaseDataset` object. The arguments for creating a `CaseDataset` object are the file name and file type. `CaseDataset` objects have a variety of methods for controlling what information is read from the file. In this example, we use `by_case` to specify that the data should be ordered by case number and`fecth` to read the data into memory. For simplicity, we just print each case to the console.

`CaseDataset` was designed with postprocessing in mind. In the previous example, we showed how to generate a 3D surface plot using the `case_input` and `case_output` variable trees of a `DOEdriver`. Below is an example of generating the same plot using `CaseRecorders` and `CaseDataset` objects instead. 

First, we setup a DOE identical to the one in the previous tutorial. What's new is that we add a `JSONCaseRecorder` to the `Analysis` assembly.

.. testcode:: simple_doe_case_recorder

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

Next, we create a 3D surface plot but we retrive the data using a `CaseDataset` object. We use `by_variable` to arrange the data by variable name rather than case numbers and `fetch()` to return the data as a dictionary with variable names as keys. 

.. testcode:: simple_doe_case_recorder

    if __name__ == "__main__":

        from mpl_toolkits.mplot3d import Axes3D
        from matplotlib import cm
        from matplotlib import pyplot as p

        from openmdao.lib.casehandlers.api import CaseDataset
        analysis = Analysis()

        analysis.run()

        case_dataset = CaseDataSet('doe.json', 'json')
        data = case_dataset.by_variable().fetch()
        x    = data['paraboloid.x']
        y    = data['paraboloid.y']
        f_xy = data['paraboloid.f_xy']

        p.ion()
        fig = p.figure()
        ax = Axes3D(fig)
        #ax = p.gca()

        slices = range(3,len(x))[::10]

        freq = 10/float(len(slices))

        for i in slices: 
            ax.clear()
            ax.set_xlim(-60,60)
            ax.set_ylim(-60,60)
            ax.set_zlim(-1000,6000)
            ax.grid(False)

            #3d surface plot
            ax.plot_trisurf(x[:i],y[:i],f_xy[:i], cmap=cm.jet, linewidth=0.2)

            p.draw()
            time.sleep(freq)


        p.ioff()

Because the creation of CSV files or printing detailed results to a console are common, OpenMDAO provides a `CSVPostProcessor` and `DumpCasePostProcessor`.

.. testsetup:: simple_doe_caserecorder
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


