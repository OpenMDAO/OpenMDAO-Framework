.. index:: geometry

.. _`doe-geometry`:

Design of Experiments with Geometry
===================================

In the overview tutorial for geometry support in OpenMDAO, a very simple example was given
of including geometry into OpenMDAO and displaying it. This example
will demonstrate the use of a geometry object as part of a design
of experiments (DOE). This tutorial will dynamically show how the
design of a simple jet engine nozzle can be varied as part of a DOE.

.. note:: This tutorial will be done in the OpenMDAO GUI. In addition, a script
  version of this tutorial is given in the examples directory of the 
  OpenMDAO distribution, specifically 
  ``examples/openmdao.examples.nozzle_geometry_doe/openmdao/examples/nozzle_geometry_doe``.

Start by creating a new clean project in the GUI. We'll name it `DOE Geometry
Tutorial`.  As in the overview tutorial, first, you should
create an instance of an assembly to work in. On the right-hand side, there is a
Library tab with a text box at the top. In the box type "assemb" and hit enter.
This will filter down the whole library so you can find things easier. Drag the
``Assembly`` and drop it into the workspace. Name it `top.`

.. figure:: library_assembly.png
   :align: center

   Adding required files to the project

There are three files we need to add to the project for our specific
problem. Click the Files tab on the left. From the File menu, select Add Files. 
Navigate to the 
``examples/openmdao.examples.nozzle_geometry_doe/openmdao/examples/nozzle_geometry_doe`` directory.
Add the files: cowl.stl, plug.stl and simple_nozzle.py.

   Setting up the geometry component

So now go back to the Library and change the filter text to "geom" and hit enter. 
Drag the ``GeomComponent`` instance and drop it into the `top` assembly.
Name it `gc` when prompted.  Whenever you want to work with geometry, 
you will always start with ``GeomComponent``. 

Double click on the GeomComponent in the Dataflow. In the editor window that 
appears, click the Slots tab. Drag from the Library the item, PlugNozzleGeometry and
drop it into the parametric_geometry slot.

Click the Outputs tab. In the Value column for the geom_out output, there should be a button
labeled View Geom. Click that button and a new window should apprear showing the 
nozzle geometry. 

Click the Inputs tab. Change the value of auto_run to be True. This causes the
GeomComponent to execute whenever any input values change. Now we can edit some of the 
input values for this geometry and see the geometry be updated in the OpenMDAO 
Geometry Viewer window. This GeomComponent has a series of deformation control point arrays 
as inputs. For example, the plug.R array, which contains 9 elements, controls the radius
of the plug, which is the pointy object. Let's try changing the value of the last element of this 
array to see how it affects the geometry. In the Name column for the plug input, click that cell to 
expand its sub elements, R and X. Click on the Value cell for R. An array editor window should appear. 
Change the value of the last element to be 5.0 and click the Submit changes button. The 
end of the plug tip should become much larger than before.

   DOE Driver Setup

From the Library, drag the ``DOEdriver`` item and drop it on the ``driver`` dataflow item in
the Workflow. A confirmation dialog will ask if you really want to replace the existing driver
with the DOEdriver. Click OK. 

Open up the editor for the DOEdriver by double clicking it in the Dataflow. Click on the Slots tab.
From the Library, drag and drop the FullFactorial item on to the DOEgenerator slot. A dialog window
will appear asking for the number of levels for this DOE. Enter 10 and click Ok.

Click the Parameters tab. Click the Add Parameter button in the lower left. Enter in these values:

   Target: gc.plug.R[8]
   Low: 0.0
   High: 5.0

and click Ok.

We should now be able to run the analysis! Right click on the Assembly, a, and select Run from the menu. 
You should notice the geometry change shape. But the analysis runs too quickly to see how the 
geometry look for each of the ten designs. 

To slow things down a bit, we will add a component to the workflow that does nothing but create a time
delay. From the Library, drag into the Assembly a SleepComp item. Name it "sc". Double click it to open its
editor window. Change the input value "sleep_time" to be 1.0. Close the editor window. Now open the 
editor window for the DOEdriver and click on the Workflow tab. Arrange your windows so that you can see the main 
Dataflow window and the Workflow tab of the DOEdriver. Drag and drop the SleepComp, sc, from the main Dataflow
to the grey box below and to the right of the DOEdriver in the DOEdriver Workflow tab. This SleepComp should
be side by side with the GeomComponent. Now the DOEdriver will run both the GeomComponent and the SleepComponent
for each design iteration. 

Now try running the analysis again. With the one second delay between each analysis, you can now easily see
the geometry change shape.



