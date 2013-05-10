.. index:: geometry

.. _`working with geometry`:

Basic Geometry
===========================

OpenMDAO has full support for integrating geometry into a modeling process. 
Before we start working with geometry, lets understand how OpenMDAO was 
designed to interact with geometry. The ``GeomComponent`` class, from 
``openmdao.lib.components.api``,  allows you to plug in the parametric 
geometry engine of your choice and specify a particular geometry model 
to work with. 

.. note:: We're going to work this tutorial in the OpenMDAO GUI. When 
  you're working with geometry, it's nice to be able to see what you're 
  building and the GUI has a built in viewer that makes this easy. If you 
  don't know how to use the GUI, check out the instructions in the  
  :ref:`GUI-OpenMDAO` section to get started.

  You could also build a script file that does the same thing we're 
  doing in the GUI. At the end of the tutorial, we'll show you what 
  that look like. You just won't be able to visualize the results. 


Start by creating a new clean project in the GUI. We'll name it "Geometry Tutorial". 
You'll be greated by a totally blank project page. First 
you should create an instance of an assembly to work in. On the right 
hand side there is a library tab, with a text box at the top. In the box type 
"assemb" and hit enter. This will filter the whole library down so you can 
find things easier. Drag the ``Assembly`` and drop it into the workspace. Name 
it "top"

.. figure:: library_assembly.png
   :align: center

   Creating the initial assembly

Now go back to the library and change the filter text to "geom" and hit enter. 
Drag the ``GeomComponent`` instance and drop it into the "top" assembly.
Name it "geom", when prompted.  Whenever you want to work with geometry, 
you will always start with ``GeomComponent``. No actual geometry has been 
loaded yet, so the "geom" instance is pretty boring. If you double click 
on it, a component editor window will come up with nothing much in it. 

.. note:: Depending on the plugins you have installed and the names of classes 
  you've defined in your project (if any), what shows up when you filter the
  library might be slightly different than what we have here.


So lets add a very simple geometry model of a box into the mix. First, switch 
over to the slots tab in the editor window for "geom". Find "BoxParametricGeometry"
in the library pane, and drag it into the ``parametric_geometry`` slot. 
When you're done, it should look like this

.. figure:: box_geom.png
   :align: center

   Dropping BoxParametricGeometry into the slot

Want to see what your newly added geometry model of a box looks like? Click 
on the outputs tab of the editor window, and then click the ``View Geom``
button next to the "geom_out" variable. This will bring up the 3D viewer in a separate 
window. 


.. figure:: box_viewer_1.png
   :align: center

   Intial box geometry

This *VERY* simple model lets you control the height of the box. 
That is its only parameter. So switch to the inputs tab in the editor 
window, and you should see the "height" variable. Set it to a new value, 
like 10. Now, go back to the 3D editor window. Nothing changed! 
``GeomComponent`` is just like any other OpenMDAO Component, it needs to be 
run in order for the new outputs to be calculated with the new input values. 
So right click on "geom" and select ``run``. Now the viewer will automatically
update with the new geometry and you can see how it got much taller. 

.. figure:: box_viewer_2.png
   :align: center

   Box with height=10


Next Steps
-----------------
That is pretty much it for the basics of working with geometry in OpenMDAO. 
Obviously for any real work, you'd want to use a more complex geometry model. 
Our next tutorial will cover working with a more substantial geometry model, but 
to do that we'll have to install a plugin that has a more powerful geometry engine. 

