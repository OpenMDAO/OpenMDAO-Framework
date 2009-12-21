.. index:: geometry_details

Geometry Interfaces in OpenMDAO (Working Document)
--------------------------------------------------

.. figure:: ../images/arch-doc/top_level.png
   :align: center

   An OpenMDAO process model that integrates CFD & FEM based on a common
   geometry model into an optimization problem

Geometry Manipulation Interface
===============================

OpenMDAO Level
______________

**(Constructor)**

The constructor, instantiates the geometry manipulator as an OpenMDAO
component. There are several specific things that have to be done once this
component is instantiated, but before it can be used in a process model:

* Instantiate the Geometry object
* Connect to the modeller
* Load the geometry
* Load the feature tree
* Query for list of all available parameters and suppression states
* Create OpenMDAO variable for each parameter using the appropriate traits
* Create OpenMDAO variable for each suppressible feature

**(Setting Parameters and Suppression States)**

Parameters are set the same way as any other OpenMDAO input variable. For
example, consider a cylinder with two parameters: radius and height. If we 
have a geometry manipulator called geo_manipulator, the geometry parameters
can be directly set:

.. testsetup:: parameter_interface

	from openmdao.main.api import Component
	from openmdao.main.api import Assembly
	from openmdao.lib.drivers.conmindriver import CONMINdriver
	from openmdao.lib.traits.unitsfloat import UnitsFloat
	from enthought.traits.api import Bool
	
	class GeoMan(Component):

	    radius = UnitsFloat(7.0, low=1., high=20.0, iostatus='in', 
                     units='m',  desc='radius')		
	    height = UnitsFloat(10.0, low=1., high=50.0, iostatus='in', 
                     units='m',  desc='height')	
	    fillet1 = Bool(True, iostatus='in')	
	
	    def __init__(self, directory=''):
	        """ Creates a new Vehicle Assembly object """

	        super(GeoMan, self).__init__(directory)
	
	class TLA(Assembly):
    
	    def __init__(self, directory=''):
        
	        super(TLA, self).__init__(directory)

	        # Create GeoMan component instances
        	self.add_container('geo_manipulator', GeoMan())

	        # Create CONMIN Optimizer instance
        	self.add_container('driver', CONMINdriver())

	self = TLA()
	geo_manipulator = GeoMan()

.. testcode:: parameter_interface

	geo_manipulator.radius = 5
	geo_manipulator.height = 15

More often, the geometry's parameters will be set as part of an optimization
problem, so they can be declared as design variables when an optimizer is added
to a model.
      
.. testcode:: parameter_interface

	# CONMIN Design Variables 
	self.driver.design_vars = ['geo_manipulator.radius', 
                                   'geo_manipulator.height']
				 
	self.driver.lower_bounds = [3.0, 6.5]
	self.driver.upper_bounds = [12, 25]

Here, self is the top level assembly that contains an optimizer, the geometry
manipulator, and some kind of process model such as the one pictured above.

The suppression of features (suppression states) can also be treated the same way
at the component level. Here, the Boolean variable fillet1 is set to False to
suppress the feature fillet1.

.. testcode:: parameter_interface

	geo_manipulator.fillet1 = False

**execute()**

Regenerates model if any parameter or suppression state changes. Raises an
exception if this process breaks associativity or causes incomplete
regeneration of the model.

Note that if no parameters or suppression states change, there is no reason to 
regenerate the geometry or to invalidate any reference to this geometry object,
which would trigger the execution of any components that depend on it (meshers
etc.) 

Note also that if the geometry is capable of providing analytical sensitivies
to the parameters, then these would be calculated here.

**save_to_egg()**

Saves current representation of the data model, including all parameters (both
name and value), suppression states (likewise), and tags.

**load()**

Loads representation of the data model, including all parameters, suppression 
states, and tags, from a saved egg.

**tag_volume(volume_label, tag_name, tag_description)**

**tag_surface(surface_label, tag_name, tag_description)**

**tag_curve(curve_label, tag_name, tag_description)**

**tag_point(point_label, tag_name, tag_description)**

Associates a geometric entity with some metadata. This is useful for marking
an entity for later use by an analysis tool (e.g. marking loads and boundary
conditions.) The most straightforward way to implement the tags' storage would
be to create each tag as an OpenMDAO variable, accessed via its tag_name.

**(Visualization)**

The requirements call for the ability to visualize the geometry. No interface
for this has been worked out. The user also needs to be able to view the 
feature tree, in order to choose parameters as design variables.

Python Component Level
______________________

At a lower level, the geometry manipulation component needs a set of functions
to interact with the geometry object, making the above interface possible at 
the OpenMDAO level. These functions are used in the geometry manipulator, and
will not be commonly seen or used by users who build or run models.

**status = initialize(modeler)**

Performs any necessary action prior to loading a model. If the geometry is to
be loaded and run in another process (or another server), some additional
things might need to be passed. A status is returned indicating any error
that occurs.

**load_model(filename)**

Loads the geometry from *filename* into the model.

**parameters = get_parameters()**

Returns a list containing a unique label and a value for all parameters in the
model.

**suppression_states = get_suppression_states()**

Returns a list containing a unique label and a boolean for all suppression
states in the model.

**feature_tree = get_feature_tree()**

Returns a data structure containing the feature tree. The format of this data
structure is not currently known. This info can be used to give the component
user a way to "visualize" the parametric model. Note that technically the
parameters and suppression_states can also be extracted from here instead of
using the given functions above.

**set_parameter(id, value)**

Sets a new value for a parameter in the model. Model must be rebuilt for the
effect of the new parameter to be realized.

**set_suppression_state(id, boolean)**

Sets a new value for a suppression state in the model. Model must be rebuilt
for the effect of the new suppression state to be realized.

**status = rebuild_model()**

Rebuilds the model based on the current parameters and suppression states. A
status is returned that indicates whether the regeneration was successful.

**terminate()**

Shuts down the geometry modeler, and performs any necessary cleanup.

The Geometry Object and its Query Interface
===========================================

General Query
_____________

Mesh Generation
_______________


The Mesh Object
===============

Use Cases
=========
