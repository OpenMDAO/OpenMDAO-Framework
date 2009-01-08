.. _glossary:


Glossary
--------


.. glossary::


      **API**
	Application Programming Interface. A set of functions that can be called from
	an application program to access features of another program.


      **Assembly**
	The Assembly class is the primary building block of the system of 
	systems aspect of this architecture. Each Assembly has a Workflow and a 
	Driver, and acts as a container for Components. An Assembly is also a
	Component, so hierarchical structures of Assemblies can be created.


      **Case**
	A collection of input names and values, and names of outputs to be stored
	along with the inputs after the process model runs.


      **CaseIterator**
	An iterator that contains a number of Cases. This iterator could be tied
	to a simple file, a database, or some object that generates cases dynamically.

      **CommandLineWrapper**
	A component that runs a separate executable program via a system call.


      **Component**
	A Container that is *runnable*, in addition to supporting several other
	framework functions, such as checkpoint/restart, stop, and invoke.


      **Components**
      	See "Component" above.
	
      **CAD**
	Computer Aided Design. An automated system for the design, drafting, and
	display of graphically oriented information. 


      **CAPRI**
	Computational Analysis Programming Interface. CAPRI is a CAD vendor-neutral
	programming interface that aids in acquiring geometry data directly from CAD
	files.

      **CFD**
	Computational Fluid Dynamics. A branch of fluid mechanics that uses numerical
	methods and algorithms to solve and analyze problems that involve fluid flows. 

      **Container**
	A container of Variables and other Containers.
	The base class of all objects within the framework that 
	support user access to input and output Variables. 

      **Driver**
	A Driver's function is to iterate over a Workflow until some
	condition is met. The simplest Driver executes a Workflow only once.
	Other Drivers, such as Optimizers or ParameterStudies, would execute
	the Workflow a variable number of times based on their inputs.  

      **egg**
	A zip file with a specified internal directory structure that
	contains a python package or module. It is similar to a jar file in java. For
	more information on eggs, see PythonEggs_.

      .. _PythonEggs: http://peak.telecommunity.com/DevCenter/PythonEggs

      **eggs**
	See "egg" above. 


      **Factory**
	An object that knows how to create objects of specific types.  


      **FactoryManager**
	All requests to create any type of Container must go through the
	FactoryManager object. Multiple Factory derived objects can be registered with
	the FactoryManager to allow creation of objects in various ways, e.g., locally
	via import and remotely via an ObjServer.

	
      **GA**
      	Genetic algorithm
	
	
      **GUI**
	Graphical User Interface. A computer operating system that is based upon icons
	and visual relationships rather than text.
	
      
      **GeomObject**
	A Component representing an object having physical dimensions and
	shape, with parameters that can be manipulated by other Components or 
	Drivers to modify its properties.



      **IPC**
	Interprocess communication


      **LAN** 
	Local Area Network. An interconnection of computers that are in relatively
	close proximity to each other, such as within a building. 


      **MDAO** 
	Multi-disciplinary Analysis & Optimization


      **model**
        A hierarchical structure with an :term:`Assembly` at its root.
       
      
      **NOSA**
        NASA Open Source Agreement. A software license approved by the Open
	Source Initiative (:term:`OSI`). The National Aeronautics and Space
	Administration (NASA) releases some software under this license.
      
      
      **ObjServer**
	A process that allows remote connections to a Component or Container
	object.  An ObjServer can represent an entire model if its top-level object
	is an Assembly, or it can represent a single object if its top-level object
	is some other type of Component.


      **ObjServerFactory**
	A process that spawns a child process (ObjServer) encapsulating an 
	instance of a particular Component or Container type.


      **OML**
	Outer Mold Line


      **OS**
	Open Source
	

      **OSI** 
	`Open Source Initiative Initiative <http://www.opensource.org>`_.


      **PBS**
	Portable Batch System (PBS) is a queueing system. Jobs are submitted to the
	queue that reflects the resources needed, and a scheduler decides which ones
	to run when nodes become available. These decisions are made on the basis of
	length of run, how long a job has been waiting, and fair sharing of resources
	among different users.


      **plug-ins**
	Python objects that can be used to extend the functionality of the framework.

           

      **PID**
	Process id


      **ResourceAllocator**
	An object responsible for allocating CPU/disk resources for a particular
	host, cluster, load balancer, etc.


      **ResourceAllocatorManager**
	An object that manages a number of child objects that are responsible for
	allocating CPU and disk resources, either locally or for a particular
	cluster or a particular load balancer.  The RAM can be queried to determine
	the current allocation of resources for the given system. This includes
	host and PID information for all Components.


      **ResourceDescription**
	An object containing information defining system attributes required to select a 
	viable place to run a code.


      **Socket**
	A placeholder for a plug-in within a :term:`Component`.


      **SocketList**
	A list that holds only objects that provide a particular interface.


      **Variable**
	A wrapper for data passed between framework components. Variables can contain
	a value, a default value, optional min/max values, and units. Variables can
	perform their own validation when being assigned to another Variable. To
	extend the framework by creating a new data type to be passed between
	Components, a developer would create a new python class that inherits from
	Variable. This Variable would have its own validation routine and could also
	have a custom viewer/editor.

      **VSP**
	Vehicle Sketch Pad. VSP is a rapid geometry creation tool used to create a
	wide range of aerospace vehicles from a combination of predefined components.
	It provides highly interactive sketching of concepts with immediate visual
	feedback.


      **Workflow**
	A Workflow controls the execution order of a group of Components. The Workflow
	class supports simple sequential execution. Other classes inheriting from 
	Workflow will support different execution schemes, e.g., concurrent execution,
	conditional execution, and looping of various types.


