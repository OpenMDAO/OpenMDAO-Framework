.. _glossary:


Glossary
--------


.. glossary::
      
      **API**
        Application Programming Interface. A set of functions that can be called from
        an application program to access features of another program.


      **Assembly**
        The Assembly class is the primary building block of the "system of 
        systems" aspect of OpenMDAO. Each assembly has a Workflow and a 
        Driver, and acts as a container for Components. An assembly is also a
        component, so hierarchical structures of assemblies can be created.
      
      
      **AXOD** 
        Axial-Flow Turbine Off-Design. A a computer code used for
	preliminary studies of the gas turbine system.
	

      **Beaker** 
        A Session and Caching library with :term:`WSGI` Middleware. 
	      
      
      **branch**
	An ordered series of revisions (see :term:`revision` below) that describes the
	history of a set of files. 	


      **CAD**
        Computer-Aided Design. An automated system for the design, drafting, and
        display of graphically oriented information. 

      
      **CAE**
        Computer-Aided Engineering. Using computers to design, analyze, and
        manufacture products and processes.


      **CAPRI**
        Computational Analysis Programming Interface. CAPRI is a CAD vendor-neutral
        programming interface that aids in acquiring geometry data directly from CAD
        files.

      **Case**
        An object containing a collection of input names and values, and names of outputs 
        to be stored along with the inputs after the process model runs.


      **CaseIterator**
        An object that iteratively supplies Case objects. This iterator could be tied
        to a simple file, a database, or some object that generates Case objects
        dynamically.


      **CFD**
        Computational Fluid Dynamics. A branch of fluid mechanics that uses numerical
        methods and algorithms to solve and analyze problems that involve fluid flows. 

      
      **CommandLineWrapper**
        A component that runs a separate executable program via a system call.


      **commit**
        To record a change. A change is committed to a developer's local
        repository.
	
      
      **Component**
        Container that is *runnable*; it also supports several other framework
        functions, such as checkpoint/restart, stop, and invoke. A component has
	input and output attributes and can perform some sort of calculation when
	it is executed. 


      **components**
      	See :term:`Component` above.
	
	
      **CONMIN**
        An interface to the NASTRAN/NASA CONMIN optimization program.
      
      
      **Container**
        A container of Variables and other Containers. The base class of all objects
        within the framework that  support user access to input and output Variables. 

      
      **Coverage**
        A Python module that measures code coverage during test execution.
	

      **decorator**
        A Python module that aims to simplify the usage of decorators for the average
        programmer and to popularize decorators by showing various non-trivial examples. A
        Python decorator is a specific change to the Python syntax that allows users to
        more conveniently alter functions and methods.

      
      **DLL**
        Dynamically Loadable Library 
	
	
      **docutils**
        Docutils is a modular system for processing documentation into useful
        formats, such as HTML, XML, and LaTeX. For input Docutils supports
        reStructuredText, an easy-to-read, what-you-see-is-what-you-get plain text
        markup syntax.


      **Driver**
        A Driver's function is to iterate over a Workflow until some
        condition is met. The simplest Driver executes a Workflow only once.
        Other Drivers, such as Optimizers or ParameterStudies, would execute
        the Workflow a variable number of times based on their inputs.  

       
      **egg**
        A zip file with a specified internal directory structure that
        contains a Python package or module. It is similar to a jar file in java. For
        more information on eggs, see PythonEggs_.

      .. _PythonEggs: http://peak.telecommunity.com/DevCenter/PythonEggs


      **eggs**
        See :term:`egg` above. 

      
      **Elixir**
        A declarative layer on top of SQLAlchemy that provides the ability to create simple
        Python classes that map directly to relational database tables. It provides many of
        the benefits of traditional databases without losing the convenience of Python
        objects.

	
      **F2PY**
        Fortran to Python interface generator.

     
      **Factory**
        An object that knows how to create objects of specific types.  


      **FactoryManager**
        All requests to create any type of Container must go through the
        FactoryManager object. Multiple Factory derived objects can be registered with
        the FactoryManager to allow creation of objects in various ways, e.g., locally
        via import and remotely via an ObjServer.


      **FEM**
	Finite Element Method. A numerical technique for finding approximate
	solutions of partial differential equations (PDE) as well as of integral
	equations. A structural analysis method which discretizes a continuum model of a
	complex structure to a discrete model of a finite number of degrees of freedom.


      **FormEncode**
        An HTML form validation, generation, and conversion package.


      **GA**
      	Genetic algorithm
	
	
      **GUI**
        Graphical User Interface. A computer operating system that is based upon icons
        and visual relationships rather than text.
    
    
      **IGeomQueryObject**
        An interface to an object having physical dimensions and shape, with
        properties that can be queried.
       

      **IPC**
        Interprocess communication


      **Jinja**
        A small but fast and easy-to-use stand-alone template engine written in pure
        Python. 


      **Jinja2**
        Jinja2 is the new and improved version of Jinja with new features and a simpler and
        faster runtime. 
	

      **JSON**
        JSON, short for JavaScript Object Notation, is a lightweight computer data
        interchange format. It is a text-based, human-readable format for representing
        simple data structures and associative arrays (called objects).


      **LAN** 
        Local Area Network. An interconnection of computers that are in relatively
        close proximity to each other, such as within a building. 


      **Mako**
        A template library written in Python that provides a familiar, non-XML syntax which
        compiles into Python modules for maximum performance.

	
      **MDAO**  
        Multi-disciplinary Analysis & Optimization

      
      **metadata**
        Metadata is information about an informational resource, such as a document (e.g,, a webpage), image,
        dataset or other resource. It describes context, content, and structure of the resource and its
        management through time. 

	
      **Method of Feasible Directions**
        A gradient descent optimization algorithm used in CONMIN.
	
	
      **model**
        A hierarchical structure with an :term:`Assembly` at its root.
       
       
      **networkx**
        A Python package for creating and manipulating graphs and networks.
	
	
      **NOSA**
        NASA Open Source Agreement. A software license approved by the Open
        Source Initiative (:term:`OSI`). The National Aeronautics and Space
        Administration (NASA) releases some software under this license.
      
      
      **nose**
        A unittest extension offering automatic test suite discovery and easy test
        authoring.

      **nosecoverage2**
        A nose plugin that exposes some features of version 3.2 of the **coverage**
        package, including html output and cobertura output.
            
      **NumPy**
        NumPy is the fundamental package needed for scientific computing with Python. See
	http://numpy.scipy.org/ 
	
      
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
        `Open Source Initiative <http://www.opensource.org>`_.


      **PBS**
        Portable Batch System (PBS) is a queueing system. Jobs are submitted to the
        queue that reflects the resources needed, and a scheduler decides which ones
        to run when nodes become available. These decisions are made on the basis of
        length of run, how long a job has been waiting, and fair sharing of resources
        among different users.


      **Paste**
        Tools for using a Web Server Gateway Interface stack.
	
      
      **PasteDeploy**
        A tool to load, configure, and compose :term:`WSGI` applications and servers.


      **PasteScript**
	A pluggable command-line frontend, including commands to setup package file layouts


      **PID**
        Process id


      **PIL**
        Python Imaging Library
	
	
      **pip**
	pip installs Python packages. It is a replacement for ``easy_install`` and uses mostly
	the same techniques for finding packages, so packages that were made easy_installable
	should be pip-installable as well.


      
      **plugin**
        A computer program that interacts with a host aplication to
        provide specific functionality without changing the host application.


      **plugins**
        See :term:`plugin`

	
      **Public Variables** 
      A component's inputs and outputs. They are called "public" variables because
      they are exposed to the framework; inputs can be set and outputs can be read
      by other framework components. In general, a public variable is a wrapper for
      data passed between framework components. It has a value, a default value, and
      may contain additional attributes like min/max values and units." Public
      variables can also perform their own validation when being assigned to another
      public variable. OpenMDAO's public variables are implemented using Traits, an
      open source extension to Python authored by Enthought, Inc.

	
      **Pyevolve**
        A complete Python genetic algorithm framework
	
	
      **Pygments**
        Pygments is a syntax highlighting package written in Python.
	
	
      **Pylons**
        A Web framework


      **pyparsing**
        A Python parsing module
	
	
      **PyYAML**
        A :term:`YAML` parser and emitter for Python.
	

      **repository**
        A store of revisions. See :term:`revision`.
	

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
        An object containing information defining system attributes required to
        select a  viable place to run a code.
     

      **reStructuredText**
        A plain-text markup syntax and parser system used for in-line
        program documentation (such as Python docstrings), for quickly
        creating simple Web pages, and for standalone documents. The
        reStructuredText parser is a component of Docutils.

     
      **revision**
        A snapshot of the state of files and directories, including their
        content. A revision also has some metadata associated with it, including
        who committed it, when it was committed, a commit message, and the
        parent revisions from which it was derived.
     
      
      **roman**
        A Python module for roman numerals.
	
	
      **Routes**   
        A Routing package for Python that matches URLs to dicts (dictionary network
        protocols) and vice versa.
	
	
      **SciPy**   
        SciPy (pronounced "Sigh Pie") is open-source software for mathematics, science, and
        engineering.

	
      **ScientificPython**
        A collection of Python modules that are useful for scientific computing.
	
	
      **setuptools**
        Tools for downloading, building, installing, upgrading, and uninstalling
        Python packages. 

      
      **simplejson**
        Simple, fast, extensible :term:`JSON` encoder/decoder for Python.
	
	
      **Sphinx**
        A Python documentation generator. It translates reStructuredText markup language into html.
	
	
      **SQLAlchemy**
        Database Abstraction Library
	
		
      **Socket**
        A placeholder for a plugin within a :term:`Component`.

      
      **SocketList**
        A list that holds only objects that provide a particular interface.


      **Sockets**
        See :term:`Socket`.
	

      **StringRef**	
	A special kind of trait that contains a string that points to some
	location in the OpenMDAO variable tree. 


      **SWIG**
        Simplified Wrapper and Interface Generator. A software development tool that connects
        programs written in C and C++ with a variety of high-level programming languages. Go to 
        http://www.swig.org/index.php for more information.

     
      **Tempita**
        Tempita is a small templating language for text substitution.
     
          
      **ticket**
        A ticket is a file contained within an issue (or bug) tracking system
        that contains information about a request, often by an end user, to
        fix a bug or make some other change to a computer software package. A
        ticket has a unique reference number (ticket number) that allows the
        submitter, software developer, or other technical staff to quickly
        locate, add to, or communicate the status of the submitter's issue or
        request. Once OpenMDAO's bug-tracking tool is in place, all changes to
        the software will require a ticket, and the ticket number will identify
        the software :term:`branch` where the change is being made.

	
      **Traits**
        A software package from Enthought, Inc. that aids in developing Python code. A trait is
        a type definition that can be used for normal Python object attributes, giving the
        attributes some additional characteristics. See http://code.enthought.com/projects/traits/. 
	The Traits package works with version 2.4 and later of Python and is similar in some ways
	to the Python property language feature. 


      **TraitType** 
        The base class used to validate and possibly convert data objects that are
        passed between linked Components.

      
      **units**
        An egg used by OpenMDAO that provides unit conversion capability for variables.
	
	
      **Variable**
        A wrapper for data passed between framework components. Variables can contain
        a value, a default value, optional min/max values, and units. Variables can
        perform their own validation when being assigned to another Variable. To
        extend the framework by creating a new data type to be passed between
        Components, a developer would create a new Python class that inherits from
        Variable. This Variable would have its own validation routine and could also
        have a custom viewer/editor.


      **VSP**
        Vehicle Sketch Pad. VSP is a rapid geometry creation tool used to create a
        wide range of aerospace vehicles from a combination of predefined components.
        It provides highly interactive sketching of concepts with immediate visual
        feedback.

    
      **WebError**
        A Python package for Web error handling and exception catching.
              
       
      **WebHelpers**
	A library of helper functions intended to make writing templates in Web applications
	easier. It's the standard function library for Pylons and TurboGears 2. It also
	contains a large number of functions not specific to the Web, including text
	processing, number formatting, date calculations, container objects, etc.

            
      **WebOb**
        :term:`WSGI` request and response object 
	

      **WebTest** 
	Helper to test :term:`WSGI` applications. This wraps any WSGI application and makes it
	easy to send test requests to that application without starting up an HTTP
	server. 
	

      **Workflow**
        A Workflow controls the execution order of a group of Components. The Workflow
        class supports simple sequential execution. Other classes inheriting from 
        Workflow will support different execution schemes, e.g., concurrent execution,
        conditional execution, and looping of various types.


      **WSGI**
        Web Server Gateway Interface. WSGI is a standard interface for Python Web
        applications to communicate with Web servers. 


      **YAML**
        YAML is a data serialization format designed for human readability and interaction
        with scripting languages. 
      
      
      **Zope**
        Zope is an open source application server for building content management
        systems, intranets, portals, and custom applications. Zope is written in
        Python, an object-oriented scripting language.

      
      **zope.component**
        Zope Component Architecture

      **zope.deferredimport** 
        The zope.deferredimport package provided facilities for defining names in modules
        that will be imported from somewhere else when used.


      **zope.deprecation** 
        This package provides a simple function called "deprecated(names, reason)" to
        deprecate Python objects.

      
      **zope.event**
        A very basic event publishing system.

      
      **zope.i18nmessageid**
        This package provides facilities for delaring  messages (which carry a
        translation source domain and possibly a default translation) within program
        source text; translation of the messages is the responsiblitiy of the "zope.i18n"
        package.

      **zope.interface**
        A package that provides an implementation of object interfaces for Python. 


      **zope.proxy**
        Generic transparent proxies. Proxies are special objects which serve as
        mostly transparent wrappers around another object, intervening in the apparent
        behavior of the wrapped object only when necessary to apply the policy (e.g.,
        access checking, location brokering, etc.) for which the proxy is responsible.


      **zope.schema**
        zope.interface extension for defining data schemas


      **ZopeSkel**
        ZopeSkel provides a collection of skeletons for quickstarting Zope and Plone
        projects.


