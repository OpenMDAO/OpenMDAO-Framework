Setting Up Problems for Automatic Architectures
===============================================

In all the previous examples, first you defined an assembly and then added the ``Discipline 1`` and ``Discipline 2``
components to that assembly. You also added at least one driver (e.g., optimizer) to the assembly. This let you 
set up a specific version of the Sellar Problem that matched up with the structure for solving a problem using 
IDF, MDF, or CO. Each example had a different set of optimizers, parameters, constraints, and objectives. 

In OpenMDAO you can automatically configure the Sellar Problem to be solved with IDF, MDF, or CO.
Using this automatic formulation will result in a lot less effort on your part. But, before you can
use the  automatic architectures, you need to make a small change to how you define the Sellar
Problem. You need to create a more general description of the Sellar Problem that is independent of
how you would solve it with any given  architecture. 

In OpenMDAO you do this with a special kind of assembly called an *ArchitectureAssembly*. When you
define your ArchitectureAssembly, in addition to adding the specific discipline analyses, you also
specify the  parameters, objectives, constraints, and coupling variables of the fundamental problem
formulation. For example:

.. testcode:: sellar_architecture_assembly


        from openmdao.main.api import ArchitectureAssembly
        from openmdao.lib.optproblems.sellar import Discipline1, Discipline2
        
        class SellarProblem(ArchitectureAssembly):
            """ Sellar test problem definition.
            Creates a new Assembly with this problem
                
            Optimal Design at (1.9776, 0, 0) 
            Optimal Objective = 3.18339"""
                
            def configure(self):         
                #add the discipline components to the assembly
                self.add('dis1', Discipline1())
                self.add('dis2', Discipline2())
                
                #START OF MDAO Problem Definition
                #Global Des Vars
                self.add_parameter(("dis1.z1","dis2.z1"), name="z1", low=-10, high=10, start=5.0)
                self.add_parameter(("dis1.z2","dis2.z2"), name="z2", low=0, high=10, start=2.0)
                
                #Local Des Vars 
                self.add_parameter("dis1.x1", low=0, high=10, start=1.0)
                
                #Coupling Vars
                self.add_coupling_var(("dis2.y1","dis1.y1"), name="y1", start=1.0)
                self.add_coupling_var(("dis1.y2","dis2.y2"), name="y2", start=1.0)
                                   
                self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)', name="obj1")
                self.add_constraint('3.16 < dis1.y1')
                self.add_constraint('dis2.y2 < 24.0')


                #END OF Sellar Problem Definition


The first part of this file imports the same discipline analyses used for the :ref:`IDF <Individual-Design-Feasible-(IDF)>`, 
:ref:`MDF <Multidisciplinary-Design-Feasible-(MDF)>`, and :ref:`CO <Collaborative-Optimization-(CO)>` tutorials. Next you 
define the `SellarProblem` class, and add the discipline analyses to it. 

:: 

        from openmdao.main.api import ArchitectureAssembly
        from openmdao.lib.optproblems.api import Discipline1, Discipline2
        
        class SellarProblem(ArchitectureAssembly):
            """ Sellar test problem definition.
            Creates a new Assembly with this problem
                
            Optimal Design at (1.9776, 0, 0) 
            Optimal Objective = 3.18339"""
                
            def configure(self):         
                #add the discipline components to the assembly
                self.add('dis1', Discipline1())
                self.add('dis2', Discipline2())
                
Once you have the components added to the assembly, you can start specifying the problem formulation. Beside the 
analysis codes themselves, any problem definition will consist of the following five things: 

  #. Global design values
  #. Local design values
  #. Objective(s)
  #. Coupling variable pairs
  #. Constraints

For the Sellar Problem, the problem formulation is specified as follows: 

:: 


                #START OF MDAO Problem Definition
                #Global Des Vars
                self.add_parameter(("dis1.z1","dis2.z1"), name="z1", low=-10, high=10, start=5.0)
                self.add_parameter(("dis1.z2","dis2.z2"), name="z2", low=0, high=10, start=2.0)
            
                #Local Des Vars 
                self.add_parameter("dis1.x1", low=0, high=10, start=1.0)
            
                #Coupling Vars
                #you can give simpler names to the global vars
                self.add_coupling_var(("dis2.y1","dis1.y1"), name="y1", start=1.0)
                self.add_coupling_var(("dis1.y2","dis2.y2"), name="y2", start=1.0)
                               
                #you can also give names to objectives
                self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)', name="obj1")
                self.add_constraint('3.16 < dis1.y1')
                self.add_constraint('dis2.y2 < 24.0')
                
Notice that nowhere in the problem formulation is there any information about optimizers, 
solvers, or any other drivers and their associated workflows.  A good way to think 
about it is that the problem formulation contains all of the information that you 
actually care about to solve the problem. The specifics of what happens when you try 
to solve it with a given architecture are a secondary concern and don't show up in your 
problem definition. Any problem that you want to solve using one of the automatic 
architectures has to be defined in the manner we showed you above. 

In the OpenMDAO standard library, we have a number of optimization test problems defined 
for you to try out. These are located in the :ref:`openmdao.lib.optproblems <openmdao.lib.optproblems.api.py>` 
section of the library. 

So once you have defined your problem, you can solve it using any of the architectures in the 
OpenMDAO standard library (or you can define your own architecture to test out). 
We currently have five architectures implemented: 

 #. IDF
 #. MDF
 #. CO
 #. BLISS 
 #. BLISS-2000
 
 
All instances of ArchitectureAssembly have a :term:`Slot` called `architecture` that lets you configure a
specific  MDAO architecture. To test this out yourself, add  the following code to the bottom of the file
where you defined the SellarProblem class from above: 

::

                if __name__=="__main__": 
                
                    from openmdao.lib.architectures.api import IDF, MDF, CO, BLISS, BLISS2000
                    
                    def display_results(): 
                        print "Minimum found at (%f, %f, %f)" % (problem.dis1.z1,
                                                        problem.dis1.z2,
                                                        problem.dis1.x1)
                        print "Couping vars: %f, %f" % (problem.dis1.y1, problem.dis2.y2)
                        print "Function calls dis1: %d, dis2: %d"%(problem.dis1.exec_count,problem.dis2.exec_count)
                        print "\n"  
                
                    print "Running SellarProblem with MDF"
                    problem = SellarProblem()
                    problem.architecture = MDF()
                    problem.run()

                    display_results()

                    print "Running SellarProblem with CO"
                    problem = SellarProblem()
                    problem.architecture = CO()
                    problem.run()

                    display_results()

                    print "Running SellarProblem with BLISS"
                    problem = SellarProblem()
                    problem.architecture = BLISS()
                    problem.run()

                    display_results()

                    print "Running SellarProblem with BLISS2000"
                    #Note that BLISS2000 is stochastic and unstable and does not reliably converge 
                    # you might get an OverflowError
                    problem = SellarProblem()
                    problem.architecture = BLISS2000()
                    problem.run()

                    display_results()

                    print "Running SellarProblem with IDF"
                    problem = SellarProblem()
                    problem.architecture = IDF()
                    problem.run()

                    display_results()
        
    
If you run that file, you should get results something like the following. The function counts 
for the results with BLISS2000 may not match exactly. BLISS2000 uses a stochastic process 
in part of its optimization process, so if you run the optimization a few times, you will 
see the function counts vary a bit. 

::

                Running SellarProblem with IDF
                Minimum found at (1.977707, 0.000000, 0.000000)
                Couping vars: 3.160000, 3.755627
                Function calls dis1: 60, dis2: 54
                
                
                Running SellarProblem with MDF
                Minimum found at (1.977639, 0.000000, -0.000001)
                Couping vars: 3.159999, 3.755278
                Function calls dis1: 227, dis2: 222
                
                
                Running SellarProblem with CO
                Minimum found at (1.980130, 0.000000, 0.000707)
                Couping vars: 3.160001, 3.790079
                Function calls dis1: 8022, dis2: 9469
                
                
                Running SellarProblem with BLISS
                Minimum found at (1.981348, 0.000001, -0.000007)
                Couping vars: 3.173192, 3.762692
                Function calls dis1: 3808, dis2: 3649
                
                
                Running SellarProblem with BLISS2000
                Minimum found at (1.955188, 0.000000, 0.079449)
                Couping vars: 3.160000, 3.730012
                Function calls dis1: 1176, dis2: 165

        
        

    
