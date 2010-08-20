
.. index:: MDAO tutorial problem

.. _Tutorial-MDAO-Architectures:

Tutorial: MDAO Architectures
============================

This tutorial shows how to create a model to solve a simple problem consisting of
two coupled disciplines using several MDAO strategies including:

#. Multidisciplinary Design Feasible (MDF)
#. Independent Design Feasible (IDF)
#. Collaborative Optimization (CO)

The tutorial will introduce you to some new topics that include using the itereation
hieararchy to set up models with nested optimization loops, using a solver to "close
the loop" in a coupled multidisciplinary simulation, and using a broadcaster to set
the values of design variables in multiple places at one time.

This tutorial covers some of the more advanced capabilities of OpenMDAO. You should read and understand
the :ref:`simple tutorial problem <Getting-Started-with-OpenMDAO>` before starting this one. An
understanding of the material presented in ref:`A-More-Complex-Tutorial-Problem` is also
recommended.

All of these tutorials use the Sellar problem which consists of 2 disciplines as follows:

.. figure:: ../images/user-guide/Sellar.png
   :align: center

Variables z1, z2, and x1 are the design variables over which we'd like to minimize
the objective. Both disciplines are functions of z1 and z2, so they are called the 
global design variables, while only the first discipline is a function of x1, so it
is called the local design variable. The two disciplines are coupled by the
coupling variables y1 and y2. Discipline 1 takes y2 as an input, and computes y1 as
an output, while discipline 2 takes y1 as in input and computes y2 as an output. As
such, the two disciplines depend on each other's output so iteration is required to
find a set of coupling variables that satisfies both equations.

Disciplines 1 and 2 were implemented in OpenMDAO as components.

.. testcode:: Disciplines

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class SellarDiscipline1(Component):
        """Component containing Discipline 1"""
        
        # pylint: disable-msg=E1101
        z1 = Float(0.0, iotype='in', desc='Global Design Variable')
        z2 = Float(0.0, iotype='in', desc='Global Design Variable')
        x1 = Float(0.0, iotype='in', desc='Local Design Variable')
        y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')
    
        y1 = Float(iotype='out', desc='Output of this Discipline')        
    
            
        def execute(self):
            """Evaluates the equation  
            y1 = z1**2 + z2 + x1 - 0.2*y2"""
            
            z1 = self.z1
            z2 = self.z2
            x1 = self.x1
            y2 = self.y2
            
            self.y1 = z1**2 + z2 + x1 - 0.2*y2
    
    
    class SellarDiscipline2(Component):
        """Component containing Discipline 2"""
        
        # pylint: disable-msg=E1101
        z1 = Float(0.0, iotype='in', desc='Global Design Variable')
        z2 = Float(0.0, iotype='in', desc='Global Design Variable')
        y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')
    
        y2 = Float(iotype='out', desc='Output of this Discipline')        
    
            
        def execute(self):
            """Evaluates the equation  
            y1 = y1**(.5) + z1 + z2"""
            
            z1 = self.z1
            z2 = self.z2
            
            # Note: this may cause some issues. However, y1 is constrained to be
            # above 3.16, so lets just let it converge, and the optimizer will 
            # throw it out
            y1 = abs(self.y1)
            
            self.y2 = y1**(.5) + z1 + z2
            
Discipline 2 contains a square root of variable y1 in its calculation. For negative values
of y1, the result would be imaginary, so the absolute value is taken before the square root
is applied. This component is clearly not valid for y1 < 0, and our first was to add
a *low* attribute to the variable definition for y1. However, the solver that was used to
converge the two disciplines occasionally forced y1 to go slightly negative. The inclusion
of the absolute value solved the problem without impacting the eventual convergence of the
solver.

**References:**

_`1`. Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concurrent
      Subspace Optimization for Multidisciplinary System Design," Proceedings
      References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
      January 1996.        
            
Multidisciplinary Design Feasible (MDF)
---------------------------------------

In a Multidisciplinary Design Feasible (MDF) problem, the disciplines are directly coupled
via some kind of solver, and the design variables are optimized in a single loop.

.. figure:: ../images/user-guide/Arch-MDF.png
   :align: center

.. figure:: ../images/user-guide/Arch-MDF-OpenMDAO.png
   :align: center   