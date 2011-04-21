Executing the Simple Optimization Problem
==========================================

To run your model, you need to create an instance of ``OptimizationUnconstrained`` and tell it to run.
You did this above using an interactive Python session. Try doing this for
``optimization_unconstrained.py``.

You can execute this model another way. You can add some code to the end of the
``optimization_unconstrained.py`` so that it can be executed in Python, either at the command line or in
the Python shell. Using the conditional ``if __name__ == "__main__":`` you can include some Python code at the bottom of ``optimization_unconstrained.py``. It will execute
only when you call it at the command line or the shell, and not when another module imports it. So
the final lines in this file are:

.. testsetup:: simple_model_Unconstrained_run

    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    __name__ = "__main__"

.. testcode:: simple_model_Unconstrained_run

    if __name__ == "__main__": 

        from openmdao.main.api import set_as_top
        opt_problem = OptimizationUnconstrained()
        set_as_top(opt_problem)

        import time
        tt = time.time()
        
        opt_problem.run()

        print "\n"
        print "CONMIN Iterations: ", opt_problem.driver.iter_count
        print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
        print "Elapsed time: ", time.time()-tt, "seconds"

.. testoutput:: simple_model_Unconstrained_run
    :hide:

    ...
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  ... seconds
        
 
In this block of code you are doing four things: 

   1. In the first statement, you create an instance of the class ``OptimizationUnconstrained`` with
      the name ``opt_problem``. 
   2. In the second statement, you set ``opt_problem`` as the top Assembly in the model hierarchy. (This will be explained in a later tutorial.)    
   3. In the fifth statement, you tell ``opt_problem`` to run. (The model will execute until the optimizer's
      termination criteria are reached.) 
   4. In the remaining statements, you define the results to print, including the elapsed time.

Please edit your copy of ``optimization_unconstrained.py`` and add the 
block of code into it. Now, save the file and type the following at the command
prompt:

::

        python optimization_unconstrained.py

This should produce the output:

:: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  0.0558300018311 seconds

Now you are ready to solve a more advanced optimization problem with constraints.
