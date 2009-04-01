The code here was developed just as a feasability check.
It's not intended to be the 'right', or even a 'good' way to do things.
It was written by someone with very little 'mool' knowledge.
Hopefully it will provide a start for a real implementation.

NOTE: because we don't have Python eggs for mool, SciPy, etc. yet, this
      code will run only if you add the following lines to the
      [app] section of your buildout.cfg file (after the recipe)
      and then re-run the buildout.  Note that the second path is there in 
      order to find the SciPy distribution, which may be in a different place
      on your system, so you may have to modify that path.

extra-paths = ${buildout:directory}/../contrib
              /usr/local/lib/python2.5/site-packages

      Once we have the required eggs, you won't need the above hack,
      and the __init__.py file can be removed from the m4 directory.

To run the examples, use the bin/python script in the buildout directory to
run doe_example.py and mid_fidelity_example.py in the examples directory.

For example, from the buildout directory, type:

   bin/python ../contrib/m4/examples/doe_example.py



