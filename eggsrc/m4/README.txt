The code here was developed just as a feasability check.
It's not intended to be the 'right', or even a 'good' way to do things.
It was written by someone with very little 'mool' knowledge.
Hopefully it will provide a start for a real implementation.

NOTE: because we don't have Python eggs for mool, SciPy, etc., this
      code will run only after subverting the 'buildout/bin/python' script
      by adding these lines after the sys.path[0:0] setting:

sys.path.append('/OpenMDAO/dev/setowns1/T0013-NPSScomponent/eggsrc')
sys.path.append('/usr/local/lib/python2.5/site-packages')

      Where 'setowns1/T0013-NPSScomponent' is replaced with your
      appropriate location.

      Once we have the required eggs, you won't need the above hack,
      and the __init__.py file can be removed.

