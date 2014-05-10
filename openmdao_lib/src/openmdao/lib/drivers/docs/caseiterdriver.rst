
.. _CaseIteratorDriver:

*CaseIteratorDriver*
~~~~~~~~~~~~~~~~~~~~

The CaseIteratorDriver provides the capability to evaluate one or more cases
on a workflow.  These evaluations can be performed either sequentially (the
default) or concurrently (subject to computational resources available).

During sequential execution, each case is evaluated by applying the inputs to
the workflow, executing the workflow, retrieving the outputs, and recording
the evaluated case.

For concurrent execution, a modified form of the assembly is packaged into an
:term:`egg`, which is a file which can be sent to a remote server for execution.
The modifications consist of replacing the assembly's driver with a default
driver, and setting that driver's workflow to the CaseIteratorDriver's workflow.
After the egg is created, the ResourceAllocationManager (RAM) is used to
allocate servers to evaluate the cases concurrently.  Servers are selected
based on egg requirements (Python version, installed packages, etc.).

Ordinarily, the model (modified form of the assembly) is loaded into a server
prior to each evaluation.  Since loading takes some time, this reloading
process can be skipped if ``reload_model`` is set False.  Some models
require reloading (typically due to limitations of certain components used),
so always reloading is the safe default.  Note that during sequential execution
no reloading takes place.

*Source Documentation for caseiterdriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
