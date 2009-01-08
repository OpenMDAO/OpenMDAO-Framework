.. index:: security


Security
========


For any system, the security mechanisms employed are determined by the types of
threats to be protected against.  


.. index:: threat definition
.. index:: pair: security; mechanism

Threat Definition
_________________


In this system, the threats to be protected against are access to a simulation
by an unapproved user and inadvertent misuse of the system by a legal user.

Users will be identified by some mechanism (such as username, password), and
simulations should not be accessed by any user not listed in the simulation's
approved users. The system will not be addressing issues regarding intentional
*spoofing* of legal user IDs or various other forms of attack on a user's
identity.

There are expected to be situations where simulation methods and/or variables
should not be accessed or modified. For example, a component has some internal
configuration which should not be changed. The system will incorporate
mechanisms to protect the component from unintentional changes by a legal user
but not against deliberate modification of variable or function access
permissions by an expert user.

Simulations are contained within server processes and accessed via network
protocols. Depending upon a site's network configuration, this may allow
access from anywhere on the Internet. It is the site's responsibility to
isolate a simulation's servers from general Internet access if necessary.


.. index:: user access
.. index:: guest ID


User Access
___________


For a given simulation, there is only one class of valid user. Either you can
access the simulation, or you cannot. There are no *special* user classes.

Access control to a simulation is performed when attempting to connect to
the simulation server. The server has an access control list which contains
the user IDs for all legal users. Initially, the access control list contains
only the user who started the simulation. That user may then add other users
or open the simulation to all (via the *guest* ID). Note that the access
control list is just another python object, so any legal user can change it.

To support fully open simulations without having to explicitly list all users,
the system defines a special *guest* account. Any user may identify him or
herself as *guest.* Any simulation including *guest* in the access control list
is accessible by any user identified as *guest.* Once a guest has accessed a
simulation, that person may perform any operation, just like any other legal
user.


.. index:: pair: simulation; distributed
.. index:: encryption

Distributed Simulations
_______________________


For distributed simulations, only the top-level server may be accessed
directly by users. Sub-servers are accessible only by other servers in the
same simulation. This is configured in a similar manner to configuring users,
where servers are *users* and a special key is used for identification.

Communication between processes will support encryption. This includes both
the communication between servers and communications between the user
interface and the top-level server.

Bulk data transfers can be performed outside the framework if a component
requires it. Developers are encouraged to use secure methods (such as scp)
in such circumstances.

.. index:: attributes; execute
.. index:: attributes; read/write
.. index:: attributes; readonly
.. index:: attributes
.. index:: pair: locking; simulation


Method and Variable Accessibility
_________________________________


Methods and variables have *attributes* controlling their accessibility.
Methods have an *execute* attribute, whereas variables have either a
*read/write* or *readonly* attribute. Any valid user of the system can
manipulate these attributes via a standard framework API.

*Locking* a simulation/component entails removing the ability to change
accessibility via the standard framework API. Such a locked configuration
can then be distributed to other users. The locking process protects
against inadvertent access by users during their simulations.


.. index:: debugging
.. index:: manhole

Debugging
_________


For debugging purposes, a *manhole* is optionally provided, which has a separate
authentication mechanism (ssh) from normal framework access. Access to the
manhole is configured at server startup and cannot be enabled afterwards. The
manhole provides access to the server's Python interpreter. Initially this will
simply be the interpreter command line prompt. Later versions may provide
higher-level commands pertinent to simulation server debugging and/or GUI
access.

