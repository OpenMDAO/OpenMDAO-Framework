"""
Exercise an EC2-based ClusterAllocator by performing a global optimization of
the Goldstein-Price function via a DOEdriver setting starting points for a
CONMINdriver.

While this was written for testing an EC2-based cluster, ClusterAllocator is
generic, it can be used with any collection of hosts accessible via ssh without
requiring entry of passwords.

This is not intended as a good example of global optimization, rather it is
an example of how an EC2-based cluster might be utilized.
"""

import logging
import os.path
import socket
import sys

from openmdao.main.api import Assembly, Component, enable_console
from openmdao.main.datatypes.api import Float
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import ClusterAllocator, ClusterHost

from openmdao.lib.casehandlers.api import CSVCaseRecorder, DumpCaseRecorder
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial

# Set False for local debugging and True when an EC2 configuration is defined.
USE_EC2 = False


class GoldsteinPrice(Component):
    """
    Goldstein-Price test function.
    Defined over [-2, 2] with a global minimum of 3 at (0, -1).
    """

    x1 = Float(low=-2, high=2, iotype='in')
    x2 = Float(low=-2, high=2, iotype='in')
    f  = Float(iotype='out')

    def execute(self):
        """ Compute f = Goldstein-Price(x1, x2) """
        x1 = self.x1
        x2 = self.x2
        f = (1 + (x1 + x2 + 1)**2 * (19 - 14*x1 + 13*x1**2 - 14*x2 + 6*x1*x2 + 3*x2**2)) \
          * (30 + (2*x1 - 3*x2)**2 * (18 - 32*x1 + 12*x1**2 + 48*x2 - 36*x1*x2 + 27*x2**2))
        self.f = f


class GPOptimization(Assembly):
    """ Global optimization of Goldstein-Price via DOE and CONMIN. """

    def configure(self):
        """ Configure a simple DOE to set start points for CONMIN. """
        self.add('gp_fun', GoldsteinPrice())

        conmin = self.add('conmin', CONMINdriver())
        conmin.workflow.add('gp_fun')
        conmin.add_parameter('gp_fun.x1')
        conmin.add_parameter('gp_fun.x2')
        conmin.add_objective('gp_fun.f')

        doe = self.add('driver', DOEdriver())
        doe.workflow.add('conmin')
        doe.add_parameter('gp_fun.x1', low=-1.5, high=1.5, start=1)
        doe.add_parameter('gp_fun.x2', low=-1.5, high=1.5, start=1)
        doe.DOEgenerator = FullFactorial(5)
        doe.add_responses(['gp_fun.f', 'gp_fun.x1', 'gp_fun.x2',
                           'gp_fun.exec_count'])
        self.recorders = [CSVCaseRecorder(), DumpCaseRecorder()]


def main():
    """ Configure a cluster and use it. """
    enable_console(logging.DEBUG)
    logging.getLogger().setLevel(0)
    print 'Client PID', os.getpid()

    # Configure cluster.
    cluster_name = 'EC2Cluster'
    machines = []
    if USE_EC2:
        # The identity file used to access EC2 via ssh.
        identity_filename = os.path.expanduser('~/.ssh/lovejoykey')
        identity_filename += '.ppk' if sys.platform == 'win32' else '.pem'

        machines.append(ClusterHost(
            hostname='ubuntu@ec2-23-22-84-182.compute-1.amazonaws.com',
            python='setowns1_2013-05-06_09.17.04.529682' \
                '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/bin/python',
            tunnel_incoming=True, tunnel_outgoing=True,
            identity_filename=identity_filename))

        machines.append(ClusterHost(
            hostname='ubuntu@ec2-54-224-216-106.compute-1.amazonaws.com',
            python='setowns1_2013-05-06_09.17.03.113077' \
                '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/bin/python',
            tunnel_incoming=True, tunnel_outgoing=True,
            identity_filename=identity_filename))

        machines.append(ClusterHost(
            hostname='ubuntu@ec2-54-235-234-97.compute-1.amazonaws.com',
            python='setowns1_2013-05-06_09.17.05.434412' \
                '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/bin/python',
            tunnel_incoming=True, tunnel_outgoing=True,
            identity_filename=identity_filename))

        machines.append(ClusterHost(
            hostname='Administrator@ec2-50-19-1-69.compute-1.amazonaws.com',
            python='setowns1_2013-05-06_09.20.17.379627' \
                '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/Scripts/python',
            tunnel_incoming=True, tunnel_outgoing=True,
            identity_filename=identity_filename))

        machines.append(ClusterHost(
            hostname='Administrator@ec2-174-129-138-250.compute-1.amazonaws.com',
            python='setowns1_2013-05-06_09.19.49.348885' \
                '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/Scripts/python',
            tunnel_incoming=True, tunnel_outgoing=True,
            identity_filename=identity_filename))

#        machines.append(ClusterHost(
#            hostname='viper.grc.nasa.gov',
#            python='OpenMDAO-Framework/devenv/bin/python',
#            tunnel_incoming=True, tunnel_outgoing=True,
#            identity_filename=None))
    else:
        # Trivial local 'cluster' for debugging without remote host issues.
        machines.append(ClusterHost(hostname=socket.getfqdn(),
                                    python=sys.executable))
#        machines.append(ClusterHost(
#            hostname='viper.grc.nasa.gov',
#            python='OpenMDAO-Framework/devenv/bin/python',
#            tunnel_incoming=True, tunnel_outgoing=True,
#            identity_filename=None))

    # Start it.
    cluster = ClusterAllocator(cluster_name, machines, allow_shell=True,
                               method='load-average')
#                               method='greedy')
#                               method='round-robin')
    print 'Cluster initialized'
    RAM.insert_allocator(0, cluster)

    n_servers = RAM.max_servers(dict(allocator=cluster_name))
    print n_servers, 'Servers:'
    for name in RAM.get_hostnames(dict(allocator=cluster_name,
                                       min_cpus=n_servers)):
        print '   ', name

    # Create model.
    top = GPOptimization()

    # Configure DOE.
    top.driver.sequential = False   # Run concurrently across cluster.
    top.driver.reload_model = False
    # Force use of only cluster hosts by adding this requirement.
    top.driver.extra_resources = dict(allocator=cluster_name)
    # This is necessary more often than it should be.
    top.driver.ignore_egg_requirements = True

    # Perform the optimization.
    top.run()


if __name__ == '__main__':
    main()

