"""
Exercise an EC2-based ClusterAllocator by performing a global optimization of
the Goldstein-Price via DOEdriver setting sarting points for CONMINdriver.

While this was written for testing an EC2-based cluster, ClusterAllocator is
generic, it can be used with any collection of hosts accessible via ssh without
requiring entry of passwords.

This is not intended as a good example of global optimization, rather it is
an example of how an EC2-based cluster might be utilized.
"""

import logging
import os
import socket
import sys

from openmdao.main.api import Assembly, Component, enable_console
from openmdao.main.datatypes.api import Float
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import ClusterAllocator, ClusterHost

from openmdao.lib.casehandlers.api import CSVCaseRecorder
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial

# Set False for local debugging and True when an EC2 configuration is defined.
USE_EC2 = True


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
        doe.add_parameter('gp_fun.x1')
        doe.add_parameter('gp_fun.x2')
        doe.DOEgenerator = FullFactorial(5)
        doe.case_outputs = ['gp_fun.f', 'gp_fun.x1', 'gp_fun.x2',
                            'gp_fun.exec_count']
        doe.recorders = [CSVCaseRecorder()]


def main():
    """ Configure a cluster and use it. """
    if USE_EC2:
        # EC2 hosts in the form user@host.
        hostnames = [
            'ubuntu@ec2-23-20-107-190.compute-1.amazonaws.com',
        ]
        # Path to OpenMDAO Python executable (assumed same on all hosts).
        python = 'setowns1_2013-04-26_10.06.38.732008' \
                 '/OpenMDAO-OpenMDAO-Framework-testbranch/devenv/bin/python'
        # It's assumed that both ends are firewalled.
        tunnel = True
        # The identity file used to access EC2 via ssh.
        identity_filename = '/home/setowns1/.ssh/lovejoykey.pem'
    else:
        # Trivial local 'cluster' for debugging without remote host issues.
        hostnames = [socket.getfqdn()]
        python = sys.executable
        tunnel = False
        identity_filename = None

    enable_console(logging.DEBUG)
    logging.getLogger().setLevel(logging.DEBUG)
    print 'Client PID', os.getpid()

    # Configure cluster.
    cluster_name = 'EC2Cluster'
    machines = []
    for hostname in hostnames:
        machines.append(ClusterHost(hostname=hostname, python=python,
                                    tunnel_incoming=tunnel,
                                    tunnel_outgoing=tunnel,
                                    identity_filename=identity_filename))
    # Start it.
    cluster = ClusterAllocator(cluster_name, machines, allow_shell=True)
    print 'Cluster initialized'
    RAM.insert_allocator(0, cluster)

    # Create model.
    top = GPOptimization()

    # Configure DOE.
    top.driver.sequential = False   # Run concurrently across cluster.
    top.driver.reload_model = False

    # Force use of only cluster hosts by adding this requirement.
    top.driver.extra_resources = dict(allocator=cluster_name)
#FIXME: this shouldn't be necessary.
    top.driver.ignore_egg_requirements = True

    # Perform the optimization.
    top.run()


if __name__ == '__main__':
    main()

