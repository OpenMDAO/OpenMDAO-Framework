"""
Test ClusterAllocator functionality. Named to ensure this is run manually since
it requires correct cluster configuration for the specified hosts.
"""

import getpass
import logging

from openmdao.main.api import enable_console
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import ClusterAllocator
from openmdao.main.mp_distributing import _SSH


def main():
    enable_console(0)
    logging.getLogger().setLevel(0)
    user = getpass.getuser()

    machines = [
        dict(hostname='havoc.grc.nasa.gov',
             python='/OpenMDAO/dev/%s/OpenMDAO-Framework/devenv/bin/python' % user),
        dict(hostname='viper.grc.nasa.gov',
             python='/Users/%s/OpenMDAO-Framework/devenv/bin/python' % user,
             tunnel=True),
    ]

    cluster_name = 'TestCluster'
    print cluster_name, 'configuration:'
    for machine in machines:
        print '    host:', machine['hostname']
        print '        python:', machine['python']
        print '           ssh:', _SSH

    cluster = ClusterAllocator(cluster_name, machines)
    print 'Cluster initialized'
    RAM.insert_allocator(0, cluster)

    hosts = RAM.get_hostnames(dict(min_cpus=2, allocator=cluster_name))
    print 'Hosts', hosts
    assert sorted(hosts) == ['havoc.grc.nasa.gov', 'viper.grc.nasa.gov']

    server, server_info = RAM.allocate(dict(allocator=cluster_name))
    print 'Server info', server_info
    assert server_info['host'] == 'havoc.grc.nasa.gov'
    print 'Remote files', server.listdir('.')
    assert sorted(server.listdir('.')) == \
           ['openmdao_log.txt', 'stderr', 'stdout']


if __name__ == '__main__':
    main()

