"""
Routines to support testing with cluster allocation.
"""

import getpass
import glob
import os.path
import platform
import shutil
import sys

from openmdao.main.resource import ResourceAllocationManager, \
                                   LocalAllocator, ClusterAllocator


# Users who have ssh configured correctly for testing.
SSH_USERS = []


def init_cluster(encrypted=True, clean_dir=True):
    """
    If not already done, initializes the ResourceAllocationManager and
    adds a cluster using encrypted or unencrypted communication.
    Returns the name of the configured cluster.
    """
    authkey = 'PublicKey' if encrypted else 'in-the-clear'
    allocators = ResourceAllocationManager.list_allocators()

    if len(allocators) == 1:
        local = ResourceAllocationManager.get_allocator(0)
        if local.max_load < 10:  # First time we've been called.
            # Ensure we aren't held up by local host load problems.
            local.max_load = 10

            if clean_dir:
                # Remove any local allocator-created directories.
                for path in glob.glob('Sim-*'):
                    shutil.rmtree(path)

    node = platform.node()
    name = '%s_%s' % (node.replace('.', '_'), authkey)
    for allocator in allocators:
        if allocator.name == name:
            return name  # Don't add multiple copies.

    machines = []
    python = sys.executable

    if node.startswith('gxterm'):
        # User environment assumed OK on this GRC cluster front-end.
        for i in range(55):
            machines.append({'hostname':'gx%02d' % i, 'python':python})
    elif local_ssh_available():
        machines.append({'hostname':node, 'python':python})

    if machines:
        cluster = ClusterAllocator(name, machines, authkey)
        ResourceAllocationManager.insert_allocator(0, cluster)
        return name
    elif not encrypted:
        # Create a LocalAllocator so we have *something*.
        name = 'LocalUnencrypted'
        for allocator in allocators:
            if allocator.name == name:
                return name  # Don't add multiple copies.
        local = LocalAllocator(name, authkey=authkey)
        ResourceAllocationManager.insert_allocator(0, local)
        return name
    return None


def local_ssh_available():
    """ Return True if this user has an authorized key for this machine. """
    if sys.platform == 'win32':
        # ssh server not typically available on Windows.
        return False

    user = getpass.getuser()
    if user not in SSH_USERS:
        # Avoid problems with users who don't have a valid environment.
        return False

    home = os.environ['HOME']
    node = platform.node()
    keyfile = os.path.join(home, '.ssh', 'authorized_keys')
    try:
        with open(keyfile, 'r') as keys:
            for line in keys:
                if line.find(user+'@'+node) > 0:
                    return True
            return False
    except IOError:
        return False

