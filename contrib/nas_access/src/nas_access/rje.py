"""
Server to process remote job requests via an intermediary file server.
Passes requests to local allocators/servers.
"""

import logging
import optparse
import os
import signal
import sys
import threading
import time

from openmdao.main.resource import ResourceAllocationManager as RAM

from nas_access.protocol import server_init, server_accept, server_heartbeat, \
                                server_cleanup, configure_ssh, configure_scp
from nas_access.wrapper import AllocatorWrapper

_DMZ_HOST = None


def main(): # pragma no cover
    """
    Runs the RJE server.

    Usage: python rje.py [--allocator=name][--dmz-host=name][--poll-delay=secs][--resources=filename]

    --allocator: string
        Allocator to provide remote access to. Default ``PBS``.

    --dmz-host: string
        DMZ file server to use. Default ``dmzfs1.nas.nasa.gov``.

    --poll-delay: int
        Seconds between checks for new client connections. Default 60.

    --resources: string
        Filename for resource configuration. If not specified then the
        default of ``~/.openmdao/resources.cfg`` will be used.
    """

    parser = optparse.OptionParser()
    parser.add_option('--allocator', action='store', type='str', default='PBS',
                      help='Allocator to provide remote access to')
    parser.add_option('--dmz-host', action='store', type='str',
                      default='dmzfs1.nas.nasa.gov',
                      help='DMZ file server to use')
    parser.add_option('--poll-delay', action='store', type='int', default=60,
                      help='Seconds between checks for new client connections')
    parser.add_option('--resources', action='store', type='str', default=None,
                      help='Filename for resource configuration')
    parser.add_option('--ssh', action='store', type='str', default=None,
                      help='ssh command (used during testing)')
    parser.add_option('--scp', action='store', type='str', default=None,
                      help='scp command (used during testing)')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)
    logging.critical('started')
    logging.debug('started')

    # Configure ssh and scp.
    if options.ssh:
        configure_ssh(options.ssh.split())
    if options.scp:
        configure_scp(options.scp.split())

    # Optionally configure resources.
    if options.resources:
        RAM.configure(options.resources)

    # Get allocator to wrap.
    try:
        allocator = RAM.get_allocator(options.allocator)
    except ValueError:
        msg = "Can't find allocator %r" % options.allocator
        print msg
        logger.error(msg)
        sys.exit(1)

    dmz_host = options.dmz_host
    poll_delay = options.poll_delay

    # Initialize protocol.
    server_init(dmz_host, logger)
    global _DMZ_HOST
    _DMZ_HOST = dmz_host
    msg = 'RJE server ready'
    print msg
    logger.info(msg)

    # And away we go...
    signal.signal(signal.SIGTERM, _sigterm_handler)
    try:
        while True:
            connection = server_accept(dmz_host, logger)
            if connection is not None:
                print 'New connection at %r' % connection.root
                logger.info('New connection at %r', connection.root)
                wrapper = AllocatorWrapper(allocator, connection)
                name = '%s_handler' % os.path.basename(connection.root)
                handler = threading.Thread(name=name,
                                           target=wrapper.process_requests)
                handler.daemon = True
                handler.start()
            else:
                server_heartbeat(dmz_host, logger)
                time.sleep(poll_delay)
    except KeyboardInterrupt:
        pass
    finally:
        _cleanup()
    sys.exit(0)


def _sigterm_handler(signum, frame):  # pragma no cover
    """ Try to go down gracefully. """
    logging.getLogger().info('sigterm_handler invoked')
    print 'sigterm_handler invoked'
    sys.stdout.flush()
    _cleanup()
    sys.exit(1)


def _cleanup():  # pragma no cover
    """ Cleanup in preparation to shut down. """
    return
    keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
    if not keep_dirs:
        server_cleanup(_DMZ_HOST, logger)


if __name__ == '__main__':
    main()

