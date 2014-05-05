"""
Routines to help obtain network information.
"""

import socket


def get_unused_ip_port():
    '''Find an unused IP port.

       ref: http://code.activestate.com/recipes/531822-pick-unused-port/

       Note: Use the port before it is taken by some other process!
    '''
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(('localhost', 0))
    sockname = sock.getsockname()
    sock.close()
    return sockname[1]  # port number
