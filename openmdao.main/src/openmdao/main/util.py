"""
Various utility functions.
"""

import os

def filexfer(src_server, src_path, dst_server, dst_path, mode=''):
    """ Transfer file from one place to another. """
    if src_server is None:
        src_file = open(src_path, 'r'+mode)
    else:
        src_file = src_server.open(src_path, 'r'+mode)

    if dst_server is None:
        dst_file = open(dst_path, 'w'+mode)
    else:
        dst_file = dst_server.open(dst_path, 'w'+mode)

    chunk = 1 << 17    # 128KB
    data = src_file.read(chunk)
    while data:
        dst_file.write(data)
        data = src_file.read(chunk)
    src_file.close()
    dst_file.close()

    if src_server is None:
        mode = os.stat(src_path).st_mode
    else:
        mode = src_server.stat(src_path).st_mode
    if dst_server is None:
        os.chmod(dst_path, mode)
    else:
        dst_server.chmod(dst_path, mode)

