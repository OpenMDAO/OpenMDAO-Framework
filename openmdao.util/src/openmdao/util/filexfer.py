import os

def filexfer(src_server, src_path, dst_server, dst_path, mode=''):
    """
    Transfer a file from one place to another.

    If `src_server` or `dst_server` is None, then the :mod:`os` module
    is used for the source or destination respectively.  Otherwise the
    respective object must support :meth:`open`, :meth:`stat`, and
    :meth:`chmod`.

    `mode` specifies any :meth:`open` mode settings in addition to 'r' or 'w'.

    After the copy has completed, permission bits from :meth:`stat` are set
    via :meth:`chmod`.
    """
    if src_server is None:
        src_file = open(src_path, 'r'+mode)
    else:
        src_file = src_server.open(src_path, 'r'+mode)

    try:
        if dst_server is None:
            dst_file = open(dst_path, 'w'+mode)
        else:
            dst_file = dst_server.open(dst_path, 'w'+mode)

        if src_server is None and dst_server is None:
            chunk = 1 << 20  # 1MB locally.
        else:
            chunk = 1 << 17  # 128KB over network.

        try:
            data = src_file.read(chunk)
            while data:
                dst_file.write(data)
                data = src_file.read(chunk)
        finally:
            dst_file.close()
    finally:
        src_file.close()

    if src_server is None:
        mode = os.stat(src_path).st_mode
    else:
        mode = src_server.stat(src_path).st_mode
    if dst_server is None:
        os.chmod(dst_path, mode)
    else:
        dst_server.chmod(dst_path, mode)

