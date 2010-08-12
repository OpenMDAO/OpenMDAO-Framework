import glob
import os
import sys
import zipfile

from openmdao.util.log import NullLogger


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

    src_server : Proxy
        Host to get file from.

    src_path : string
        Path to file on `src_server`.

    dst_server : Proxy
        Host to put file to.

    dst_path : string
        Path to file on `dst_server`.

    mode : string
        Mode settings for :func:`open`, not including 'r' or 'w'.
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


def pack_zipfile(patterns, filename, logger=NullLogger):
    """
    Create 'zip' file `filename` of files in `patterns`.
    Returns ``(nfiles, nbytes)``.

    patterns : list
        List of :mod:`glob` style patterns.

    filename : string
        Name of zip file to create.

    logger : Logger
        Used for recording progress.
    """
    nfiles = 0
    nbytes = 0
    zipped = zipfile.ZipFile(filename, 'w')
    try:
        for pattern in patterns:
            for path in glob.glob(pattern):
                size = os.path.getsize(path)
                logger.debug("packing '%s' (%d)...", path, size)
                zipped.write(path)
                nfiles += 1
                nbytes += size
    finally:
        zipped.close()
    return (nfiles, nbytes)


def unpack_zipfile(filename, logger=NullLogger):
    """
    Unpack 'zip' file `filename`.
    Returns ``(nfiles, nbytes)``.

    filename : string
        Nmae of zip file to unpack.

    logger : Logger
        Used for recording progress.
    """
    nfiles = 0
    nbytes = 0
    zipped = zipfile.ZipFile(filename, 'r')
    try:
        for info in zipped.infolist():
            size = info.file_size
            logger.debug("unpacking '%s' (%d)...", info.filename, size)
            zipped.extract(info)
            nfiles += 1
            nbytes += size
    finally:
        zipped.close()
    return (nfiles, nbytes)

