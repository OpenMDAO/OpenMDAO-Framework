import fnmatch
import glob
import os
import sys
import zipfile

from openmdao.util.log import NullLogger


def filexfer(src_server, src_path, dst_server, dst_path, mode='',
             set_permissions=True):
    """
    Transfer a file from one place to another.

    If `src_server` or `dst_server` is None, then the :mod:`os` module
    is used for the source or destination respectively.  Otherwise the
    respective object must support :meth:`open`, :meth:`stat`, and
    :meth:`chmod`.

    After the copy has completed, permission bits from :meth:`stat` are set
    via :meth:`chmod`.

    src_server: Proxy
        Host to get file from.

    src_path: string
        Path to file on `src_server`.

    dst_server: Proxy
        Host to put file to.

    dst_path: string
        Path to file on `dst_server`.

    mode: string
        Mode settings for :func:`open`, not including 'r' or 'w'.

    set_permissions: bool
        If ``True`` then permissions of `src_path` are set on `dst_path`.
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

    if set_permissions:
        if src_server is None:
            mode = os.stat(src_path).st_mode
        else:
            mode = src_server.stat(src_path).st_mode
        if dst_server is None:
            os.chmod(dst_path, mode)
        else:
            dst_server.chmod(dst_path, mode)


def pack_zipfile(patterns, filename, logger=None):
    """
    Create 'zip' file `filename` of files in `patterns`.
    Returns ``(nfiles, nbytes)``.

    patterns: list
        List of :mod:`fnmatch` style patterns.

    filename: string
        Name of zip file to create.

    logger: Logger
        Used for recording progress.

    .. note::
        The code uses :meth:`glob.glob` to process `patterns`.
        It does not check for the existence of any matches.

    """
    logger = logger or NullLogger()

    # Scan to see if we have to use zip64 flag.
    nbytes = 0
    for pattern in patterns:
        for path in glob.glob(pattern):
            nbytes += os.path.getsize(path)
    zip64 = nbytes > zipfile.ZIP64_LIMIT
    compression = zipfile.ZIP_DEFLATED

    nfiles = 0
    nbytes = 0
    with zipfile.ZipFile(filename, 'w', compression, zip64) as zipped:
        for pattern in patterns:
            for path in glob.glob(pattern):
                size = os.path.getsize(path)
                logger.debug("packing '%s' (%d)...", path, size)
                zipped.write(path)
                nfiles += 1
                nbytes += size

    return (nfiles, nbytes)


def unpack_zipfile(filename, logger=None, textfiles=None):
    """
    Unpack 'zip' file `filename`.
    Returns ``(nfiles, nbytes)``.

    filename: string
        Name of zip file to unpack.

    logger: Logger
        Used for recording progress.

    textfiles: list
        List of :mod:`fnmatch` style patterns specifying which unpacked files
        are text files possibly needing newline translation. If not supplied,
        the first 4KB of each is scanned for a zero byte. If none is found, then the
        file is assumed to be a text file.
    """
    logger = logger or NullLogger()

    # ZipInfo.create_system code for local system.
    local_system = 0 if sys.platform == 'win32' else 3

    nfiles = 0
    nbytes = 0
    with zipfile.ZipFile(filename, 'r') as zipped:
        for info in zipped.infolist():
            filename, size = info.filename, info.file_size
            logger.debug('unpacking %r (%d)...', filename, size)
            zipped.extract(info)

            if sys.platform != 'win32':
                # Set permissions, extract() doesn't.
                rwx = (info.external_attr >> 16) & 0777
                if rwx:
                    os.chmod(filename, rwx)  # Only if something valid.

            # Requires mismatched systems.
            if info.create_system != local_system:  # pragma no cover
                if textfiles is None:
                    with open(filename, 'rb') as inp:
                        data = inp.read(1 << 12)
                    if '\0' not in data:
                        logger.debug('translating %r...', filename)
                        translate_newlines(filename)
                else:
                    for pattern in textfiles:
                        if fnmatch.fnmatch(filename, pattern):
                            logger.debug('translating %r...', filename)
                            translate_newlines(filename)
            nfiles += 1
            nbytes += size

    return (nfiles, nbytes)


def translate_newlines(filename):
    """
    Translate the newlines of `filename` to the local standard.

    filename: string
        Name of the file to be translated.
        The translated file will replace this file.
    """
    with open(filename, 'rU') as inp:
        with open('__translated__', 'w') as out:
            for line in inp:
                out.write(line)
    os.remove(filename)
    os.rename('__translated__', filename)

