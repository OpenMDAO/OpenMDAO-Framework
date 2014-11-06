
def deep_hasattr(obj, pathname):
    """Returns True if the attrbute indicated by the given pathname
    exists; False otherwise.
    """
    try:
        parts = pathname.split('.')
        for name in parts[:-1]:
            obj = getattr(obj, name)
    except Exception:
        return False
    return hasattr(obj, parts[-1])


def deep_getattr(obj, pathname):
    """Returns a tuple of the form (value, restofpath), if restofpath
    is None, value is the actual desired value, else it's the closest
    containing object in this process, and restofpath will contain
    the string that would resolve the desired object within the
    containing object.  pathname is a dotted path string and obj
    is the starting containing object.
    """
    parts = pathname.split('.')

    for i, name in enumerate(parts):
        try:
            obj = getattr(obj, name)
        except AttributeError:
            return (obj, '.'.join(parts[i:]))

    return (obj, None)
