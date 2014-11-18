# Copyright 2009-2014 MongoDB, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Tools for creating and manipulating SON, the Serialized Ocument Notation.

Regular dictionaries can be used instead of SON objects, but not when the order
of keys is important. A SON object can be used just like a normal Python
dictionary."""

import copy
import re

import bson

# This sort of sucks, but seems to be as good as it gets...
# This is essentially the same as re._pattern_type
RE_TYPE = type(re.compile(""))


try:
    import uuid
    _use_uuid = True
except ImportError:
    _use_uuid = False

def has_uuid():
    """Is the uuid module available?
    .. versionadded:: 2.3
    """
    return _use_uuid

class SON(dict):
    """SON data.

    A subclass of dict that maintains ordering of keys and provides a
    few extra niceties for dealing with SON. SON objects can be
    converted to and from BSON.

    The mapping from Python types to BSON types is as follows:

    =======================================  =============  ===================
    Python Type                              BSON Type      Supported Direction
    =======================================  =============  ===================
    None                                     null           both
    bool                                     boolean        both
    int [#int]_                              int32 / int64  py -> bson
    long                                     int64          both
    float                                    number (real)  both
    string                                   string         py -> bson
    unicode                                  string         both
    list                                     array          both
    dict / `SON`                             object         both
    datetime.datetime [#dt]_ [#dt2]_         date           both
    `bson.regex.Regex` / compiled re [#re]_  regex          both
    `bson.binary.Binary`                     binary         both
    `bson.objectid.ObjectId`                 oid            both
    `bson.dbref.DBRef`                       dbref          both
    None                                     undefined      bson -> py
    unicode                                  code           bson -> py
    `bson.code.Code`                         code           py -> bson
    unicode                                  symbol         bson -> py
    bytes (Python 3) [#bytes]_               binary         both
    =======================================  =============  ===================

    Note that to save binary data it must be wrapped as an instance of
    `bson.binary.Binary`. Otherwise it will be saved as a BSON string
    and retrieved as unicode.

    .. [#int] A Python int will be saved as a BSON int32 or BSON int64 depending
       on its size. A BSON int32 will always decode to a Python int. In Python 2.x
       a BSON int64 will always decode to a Python long. In Python 3.x a BSON
       int64 will decode to a Python int since there is no longer a long type.
    .. [#dt] datetime.datetime instances will be rounded to the nearest
       millisecond when saved
    .. [#dt2] all datetime.datetime instances are treated as *naive*. clients
       should always use UTC.
    .. [#re] :class:`~bson.regex.Regex` instances and regular expression
       objects from ``re.compile()`` are both saved as BSON regular expressions.
       BSON regular expressions are decoded as Python regular expressions by
       default, or as :class:`~bson.regex.Regex` instances if the ``compile_re``
       option is set to ``False``.
    .. [#bytes] The bytes type from Python 3.x is encoded as BSON binary with
       subtype 0. In Python 3.x it will be decoded back to bytes. In Python 2.x
       it will be decoded to an instance of :class:`~bson.binary.Binary` with
       subtype 0.
    """

    def __init__(self, data=None, **kwargs):
        self.__keys = []
        dict.__init__(self)
        self.update(data)
        self.update(kwargs)

    def __new__(cls, *args, **kwargs):
        instance = super(SON, cls).__new__(cls, *args, **kwargs)
        instance.__keys = []
        return instance

    def __repr__(self):
        result = []
        for key in self.__keys:
            result.append("(%r, %r)" % (key, self[key]))
        return "SON([%s])" % ", ".join(result)

    def __setitem__(self, key, value):
        if key not in self:
            self.__keys.append(key)
        dict.__setitem__(self, key, value)

    def __delitem__(self, key):
        self.__keys.remove(key)
        dict.__delitem__(self, key)

    def keys(self):
        return list(self.__keys)

    def copy(self):
        other = SON()
        other.update(self)
        return other

    # TODO this is all from UserDict.DictMixin. it could probably be made more
    # efficient.
    # second level definitions support higher levels
    def __iter__(self):
        for k in self.keys():
            yield k

    def has_key(self, key):
        return key in self.keys()

    def __contains__(self, key):
        return key in self.keys()

    # third level takes advantage of second level definitions
    def iteritems(self):
        for k in self:
            yield (k, self[k])

    def iterkeys(self):
        return self.__iter__()

    # fourth level uses definitions from lower levels
    def itervalues(self):
        for _, v in self.iteritems():
            yield v

    def values(self):
        return [v for _, v in self.iteritems()]

    def items(self):
        return [(key, self[key]) for key in self]

    def clear(self):
        for key in self.keys():
            del self[key]

    def setdefault(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            self[key] = default
        return default

    def pop(self, key, *args):
        if len(args) > 1:
            raise TypeError("pop expected at most 2 arguments, got "\
                                + repr(1 + len(args)))
        try:
            value = self[key]
        except KeyError:
            if args:
                return args[0]
            raise
        del self[key]
        return value

    def popitem(self):
        try:
            k, v = self.iteritems().next()
        except StopIteration:
            raise KeyError('container is empty')
        del self[k]
        return (k, v)

    def update(self, other=None, **kwargs):
        # Make progressively weaker assumptions about "other"
        if other is None:
            pass
        elif hasattr(other, 'iteritems'):  # iteritems saves memory and lookups
            for k, v in other.iteritems():
                self[k] = v
        elif hasattr(other, 'keys'):
            for k in other.keys():
                self[k] = other[k]
        else:
            for k, v in other:
                self[k] = v
        if kwargs:
            self.update(kwargs)

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default

    def __eq__(self, other):
        """Comparison to another SON is order-sensitive while comparison to a
        regular dictionary is order-insensitive.
        """
        if isinstance(other, SON):
            return len(self) == len(other) and self.items() == other.items()
        return self.to_dict() == other

    def __ne__(self, other):
        return not self == other

    def __len__(self):
        return len(self.keys())

    def to_dict(self):
        """Convert a SON document to a normal Python dictionary instance.

        This is trickier than just *dict(...)* because it needs to be
        recursive.
        """

        def transform_value(value):
            if isinstance(value, list):
                return [transform_value(v) for v in value]
            elif isinstance(value, dict):
                return dict([
                    (k, transform_value(v))
                    for k, v in value.iteritems()])
            else:
                return value

        return transform_value(dict(self))

    def __deepcopy__(self, memo):
        out = SON()
        val_id = id(self)
        if val_id in memo:
            return memo.get(val_id)
        memo[val_id] = out
        for k, v in self.iteritems():
            if not isinstance(v, RE_TYPE):
                v = copy.deepcopy(v, memo)
            out[k] = v
        return out





# Copyright 2009-2014 MongoDB, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Tools for using Python's :mod:`json` module with BSON documents.

This module provides two helper methods `dumps` and `loads` that wrap the
native :mod:`json` methods and provide explicit BSON conversion to and from
json.  This allows for specialized encoding and decoding of BSON documents
into `Mongo Extended JSON
<http://www.mongodb.org/display/DOCS/Mongo+Extended+JSON>`_'s *Strict*
mode.  This lets you encode / decode BSON documents to JSON even when
they use special BSON types.

Example usage (serialization):

.. doctest::

   >>> from bson import Binary, Code
   >>> from bson.json_util import dumps
   >>> dumps([{'foo': [1, 2]},
   ...        {'bar': {'hello': 'world'}},
   ...        {'code': Code("function x() { return 1; }")},
   ...        {'bin': Binary("\x01\x02\x03\x04")}])
   '[{"foo": [1, 2]}, {"bar": {"hello": "world"}}, {"code": {"$code": "function x() { return 1; }", "$scope": {}}}, {"bin": {"$binary": "AQIDBA==", "$type": "00"}}]'

Example usage (deserialization):

.. doctest::

   >>> from bson.json_util import loads
   >>> loads('[{"foo": [1, 2]}, {"bar": {"hello": "world"}}, {"code": {"$scope": {}, "$code": "function x() { return 1; }"}}, {"bin": {"$type": "00", "$binary": "AQIDBA=="}}]')
   [{u'foo': [1, 2]}, {u'bar': {u'hello': u'world'}}, {u'code': Code('function x() { return 1; }', {})}, {u'bin': Binary('...', 0)}]

Alternatively, you can manually pass the `default` to :func:`json.dumps`.
It won't handle :class:`~bson.binary.Binary` and :class:`~bson.code.Code`
instances (as they are extended strings you can't provide custom defaults),
but it will be faster as there is less recursion.

.. versionchanged:: 2.7
   Preserves order when rendering SON, Timestamp, Code, Binary, and DBRef
   instances. (But not in Python 2.4.)

.. versionchanged:: 2.3
   Added dumps and loads helpers to automatically handle conversion to and
   from json and supports :class:`~bson.binary.Binary` and
   :class:`~bson.code.Code`

.. versionchanged:: 1.9
   Handle :class:`uuid.UUID` instances, whenever possible.

.. versionchanged:: 1.8
   Handle timezone aware datetime instances on encode, decode to
   timezone aware datetime instances.

.. versionchanged:: 1.8
   Added support for encoding/decoding :class:`~bson.max_key.MaxKey`
   and :class:`~bson.min_key.MinKey`, and for encoding
   :class:`~bson.timestamp.Timestamp`.

.. versionchanged:: 1.2
   Added support for encoding/decoding datetimes and regular expressions.
"""

import base64
import calendar
import datetime
import re

json_lib = True
try:
    import json
except ImportError:
    try:
        import simplejson as json
    except ImportError:
        json_lib = False

# import bson
# from bson import EPOCH_AWARE, RE_TYPE, SON
#from bson.binary import Binary
# from bson.code import Code
# from bson.dbref import DBRef
# from bson.max_key import MaxKey
# from bson.min_key import MinKey
# from bson.objectid import ObjectId
# from bson.regex import Regex
# from bson.timestamp import Timestamp

# from bson.py3compat import PY3, binary_type, string_types
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

def b(s):
    # See comments above. In python 2.x b('foo') is just 'foo'.
    return s

def bytes_from_hex(h):
    return h.decode('hex')

binary_type = str
# 2to3 will convert this to "str". That's okay
# since we won't ever get here under python3.
text_type   = unicode
next_item   = "next"


try:
    from uuid import UUID
except ImportError:
    # Python2.4 doesn't have a uuid module.
    pass

# from bson.py3compat import PY3, binary_type

"""Tools for representing BSON binary data.
"""

BINARY_SUBTYPE = 0
"""BSON binary subtype for binary data.

This is the default subtype for binary data.

.. versionadded:: 1.5
"""

FUNCTION_SUBTYPE = 1
"""BSON binary subtype for functions.

.. versionadded:: 1.5
"""

OLD_BINARY_SUBTYPE = 2
"""Old BSON binary subtype for binary data.

This is the old default subtype, the current
default is :data:`BINARY_SUBTYPE`.

.. versionadded:: 1.7
"""

OLD_UUID_SUBTYPE = 3
"""Old BSON binary subtype for a UUID.

:class:`uuid.UUID` instances will automatically be encoded
by :mod:`bson` using this subtype.

.. versionadded:: 2.1
"""

UUID_SUBTYPE = 4
"""BSON binary subtype for a UUID.

This is the new BSON binary subtype for UUIDs. The
current default is :data:`OLD_UUID_SUBTYPE` but will
change to this in a future release.

.. versionchanged:: 2.1
   Changed to subtype 4.
.. versionadded:: 1.5
"""

JAVA_LEGACY = 5
"""Used with :attr:`pymongo.collection.Collection.uuid_subtype`
to specify that UUIDs should be stored in the legacy byte order
used by the Java driver.

:class:`uuid.UUID` instances will automatically be encoded
by :mod:`bson` using :data:`OLD_UUID_SUBTYPE`.

.. versionadded:: 2.3
"""

CSHARP_LEGACY = 6
"""Used with :attr:`pymongo.collection.Collection.uuid_subtype`
to specify that UUIDs should be stored in the legacy byte order
used by the C# driver.

:class:`uuid.UUID` instances will automatically be encoded
by :mod:`bson` using :data:`OLD_UUID_SUBTYPE`.

.. versionadded:: 2.3
"""

ALL_UUID_SUBTYPES = (OLD_UUID_SUBTYPE, UUID_SUBTYPE, JAVA_LEGACY, CSHARP_LEGACY)

MD5_SUBTYPE = 5
"""BSON binary subtype for an MD5 hash.

.. versionadded:: 1.5
"""

USER_DEFINED_SUBTYPE = 128
"""BSON binary subtype for any user defined structure.

.. versionadded:: 1.5
"""


class Binary(binary_type):
    """Representation of BSON binary data.

    This is necessary because we want to represent Python strings as
    the BSON string type. We need to wrap binary data so we can tell
    the difference between what should be considered binary data and
    what should be considered a string when we encode to BSON.

    Raises TypeError if `data` is not an instance of :class:`str`
    (:class:`bytes` in python 3) or `subtype` is not an instance of
    :class:`int`. Raises ValueError if `subtype` is not in [0, 256).

    .. note::
      In python 3 instances of Binary with subtype 0 will be decoded
      directly to :class:`bytes`.

    :Parameters:
      - `data`: the binary data to represent
      - `subtype` (optional): the `binary subtype
        <http://bsonspec.org/#/specification>`_
        to use
    """

    _type_marker = 5

    def __new__(cls, data, subtype=BINARY_SUBTYPE):
        if not isinstance(data, binary_type):
            raise TypeError("data must be an "
                            "instance of %s" % (binary_type.__name__,))
        if not isinstance(subtype, int):
            raise TypeError("subtype must be an instance of int")
        if subtype >= 256 or subtype < 0:
            raise ValueError("subtype must be contained in [0, 256)")
        self = binary_type.__new__(cls, data)
        self.__subtype = subtype
        return self

    @property
    def subtype(self):
        """Subtype of this binary data.
        """
        return self.__subtype

    def __getnewargs__(self):
        # Work around http://bugs.python.org/issue7382
        data = super(Binary, self).__getnewargs__()[0]
        if PY3 and not isinstance(data, binary_type):
            data = data.encode('latin-1')
        return data, self.__subtype

    def __eq__(self, other):
        if isinstance(other, Binary):
            return ((self.__subtype, binary_type(self)) ==
                    (other.subtype, binary_type(other)))
        # We don't return NotImplemented here because if we did then
        # Binary("foo") == "foo" would return True, since Binary is a
        # subclass of str...
        return False

    def __ne__(self, other):
        return not self == other

    def __repr__(self):
        return "Binary(%s, %s)" % (binary_type.__repr__(self), self.__subtype)


class UUIDLegacy(Binary):
    """UUID wrapper to support working with UUIDs stored as legacy
    BSON binary subtype 3.

    .. doctest::

      >>> import uuid
      >>> from bson.binary import Binary, UUIDLegacy, UUID_SUBTYPE
      >>> my_uuid = uuid.uuid4()
      >>> coll = db.test
      >>> coll.uuid_subtype = UUID_SUBTYPE
      >>> coll.insert({'uuid': Binary(my_uuid.bytes, 3)})
      ObjectId('...')
      >>> coll.find({'uuid': my_uuid}).count()
      0
      >>> coll.find({'uuid': UUIDLegacy(my_uuid)}).count()
      1
      >>> coll.find({'uuid': UUIDLegacy(my_uuid)})[0]['uuid']
      UUID('...')
      >>>
      >>> # Convert from subtype 3 to subtype 4
      >>> doc = coll.find_one({'uuid': UUIDLegacy(my_uuid)})
      >>> coll.save(doc)
      ObjectId('...')
      >>> coll.find({'uuid': UUIDLegacy(my_uuid)}).count()
      0
      >>> coll.find({'uuid': {'$in': [UUIDLegacy(my_uuid), my_uuid]}}).count()
      1
      >>> coll.find_one({'uuid': my_uuid})['uuid']
      UUID('...')

    Raises TypeError if `obj` is not an instance of :class:`~uuid.UUID`.

    :Parameters:
      - `obj`: An instance of :class:`~uuid.UUID`.
    """

    def __new__(cls, obj):
        if not isinstance(obj, UUID):
            raise TypeError("obj must be an instance of uuid.UUID")
        # Python 3.0(.1) returns a bytearray instance for bytes (3.1 and
        # newer just return a bytes instance). Convert that to binary_type
        # for compatibility.
        self = Binary.__new__(cls, binary_type(obj.bytes), OLD_UUID_SUBTYPE)
        self.__uuid = obj
        return self

    def __getnewargs__(self):
        # Support copy and deepcopy
        return (self.__uuid,)

    @property
    def uuid(self):
        """UUID instance wrapped by this UUIDLegacy instance.
        """
        return self.__uuid

    def __repr__(self):
        return "UUIDLegacy('%s')" % self.__uuid


# _RE_OPT_TABLE = {
#     "i": re.I,
#     "l": re.L,
#     "m": re.M,
#     "s": re.S,
#     "u": re.U,
#     "x": re.X,
# }


def dumps(obj, *args, **kwargs):
    """Helper function that wraps :class:`json.dumps`.

    Recursive function that handles all BSON types including
    :class:`~bson.binary.Binary` and :class:`~bson.code.Code`.

    .. versionchanged:: 2.7
       Preserves order when rendering SON, Timestamp, Code, Binary, and DBRef
       instances. (But not in Python 2.4.)
    """
    if not json_lib:
        raise Exception("No json library available")
    return json.dumps(_json_convert(obj), *args, **kwargs)


def loads(s, *args, **kwargs):
    """Helper function that wraps :class:`json.loads`.

    Automatically passes the object_hook for BSON type conversion.

    :Parameters:
      - `compile_re` (optional): if ``False``, don't attempt to compile BSON
        regular expressions into Python regular expressions. Return instances
        of :class:`~bson.bsonregex.BSONRegex` instead.

    .. versionchanged:: 2.7
       Added ``compile_re`` option.
    """
    if not json_lib:
        raise Exception("No json library available")

    compile_re = kwargs.pop('compile_re', True)
    kwargs['object_hook'] = lambda dct: object_hook(dct, compile_re)
    return json.loads(s, *args, **kwargs)


def _json_convert(obj):
    """Recursive helper method that converts BSON types so they can be
    converted into json.
    """
    if hasattr(obj, 'iteritems') or hasattr(obj, 'items'):  # PY3 support
        return SON(((k, _json_convert(v)) for k, v in obj.iteritems()))
    elif hasattr(obj, '__iter__') and not isinstance(obj, string_types):
        return list((_json_convert(v) for v in obj))
    try:
        return default(obj)
    except TypeError:
        return obj


def object_hook(dct, compile_re=True):
    if "$oid" in dct:
        return ObjectId(str(dct["$oid"]))
    if "$ref" in dct:
        return DBRef(dct["$ref"], dct["$id"], dct.get("$db", None))
    if "$date" in dct:
        secs = float(dct["$date"]) / 1000.0
        return EPOCH_AWARE + datetime.timedelta(seconds=secs)
    if "$regex" in dct:
        flags = 0
        # PyMongo always adds $options but some other tools may not.
        for opt in dct.get("$options", ""):
            flags |= _RE_OPT_TABLE.get(opt, 0)

        if compile_re:
            return re.compile(dct["$regex"], flags)
        else:
            return Regex(dct["$regex"], flags)
    if "$minKey" in dct:
        return MinKey()
    if "$maxKey" in dct:
        return MaxKey()
    if "$binary" in dct:
        if isinstance(dct["$type"], int):
            dct["$type"] = "%02x" % dct["$type"]
        subtype = int(dct["$type"], 16)
        if subtype >= 0xffffff80:  # Handle mongoexport values
            subtype = int(dct["$type"][6:], 16)
        return Binary(base64.b64decode(dct["$binary"].encode()), subtype)
    if "$code" in dct:
        return Code(dct["$code"], dct.get("$scope"))
    #if bson.has_uuid() and "$uuid" in dct:
    if has_uuid() and "$uuid" in dct:
        return uuid.UUID(dct["$uuid"])
        #return bson.uuid.UUID(dct["$uuid"])
    return dct


def default(obj):
    # We preserve key order when rendering SON, DBRef, etc. as JSON by
    # returning a SON for those types instead of a dict. This works with
    # the "json" standard library in Python 2.6+ and with simplejson
    # 2.1.0+ in Python 2.5+, because those libraries iterate the SON
    # using PyIter_Next. Python 2.4 must use simplejson 2.0.9 or older,
    # and those versions of simplejson use the lower-level PyDict_Next,
    # which bypasses SON's order-preserving iteration, so we lose key
    # order in Python 2.4.
    if isinstance(obj, Binary):
        return SON([
            ('$binary', base64.b64encode(obj).decode()),
            ('$type', "%02x" % obj.subtype)])
    # if isinstance(obj, ObjectId):
    #     return {"$oid": str(obj)}
    # if isinstance(obj, DBRef):
    #     return _json_convert(obj.as_doc())
    # if isinstance(obj, datetime.datetime):
    #     # TODO share this code w/ bson.py?
    #     if obj.utcoffset() is not None:
    #         obj = obj - obj.utcoffset()
    #     millis = int(calendar.timegm(obj.timetuple()) * 1000 +
    #                  obj.microsecond / 1000)
    #     return {"$date": millis}
    # if isinstance(obj, (RE_TYPE, Regex)):
    #     flags = ""
    #     if obj.flags & re.IGNORECASE:
    #         flags += "i"
    #     if obj.flags & re.LOCALE:
    #         flags += "l"
    #     if obj.flags & re.MULTILINE:
    #         flags += "m"
    #     if obj.flags & re.DOTALL:
    #         flags += "s"
    #     if obj.flags & re.UNICODE:
    #         flags += "u"
    #     if obj.flags & re.VERBOSE:
    #         flags += "x"
    #     if isinstance(obj.pattern, unicode):
    #         pattern = obj.pattern
    #     else:
    #         pattern = obj.pattern.decode('utf-8')
    #     return SON([("$regex", pattern), ("$options", flags)])
    # if isinstance(obj, MinKey):
    #     return {"$minKey": 1}
    # if isinstance(obj, MaxKey):
    #     return {"$maxKey": 1}
    # if isinstance(obj, Timestamp):
    #     return SON([("t", obj.time), ("i", obj.inc)])
    # if isinstance(obj, Code):
    #     return SON([('$code', str(obj)), ('$scope', obj.scope)])
    # if PY3 and isinstance(obj, binary_type):
    #     return SON([
    #         ('$binary', base64.b64encode(obj).decode()),
    #         ('$type', "00")])
    # if bson.has_uuid() and isinstance(obj, bson.uuid.UUID):
    #     return {"$uuid": obj.hex}
    # raise TypeError("%r is not JSON serializable" % obj)
