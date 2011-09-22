"""
Support for XML object data messaging between ModelCenter and AnalysisServer.
"""

import numpy
import xml.etree.cElementTree as ElementTree
from xml.sax.saxutils import escape

from openmdao.main.api import Container
from openmdao.main.mp_support import is_instance
from openmdao.lib.datatypes.api import Array, Bool, Enum, Float, Int, List, Str

# Attributes to ignore.
_IGNORE_ATTR = ('iotype',)


# Map from class to class name.
_CLASS_MAP = {}

def register(cls, name):
    """ Register `name` for `cls`. """
    _CLASS_MAP[cls] = name

def _lookup(obj):
    """ Return class name for `obj`. """
    cls = obj.__class__
    try:
        return _CLASS_MAP[cls]
    except KeyError:
        return '%s.%s' % (cls.__module__, cls.__name__)


def _float2str(val):
    """ Return full-precision string rep for `val`. """
    # In Python2.7 this is obsoleted by just '%r'.
    return '%.16g' % val


def get_as_xml(container, name):
    """ Return XML for `container` with `name`. """
    xml = []
    xml.append("""\
<?xml version="1.0" encoding="utf-8"?>\
<Object className="%s" type="object" nonStrictType="false" customSerialization="false">""" % _lookup(container))
    _get_as_xml(container, name, xml)
    xml.append('</Object>')
    return ''.join(xml)

def _get_as_xml(container, name, xml):
    """ Recursive helper for :meth:`get_as_xml`. """
    xml.append('<members>')
    # Using ._alltraits().items() since .items() returns a generator.
    for name, trait in sorted(container._alltraits().items(),
                              key=lambda item: item[0]):
        if name in _IGNORE_ATTR or name.startswith('_'):
            continue
        val = getattr(container, name)
        if is_instance(val, Container):
            xml.append("""\
<member name="%s" type="object" access="public" className="%s">\
""" % (name, _lookup(val)))
            _get_as_xml(val, name, xml)
            xml.append('</member>')
        else:
            ttype = trait.trait_type
            if isinstance(ttype, Array):
                _get_array(name, val, trait, xml, True)
            elif isinstance(ttype, List):
                _get_array(name, val, trait, xml, False)
            elif isinstance(ttype, Bool):
                _get_bool(name, val, trait, xml)
            elif isinstance(ttype, Enum):
                _get_enum(name, val, trait, xml)
            elif isinstance(ttype, Float):
                _get_float(name, val, trait, xml)
            elif isinstance(ttype, Int):
                _get_int(name, val, trait, xml)
            elif isinstance(ttype, Str):
                _get_str(name, val, trait, xml)
            else:
                raise RuntimeError('%s.%s: unsupported type'
                                   % (container.get_pathname(), name))
    xml.append('</members>')


def _get_array(name, val, trait, xml, is_array):
    """ Helper for :meth:`_get_as_xml`. """
    if is_array:
        converters = {'f':float, 'i':int, 'S':str}
        kind = val.dtype.kind
        try:
            typ = converters[kind]
        except KeyError:
            raise RuntimeError('%s.%s: unsupported dtype %r (%r)'
                               % (container.get_pathname(), name,
                                  val.dtype, kind))
    elif val:
        typ = type(val[0])
    else:
        typ = str  # HACK!

    if typ is float:
        fmt = '%.16g'
        valtyp = 'double[]'
    elif typ is int:
        fmt = '%d'
        valtyp = 'long[]'
    else:
        fmt = '"%s"'
        valtyp = 'string[]'

    if is_array and len(val.shape) > 1:
        valstr = 'bounds[%s] {%s}' % (
                 ', '.join(['%d' % dim for dim in val.shape]),
                 ', '.join([fmt % value for value in val.flat]))
    else:
        valstr = ', '.join([fmt % value for value in val])
    if typ is str:
        valstr = escape(valstr.encode('string_escape'))

    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))

    if len(val):
        if is_array and len(val.shape) > 1:
            first = '%s' % val.flat[0]
        else:
            first = '%s' % val[0]
        if typ is str:
            first = escape(first.encode('string_escape'))
    else:
        first = ''

    if is_array:
        dims = ', '.join(['%d' % dim for dim in val.shape])
        length = '%d' % val.shape[0]
        ndims = '%d' % len(val.shape)
    else:
        dims = '%d' % len(val)
        length = '%d' % len(val)
        ndims = '1'

    xml.append("""\
<member name="%s" type="%s" access="public">%s\
<properties>\
<property name="description">%s</property>\
<property name="dimensions">%s</property>\
<property name="enumAliases"/>\
<property name="enumValues"/>\
<property name="first">%s</property>\
<property name="length">%s</property>\
<property name="lockResize">true</property>\
<property name="numDimensions">%s</property>\
""" % (name, valtyp, valstr, desc, dims, first, length, ndims))

    if typ is not str:
        units = trait.units or ''
        hlb = 'false' if trait.low is None else 'true'
        hub = 'false' if trait.high is None else 'true'
        if typ is float:
            low = '0.0' if trait.low is None else _float2str(trait.low)
            high = '0.0' if trait.high is None else _float2str(trait.high)
        else:
            low = '0' if trait.low is None else str(trait.low)
            high = '0' if trait.high is None else str(trait.high)

        xml.append("""\
<property name="units">%s</property>\
<property name="hasLowerBound">%s</property>\
<property name="lowerBound">%s</property>\
<property name="hasUpperBound">%s</property>\
<property name="upperBound">%s</property>\
""" % (units, hlb, low, hub, high))

    xml.append("""\
</properties>\
</member>""")


def _get_bool(name, val, trait, xml):
    """ Helper for :meth:`_get_as_xml`. """
    valstr = 'true' if val else 'false'
    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))

    xml.append("""\
<member name="%s" type="boolean" access="public">%s\
<properties>\
<property name="description">%s</property>\
</properties>\
</member>""" % (name, valstr, desc))


def _get_enum(name, val, trait, xml):
    """ Helper for :meth:`_get_as_xml`. """
    etyp = type(trait.values[0])
    if etyp == float:
        valtyp = 'double'
        valstr = _float2str(val)
        values = ', '.join([_float2str(value) for value in trait.values])
    elif etyp == int:
        valtyp = 'long'
        valstr = str(val)
        values = ', '.join(['%s' % value for value in trait.values])
    else:
        valtyp = 'string'
        valstr = escape(val.encode('string_escape'))
        values = ', '.join(['"%s"' % value for value in trait.values])

    if trait.aliases:
        aliases = ', '.join(['"%s"' % alias for alias in trait.aliases])
    else:
        aliases = ''

    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))
    units = trait.units or ''

    xml.append("""\
<member name="%s" type="%s" access="public">%s\
<properties>\
<property name="description">%s</property>\
<property name="units">%s</property>\
<property name="enumValues">%s</property>\
<property name="enumAliases">%s</property>\
<property name="hasLowerBound">false</property>\
<property name="lowerBound"/>\
<property name="hasUpperBound">false</property>\
<property name="upperBound"/>\
</properties>\
</member>""" % (name, valtyp, valstr, desc, units, values, aliases))


def _get_float(name, val, trait, xml):
    """ Helper for :meth:`_get_as_xml`. """
    valstr = _float2str(val)
    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))
    units = trait.units or ''
    hlb = 'false' if trait.low is None else 'true'
    low = '0.0' if trait.low is None else _float2str(trait.low)
    hub = 'false' if trait.high is None else 'true'
    high = '0.0' if trait.high is None else _float2str(trait.high)

    xml.append("""\
<member name="%s" type="double" access="public">%s\
<properties>\
<property name="description">%s</property>\
<property name="units">%s</property>\
<property name="enumValues"/>\
<property name="enumAliases"/>\
<property name="hasLowerBound">%s</property>\
<property name="lowerBound">%s</property>\
<property name="hasUpperBound">%s</property>\
<property name="upperBound">%s</property>\
</properties>\
</member>""" % (name, valstr, desc, units, hlb, low, hub, high))


def _get_int(name, val, trait, xml):
    """ Helper for :meth:`_get_as_xml`. """
    valstr = str(val)
    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))
    hlb = 'false' if trait.low is None else 'true'
    low = '0' if trait.low is None else str(trait.low)
    hub = 'false' if trait.high is None else 'true'
    high = '0' if trait.high is None else str(trait.high)

    xml.append("""\
<member access="public" name="%s" type="long">%s\
<properties>\
<property name="description">%s</property>\
<property name="units"/>\
<property name="enumValues"/>\
<property name="enumAliases"/>\
<property name="hasLowerBound">%s</property>\
<property name="lowerBound">%s</property>\
<property name="hasUpperBound">%s</property>\
<property name="upperBound">%s</property>\
</properties>\
</member>""" % (name, valstr, desc, hlb, low, hub, high))


def _get_str(name, val, trait, xml):
    """ Helper for :meth:`_get_as_xml`. """
    valstr = escape(val.encode('string_escape'))
    desc = trait.desc or ''
    desc = escape(desc.encode('string_escape'))

    xml.append("""\
<member name="%s" type="string" access="public">%s\
<properties>\
<property name="description">%s</property>\
<property name="enumValues"/>\
<property name="enumAliases"/>\
</properties>\
</member>""" % (name, valstr, desc))



def set_from_xml(container, xml):
    """ Set values in `container` from `xml`. """
    start = xml.find('<Object')
    xml = xml[start:]
    root = ElementTree.fromstring(xml)
    _set_from_xml(container, root)

def _set_from_xml(container, root):
    """ Recursive helper for :meth:`set_from_xml`. """
    members = root.find('members')
    for member in members.findall('member'):
        name = member.attrib['name']
        obj = getattr(container, name)
        if isinstance(obj, Container):
            _set_from_xml(obj, member)
        else:
            trait = container.get_dyn_trait(name)
            ttype = trait.trait_type
            if isinstance(ttype, Array):
                _set_array(container, name, member, True)
            elif isinstance(ttype, List):
                _set_array(container, name, member, False)
            elif isinstance(ttype, Bool):
                _set_bool(container, name, member)
            elif isinstance(ttype, Enum):
                try:
                    i = trait.aliases.index(member.text)
                except (AttributeError, ValueError):
                    etyp = type(trait.values[0])
                    if etyp == float:
                        _set_float(container, name, member)
                    elif etyp == int:
                        _set_int(container, name, member)
                    else:
                        _set_str(container, name, member)
                else:
                    setattr(container, name, trait.values[i])
            elif isinstance(ttype, Float):
                _set_float(container, name, member)
            elif isinstance(ttype, Int):
                _set_int(container, name, member)
            elif isinstance(ttype, Str):
                _set_str(container, name, member)
            else:
                raise RuntimeError('Unsupported type %r for %s.%s'
                                   % (ttype, container.get_pathname(), name))


def _set_array(container, name, member, is_array):
    """ Helper for :meth:`_set_from_xml`. """
    if is_array:
        converters = {'f':float, 'i':int, 'S':str}
        val = getattr(container, name)
        kind = val.dtype.kind
        try:
            dtyp = converters[kind]
        except KeyError:
            raise RuntimeError('Unsupported dtype %r (%r) for %s.%s'
                               % (val.dtype, kind,
                                  container.get_pathname(), name))
    else:
        dtyp = str  # HACK!

    text = member.text
    if dtyp == str:
        text = text.decode('string_escape')

    if is_array:
        if text.startswith('bounds['):
            dims, rbrack, rest = text[7:].partition(']')
            dims = [int(val.strip(' "')) for val in dims.split(',')]
            junk, lbrace, rest = rest.partition('{')
            data, rbrace, rest = rest.partition('}')
            value = numpy.array([dtyp(val.strip(' "'))
                                 for val in data.split(',')]).reshape(dims)
        else:
            value = numpy.array([dtyp(val.strip(' "'))
                                 for val in text.split(',')])
    else:
        value = [dtyp(val.strip(' "')) for val in text.split(',')]

    setattr(container, name, value)


def _set_bool(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'boolean':
        val = True if member.text == 'true' else False
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))

def _set_float(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'double':
        val = float(member.text)
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))

def _set_int(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'long':
        val = int(member.text)
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))

def _set_str(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'string':
        val = member.text.strip()
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))

