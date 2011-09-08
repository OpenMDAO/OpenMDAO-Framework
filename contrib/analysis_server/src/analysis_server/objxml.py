"""
Support for XML object data messaging between ModelCenter and AnalysisServer.
"""

import xml.etree.cElementTree as ElementTree
from xml.sax.saxutils import escape

from openmdao.main.api import Container
from openmdao.lib.datatypes.api import Bool, Float, Int, Str

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
    for name, trait in sorted(container._alltraits().items(),
                              key=lambda item: item[0]):
        if name in _IGNORE_ATTR or name.startswith('_'):
            continue
        val = getattr(container, name)
        if isinstance(val, Container):
            xml.append("""\
<member name="%s" type="object" access="public" className="%s">\
""" % (name, _lookup(val)))
            _get_as_xml(val, name, xml)
            xml.append('</member>')
        else:
            desc = trait.desc or ''
            desc = escape(desc.encode('string_escape'))

            ttype = trait.trait_type
            if isinstance(ttype, Bool):
                valstr = 'true' if val else 'false'
                xml.append("""\
<member name="%s" type="boolean" access="public">%s\
<properties>\
<property name="description">%s</property>\
</properties>\
</member>""" % (name, valstr, desc))

            elif isinstance(ttype, Float):
                valstr = _float2str(val)
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

            elif isinstance(ttype, Int):
                valstr = str(val)
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

            elif isinstance(ttype, Str):
                valstr = escape(val.encode('string_escape'))
                xml.append("""\
<member name="%s" type="string" access="public">%s\
<properties>\
<property name="description">%s</property>\
<property name="units"/>\
<property name="enumValues"/>\
<property name="enumAliases"/>\
</properties>\
</member>""" % (name, valstr, desc))

            else:
                raise RuntimeError('%s: unsupported type' % name)

    xml.append('</members>')


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
        typ = member.attrib['type']
        obj = getattr(container, name)
        if isinstance(obj, Container):
            _set_from_xml(obj, member)
        else:
            trait = container.get_dyn_trait(name)
            ttype = trait.trait_type
            if isinstance(ttype, Float):
                if typ == 'double':
                    val = float(member.text)
                    setattr(container, name, val)
                else:
                    raise RuntimeError('Unsupported type %r for %s'
                                       % (typ, name))
            elif isinstance(ttype, Int):
                if typ == 'long':
                    val = int(member.text)
                    setattr(container, name, val)
                else:
                    raise RuntimeError('Unsupported type %r for %s'
                                       % (typ, name))
            elif isinstance(ttype, Str):
                if typ == 'string':
                    val = member.text.strip()
                    setattr(container, name, val)
                else:
                    raise RuntimeError('Unsupported type %r for %s'
                                       % (typ, name))
            else:
                raise RuntimeError('Unsupported type %r for %s'
                                   % (ttype, name))

