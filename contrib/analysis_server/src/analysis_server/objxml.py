"""
Support for XML object data messaging between ModelCenter and AnalysisServer.
"""

import numpy
import xml.etree.cElementTree as ElementTree
from xml.sax.saxutils import escape

from openmdao.main.api import Container, VariableTree
from openmdao.main.mp_support import is_instance
from openmdao.main.datatypes.api import Array, Bool, Enum, Float, Int, List, Str

from analysis_server.units import get_translation

# Attributes to ignore.
_IGNORE_ATTR = ('iotype',)


# Map from class to class name.
_CLASS_MAP = {}

# Not currently used. Intended for supporting specified AnalysisServer names.
'''
def register(cls, name):
    """ Register `name` for `cls`. """
    _CLASS_MAP[cls] = name
'''

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


def get_as_xml(container):
    """
    Return XML for `container`.

    container: Container
        Object to return XML representation of.
    """
    xml = []
    xml.append("""\
<?xml version="1.0" encoding="utf-8"?>\
<Object className="%s" type="object" nonStrictType="false" customSerialization="false">""" % _lookup(container))
    _get_as_xml(container, xml)
    xml.append('</Object>')
    return ''.join(xml)

def _get_as_xml(container, xml):
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
            _get_as_xml(val, xml)
            xml.append('</member>')
        else:
            ttype = trait.trait_type
            if isinstance(ttype, Array):
                _get_array(name, val, trait, xml, True, container)
            elif isinstance(ttype, List):
                _get_array(name, val, trait, xml, False, container)
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


def _get_array(name, val, trait, xml, is_array, container):
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
    else:
        inner_traits = trait.trait_type.inner_traits()
        if inner_traits:
            inner_type = inner_traits[0].trait_type
            if isinstance(inner_type, Float):
                typ = float
            elif isinstance(inner_type, Int):
                typ = int
            elif isinstance(inner_type, Str):
                typ = str
            else:
                raise TypeError('%s.%s: unsupported List element type %r'
                                % (container.get_pathname(), name, inner_type))
        else:
            raise TypeError('%s.%s: undefined List element type'
                            % (container.get_pathname(), name))

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
            first = _float2str(val.flat[0])
        else:
            first = str(val[0])
        if typ is str:
            first = escape(first.encode('string_escape'))
    else:
        first = ''

    if is_array:
        dims = ', '.join(['%d' % dim for dim in val.shape])
        length = '%d' % val.shape[0]
        lock_resize = 'true'
        ndims = '%d' % len(val.shape)
    else:
        dims = '%d' % len(val)
        length = '%d' % len(val)
        lock_resize = 'false'
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
<property name="lockResize">%s</property>\
<property name="numDimensions">%s</property>\
""" % (name, valtyp, valstr, desc, dims, first, length, lock_resize, ndims))

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
    """
    Set values in `container` from `xml`.

    container: Container
        Contains variables to be set.

    xml: string
        Representation of values to be set.
    """
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
            typ = converters[kind]
        except KeyError:
            raise RuntimeError('Unsupported dtype %r (%r) for %s.%s'
                               % (val.dtype, kind,
                                  container.get_pathname(), name))
    else:
        trait = container.get_dyn_trait(name)
        inner_traits = trait.trait_type.inner_traits()
        if inner_traits:
            inner_type = inner_traits[0].trait_type
            if isinstance(inner_type, Float):
                typ = float
            elif isinstance(inner_type, Int):
                typ = int
            elif isinstance(inner_type, Str):
                typ = str
            else:
                raise TypeError('%s.%s: unsupported List element type %r'
                                % (container.get_pathname(), name, inner_type))
        else:
            raise TypeError('%s.%s: undefined List element type'
                            % (container.get_pathname(), name))

    text = member.text or ''
    if typ == str:
        text = text.decode('string_escape')

    if is_array:
        if text.startswith('bounds['):
            dims, rbrack, rest = text[7:].partition(']')
            dims = [int(val.strip(' "')) for val in dims.split(',')]
            junk, lbrace, rest = rest.partition('{')
            data, rbrace, rest = rest.partition('}')
            value = numpy.array([typ(val.strip(' "'))
                                 for val in data.split(',')]).reshape(dims)
        elif text:
            value = numpy.array([typ(val.strip(' "'))
                                 for val in text.split(',')])
        else:
            value = numpy.array([])
    else:
        if text:
            value = [typ(val.strip(' "')) for val in text.split(',')]
        else:
            value = []

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
        val = float(member.text or '')
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))


def _set_int(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'long':
        val = int(member.text or '')
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))


def _set_str(container, name, member):
    """ Helper for :meth:`_set_from_xml`. """
    typ = member.attrib['type']
    if typ == 'string':
        text = member.text or ''
        val = text.decode('string_escape')
        setattr(container, name, val)
    else:
        raise RuntimeError('Unsupported type %r for %s.%s'
                           % (typ, container.get_pathname(), name))



def populate_from_xml(vartree, xml):
    """
    Populate `vartree` from `xml`.

    vartree: VariableTree
        Tree to be populated.

    xml: string
        Representation of tree structure.
    """
    start = xml.find('<Object')
    xml = xml[start:]
    root = ElementTree.fromstring(xml)
    _populate_from_xml(vartree, root)

def _populate_from_xml(vartree, root):
    """ Recursive helper for :meth:`populate_from_xml`. """
    members = root.find('members')
    for member in members.findall('member'):
        typ = member.attrib['type']
        if typ == 'object':
            _add_object(vartree, member)
        else:
            properties = member.find('properties')
            props = {}
            enum = False
            for property in properties.findall('property'):
                name = property.attrib['name']
                text = property.text
                if name in ('enumValues', 'enumAliases') and text:
                    enum = True
                props[name] = text
            if enum:
                _add_enum(vartree, member, props)
            else:
                if typ in ('double[]', 'long[]', 'string[]'):
                    _add_array(vartree, member, props)
                elif typ == 'boolean':
                    _add_bool(vartree, member, props)
                elif typ == 'double':
                    _add_float(vartree, member, props)
                elif typ == 'long':
                    _add_int(vartree, member, props)
                elif typ == 'string':
                    _add_str(vartree, member, props)
                else:
                    raise RuntimeError('unsupported member type %r' % typ)


def _add_array(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    typ = member.attrib['type']
    cvt = {'double[]':float, 'long[]':int, 'string[]':str}[typ]
    if props.get('lockResize') == 'false' and props.get('numDimensions') == '1':
        objtyp = 'List'
    else:
        objtyp = 'Array'
    text = member.text or ''
    args = {}

    if objtyp == 'Array':
        args['dtype'] = cvt
        if text.startswith('bounds['):
            dims, rbrack, rest = text[7:].partition(']')
            dims = [int(val.strip(' "')) for val in dims.split(',')]
            junk, lbrace, rest = rest.partition('{')
            data, rbrace, rest = rest.partition('}')
            value = numpy.array([cvt(val.strip(' "'))
                                 for val in data.split(',')]).reshape(dims)
        elif text:
            value = numpy.array([cvt(val.strip(' "'))
                                 for val in text.split(',')])
        else:
            value = numpy.array([])
        args['default_value'] = value
    else:
        args['trait'] = {'double[]':Float, 'long[]':Int, 'string[]':Str}[typ]
        if text:
            value = [cvt(val.strip(' "')) for val in text.split(',')]
            args['value'] = value

    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    units = props.get('units')
    if units:
        args['units'] = get_translation(units)
    low = props.get('hasLowerBound')
    if low == 'true':
        args['low'] = cvt(props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args['high'] = cvt(props.get('upperBound'))

    if objtyp == 'Array':
        vartree.add(name, Array(**args))
    else:
        vartree.add(name, List(**args))


def _add_bool(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    args = {}
    args['default_value'] = member.text == 'true'
    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    vartree.add(name, Bool(**args))


def _add_enum(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    typ = member.attrib['type']
    cvt = {'double':float, 'long':int, 'string':str}[typ]
    args = {}
    if typ == 'string':
        args['default_value'] = member.text.decode('string_escape')
    else:
        args['default_value'] = cvt(member.text)
    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    values = props.get('enumValues')
    if values:
        if typ == 'string':
            values = values.decode('string_escape')
        args['values'] = [cvt(val.strip(' "')) for val in values.split(',')]
    aliases = props.get('enumAliases')
    if aliases:
        aliases = aliases.decode('string_escape')
        args['aliases'] = [val.strip(' "') for val in aliases.split(',')]
    units = props.get('units')
    if units:
        args['units'] = get_translation(units)
    vartree.add(name, Enum(**args))


def _add_float(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    args = {}
    args['default_value'] = float(member.text)
    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    units = props.get('units')
    if units:
        args['units'] = get_translation(units)
    low = props.get('hasLowerBound')
    if low == 'true':
        args['low'] = float(props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args['high'] = float(props.get('upperBound'))
    vartree.add(name, Float(**args))


def _add_int(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    args = {}
    args['default_value'] = int(member.text)
    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    low = props.get('hasLowerBound')
    if low == 'true':
        args['low'] = int(props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args['high'] = int(props.get('upperBound'))
    vartree.add(name, Int(**args))


def _add_str(vartree, member, props):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    args = {}
    args['default_value'] = member.text.decode('string_escape')
    desc = props.get('description')
    if desc:
        args['desc'] = desc.decode('string_escape')
    vartree.add(name, Str(**args))


def _add_object(vartree, member):
    """ Helper for :meth:`_populate_from_xml`. """
    name = member.attrib['name']
    obj = vartree.add(name, VariableTree(iotype=vartree.iotype))
    _populate_from_xml(obj, member)  # Recurse.



# Currently unused. While the code works, there are issues regarding how
# other components can access the generated definition, as well as how the
# definition could be accessed while loading from a pickled state.
'''
def generate_from_xml(xml):
    """
    Generate class definition string from `xml`.

    xml: string
        Representation of class to be defined.
    """
    start = xml.find('<Object')
    xml = xml[start:]
    root = ElementTree.fromstring(xml)
    class_def = []
    subclasses = []
    cls = root.attrib['className'].replace('.', '_')
    class_def.append("""
class %s(VarTreeMixin, VariableTree):
    def __init__(self, iotype, client, rpath):
        VarTreeMixin.__init__(self, iotype, client, rpath)
        desc = client.get(rpath+'.description')
        VariableTree.__init__(self, doc=desc, iotype=iotype)
""" % cls)
    _generate_from_xml(root, class_def, subclasses)
    class_def.extend(subclasses)
    return (cls, '\n'.join(class_def))

def _generate_from_xml(root, class_def, subclasses):
    """ Recursive helper for :meth:`generate_from_xml`. """
    members = root.find('members')
    for member in members.findall('member'):
        typ = member.attrib['type']
        if typ == 'object':
            _generate_object(member, class_def, subclasses)
        else:
            properties = member.find('properties')
            props = {}
            enum = False
            for property in properties.findall('property'):
                name = property.attrib['name']
                text = property.text
                if name in ('enumValues', 'enumAliases') and text:
                    enum = True
                props[name] = text
            if enum:
                _generate_enum(member, props, class_def)
            else:
                if typ in ('double[]', 'long[]', 'string[]'):
                    _generate_array(member, props, class_def)
                elif typ == 'boolean':
                    _generate_bool(member, props, class_def)
                elif typ == 'double':
                    _generate_float(member, props, class_def)
                elif typ == 'long':
                    _generate_int(member, props, class_def)
                elif typ == 'string':
                    _generate_str(member, props, class_def)
                else:
                    raise RuntimeError('unsupported member type %r' % typ)


def _generate_array(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    typ = member.attrib['type']
    cvt = {'double[]':float, 'long[]':int, 'string[]':str}[typ]
    if props.get('lockResize') == 'false' and props.get('numDimensions') == '1':
        objtyp = 'List'
    else:
        objtyp = 'Array'
    text = member.text or ''
    args = []

    if objtyp == 'Array':
        dtype = {'double[]':'float', 'long[]':'int', 'string[]':'str'}[typ]
        args.append('dtype=%s' % dtype)
        if text.startswith('bounds['):
            dims, rbrack, rest = text[7:].partition(']')
            dims = [cvt(val.strip(' "')) for val in dims.split(',')]
            junk, lbrace, rest = rest.partition('{')
            data, rbrace, rest = rest.partition('}')
            value = numpy.array([cvt(val.strip(' "'))
                                 for val in data.split(',')]).reshape(dims)
        elif text:
            value = numpy.array([cvt(val.strip(' "'))
                                 for val in text.split(',')])
        else:
            value = numpy.array([])
        args.append('default_value=%s' % value.tolist())
    else:
        dtype = {'double[]':'Float', 'long[]':'Int', 'string[]':'Str'}[typ]
        args.append(dtype)
        if text:
            value = [cvt(val.strip(' "')) for val in text.split(',')]
            args.append('value=%s' % value)

    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    units = props.get('units')
    if units:
        args.append('units=%r' % get_translation(units))
    low = props.get('hasLowerBound')
    if low == 'true':
        args.append('low=%s' % props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args.append('high=%s' % props.get('upperBound'))
    args = ', '.join(args)
    class_def.append('        self.add(%r, %s(%s))' % (name, objtyp, args))


def _generate_bool(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    args = []
    args.append('True' if member.text == 'true' else 'False')
    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    args = ', '.join(args)
    class_def.append('        self.add(%r, Bool(%s))' % (name, args))


def _generate_enum(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    typ = member.attrib['type']
    args = []
    if typ == 'string':
        args.append('%r' % member.text.decode('string_escape'))
    else:
        args.append(member.text)
    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    values = props.get('enumValues')
    if values:
        if typ == 'string':
            values = values.decode('string_escape')
        args.append('values=(%s)' % values)
    aliases = props.get('enumAliases')
    if aliases:
        args.append('aliases=(%s)' % aliases.decode('string_escape'))
    units = props.get('units')
    if units:
        args.append('units=%r' % get_translation(units))
    args = ', '.join(args)
    class_def.append('        self.add(%r, Enum(%s))' % (name, args))


def _generate_float(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    args = []
    args.append(member.text)
    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    units = props.get('units')
    if units:
        args.append('units=%r' % get_translation(units))
    low = props.get('hasLowerBound')
    if low == 'true':
        args.append('low=%s' % props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args.append('high=%s' % props.get('upperBound'))
    args = ', '.join(args)
    class_def.append('        self.add(%r, Float(%s))' % (name, args))


def _generate_int(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    args = []
    args.append(member.text)
    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    low = props.get('hasLowerBound')
    if low == 'true':
        args.append('low=%s' % props.get('lowerBound'))
    high = props.get('hasUpperBound')
    if high == 'true':
        args.append('high=%s' % props.get('upperBound'))
    args = ', '.join(args)
    class_def.append('        self.add(%r, Int(%s))' % (name, args))


def _generate_str(member, props, class_def):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    args = []
    args.append('%r' % member.text.decode('string_escape'))
    desc = props.get('description')
    if desc:
        args.append('desc=%r' % desc.decode('string_escape'))
    args = ', '.join(args)
    class_def.append('        self.add(%r, Str(%s))' % (name, args))


def _generate_object(member, class_def, subclasses):
    """ Helper for :meth:`_generate_from_xml`. """
    name = member.attrib['name']
    cls = member.attrib['className'].replace('.', '_')
    class_def.append('        self.add(%r, %s(iotype=iotype))' % (name, cls))

    subclass_def = []  # Recurse.
    subclasses.append("""
class %s(VariableTree):
    def __init__(self, *args, **kwargs):
        super(%s, self).__init__(*args, **kwargs)
""" % (cls, cls))
    _generate_from_xml(member, subclasses, subclass_def)
    subclasses.append('')
    subclasses.extend(subclass_def)
'''

