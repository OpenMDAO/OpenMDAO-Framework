
from enthought.traits.api import HasTraits, MetaHasTraits, Any, Python, Event
from enthought.traits.trait_base import not_none

excludes = (Any, Python, Event)

def get_traits_info(app, what, name, obj, options, lines):
    if not (isinstance(obj, MetaHasTraits) or isinstance(obj, HasTraits)):
        return
    
    traitdict = obj.class_traits()
    
    lines.extend(['','Traits list:',''])
    for t,val in traitdict.items():
        if not isinstance(val.trait_type, excludes):
            lines.extend([t+": %s"%val.desc,''])
        

def setup(app):
    app.connect('autodoc-process-docstring', get_traits_info)


if __name__ == '__main__':
    from openmdao.main.api import Assembly
    lines = []
    get_traits_info(None, 'class', 'foo', Assembly, None, lines)
    print 'lines = %s', lines