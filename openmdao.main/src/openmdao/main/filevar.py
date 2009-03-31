#public symbols
__all__ = ['FileVariable']
__version__ = "0.1"

from openmdao.main.variable import Variable, UNDEFINED

class FileVariable(Variable):
    """ A Variable wrapper for a file path attribute. Metadata for the file
    is available in the 'metadata' attribute of the variable.  Standard
    metadata includes:
        'content_type': string
            default ''
        'content_encoding': string
            default ''
        'binary': bool
            default False
        'big_endian': bool
            default False
        'single_precision': bool
            default False
        'unformatted': bool
            default False
        'recordmark_8': bool
            default False
    """
    
    def __init__(self, name, parent, iostatus, ref_name=None, ref_parent=None,
                 default=UNDEFINED, doc=None, metadata=None):
        super(FileVariable, self).__init__(name, parent, iostatus, 
                                           val_type=basestring, 
                                           ref_name=ref_name,
                                           ref_parent=ref_parent,
                                           default=default, doc=doc)
        self.metadata = {
            'content_type' : '',
            'content_encoding' : '',
            'binary' : False,
            'big_endian' : False,
            'single_precision' : False,
            'unformatted' : False,
            'recordmark_8' : False, }

        if metadata is not None:
            self.metadata.update(metadata)

