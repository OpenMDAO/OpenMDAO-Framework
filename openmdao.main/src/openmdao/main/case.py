from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator


class Case(object):
    """Contains all information necessary to specify an input *case*, i.e., a
    list of name, index, value tuples for all inputs to the case, all outputs
    collected after running the case, an indicator of the exit status of the
    case, a string containing error messages associated with the running of
    the case, and an optional case identifier. The value entry of output tuples
    should be set to *None* prior to executing the case.

    """
    def __init__(self, inputs=None, outputs=None, max_retries=None,
                 retries=None, msg='', ident=''):
        """If inputs or outputs are supplied to the constructor, each must be an
        iterator that returns (name,index,value) tuples. 
        
        """
        self.inputs = inputs or []      # a list of name,index,value tuples 
        self.outputs = outputs or []    # a list of name,index,value tuples 
                                        #   Values for each output will be filled
                                        #   in after the case completes by calling 
                                        #   the update function
        self.max_retries = max_retries  # times to retry after error(s)
        self.retries = retries          # times case was retried
        self.msg = msg                  # If non-null, error message.
                                        # Implies outputs are invalid.  
        self.ident = ident
                                        
    def __str__(self):
        return 'Case %s:\n' \
               '    inputs: %s\n' \
               '    outputs: %s\n' \
               '    max_retries: %s, retries: %s\n' \
               '    msg: %s' % \
               (self.ident, self.inputs, self.outputs,
                self.max_retries, self.retries, self.msg)

    def apply_inputs(self, scope):
        """Set all of the inputs in this case to their specified values in
        the given scope.
        """
        if self.retries is None:
            self.retries = 0
        else:
            self.retries += 1
        for name,index,value in self.inputs:
            scope.set(name, value, index)
            
    def update_outputs(self, scope, msg=None):
        """Update the value of all outputs of interest, using the given 
        scope, and/or set error message.
        """
        if msg:
            self.msg = msg
        # TODO: make this smart enough to do a multiget on a component
        #       instead of multiple individual gets
        outs = self.outputs
        for i,tup in enumerate(outs):
            outs[i] = (tup[0], tup[1], scope.get(tup[0], tup[1]))

    def add_input(self, name, value, index=None):
        """Convenience function for adding an input"""
        self.inputs.append((name, index, value))

    def add_output(self, name, index=None):
        """Convenience function for adding an output"""
        self.outputs.append((name, index, None))
        
#class FileCaseIterator(object):
    #"""An iterator that returns :class:`Case` objects from a file having the
    #simple format below, where a blank line indicates a separation between two
    #cases.  Whitespace outside of quotes is ignored.  Outputs are indicated
    #by the lack of an assignment.
    
#.. todo:: Convert value strings to appropriate type

#.. todo:: Allow multi-line values (strings, arrays, etc.) on right hand side
       
#.. todo:: Allow array indexing for inputs, outputs, or RHS values
    
#.. parsed-literal::
    
       ## Example of an input file
    
       #someinput = value1
       #blah = value2
       #foo = 'abcdef'
       #someoutput
       #output2

       #someinput = value3
       #blah = value4
    
    #"""
    
    #implements(ICaseIterator)
    
    #def __init__(self, scope, fname):
        #if isinstance(fname, basestring):
            #self.inp = open(fname, 'r')
        #else:
            #self.inp = fname
        #self.scope = scope
        #self.line_number = 0
        #self.ident = 0
    
    #def __iter__(self):
        #return self._next_case()
        
    #def _next_case(self):
        #""" Generator which returns cases as they are seen in the stream. """
        #inputs = []
        #outputs = []
        #for line in self.inp:
            #self.line_number += 1
            #line = line.strip()
            #if line.startswith('#'):  # comment line
                #continue
            #if line == '':  # blank line
                #if len(inputs) > 0:
                    #self.ident += 1
                    #newcase = Case(inputs, outputs, ident=str(self.ident))
                    #inputs = []
                    #outputs = []
                    #yield newcase
                #else: # extra blank line. ignore
                    #pass
            #else:
                #parts = line.split('=')
                #if len(parts) > 1:        # it's an input assignment
                    #inputs.append((parts[0].strip(), None, parts[1].strip()))
                #else:
                    #outputs.append((parts[0].strip(), None, None))
                    
        #if len(inputs) > 0:
            #self.ident += 1
            #newcase = Case(inputs, outputs, ident=str(self.ident))
            #inputs = []
            #outputs = []
            #yield newcase
                    

