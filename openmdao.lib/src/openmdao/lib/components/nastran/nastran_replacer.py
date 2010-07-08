import re
import copy

from nastran_util import stringify, nastran_replace_inline

class NastranReplacer(object):

    def __init__(self, text):
        self.text = text

    def replace(self, input_variables):

        nastran_text = self.text
        
        # we want all the variables in the nastran text

        variable_match = re.compile("%(\w+)")
        all_variables = set()
    
        for index, line in enumerate(nastran_text):
            matches = re.findall(variable_match, line)
            for match in matches:
                if match in all_variables:
                    print "There is a duplicate variable called", match
                    print "They will get the same value"
                all_variables.add(match)

        # make sure we have all the variables that are declared
        # in the file
        extras = all_variables.difference(input_variables.keys())
        if len(extras) > 0:
            raise ValueError("There are variables in the file that aren't " +\
                             "provided as input variables: " + \
                             str(extras))

        #print "input vars:", input_variables.keys()

        new_nastran_text = '\n'.join(nastran_text)

        new_nastran_text = []
        for line in nastran_text:
            new_nastran_text.append(copy.copy(line))
            matches = re.findall(variable_match, line)
            for match in matches:
                #print "replacing", match, new_nastran_text, line
                new_nastran_text[-1] = \
                      nastran_replace_inline(new_nastran_text[-1],\
                          "%" + match, \
                          stringify(input_variables[match][2]))

        self.text = new_nastran_text
                
                
