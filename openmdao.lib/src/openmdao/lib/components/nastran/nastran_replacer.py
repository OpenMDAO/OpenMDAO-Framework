"""Defines NastranReplacer, an object which provides a
crude and simple search and replace function."""
import re

from nastran_util import stringify, nastran_replace_inline

variable_match = re.compile("%([*\w]+)")

class NastranReplacer(object):
    """A kind of dummy object that just replaces variables
    in a text with their corresponding values."""

    def __init__(self, text):
        """Give it the text.

        Parameters
        ----------
        text : [str]
            The strings should be the individual lines without
            the newline
        """
        self.text = text

    def replace(self, input_variables):
        """
        Replace the input_variables in the text with the
        corresponding values.

        Parameters
        ----------
        input_variables: {variable_name : value}
            We want to replace the instances of variable_name
            with value. variable_name should be a string; value
            should be a strigafiable object. Note that although
            the actual variables are specified like %varname in
            the Nastran file, the variable name here should just
            be ``varname''.

        Notes
        -----
        Changes self.text. Running this twice should probably
        error out since it wouldn't find the variables it was meant
        to replace.

        """

        # should be an array of lines
        nastran_text = self.text

        # we want all the variables in the nastran text
        all_variables = set()

        for line in nastran_text:
            matches = re.findall(variable_match, line)
            for match in matches:
                if match in all_variables:
                    # This should be given to the user... but
                    # maybe not in stdout
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

        not_used = set(input_variables.keys()).difference(all_variables)
        if len(not_used) > 0:
            # we really want to be logging this to DEBUG, or equiv
            print "Hey, just a heads up. You are passing in variables " +\
                  "that are not being replaced because they are not " +\
                  "in the file. This could indicate that you made a " +\
                  "mistake, or not. The offending variables are: " + \
                  str(not_used)


        new_nastran_text = []
        for line in nastran_text:
            new_nastran_text.append(line[:])
            matches = re.findall(variable_match, line)
            for match in matches:
                # If the variable starts with ``*'', we will not
                # simply replace the 8 character block, but we
                # will just overwrite the location of the variable
                if match.startswith("*"):
                    value = str(input_variables[match])
                    if len(value) < (len(match) + 1):
                        value = value.ljust(len(match)+1)
                    start_pos = new_nastran_text[-1].find("%" + match)
                    end_pos = start_pos + len(value) # exclusive
                    new_nastran_text[-1] = new_nastran_text[-1][:start_pos] + \
                                           value + \
                                           new_nastran_text[-1][end_pos:]


                #print "replacing", match, new_nastran_text, line
                else:
                    new_nastran_text[-1] = \
                         nastran_replace_inline(new_nastran_text[-1],\
                             "%" + match, \
                             stringify(input_variables[match]))

        self.text = new_nastran_text


