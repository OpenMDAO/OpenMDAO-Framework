from os import fdopen, remove, path
from tempfile import mkdtemp
from shutil import rmtree
import re
import copy

from openmdao.lib.api import Float, Str
from openmdao.lib.components.external_code import ExternalCode

from enthought.traits.api import TraitError

from openmdao.util.filewrap import FileParser

from nastran_replacer import NastranReplacer
from nastran_maker import NastranMaker
from nastran_output import NastranOutput
from nastran_output_helpers import *
from nastran_parser import NastranParser
from nastran_util import stringify, nastran_replace_inline

class NastranComponent(ExternalCode):

    nastran_filename = Str(iotype="in", desc="Input filename with \
                                          placeholder variables")
    nastran_command = Str(iotype="in", desc="Location of nastran \
                                          executable")

    output_filename = Str(iotype="out", desc="Output filename")

    parser = None # it's a NastranParser?

    def execute(self):
        # We are going to keep track of all the ways we
        # can manage input/output:
        #  - the crude way (NastranReplacer, NastranOutput)
        #    correspond to input_variables, output_variables
        #  - the better way (NastranMaker, NastranParser)
        #    correspond to smart_replacements and grid_outputs

        # all of these are {"traitname" : trait}
        input_variables = {}
        smart_replacements = {}
        output_variables = {}
        grid_outputs = {}
        
        for name, trait in self.traits().iteritems():
            if trait.iotype == "in":
                # nastran_var is a variable that should be replaced
                if trait.nastran_var:
                    # if the variable is longer than seven characters
                    # it won't fit in the nastran file, since the limit
                    # there is 8 characters (%xxxxxxx)
                    if len(trait.nastran_var) > 7:
                        raise ValueError("The variable " + trait.nastran + \
                                         " is too long to be a variable")
                    input_variables[name] = trait

                # it could also be a smart replacement, but we'll have
                # to specify the card, id, and fieldnum
                if trait.nastran_card and trait.nastran_id and trait.nastran_fieldnum:
                    smart_replacements[name] = trait

                elif trait.nastran_card or trait.nastran_id or trait.nastran_fieldnum:
                    raise Exception("You specified at least one of " + \
                                    "nastran_card, nastran_id, and " + \
                                    "nastran_fieldnum, but you did " + \
                                    "not specify all of them. You " + \
                                    "most probably mistyped.")
                
            elif trait.iotype == "out":
                # if we want to supply a function that will parse
                # out the wanted information from the output object
                # and the fileparser, then this
                if trait.nastran_func:
                    output_variables[name] = trait

                # this is the grid method of accessing. We have to
                # specify a header, row, and attribute and
                # the output variable will be set to that value
                if trait.nastran_header and trait.nastran_constraints and trait.nastran_columns:
                    grid_outputs[name] = trait
                elif trait.nastran_header or trait.nastran_constraints or trait.nastran_columns:
                    raise Exception("You specified at least one of " + \
                                    "nastran_header, nastran_constrains"+\
                                    ", and nastran_columns, but you " + \
                                    "did not specify all them. You " + \
                                    "most probably mistyped")
                    
        # let's do our work in a tmp dir
        tmpdir = mkdtemp()
        tmppath = path.join(tmpdir, "input.bdf")
        tmpfh = open(tmppath, "w")

        # raw nastran file supplied by user
        fh = open(self.nastran_filename, "r")
        nastran_text = fh.read().split("\n")
        fh.close()

        # replace the variables in the nastran text using Replacer
        replacer = NastranReplacer(nastran_text)
        varname2value = {}
        for name, trait in input_variables.iteritems():
            varname2value[trait.nastran_var] = self.__getattribute__(name)
        replacer.replace(varname2value)
        nastran_text = replacer.text

        # use nastran maker to intelligently replace
        # values in cards
        maker = NastranMaker(nastran_text)
        for name, trait in smart_replacements.iteritems():
            value = self.__getattribute__(name)
            maker.set(trait.nastran_card,
                      trait.nastran_id,
                      trait.nastran_fieldnum, value)
        maker.write_to_file(tmpfh, 10001)
        
        tmpfh.close()


        # Then we run the nastran file
        self.command = self.nastran_command + " " + \
                       tmppath + " batch=no out=" + tmpdir + \
                       " dbs=" + tmpdir

        # This calls ExternalCode's execute which will run
        # the nastran command via subprocess
        super(NastranComponent, self).execute()

        # And now we parse the output
        
        # what is the new file called?
        self.output_filename = path.join(tmpdir, "input.out")

        # perhaps this should be logged, or something
        print self.output_filename

        filep = FileParser()
        filep.set_file(self.output_filename)
        filep.set_delimiters(" ")

        output = NastranOutput(filep)

        for output_name, output_trait in output_variables.iteritems():
            # We run trait.nastran_func on filp and output and get the
            # final value we want
            self.__setattr__(output_name,
                             output_trait.nastran_func(filep, output))


        # This is the grid parser.
        self.parser = NastranParser(filep.data)
        self.parser.parse()

        for name, trait in grid_outputs.iteritems():
            header = trait.nastran_header
            subcase = trait.nastran_subcase
            constraints = trait.nastran_constraints
            columns = trait.nastran_columns
            result = self.parser.get(header, subcase, \
                                     constraints, columns)
            
            # nastran_{row,column} might be kinda silly
            # in most cases, the user will probably just call
            # self.parser.get on her own
            nastran_row = trait.nastran_row
            nastran_column = trait.nastran_column
            row = nastran_row if nastran_row else 0
            col = nastran_column if nastran_column else 0

            # Yeah, automatic conversion to float is tacky, but
            # the user can always call self.parser.get on her own.
            self.__setattr__(name, float(result[row][col]))

        # get rid of our tmp dir
        rmtree(tmpdir)
