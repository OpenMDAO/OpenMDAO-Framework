"""nastran.py defines NastranComponent.
"""

from os import fdopen, remove, path
from tempfile import mkdtemp
from shutil import rmtree
import re
import copy

from openmdao.lib.api import Float, Str, Bool
from openmdao.lib.components.external_code import ExternalCode

from enthought.traits.api import TraitError

from openmdao.util.filewrap import FileParser

from nastran_replacer import NastranReplacer
from nastran_maker import NastranMaker
from nastran_parser import NastranParser
from nastran_util import stringify, nastran_replace_inline

from openmdao.lib.datatypes.float import Float
from openmdao.lib.datatypes.int import Int
from openmdao.lib.datatypes.array import Array

class NastranComponent(ExternalCode):
    """All Nastran-capable components should be subclasses of NastranComponent

    By subclassing NastranComponent, any component should have easy access
    to NastranMaker, NastranReplacer, and NastranParser. Your subclass
    must specify how to handle the input and output variables to NastranComponent
    by specifying nastran specific attributes on the traits. All of these
    attributes are described further in the :ref:`NastranComponent<NastranComponent>` docs.

    Note: This component does nothing with external_files. If you want to deal with
    that, then do so in your subclass.
    """


    nastran_filename = Str(iotype="in", desc="Input filename with \
                                              placeholder variables")
    nastran_command = Str(iotype="in", desc="Location of nastran \
                                             executable")
    nastran_command_args = Str(iotype="in", desc="Arguments to the \
                                                  nastran command")

    output_filename = Str(iotype="out", desc="Output filename")

    delete_tmp_files = Bool(True, iotype="in", desc="Should I delete \
                            the temporary files?")

    keep_first_iteration = Bool(True, iotype="in", desc="If I am \
    deleting temporary files, should I keep the first one?")

    keep_last_iteration = Bool(True, iotype="in", desc="If I am \
    deleting temporary files, should I keep the last one?")

    def __init__(self, *args, **kwargs):
        super(NastranComponent, self).__init__(*args, **kwargs)

        # We're initializing parser here so that it's not an
        # honest-to-god trait, just an attribute that can
        # be accessed from the the class that subclasses NastranComponent
        self.parser = None

        # This variables are just to keep track of what we've
        # deleted if you select keep_first_iteration or keep_last_iteration
        self._seen_first_iteration = False
        self._last_seen_iteration = ""


    def execute(self):
        """Runs the NastranComponent.

        We are overiding ExternalCode's execute function. First we
        setup the input file (by running NastranReplacer and then
        NastranMaker). Next we run Nastran by calling our
        parent's execute function. Finally, we parse the data
        and set the output variables given to us.

        RuntimeError
            The component relies on ExternalCode which can throw all
            sorts of RuntimeError like exceptions (RunStopped,
            RunInterrupted also included)
            
        Filesystem-type Errors
            NastranComponent makes a temporary directory with mkdtemp
            in the temp module. If that fails, the error just
            propagates up.


        While there are no explicit parameters or return values for this
        function, it gets all the input it needs from the design
        variables that are connected to the subclass of NastranComponent.
        This should be described pretty well in the README.

        """
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
                    raise RuntimeError("You specified at least one of " + \
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
                    raise RuntimeError("You specified at least one of " + \
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

        # note: fh.readlines() won't work because it doesn't
        # strip the newline at the end for you. So whatever,
        # we'll just use split on newlines.
        nastran_text = fh.read().split("\n")
        fh.close()

        # replace the variables in the nastran text using Replacer
        replacer = NastranReplacer(nastran_text)
        varname2value = {}
        for name, trait in input_variables.iteritems():
            varname2value[trait.nastran_var] = getattr(self, name)
        replacer.replace(varname2value)
        nastran_text = replacer.text

        # use nastran maker to intelligently replace
        # values in cards
        maker = NastranMaker(nastran_text)
        for name, trait in smart_replacements.iteritems():
            value = getattr(self, name)
            maker.set(trait.nastran_card,
                      trait.nastran_id,
                      trait.nastran_fieldnum, value)
        self.nastran_maker_hook(maker)
        maker.write_to_file(tmpfh, 10001)

        tmpfh.close()

        # what is the new file called?
        self.output_filename = path.join(tmpdir, "input.out")

        # perhaps this should be logged, or something
        print self.output_filename

        # Then we run the nastran file
        self.command = self.nastran_command + " " + \
                       tmppath + " " + self.nastran_command_args + \
                       " batch=no out=" + tmpdir + \
                       " dbs=" + tmpdir

        # This calls ExternalCode's execute which will run
        # the nastran command via subprocess
        super(NastranComponent, self).execute()

        # And now we parse the output

        filep = FileParser()
        filep.set_file(self.output_filename)
        filep.set_delimiters(" ")

        # Before we start, we want to make sure we aren't
        # dealing with a failed run. So we search for "FATAL"
        for line in filep.data:
            if "FATAL" in line:
                raise RuntimeError("There was a problem with " + \
                                   "Nastran. It failed to run " + \
                                   "correctly. If you want to see " +\
                                   "the output, check out " + \
                                   self.output_filename)


        for output_name, output_trait in output_variables.iteritems():
            # We run trait.nastran_func on filep and get the
            # final value we want
            setattr(self, output_name,
                    output_trait.nastran_func(filep))


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
            row = nastran_row or 0
            col = nastran_column or 0

            # Now we'll try to guess the conversion we should
            # perform by inspecting the type of trait that
            # is requesting this value
            converter = lambda x: x
            type_understood_as = "unsure"
            if isinstance(trait.trait_type, Float):
                converter = float
                type_understood_as = "float"
            elif isinstance(trait.trait_type, Int):
                converter = lambda x: int(float(x))
                type_understood_as = "int"
            elif isinstance(trait.trait_type, Array):
                # we aren't actually going to return the entire
                # grid because you should use parser if you
                # want to do that.
                converter = lambda x: [x]
                type_understood_as = "array"

            try:
                setattr(self, name, converter(result[row][col]))
            except ValueError, ve:
                import sys
                print >>sys.sterr, "Unable to convert string " + \
                      result[row][col] +  " to " + type_understood_as
                raise

        # get rid of our tmp dir
        tmpdir_to_delete = ""
        if self.delete_tmp_files:
            if self.keep_first_iteration:
                if not self._seen_first_iteration:
                    self._seen_first_iteration = True
                else:
                    if self.keep_last_iteration: # keep both
                        tmpdir_to_delete = self._last_seen_iteration
                        self._last_seen_iteration = tmpdir
                    else: # just keep first
                        tmpdir_to_delete = tmpdir
            else:
                if self.keep_last_iteration: # only keep last
                    tmpdir_to_delete = self._last_seen_iteration
                    self._last_seen_iteration = tmpdir
                else: # don't keep anything
                    tmpdir_to_delete = tmpdir

            if tmpdir_to_delete:
                rmtree(tmpdir_to_delete)

    def nastran_maker_hook(self, maker):
        """A subclass can override this function to dynamically
        add variables to NastranMaker.

        maker: NastranMaker object
            This NastranMaker object already has all the variables that
            were specified in the traits.

        The return will be ignored. Right after this function exits
        the Nastran input file will be written out to a file.
        """
        pass
