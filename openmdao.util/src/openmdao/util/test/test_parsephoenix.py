"""
Testing the ParsePhoenixWrapper utility.
"""

import unittest, os

from numpy import array, isnan

from openmdao.util.parse_phoenixwrapper import parse_phoenixwrapper


class TestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        self.infile = 'phx_text_input.txt'
        self.outfile = 'phx_text_output.txt'
    
    def tearDown(self):
        if os.path.exists(self.infile):
           os.remove(self.infile)
        if os.path.exists(self.outfile):
            os.remove(self.outfile)
        pass
    
    def test_phx(self):
        
        phx = []
        phx.append("#Comment\n")
        phx.append("\n")
        phx.append('variable: int1   integer    input  description="Description1"\n')
        phx.append('variable: float   double    output  description="Description2" default=3.14159\n')
        phx.append('variable: ERROR   string    input  description="Description4"  default="ZZZ"\n')
        phx.append('variable: ERROR2   string    output  description="Description4"\n')
        phx.append("variable: units   double    input  description='Description5' units='ft'\n")
        phx.append("variable: array1   double[]    input  description='Description5' \n")
        phx.append('variable: iopt    integer    input  description="Execution Type"  enumValues=1,2,3,4  enumAliases="Analysis","Parametric Variation","Optimization","Contour or Thumbprint plot"\n')
        phx.append("variable: unitignore   double    input  description='Description5' units='dB'\n")
        phx.append("variable: unitreplace   double    input  description='Description5' units='mph'\n")
        phx.append("variable: bool   boolean    input default=false\n")
        phx.append('variable: stringbadquote   string    input  description="Description4"  default="xx\n')
        phx.append('variable: zfile file  output\n')
        phx.append("variable: unitfullreplace   double    output  description='Description5' units='R'\n")
        phx.append('variable: floatbadquote   double    output  description="Aa, Bb, Cc\n')
        phx.append("\n")
        phx.append('setGroup "input.deep"\n')
        phx.append('variable: int2   integer    input  description="Description3" default=5\n')
        phx.append("####\n")
        
        outfile = open(self.infile, 'w')
        outfile.writelines(phx)
        outfile.close()

        parse_phoenixwrapper(self.infile, self.outfile, "TestComp")
        
        infile = open(self.outfile, 'r')
        result = infile.readlines()
        infile.close()
        
        self.assertEqual(result[0], '"""\n')
        self.assertEqual(result[1], 'OpenMDAO Wrapper for TestComp\n')
        self.assertEqual(result[2], 'Automatically generated from phx_text_input.txt with parse_phoenixwrapper.\n')
        self.assertEqual(result[3], '"""\n')
        self.assertEqual(result[4], '\n')
        self.assertEqual(result[5], 'from numpy import float32 as numpy_float32\n')
        self.assertEqual(result[6], '\n')
        self.assertEqual(result[7], 'from openmdao.main.api import Component, Container\n')
        self.assertEqual(result[8], 'from openmdao.lib.datatypes.api import Int, Float, Str, Array, Enum, Bool, File\n')
        self.assertEqual(result[9], '\n')
        self.assertEqual(result[10], '\n')
        self.assertEqual(result[11], 'class TestComp_input_deep(Container):\n')
        self.assertEqual(result[12], '    """Container for input.deep"""\n')
        self.assertEqual(result[13], '\n')
        self.assertEqual(result[14], '    # OpenMDAO Variables\n')
        self.assertEqual(result[15], "    int2 = Int(5, iotype='in', doc='Description3')\n")
        self.assertEqual(result[16], '\n')
        self.assertEqual(result[17], '\n')
        self.assertEqual(result[18], 'class TestComp_input(Container):\n')
        self.assertEqual(result[19], '    """Container for input"""\n')
        self.assertEqual(result[20], '\n')
        self.assertEqual(result[21], '    # OpenMDAO Variables\n')
        self.assertEqual(result[22], '\n')
        self.assertEqual(result[23], "    def __init__(self, directory=''):\n")
        self.assertEqual(result[24], '        """Constructor for the TestComp_input component"""\n')
        self.assertEqual(result[25], '\n')
        self.assertEqual(result[26], "        super(TestComp_input, self).__init__(directory)\n")
        self.assertEqual(result[27], '\n')
        self.assertEqual(result[28], "        # Variable Containers\n")
        self.assertEqual(result[29], "        self.add('deep',  TestComp_input_deep())\n")
        self.assertEqual(result[30], '\n')
        self.assertEqual(result[31], '\n')
        self.assertEqual(result[32], 'class TestComp(Component):\n')
        self.assertEqual(result[33], '    """Wrapper for TestComp"""\n')
        self.assertEqual(result[34], '\n')
        self.assertEqual(result[35], '    # OpenMDAO Variables\n')
        self.assertEqual(result[36], "    int1 = Int(0, iotype='in', doc='Description1')\n")
        self.assertEqual(result[37], "    float = Float(3.14159, iotype='out', doc='Description2')\n")
        self.assertEqual(result[38], "    ERROR = Str('ZZZ', iotype='in', doc='Description4')\n")
        self.assertEqual(result[39], "    ERROR2 = Str('', iotype='out', doc='Description4')\n")
        self.assertEqual(result[40], "    units = Float(0.0, iotype='in', units='ft', doc='Description5')\n")
        self.assertEqual(result[41], "    array1 = Array(iotype='in', dtype=numpy_float32, doc='Description5')\n")
        self.assertEqual(result[42], "    iopt = Enum((1,2,3,4), iotype='in', doc='Execution Type', aliases=('Analysis', 'Parametric Variation', 'Optimization', 'Contour or Thumbprint plot'))\n") 
        self.assertEqual(result[43], "    unitignore = Float(0.0, iotype='in', doc='Description5')\n")
        self.assertEqual(result[44], "    unitreplace = Float(0.0, iotype='in', units='mi/h', doc='Description5')\n")
        self.assertEqual(result[45], "    bool = Bool(False, iotype='in')\n")
        self.assertEqual(result[46], "    stringbadquote = Str('xx', iotype='in', doc='Description4')\n")
        self.assertEqual(result[47], "    zfile = File(iotype='out', path='Insert_Filename_Here')\n")
        self.assertEqual(result[48], "    unitfullreplace = Float(0.0, iotype='out', units='degR', doc='Description5')\n")
        self.assertEqual(result[49], "    floatbadquote = Float(0.0, iotype='out', doc='Aa, Bb, Cc')\n")
        
        
    def test_small_phx(self):
        
        phx = []
        phx.append('variable: int1   integer    input  description="Description1"\n')
        
        outfile = open(self.infile, 'w')
        outfile.writelines(phx)
        outfile.close()

        parse_phoenixwrapper(self.infile, self.outfile, "TestComp")
        
        infile = open(self.outfile, 'r')
        result = infile.readlines()
        infile.close()
        
        self.assertEqual(result[6], 'from openmdao.main.api import Component\n')
        
    def test_bad_datatype(self):
        
        phx = []
        phx.append('variable: int1   badtype    input  description="Description1"\n')

        outfile = open(self.infile, 'w')
        outfile.writelines(phx)
        outfile.close()

        try:
            parse_phoenixwrapper(self.infile, self.outfile, "TestComp")
        except KeyError, err:
            msg = "'Unhandled Modelcenter input type - badtype'"
            self.assertEqual(str(err), msg)
        else:
            self.fail('KeyError expected')  
            

if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

