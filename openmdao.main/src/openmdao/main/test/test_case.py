"""
Test for Case iterators.
"""

import unittest

import cStringIO

from openmdao.main.api import Case, FileCaseIterator, ListCaseIterator

class CaseTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        pass

    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_file(self):
        data = """
               # Example of an input file
    
               someinput = value1
               blah = value2
               foo = 'abcdef'
               someoutput
               output2

               someinput = value3
               blah = value4
               someoutput

               """

        iterator = FileCaseIterator(None, cStringIO.StringIO(data))

        for i, case in enumerate(iterator):
            if i == 0:
                self.assertEqual(len(case.inputs), 3)
                self.assertEqual(len(case.outputs), 2)

                self.assertEqual(case.inputs[0][0], 'someinput')
                self.assertEqual(case.inputs[0][1], None)
                self.assertEqual(case.inputs[0][2], 'value1')
                self.assertEqual(len(case.inputs[0]), 3)

                self.assertEqual(case.inputs[1][0], 'blah')
                self.assertEqual(case.inputs[1][1], None)
                self.assertEqual(case.inputs[1][2], 'value2')
                self.assertEqual(len(case.inputs[1]), 3)

                self.assertEqual(case.inputs[2][0], 'foo')
                self.assertEqual(case.inputs[2][1], None)
                self.assertEqual(case.inputs[2][2], "'abcdef'")
                self.assertEqual(len(case.inputs[2]), 3)

                self.assertEqual(case.outputs[0][0], 'someoutput')
                self.assertEqual(case.outputs[0][1], None)
                self.assertEqual(case.outputs[0][2], None)
                self.assertEqual(len(case.outputs[0]), 3)

                self.assertEqual(case.outputs[1][0], 'output2')
                self.assertEqual(case.outputs[1][1], None)
                self.assertEqual(case.outputs[1][2], None)
                self.assertEqual(len(case.outputs[1]), 3)

            elif i == 1:
                self.assertEqual(len(case.inputs), 2)
                self.assertEqual(len(case.outputs), 1)

                self.assertEqual(case.inputs[0][0], 'someinput')
                self.assertEqual(case.inputs[0][1], None)
                self.assertEqual(case.inputs[0][2], 'value3')
                self.assertEqual(len(case.inputs[0]), 3)

                self.assertEqual(case.inputs[1][0], 'blah')
                self.assertEqual(case.inputs[1][1], None)
                self.assertEqual(case.inputs[1][2], 'value4')
                self.assertEqual(len(case.inputs[1]), 3)

                self.assertEqual(case.outputs[0][0], 'someoutput')
                self.assertEqual(case.outputs[0][1], None)
                self.assertEqual(case.outputs[0][2], None)
                self.assertEqual(len(case.outputs[0]), 3)

            else:
                self.fail('Too many cases!')


    def test_list(self):
        outputs = [('z1', None, None), ('z2', None, None)]
        cases = []
        for i in range(5):
            inputs = [('x', None, i), ('y', None, i*2)]
            cases.append(Case(inputs, outputs))
        iterator = ListCaseIterator(cases)

        for i, case in enumerate(iterator):
            self.assertEqual(len(case.inputs), 2)
            self.assertEqual(len(case.outputs), 2)

            self.assertEqual(case.inputs[0][0], 'x')
            self.assertEqual(case.inputs[0][1], None)
            self.assertEqual(case.inputs[0][2], i)
            self.assertEqual(len(case.inputs[0]), 3)

            self.assertEqual(case.inputs[1][0], 'y')
            self.assertEqual(case.inputs[1][1], None)
            self.assertEqual(case.inputs[1][2], i*2)
            self.assertEqual(len(case.inputs[1]), 3)

            self.assertEqual(case.outputs[0][0], 'z1')
            self.assertEqual(case.outputs[0][1], None)
            self.assertEqual(case.outputs[0][2], None)
            self.assertEqual(len(case.outputs[0]), 3)

            self.assertEqual(case.outputs[1][0], 'z2')
            self.assertEqual(case.outputs[1][1], None)
            self.assertEqual(case.outputs[1][2], None)
            self.assertEqual(len(case.outputs[1]), 3)

        self.assertEqual(i, 4)


if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(BoolTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

