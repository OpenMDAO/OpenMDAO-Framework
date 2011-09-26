import sys

from openmdao.main.api import Assembly, Component, Container, FileRef, \
                              set_as_top
from openmdao.main.rbac import rbac

from openmdao.lib.datatypes.api import Array, Bool, Enum, File, Float, \
                                       Int, List, Str

class SubObj(Container):
    """ Sub-object under TopObject. """

    def __init__(self, *args, **kwargs):
        super(SubObj, self).__init__(*args, **kwargs)
        iotype = kwargs.get('iotype', None)
        self.add('sob', Bool(False, iotype=iotype))
        self.add('sof', Float(0.284, units='lb/inch**3', iotype=iotype))
        self.add('soi', Int(3, iotype=iotype))
        self.add('sos', Str('World', iotype=iotype))


class TopObj(Container):
    """ Top-level object variable. """

    def __init__(self, *args, **kwargs):
        super(TopObj, self).__init__(*args, **kwargs)
        iotype = kwargs.get('iotype', None)
        self.add('subobj', SubObj(iotype=iotype))
        self.add('tob', Bool(True, iotype=iotype))
        self.add('tof', Float(0.5, units='inch', iotype=iotype))
        self.add('toi', Int(42, iotype=iotype))
        self.add('tos', Str('Hello', iotype=iotype))
        self.add('tofe', Enum(iotype='in', values=(2.781828, 3.14159),
                              aliases=('e', 'pi'), desc='Float enum', units='m'))
        self.add('toie', Enum(iotype='in', values=(9, 8, 7, 1), desc='Int enum'))
        self.add('tose', Enum(iotype='in', values=('cold', 'hot', 'nice'),
                              desc='Str enum'))

        self.add('tof1d', Array(dtype=float, iotype=iotype,
                                desc='1D float array', units='cm',
                                default_value=[1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5],
                                low=0, high=10))

        self.add('tof2d', Array(dtype=float, iotype=iotype,
                                desc='2D float array', units='mm',
                                default_value=[ [1.5, 2.5, 3.5, 4.5],
                                                [5.5, 6.5, 7.5, 8.5] ]))

        self.add('tof3d', Array(dtype=float, iotype=iotype,
                                desc='3D float array',
                                default_value=[ [ [1.5, 2.5, 3.5],
                                                  [4.5, 5.5, 6.5],
                                                  [7.5, 8.5, 9.5] ],
                                                [ [10.5, 20.5, 30.5],
                                                  [40.5, 50.5, 60.5],
                                                  [70.5, 80.5, 90.5] ] ]))

        self.add('toi1d', Array(dtype=int, iotype=iotype, desc='1D int array',
                                default_value=[1, 2, 3, 4, 5, 6, 7, 8, 9]))

        self.add('tos1d', List(Str, iotype=iotype, desc='1D string array',
                               value=['Hello', 'from', 'TestComponent.tos1d']))

        self.add('toflst', List(Float, iotype=iotype, desc='Float list'))
        self.add('toilst', List(Int, iotype=iotype, desc='Int list'))


class TestComponent(Component):
    """ Just something to test with. """

    x = Float(iotype='in', default_value=2, desc='X input')
    y = Float(iotype='in', default_value=3, desc='Y input',
              low=-10, high=10, units='ft')
    z = Float(iotype='out', desc='Z output', units='ft')
    exe_count = Int(iotype='out', desc='Execution count')

    in_file = File(iotype='in', local_path='inFile.dat', desc='Input file')
    out_file = File(iotype='out', path='outFile.dat', desc='Output file')

    def __init__(self):
        super(TestComponent, self).__init__()
        self.add('sub_group', SubGroup())
        self.add('obj_input', TopObj(iotype='in'))
        self.add('obj_output', TopObj(iotype='out'))

    def execute(self):
        if self.x < 0:
            raise RuntimeError('x %s is < 0' % self.x)
        self.z = self.x * self.y
        self.exe_count += 1
        with self.in_file.open() as inp:
            with open(self.out_file.path, 'w') as out:
                out.write(inp.read())
            
        self._logger.info('    %s %s %s', self.x, self.y, self.z)
        sys.stdout.write('stdout: %s %s %s\n' % (self.x, self.y, self.z))
        sys.stdout.flush()
#        sys.stderr.write('stderr: %s %s %s\n' % (self.x, self.y, self.z))
#        sys.stderr.flush()

        # Copy input object to output object.
        self.obj_output.tob = self.obj_input.tob
        self.obj_output.tof = self.obj_input.tof
        self.obj_output.toi = self.obj_input.toi
        self.obj_output.tos = self.obj_input.tos

        self.obj_output.tofe = self.obj_input.tofe
        self.obj_output.toie = self.obj_input.toie
        self.obj_output.tose = self.obj_input.tose

        self.obj_output.tof1d = self.obj_input.tof1d
        self.obj_output.tof2d = self.obj_input.tof2d
        self.obj_output.tof3d = self.obj_input.tof3d
        self.obj_output.toi1d = self.obj_input.toi1d
        self.obj_output.tos1d = self.obj_input.tos1d

        self.obj_output.toflst = self.obj_input.toflst
        self.obj_output.toilst = self.obj_input.toilst

        self.obj_output.subobj.sob = self.obj_input.subobj.sob
        self.obj_output.subobj.sof = self.obj_input.subobj.sof
        self.obj_output.subobj.soi = self.obj_input.subobj.soi
        self.obj_output.subobj.sos = self.obj_input.subobj.sos
        
    @rbac(('owner', 'user'))
    def cause_exception(self):
        self.raise_exception("It's your own fault...", RuntimeError)

    @rbac(('owner', 'user'))
    def float_method(self):
        return self.z

    @rbac(('owner', 'user'))
    def int_method(self):
        return self.exe_count

    @rbac(('owner', 'user'))
    def null_method(self):
        return

    @rbac(('owner', 'user'))
    def str_method(self):
        msg = 'current state: x %r, y %r, z %r, exe_count %r' \
              % (self.x, self.y, self.z, self.exe_count)
        return msg


class SubGroup(Container):
    """ For checking subcontainer access. """

    b = Bool(iotype='in', default_value=True, desc='A boolean')
    f = Float(iotype='in', default_value=0.5, desc='A float')
    i = Int(iotype='in', default_value=7, desc='An int')
    s = Str(iotype='in', default_value='Hello World!  ( & < > )',
            desc='A string')

    fe = Enum(iotype='in', values=(2.781828, 3.14159),
                           aliases=('e', 'pi'), desc='Float enum', units='m')
    ie = Enum(iotype='in', values=(9, 8, 7, 1), desc='Int enum')
    se = Enum(iotype='in', values=('cold', 'hot', 'nice'), desc='Str enum')

    f1d = Array(dtype=float, iotype='in', desc='1D float array', units='cm',
                default_value=[1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5],
                low=0, high=10)

    f2d = Array(dtype=float, iotype='in', desc='2D float array', units='mm',
                default_value=[ [1.5, 2.5, 3.5, 4.5],
                                [5.5, 6.5, 7.5, 8.5] ])

    f3d = Array(dtype=float, iotype='in', desc='3D float array',
                default_value=[ [ [1.5, 2.5, 3.5],
                                  [4.5, 5.5, 6.5],
                                  [7.5, 8.5, 9.5] ],
                                [ [10.5, 20.5, 30.5],
                                  [40.5, 50.5, 60.5],
                                  [70.5, 80.5, 90.5] ] ])

    i1d = Array(dtype=int, iotype='in', desc='1D int array',
                default_value=[1, 2, 3, 4, 5, 6, 7, 8, 9])

    s1d = List(Str, iotype='in', desc='1D string array',
               value=['Hello', 'from', 'TestComponent.SubGroup'])

    flst = List(Float, iotype='in', desc='List of floats')
    ilst = List(Int, iotype='in', desc='List of ints')


class Bogus(object):
    """ To test instantiation. """

    def __init__(self, need_one_argument):
        self._arg = need_one_argument


if __name__ == '__main__':
    top = set_as_top(Assembly())
    comp = top.add('comp', TestComponent())
    comp.in_file = FileRef(path='ASTestComp-0.1.cfg', owner=top)
    comp.run()
    for path in ('x', 'y', 'z', 'exe_count',
                 'sub_group.b', 'sub_group.f', 'sub_group.i', 'sub_group.s',
                 'sub_group.fe', 'sub_group.ie', 'sub_group.se',
                 'sub_group.f1d', 'sub_group.i1d', 'sub_group.s1d'):
        print '%s: %s' % (path, comp.get(path))

