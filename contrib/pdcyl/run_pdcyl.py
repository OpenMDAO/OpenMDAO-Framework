
from pdcyl_comp import PdcylComp


if __name__ == "__main__": # pragma: no cover         

    from numpy import array
    
    my_comp = PdcylComp()
    
    # Note, change command to point to where your copy of pdcyl resides
    my_comp.command = ['/home/ktmoore1/work/PDCYL/pdcyl']
    my_comp.load_model('transport.in')
    my_comp.run()
    
    from openmdao.main.container import dump
    dump(my_comp, recurse=True)
