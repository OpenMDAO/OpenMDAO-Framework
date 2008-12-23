
# pylint: disable-msg=C0111

from openmdao.main.hierarchy import HierarchyMember


def setup():
    h1 = HierarchyMember('h1', None)
    h11 = HierarchyMember('h11', h1)    
    h12 = HierarchyMember('h12', h1)
    h121 = HierarchyMember('h121', h12)
    h122 = HierarchyMember('h122', h12)
    return (h1,h11,h12,h121,h122)


def test_pathnames():
    h1,h11,h12,h121,h122 = setup()

    assert h1.get_pathname() == 'h1'
    assert h11.get_pathname() == 'h1.h11'
    assert h12.get_pathname() == 'h1.h12'    
    assert h121.get_pathname() == 'h1.h12.h121'
    assert h122.get_pathname() == 'h1.h12.h122'    
