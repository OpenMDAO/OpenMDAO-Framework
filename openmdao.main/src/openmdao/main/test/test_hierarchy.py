
# pylint: disable-msg=C0111

from openmdao.main import HierarchyMember


def setup():
    h1 = HierarchyMember('h1', None, desc="a hierarchy member")
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

def test_doc():
    h1,h11,h12,h121,h122 = setup()
    
    assert h1.__doc__ == "a hierarchy member"
    
def test_error_handling():
    h1,h11,h12,h121,h122 = setup()
    
    try:
        h121.raise_exception("bad value", ValueError)
    except ValueError, err:
        if str(err) != "h1.h12.h121: bad value":
            raise AssertionError(str(err)+"==h1.h12.h121: bad value")
        
    h121.error("can't start server")
    h121.warning("I wouldn't recommend that")
    h121.info("fyi")
    h121.debug("dump value = 3")
    
    
