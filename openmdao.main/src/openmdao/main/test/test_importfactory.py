
# pylint: disable-msg=C0111

from openmdao.main.api import ImportFactory

def test_import():
    ifactory = ImportFactory()
    mycomp = ifactory.create('openmdao.main.component.Component')
    assert mycomp.__class__.__name__ == 'Component'

def test_bad_import():
    ifactory = ImportFactory()
    obj = ifactory.create('boguscompname')
    assert obj is None

def test_version_import():
    ifactory = ImportFactory()
    obj = ifactory.create('openmdao.main.component.Component', version='1.2')
    assert obj is None

def test_server_import():
    ifactory = ImportFactory()
    obj = ifactory.create('openmdao.main.component.Component', 
                          server='open_mdao_srv')
    assert obj is None

def test_res_desc_import():
    ifactory = ImportFactory()
    obj = ifactory.create('openmdao.main.component.Component', 
                          res_desc={'my_attribute':4})
    assert obj is None


if __name__ == '__main__':
    test_import()
    test_bad_import()
    test_version_import()
    test_server_import()
    test_res_desc_import()
    
