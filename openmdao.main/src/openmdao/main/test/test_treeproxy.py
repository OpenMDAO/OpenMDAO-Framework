import unittest

from openmdao.main.treeproxy import TreeProxy, all_tree_names

class FakeOpaque(object):
    def __init__(self, contents):
        object.__setattr__(self, '_contents', contents)
        object.__setattr__(self, '_allobjs', all_tree_names(contents.keys()))
        
    def get(self, name, index=None):
        if index:
            val = self._contents[name]
            for i in index:
                val = val[i]
            return val
        else:
            return self._contents[name]
    
    def set(self, name, value, index=None):
        if index:
            val = self._contents[name]
            for i in index[:-1]:
                val = val[i]
            val[index[-1]] = value
        else:
            if name in self._contents:
                self._contents[name] = value
            else:
                raise AttributeError("'%s' not found" % name)
            
    def call(self, name, *args, **kwargs):
        if name in self:
            self._contents[name].__call__(*args, **kwargs)
        else:
            raise AttributeError("'%s' not found" % name)

    def __contains__(self, name):
        return name in self._allobjs


class TreeProxyTestCase(unittest.TestCase):

    def test_treeproxy(self):
        contents = {
            'a.b.f': 1.1,
            'a.array1': [1,2,3],
            'i1': 5,
            's1': 'foobar',
            'x.y.z.q': 2.2,
            'a.b.mylen': len
            }
        fo = FakeOpaque(contents)
        tp = TreeProxy(fo, '')
        self.assertEqual(tp.i1, 5)
        self.assertEqual(tp.a.b.f, 1.1)
        self.assertTrue(isinstance(tp.a.b, TreeProxy))
        self.assertEqual(tp.a.b._path, 'a.b.')
        self.assertEqual(tp.a.array1[1], 2)
        try:
            tp.a.b.q = 5
        except AttributeError as err:
            self.assertEqual(str(err), "'a.b.q' not found")
        else:
            self.fail("expected AttributeError")
            
        try:
            x = tp.a.b.q
        except AttributeError as err:
            self.assertEqual(str(err), "'a.b.q' not found")
        else:
            self.fail("expected AttributeError")
            
        tp.x.y.z.q = 99.4
        self.assertEqual(tp.x.y.z.q, 99.4)
        self.assertEqual(fo._contents['x.y.z.q'], 99.4)
        
        tp.i1 = 8
        self.assertEqual(tp.i1, 8)
        self.assertEqual(fo._contents['i1'], 8)
        
        self.assertEqual(tp.a.b.mylen([1,2,3,4]), 4)
        prox = tp.a.b.mylen
        self.assertEqual(prox([1,2,3]), 3)
        prox = tp.a.b
        self.assertEqual(prox.mylen([1,2,3,4,5]), 5)
        
        # make sure we can't access stuff without the proxy
        try:
            self.assertEqual(fo.a.b.mylen([1,2,3,4]), 4)
        except AttributeError as err:
            self.assertEqual(str(err), "'FakeOpaque' object has no attribute 'a'")
        else:
            self.fail("AttributeError expected")

        
if __name__ == '__main__':
    unittest.main()