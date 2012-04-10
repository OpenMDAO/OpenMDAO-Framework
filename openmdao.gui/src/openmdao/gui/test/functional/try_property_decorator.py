class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

    @property
    def age(self):
        print "in prop"
        return self._age

    @age.setter
    def age(self, age):
        assert age >= 0
        self._age = age

    @property
    def junk(self):
        print "in prop for junk"
        return "junk"


p = Person("joe", 20)


