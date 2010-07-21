
class HasEvents(object): 
    """This class provides an implementation of the IHasEvents interface"""

    def __init__(self, parent):
        self._events = set()
        self._parent = parent

    def add_event(self, name):
        """Adds a parameter to the driver. 
        
        name : string
            name of the event variable the driver should set during execution
        """
        if name in self._events: 
            self._parent.raise_exception("Trying to add event '%s' to driver, "
                                         "but it's already there" % name,
                                         AttributeError)
        if not self._parent.parent.contains(name):
            self._parent.raise_exception("Can't add event '%s' because it doesn't exist" % name,
                                         AttributeError)
        self._events.add(name)
            
    def remove_event(self, name):
        try:
            self._events.remove(name)
        except KeyError:
            self._parent.raise_exception("Trying to remove event '%s' "
                                         "that is not in the driver." % name,
                                         AttributeError)
    def get_events(self):
        return list(self._events)
    
    def clear_events(self): 
        self._events = set()
