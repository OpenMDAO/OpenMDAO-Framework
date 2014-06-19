import weakref


class HasEvents(object):
    """This class provides an implementation of the IHasEvents interface."""

    def __init__(self, parent):
        self._events = []
        self._parent = None if parent is None else weakref.ref(parent)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = self.parent
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        parent = state['_parent']
        self._parent = None if parent is None else weakref.ref(parent)

    @property
    def parent(self):
        """ The object we are a delegate of. """
        return None if self._parent is None else self._parent()

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from
        the target object is 'empty' or not.  If it's empty then it's not an
        error if the replacing object doesn't have this delegate.
        """
        return len(self._events)

    def add_event(self, name):
        """Adds an event variable to be set by the driver.

        name: string
            Name of the event variable the driver should set during execution.
        """
        if name in self._events:
            self.parent.raise_exception("Trying to add event '%s' to driver, "
                                        "but it's already there" % name,
                                        AttributeError)
        # this is the easiest way to check for existence of an Event.
        # if the named event trait doesn't exist, the call to get_metadata will
        # raise an exception
        try:
            typ = self.parent.parent.get_metadata(name, 'type')
        except AttributeError:
            self.parent.raise_exception("Can't add event '%s' because it "
                                        "doesn't exist" % (name), AttributeError)
        if typ != 'event':
            self.parent.raise_exception("'%s' is not an event" % name, TypeError)

        self._events.append(name)

    def remove_event(self, name):
        """Remove the name of the specified event variable from the driver's
        list of event variables to be set during execution.
        """
        try:
            self._events.remove(name)
        except ValueError:
            self.parent.raise_exception("Trying to remove event '%s' "
                                        "that is not in the driver." % name,
                                        AttributeError)
    def get_events(self):
        """Return the list of event variables to be set by this driver."""
        return self._events

    def list_available_events(self):
        """Return a list of all available events in the driver's workflow."""

        events = []
        for comp in self.parent.workflow.__iter__():

            tset1 = set(comp._alltraits(events=True))
            tset2 = set(comp._alltraits(events=False))
            event_set = tset1.difference(tset2)
            # Remove the Enthought events common to all has_traits objects
            event_set.remove('trait_added')
            event_set.remove('trait_modified')

            for item in event_set:
                name = '%s.%s' % (comp.name, item)
                if name not in self._events:
                    events.append(name)

        return events

    def clear_events(self):
        """Remove all event variables from the driver's list."""
        self._events = []

    def set_events(self):
        """Set all events in the event list."""
        scope = self.parent.parent
        for event in self._events:
            scope.set(event, True)

    def mimic(self, target):
        """Mimic the target HasEvents by copying its event list."""
        self.clear_events()
        self._events = target.get_events()[:]

