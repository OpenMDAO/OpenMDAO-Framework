import nastran_output_helpers as helpers

# TODO: add support for registering a set of helper functions
# so that anyone can just add their own... although it's
# not totally necessary because the user writes her own functions
# to access the data she wants and she can just pass in the function
# The problem with this approach is that the function loses its
# context so it cannot call any other functions that aren't in
# the helper file.

class NastranOutput(object):
    def __init__(self, filep):
        self.filep = filep

    # We are only dealing with variables that don't
    # start with an underscore.
    # When we request a member, if it doesn't exist
    # (ie, initialized to something that evalutates to false)
    # then we generate it by calling the function with the same
    # name
    def __getattr__(self, name):
        if not hasattr(helpers, name):
            raise AttributeError("Cannot compute " + name)

        filep = object.__getattribute__(self, "filep")
        result = helpers.__getattribute__(name)(filep, self)
        self.__setattr__(name, result)
        return result
