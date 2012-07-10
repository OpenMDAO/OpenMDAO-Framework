
import pprint

class CommandRecorder(object):
    # if this is True, the @recorded and @recorded_funct decorators will cause function names 
    # and args to be recorded
    changes_are_recorded = True
    cmd_history = []
    tmp_history = []
    count = 0

    @staticmethod
    def record(fname, obj, *args, **kwargs):
        CommandRecorder.count += 1
        if CommandRecorder.count == 1:
            CommandRecorder.tmp_history.append((fname, id(obj), obj.__class__.__module__, obj.__class__.__name__,
                                                args, kwargs))
        
    @staticmethod
    def pop():
        try:
            if CommandRecorder.count == 1:
                val = CommandRecorder.tmp_history.pop()
                CommandRecorder.cmd_history.append(val)
                pprint.pprint(val)
        finally:
            CommandRecorder.count -= 1
        
        
    @staticmethod
    def reset():
        CommandRecorder.count = 0
        CommandRecorder.cmd_history = []
        CommandRecorder.tmp_history = []

