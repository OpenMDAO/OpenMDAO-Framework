
import threading
import inspect
from cStringIO import StringIO

import pprint
import types

def _find_name(obj, frame):
    for k,v in frame.f_locals.items():
        if v is obj:
            return k
    for k,v in frame.f_globals.items():
        if v is obj:
            return k

def _cvt_args(args, frame):
    newargs = args[:]
    for i,arg in enumerate(newargs):
        name = _find_name(arg, frame)
        if name is not None:
            newargs[i] = name
    return newargs
        
def _cvt_kwargs(kwargs, frame):
    newargs = []
    for key, value in kwargs.items():
        try:
            newargs.append((key, value.__name__))
        except:
            newargs.append((key, value))
    return newargs
        
class CommandRecorder(object):
    # if this is True, the @recorded and @recorded_funct decorators will cause function names 
    # and args to be recorded
    active = True
    cmd_history = []
    tmp_history = []
    obj_map = {}  # maps ids to names
    count = 0
    _lock = threading.Lock()

    @staticmethod
    def record(fnc, obj, args, kwargs):
        with CommandRecorder._lock:
            CommandRecorder.count += 1
            if CommandRecorder.count == 1:
                stack = inspect.getouterframes(inspect.currentframe())
                for i,s in enumerate(stack):
                    if s[3] == '_wrap':
                        frame = stack[i+1]
                        break
                CommandRecorder.tmp_history.append((
                    fnc.__name__, 
                    _cvt_args(args, frame[0]), 
                    _cvt_kwargs(kwargs, frame[0]), 
                    {'stack': frame[1:] }
                ))
                #CommandRecorder.tmp_history.append((fname, id(obj), obj.__class__.__module__, obj.__class__.__name__,
                                                    #args, kwargs))
        
    @staticmethod
    def pop():
        with CommandRecorder._lock:
            try:
                if CommandRecorder.count == 1:
                    val = CommandRecorder.tmp_history.pop()
                    CommandRecorder.cmd_history.append(val)
                    pprint.pprint(val)
            finally:
                CommandRecorder.count -= 1
        
        
    @staticmethod
    def reset():
        with CommandRecorder._lock:
            CommandRecorder.count = 0
            CommandRecorder.cmd_history = []
            CommandRecorder.tmp_history = []


def recorded(fnc):
    """A decorator that records a method call and its args for later playback.
    """
    def _wrap(self, *args, **kwargs):
        if CommandRecorder.active:
            CommandRecorder.record(fnc, self, (self,)+args, kwargs)
            try:
                ret = fnc(self, *args, **kwargs)
            finally:
                CommandRecorder.pop()
            return ret
        else:
            return fnc(self, *args, **kwargs)
    return _wrap


def recorded_funct(fnc):
    """A decorator that records a function call and its args for later playback.
    """
    def _wrap(*args, **kwargs):
        if CommandRecorder.active:
            CommandRecorder.record(fnc, None, args, kwargs)
            try:
                ret = fnc(*args, **kwargs)
            finally:
                CommandRecorder.pop()
            return ret
        else:
            return fnc(*args, **kwargs)
    return _wrap

