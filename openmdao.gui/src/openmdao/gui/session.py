
""" Ref: http://caines.ca/blog/programming/sessions-in-tornado (27 OCT/09)

    From Gregg Caines blog:
    
    In case anyone's interested, here's my sessions.py that I use for doing a
    pickle-based session (stored as a file in a directory of your choosing) in
    Tornado.  Feel free to use it however you please.  If I write something
    more scalable one day, I'll post it too.

    Usage:
    
    In your application script:
    
    ::
    
      settings["session_secret"] = 'some secret password!!'
      settings["session_dir"] = 'sessions'  # the directory to store sessions in
      application.session_manager = session.TornadoSessionManager(settings["session_secret"], settings["session_dir"])

    In your RequestHandler (probably in ``__init__``):
    
    ::
    
      self.session = session.TornadoSession(self.application.session_manager, self)

    After that, you can use it like this (in get(), post(), etc):
    
    ::
    
      self.session['blah'] = 1234
      self.save()
      blah = self.session['blah']
      etc.

    The basic session mechanism is this:
    
    * Take some data, pickle it, store it somewhere.
    * Assign an id to it. Run that id through a HMAC (NOT just a hash function) to prevent tampering.
    * Put the id and HMAC output in a cookie.
    * When you get a request, load the id, verify the HMAC. If it matches, load the data from wherever you put it and depickle it.

"""

import pickle
import os.path
import hmac
import hashlib
import uuid


class Session(dict):
    """ A Session is basically a dict with a session_id and an hmac_digest string to verify access rights.
    """

    def __init__(self, session_id, hmac_digest):
        self.session_id = session_id
        self.hmac_digest = hmac_digest


class SessionManager(object):
    """ SessionManager handles the cookie and file read/writes for a Session.
    """

    def __init__(self, secret, session_dir = ''):
        self.secret = secret

        # figure out where to store the session file
        if session_dir == '':
            session_dir = os.path.join(os.path.dirname(__file__), 'sessions')
        self.session_dir = session_dir

    def _read(self, session_id):
        session_path = self._get_session_path(session_id)
        try:
            data = pickle.load(open(session_path))
            if type(data) == type({}):
                return data
            else:
                return {}
        except IOError:
            return {}

    def get(self, session_id = None, hmac_digest = None):
        # set up the session state (create it from scratch, or from parameters
        if session_id == None:
            session_should_exist = False
            session_id = self._generate_uid()
            hmac_digest = self._get_hmac_digest(session_id)
        else:
            session_should_exist = True
            session_id = session_id
            hmac_digest = hmac_digest   # keyed-Hash Message Authentication Code

        # make sure the HMAC digest we generate matches the given one, to validate
        expected_hmac_digest = self._get_hmac_digest(session_id)
        if hmac_digest != expected_hmac_digest:
            raise InvalidSessionException()

        # create the session object
        session = Session(session_id, hmac_digest)

        # read the session file, if this is a pre-existing session
        if session_should_exist:
            data = self._read(session_id)
            for i, j in data.iteritems():
                session[i] = j

        return session

    def _get_session_path(self, session_id):
        return os.path.join(self.session_dir, 'SESSION' + str(session_id))

    def set(self, session):
        session_path = self._get_session_path(session.session_id)
        session_file = open(session_path, 'wb')
        pickle.dump(dict(session.items()), session_file)
        session_file.close()

    def _get_hmac_digest(self, session_id):
        return hmac.new(session_id, self.secret, hashlib.sha1).hexdigest()

    def _generate_uid(self):
        base = hashlib.md5(self.secret + str(uuid.uuid4()))
        return base.hexdigest()


class TornadoSessionManager(SessionManager):
    """ A TornadoSessionManager is a SessionManager that is specifically for
        use in Tornado and uses Tornado's cookies.
    """

    def get(self, requestHandler = None):
        if requestHandler == None:
            return super(TornadoSessionManager, self).get()
        else:
            session_id = requestHandler.get_secure_cookie("session_id")
            hmac_digest = requestHandler.get_secure_cookie("hmac_digest")
            session = super(TornadoSessionManager, self).get(session_id, hmac_digest)
            # Added this, original code assumed tornado would set these cookies?
            if session_id is None:
                self.set(requestHandler, session)
            return session

    def set(self, requestHandler, session):
        requestHandler.set_secure_cookie("session_id", session.session_id)
        requestHandler.set_secure_cookie("hmac_digest", session.hmac_digest)
        return super(TornadoSessionManager, self).set(session)


class TornadoSession(Session):
    """ A TornadoSession is a Session object for use in Tornado.
    """

    def __init__(self, tornado_session_manager, request_handler):
        self.session_manager = tornado_session_manager
        self.request_handler = request_handler
        # get the session object's data and transfer it to this session item
        try:
            plain_session = tornado_session_manager.get(request_handler)
        except InvalidSessionException:
            plain_session = tornado_session_manager.get()

        for i, j in plain_session.iteritems():
            self[i] = j
        self.session_id = plain_session.session_id
        self.hmac_digest = plain_session.hmac_digest

    def save(self):
        self.session_manager.set(self.request_handler, self)


class InvalidSessionException(Exception):
    pass
