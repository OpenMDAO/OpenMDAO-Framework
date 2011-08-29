
"""
A script to run an OpenMDAO branch test triggered by a post_recieve
hook on github.
"""

import web
import json
import pprint
import StringIO

class runtests:
    def GET(self):
        i = web.input(name = 'web')
        return 'Hello, ' + web.websafe(i.name) + '!'

    def POST(self):
        data = web.input('payload')
        payload = json.loads(data.payload)
        f = StringIO.StringIO()
        f.write("repo url: %s\n" % payload['repository']['url'])
        f.write("before: %s\n" % payload['before'])
        f.write("after: %s\n" % payload['after'])
        f.write("ref: %s\n" % payload['ref'])
        f.write("commits:\n")
        for commit in payload['commits']:
            f.write("%s: %s\n" % (commit['author']['name'],
                                  commit['message']))
        print f.getvalue()
        web.sendmail('openmdao@web103.webfaction.com',
                     ['naylor.b@gmail.com'],
                     'your latest test results',
                     f.getvalue())

if __name__ == "__main__":
    urls = ('/', 'runtests')
    app = web.application(urls, globals())
    app.run()


