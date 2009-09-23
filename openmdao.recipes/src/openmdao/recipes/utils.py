"""
A number of utility functions/classes that are useful in buildout recipes.
"""

import logging
from pkg_resources import WorkingSet

__empty_ws = WorkingSet()

def _find_req_deps(req, env, deps, depnames):
    """Locate dependent distributions in the given Environment (env),
    based on the given Requirement (req).
    """
    dist = env.best_match(req, __empty_ws)
    if dist is None:
        logging.error('No distrib found for %s' % req)
    else:
        if dist.project_name not in depnames:
            deps.append(dist)
            for r in dist.requires():
                _find_req_deps(r, env, deps, depnames)


def find_all_deps(reqs, env):
    """Return a unique list of dependent distributions found in 
    the given Environment (env), based on the given list of 
    Requirements (reqs).
    """
    depnames = set()
    deps = []
    for req in reqs:
        _find_req_deps(req, env, deps, depnames)
    return deps

