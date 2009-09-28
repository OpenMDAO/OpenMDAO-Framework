"""
A number of utility functions/classes that are useful in buildout recipes.
"""

import logging
from pkg_resources import WorkingSet

__empty_ws = WorkingSet()

def _find_req_deps(req, env, depnames):
    """Locate dependent distributions in the given Environment (env), based on
    the given Requirement (req).
    
    Returns a tuple of the form (found, not_found), where found is a list of
    dists and not_found is a list of requirements the did not yield a dist.
    """
    found = []
    not_found = []
    dist = env.best_match(req, __empty_ws)
    if dist is None:
        not_found.append(req)
    elif dist.project_name not in depnames:
        found.append(dist)
        depnames.add(dist.project_name)
        for r in dist.requires():
            f, nf = _find_req_deps(r, env, depnames)
            found.extend(f)
            not_found.extend(nf)
    return (found, not_found)


def find_all_deps(reqs, env):
    """Given an Environment (env), and a list of Requirements (reqs),
    returns a tuple of the form (found, not_found), where found is a
    list of found distributions and not_found is a list of requirements
    that did not yield a distribution.
    """
    found = []
    not_found = []
    depnames = set()
    for req in reqs:
        f, nf = _find_req_deps(req, env, depnames)
        found.extend(f)
        not_found.extend(nf)
    return (found, not_found)

