
def driver_map(driver, inputs, outputs):
    """ Return mapped names for `driver` as ``(inputs, outputs)``. """
    if hasattr(driver, 'parent'):
        prefix = driver.parent.get_pathname()
        if prefix:
            prefix += '.'
    else:
        prefix = ''

    pseudos = {}
    if hasattr(driver, 'eval_objectives'):
        for obj in driver.get_objectives().values():
            pseudos[obj.pcomp_name] = 'Objective(%s)' % obj

    if hasattr(driver, 'eval_responses'):
        for response in driver.get_responses().values():
            pseudos[response.pcomp_name] = 'Response(%s)' % response

    constraints = []
    if hasattr(driver, 'get_eq_constraints'):
        constraints.extend(driver.get_eq_constraints().values())
    if hasattr(driver, 'get_ineq_constraints'):
        constraints.extend(driver.get_ineq_constraints().values())
    for con in constraints:
        pseudos[con.pcomp_name] = 'Constraint(%s)' % con

    outputs = [pseudos[name] if name in pseudos else name
               for name in outputs]

    return ([prefix+name for name in inputs],
            [prefix+name for name in outputs])

