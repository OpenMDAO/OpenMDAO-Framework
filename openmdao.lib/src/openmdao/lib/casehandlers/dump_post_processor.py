import sys

from query import DictList


def caseset_query_dump(data, cds, out=None):
    """
    Dump `data`, the result of a query on :class:`CaseDataset` `cds`.
    `out` is an object with a :meth:`write` method, or a filename string.
    Default output is ``sys.stdout``.
    """
    by_variable = False
    if isinstance(data, DictList):
        # by_variable() or single case.
        # Assuming DictList of all lists => by_variable().
        for item in data:
            if not isinstance(item, list):
                break
        else:
            by_variable = True
        keys = data.keys()
        rows = [data]
    elif isinstance(data, list) and hasattr(data[0], 'keys'): # by_case()
        keys = data[0].keys()
        rows = data
    else:
        raise ValueError("'data' is of unexpected type %s" % type(data))

    # Map driver_id to pathname.
    drivers = {}
    for driver in cds.drivers:
        drivers[driver['_id']] = driver['name']

    # Determine inputs & outputs, map pseudos to expression names.
    expressions = cds.simulation_info['expressions']
    metadata = cds.simulation_info['variable_metadata']
    inputs = []
    outputs = []
    pseudos = {}
    for name in sorted(keys):
        if name in metadata:
            if metadata[name]['iotype'] == 'in':
                inputs.append(name)
            else:
                outputs.append(name)
        elif '_pseudo_' in name and not name.endswith('.out0'):
            for exp_name, exp_dict in expressions.items():
                if exp_dict['pcomp_name'] == name:
                    pseudos[name] = '%s(%s)' % (exp_dict['data_type'], exp_name)
                    break
            else:
                raise RuntimeError('Cannot find %r in expressions' % name)
            outputs.append(name)
        else:
            outputs.append(name)

    # Open file if necessary.
    if out is None:
        out = sys.stdout
    elif isinstance(out, basestring):
        out = open(out, 'w')
    write = out.write

    # Dump data.
    if by_variable:
        if inputs:
            write('Inputs:\n')
            for name in inputs:
                write('    %s:\n' % name)
                for value in data[name]:
                    write('        %s\n' % value)
        if outputs:
            write('Outputs:\n')
            for name in outputs:
                write('    %s:\n'
                      % (pseudos[name] if name in pseudos else name))
                for value in data[name]:
                    if name == '_driver_id':
                        value = drivers[value]
                    write('        %s\n' % value)
    else:  # by_case
        for row in rows:
            write('Case:\n')
            if inputs:
                write('   inputs:\n')
                for name in inputs:
                    write('      %s: %s\n' % (name, row[name]))
            if outputs:
                write('   outputs:\n')
                for name in outputs:
                    value = row[name]
                    if name == '_driver_id':
                        value = drivers[row[name]]
                    if name in pseudos:
                        name = pseudos[name]
                    write('      %s: %s\n' % (name, value))



if __name__ == '__main__':
    from query import CaseDataset
    cds = CaseDataset('test/nested.json', 'json')
    caseset_query_dump(cds.data.fetch(), cds)
    caseset_query_dump(cds.data.by_variable().fetch(), cds)
    caseset_query_dump(cds.data.fetch()[-1], cds)

