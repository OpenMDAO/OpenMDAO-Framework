
import ast

def parse_ast(contents, fname, mode='exec'):
    """Wrapper for ast.parse() that cleans the contents of CRs and ensures
    it ends with a newline."""
    contents = contents.replace('\r', '')  # py26 barfs on CRs
    if not contents.endswith('\n'):
        contents += '\n'  # to make ast.parse happy :(
    return ast.parse(contents, filename=fname, mode=mode)


def text_to_node(text, lineno=None):
    """Given a Python source string, return the corresponding AST node.
    The outer Module node is removed so that the node corresponding to the
    given text can be added to an existing AST.
    """
    modnode = ast.parse(text, 'exec')
    if len(modnode.body) == 1:
        node = modnode.body[0]
    else:
        node = modnode.body

    # If specified, fixup line numbers.
    if lineno is not None:
        node.lineno = lineno
        node.col_offset = 0
        for child in ast.iter_child_nodes(node):
            child.lineno = lineno
            child.col_offset = 0

    return node
