"""Defines helpful functions that are used in conjunction with
NastranComponent"""
import re

# we have to replace the old_string and it's entire block
# with the new string in the same block
def nastran_replace_inline(big_string, old_string, new_string):
    """Find a string and replace it with another in one big string.

    In big_string (probably a line), find old_string and replace
    it with new_string. The trick is that because a lot of Nastran
    is based on 8 character blocks, this will find the 8 character block
    currently occupied by old_string and replace that entire block
    with new_string.

    big_string, old_string, new_string: str

    """
    index = big_string.find(old_string)
    block = index / 8
    return big_string[:8*block] + new_string.ljust(8) + \
           big_string[8*block+8:]


def stringify(thing, length=8):
    """Convert thing to a string of a certain length.

    This function tries to make the best use of space. For
    integers and floaters, it will try to get the most sig-
    nicant digits while staying within ``length``. For
    everything else, we just try to stick in a string.

    thing : anything
        What we want to convert to a string.

    """
    if len(str(thing)) <= length:
        return str(thing)

    # okay, if the number is more than
    # 8 characters, we are treating it as a float
    # which, for nastran, means it MUST have a
    # decimal point. (lord knows why)

    # it best be a number
    if .01 <= abs(thing) < 1:
        return re.sub("0[.]", ".", str(thing))[:(length-1)]

    # we need to make sure to include the dot at the
    # end, in order to make it a float
    if -(10 ** (length-2)) < thing < -1 or 1 < thing < 10 ** (length-1):
        maybe = str(thing)[:length-1]
        if "." in maybe:
            return str(thing)[:length]
        else:
            return str(thing)[:length-1] + "."

    for i in range(length-4, 0, -1):
        myformat = "%." + str(i) + "e"
        possible = (myformat % thing).replace("e-0", "-").replace("e+0", "+")
        possible = possible.replace("e", "")
        if len(possible) <= length:
            return possible

    raise RuntimeError("Unable to reduce " + str(thing) + \
                       " to " + str(length) + " characters wide." + \
                       " If it's not a integer of a float, and just" + \
                       " some string that is too long, then this" + \
                       " function will not help you.")



