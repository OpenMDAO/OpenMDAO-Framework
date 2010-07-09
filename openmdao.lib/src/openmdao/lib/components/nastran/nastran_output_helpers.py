tandr = ["T1", "T2", "T3", "R1", "R2", "R3"]
stresses = ["axial", "axial-safety", "torsional", "torsional-safety"]
forces = ["axial", "torque"]

# These helpers should have the same name as the
# variables they are supposed to generate.

# Each function returns the result it computes

# IMPORTANT: do to the fickle nature of the
# anchors in the FileParsers, you must make sure
# that no one is changing the anchors once you set it.
# This can be very hard to catch: if you set the anchor
# and then look at something in output, it is
# possible (likely) that in order to calculate the
# value you just requested, they mess up the anchor
# on you. Naturally, the function you call should
# be well behaved and save your anchor, but beware.

# A convenient way to save the anchor is to decorate
# your function with a conveniently named save_anchor
# decorator

# IMPORTANT: the function f cannot read the
# varaible output.f because that will recursively
# call __getattribute__

# Beware of innocent looking expressions which
# in fact read the variable. Something like
# output.f[0] = 5 does read output.f, so beware.

# There really shouldn't be a reason you're accessing
# output.f, for most of the stuff, you look at the
# existing data in the output object and then
# parse the stuff you care about from filep

# This should probably not be used. It's better to use
# NastranParser to get the grid data. But these can be
# useful. One good example is reading the weight of
# a model. It's not really in a grid, so it can be
# parsed effectively with a helper function

# please save the anchor
def save_anchor(fn):
    def new(filep, output):
        old_anchor = filep.current_row
        filep.reset_anchor()
        result = fn(filep, output)
        filep.current_row = old_anchor
        return result
    return new

# to format a "N A S T R A N" string
def dumbcaps(s):
    return " ".join(list(s))

@save_anchor
def number_of_grid_points(filep, output):
    filep.reset_anchor()
    filep.mark_anchor("NUMBER OF GRID     POINTS")
    return filep.transfer_var(0,6)

@save_anchor
def number_of_crod_elements(filep, output):
    filep.reset_anchor()
    filep.mark_anchor("NUMBER OF CROD     ELEMENTS")
    return filep.transfer_var(0,6)

@save_anchor
def displacement_vector(filep, output):

    points = output.number_of_grid_points

    filep.reset_anchor()
    filep.mark_anchor(dumbcaps("DISPLACEMENT VECTOR"))

    result = [{} for i in range(points)]
    for row in range(3,3+points):
        for field, item in enumerate(tandr):
            result[row-3][item] = filep.transfer_var(row, field+3)
    return result

@save_anchor
def forces_in_rod_elements(filep, output):
    filep.reset_anchor()
    filep.mark_anchor(dumbcaps("FORCES IN ROD ELEMENTS  (CROD)"))

    forces = [{} for i in range(total_crods)]

    for element in range(0, total_crods-1, 2):
        forces[element]["axial"] = filep.transfer_var(3+element/2,2)
        forces[element]["torque"] = filep.transfer_var(3+element/2,3)

        forces[element+1]["axial"] = filep.transfer_var(3+element/2,5)
        forces[element+1]["torque"] = filep.transfer_var(3+element/2,6)

    if total_crods % 2 == 1:
        forces[total_crods-1]["axial"] = filep.transfer_var(3+total_crods/2,2)
        forces[total_crods-1]["torque"] = filep.transfer_var(3+total_crods/2,3)

    return forces


@save_anchor
def stresses_in_rod_elements(filep, output):
    filep.reset_anchor()
    filep.mark_anchor(dumbcaps("STRESSES IN ROD ELEMENTS ") + "  " +\
                      dumbcaps(" (CROD)"))
    total_crods = output.number_of_crod_elements
    stresses = [{} for i in range(total_crods)]

    # for 3 guys, we only want to do one line (then a half)
    # for four guys, we want to do 2 lines
    for accounted in range(0, total_crods-1, 2):
        spot = 2 # what spot are we looking at?

        spot = _parse_half_column_stresses(filep, stresses, spot, accounted)

        # we just encounted another element!
        spot += 1

        _parse_half_column_stresses(filep, stresses, spot, accounted+1)

    # now, it's possible that there's still one half a row left
    if total_crods % 2 == 1:
        _parse_half_column_stresses(filep, stresses, 2, total_crods-1)

    return stresses


def _parse_half_column_stresses(filep, stresses, spot, row):
    stresses[row]["axial"] = filep.transfer_var(3+row/2, spot)
    spot += 1
    # this might be dangerous, but the output is rather annoying
    if stresses[row]["axial"] != 0.0:
        stresses[row]["axial-safety"] = \
                    filep.transfer_var(3 + row/2, spot)
        spot += 1

    stresses[row]["torsional"] = filep.transfer_var(3 + row/2, spot)
    spot += 1
    if stresses[row]["torsional"] != 0.0:
        stresses[row]["torsional-safety"] = \
                    filep.transfer_var(3 + row/2, spot)
        spot += 1

    return spot

@save_anchor
def masses(filep, output):
    filep.reset_anchor()
    filep.mark_anchor("MASS AXIS SYSTEM (S)")
    masses = []
    for i in range(1,4):
        masses.append(filep.transfer_var(i, 2))
    return masses



# for the testing
def nothing(filep, output):
    return 1
