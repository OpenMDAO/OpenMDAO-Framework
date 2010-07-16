
import sys
import re
import operator
import copy

NORMAL_LINE_LEN = 100

class NastranParser(object):

    def __init__(self, text):
        # text should be array of lines
        self.text = text

    def parse(self):
        # parse into pages
        pages = [[]]
        for line in self.text:
            if len(re.sub(" +", "", line)) == 0:
                continue
            pages[-1].append(line)
            if "PAGE" in line:
                pages.append([])

        # if we end with newlines, then we'll have a dumb page
        # at the end
        if len(pages[-1]) == 0:
            del pages[-1]

        headers = []
        subcases = []
        grids = []

        for page in pages:
            # try to find header
            possibles = []
            for row, line in enumerate(page):
                possibles.append({"score" : _header_score(line, row),
                                  "row" : row,
                                  "actual" : line,
                                  "clean" : readable_header(line)})
            headers.append(max(possibles,
                               key=operator.itemgetter("score")))

            header_row = headers[-1]["row"]

            # is this a subcase thing?
            subcases.append(None)
            for line in page:
                search = re.search("SUBCASE (\d+)", line)
                if search:
                    subcases[-1] = int(search.group(1))
                    break

            # now we'll interpret it as a grid and parse it
            # include everything after the header except the last line
            # which has the page number
            # also remove empty lines
            row = 0
            for row in range(header_row+1, len(page)):
                if len(page[row].strip()) > 0:
                    break
            grid = page[row:-1]

            # if it has user warnings, we remove the lines from the user
            # warning to the end
            messages_line = None
            for line_index, line in enumerate(grid):
                if re.search("USER WARNING MESSAGE \d+", line) or \
                   re.search("SYSTEM INFORMATION MESSAGE \d+", line):
                    messages_line = line_index
                    break

            grid = grid[:messages_line]
            grids.append(self._parse_grid(grid))


        # sometimes grids span many pages
        # so if we find two grids with the same heading and subcase
        # coalesce!
        last_header = ""
        last_subcase = -1
        page_index = 0
        while page_index < len(headers):
            #print headers[page_index], page_index, len(headers)
            if headers[page_index]["actual"] == last_header and \
               subcases[page_index] == last_subcase:
                # merge grids (but not header again)
                #print page_index
                for row in grids[page_index][1:]:
                    grids[page_index-1].append(row)
                del headers[page_index]
                del subcases[page_index]
                del grids[page_index]

            else:
                last_header = headers[page_index]["actual"]
                last_subcase = subcases[page_index]

                page_index += 1

        self.grids = grids
        self.headers = headers
        self.subcases = subcases

    # a helper functiont to parser the grid
    def _parse_grid(self, grid):
        if len(grid) == 0:
            return

        # find columns
        columns = []
        max_len_row = len(max(grid, key=len))
        possible = True
        for i in range(max_len_row):
            possible = True
            for line in grid:
                if len(line) > i and line[i] != ' ':
                    possible = False
                    break
            if possible:
                # There may be multiple spaces in between
                # two items in a row, but we only want to
                # remember one column
                if len(columns) > 0 and columns[-1] == i-1:
                    columns[-1] = i
                else: columns.append(i)

        # this is not a grid
        if len(columns) == 0:
            return

        # if the line doesn't end with a space, we still want
        # to recognize the last column
        if not possible:
            columns.append(max_len_row)

        # grid split by columns
        split_grid = []
        for line in grid:
            split_grid.append([])
            last_column = columns[0]
            for c in columns[1:]:
                split_grid[-1].append(line[last_column:c].strip())
                last_column = c

        # this is not a grid
        if len(split_grid) == 0 or len(split_grid[0]) == 0:
            return

        # we want to save the split grid (without joining headers
        # or anything) for later.
        divided_grid = copy.deepcopy(split_grid)

        # identify header rows
        index = 0
        for index, row in enumerate(split_grid):
            if not re.match("[ ./a-zA-Z]+", row[0]):
                break

        # merge top (index-1) rows
        for row in split_grid[1:index]:
            for cell_index in range(len(row)):
                if split_grid[0][cell_index] == "" or \
                   row[cell_index] == "":
                    split_grid[0][cell_index] += row[cell_index]
                else: split_grid[0][cell_index] += " " + row[cell_index]

        num_header_merged_rows = index-1

        # now remove the merged ones:
        merged_grid = []
        for row_index, row in enumerate(split_grid):
            if row_index < 1 or row_index >= index:
                merged_grid.append(row)

        # if a grid has two identical columns: coalesce
        # first we check to see if we have two identical columns
        identical = True
        row_length = len(merged_grid[0])
        for cell in range(row_length/2):
            if merged_grid[0][cell] != merged_grid[0][cell+row_length/2]:
                identical = False
                break

        # now that we know we have two identical columns
        # add new rows. The convention we're following now
        # is that something like:
        #
        #  header  header
        #  ------  ------
        #  1       2
        #  3       4
        #  5       6
        #
        # will turn into [1,2,3,4,5,6]
        #
        if identical and row_length % 2 == 0:
            singular_grid = []
            singular_grid.append([])
            for cell in merged_grid[0][:row_length/2]:
                singular_grid[0].append(cell)

            for row in merged_grid[1:]:
                singular_grid.append(
                    ["" for i in range(len(row)/2)])
                singular_grid.append(
                    ["" for i in range(len(row)/2)])

                for index in range(len(row)):
                    if index < len(row)/2:
                        singular_grid[-2][index] = row[index]
                    else: singular_grid[-1][index-len(row)/2] = row[index]

            merged_grid = singular_grid

        # normalize the grid. Sometimes our algorithm
        # is a little to promiscuous when choosing columns, so
        # we have to merge some columns together now.

        # sometimes we pick too many columns.
        # indicators: empty header cells and header cells with no data
        # so we have to merge them back together

        # first let's guess how many headers there should be
        most_matches = []
        for row_index in range(num_header_merged_rows+1):
            matches = re.findall("[-.a-zA-Z0-9]+ ?[-.a-zA-Z0-9]* ?[-.a-zA-Z0-9]*",
                       grid[row_index])
            if len(matches) > len(most_matches):
                most_matches = matches

        # if there are too many headers, it should be fixed
        # NOTE: this only works for non-merged headers... mainly
        # because those are the only ones affected.
        if len(merged_grid[0]) > len(most_matches):
            most_matches = map(lambda x: x.strip(), most_matches)
            accounted = 0
            to_merge = []
            cell_index = 0
            while cell_index < len(merged_grid[0])-1:
                if accounted == len(most_matches):
                    # this can indicate that it wasnt a grid
                    #print "i guess there weren't enough matches...? ..."
                    break
                if merged_grid[0][cell_index] == most_matches[accounted]:
                    # we want to look at the next one UNLESS we
                    # currently have a column of empties and the
                    # the next guy is empty
                    #print "BOOOO", cell_index
                    #print [row[cell_index] == "" for row in merged_grid[1:]]
                    #print merged_grid[0][cell_index+1]
                    if all([row[cell_index] == "" for row in \
                            merged_grid[1:]]) and \
                            merged_grid[0][cell_index+1] == "":
                        to_merge.append((cell_index, cell_index+1))
                        accounted += 1
                        cell_index += 2
                    else:
                        accounted += 1
                # if we see that we can combine two columns, we do so
                # TODO: We probably want to examine the contents
                # of the columns instead of just the headers. We
                # could peg certain headers and then fill in the rest
                # The way it works now kinda assumes nastran will
                # always left align their numbers... which I think
                # is trueish
                elif merged_grid[0][cell_index] + " " + merged_grid[0][cell_index+1] == most_matches[accounted] or merged_grid[0][cell_index] + merged_grid[0][cell_index+1] == most_matches[accounted] or (merged_grid[0][cell_index].startswith(most_matches[accounted]) and merged_grid[0][cell_index+1] == ""):
                    to_merge.append((cell_index, cell_index+1))
                    accounted += 1
                    cell_index += 1

                else: cell_index += 1

            normalized_grid = _merge_columns(merged_grid, to_merge)
            if normalized_grid:
                merged_grid = normalized_grid

        # we continue our process of normalization, but this time
        # from the bottom. We are going to consider how the data is
        # layed out and if there are not enough headers, we will make
        # them. Do note, we will not delete headers here if it
        # seems there are too many because (1) this is done in other
        # places and (2) empty columns can exist.
        empties = []
        # we only want data
        for row in grid[num_header_merged_rows+1:]:
            empties.append(set())
            for index in range(max_len_row):
                if index >= len(row) or row[index] == " ":
                    empties[-1].add(index)

        # get all communal empties.
        common = sorted(list(reduce(lambda x, y: x.intersection(y), \
                                    empties)))
        divisions = []
        last = common[0]
        for item in common[1:]:
            if item != last+1:
                divisions.append(last)
            last = item

        # note, this cannot apply to two column thingies,
        # cuz we are purposely reducing the number of headers
        if len(divisions) > len(merged_grid[0]) and not identical \
               and num_header_merged_rows > 0:
            # this stuff also only applies to multi-header
            # stuff. If not, where would the header go?


            # let's try and line up our data columns with
            # the last row of headers, instead of entire things.
            # when only considering the last row, sometimes its
            # pretty sane.
            little_grid = self._parse_grid(grid[num_header_merged_rows:])
            # now try to match up the family to the elements. In
            # general, the elements will be under the family. So
            # we just have to go through the headers and see
            # who got stuck together. If more than one little header
            # got stuck, it was probably in a family.

            complex_header_index = 0
            last_complex_header = merged_grid[0][complex_header_index]
            complex_groups = [[]]
            for index, little_header in enumerate(little_grid[0]):
                if not little_header:
                    continue
                if little_header in last_complex_header:
                    complex_groups[-1].append(index)
                    _, _, last_complex_header = \
                       last_complex_header.partition(little_header)

                    # okay, if the next guy is not in the
                    # the reamining thing, then just go onto the next
                    # one.
                    if index+1 < len(little_grid[0]) and \
                           (little_grid[0][index+1] not in last_complex_header):
                        complex_groups.append([])
                        complex_header_index += 1
                        last_complex_header = merged_grid[0][complex_header_index]
                else:
                    complex_groups.append([])
                    complex_header_index += 1
                    last_complex_header = merged_grid[0][complex_header_index]
            # now we have to change the family member jumbo
            # into difference family-member1, family-member2 column
            # names
            seen = 0
            for gindex, group in enumerate(complex_groups):
                if len(group) <= 1:
                    little_grid[0][seen] = merged_grid[0][gindex]
                    seen += len(group)
                    continue
                # implicit assumption that there are only two
                # rows of headers
                family_name = divided_grid[0][gindex]
                for element in group:
                    little_grid[0][element] = family_name + " " + \
                                              little_grid[0][element]

                seen += len(group)

            merged_grid = little_grid



        return merged_grid


    # row width is to enable you to specify a wide row (perhaps each
    # id has two values for a certain column
    def get(self, header, subcase, constraints, column_names, row_width=1):
        # find the grid we're talking about my matching
        # the header
        myindex = None
        maybeindex = None # for partial matches
        for index, grid in enumerate(self.grids):
            if self.headers[index]["actual"].strip() == header or \
                   self.headers[index]["clean"] == header:
                if not subcase or \
                   (subcase and self.subcases[index] == subcase):
                    myindex = index
                    break
                else:
                    print "subcase mismatch!"
                    print "should be subcase", subcase
                    print "but the header's subcase is", self.subcases[index]
            if header in self.headers[index]["actual"].strip() or \
                header in self.headers[index]["clean"]:
                if not subcase or \
                   (subcase and self.subcases[index] == subcase):
                    maybeindex = index


        if myindex is None:
            if maybeindex is None:
                raise Exception("Could not find " + header + \
                                " in:\n" + \
                                "\n".join(map(lambda x: x["actual"].strip(), self.headers)) + "\n - or -\n" + \
                                "\n".join(map(operator.itemgetter("clean"), self.headers)))
            else:
                myindex = maybeindex

        # apply the dictionary of constraints in order
        # to eliminate rows that don't work (simple where clause)
        mygrid = self.grids[myindex]
        available_rows = range(1,len(mygrid)) # ignore header
        to_delete = set([])
        for cname, cvalue in constraints.iteritems():
            column_num = mygrid[0].index(cname)
            row = 1 # ignore header
            while row < len(mygrid):
                if mygrid[row][column_num] != cvalue:
                    to_delete.add(row)
                else:
                    # skip the next (row_width-1) rows
                    row += row_width-1
                row += 1


        to_delete = sorted(list(to_delete), reverse=True)
        for row in to_delete:
            available_rows.remove(row)

        # now, in the remaining rows, we will
        # take the columns specified
        if column_names == "*":
            column_nums = range(len(mygrid[0]))
        else:
            try:
                column_nums = map(mygrid[0].index, column_names)
            except ValueError, ve:
                print "Could not find column names", column_names, \
                      "in", mygrid[0]
                raise

        result = []
        for row in available_rows:
            result.append([])
            for column in column_nums:
                result[-1].append(mygrid[row][column])

        if row_width > 1:
            for i in range(0, len(result),row_width):
                result = result[:i] + [zip(*result[i:i+row_width])] + result[i+row_width:]


        return result


# returns int between 0-200
# 200 is a sure header... it's really just that big
# numbers are more likely than smaller numbers
def _header_score(line, row):
    # if the line is done in dumbcaps, there
    # is a high probability it is a header
    if len(line.strip()) < 2:
        return 0

    # if no alphabetic characters, then no go
    if len(filter(lambda x: x.isalpha(), line)) < 1:
        return 0

    if _is_dumbcaps(line):
        return 200

    score = 0

    # we favor being at the top of the page
    score += 50 - 5 * row

    # we favor having fewer words
    words = filter(lambda x: len(x.strip()) > 1, line.strip().split(' '))
    score += 40 - 4 * len(words)

    # we heavily favor being near the middle
    # so we count spaces on the left
    score -= NORMAL_LINE_LEN/2 - len(filter(lambda x: len(x) == 0, line.rstrip(' ').split(' ')))

    # If subcase is in the header, almost immediatley disqualified
    score -= 100 if "SUBCASE" in line else 0

    return score


def _is_dumbcaps(line):
    good = bad = 0
    seen_space = True
    for c in line:
        if c == ' ':
            if not seen_space: good += 1
            seen_space = True
        else:
            if not seen_space:
                bad += 1
            else: good += 1
            seen_space = False
    if good > 1.5 * bad:
        return True
    return False

def readable_header(line):
    if line[0].isdigit():
        line = line[1:]

    if _is_dumbcaps(line):
        line = line.strip()

        # in order to avoid nasty six spaces
        line = re.sub('(?P<last>[a-zA-Z])   ', '\g<last>_', line)
        line = line.replace(' ', '')
        line = line.replace('_', ' ')
        return line.lower()

    else:
        line = line.strip()
        line = re.sub(" +", " ", line)
        return line.lower()

# no overlapping sets of columns to merge
# columns_to_merge should be of the form [(x,y), (w,z)]
# where x<y<w<z
def _merge_columns(grid, columns_to_merge):
    # merge the values
    to_delete = set()
    for staying, coming in columns_to_merge:
        to_delete.add(coming)
        for row in grid:
            if row[staying] == "" or row[coming] == "":
                row[staying] += row[coming]
            else:
                row[staying] += " " + row[coming]

    # delete the old columns
    newgrid = []
    for row in grid:
        newgrid.append([])
        for cell_index in range(len(row)):
            if cell_index not in to_delete:
                newgrid[-1].append(row[cell_index])

    return newgrid


# If we want to test how NastranParser will
# parse a file, we can just run this file
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print "This program takes one argument: the file you wish to analyze"
        sys.exit(1)

    fh = open(sys.argv[1], "r")
    text = fh.read().split("\n")
    fh.close()
    parser = NastranParser(text)
    parser.parse()

    assert len(parser.headers) == len(parser.grids)
    for index, header in enumerate(parser.headers):
        print "||||", header["clean"]
        grid = parser.grids[index]
        if grid is None:
            print
            continue
        for row in grid:
            print " | ".join(row),
            print
