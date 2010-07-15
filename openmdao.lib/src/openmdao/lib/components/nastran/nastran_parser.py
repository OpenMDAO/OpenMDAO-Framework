
import sys
import re
import operator

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

        headers = []
        subcases = []
        grids = []


        for page in pages:
            # try to find header
            possibles = []
            for row, line in enumerate(page):
                possibles.append((_header_score(line, row), (row, line)))
            headers.append(max(possibles, key=operator.itemgetter(0)))

            header_row = headers[-1][1][0]

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
            merged_grid = None
            grids.append(None)

            if len(grid) != 0:

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
                        if len(columns) > 0 and columns[-1] == i-1:
                            columns[-1] = i
                        else: columns.append(i)

                if len(columns) == 0:
                    # this is not a grid
                    continue

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

                if len(split_grid) == 0 or len(split_grid[0]) == 0:
                    # this is not a grid
                    continue

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
                identical = True
                row_length = len(merged_grid[0])
                for cell in range(row_length/2):
                    if merged_grid[0][cell] != merged_grid[0][cell+row_length/2]:
                        identical = False
                        break

                # if we've seen everything twice
                singular_grid = []
                if identical and row_length % 2 == 0:

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

                if singular_grid:
                    merged_grid = singular_grid

                #print "\n".join(map(str, merged_grid))

                # now we have to normalize it... cuz our little
                # algorithm doesn't always work

                normalized_grid = []

                # sometimes we pick too many columns.
                # indicators: empty header cells and header cells with no data
                # so we have to merge them back together

                # first let's guess how many headers there should be
                most_matches = []
                for row_index in range(num_header_merged_rows+1):
                    matches = re.findall("[.a-zA-Z0-9]+ ?[.a-zA-Z0-9]* ?[.a-zA-Z0-9]*",
                               grid[row_index])
                    if len(matches) > len(most_matches):
                        most_matches = matches

                # if there are too many headers, it should be fixed
                # NOTE: this only works for non-merged headers... mainly
                # because those are the only ones affecteed.
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


            grids[-1] = merged_grid

        # sometimes grids span many pages
        # so if we find two grids with the same heading and subcase
        # coalesce!
        #print len(pages)
        #print len(headers)
        #print len(subcases)
        #print len(grids)


        last_header = ""
        last_subcase = -1
        page_index = 0
        while page_index < len(headers):
            #print headers[page_index], page_index, len(headers)
            if headers[page_index][1][1] == last_header and \
               subcases[page_index] == last_subcase:
                # merge grids (but not header again)
                #print page_index
                for row in grids[page_index][1:]:
                    grids[page_index-1].append(row)
                del headers[page_index]
                del subcases[page_index]
                del grids[page_index]

            else:
                last_header = headers[page_index][1][1]
                last_subcase = subcases[page_index]

                page_index += 1




        #for page_index in range(len(headers)):
            #print '---------------------------------------'
            #print _readable_header(headers[page_index][1][1]), subcases[page_index]
            #print '---------------------------------------'
            #if grids[page_index]:
            #    for row in grids[page_index]:
            #        print "   |   ".join(row)


        self.grids = grids
        self.headers = headers
        self.subcases = subcases

    # row width is to enable you to specify a wide row (perhaps each
    # id has two values for a certain column
    def get(self, header, subcase, constraints, column_names, row_width=1):
        # find the grid we're talking about my matching
        # the header
        myindex = None
        for index, grid in enumerate(self.grids):
            if self.headers[index][1][1].strip() == header or \
                   _readable_header(self.headers[index][1][1]) == header:
                if not subcase or \
                   (subcase and self.subcases[index] == subcase):
                    myindex = index
                    break
                else:
                    print "subcase mismatch!"
                    print "should be subcase", subcase
                    print "but the header's subcase is", self.subcases[index]

        if not myindex:
            raise Exception("Could not find " + header + " in:\n" + \
                            "\n".join(map(lambda x: x[1][1].strip(), self.headers)) + "\n - or -\n" + \
                            "\n".join(map(lambda x: _readable_header(x[1][1]), self.headers)))

        # apply the dictionary of constraints in order
        # to eliminate rows that don't work (simple where clause)
        mygrid = self.grids[myindex]
        available_rows = range(1,len(mygrid)) # ignore header
        for cname, cvalue in constraints.iteritems():
            column_num = mygrid[0].index(cname)
            to_delete = []
            row = 1 # ignore header
            while row < len(mygrid):
                if mygrid[row][column_num] != cvalue:
                    to_delete.append(row)
                else:
                    # skip the next (row_width-1) rows
                    row += row_width-1
                row += 1


            to_delete.sort(reverse=True)
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

def _readable_header(line):
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
        print "||||", _readable_header(header[1][1])
        grid = parser.grids[index]
        if grid is None:
            print
            continue
        for row in grid:
            print " | ".join(row),
            print
