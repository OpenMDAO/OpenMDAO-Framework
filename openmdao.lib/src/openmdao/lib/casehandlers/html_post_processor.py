import os

def caseset_query_to_html(query, filename='cases.html'):
    cds_visualizer_path = os.path.join(
        os.path.dirname(__file__),
        'visual_post_processing',
        'case_dataset_visualizer.html'
    )

    if os.path.isfile(filename):
        os.remove(filename)

    with open(cds_visualizer_path, 'r') as file_in:

        #Opened in append mode to prevent `Query.write`
        #from closing the file
        with open(filename, 'a') as file_out:
            for line in file_in:
                #Look for line where case data needs to be inserted
                if '{case_data}' in line:
                    break

                #Otherwise, write current line of visualizer to file
                file_out.write(line)

            #Replaces '{case_data}' with actual case data
            query.write(file_out)

            #Finish generating rest of file
            for line in file_in:
                file_out.write(line)
