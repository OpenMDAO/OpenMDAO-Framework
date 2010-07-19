#readclas.py
#     """ 
#     program reads an input file, and writes file
#     with changed values
#     input file name   = inpf_name
#     output file name  = outf_name
#     variable name     = newvar0[]
#     variable value    = newvar1[]
#
#     """

import sys, os.path
import shutil

if '.'  not in sys.path:
    sys.path.append('.')


class readfile(object):
    def __init__(self, inpf_name, outf_name,newvar0,newvar1):
        self.inpfile = inpf_name
        self.Outfile = outf_name 
        self.def_name = newvar0
        self.def_value = newvar1 

    def generate(self):
        """ create a new file """
        inpfile = self.inpfile
        Outfile = self.Outfile
        FileName = open(inpfile,'r')
        OutFName = open(Outfile,'w')
        #OutFName = open(inpfile+'O','w')

        def_name = self.def_name
        def_value= self.def_value

        #print  'def_name[0] =',def_name[0]
        #print  'def_name[1] =',def_name[1]
        #print  'def_valu[0] =',def_value[0]
        #print  'def_valu[1] =',def_value[1]

        line = FileName.readline()
        OutFName.write(line)
        line = FileName.readline()
        OutFName.write(line)
        for line in FileName:
            if('&' in line):
                OutFName.write(line)

            elif('$' in line):
                OutFName.write(line)
            elif('/' in line):
                OutFName.write(line)
            elif(line == '' ):
                OutFName.write(line)
            elif('=' not in line  ):
                OutFName.write(line)
            else:        
                fields = line.split( '=')
                var_name =  fields[0]
                var_val =  fields[-1]
                field = fields[0].strip()
                i = 0
                while i <  len(def_name):
                    if  field == def_name[i]:
                        fields[1] = str(def_value[i])+',\r\n' 
                
                    i = i + 1
                oline = fields[0] +' = '+ fields[-1]
                OutFName.write(oline) 
        FileName.close()       
        OutFName.close()

#if  __name__ == '__main__':
 
#     one = readfile(inpf_name='one_stage.inp',outf_name='one_stageO.inp', \
#
#          newvar0=['TTIN','PTIN'], newvar1=[400.,14.0])
#     one.generate()
     
