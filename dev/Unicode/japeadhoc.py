#! /bin/env python2

"""
        $Id$
        
"""

import codecs
import sys
import getopt
import exceptions
import unicodedata

def tabulate(pathin, pathout, inputcodec):
    
    try:
      file = codecs.open(pathin, 'r', inputcodec)
      text=file.read()
      file.close()
    except Exception, e:
      fatal(e, "Opening file %s(%s)"%(pathin, inputcodec))
    try:
      if not pathout:
         pathout = 'stdout'
         out     = sys.stdout
      else:
         out     = open(pathout, 'w', 0766)

      decoding = {}
      encoding = [ i for i in xrange(0, 256) ]
      index    = 128
      for char in text:
          byte = ord(char)
          if byte>=128:
             if not decoding.has_key(byte):
                if index<256:
                   decoding[byte] = index
                   encoding[index] = byte
                   index+=1
                else:
                   fatal("Too many special characters for octet-encoding", "File %s(%s)"%(pathin, inputcodec))

      #
      # output the encoding (right now it's as a Python dict body)
      #
      for i in xrange(128, index):
          name = unicodedata.name(unichr(encoding[i]), "?")
          out.write("%03d: 0x%04x, # %s\n"%(i, encoding[i], name))
             
      
      out.close()
    except exceptions.UnicodeError, e:
      fatal(e, "Translating file %s(%s) to %s(%s)"%(pathin, inputcodec, pathout, outputcodec))

def main():
    inputcodec  = 'utf_8'
    outputpath  = None
    inputpath   = None
    try:
     optlist, paths = getopt.getopt(sys.argv[1:], "hO:I:KL", ['help'])
    except Exception, e:
     report(e, "parsing parameters")
     optlist, paths = [], []
    for flag, arg in optlist:
        if flag=='-K':
           inputcodec = 'jape_konstanz'
        elif flag=='-L':
           inputcodec = 'jape_laura'
        elif flag=='-I':
           inputcodec = arg
        elif flag=='-i':
           inputpath = arg      
        elif flag=='-o':
           outputpath = arg
        elif flag=='-h' or flag=='--help':
           optlist, paths = [], []

        if inputpath and outputpath:
           tabulate(inputpath, outputpath, inputcodec)  
           inputpath = outputpath = None
           
    for path in paths:
        tabulate(path, None, inputcodec)  

    if not optlist and not paths:
       usage()

def fatal(error, gloss=""):
          report(error, gloss, fatal=1)

def report(error, gloss="", fatal=None):
          if gloss:
             sys.stderr.write(gloss+": ")
          sys.stderr.write(str(error)+"\n")
          if fatal:
             sys.exit(1)

def usage():
     report("""$Revision$
       Switches are
          -K     -- source encoding is jape_konstanz
          -L     -- source encoding is jape_laura
          -I enc -- source encoding is enc (default utf8)
     """)
        
        
if __name__ == '__main__':
   main()
   



