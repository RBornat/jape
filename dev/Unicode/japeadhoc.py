#! /bin/env python2

"""
        $Id$
        
"""

import codecs
import sys
import getopt
import exceptions
import unicodedata
from   japeenc import *

       

def tabulate(pathin, inputcodec, enc):
    
    try:
      file = codecs.open(pathin, 'r', inputcodec)
      text=file.read()
      file.close()
    except Exception, e:
      fatal(e, "Opening file %s(%s)"%(pathin, inputcodec))

    for char in text: enc.add(ord(char))

def main():
    inputcodec  = 'utf_8'
    try:
     optlist, paths = getopt.getopt(sys.argv[1:], "hI:KL", ['help'])
    except Exception, e:
     report(e, "parsing parameters")
     optlist, paths = [], []

    enc = Enc()
    
    for flag, arg in optlist:
        if flag=='-K':
           inputcodec = 'jape_konstanz'
        elif flag=='-L':
           inputcodec = 'jape_laura'
        elif flag=='-I':
           inputcodec = arg
        elif flag=='-h' or flag=='--help':
           optlist, paths = [], []
           
    for path in paths:
        tabulate(path, inputcodec, enc)  

    enc.write()

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
       Construct an ad-hoc encoding vector
       
       Usage: [switches]* files*
       
       Switches are
          -K     -- source encoding is jape_konstanz
          -L     -- source encoding is jape_laura
          -I enc -- source encoding is enc (default utf8)
     """)
        
        
if __name__ == '__main__':
   main()
   






