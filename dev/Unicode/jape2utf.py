#! /bin/env python2

"""
        $Id$

        This little program translates between unicode
        encodings known to the python codecs base. 

        It also knows about the jape_konstanz and jape_laura
        encodings.

        
"""

import codecs
import sys
import getopt
import exceptions

def translate(pathin, pathout, inputcodec, outputcodec):
    
    try:
      file = open(pathin, 'r')
    except Exception, e:
      fatal(e, "Opening file %s(%s)"%(pathin, inputcodec))
    try:
      bytes = file.read()  
      file.close()
      if not pathout:
         pathout = 'stdout'
         out     = sys.stdout
      else:
         out     = open(pathout, 'w', 0766)
         report (pathout, "Writing output file %s (%d)"%(pathout, len(bytes)))
       
      out=codecs.EncodedFile(out, inputcodec, outputcodec)
      out.write(bytes)
      out.close()
    except exceptions.UnicodeError, e:
      fatal(e, "Translating file %s(%s) to %s(%s)"%(pathin, inputcodec, pathout, outputcodec))

def main():
    inputcodec  = 'utf_8'
    outputcodec = 'utf_8'
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
        elif flag=='-O':
           outputcodec = arg        
        elif flag=='-i':
           inputpath = arg      
        elif flag=='-o':
           outputpath = arg
        elif flag=='-h' or flag=='--help':
           optlist, paths = [], []

        if inputpath and outputpath:
           translate(inputpath, outputpath, inputcodec, outputcodec)  
           inputpath = outputpath = None
           
    for path in paths:
        translate(path, None, inputcodec, outputcodec)

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
     This program translates between unicode (and jape) encodings.
     Usage: jape2utf [command*]
     Commands are:
       -L             -- set jape_laura input encoding
       -K             -- set jape_konstanz input encoding
       -I inputcodec  -- set given input encoding  (default utf8)
       -O outputcodec -- set given output encoding (default utf8)
       -i inpath      -- translate from file at inpath
       -o outpath     -- translate to file at outpath
       path           -- translate from file at path to stdout
       -h             -- print help text and exit immediately
       --help         -- ditto

       Examples: 
       
          Print a k-coded jape theory on a (utf-8 capable) printer

                jape2utf -K equality_theory.j | lpr

          Edit a k-coded jape theory in a utf-capable editor  

                jape2utf -K equality_theory.j > foo
                editor foo
                jape2utf -O jape_konstanz  -i foo -o equality_theory.j

                
     """)
        
        
if __name__ == '__main__':
   main()
   

