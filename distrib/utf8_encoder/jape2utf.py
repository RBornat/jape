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
import tempfile
import os
from   StringIO import *
from   unicodedata import name
from   japeenc import *

def translit(inputcodec, outputcodec, fmt, width=8):
    if not fmt: fmt = " %(c)03d %(c)c"
    out=codecs.EncodedFile(sys.stdout, inputcodec, outputcodec)
    for row in xrange(128, 256, width):
        for char in xrange(row, row+width):
          try:
            out.write(fmt%{'c':char})
          except exceptions.UnicodeError, e:
            out.write("?")
        out.write("\n")

def encoding(inputcodec, fmt):
    #
    # This complexity is warranted only by the curiously
    # opaque way in which the encoding/decoding machinery
    # is set up (and the nongenerality of StringIO)
    #
    if not fmt:
       fmt = "%(char)03d 0x%(enc)04x %(name)s"
       
    base     = tempfile.mktemp()
    basefile = open(base, 'w')
    out=codecs.EncodedFile(basefile, inputcodec, 'utf8')
    for char in xrange(128, 256):
        try:
          out.write("%c"%char)
        except exceptions.UnicodeError, e:
          out.write("?")
    out.close()
    out = codecs.open(base, 'r', 'utf8')
    enc = Enc()
    for char in out.read():
        enc.add(char)
    os.remove(base)
    
    for i in xrange(128, 256):
        name = unicodedata.name((enc.encoding[i]), "?")
        print fmt%{ 'char': i, 'enc': ord(enc.encoding[i]), 'name': name}


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
      out=codecs.EncodedFile(open("/dev/null", 'w'), inputcodec, outputcodec)
      count = 0
      lines = bytes.split("\n")
      for line in lines:
          count += 1
          try:
             out.write(line)
          except exceptions.UnicodeError, _:
             e = str(e) + " at line " + `count` +": " + line
             break
      fatal(e, "Translating file %s(%s) to %s(%s)"%(pathin, inputcodec, pathout, outputcodec))

def main():
    inputcodec  = 'utf_8'
    outputcodec = 'utf_8'
    outputpath  = None
    inputpath   = None
    tablewidth  = 8
    try:
     optlist, paths = getopt.getopt(sys.argv[1:], "Eho:i:O:I:KLTt:w:", ['enc=', 'help', 'table=', 'width='])
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
        elif flag=='-E' or flag=='--enc':
           encoding(inputcodec, arg)
           paths = []
        elif flag=='-T':
           translit(inputcodec, outputcodec, None, tablewidth)
           paths = []
        elif flag=='--table' or flag=='-t':
           translit(inputcodec, outputcodec, arg, tablewidth)
           paths = []
        elif flag=='-w' or flag=='--width':
           tablewidth=int(arg)
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

     Special commands:
       -h             -- print help text and exit immediately
       --help         -- ditto
       
      Human-readable 
       --table=fmt    -- output the input encoding in the output encoding using fmt
       -t fmt         -- ditto
       -T             -- equivalent to -t "%(c)03d %(c)c" 
       -w <int>
       --width=<int>  -- characters per line of the encoding table

      Compiler-readable
       --enc=fmt      -- output the inut encoding using fmt (one char per line)
       -E             -- equivlent to --enc="%(char)%03d 0x%(enc)04x %(name)s"
       
       Examples: 
       
          Print a k-coded jape theory on a (utf-8 capable) printer

                jape2utf -K equality_theory.j | lpr

          Edit a k-coded jape theory in a utf-capable editor  

                jape2utf -K equality_theory.j > foo
                editor foo
                jape2utf -O jape_konstanz  -i foo -o equality_theory.j

          Show the Konstanz encoding table in utf8

                jape2utf -K -T                 
     """)
        
        
if __name__ == '__main__':
   main()
   






