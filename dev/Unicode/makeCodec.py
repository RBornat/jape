#!/bin/env python2
"""
        $Id$

        A run-once program to make a python codec or a 
        java JapeCharEncoding vector from a specification
        in a format derived from the old JapeFont .enc format.

        No point going to town on this program or the specification format,
        since they'll each be used about once per Jape encoding.
"""
from re import split
from string import atoi, join

import sys
import unicodedata

def splitLine (line):
    """
        encoding lines look like

                charname % charnumber = unicodenumber
                
        charname is either .notdef, or the (metafont) identifier
        of a character in one of the JapeFont metafont driver files.
    """
    fields = filter(lambda f: f<>'', split("[%= \n]*", line))
    return fields

def doFile(f):
    file = open(f)
    for line in file.readlines():
        fields = splitLine(line)
        if fields<>[]:
           doLine(*fields)


encoding = [i  for i in xrange(0, 256)]
charname = ["" for i in xrange(0, 256)]

def doLine(name, code, unicode=None):
    """
      Make an entry in the encoding and name vectors
    """
    global encoding
    code = atoi(code)
    if unicode: 
       encoding[code] = atoi(unicode, 16)
    if name<>".notdef":
       charname[code] = name

if __name__ == '__main__':

   sys.stderr.write("Don't forget to set enc and java in this program\n")
   
   kon  = 0
   java = 0
   
   if kon:
      enc=("KonstanzEncoding")
   else:
      enc=("LauraEncoding")
      
   doFile(enc)
   
   if java:
      for row in xrange(0, 256, 4):
              fields = ["                0x%04x"%encoding[row+col] for col in xrange(0, 4)]
              print("  "+join(fields, ", ")+",")
              fields = ["%22s"%charname[row+col] for col in xrange(0, 4)]
              print("//"+join(fields, "  "))
                  
   else:
     for char in xrange(128, 256):
        if encoding[char]<>char:
             try:
              name = unicodedata.name(unichr(encoding[char]), charname[char])
             except ValueError, e:
              name=charname[char]
             print "\t0x%04x: 0x%04x, # %s"%(char, encoding[char], name)
              






