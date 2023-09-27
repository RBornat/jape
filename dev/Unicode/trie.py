#!/bin/env python2

"""
        Support functions for multibyte mnemonic encodings of
        unicode characters.

        $Id$
        
"""

def index(str):  return str[0]

class Trie:
    '''Represents r: string ++> value'''

    def __init__(self):
        '''ensures r := {}'''
        self.subtries = {}
        self.value    = None

    
    def add(self, str, value=None):
        """
                r (+):= { str |-> value }
        """
        if str:
           key = index(str)
           if not self.subtries.has_key(key): self.subtries[key] = Trie()
           self.subtries[key].add(str[1:], value)
        else:
           self.value = value

    def value(self, str):
        """ 
                returns r[str] 
        """
        if str:
           key = index(str)
           if not self.subtries.has_key(key): return None
           return self.subtries[key].value(str[1:])
        else:
           return self.value

    def printOn(self, stream, ind=0):
        """
                Print a multiline representation of the mapping  
        """
        for k in self.subtries.keys():
               stream.write("%s%s"%(ind*'| ', `k`))
               subtree = self.subtries[k]
               if subtree.value:
                  stream.write(" = %s"%(`subtree.value`))
               stream.write("\n")
               subtree.printOn(stream, ind+1)

    def __repr__(self):
      if self.subtries:
        buf = " ("
        for k in self.subtries.keys():
            buf += `k`
            subtree = self.subtries[k]
            if subtree.value: buf += (" "+`subtree.value`)
            subtrierep = self.subtries[k].__repr__()
            buf += subtrierep
            if subtrierep and subtrierep[-1]==')': buf += " "

        return buf + ")"
      else:
        return ""

    def __str__(self): return self.__repr__()
    
        
class Cursor:
    """
        Represents the state of an automaton that is ``desugaring''
        an input stream by replacing occurrences of domain elements of
        a trie by the corresponding range elements. Ambiguities in
        are resolved by translating the maximal-left-substring in the
        domain.
        
    """
    def __init__(self, trie):
        self.start = trie       # original trie
        self.node  = trie       # current location
        self.value = None       # last value
        self.buf   = ""         # characters read since last output

    def move(self, ch):
        """
          Consume a character:

          Returns:
          
            None,           nothing to output
            (None, buf)     buf to output -- consumed no sugar since last output
            (val, buf)      buf to output -- consumed sugar for val since last output

        """
        key = ch

        if self.node.value:
           self.value = self.node.value
           self.buf   = ""
           
        if self.node.subtries.has_key(key): 
           self.node = self.node.subtries[key]
           self.buf += ch
           return None
           
        elif self.value:
           val        = self.value
           buf        = self.buf+ch
           self.value = None
           self.buf   = ""
           self.node  = self.start
           return (val, buf)

        else:
           buf        = self.buf+ch
           self.buf   = ""
           self.node  = self.start
           return (None, buf)

           
           
           
        
if __name__ == '__main__':
    import sys
    err = 0
    t = Trie()
    t.add('->',      r"""\arrow""")
    t.add('+>',      r"""\pfun""")
    t.add('>=',      r"""\geq""")
    t.add('->>',     r"""\surjection""")
    t.add('+>>',     r"""\partialsurjection""")
    t.add('++>',     r"""\fmap""")
    t.add('++',      r"""\cat""")
    t.add('foo',     r"""\fu""")
    t.add('bar',     r"""\bar""")
    t.add('fooobar', r"""\fubar""")
    t.printOn(sys.stderr)
    print t
    c = Cursor(t)
    sys.stdout.write(":: ")
    sys.stdout.flush()
    line = sys.stdin.readline()
    while line:
          buf  = ""
          for ch in line:
              v = c.move(ch)
              if v: 
                 r, k = v
                 if r: buf += "{"+r+"}"
                 buf += k
                 
          sys.stdout.write(buf)
          sys.stdout.flush()
          sys.stdout.write(":: ")
          sys.stdout.flush()
          line = sys.stdin.readline()

