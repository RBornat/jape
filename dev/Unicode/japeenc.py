"""
        Construct special-character encoding and decoding vectors.
"""
import sys
import unicodedata

class Enc:
      def __init__(self):
         self.decoding = {}
         self.encoding = map(unichr, range(0, 256))
         self.index    = 128

      def add(self, byte):
          if byte>=128:
             if not self.decoding.has_key(byte):
                if self.index<256:
                   self.decoding[byte] = self.index
                   self.encoding[self.index] = byte
                   self.index+=1
                else:
                   raise Exception("Octet encoding overflow.")

      def write(self, out=None):
          """
            output the encoding (right now it's as a Python dict body)
          """
          if not out:
             out = sys.stdout
          for i in xrange(128, self.index):
              name = unicodedata.name(unichr(self.encoding[i]), "?")
              out.write("%03d: 0x%04x, # %s\n"%(i, self.encoding[i], name))

