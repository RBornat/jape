""" Python Character Mapping Codec

    For the Jape Konstanz encoding

    Bernard Sufrin

"""#"

import codecs

### Codec APIs

class Codec(codecs.Codec):

    def encode(self,input,errors='strict'):

        return codecs.charmap_encode(input,errors,encoding_map)
        
    def decode(self,input,errors='strict'):

        return codecs.charmap_decode(input,errors,decoding_map)

class StreamWriter(Codec,codecs.StreamWriter):
    pass
        
class StreamReader(Codec,codecs.StreamReader):
    pass

### encodings module API

def getregentry():

    return (Codec().encode,Codec().decode,StreamReader,StreamWriter)

### Decoding Map

decoding_map = codecs.make_identity_dict(range(256))

#
# led/ted perp has to be different -- tcl/tk assumes
# that none of an encoding works if the first special
# character has no rendition in the given font
# (BAS: Jan 2003)
#
tkBug = 0

if tkBug:
   perp = 0x2310 # laterally reversed not
   prep = 0x2319 # inverted laterally reversed not
else:
   perp = 0x2ADF # superscript top
   prep = 0x2AE0 # superscript bottom


jenc = ([
          perp,   0x00C5, 0x0393, 0x00C9, 0x00D1, 0x00D6, 0x00DC, 0x00E1, 0x00E0, 0x00E2, 0x2AE2, 0x27E8, 0x00E5, 0x00E7, 0x00E9, 0x00E8,
       ## perp,   Aring,  Gamma,  Eacute, Ntilde, Odier,  Udier,  aacute, agrave, acircm, stile3, seqbra, aring,  ccedil, eacute, egrave
          0x00EA, 0x25C1, 0x00ED, 0x00EC, 0x00EE, 0x21DD, 0x27E9, 0x97,   0x98,   0x99,   0x00F6, 0x21D0, 0x00FA, 0x00F9, 0x00FB, 0x21CC,
       ## ecircm, ltrian, iacute, igrave, icircm, sttilde,seqket, ?oacute,?ograve,?ocircm,bararr, leftar, uacute, ugrave, ucircm, harp2
          0x03a4, 0x00B0, 0x00A2, 0x00A3, 0x00A7, 0x2022, 0x2227, 0x2286, 0x00AE, 0x00A9, 0x2122, 0x00B4, 0x00A8, 0x2260, 0x00C6, 0x00D8,
       ## Tserif, degree, cent,   pound,  para,   bullet, logand, subset, regist, copyri, tradma, acute,  umlaut, noteq,  AE,     Oslash
          0x221E, 0x00B1, 0x2264, 0x2265, 0x22B8, 0x00B5, 0x2202, 0x2211, 0x220F, 0x03C0, 0x222B, 0x2297, 0x2295, 0x2126, 0x00E6, 0x00F8,
       ## infin,  plusmi, lesseq, greaeq, lolli,  mu,     delta,  Sigma,  Pi,     pi,     curlyS, xcircl, plusci, Omega,  ae,     oslash
          0x00BF, 0x00A1, 0x00AC, 0x221A, 0x0192, 0x2248, 0x2206, 0x00AB, 0x00BB, 0x2026, 0x00A0, 0x00C0, 0x00C3, 0x00D5, 0x0152, 0x0153,
       ## seuq,   alxce,  not,    root,   curlyf, curlyeq,Delta,  guibra, guiket, ...,    nbspace,Agrave, Atilde, Otilde, OE,     oe,
          0x2013, 0x2014, 0x201C, 0x201D, 0x2018, 0x2019, 0x00F7, 0x25CA, 0x21A6, 0x22A5, 0x2208, 0x21d2, 0x2234, 0x27E6, 0x27E7, 0x2229,
       ## endash, emdash, quote,  etouq,  squote, etouqs, divide, lozeng, mapsto, bottom, member, 2arrow, ::,     sembra, semket, interse,
          0x214B, 0x297D, 0x25AA, 0x201E, 0x2203, 0x27DB, 0x22A2, 0x2192, 0x2200, 0x2261, 0x2194, 0x2228, 0x039B, 0x22A7, 0x22A9, 0x222A,
       ## srepma, fishta, blksq,  lowquot,exists, stiboth,stile,  arrow,  forall, equiv,  lrarrow,logor,  Lambda, models, forces, union
          0x27DA, 0x223C, 0x2135, 0x00DB, 0x00D7, 0x2292, 0x25A1, 0x225C, prep,   0x25CF, 0x2283, 0x03BB, 0x00B8, 0x02DD, 0x0328, 0x02C7
       ## bimodel,tildop, aleph,  Ucircm, times,  sqgee,  whsqua, eqdef,  prep,   dot,    hook,   lambda, cedilla,2acute, ogonek, caron
       ])

for i in xrange(128, 256): 
    decoding_map[i] = jenc[i-128]

jenc = []

### Encoding Map

encoding_map = codecs.make_encoding_map(decoding_map)







