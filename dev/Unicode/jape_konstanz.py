""" Python Character Mapping Codec

    For the Jape Laura encoding

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
decoding_map.update({
	0x0080: 0x22a4, # DOWN TACK
	0x0082: 0x0393, # GREEK CAPITAL LETTER GAMMA
	0x008a: 0x2ae2, # believes
	0x0091: 0x2282, # SUBSET OF
	0x0095: 0x22a6, # ASSERTION
	0x009a: 0x27fe, # double_mapsto
	0x009f: 0x21c6, # LEFTWARDS ARROW OVER RIGHTWARDS ARROW
	0x00a1: 0x2218, # RING OPERATOR
	0x00a4: 0x00a7, # SECTION SIGN
	0x00a5: 0x2219, # BULLET OPERATOR
	0x00a6: 0x2227, # LOGICAL AND
	0x00a7: 0x2286, # SUBSET OF OR EQUAL TO
	0x00ad: 0x2260, # NOT EQUAL TO
	0x00af: 0x2205, # EMPTY SET
	0x00b0: 0x221e, # INFINITY
	0x00b2: 0x2264, # LESS-THAN OR EQUAL TO
	0x00b3: 0x2285, # NOT A SUPERSET OF
	0x00b4: 0x22b8, # MULTIMAP
	0x00b5: 0x03bc, # GREEK SMALL LETTER MU
	0x00b6: 0x03b4, # GREEK SMALL LETTER DELTA
	0x00b7: 0x03a3, # GREEK CAPITAL LETTER SIGMA
	0x00b8: 0x03a0, # GREEK CAPITAL LETTER PI
	0x00b9: 0x03c0, # GREEK SMALL LETTER PI
	0x00ba: 0x222b, # INTEGRAL
	0x00bb: 0x2297, # CIRCLED TIMES
	0x00bc: 0x2295, # CIRCLED PLUS
	0x00bd: 0x03a9, # GREEK CAPITAL LETTER OMEGA
	0x00c2: 0x00ac, # NOT SIGN
	0x00c3: 0x2713, # CHECK MARK
	0x00c4: 0x1d53d, # fatletF
	0x00c5: 0x2248, # ALMOST EQUAL TO
	0x00c6: 0x0394, # GREEK CAPITAL LETTER DELTA
	0x00c7: 0x27ea, # muchless
	0x00c8: 0x27eb, # muchgreater
	0x00c9: 0x22ef, # MIDLINE HORIZONTAL ELLIPSIS
	0x00d0: 0x23af, # hyphen
	0x00d1: 0x23bd, # underbar
	0x00d2: 0x201c, # LEFT DOUBLE QUOTATION MARK
	0x00d3: 0x201d, # RIGHT DOUBLE QUOTATION MARK
	0x00d6: 0x00b8, # CEDILLA
	0x00d7: 0x22c4, # DIAMOND OPERATOR
	0x00d8: 0x21a6, # RIGHTWARDS ARROW FROM BAR
	0x00d9: 0x22a5, # UP TACK
	0x00da: 0x2208, # ELEMENT OF
	0x00db: 0x21d2, # RIGHTWARDS DOUBLE ARROW
	0x00dc: 0x2982, # fatcolon
	0x00dd: 0x27e6, # fatbra
	0x00de: 0x27e7, # fatket
	0x00df: 0x2229, # INTERSECTION
	0x00e0: 0x05e6, # HEBREW LETTER TSADI
	0x00e2: 0x2afe, # boxsquare
	0x00e4: 0x2203, # THERE EXISTS
	0x00e6: 0x22a2, # RIGHT TACK
	0x00e7: 0x2192, # RIGHTWARDS ARROW
	0x00e8: 0x2200, # FOR ALL
	0x00e9: 0x2261, # IDENTICAL TO
	0x00ea: 0x2194, # LEFT RIGHT ARROW
	0x00eb: 0x2228, # LOGICAL OR
	0x00ec: 0x039b, # GREEK CAPITAL LETTER LAMDA
	0x00ed: 0x22a7, # MODELS
	0x00ef: 0x222a, # UNION
	0x00f2: 0x05d0, # HEBREW LETTER ALEF
	0x00f4: 0x00d7, # MULTIPLICATION SIGN
	0x00f5: 0x2292, # SQUARE ORIGINAL OF OR EQUAL TO
	0x00f7: 0x2259, # ESTIMATES
	0x00f8: 0x22bb, # XOR
	0x00f9: 0x22a1, # SQUARED DOT OPERATOR
	0x00fa: 0x2283, # SUPERSET OF
	0x00fb: 0x03bb, # GREEK SMALL LETTER LAMDA
	0x00fc: 0x21d4, # LEFT RIGHT DOUBLE ARROW
	0x00fd: 0x21d5, # UP DOWN DOUBLE ARROW
})

### Encoding Map

encoding_map = codecs.make_encoding_map(decoding_map)


