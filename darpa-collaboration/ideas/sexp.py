# sexpParser.py
#
# Demonstration of the pyparsing module, implementing a simple S-expression
# parser.
#
# Copyright 2007, by Paul McGuire
#
"""
BNF reference: http://theory.lcs.mit.edu/~rivest/sexp.txt

<sexp>	  	:: <string> | <list>
<string>   	:: <display>? <simple-string> ;
<simple-string>	:: <raw> | <token> | <base-64> | <hexadecimal> |
			   <quoted-string> ;
<display>  	:: "[" <simple-string> "]" ;
<raw>	   	:: <decimal> ":" <bytes> ;
<decimal>  	:: <decimal-digit>+ ;
		-- decimal numbers should have no unnecessary leading zeros
<bytes> 	-- any string of bytes, of the indicated length
<token>	   	:: <tokenchar>+ ;
<base-64>  	:: <decimal>? "|" ( <base-64-char> | <whitespace> )* "|" ;
<hexadecimal>   :: "#" ( <hex-digit> | <white-space> )* "#" ;
<quoted-string> :: <decimal>? <quoted-string-body>
<quoted-string-body> :: "\"" <bytes> "\""
<list>	   	:: "(" ( <sexp> | <whitespace> )* ")" ;
<whitespace> 	:: <whitespace-char>* ;
<token-char>  	:: <alpha> | <decimal-digit> | <simple-punc> ;
<alpha>	      	:: <upper-case> | <lower-case> | <digit> ;
<lower-case>  	:: "a" | ... | "z" ;
<upper-case>  	:: "A" | ... | "Z" ;
<decimal-digit> :: "0" | ... | "9" ;
<hex-digit>     :: <decimal-digit> | "A" | ... | "F" | "a" | ... | "f" ;
<simple-punc> 	:: "-" | "." | "/" | "_" | ":" | "*" | "+" | "=" ;
<whitespace-char> :: " " | "\t" | "\r" | "\n" ;
<base-64-char> 	:: <alpha> | <decimal-digit> | "+" | "/" | "=" ;
<null>	      	:: "" ;
"""

from pyparsing import *
from base64 import b64decode
import pprint
# sys.path.append('/home/andrei/replicated/research')

def verifyLen(t):
    t = t[0]
    if t.len is not None:
	t1len = len(t[1])
	if t1len != t.len:
	    raise ParseFatalException, \
		    "invalid data of length %d, expected %s" % (t1len, t.len)
    return t[1]

# define punctuation literals
LPAR, RPAR, LBRK, RBRK, LBRC, RBRC, VBAR, HASH = map(Suppress, "()[]{}|#")

decimal = Word("-0123456789",nums).setParseAction(lambda t: int(t[0]))
bytes = Word(printables)
raw = Group(decimal.setResultsName("len") + Suppress(":") + bytes).setParseAction(verifyLen)
token = Word(alphanums + "-./_:*+=")
base64_ = Group(Optional(decimal,default=None).setResultsName("len") + VBAR
    + OneOrMore(Word( alphanums +"+/=" )).setParseAction(lambda t: b64decode("".join(t)))
    + VBAR).setParseAction(verifyLen)

hexadecimal = ("#" + OneOrMore(Word(hexnums)) + "#")\
		.setParseAction(lambda t: int("".join(t[1:-1]),16))
qString = Group(Optional(decimal,default=None).setResultsName("len") +
			dblQuotedString.setParseAction(removeQuotes)).setParseAction(verifyLen)
simpleString = raw | token | base64_ | hexadecimal | qString

# extended definitions
real = Regex(r"[+-]?\d+\.\d*([eE][+-]?\d+)?").setParseAction(lambda tokens: float(tokens[0]))
token = Word(alphanums + "-./_:*+=!<>")

simpleString = real | decimal | raw | token | base64_ | hexadecimal | qString

display = LBRK + simpleString + RBRK
string_ = Optional(display) + simpleString

sexp = Forward()
sexpList = Optional(HASH) + Group(LPAR + ZeroOrMore(sexp) + RPAR)
sexp << ( string_ | sexpList )
