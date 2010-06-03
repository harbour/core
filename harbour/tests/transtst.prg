/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    test code for TRANSFORM() function
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */


static s_stop := .f.

proc main()
test()
return

function transtest( xVal, cPict, cExpect )
local cResult := transform( xVal, cPict )
if !cResult == cExpect
   s_stop := .t.
endif
return iif( cResult == cExpect, "[OK]  ", "[ERROR] ["+cExpect+"] => ") + ;
       "[" + cResult + "]"

proc stop()
if s_stop
   ? repl("*",50)
   inkey(0)
   s_stop := .f.
endif
return

proc test()
local dt
? transtest( "abcd", "@9!*", "ABCD" )
? transtest( "abcd", "@_9!*", "ABCD" )
? transtest( "abcd", "@_9"+chr(9)+"9!9", "aBc" )
? transtest( "abcd", "@!!!", "ABCD" )
? transtest( "abcd", "@9", "abcd" )
?
? transtest( 134.24, "99,999.99", "   134.24" )
? transtest( 134.24, "@E 99,999.99", "   134,24" )
? transtest( -134.24,"@E 99,999.99", "  -134,24" )
? transtest( 134.24, "@E99,999.99", "       134,24" )
? transtest( -134.24, "@E99,999.99", "      -134,24" )
?
? transtest(-7, "@X 9999", "   7 DB")
? transtest(stod("19920509"), "@E", "09/05/92")
? transtest(val("3.10"),"@X", "3.10")
? transtest(0.80,".9999", ".8000" )
? transtest(-0.80, ".9999", ".****" )
? transtest(12345.123, "@X99", "     12345.123")
? transtest(-12345.123, "@X99", "     12345.123 DB" )
? transtest( 123456.78, "@E", "    123456,78")
? transtest(0,"@C 9.99", "0.00")
stop()
#ifdef __HARBOUR__
dt:=stod("19871231")
set date format to "MM:DD:YYYY"
? transtest( dt, "@E", "31:12:1987" )
set date format to "DD:MM:YYYY"
? transtest( dt, "@E", "31:12:1987")
set date format to "YYYY:MM:DD"
? transtest( dt, "@E", "31:12:1987")
set date format to "YYYY:DD:MM"
? transtest( dt, "@E", "31:12:1987")
set date format to "YY:MM:DD"
? transtest( dt, "@E", "31:12:87")
set date format to "MM:DD:YY"
? transtest( dt, "@E", "31:12:87")
set date format to "DD:MM:YY"
? transtest( dt, "@E", "31:12:87")
set date format to "<YY:DD.MM>"
? transtest( dt, "@E", "<31:12.87>")
set date format to "|YY|MM|DD|"
? transtest( dt, "@E", "|31|12|87|")
set date format to "MM<DD>YY"
? transtest( dt, "@E", "31<12>87")
stop()
#endif

? transtest( -5, "@(Z $###,##9.99",  "(      5.00)" )
? transtest( -10, "@)Z $###,##9.99", "$    (10.00)" )
? transtest( -20, "@Z $###,##9.99",  "$    -20.00" )
? transtest(100,"9999.", " 100.")
? transtest(1.1,"@B!99.99", "1.1         ")
? transtest(12.345,"@R 99/99", "  /12")
? transtest( "1234567890", "@9", "1234567890")
? transtest( 1234567890, "@9", " 1234567890")
? transtest( 1234, "9 999", "1 234" )
? transtest( 123.123456, "999.99.99.99", "123.12.45.  " )
? transtest( 123.123456, "$$$.$$.$$.$$", "123.12.45.  " )
? transtest( 123.123456, "***.**.**.**", "123.12.45.  " )
? transtest( 99999, "9.999", "*.***" )
? transtest(    99, "*.***", "*.***" )
? transtest( 12345, "9999.", "****." )
stop()
? transtest(-12345.00, "@(", "(    12345.00)")
? transtest(-12345.00, "@)", "    (12345.00)")
? transtest(-123456789.00, "@(", "(123456789.00)")
? transtest(-123456789.00, "@)", "(123456789.00)")
? transtest(-1234567890, "@(", "(         1234567890)")
? transtest(-1234567890, "@)", "         (1234567890)")
? transtest(-12345, "@( [999999]", "( 12345])")
? transtest(-12345, "@) [999999]", "[(12345])")
? transtest(-12345, "@( $999999", "( 12345)")
? transtest(-12345, "@) $999999", "$(12345)")
? transtest(-12345, "@( #999999", "( 12345)")
? transtest(-12345, "@) #999999", " (12345)")
? transtest(-12345, "@( $99999", "(12345)")
? transtest(-12345, "@) $99999", "(12345)")
? transtest(-12345, "@( #99999", "(12345)")
? transtest(-12345, "@) #99999", "(12345)")
? transtest(-12345, "@( 6798^999", "(7*8^***)")
? transtest(-12345, "@( 9798^9999", "(718^2345)")
stop()
?
tofix()
return

proc tofix()
? transtest( 134.24, "@E99,999.99", "       134,24" )
? transtest( -134.24, "@E99,999.99", "      -134,24" )
? transtest(0.80,".9999", ".8000")
? transtest(-0.80,".9999", ".****")
? transtest(12345.123, "@X99", "     12345.123")
? transtest(-12345.123, "@X99", "     12345.123 DB")
? transtest( 123456.78, "@E", "    123456,78")
? transtest(0,"@C 9.99", "0.00")
? transtest(1.1,"@B!99.99", "1.1         ")
? transtest(-12345, "@) [999999]", "[(12345])")
? transtest(-12345, "@) $999999", "$(12345)")
? transtest(-12345, "@) *999999", "*(12345)")
? transtest(-12345, "@) #999999", " (12345)")
? transtest(-12345, "@) *9$9*999]", "*($12345])")
? transtest(-12345, "@) *999*999]", "* (12345])")
? transtest(-12345, "@) 0999*999]", "0 (12345])")
? transtest(-12345, "@) 1999*999]", "1 (12345])")
? transtest(-12345, "@) *[99*999]", "([ 12345])")
? transtest(-12345, "@) *****999]", "(**12345])")
? transtest(-12345, "@) *1***999]", "(1*12345])")
? transtest(-12345, "@) * 999999]", "* (12345])")
? transtest( -5, "@(Z $###,##9.99",  "(      5.00)" )
? transtest( -10, "@)Z $###,##9.99", "$    (10.00)" )
? transtest( -5, "@(Z $999,999.99",  "(      5.00)" )
? transtest( -10, "@)Z $999,999.99", "$    (10.00)" )
? transtest( -5, "@(Z 999,999.99",   "(     5.00)" )
? transtest( -10, "@)Z 999,999.99",  "    (10.00)" )
? transtest( -20, "@Z $###,##9.99",  "$    -20.00" )
? transtest(0.1,".9", ".1")
? transtest(0.0,".9", ".0")
? transtest(1,".9", ".*")
? transtest(.456,".9", ".5")
? transtest(123,"99.-", "**.-")
stop()
? transtest(-123.45,"999,999.99", "   -123.45")
? transtest(-123456.78,"999,999,999.99", "   -123,456.78")
? transtest(-123456.78,"$$$,$$$,$$$.$$", "$$ -123,456.78")
? transtest(-123456.78,"***,***,***.**", "***-123,456.78")
? transtest(123456.78,"@E 888,$$$,$$$.$$", "888.123.456,78")
? transtest(123456.78,"@E 888x,$$$,$$$.$$", "888xx123.456,78")
? transtest(123456.78,"@E 888x,,$$$,$$$.$$", "888xxx123.456,78")
? transtest(123456.78,"@E 8,88x,,$$$,$$$.$$", "8.88xxx123.456,78")
? transtest(123456.78,"@E 8,88x,,$$$,,$$$.$$", "8.88xxx123..456,78")
? transtest(123456.78,"@E 8,88x,,$$$,,$$$.$$77,7", "8.88xxx123..456,7877,7")
? transtest(123456,"@E 8,88x,,$$$,,$$$77,7", "8.88xxx123..45677,7")
? transtest(123456,"@E -,999,999", " -123.456")
? transtest(12345,"@E -,999,999", " - 12.345")
? transtest(12345,"@E -,|999,999", " -| 12.345")
? transtest(12345,"@E ^-,|999,999", "^^-| 12.345")
? transtest(12345,"@E 1-,|999,999", "11-| 12.345")
? transtest(12345,"@E |--,|999,999", "|---| 12.345")
stop()
? transtest(12.34,"@E 99'99", "  '12")
? transtest(12.34,"99,99,11", "   12,11")
? transtest(12.34,"@E 99,99,11", "   12,11")
? transtest(12.34,"@E 99,", "12,")
? transtest(12.34,"@E 9,9", "1.2")
? transtest(12.34,"@E ab,cd.ef9,9.99,.--", "abbcd,ef***,* ,.--")
? transtest(12.34,"@E ab,cd,ef9,9.99,.--", "abbcddef1.2,34,.--")
? transtest(12.34,"@E ,ab,cd,ef9,9.99,.--", ",abbcddef1.2,34,.--")
? transtest(12.34,"@E ,,,,99,.99,.--", ",,,,12.,34,.--")
? transtest(124.4,"@E ,,,,9,9.99,.--", ",,,,***,**,.--")
? transtest(  1.2,"@E ,,,,*,*.**,.--", ",,,,**1,20,.--")
? transtest(12.34,"@E ,,,,*,*.**,.--", ",,,,1.2,34,.--")
? transtest(12.34,"@E ,,,,*,*.**,.--,--", ",,,,1.2,34,.--,--")
? transtest(12.34,"@E ,,,,*,*,.,**", ",,,,1.2.,,34")
? transtest(12.34,",,,,*,*,.,**", ",,,,1,2,..34")
? transtest(12.34,",,,,*,*,.,*|,*", ",,,,1,2,..3||4")
? transtest(12.34,",,,,*,*,.,*,*", ",,,,1,2,..3,4")
? transtest(123.345678912,"@E 999.99.99,99.99.", "123,34,67.89,  .")
#ifdef __HARBOUR__
? transtest(  1234567890123456789, "99999999999999999999", " 1234567890123456789" )
? transtest( -1234567890123456789, "99999999999999999999", "-1234567890123456789" )
#else
? transtest(  1234567890123456789, "99999999999999999999", " 1234567890123457000" )
? transtest( -1234567890123456789, "99999999999999999999", "-1234567890123457000" )
#endif
stop()
set(_SET_DATEFORMAT,"YYYY/MM/DD")
? transtest(12345678,"@D", "1234/56/78")
set(_SET_DATEFORMAT,"YYYY.MM.DD")
? transtest(1234.56789,"@D", "1234.56.9 ")
set(_SET_DATEFORMAT,"YYYY.MM:DD")
? transtest(1234.56789,"@D", "1234.56:79")
? transtest(123.345678912,"@D ", " 123.34:57")
set(_SET_DATEFORMAT,"MM-DD-YYYY")
? transtest(.t.,"@RE <|,yY#lL,|>", "99-99-9999T")
? transtest(.f.,"@RE <|,yY#lL,|>", "99-99-9999F")
? transtest(.t.,"@RD <|,yY#lL,|>", "99-99-9999T")
? transtest(.f.,"@RD <|,yY#lL,|>", "99-99-9999F")
? transtest(.f.,"@DE <|,yY#lL,|>", "9")
? transtest("abcdefghij","@S15! <XXXXXXXX>", "<BCDEFGHI>")
? transtest("abcdefghij","@S0! <XXXXXXXX>", "<BCDEFGHI>")
? transtest("abcdefghij","@S5! <XXXXXXXX>", "<BCDE")
stop()
set fixed on
? transtest( 1234,         ,      "            1234" )
? transtest( 1234,     ""  ,      "            1234" )
? transtest( 1234,     "@" ,      "            1234" )
? transtest( 1234,     "@!",      "            1234" )
? transtest( -1234,        ,      "           -1234" )
? transtest( -1234,    "@" ,      "           -1234" )
#ifdef HB_CLP_STRICT
? transtest( round(123,0),      , "       123.00" )
? transtest( round(123,0), "@!",  "       123.00" )
? transtest( round(123.0,0),    , "       123.00" )
? transtest( round(123.0,0),"@!", "       123.00" )
#endif
stop()
? transtest( 1234.567,     ,      "      1234.57" )
? transtest( 1234.567,   "",      "      1234.57" )
? transtest( 1234.567, "@" ,      "      1234.57" )
? transtest( 1234.567, "@!",      "      1234.57" )
? transtest( -1234.567,    ,      "     -1234.57" )
? transtest( -1234.567, "@",      "     -1234.57" )
? transtest( val("-1.0"),  ,      "-1.00" )
? transtest( val("-1.0"), "@",    "-1.00" )
? transtest( val("-123"),  ,      "      -123" )
? transtest( val("-123"), "@",    "      -123" )
? transtest( 0,            ,      "               0" )
? transtest( 0.0,          ,      "         0.00" )
? transtest( val("1"),     ,      "      1" )
? transtest( val("12"),    ,      "      12" )
? transtest( val("123"),   ,      "      123" )
? transtest( val("1234"),  ,      "      1234" )
stop()
set decimal to 3
? transtest( 0.0,          ,      "         0.000" )
? transtest( val("1"),     ,      "        1" )
? transtest( val("12"),    ,      "        12" )
? transtest( val("123"),   ,      "        123" )
? transtest( val("1234"),  ,      "        1234" )
set decimal to 4
? transtest( 0.0,          ,      "         0.0000" )
? transtest( val("1"),     ,      "          1" )
? transtest( val("12"),    ,      "          12" )
? transtest( val("123"),   ,      "          123" )
? transtest( val("1234"),  ,      "          1234" )
set fixed off
stop()
? transtest( -1234, ,          "     -1234" )
? transtest( -1234, "@B",      "-1234     " )
? transtest( -1234, "@(",      "(     1234)" )
? transtest( -1234, "@)",      "     (1234)" )
? transtest( -1234, "@B)",     "(1234)     " )
? transtest( -1234, "@B(",     "(1234)     " )
? transtest( "(  12)", "@B(",  "(  12)" )
? transtest( "(  12)", "@B)",  "(  12)" )
? transtest( "   12", "@B(",   "12   " )
? transtest( "   12", "@B)",   "12   " )
#ifdef __HARBOUR__
? transtest( 1234, "@L",       "0000001234" )
? transtest( 1234, "@0",       "0000001234" )
? transtest( 1234, "@L(",      "0000001234" )
? transtest( 1234, "@0)",      "0000001234" )
? transtest( -1234, "@L(",     "(000001234)" )
? transtest( -1234, "@0)",     "(000001234)" )
/* please test it with FoxPro and Xbase++ to check if they give the same result */
? transtest( -1234, "@L",      "-000001234" )
? transtest( -1234, "@0",      "-000001234" )
#endif
/* FlagShip extensions */
? transtest( -1234, "@Z",      "     -1234" )
? transtest( 1234, "@Z",       "      1234" )
? transtest( -1234, "@F",      "     -1234" )
? transtest( 1234, "@F",       "      1234" )
? transtest( -1234, "@T",      "     -1234" )
? transtest( 1234, "@T",       "      1234" )

? transtest( 123456789.12, "@,39 999,999,999.99",  "123,456,789.12" )
? transtest( 123456789.12, "@,39 999,999,999.99",  "123,456,789.12" )
? transtest( 123.456, "@R 9 9 9.9", "1 2 3.5" )
stop()
return

#ifndef __HARBOUR__
func stod(s)
local cDf:=set(_SET_DATEFORMAT,"YYYY/MM/DD"), dt
dt:=ctod(stuff(stuff(s,7,0,"/"),5,0,"/"))
set(_SET_DATEFORMAT,cDf)
return dt
#endif
