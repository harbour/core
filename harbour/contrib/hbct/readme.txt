/*
 * $Id$
 */

CA-T**ls Compatible Library for Harbour
=======================================

The goal of this library is to provide the functionality
of the original CA-T**ls 3 Library for CA-Cl*pper.

Viktor Szakats (harbour syenar.net)


Directories: ./     contains all the source files, include files, make files
                    and the general text files (like tthe one you are just
                    reading !),
             tests  contains some test programs for the functions implemented
                    in the CT3 library,
             alt    contains alternative function implementations (not
                    all are complete, be aware !), mostly in
                    Harbour, if the function is implemented in C and vice
                    versa.

Changes and Enhancements over the original CA-T**ls 3 Library
Martin Vogel <vogel@inttec.de>
=============================================================

* ADDASCII()   New 4th parameter to enable a carry over in the addition
               process

* ATREPL()     New 6th parameter to specify characters to be ignored

* ATTOKEN()    New 4th parameter to specify a skip width equal to the
               TOKEN() function

+ CHARHIST()   generates a character histogram of a string

+ CHARRLL()    bitwise roll to the left operation on characters

+ CHARRLR()    bitwise roll to the right operation on characters

+ CHARSHL()    bitwise shift left operation on characters

+ CHARSHR()    bitwise shift right operation on characters

+ CHARSLIST()  generates a sorted character list of a string

+ CHARSUB()    subtracts corresponding ASCII values

+ COSH()       hyperbolic cosine

+ CSETARGERR() set behaviour on argument errors

+ CTINIT()     library init function

+ CTEXIT()     library exit function

+ DAYSINMONTH() Returns the number of days in month

+ DAYSTOMONTH() Total number of days from first of Jan to beginning of nMonth.

+ NUMANDX()    \
+ NUMORX()     |
+ NUMXORX()    |- equivalents to NUMAND(),... functions, but working
+ NUMNOTX()    |  with any bit number of 1 to 32
+ NUMROLX()    |
+ NUMMIRRX()   /

* SETATLIKE()  2nd parameter can be passed by reference so that SETATLIKE
               can store the acutal wildcard character in it

+ SINH()       hyperbolic sine

* TABPACK()    new 4th, 5th and 6th parameter to let one set the carriage-return-line-feed string,
* TABEXPAND()  the tab character and the chr(141)[soft line feed] behaviour

+ TANH()       hyperbolic tangent

* TOKEN()      New 5th and 6th parameter where the function can store
               the tokenizer before and after the extracted token.

* TOKENINIT()  all incremental tokenizer functions
                 TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),
                 TOKENAT(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
               now support locally stored token environments

+ TOKENEXIT()  new function related to TOKENINIT

+ TOKENNUM()   numtoken() functionality for incremental tokenizer

* TOKENLOWER() New 4th parameter <nSkipWidth>

* TOKENUPPER() New 4th parameter <nSkipWidth>

* WEEK()       New 2nd parameter <lSWN>; week() function can either return
               "simple week number" of week number defined by ISO 6801

+ WORDREM()    remove double characters from a string
