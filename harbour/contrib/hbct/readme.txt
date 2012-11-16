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

* AddAscii()   New 4th parameter to enable a carry over in the addition
               process

* AtRepl()     New 6th parameter to specify characters to be ignored

* AtToken()    New 4th parameter to specify a skip width equal to the
               Token() function

+ CharHist()   generates a character histogram of a string

+ CharRll()    bitwise roll to the left operation on characters

+ CharRlr()    bitwise roll to the right operation on characters

+ CharShl()    bitwise shift left operation on characters

+ CharShr()    bitwise shift right operation on characters

+ CharSList()  generates a sorted character list of a string

+ CharSub()    subtracts corresponding ASCII values

+ Cosh()       hyperbolic cosine

+ CSetArgErr() set behaviour on argument errors

+ ctinit()     library init function

+ ctexit()     library exit function

+ DaysInMonth() Returns the number of days in month

+ DaysToMonth() Total number of days from first of Jan to beginning of nMonth.

* SetAtLike()  2nd parameter can be passed by reference so that SETATLIKE
               can store the acutal wildcard character in it

+ Sinh()       hyperbolic sine

* TabPack()    new 4th, 5th and 6th parameter to let one set the carriage-return-line-feed string,
* TabExpand()  the tab character and the Chr(141)[soft line feed] behaviour

+ Tanh()       hyperbolic tangent

* Token()      New 5th and 6th parameter where the function can store
               the tokenizer before and after the extracted token.

* TokenInit()  all incremental tokenizer functions
                 TokenInit(),TokenExit(),TokenNext(),TokenNum(),
                 TokenAt(),SaveToken(),RestToken(),TokenEnd()
               now support locally stored token environments

+ TokenExit()  new function related to TOKENINIT

+ TokenNum()   NumToken() functionality for incremental tokenizer

* TokenLower() New 4th parameter <nSkipWidth>

* TokenUpper() New 4th parameter <nSkipWidth>

* Week()       New 2nd parameter <lSWN>; Week() function can either return
               "simple week number" of week number defined by ISO 6801

+ WordRem()    remove double characters from a string
