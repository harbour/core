/*
 * $Id$
 */

CA-T**ls Compatible Library for Harbour
=======================================

The goal of this library is to provide the functionality 
of the original CA-T**ls 3 Library for CA-Cl*pper.

Victor Szakats <info@szelvesz.hu>


Changes and Enhancements over the original CA-T**ls 3 Library
=============================================================

* ADDASCII()   New 4th parameter to enable a carry over in the addition
               process

* SETATLIKE()  2nd parameter can be passed by reference so that SETATLIKE
               can store the acutal wildcard character in it

+ CHARSUB      subtracts corresponding ASCII values

+ CHARSHL      bitwise shift left operation on characters

+ CHARSHR      bitwise shift right operation on characters

+ CHARRLL      bitwise roll to the left operation on characters

+ CHARRLR      bitwise roll to the right operation on characters

