// Implementation of operator overload in Harbour
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oString := MyTString():New( "Hello" )

   ? "Testing MyTString() operator overloading"
   ? ValType( oString )
   ? "---"
   ? "Before:", oString:cValue
   ?
   ? "Equal                   :", oString =  "Hello"
   ? "Exactly Equal           :", oString == "Hello"
   ? "Not Equal !=            :", oString != "Hello"
   ? "Not Equal <>            :", oString <> "Hello"
   ? "Not Equal #             :", oString #  "Hello"
   ? "Substring $             :", oString $  "Hello"
   ? "Less than               :", oString <  "Hello"
   ? "Less than or Equal      :", oString <= "Hello"
   ? "Greater than            :", oString >  "Hello"
   ? "Greater than or Equal   :", oString >= "Hello"
   ? "Concatenation +         :", oString +  "Hello"
   ? "Concatenation -         :", oString -  "Hello"
   ? "Array index[ 2 ]        :", oString[ 2 ]
   ? "Array index[ 3 ] := 'X' :", oString[ 3 ] := 'X'
   ?
   ? "After:", oString:cValue

   RETURN

CREATE CLASS MyTString

   VAR cValue

   METHOD New( cText ) INLINE ::cValue := cText, self

   OPERATOR "="  ARG cArg INLINE ::cValue =  cArg
   OPERATOR "==" ARG cArg INLINE ::cValue == cArg
   OPERATOR "!=" ARG cArg INLINE ::cValue != cArg
   OPERATOR "<"  ARG cArg INLINE ::cValue <  cArg
   OPERATOR "<=" ARG cArg INLINE ::cValue <= cArg
   OPERATOR ">"  ARG cArg INLINE ::cValue >  cArg
   OPERATOR ">=" ARG cArg INLINE ::cValue >= cArg
   OPERATOR "+"  ARG cArg INLINE ::cValue +  cArg
   OPERATOR "-"  ARG cArg INLINE ::cValue -  cArg
   OPERATOR "$"  ARG cArg INLINE ::cValue $  cArg
   OPERATOR "[]" ARG nIndex INLINE iif( PCount() > 2, ;
      ::cValue := Stuff( ::cValue, nIndex, 1, hb_PValue( 3 ) ), ;
      SubStr( ::cValue, nIndex, 1 ) )

ENDCLASS
