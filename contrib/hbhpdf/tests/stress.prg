/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbhpdf"

PROCEDURE Main()

   LOCAL cFunc

   /* Should not GPF */
   FOR EACH cFunc IN FuncList( "HPDF_" )
      ? cFunc
      Do( cFunc )
   NEXT

   RETURN

STATIC FUNCTION FuncList( cPrefix )

   LOCAL aList := {}

   LOCAL nCount := __dynsCount()
   LOCAL cName
   LOCAL tmp

   cPrefix := hb_asciiUpper( cPrefix )

   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF hb_LeftEq( cName, cPrefix )
         AAdd( aList, cName )
      ENDIF
   NEXT

   RETURN ASort( aList )
