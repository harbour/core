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

   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF Left( cName, Len( cPrefix ) ) == hb_asciiUpper( cPrefix )
         AAdd( aList, cName )
      ENDIF
   NEXT

   ASort( aList )

   RETURN aList
