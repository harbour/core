/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

/* Call all public functions. They should not GPF. */

PROCEDURE Main()

   LOCAL cFunc

   FOR EACH cFunc IN FuncList()
      OutStd( cFunc + hb_eol() )
      BEGIN SEQUENCE WITH {| e | Break( e ) }
         Do( cFunc )
      END SEQUENCE
   NEXT

   RETURN

STATIC FUNCTION FuncList()

   LOCAL aList := {}

   LOCAL nCount := __dynsCount()
   LOCAL cName
   LOCAL tmp

   LOCAL aExclude := { ;
      "__ERRINHANDLER", ;
      "ERRINHANDLER", ;
      "ERRINHAN", ;
      "__QUIT" }

   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF hb_IsFunction( cName ) .AND. AScan( aExclude, {| tmp | tmp == cName } ) == 0
         AAdd( aList, cName )
      ENDIF
   NEXT

   RETURN ASort( aList )
