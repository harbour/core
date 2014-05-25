/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

/* Call all public functions. They should not GPF. */

REQUEST __HB_EXTERN__

PROCEDURE Main()

   LOCAL cFunc

   FOR EACH cFunc IN FuncList()
      OutStd( cFunc:__enumIndex(), Len( cFunc:__enumBase() ), cFunc + hb_eol() )
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
      "Main", ;
      "__errInHandler", ;
      "ErrorInHandler", ;
      "ErrorInHan", ;
      "__Quit", ;
      "_WSTACK", "CTWINIT", "CTWLASTKEY", ;
      "hb_trace", ;
      "MemoEdit", ;
      "__Accept", ;
      "__Input", ;
      "__Wait" }

   FOR tmp := 1 TO nCount
      IF hb_IsFunction( cName := __dynsGetName( tmp ) ) .AND. ;
         AScan( aExclude, {| tmp | hb_LeftEqI( cName, tmp ) } ) == 0
         AAdd( aList, cName )
      ENDIF
   NEXT

   RETURN ASort( aList )
