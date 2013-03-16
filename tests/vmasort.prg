
#ifndef __HARBOUR__
   #define hb_ntos( n ) LTrim( Str( n ) )
#endif

PROCEDURE Main( nPass )

   LOCAL aTest
   LOCAL aOrig

   IF nPass == NIL
      nPass := 1
   ELSE
      nPass := Val( nPass )
   ENDIF

   ? "Testing ASort() with " + hb_ntos( nPass ) + " loops."
   ?
   aTest := aMkArray( nPass )
   aOrig := AClone( aTest )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "Original.....:", aDump( aOrig )
   ? "asort.c......:", aDump( ASort( aTest ) )
// ? "asort.c.block:", aDump( ASort( aTest,,, {| x, y | x < y } ) )

   RETURN

STATIC FUNCTION aMkArray( nPass )

   LOCAL aData := {}
   LOCAL n
   LOCAL nMult := 200
   LOCAL nMid  := ( nMult / 2 ) + 1
   LOCAL nMax  := nPass * nMult

   FOR n := 1 TO nMax
      AAdd( aData, NIL )
      AAdd( aData, nMid - n )
      AAdd( aData, Date() - n )
      AAdd( aData, iif( n % 2 == 0, .F., .T. ) )
      AAdd( aData, Replicate( Chr( 64 + ( n % 256 ) ), nPass ) )
      AAdd( aData, {|| n } )
      AAdd( aData, Array( n ) )
      AAdd( aData, ErrorNew() )
   NEXT

   RETURN aData

STATIC FUNCTION aDump( a )

   LOCAL cStr := ""
   LOCAL n := Len( a )
   LOCAL i

   FOR i := 1 TO n
      cStr += AllTrim( xToStr( a[ i ] ) ) + " "
   NEXT

   RETURN cStr

STATIC FUNCTION xToStr( xValue )

   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C" .OR. cType == "M"
      RETURN xValue
   CASE cType == "N"
      RETURN hb_ntos( xValue )
   CASE cType == "D"
      RETURN DToC( xValue )
   CASE cType == "L"
      RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "U"
      RETURN "NIL"
   CASE cType == "A"
      RETURN "{.}"
   CASE cType == "B"
      RETURN "{|| }"
   CASE cType == "O"
      RETURN "[O]"
   ENDCASE

   RETURN xValue
