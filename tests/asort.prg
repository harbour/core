#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL oError := ErrorNew()

   LOCAL a := { 3, 2, 1 }
   LOCAL b := { 10 }
   LOCAL c := { 2, .T., "B", NIL, { 1 }, {|| b }, oError, Date(), 1, .F., "A", NIL, Date() - 1, { 0 }, {|| a }, oError }

   ? "Original...:", ADump( a )
   ? "ASort......:", ADump( ASort( AClone( a ) ) )
   ? "ASort.block:", ADump( ASort( AClone( a ), , , {| x, y | x < y } ) )
   ?
   ? "Original...:", ADump( b )
   ? "ASort......:", ADump( ASort( AClone( b ) ) )
   ? "ASort.block:", ADump( ASort( AClone( b ), , , {| x, y | x < y } ) )
   ?
   ? "Original...:", ADump( c )
   ? "ASort......:", ADump( ASort( AClone( c ) ) )
   ? "ASort.block:", ADump( ASort( AClone( c ), , , {| x, y | XToStr( x ) < XToStr( y ) } ) )
   ?

   RETURN

STATIC FUNCTION ADump( a )

   LOCAL cStr := ""
   LOCAL n := Len( a )
   LOCAL i

   FOR i := 1 TO n
      cStr += AllTrim( XToStr( a[ i ] ) ) + " "
   NEXT

   RETURN cStr

STATIC FUNCTION XToStr( xValue )

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
