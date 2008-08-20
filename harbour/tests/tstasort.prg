//
// $Id$
//

function main()

  local oError := ErrorNew()

  local a := { 3, 2, 1 }
  local b := { 10 }
  local c := { 2, .T., "B", NIL, { 1 }, {|| b }, oError, Date(), 1, .F., "A", NIL, Date() - 1, { 0 }, {|| a }, oError }
  local t

  ?
  ?
  ? "Original.....:", aDump( t := a )
  ? "Asort.c......:", aDump( aSort( t := aClone( a ) ) )
  ? "Asort.c.block:", aDump( aSort( t := aClone( a ), , , {| x, y | x < y } ) )
  ?
  ? "Original.....:", aDump( t := b )
  ? "Asort.c......:", aDump( aSort( t := aClone( b ) ) )
  ? "Asort.c.block:", aDump( aSort( t := aClone( b ), , , {| x, y | x < y } ) )
  ?
  ? "Original.....:", aDump( t := c )
  ? "Asort.c......:", aDump( aSort( t := aClone( c ) ) )
  ? "Asort.c.block:", aDump( aSort( t := aClone( c ), , , {| x, y | xToStr(x) < xToStr(y) } ) )

return nil

function aDump( a )

  local cStr := ""
  local n := len( a )
  local i

  for i := 1 to n
    cStr += alltrim( xToStr( a[i] ) ) + " "
  next

return cStr

function xToStr( xValue )

  LOCAL cType := ValType( xValue )

  do case
    case cType == "C" .or. cType == "M"
      return xValue
    case cType == "N"
      return AllTrim( Str( xValue ) )
    case cType == "D"
      return DToC( xValue )
    case cType == "L"
      return iif( xValue, ".T.", ".F." )
    case cType == "U"
      return "NIL"
    case cType == "A"
      return "{.}"
    case cType == "B"
      return "{|| }"
    case cType == "O"
      return "[O]"
  endcase

return xValue
