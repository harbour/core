//
// $Id$
//

function Main( nPass )

  LOCAL aTest
  LOCAL aOrig

  if nPass == NIL
    nPass := 1
  else
    nPass := Val( nPass )
  endif

  ? "Testing aSort with " + Str( nPass ) + " loops."
  ?
  aTest := aMkArray( nPass )
  aOrig := aClone( aTest )

  set( _SET_DATEFORMAT, "mm/dd/yyyy" )

  ? "Original.....:", aDump( aOrig )
  ? "Asort.c......:", aDump( aSort( aTest ) )
//  ? "Asort.c.block:", aDump( aSort( aTest, , , {| x, y | x < y } ) )

return nil

static function aMkArray( nPass )

  LOCAL aData := {}
  LOCAL n
  LOCAL nMult := 200
  LOCAL nMid  := ( nMult / 2 ) + 1
  LOCAL nMax  := nPass * nMult

  for n := 1 to nMax
    aAdd( aData, NIL )
    aAdd( aData, nMid - n )
    aAdd( aData, Date() - n )
    aAdd( aData, if( n % 2 == 0, .f., .t. ) )
    aAdd( aData, Replicate( Chr( 64 + ( n % 256 ) ) , nPass ) )
    aAdd( aData, {|| n } )
    aAdd( aData, Array( n ) )
    aAdd( aData, ErrorNew() )
  next

return aData

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
      return if( xValue, ".T.", ".F." )
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
