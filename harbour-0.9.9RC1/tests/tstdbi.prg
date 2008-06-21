/*
 * $Id$
 */

#ifdef __HARBOUR__
   #define CRLF  hb_osnewline()
#else
   #define CRLF  chr(13)+chr(10)
#endif

funct main()

  local i
  local cStr := ""

  USE "test" NEW

  for i := 1 to 100
    cStr += Str( i ) + " " + xToStr( DbInfo( i ) ) + CRLF
  next
  cStr += Str(  101 ) + " " + xToStr( DbInfo(  101 ) ) + CRLF
  cStr += Str(  101 ) + " " + xToStr( DbInfo(  101, 1 ) ) + CRLF
  cStr += Str(  101 ) + " " + xToStr( DbInfo(  101, 2 ) ) + CRLF
  cStr += Str(  102 ) + " " + xToStr( DbInfo(  102 ) ) + CRLF
  cStr += Str(  101 ) + " " + xToStr( DbInfo(  102, 1 ) ) + CRLF
  cStr += Str(  101 ) + " " + xToStr( DbInfo(  102, 2 ) ) + CRLF
  cStr += Str(  999 ) + " " + xToStr( DbInfo(  999 ) ) + CRLF
  cStr += Str( 1000 ) + " " + xToStr( DbInfo( 1000 ) ) + CRLF

#ifdef __HARBOUR__
  MemoWrit( "dbihb.txt", cStr )
#else
  MemoWrit( "dbicl.txt", cStr )
#endif

  ? DbRecordInfo( 1 )
  ? DbRecordInfo( 2 )
  ? DbRecordInfo( 3 )
  ? DbRecordInfo( 4 )
  ? DbRecordInfo( 5 )

  ? DbFieldInfo( 1, 1 )
  ? DbFieldInfo( 2, 1 )
  ? DbFieldInfo( 3, 1 )
  ? DbFieldInfo( 4, 1 )

return nil

static function xToStr( xValue )

  LOCAL cType := ValType( xValue )
  LOCAL cRet  := ""

  do case
    case cType == "N"
      cRet  := Str( xValue )
    case cType == "D"
      cRet  := DToC( xValue )
    case cType == "C" .or. cType == "M"
      cRet := xValue
    case cType == "L"
      cRet := if( xValue, ".T.", ".F." )
    case cType == "A"
      cRet := "A" + AllTrim( Str( Len( xValue ) ) )
    case cType == "U"
      cRet := "NIL"
  endcase

return cRet
