#ifndef __HARBOUR__
   #xtranslate hb_eol() => ( Chr( 13 ) + Chr( 10 ) )
#endif

PROCEDURE Main()

   LOCAL i
   LOCAL cStr

   USE test NEW

   cStr := ""
   FOR i := 1 TO 100
      cStr += Str( i ) + " " + XToStr( dbInfo( i ) ) + hb_eol()
   NEXT
   cStr += Str(  101 ) + " " + XToStr( dbInfo(  101 ) ) + hb_eol()
   cStr += Str(  101 ) + " " + XToStr( dbInfo(  101, 1 ) ) + hb_eol()
   cStr += Str(  101 ) + " " + XToStr( dbInfo(  101, 2 ) ) + hb_eol()
   cStr += Str(  102 ) + " " + XToStr( dbInfo(  102 ) ) + hb_eol()
   cStr += Str(  101 ) + " " + XToStr( dbInfo(  102, 1 ) ) + hb_eol()
   cStr += Str(  101 ) + " " + XToStr( dbInfo(  102, 2 ) ) + hb_eol()
   cStr += Str(  999 ) + " " + XToStr( dbInfo(  999 ) ) + hb_eol()
   cStr += Str( 1000 ) + " " + XToStr( dbInfo( 1000 ) ) + hb_eol()

#ifdef __HARBOUR__
   MemoWrit( "dbi_hb.txt", cStr )
#else
   MemoWrit( "dbi_cl.txt", cStr )
#endif

   ? dbRecordInfo( 1 )
   ? dbRecordInfo( 2 )
   ? dbRecordInfo( 3 )
   ? dbRecordInfo( 4 )
   ? dbRecordInfo( 5 )

   ? dbFieldInfo( 1, 1 )
   ? dbFieldInfo( 2, 1 )
   ? dbFieldInfo( 3, 1 )
   ? dbFieldInfo( 4, 1 )

   RETURN

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
