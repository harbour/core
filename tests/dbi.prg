#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#include "dbinfo.ch"
#include "dbstruct.ch"

PROCEDURE Main()

   LOCAL i
   LOCAL cStr

   USE test.dbf READONLY

   cStr := ""
   FOR i := 1 TO 100
      cStr += Str( i ) + " " + XToStr( dbInfo( i ) ) + hb_eol()
   NEXT
   cStr += ;
      Str( DBI_DB_VERSION ) + " " + XToStr( dbInfo( DBI_DB_VERSION ) ) + hb_eol() + ;
      Str( DBI_DB_VERSION ) + " " + XToStr( dbInfo( DBI_DB_VERSION, 1 ) ) + hb_eol() + ;
      Str( DBI_DB_VERSION ) + " " + XToStr( dbInfo( DBI_DB_VERSION, 2 ) ) + hb_eol() + ;
      Str( DBI_RDD_VERSION ) + " " + XToStr( dbInfo( DBI_RDD_VERSION ) ) + hb_eol() + ;
      Str( DBI_RDD_VERSION ) + " " + XToStr( dbInfo( DBI_RDD_VERSION, 1 ) ) + hb_eol() + ;
      Str( DBI_RDD_VERSION ) + " " + XToStr( dbInfo( DBI_RDD_VERSION, 2 ) ) + hb_eol() + ;
      Str( 999 ) + " " + XToStr( dbInfo(  999 ) ) + hb_eol() + ;
      Str( 1000 ) + " " + XToStr( dbInfo( 1000 ) ) + hb_eol()

#ifdef __HARBOUR__
   MemoWrit( "dbi_hb.txt", cStr )
#else
   MemoWrit( "dbi_cl.txt", cStr )
#endif

   ? dbRecordInfo( DBRI_DELETED )
   ? dbRecordInfo( DBRI_LOCKED )
   ? dbRecordInfo( DBRI_RECSIZE )
   ? dbRecordInfo( DBRI_RECNO )
   ? dbRecordInfo( DBRI_UPDATED )

   ? dbFieldInfo( DBS_NAME, 1 )
   ? dbFieldInfo( DBS_TYPE, 1 )
   ? dbFieldInfo( DBS_LEN, 1 )
   ? dbFieldInfo( DBS_DEC, 1 )

   RETURN

STATIC FUNCTION XToStr( xValue )

   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C" .OR. ;
        cType == "M" ; RETURN xValue
   CASE cType == "N" ; RETURN hb_ntos( xValue )
   CASE cType == "D" ; RETURN DToC( xValue )
#ifdef __HARBOUR__
   CASE cType == "T" ; RETURN hb_TToC( xValue )
#endif
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "A" ; RETURN "{.}"
   CASE cType == "B" ; RETURN "{|| }"
   CASE cType == "O" ; RETURN "[O]"
   ENDCASE

   RETURN xValue
