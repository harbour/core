/*
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "postgres.ch"

#define _STRU_FIELDNAME     1
#define _STRU_FIELDTYPE     2
#define _STRU_FIELDLEN      3
#define _STRU_FIELDDEC      4
#define _STRU_TABLE         5
#define _STRU_TABLECOL      6

CREATE CLASS TPQServer

   VAR pDb
   VAR lTrans
   VAR lAllCols  INIT .T.
   VAR Schema    INIT "public"
   VAR lError    INIT .F.
   VAR cError    INIT ""
   VAR lTrace    INIT .F.
   VAR pTrace

   METHOD New( cHost, cDatabase, cUser, cPass, nPort, cSchema, hCustom )
   METHOD Destroy()
   METHOD Close()              INLINE ::Destroy()

   METHOD StartTransaction()
   METHOD TransactionStatus()  INLINE PQtransactionStatus( ::pDb )
   METHOD Commit()
   METHOD Rollback()

   METHOD Query( cQuery )
   METHOD Execute( cQuery )    INLINE ::Query( cQuery )
   METHOD SetSchema( cSchema )

   METHOD NetErr()             INLINE ::lError
   METHOD ErrorMsg()           INLINE ::cError

   METHOD TableExists( cTable )
   METHOD ListTables()
   METHOD TableStruct( cTable )
   METHOD CreateTable( cTable, aStruct )
   METHOD DeleteTable( cTable  )
   METHOD TraceOn( cFile )
   METHOD TraceOff()
   METHOD SetVerbosity( num )  INLINE PQsetErrorVerbosity( ::pDb, iif( num >= 0 .AND. num <= 2, num, 1 )  )

ENDCLASS

METHOD New( cHost, cDatabase, cUser, cPass, nPort, cSchema, hCustom ) CLASS TPQserver

   LOCAL res
   LOCAL item

   LOCAL cConnect := ;
      iif( HB_ISSTRING( cDatabase ), " dbname = " + EscapeParam( cDatabase ), "" ) + ;
      iif( HB_ISSTRING( cHost ), " host = " + EscapeParam( cHost ), "" ) + ;
      iif( HB_ISSTRING( cUser ), " user = " + EscapeParam( cUser ), "" ) + ;
      iif( HB_ISSTRING( cPass ), " password = " + EscapeParam( cPass ), "" ) + ;
      iif( HB_ISNUMERIC( nPort ), " port = " + hb_ntos( nPort ), "" )

   IF HB_ISHASH( hCustom )
      FOR EACH item IN hCustom
         cConnect += " " + item:__enumKey() + " = " + EscapeParam( item )
      NEXT
   ENDIF

   ::pDB := PQconnectdb( cConnect )

   IF PQstatus( ::pDb ) != CONNECTION_OK
      ::lError := .T.
      ::cError := PQerrorMessage( ::pDb )
   ELSE
      IF HB_ISSTRING( cSchema )
         ::SetSchema( cSchema )
      ELSE
         res := PQexec( ::pDB, "SELECT current_schema()" )
         IF PQresultStatus( res ) == PGRES_TUPLES_OK
            ::Schema := PQgetvalue( res, 1, 1 )
         ENDIF
      ENDIF
   ENDIF

   RETURN self

METHOD Destroy() CLASS TPQserver

   ::TraceOff()
   ::pDb := NIL

   RETURN NIL

METHOD SetSchema( cSchema ) CLASS TPQserver

   LOCAL res
   LOCAL result

   IF PQstatus( ::pDb ) == CONNECTION_OK
      ::Schema := cSchema
      res := PQexec( ::pDB, "SET search_path TO " + cSchema )
      result := ( PQresultStatus( res ) == PGRES_COMMAND_OK )
   ELSE
      result := .F.
   ENDIF

   RETURN result

METHOD StartTransaction() CLASS TPQserver

   LOCAL res := PQexec( ::pDB, "BEGIN" )

   IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""
   ENDIF

   RETURN ::lError

METHOD Commit() CLASS TPQserver

   LOCAL res := PQexec( ::pDB, "COMMIT" )

   IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""
   ENDIF

   RETURN ::lError

METHOD Rollback() CLASS TPQserver

   LOCAL res := PQexec( ::pDB, "ROLLBACK" )

   IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""
   ENDIF

   RETURN ::lError

METHOD Query( cQuery ) CLASS TPQserver
   RETURN TPQQuery():New( ::pDB, cQuery, ::lAllCols, ::Schema )

METHOD TableExists( cTable ) CLASS TPQserver

   LOCAL result

   LOCAL res := PQexec( ::pDB, ;
      "SELECT table_name" + ;
      "  FROM information_schema.tables" + ;
      " WHERE table_type = 'BASE TABLE' AND table_schema = " + DataToSql( ::Schema ) + " AND table_name = " + DataToSql( Lower( cTable ) ) )

   IF ( ::lError := PQresultStatus( res ) != PGRES_TUPLES_OK )
      ::cError := PQresultErrorMessage( res )
      result := .F.
   ELSE
      ::cError := ""
      result := ( PQlastrec( res ) != 0 )
   ENDIF

   RETURN result

METHOD ListTables() CLASS TPQserver

   LOCAL result := {}
   LOCAL i

   LOCAL res := PQexec( ::pDB, ;
      "SELECT table_name" + ;
      "  FROM information_schema.tables" + ;
      " WHERE table_schema = " + DataToSql( ::Schema ) + " AND table_type = 'BASE TABLE'" )

   IF ( ::lError := PQresultStatus( res ) != PGRES_TUPLES_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      FOR i := 1 TO PQlastrec( res )
         AAdd( result, PQgetvalue( res, i, 1 ) )
      NEXT
      ::cError := ""
   ENDIF

   RETURN result

METHOD TableStruct( cTable ) CLASS TPQserver

   LOCAL result := {}
   LOCAL i
   LOCAL cField
   LOCAL cType
   LOCAL nSize
   LOCAL nDec

   LOCAL res := PQexec( ::pDB, ;
      "SELECT column_name, data_type, character_maximum_length, numeric_precision, numeric_scale" + ;
      "  FROM information_schema.columns" + ;
      " WHERE table_schema = " + DataToSql( ::Schema ) + " AND table_name = " + DataToSql( Lower( cTable ) ) + ;
      " ORDER BY ordinal_position" )

   IF ( ::lError := PQresultStatus( res ) != PGRES_TUPLES_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""

      FOR i := 1 TO PQlastrec( res )

         cField := PQgetvalue( res, i, 1 )
         cType  := PQgetvalue( res, i, 2 )
         nSize  := PQgetvalue( res, i, 4 )
         nDec   := PQgetvalue( res, i, 5 )

         DO CASE
         CASE "char" $ cType
            cType := "C"
            nSize := Val( PQgetvalue( res, i, 3 ) )
            nDec  := 0

         CASE "text" $ cType
            cType := "M"
            nSize := 10
            nDec := 0

         CASE "boolean" $ cType
            cType := "L"
            nSize := 1
            nDec  := 0

         CASE "smallint" $ cType
            cType := "N"
            nSize := 5
            nDec  := 0

         CASE "integer" $ cType .OR. "serial" $ cType
            cType := "N"
            nSize := 9
            nDec  := 0

         CASE "bigint" $ cType .OR. "bigserial" $ cType
            cType := "N"
            nSize := 19
            nDec  := 0

         CASE "decimal" $ cType .OR. "numeric" $ cType
            cType := "N"
            nDec  := Val( nDec )
            /* Postgres doesn't store ".", but .dbf does, it can cause data width problem */
            nSize := Val( nSize ) + iif( nDec > 0, 1, 0 )

            /* Numeric/Decimal without scale/precision can genarete big values, so, i limit this to 10,5 */

            IF nDec > 100
               nDec := 5
            ENDIF

            IF nSize > 100
               nSize := 15
            ENDIF

         CASE "real" $ cType .OR. "float4" $ cType
            cType := "N"
            nSize := 15
            nDec  :=  4

         CASE "double precision" $ cType .OR. "float8" $ cType
            cType := "N"
            nSize := 19
            nDec  := 9

         CASE "money" $ cType
            cType := "N"
            nSize := 9
            nDec  := 2

         CASE "timestamp" $ cType
            cType := "C"
            nSize := 20
            nDec  := 0

         CASE "date" $ cType
            cType := "D"
            nSize := 8
            nDec  := 0

         CASE "time" $ cType
            cType := "C"
            nSize := 10
            nDec  := 0

         OTHERWISE
            /* Unsuported */
            cType := "U"
            nSize := 0
            nDec  := -1

         ENDCASE

         IF !( cType == "U" )
            AAdd( result, { cField, cType, nSize, nDec } )
         ENDIF
      NEXT
   ENDIF

   RETURN result

METHOD CreateTable( cTable, aStruct ) CLASS TPQserver

   LOCAL res
   LOCAL fld

   LOCAL cQuery := "CREATE TABLE " + ::Schema + "." + cTable + "( "

   FOR EACH fld IN aStruct

      cQuery += fld[ _STRU_FIELDNAME ]

      SWITCH fld[ _STRU_FIELDTYPE ]
      CASE "C"
         cQuery += " Char(" + hb_ntos( fld[ _STRU_FIELDLEN ] ) + ")"
         EXIT
      CASE "D"
         cQuery += " Date "
         EXIT
      CASE "N"
         cQuery += " Numeric(" + hb_ntos( fld[ _STRU_FIELDLEN ] ) + "," + hb_ntos( fld[ _STRU_FIELDDEC ] ) + ")"
         EXIT
      CASE "L"
         cQuery += " boolean "
         EXIT
      CASE "M"
         cQuery += " text "
         EXIT
      ENDSWITCH

      IF fld:__enumIsLast()
         cQuery += ")"
      ELSE
         cQuery += ","
      ENDIF
   NEXT

   res := PQexec( ::pDB, cQuery )

   IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""
   ENDIF

   RETURN ! ::lError

METHOD DeleteTable( cTable ) CLASS TPQserver

   LOCAL res := PQexec( ::pDB, "DROP TABLE " + ::Schema + "." + cTable  )

   IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
      ::cError := PQresultErrorMessage( res )
   ELSE
      ::cError := ""
   ENDIF

   RETURN ! ::lError

METHOD TraceOn( cFile ) CLASS TPQserver

   ::pTrace := PQtracecreate( cFile )

   IF ::pTrace != NIL
      PQtrace( ::pDb, ::pTrace )
      ::lTrace := .T.
   ENDIF

   RETURN NIL

METHOD TraceOff() CLASS TPQserver

   IF ::pTrace != NIL
      PQuntrace( ::pDb )
      ::pTrace := NIL
   ENDIF

   ::lTrace := .F.

   RETURN NIL


CREATE CLASS TPQQuery

   VAR pQuery
   VAR pDB

   VAR nResultStatus

   VAR lBof
   VAR lEof
   VAR lRead
   VAR lAllCols INIT .T.

   VAR lError   INIT .F.
   VAR cError   INIT ""

   VAR cQuery
   VAR nRecno
   VAR nFields
   VAR nLastrec

   VAR aStruct
   VAR aKeys
   VAR TableName
   VAR Schema
   VAR rows     INIT 0

   METHOD New( pDB, cQuery, lAllCols, cSchema, res )
   METHOD Destroy()
   METHOD Close()            INLINE ::Destroy()

   METHOD Refresh( lQuery, lMeta )
   METHOD Fetch()            INLINE ::Skip()
   METHOD Read()
   METHOD Skip( nRecno )

   METHOD Bof()              INLINE ::lBof
   METHOD Eof()              INLINE ::lEof
   METHOD RecNo()            INLINE ::nRecno
   METHOD LastRec()          INLINE ::nLastrec
   METHOD Goto( nRecno )

   METHOD NetErr()           INLINE ::lError
   METHOD ErrorMsg()         INLINE ::cError

   METHOD FCount()           INLINE ::nFields
   METHOD FieldName( nField )
   METHOD FieldPos( cField )
   METHOD FieldLen( nField )
   METHOD FieldDec( nField )
   METHOD FieldType( nField )
   METHOD Update( oRow )
   METHOD Delete( oRow )
   METHOD Append( oRow )
   METHOD SetKey()

   METHOD Changed( nField )  INLINE !( ::aRow[ nField ] == ::aOld[ nField ] )
   METHOD Blank()            INLINE ::GetBlankRow()

   METHOD Struct()

   METHOD FieldGet( nField, nRow )
   METHOD GetRow( nRow )
   METHOD GetBlankRow()

ENDCLASS


METHOD New( pDB, cQuery, lAllCols, cSchema, res ) CLASS TPQquery

   ::pDB := pDB
   ::nResultStatus := -1
   ::cQuery := cQuery
   ::lAllCols := lAllCols
   ::Schema := cSchema

   IF res != NIL
      ::pQuery := res
   ENDIF

   ::Refresh( res == NIL )

   RETURN self

METHOD Destroy() CLASS TPQquery

   IF ::nResultStatus != -1
      ::pQuery := NIL
      ::nResultStatus := -1
   ENDIF

   RETURN .T.

METHOD Refresh( lQuery, lMeta ) CLASS TPQquery

   LOCAL res
   LOCAL aStruct := {}
   LOCAL aTemp
   LOCAL i
   LOCAL cType, nDec, nSize

   ::Destroy()

   ::lBof     := .T.
   ::lEof     := .T.
   ::lRead    := .F.
   ::nRecno   := 0
   ::nLastrec := 0
   ::Rows     := 0

   IF hb_defaultValue( lQuery, .T. )
      res := PQexec( ::pDB, ::cQuery )
   ELSE
      res := ::pQuery
   ENDIF

   ::nResultStatus := PQresultStatus( res )

   IF ::nResultStatus == PGRES_TUPLES_OK

      IF hb_defaultValue( lMeta, .T. )

         ::aStruct := {}
         ::nFields := 0

         /* Get some information about metadata */
         aTemp := PQmetadata( res )

         IF HB_ISARRAY( aTemp )

            FOR EACH i IN aTemp

               cType := i[ HBPG_META_FIELDTYPE ]
               nSize := i[ HBPG_META_FIELDLEN ]
               nDec  := i[ HBPG_META_FIELDDEC ]

               DO CASE
               CASE "char" $ cType
                  cType := "C"

               CASE "numeric" $ cType .OR. "decimal" $ cType
                  cType := "N"

                  /* Postgres don't store ".", but .dbf does, it can cause data width problem */
                  IF nDec > 0
                     nSize++
                     /* Numeric/Decimal without scale/precision can genarete big values, so, i limit this to 10,5 */
                     IF nDec > 100
                        nDec := 5
                     ENDIF
                  ENDIF

                  IF nSize > 100
                     nSize := 15
                  ENDIF

               CASE "date" $ cType
                  cType := "D"
                  nSize := 8

               CASE "text" $ cType
                  cType := "M"

               CASE "boolean" $ cType
                  cType := "L"
                  nSize := 1

               CASE "smallint" $ cType
                  cType := "N"
                  nSize := 5

               CASE "integer" $ cType .OR. "serial" $ cType
                  cType := "N"
                  nSize := 9

               CASE "bigint" $ cType .OR. "bigserial" $ cType
                  cType := "N"
                  nSize := 19

               CASE "real" $ cType .OR. "float4" $ cType
                  cType := "N"
                  nSize := 15
                  nDec  :=  4

               CASE "double precision" $ cType .OR. "float8" $ cType
                  cType := "N"
                  nSize := 19
                  nDec  := 9

               CASE "money" $ cType
                  cType := "N"
                  nSize := 10
                  nDec  := 2

               CASE "timestamp" $ cType
                  cType := "C"
                  nSize := 20

               CASE "time" $ cType
                  cType := "C"
                  nSize := 10

               OTHERWISE
                  /* Unsuported */
                  cType := "K"
               ENDCASE

               AAdd( aStruct, { ;
                  i[ HBPG_META_FIELDNAME ], ;
                  cType, ;
                  nSize, ;
                  nDec, ;
                  i[ HBPG_META_TABLE ], ;
                  i[ HBPG_META_TABLECOL ] } )
            NEXT

            ::nFields := PQfcount( res )

            ::aStruct := aStruct

         ENDIF
      ENDIF

      ::nLastrec := PQlastrec( res )
      ::lError := .F.
      ::cError := ""

      IF ::nLastrec != 0
         ::nRecno := 1
         ::lBof := .F.
         ::lEof := .F.
      ENDIF

   ELSE
      IF ( ::lError := ::nResultStatus == PGRES_COMMAND_OK )
         ::cError := ""
         ::rows   := Val( PQcmdTuples( res ) )
      ELSE
         ::cError := PQresultErrorMessage( res )
      ENDIF
   ENDIF

   ::pQuery := res

   RETURN ! ::lError

METHOD Struct() CLASS TPQquery

   LOCAL result := Array( Len( ::aStruct ) )
   LOCAL i

   FOR i := 1 TO Len( ::aStruct )
      result[ i ] := { ;
         ::aStruct[ i ][ _STRU_FIELDNAME ], ;
         ::aStruct[ i ][ _STRU_FIELDTYPE ], ;
         ::aStruct[ i ][ _STRU_FIELDLEN ], ;
         ::aStruct[ i ][ _STRU_FIELDDEC ] }
   NEXT

   RETURN result

METHOD Read() CLASS TPQquery

   IF ! ::lEof
      IF ::lRead
         ::Skip( 1 )
      ELSE
         ::lRead := .T.
      ENDIF
   ENDIF

   RETURN ! ::lEof

METHOD Skip( nrecno ) CLASS TPQquery

   hb_default( @nRecno, 1 )

   IF ::nRecno + nRecno > 0 .AND. ::nRecno + nRecno <= ::nLastrec
      ::nRecno := ::nRecno + nRecno
      ::lEof := .F.
      ::lBof := .F.
   ELSE
      IF ::nRecno + nRecno > ::nLastRec
         ::nRecno := ::nLastRec + 1
         ::lEof := .T.
      ENDIF

      IF ::nRecno + nRecno < 1
         ::nRecno := 1
         ::lBof := .T.
      ENDIF
   ENDIF

   RETURN .T.

METHOD Goto( nRecno ) CLASS TPQquery

   IF nRecno > 0 .AND. nRecno <= ::nLastrec
      ::nRecno := nRecno
      ::lEof := .F.
   ENDIF

   RETURN .T.

METHOD FieldPos( cField ) CLASS TPQquery

   cField := RTrim( Lower( cField ) )

   RETURN AScan( ::aStruct, {| x | x[ _STRU_FIELDNAME ] == cField } )

METHOD FieldName( nField ) CLASS TPQquery

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ELSEIF nField < 1 .OR. nField > Len( ::aStruct )
      nField := 0
   ENDIF

   IF nField > 0
      result := ::aStruct[ nField ][ _STRU_FIELDNAME ]
   ENDIF

   RETURN result

METHOD FieldType( nField ) CLASS TPQquery

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ELSEIF nField < 1 .OR. nField > Len( ::aStruct )
      nField := 0
   ENDIF

   IF nField > 0
      result := ::aStruct[ nField ][ _STRU_FIELDTYPE ]
   ENDIF

   RETURN result

METHOD FieldLen( nField ) CLASS TPQquery

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ELSEIF nField < 1 .OR. nField > Len( ::aStruct )
      nField := 0
   ENDIF

   IF nField > 0
      result := ::aStruct[ nField ][ _STRU_FIELDLEN ]
   ENDIF

   RETURN result

METHOD FieldDec( nField ) CLASS TPQquery

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ELSEIF nField < 1 .OR. nField > Len( ::aStruct )
      nField := 0
   ENDIF

   IF nField > 0
      result := ::aStruct[ nField ][ _STRU_FIELDDEC ]
   ENDIF

   RETURN result

METHOD Delete( oRow ) CLASS TPQquery

   LOCAL res
   LOCAL i
   LOCAL nField
   LOCAL xField
   LOCAL cWhere := ""
   LOCAL aParams := {}

   ::SetKey()

   IF ! Empty( ::Tablename ) .AND. ! Empty( ::aKeys )
      FOR EACH i IN ::aKeys
         nField := oRow:FieldPos( i )
         xField := oRow:FieldGetOld( nField )

         cWhere += i + " = $" + hb_ntos( i:__enumIndex() )

         AAdd( aParams, ValueToString( xField ) )

         IF ! i:__enumIsLast()
            cWhere += " AND "
         ENDIF
      NEXT

      IF !( cWhere == "" )

         res := PQexecParams( ::pDB, ;
            "DELETE FROM " + ::Schema + "." + ::Tablename + " WHERE " + cWhere, aParams )

         IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
            ::cError := PQresultErrorMessage( res )
            ::rows   := 0
         ELSE
            ::cError := ""
            ::rows   := Val( PQcmdTuples( res ) )
         ENDIF
      ENDIF
   ELSE
      ::lError := .T.
      ::cError := "There is no primary keys or query is a joined table"
   ENDIF

   RETURN ! ::lError

METHOD Append( oRow ) CLASS TPQquery

   LOCAL cQuery
   LOCAL i
   LOCAL res
   LOCAL lChanged := .F.
   LOCAL aParams := {}
   LOCAL nParams := 0

   ::SetKey()

   IF ! Empty( ::Tablename )

      cQuery := "INSERT INTO " + ::Schema + "." + ::Tablename + "("

      FOR i := 1 TO oRow:FCount()
         IF ::lAllCols .OR. oRow:changed( i )
            lChanged := .T.
            cQuery += oRow:FieldName( i ) + ","
         ENDIF
      NEXT

      cQuery := hb_StrShrink( cQuery ) + ") VALUES ("

      FOR i := 1 TO oRow:FCount()
         IF ::lAllCols .OR. oRow:Changed( i )
            nParams++
            cQuery += "$" + hb_ntos( nParams ) + ","
            AAdd( aParams, ValueToString( oRow:FieldGet( i ) ) )
         ENDIF
      NEXT

      cQuery := hb_StrShrink( cQuery ) + ")"

      IF lChanged
         res := PQexecParams( ::pDB, cQuery, aParams )

         IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
            ::cError := PQresultErrorMessage( res )
            ::rows   := 0
         ELSE
            ::cError := ""
            ::rows   := Val( PQcmdTuples( res ) )
         ENDIF
      ENDIF
   ELSE
      ::lError := .T.
      ::cError := "Cannot insert in a joined table, or unknown error"
   ENDIF

   RETURN ! ::lError

METHOD Update( oRow ) CLASS TPQquery

   LOCAL cQuery
   LOCAL i
   LOCAL nField
   LOCAL xField
   LOCAL cWhere
   LOCAL res
   LOCAL lChanged := .F.
   LOCAL aParams := {}
   LOCAL nParams := 0

   ::SetKey()

   IF ! Empty( ::Tablename ) .AND. ! Empty( ::aKeys )

      cWhere := ""

      FOR EACH i IN ::aKeys

         nField := oRow:FieldPos( i )
         xField := oRow:FieldGetOld( nField )

         cWhere += i + "=" + DataToSql( xField )

         IF ! i:__enumIsLast()
            cWhere += " AND "
         ENDIF
      NEXT

      cQuery := "UPDATE " + ::Schema + "." + ::Tablename + " SET "

      FOR i := 1 TO oRow:FCount()
         IF ::lAllCols .OR. oRow:Changed( i )
            lChanged := .T.
            nParams++
            cQuery += oRow:FieldName( i ) + " = $" + hb_ntos( nParams ) + ","
            AAdd( aParams, ValueToString( oRow:FieldGet( i ) ) )
         ENDIF
      NEXT

      IF !( cWhere == "" ) .AND. lChanged

         cQuery := hb_StrShrink( cQuery ) + " WHERE " + cWhere

         res := PQexecParams( ::pDB, cQuery, aParams )

         IF ( ::lError := PQresultStatus( res ) != PGRES_COMMAND_OK )
            ::cError := PQresultErrorMessage( res )
            ::rows   := 0
         ELSE
            ::cError := ""
            ::rows   := Val( PQcmdTuples( res ) )
         ENDIF
      ENDIF
   ELSE
      ::lError := .T.
      ::cError := "Cannot insert in a joined table, or unknown error"
   ENDIF

   RETURN ! ::lError

METHOD FieldGet( nField, nRow ) CLASS TPQquery

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ELSEIF nField < 1 .OR. nField > ::nFields
      nField := 0
   ENDIF

   IF nField > 0 .AND. ::nResultStatus == PGRES_TUPLES_OK

      IF ! HB_ISNUMERIC( nRow )
         nRow := ::nRecno
      ENDIF

      result := PQgetvalue( ::pQuery, nRow, nField )

      SWITCH ::aStruct[ nField ][ _STRU_FIELDTYPE ]
      CASE "C"
      CASE "M"
         IF result != NIL
            result := result
         ELSE
            result := ""
         ENDIF
         EXIT

      CASE "N"
         IF result != NIL
            result := Val( result )
         ELSE
            result := 0
         ENDIF
         EXIT

      CASE "D"
         IF result != NIL
            result := hb_SToD( StrTran( result, "-" ) )
         ELSE
            result := hb_SToD()
         ENDIF
         EXIT

      CASE "L"
         IF result != NIL
            result := ( result == "t" )
         ELSE
            result := .F.
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN result

METHOD Getrow( nRow ) CLASS TPQquery

   LOCAL result
   LOCAL aRow
   LOCAL aOld
   LOCAL nCol

   IF ! HB_ISNUMERIC( nRow )
      nRow := ::nRecno
   ENDIF

   IF ::nResultStatus == PGRES_TUPLES_OK

      IF nRow > 0 .AND. nRow <= ::nLastRec

         aRow := Array( ::nFields )
         aOld := Array( ::nFields )

         FOR nCol := 1 TO ::nFields
            aRow[ nCol ] := ::FieldGet( nCol, nRow )
            aOld[ nCol ] := ::FieldGet( nCol, nRow )
         NEXT

         result := TPQRow():New( aRow, aOld, ::aStruct )

      ELSEIF nRow > ::nLastrec
         result := ::GetBlankRow()
      ENDIF
   ENDIF

   RETURN result

METHOD GetBlankRow() CLASS TPQquery

   LOCAL aRow := Array( ::nFields )
   LOCAL aOld := Array( ::nFields )
   LOCAL i

   FOR i := 1 TO ::nFields
      SWITCH ::aStruct[ i ][ _STRU_FIELDTYPE ]
      CASE "C"
      CASE "M"
         aRow[ i ] := ""
         aOld[ i ] := ""
         EXIT
      CASE "N"
         aRow[ i ] := 0
         aOld[ i ] := 0
         EXIT
      CASE "L"
         aRow[ i ] := .F.
         aOld[ i ] := .F.
         EXIT
      CASE "D"
         aRow[ i ] := hb_SToD()
         aOld[ i ] := hb_SToD()
         EXIT
      ENDSWITCH
   NEXT

   RETURN TPQRow():New( aRow, aOld, ::aStruct )

METHOD SetKey() CLASS TPQquery

   LOCAL cQuery
   LOCAL i, x
   LOCAL nTableId, xTableId := -1
   LOCAL nCount := 0
   LOCAL res
   LOCAL nPos

   IF ::nResultStatus == PGRES_TUPLES_OK
      IF ::Tablename == NIL
         /* set the table name looking for table oid */
         FOR EACH i IN ::aStruct
            /* Store table codes oid */
            nTableId := i[ _STRU_TABLE ]

            IF nTableId != xTableId
               xTableId := nTableId
               nCount++
            ENDIF
         NEXT

         IF nCount == 1
            /* first, try get the table name from select, else get from pg_catalog */
            IF ( nPos := At( "FROM ", Upper( ::cQuery ) ) ) != 0
               cQuery := Lower( LTrim( SubStr( ::cQuery, nPos + 5 ) ) )

               IF ( nPos := At( ".", cQuery ) ) != 0
                  ::Schema := AllTrim( Left( cQuery, nPos - 1 ) )
                  cQuery := SubStr( cQuery, nPos + 1 )
               ENDIF

               IF ( nPos := At( " ", cQuery ) ) != 0
                  ::Tablename := RTrim( Left( cQuery, nPos ) )
               ELSE
                  ::Tablename := cQuery
               ENDIF
            ENDIF

            IF Empty( ::Tablename )

               res := PQexec( ::pDB, "SELECT relname FROM pg_class WHERE oid = " + hb_ntos( xTableId ) )

               IF PQresultStatus( res ) == PGRES_TUPLES_OK .AND. PQlastrec( res ) != 0
                  ::Tablename := RTrim( PQgetvalue( res, 1, 1 ) )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ::aKeys == NIL .AND. ! Empty( ::Tablename )

         /* Set the table primary keys */
         res := PQexec( ::pDB, ;
            "SELECT c.attname" + ;
            "  FROM pg_class a, pg_class b, pg_attribute c, pg_index d, pg_namespace e" + ;
            " WHERE a.oid = d.indrelid" + ;
            "   AND a.relname = '" + ::Tablename + "'" + ;
            "   AND b.oid = d.indexrelid" + ;
            "   AND c.attrelid = b.oid" + ;
            "   AND d.indisprimary" + ;
            "   AND e.oid = a.relnamespace" + ;
            "   AND e.nspname = " + DataToSql( ::Schema ) )

         IF PQresultStatus( res ) == PGRES_TUPLES_OK .AND. PQlastrec( res ) != 0
            ::aKeys := {}

            FOR x := 1 TO PQlastrec( res )
               AAdd( ::aKeys, PQgetvalue( res, x, 1 ) )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

CREATE CLASS TPQRow

   VAR aRow
   VAR aOld
   VAR aStruct

   METHOD New( row, old, struct )

   METHOD FCount()              INLINE Len( ::aRow )
   METHOD FieldGet( nField )
   METHOD FieldPut( nField, Value )
   METHOD FieldName( nField )
   METHOD FieldPos( cField )
   METHOD FieldLen( nField )
   METHOD FieldDec( nField )
   METHOD FieldType( nField )
   METHOD Changed( nField )     INLINE !( ::aRow[ nField ] == ::aOld[ nField ] )
   METHOD FieldGetOld( nField ) INLINE ::aOld[ nField ]

ENDCLASS


METHOD new( row, old, struct ) CLASS TPQrow

   ::aRow := row
   ::aOld := old
   ::aStruct := struct

   RETURN self

METHOD FieldGet( nField ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      result := ::aRow[ nField ]
   ENDIF

   RETURN result

METHOD FieldPut( nField, Value ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      result := ::aRow[ nField ] := Value
   ENDIF

   RETURN result

METHOD FieldName( nField ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ _STRU_FIELDNAME ]
   ENDIF

   RETURN result

METHOD FieldPos( cField ) CLASS TPQrow

   cField := RTrim( Lower( cField ) )

   RETURN AScan( ::aStruct, {| x | x[ _STRU_FIELDNAME ] == cField } )

METHOD FieldType( nField ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ _STRU_FIELDTYPE ]
   ENDIF

   RETURN result

METHOD FieldLen( nField ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ _STRU_FIELDLEN ]
   ENDIF

   RETURN result

METHOD FieldDec( nField ) CLASS TPQrow

   LOCAL result

   IF HB_ISSTRING( nField )
      nField := ::FieldPos( nField )
   ENDIF

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ _STRU_FIELDDEC ]
   ENDIF

   RETURN result

STATIC FUNCTION EscapeParam( cString )

   cString := hb_StrReplace( cString, { ;
      "'" => "\'", ;
      "\" => "\\" } )

   RETURN iif( Empty( cString ) .OR. " " $ cString, "'" + cString + "'", cString )

STATIC FUNCTION DataToSql( xField )

   SWITCH ValType( xField )
   CASE "C"
   CASE "M" ; RETURN "'" + StrTran( xField, "'", " " ) + "'"
   CASE "D" ; RETURN DToS( xField )
   CASE "N" ; RETURN hb_ntos( xField )
   CASE "L" ; RETURN iif( xField, "'t'", "'f'" )
   ENDSWITCH

   RETURN "NULL"

STATIC FUNCTION ValueToString( xField )

   SWITCH ValType( xField )
   CASE "C"
   CASE "M" ; RETURN xField
   CASE "D" ; RETURN DToS( xField )
   CASE "N" ; RETURN hb_ntos( xField )
   CASE "L" ; RETURN iif( xField, "t", "f" )
   ENDSWITCH

   RETURN NIL
