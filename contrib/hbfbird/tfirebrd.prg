/*
 * Firebird RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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

#define SQL_TEXT            452
#define SQL_VARYING         448
#define SQL_SHORT           500
#define SQL_LONG            496
#define SQL_FLOAT           482
#define SQL_DOUBLE          480
#define SQL_D_FLOAT         530
#define SQL_TIMESTAMP       510
#define SQL_BLOB            520
#define SQL_ARRAY           540
#define SQL_QUAD            550
#define SQL_TYPE_TIME       560
#define SQL_TYPE_DATE       570
#define SQL_INT64           580

CREATE CLASS TFbServer

   VAR db
   VAR trans
   VAR StartedTrans
   VAR nError
   VAR lError
   VAR dialect

   METHOD New( cServer, cUser, cPassword, nDialect )
   METHOD Destroy()  INLINE FBClose( ::db )
   METHOD Close()    INLINE FBClose( ::db )

   METHOD TableExists( cTable )
   METHOD ListTables()
   METHOD TableStruct( cTable )

   METHOD StartTransaction()
   METHOD Commit()
   METHOD Rollback()

   METHOD Execute( cQuery )
   METHOD Query( cQuery )

   METHOD Update( oRow, cWhere )
   METHOD Delete( oRow, cWhere )
   METHOD Append( oRow )

   METHOD NetErr()   INLINE ::lError
   METHOD Error()    INLINE FBError( ::nError )
   METHOD ErrorNo()  INLINE ::nError

ENDCLASS

METHOD New( cServer, cUser, cPassword, nDialect ) CLASS TFbServer

   ::lError := .F.
   ::nError := 0
   ::StartedTrans := .F.
   ::Dialect := hb_defaultValue( nDialect, 1 )

   ::db := FBConnect( cServer, cUser, cPassword )

   IF HB_ISNUMERIC( ::db )
      ::lError := .T.
      ::nError := ::db
   ENDIF

   RETURN self

METHOD StartTransaction() CLASS TFbServer

   LOCAL result := .F.

   ::trans := FBStartTransaction( ::db )

   IF HB_ISNUMERIC( ::trans )
      ::lError := .T.
      ::nError := ::trans
   ELSE
      result := .T.
      ::lError := .F.
      ::lnError := 0
      ::StartedTrans := .T.
   ENDIF

   RETURN result

METHOD Rollback() CLASS TFbServer

   LOCAL result := .F.
   LOCAL n

   IF ::StartedTrans
      IF ( n := FBRollback( ::trans ) ) < 0
         ::lError := .T.
         ::nError := n
      ELSE
         ::lError := .F.
         ::nError := 0
         result := .T.
         ::StartedTrans := .F.
      ENDIF
   ENDIF

   RETURN result

METHOD Commit() CLASS TFbServer

   LOCAL result := .F.
   LOCAL n

   IF ::StartedTrans
      IF ( n := FBCommit( ::trans ) ) < 0
         ::lError := .T.
         ::nError := n
      ELSE
         ::lError := .F.
         ::nError := 0
         result := .T.
         ::StartedTrans := .F.
      ENDIF
   ENDIF

   RETURN result

METHOD Execute( cQuery ) CLASS TFbServer

   LOCAL result
   LOCAL n

   cQuery := RemoveSpaces( cQuery )

   IF ::StartedTrans
      n := FBExecute( ::db, cQuery, ::dialect, ::trans )
   ELSE
      n := FBExecute( ::db, cQuery, ::dialect )
   ENDIF

   IF n < 0
      ::lError := .T.
      ::nError := n
      result := .F.
   ELSE
      ::lError := .F.
      ::nError := 0
      result := .T.
   ENDIF

   RETURN result

METHOD Query( cQuery ) CLASS TFbServer
   RETURN TFBQuery():New( ::db, cQuery, ::dialect )

METHOD TableExists( cTable ) CLASS TFbServer

   LOCAL result := .F.

   LOCAL cQuery := ;
      "select rdb$relation_name" + ;
      "  from rdb$relations" + ;
      " where rdb$relation_name = " + '"' + Upper( cTable ) + '"'

   LOCAL qry := FBQuery( ::db, cQuery, ::dialect )

   IF HB_ISARRAY( qry )
      result := ( FBFetch( qry ) == 0 )

      FBFree( qry )
   ENDIF

   RETURN result

METHOD ListTables() CLASS TFbServer

   LOCAL result := {}

   LOCAL cQuery := ;
      "select rdb$relation_name" + ;
      "  from rdb$relations" + ;
      ' where rdb$relation_name not like "RDB$%"' + ;
      "   and rdb$view_blr is null" + ;
      " order by 1"

   LOCAL qry := FBQuery( ::db, RemoveSpaces( cQuery ), ::dialect )

   IF HB_ISARRAY( qry )
      DO WHILE FBFetch( qry ) == 0
         AAdd( result, hb_defaultValue( FBGetData( qry, 1 ), "" ) )
      ENDDO

      FBFree( qry )
   ENDIF

   RETURN result

METHOD TableStruct( cTable ) CLASS TFbServer

   LOCAL result := {}
   LOCAL cType, nSize, cDomain, cField, nType, nDec

   LOCAL cQuery := ;
      "select" + ;
      "   a.rdb$field_name," + ;
      "   b.rdb$field_type," + ;
      "   b.rdb$field_length," + ;
      "   b.rdb$field_scale * -1," + ;
      "   a.rdb$field_source" + ;
      " from" + ;
      "   rdb$relation_fields a, rdb$fields b" + ;
      " where" + ;
      "   a.rdb$field_source = b.rdb$field_name" + ;
      "   and a.rdb$relation_name = " + '"' + Upper( cTable ) + '"' + ;
      " order by" + ;
      "   a.rdb$field_position"

   LOCAL qry := FBQuery( ::db, RemoveSpaces( cQuery ), ::dialect )

   IF HB_ISARRAY( qry )
      DO WHILE FBFetch( qry ) == 0
         cField  := hb_defaultValue( FBGetData( qry, 1 ), "" )
         nType   := Val( hb_defaultValue( FBGetData( qry, 2 ), "" ) )
         nSize   := Val( hb_defaultValue( FBGetData( qry, 3 ), "" ) )
         nDec    := Val( hb_defaultValue( FBGetData( qry, 4 ), "" ) )
         cDomain := hb_defaultValue( FBGetData( qry, 5 ), "" )

         SWITCH nType
         CASE 7  // SMALLINT
            IF "BOOL" $ cDomain
               cType := "L"
               nSize := 1
               nDec := 0
            ELSE
               cType := "N"
               nSize := 5
            ENDIF
            EXIT

         CASE 8  // INTEGER
         CASE 9
            cType := "N"
            nSize := 9
            EXIT

         CASE 10  // FLOAT
         CASE 11
            cType := "N"
            nSize := 15
            EXIT

         CASE 12  // DATE
            cType := "D"
            nSize := 8
            EXIT

         CASE 13  // TIME
            cType := "C"
            nSize := 10
            EXIT

         CASE 14  // CHAR
            cType := "C"
            EXIT

         CASE 16  // INT64
            cType := "N"
            nSize := 9
            EXIT

         CASE 27  // DOUBLE
            cType := "N"
            nSize := 15
            EXIT

         CASE 35  // TIMESTAMP
            cType := "D"
            nSize := 8
            EXIT

         CASE 37  // VARCHAR
         CASE 40
            cType := "C"
            EXIT

         CASE 261  // BLOB
            cType := "M"
            nSize := 10
            EXIT

         OTHERWISE
            cType := "C"
            nDec := 0
         ENDSWITCH

         AAdd( result, { cField, cType, nSize, nDec } )

      ENDDO

      FBFree( qry )
   ENDIF

   RETURN result

METHOD Delete( oRow, cWhere ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL aKeys, i, nField, xField

   LOCAL aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Cannot delete joined tables

      IF ! HB_ISSTRING( cWhere )
         aKeys := oRow:GetKeyField()

         cWhere := ""
         FOR EACH i IN aKeys
            nField := oRow:FieldPos( i )
            xField := oRow:FieldGet( nField )

            cWhere += i + "=" + DataToSql( xField )

            IF ! i:__enumIsLast()
               cWhere += ","
            ENDIF
         NEXT
      ENDIF

      IF ! Empty( cWhere )
         result := ::Execute( "DELETE FROM " + aTables[ 1 ] + " WHERE " + cWhere )
      ENDIF
   ENDIF

   RETURN result

METHOD Append( oRow ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL cQuery, i

   LOCAL aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Can insert only one table, not in joined tables

      cQuery := "INSERT INTO " + aTables[ 1 ] + "("
      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            // Send only changed field
            cQuery += oRow:FieldName( i ) + ","
         ENDIF
      NEXT

      cQuery := hb_StrShrink( cQuery ) + ") VALUES ("

      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            cQuery += DataToSql( oRow:FieldGet( i ) ) + ","
         ENDIF
      NEXT

      cQuery := hb_StrShrink( cQuery ) + ")"

      result := ::Execute( cQuery )
   ENDIF

   RETURN result

METHOD Update( oRow, cWhere ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL aKeys, cQuery, i, nField, xField

   LOCAL aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Cannot insert joined tables

      IF ! HB_ISSTRING( cWhere )
         aKeys := oRow:GetKeyField()

         cWhere := ""
         FOR EACH i IN aKeys
            nField := oRow:FieldPos( i )
            xField := oRow:FieldGet( nField )

            cWhere += i + "=" + DataToSql( xField )

            IF ! i:__enumIsLast()
               cWhere += ", "
            ENDIF
         NEXT
      ENDIF

      cQuery := "UPDATE " + aTables[ 1 ] + " SET "
      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            cQuery += oRow:FieldName( i ) + " = " + DataToSql( oRow:FieldGet( i ) ) + ","
         ENDIF
      NEXT

      IF ! Empty( cWhere )
         result := ::Execute( hb_StrShrink( cQuery ) + " WHERE " + cWhere )
      ENDIF
   ENDIF

   RETURN result

CREATE CLASS TFbQuery

   VAR nError
   VAR lError
   VAR Dialect
   VAR lBof
   VAR lEof
   VAR nRecno
   VAR qry
   VAR aStruct
   VAR numcols
   VAR closed
   VAR db
   VAR query
   VAR aKeys
   VAR aTables

   METHOD New( nDB, cQuery, nDialect )
   METHOD Destroy()
   METHOD Close()            INLINE ::Destroy()

   METHOD Refresh()
   METHOD Fetch()
   METHOD Skip()             INLINE ::Fetch()

   METHOD Bof()              INLINE ::lBof
   METHOD Eof()              INLINE ::lEof
   METHOD RecNo()            INLINE ::nRecno

   METHOD NetErr()           INLINE ::lError
   METHOD Error()            INLINE FBError( ::nError )
   METHOD ErrorNo()          INLINE ::nError

   METHOD FCount()           INLINE ::numcols
   METHOD Struct()
   METHOD FieldName( nField )
   METHOD FieldPos( cField )
   METHOD FieldLen( nField )
   METHOD FieldDec( nField )
   METHOD FieldType( nField )

   METHOD FieldGet( nField )
   METHOD GetRow()
   METHOD GetBlankRow()
   METHOD Blank()            INLINE ::GetBlankRow()
   METHOD GetKeyField()

ENDCLASS

METHOD New( nDB, cQuery, nDialect ) CLASS TFbQuery

   ::db := nDb
   ::query := RemoveSpaces( cQuery )
   ::dialect := nDialect
   ::closed := .T.
   ::aKeys := NIL

   ::Refresh()

   RETURN self

METHOD Refresh() CLASS TFbQuery

   LOCAL qry, result, i, aTable := {}

   IF ! ::closed
      ::Destroy()
   ENDIF

   ::lBof := .T.
   ::lEof := .F.
   ::nRecno := 0
   ::closed := .F.
   ::numcols := 0
   ::aStruct := {}
   ::nError := 0
   ::lError := .F.

   result := .T.

   qry := FBQuery( ::db, ::query, ::dialect )

   IF HB_ISARRAY( qry )
      ::numcols := qry[ 4 ]

      ::aStruct := StructConvert( qry[ 6 ], ::db, ::dialect )

      ::lError := .F.
      ::nError := 0
      ::qry := qry

      /* Tables in query */
      FOR EACH i IN ::aStruct
         IF hb_AScan( aTable, i[ 5 ], , , .T. ) == 0
            AAdd( aTable, i[ 5 ] )
         ENDIF
      NEXT

      ::aTables := aTable

   ELSE
      ::lError := .T.
      ::nError := qry
   ENDIF

   RETURN result

METHOD Destroy() CLASS TFbQuery

   LOCAL result := .T.
   LOCAL n

   IF ! ::lError .AND. ( n := FBFree( ::qry ) ) < 0
      ::lError := .T.
      ::nError := n
   ENDIF

   ::closed := .T.

   RETURN result

METHOD Fetch() CLASS TFbQuery

   LOCAL result := .F.
   LOCAL fetch_stat

   IF ! ::lError .AND. ! ::lEof .AND. ! ::Closed

      fetch_stat := FBFetch( ::qry )

      ::nRecno++

      IF fetch_stat == 0
         ::lBof := .F.
         result := .T.
      ELSE
         ::lEof := .T.
      ENDIF
   ENDIF

   RETURN result

METHOD Struct() CLASS TFbQuery

   LOCAL result := {}
   LOCAL i

   IF ! ::lError
      FOR EACH i IN ::aStruct
         AAdd( result, { i[ 1 ], i[ 2 ], i[ 3 ], i[ 4 ] } )  // DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC
      NEXT
   ENDIF

   RETURN result

METHOD FieldPos( cField ) CLASS TFbQuery

   IF ! ::lError
      cField := RTrim( Upper( cField ) )
      RETURN AScan( ::aStruct, {| x | x[ 1 ] == cField } )
   ENDIF

   RETURN 0

METHOD FieldName( nField ) CLASS TFbQuery

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 1 ]
   ENDIF

   RETURN NIL

METHOD FieldType( nField ) CLASS TFbQuery

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 2 ]
   ENDIF

   RETURN NIL

METHOD FieldLen( nField ) CLASS TFbQuery

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 3 ]
   ENDIF

   RETURN NIL

METHOD FieldDec( nField ) CLASS TFbQuery

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 4 ]
   ENDIF

   RETURN NIL

METHOD FieldGet( nField ) CLASS TFbQuery

   LOCAL result, aBlob, i

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct ) .AND. ! ::closed

      /* TODO: Convert to right data type */

      result := FBGetData( ::qry, nField )

      SWITCH ::aStruct[ nField ][ 2 ]
      CASE "M"  /* Blob */
         IF HB_ISPOINTER( result )
            aBlob := FBGetBlob( ::db, result )
            result := ""
            IF HB_ISARRAY( aBlob )
               FOR EACH i IN aBlob
                  result += i
               NEXT
            ENDIF
#if 0
            result := FBGetBlob( ::db, result )
#endif
         ELSE
            result := ""
         ENDIF
         EXIT

      CASE "N"
         result := iif( HB_ISSTRING( result ), Val( result ), 0 )
         EXIT

      CASE "D"
         result := hb_SToD( result )
         EXIT

      CASE "L"
         result := ( HB_ISSTRING( result ) .AND. Val( result ) == 1 )
         EXIT

      ENDSWITCH
   ENDIF

   RETURN result

METHOD Getrow() CLASS TFbQuery

   LOCAL aRow
   LOCAL i

   IF ! ::lError .AND. ! ::closed

      aRow := Array( ::numcols )

      FOR i := 1 TO ::numcols
         aRow[ i ] := ::FieldGet( i )
      NEXT

      RETURN TFBRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
   ENDIF

   RETURN NIL

METHOD GetBlankRow() CLASS TFbQuery

   LOCAL aRow
   LOCAL i

   IF ! ::lError
      aRow := Array( ::numcols )

      FOR i := 1 TO ::numcols

         SWITCH ::aStruct[ i ][ 2 ]
         CASE "C"
         CASE "M"
            aRow[ i ] := ""
            EXIT
         CASE "N"
            aRow[ i ] := 0
            EXIT
         CASE "L"
            aRow[ i ] := .F.
            EXIT
         CASE "D"
            aRow[ i ] := hb_SToD()
            EXIT
         ENDSWITCH
      NEXT

      RETURN TFBRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
   ENDIF

   RETURN NIL

METHOD GetKeyField() CLASS TFbQuery

   IF ::aKeys == NIL
      ::aKeys := KeyField( ::aTables, ::db, ::dialect )
   ENDIF

   RETURN ::aKeys

CREATE CLASS TFbRow

   VAR aRow
   VAR aStruct
   VAR aChanged
   VAR aKeys
   VAR db
   VAR dialect
   VAR aTables

   METHOD New( row, struct, nDB, nDialect, aTable )
   METHOD Changed( nField )
   METHOD GetTables()        INLINE ::aTables
   METHOD FCount()           INLINE Len( ::aRow )
   METHOD FieldGet( nField )
   METHOD FieldPut( nField, Value )
   METHOD FieldName( nField )
   METHOD FieldPos( cField )
   METHOD FieldLen( nField )
   METHOD FieldDec( nField )
   METHOD FieldType( nField )
   METHOD GetKeyField()

ENDCLASS

METHOD New( row, struct, nDb, nDialect, aTable ) CLASS TFbRow

   ::aRow := row
   ::aStruct := struct
   ::db := nDB
   ::dialect := nDialect
   ::aTables := aTable
   ::aChanged := Array( Len( row ) )

   RETURN self

METHOD Changed( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      RETURN ::aChanged[ nField ] != NIL
   ENDIF

   RETURN NIL

METHOD FieldGet( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      RETURN ::aRow[ nField ]
   ENDIF

   RETURN NIL

METHOD FieldPut( nField, Value ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      ::aChanged[ nField ] := .T.
      RETURN ::aRow[ nField ] := Value
   ENDIF

   RETURN NIL

METHOD FieldName( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 1 ]
   ENDIF

   RETURN NIL

METHOD FieldPos( cField ) CLASS TFbRow

   cField := RTrim( Upper( cField ) )

   RETURN AScan( ::aStruct, {| x | x[ 1 ] == cField } )

METHOD FieldType( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 2 ]
   ENDIF

   RETURN NIL

METHOD FieldLen( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 3 ]
   ENDIF

   RETURN NIL

METHOD FieldDec( nField ) CLASS TFbRow

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      RETURN ::aStruct[ nField ][ 4 ]
   ENDIF

   RETURN NIL

METHOD GetKeyField() CLASS TFbRow

   IF ::aKeys == NIL
      ::aKeys := KeyField( ::aTables, ::db, ::dialect )
   ENDIF

   RETURN ::aKeys

STATIC FUNCTION KeyField( aTables, db, dialect )

   LOCAL cTable, cQuery
   LOCAL qry
   LOCAL aKeys := {}

   /* Check row, many tables exists in current query, so we must have only one table */

   IF Len( aTables ) == 1
      cTable := aTables[ 1 ]

      cQuery := ;
         "select" + ;
         "   a.rdb$field_name" + ;
         " from" + ;
         "   rdb$index_segments a," + ;
         "   rdb$relation_constraints b" + ;
         " where" + ;
         "   a.rdb$index_name = b.rdb$index_name and" + ;
         '   b.rdb$constraint_type = "PRIMARY KEY" and' + ;
         "   b.rdb$relation_name = " + DataToSql( cTable ) + ;
         " order by" + ;
         "   b.rdb$relation_name," + ;
         "   a.rdb$field_position"

      qry := FBQuery( db, RemoveSpaces( cQuery ), dialect )

      IF HB_ISARRAY( qry )
         DO WHILE FBFetch( qry ) == 0
            AAdd( aKeys, RTrim( hb_defaultValue( FBGetData( qry, 1 ), "" ) ) )
         ENDDO

         FBFree( qry )
      ENDIF
   ENDIF

   RETURN aKeys

STATIC FUNCTION DataToSql( xField )

   SWITCH ValType( xField )
   CASE "C" ; RETURN '"' + StrTran( xField, '"', " " ) + '"'
   CASE "D" ; RETURN '"' + hb_DToC( xField, "mm/dd/yyyy" ) + '"'
   CASE "N" ; RETURN hb_ntos( xField )
   CASE "L" ; RETURN iif( xField, "1", "0" )
   ENDSWITCH

   RETURN ""

STATIC FUNCTION StructConvert( aStru, db, dialect )

   LOCAL aNew := {}

   LOCAL cField
   LOCAL nType
   LOCAL cType
   LOCAL nSize
   LOCAL nDec
   LOCAL cTable
   LOCAL cDomain
   LOCAL i
   LOCAL qry
   LOCAL cQuery
   LOCAL aDomains := {}
   LOCAL nVal

   LOCAL cTables := ""
   LOCAL cFields := ""

   /* create table list and field list */
   FOR EACH i IN aStru
      cTables += DataToSql( i[ 5 ] )
      cFields += DataToSql( i[ 1 ] )

      IF ! i:__enumIsLast()
         cTables += ","
         cFields += ","
      ENDIF
   NEXT

   /* Look for domains */
   cQuery := ;
      "select rdb$relation_name, rdb$field_name, rdb$field_source" + ;
      "  from rdb$relation_fields" + ;
      ' where rdb$field_name not like "RDB$%"' + ;
      "   and rdb$relation_name in (" + cTables + ")" + ;
      "   and rdb$field_name in (" + cFields + ")"

   IF HB_ISARRAY( qry := FBQuery( db, RemoveSpaces( cQuery ), dialect ) )

      DO WHILE FBFetch( qry ) == 0
         AAdd( aDomains, { ;
            hb_defaultValue( FBGetData( qry, 1 ), "" ), ;
            hb_defaultValue( FBGetData( qry, 2 ), "" ), ;
            hb_defaultValue( FBGetData( qry, 3 ), "" ) } )
      ENDDO

      FBFree( qry )
   ENDIF

   FOR EACH i IN aStru

      cField := RTrim( i[ 7 ] )
      nType  := i[ 2 ]
      nSize  := i[ 3 ]
      nDec   := i[ 4 ] * -1
      cTable := RTrim( i[ 5 ] )

      nVal := AScan( aDomains, {| x | RTrim( x[ 1 ] ) == cTable .AND. RTrim( x[ 2 ] ) == cField } )

      cDomain := iif( nVal > 0, aDomains[ nVal ][ 3 ], "" )

      SWITCH nType
      CASE SQL_TEXT
         cType := "C"
         EXIT
      CASE SQL_VARYING
         cType := "C"
         EXIT
      CASE SQL_SHORT
         /* Firebird doesn't have boolean field, so if you define domain with BOOL then i will consider logical, ex:
            create domain boolean_field as smallint default 0 not null check (value in (0,1)) */

         IF "BOOL" $ cDomain
            cType := "L"
            nSize := 1
            nDec := 0
         ELSE
            cType := "N"
            nSize := 5
         ENDIF
         EXIT
      CASE SQL_LONG
         cType := "N"
         nSize := 9
         EXIT
      CASE SQL_INT64
         cType := "N"
         nSize := 9
         EXIT
      CASE SQL_FLOAT
         cType := "N"
         nSize := 15
         EXIT
      CASE SQL_DOUBLE
         cType := "N"
         nSize := 15
         EXIT
      CASE SQL_TIMESTAMP
         cType := "T"
         nSize := 8
         EXIT
      CASE SQL_TYPE_DATE
         cType := "D"
         nSize := 8
         EXIT
      CASE SQL_TYPE_TIME
         cType := "C"
         nSize := 8
         EXIT
      CASE SQL_BLOB
         cType := "M"
         nSize := 10
         EXIT
      OTHERWISE
         cType := "C"
         nDec := 0
      ENDSWITCH

      AAdd( aNew, { cField, cType, nSize, nDec, cTable, cDomain } )
   NEXT

   RETURN aNew

STATIC FUNCTION RemoveSpaces( cQuery )

   DO WHILE "  " $ cQuery
      cQuery := StrTran( cQuery, "  ", " " )
   ENDDO

   RETURN cQuery
