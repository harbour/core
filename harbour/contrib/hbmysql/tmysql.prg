/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL DBMS classes.
 * These classes try to emulate clipper dbXXXX functions on a SQL query
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/*
 2002-01-28 21:30 UTC+0100 Patrick Mast <email@patrickmast.com>
   * contrib/mysql/tmysql
     + Added DateTime field
     * Added more info on Alert message for Unknown type
     * Modified ClipValue2SQL() to process empty strings
 */

#include "hbclass.ch"

#include "common.ch"
#include "dbstruct.ch"
#include "set.ch"

#include "mysql.ch"


// Every single row of an answer
CREATE CLASS TMySQLRow

   VAR   aRow              // a single row of answer
   VAR   aDirty            // array of booleans set to .T. if corresponding field of aRow has been changed
   VAR   aOldValue         // If aDirty[n] is .T. aOldValue[n] keeps a copy of changed value if aRow[n] is part of a primary key
   //DAVID:
   VAR   aOriValue         // Original values ( same as TMySQLtable:aOldValue )

   VAR   aFieldStruct      // type of each field
   VAR   cTable            // Name of table containing this row, empty if TMySQLQuery returned this row

   METHOD   New( aRow, aFStruct, cTableName )     // Create a new Row object

   METHOD   FieldGet( cnField )          // Same as clipper ones, but FieldGet() and FieldPut() accept a string as
   METHOD   FieldPut( cnField, Value )   // field identifier, not only a number
   METHOD   FieldName( nNum )
   METHOD   FieldPos( cFieldName )

   METHOD   FieldLen( nNum )          // Length of field N
   METHOD   FieldDec( nNum, lFormat ) // How many decimals in field N
   METHOD   FieldType( nNum )         // Clipper type of field N

   METHOD   MakePrimaryKeyWhere()     // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New( aRow, aFStruct, cTableName ) CLASS TMySQLRow

   DEFAULT cTableName TO ""
   DEFAULT aFStruct TO {}

   ::aRow := aRow
   //DAVID:
   ::aOriValue := AClone( aRow )    // Original values ( same as TMySQLtable:aOldValue )

   ::aFieldStruct := aFStruct
   ::cTable := cTableName

   ::aDirty := Array( Len( ::aRow ) )
   ::aOldValue := Array( Len( ::aRow ) )

   AFill( ::aDirty, .F. )

   RETURN Self


METHOD FieldGet( cnField ) CLASS TMySQLRow

   LOCAL nNum := iif( ISCHARACTER( cnField ), ::FieldPos( cnField ), cnField )

   IF nNum > 0 .AND. nNum <= Len( ::aRow )

      // Char fields are padded with spaces since a real .dbf field would be
      IF ::FieldType( nNum ) == "C"
         RETURN PadR( ::aRow[nNum], ::aFieldStruct[nNum][MYSQL_FS_LENGTH] )
      ELSE
         RETURN ::aRow[nNum]
      ENDIF
   ENDIF

   RETURN NIL


METHOD FieldPut( cnField, Value ) CLASS TMySQLRow

   LOCAL nNum := iif( ISCHARACTER( cnField ), ::FieldPos( cnField ), cnField )

   IF nNum > 0 .AND. nNum <= Len( ::aRow )

      IF Valtype( Value ) == Valtype( ::aRow[nNum] ) .OR. ::aRow[nNum] == NIL

         // if it is a char field remove trailing spaces
         IF ISCHARACTER( Value )
            Value := RTrim( Value )
         ENDIF

         // Save starting value for this field
         IF !::aDirty[nNum]
            ::aOldValue[nNum] := ::aRow[nNum]
            ::aDirty[nNum] := .T.
         ENDIF

         ::aRow[nNum] := Value

         RETURN Value
      ENDIF
   ENDIF

   RETURN NIL


// Given a field name returns it's position
METHOD FieldPos( cFieldName ) CLASS TMySQLRow

   LOCAL cUpperName := Upper( cFieldName )

   RETURN AScan( ::aFieldStruct, {| aItem | Upper( aItem[MYSQL_FS_NAME] ) == cUpperName } )


// Returns name of field N
METHOD FieldName( nNum ) CLASS TMySQLRow

   RETURN iif( nNum >= 1 .AND. nNum <= Len( ::aFieldStruct ), ::aFieldStruct[nNum][MYSQL_FS_NAME], "" )


METHOD FieldLen( nNum ) CLASS TMySQLRow

   RETURN iif( nNum >= 1 .AND. nNum <= Len( ::aFieldStruct ), ::aFieldStruct[nNum][MYSQL_FS_LENGTH], 0 )

/*
   lFormat: when .T. method returns number of formatted decimal places from mysql table otherwise _SET_DECIMALS.
   lFormat is usefull for copying table structure from mysql to dbf
*/
METHOD FieldDec( nNum, lFormat ) CLASS TMySQLRow

   DEFAULT lFormat TO .F.

   IF nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )

       IF !lFormat .AND. ( ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR. ;
                           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE )
         RETURN Set( _SET_DECIMALS )
      ELSE
         RETURN ::aFieldStruct[nNum][MYSQL_FS_DECIMALS]
      ENDIF
   ENDIF

   RETURN 0


METHOD FieldType( nNum ) CLASS TMySQLRow

   LOCAL cType := "U"

   IF nNum >=1 .AND. nNum <= Len( ::aFieldStruct )

      DO CASE
      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
         cType := "L"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
         cType := "N"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
         cType := "D"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
         cType := "M"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
         cType := "C"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE
         cType := "N"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
         cType := "M"

      OTHERWISE
         cType := "U"

      ENDCASE
   ENDIF

   RETURN cType


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLRow

   LOCAL ni, cWhere := " WHERE "

   FOR nI := 1 TO Len( ::aFieldStruct )

      // search for fields part of a primary key
      IF hb_bitAnd( ::aFieldStruct[nI][MYSQL_FS_FLAGS], PRI_KEY_FLAG ) == PRI_KEY_FLAG .OR. ;
         hb_bitAnd( ::aFieldStruct[nI][MYSQL_FS_FLAGS], MULTIPLE_KEY_FLAG ) == MULTIPLE_KEY_FLAG

         cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="

         // if a part of a primary key has been changed, use original value
         IF ::aDirty[nI]
            cWhere += ClipValue2SQL( ::aOldValue[nI] )
         ELSE
            cWhere += ClipValue2SQL( ::aRow[nI] )
         ENDIF

         cWhere += " AND "
      ENDIF

   NEXT

   // remove last " AND "
   cWhere := Left( cWhere, Len( cWhere ) - 5 )

   RETURN cWhere

/* ----------------------------------------------------------------------------------------*/

// Every single query submitted to MySQL server
CREATE CLASS TMySQLQuery

   DATA  nSocket           // connection handle to MySQL server
   DATA  nResultHandle     // result handle received from MySQL

   DATA  cQuery            // copy of query that generated this object

   DATA  nNumRows          // number of rows available on answer NOTE MySQL is 0 based
   DATA  nCurRow           // I'm currently over row number

   //DAVID:
   DATA  lBof
   DATA  lEof

   //DAVID:
   DATA  lFieldAsData      //Use fields as object DATA. For compatibility
                           //Names of fields can match name of TMySQLQuery/Table DATAs,
                           //and it is dangerous. ::lFieldAsData:=.F. can fix it
   DATA  aRow              //Values of fields of current row

   DATA  nNumFields        // how many fields per row
   DATA  aFieldStruct      // type of each field, a copy is here a copy inside each row

   DATA  lError            // .T. if last operation failed

   METHOD   New( nSocket, cQuery )       // New query object
   METHOD   Destroy()
   METHOD   End() INLINE ::Destroy()
   METHOD   Refresh()                  // ReExecutes the query (cQuery) so that changes to table are visible

   METHOD   GetRow( nRow )               // return Row n of answer

   METHOD   Skip( nRows )                // Same as clipper ones

   METHOD   Bof() INLINE ::lBof    //DAVID:  ::nCurRow == 1
   METHOD   Eof() INLINE ::lEof    //DAVID:  ::nCurRow == ::nNumRows
   METHOD   RecNo() INLINE ::nCurRow
   METHOD   LastRec() INLINE ::nNumRows
   METHOD   GoTop() INLINE ::GetRow( 1 )
   METHOD   GoBottom() INLINE ::GetRow( ::nNumRows )
   METHOD   GoTO( nRow ) INLINE ::GetRow( nRow )

   METHOD   FCount()

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                           // Returns textual description of last error and clears ::lError

   METHOD   FieldName( nNum )
   METHOD   FieldPos( cFieldName )
   METHOD   FieldGet( cnField )

   METHOD   FieldLen( nNum )             // Length of field N
   METHOD   FieldDec( nNum, lFormat )    // How many decimals in field N
   METHOD   FieldType( nNum )            // Clipper type of field N

ENDCLASS


METHOD New( nSocket, cQuery ) CLASS TMySQLQuery

   LOCAL nI, aField

   ::nSocket := nSocket
   ::cQuery := cQuery

   ::lError := .F.
   ::aFieldStruct := {}
   ::nCurRow := 1
   ::nResultHandle := NIL
   ::nNumFields := 0
   ::nNumRows := 0
   //DAVID:
   ::lBof := .T.
   ::lEof := .T.

   ::lFieldAsData := .T.     //Use fields as object DATA. For compatibility
   ::aRow := {}              //Values of fields of current row

   IF mysql_query( nSocket, cQuery ) == 0

      // save result set
      IF !Empty( ::nResultHandle := mysql_store_result( nSocket ) )

         ::nNumRows := mysql_num_rows( ::nResultHandle )
         ::nNumFields := mysql_num_fields( ::nResultHandle )
         //DAVID:
         ::aRow := Array( ::nNumFields )

         FOR nI := 1 TO ::nNumFields

            aField := mysql_fetch_field( ::nResultHandle )
            AAdd( ::aFieldStruct, aField )
            //DAVID:
            IF ::lFieldAsData
               __ObjAddData( Self, ::aFieldStruct[nI][MYSQL_FS_NAME] )
            ENDIF

         NEXT

         ::getRow( ::nCurRow )

      ELSE
         // Should query have returned rows? (Was it a SELECT like query?)

         IF ( ::nNumFields := mysql_num_fields( nSocket ) ) == 0

            // Was not a SELECT so reset ResultHandle changed by previous mysql_store_result()
            ::nResultHandle := NIL

         ELSE
            ::lError := .T.

         ENDIF
      ENDIF

   ELSE
      ::lError := .T.
   ENDIF

   RETURN Self


METHOD Refresh() CLASS TMySQLQuery

   // free present result handle
   mysql_free_result( ::nResultHandle )

   ::lError := .F.

   IF mysql_query( ::nSocket, ::cQuery ) == 0

      // save result set
      ::nResultHandle := mysql_store_result( ::nSocket )
      ::nNumRows := mysql_num_rows( ::nResultHandle )

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      IF ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      ENDIF

      ::getRow( ::nCurRow )

   ELSE
    /*  ::aFieldStruct := {}
      ::nResultHandle := NIL
      ::nNumFields := 0
      ::nNumRows := 0
      */
      ::lError := .T.

   ENDIF

   RETURN !::lError


METHOD Skip( nRows ) CLASS TMySQLQuery
//DAVID:
   LOCAL lbof

   // NOTE: MySQL row count starts from 0
   DEFAULT nRows TO 1

   //DAVID:
   ::lBof := ( Empty( ::LastRec() ) )

   IF nRows == 0
      // No move

   ELSEIF nRows < 0
      // Negative movement
      //DAVID: ::nCurRow := Max( ::nCurRow + nRows, 1 )
      IF ( ( ::recno() + nRows ) + 0 ) < 1
         nRows := - ::recno() + 1
         //Clipper: only SKIP movement can set BOF() to .T.
         ::lBof := .T.  //Try to skip before first record
      ENDIF
   ELSE
      // positive movement
      //DAVID: ::nCurRow := Min( ::nCurRow + nRows, ::nNumRows )
      IF ( ( ::recno() + nRows ) + 0 ) > ::lastrec()
         nRows := ::lastrec() - ::recno() + 1
      ENDIF
   ENDIF

   //DAVID:
   ::nCurRow := ::nCurRow + nRows

   //DAVID: maintain ::bof() true until next movement
   //Clipper: only SKIP movement can set BOF() to .T.
   lbof := ::bof()

//   mysql_data_seek( ::nResultHandle, ::nCurRow - 1 )
   ::getRow( ::nCurrow )

   IF lbof
      ::lBof := .T.
   ENDIF

//DAVID: DBSKIP() RETURN NIL  RETURN ::nCurRow
   RETURN NIL


/* Given a three letter month name gives back month number as two char string (ie. Apr -> 04) */
STATIC FUNCTION NMonth( cMonthValue )

   STATIC s_cMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec" }

   RETURN PadL( AScan( s_cMonths, cMonthValue ), 2, "0" )


// Get row n of a query and return it as a TMySQLRow object
METHOD GetRow( nRow ) CLASS TMySQLQuery

   //DAVID: replaced by ::aRow   LOCAL aRow := NIL
   LOCAL oRow := NIL
   LOCAL i

   //DAVID: use current row  DEFAULT nRow TO 0
   DEFAULT nRow TO ::nCurRow

   IF ::nResultHandle != NIL

      //DAVID:
      ::lBof := ( Empty( ::LastRec() ) )

      IF nRow < 1 .OR. nRow > ::lastrec()  //Out of range
         // Equal to Clipper behaviour
         nRow := ::lastrec() + 1  //LASTREC()+1
         ::nCurRow := ::lastrec() + 1
         // ::lEof := .T.
      ENDIF

      IF nRow >= 1 .AND. nRow <= ::nNumRows

         // NOTE: row count starts from 0
         mysql_data_seek( ::nResultHandle, nRow - 1 )
         ::nCurRow := nRow
//DAVID:      ELSE
         //DAVID: use current row  ::nCurRow++
      ENDIF

      //DAVID:
      ::lEof := ( ::Recno() > ::LastRec() )
      ::aRow := NIL

      IF ::eof()
         // Phantom record with empty fields
         ::aRow := Array( Len( ::aFieldStruct ) )
         AFill( ::aRow, "" )
      ELSE
         ::aRow := mysql_fetch_row( ::nResultHandle )
      ENDIF

      IF ::aRow != NIL

         // Convert answer from text field to correct clipper types
         FOR i := 1 TO ::nNumFields
            DO CASE
            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
               //DAVID:
               IF ::aRow[i] == NIL
                  ::aRow[i] := "0"
               ENDIF
               ::aRow[i] := Val( ::aRow[i] ) != 0

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE .OR. ;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
               //DAVID:
               IF ::aRow[i] == NIL
                  ::aRow[i] := "0"
               ENDIF
               ::aRow[i] := Val( ::aRow[i] )

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
               //DAVID:
               IF ::aRow[i] == NIL
                  ::aRow[i] := "0"
               ENDIF
               ::aRow[i] := Val( ::aRow[i] )

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
               IF Empty( ::aRow[i] )
                  ::aRow[i] := hb_SToD( "" )
               ELSE
                  // Date format YYYY-MM-DD
                  ::aRow[i] := hb_SToD( Left( ::aRow[i], 4 ) + SubStr( ::aRow[i], 6, 2 ) + Right( ::aRow[i], 2 ) )
               ENDIF

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
               // Memo field

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE .OR.;
                 ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE
               // char field

            CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
               // DateTime field

            OTHERWISE

               //DAVID: Alert( "Unknown type from SQL Server Field: " + hb_NToS( i ) + " is type " + hb_NToS( ::aFieldStruct[i][MYSQL_FS_TYPE] ) )
               // QOUT( "Unknown type from SQL Server Field: " + hb_NToS( i ) + " is type " + hb_NToS( ::aFieldStruct[i][MYSQL_FS_TYPE] ) )

            ENDCASE

            //DAVID:
            IF ::lFieldAsData
               __objsetValuelist( Self, { { ::aFieldStruct[i][MYSQL_FS_NAME], ::aRow[i] } } )
            ENDIF

         NEXT

         oRow := TMySQLRow():New( ::aRow, ::aFieldStruct )

      ENDIF

   ENDIF
  //DAVID: IF ::arow == NIL; msginfo( "::arow NIL" ); ENDIF

   RETURN iif( ::aRow == NIL, NIL, oRow )


// Free result handle and associated resources
METHOD Destroy() CLASS TMySQLQuery

   mysql_free_result( ::nResultHandle )

   RETURN Self


METHOD FCount() CLASS TMySQLQuery

   RETURN ::nNumFields


METHOD Error() CLASS TMySQLQuery

   ::lError := .F.

   RETURN mysql_error( ::nSocket )

// Given a field name returns it's position
METHOD FieldPos( cFieldName ) CLASS TMySQLQuery

   LOCAL cUpperName, nPos

   cUpperName := Upper( cFieldName )

   //DAVID: nPos := AScan( ::aFieldStruct, {|aItem| iif( Upper( aItem[MYSQL_FS_NAME] ) == cUpperName, .T., .F. ) } )
   nPos := AScan( ::aFieldStruct, {| aItem | Upper( aItem[MYSQL_FS_NAME]) == cUpperName } )

   /*
   nPos := 0
   DO WHILE ++nPos <= Len( ::aFieldStruct )
      IF Upper( ::aFieldStruct[nPos][MYSQL_FS_NAME] ) == cUpperName
         EXIT
      ENDIF
   ENDDO

   // I haven't found field name
   IF nPos > Len( ::aFieldStruct )
      nPos := 0
   ENDIF
   */

   RETURN nPos


// Returns name of field N
METHOD FieldName( nNum ) CLASS TMySQLQuery

   IF nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      RETURN ::aFieldStruct[nNum][MYSQL_FS_NAME]
   ENDIF

   RETURN ""

METHOD FieldGet( cnField ) CLASS TMySQLQuery

   LOCAL nNum, Value

   IF ISCHARACTER( cnField )
      nNum := ::FieldPos( cnField )
   ELSE
      nNum := cnField
   ENDIF

   IF nNum > 0 .AND. nNum <= ::nNumfields
      //DAVID: Value := __objsendmsg( Self,::aFieldStruct[nNum][MYSQL_FS_NAME] )
      Value := ::aRow[ nNum ]

      // Char fields are padded with spaces since a real .dbf field would be
      IF ::FieldType( nNum ) == "C"
         RETURN PadR( Value,::aFieldStruct[nNum][MYSQL_FS_LENGTH] )
      ELSE
         RETURN Value
      ENDIF
   ENDIF

   RETURN NIL


METHOD FieldLen( nNum ) CLASS TMySQLQuery

   IF nNum >=1 .AND. nNum <= Len( ::aFieldStruct )
      RETURN ::aFieldStruct[nNum][MYSQL_FS_LENGTH]
   ENDIF

   RETURN 0


/*
   lFormat: when .T. method returns number of formatted decimal places from mysql table otherwise _SET_DECIMALS.
   lFormat is usefull for copying table structure from mysql to dbf
*/
METHOD FieldDec( nNum, lFormat ) CLASS TMySQLQuery

   DEFAULT lFormat TO .F.

   IF nNum >=1 .AND. nNum <= Len( ::aFieldStruct )
      IF !lFormat .AND. ( ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR. ;
                          ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE )
         RETURN Set( _SET_DECIMALS )
      ELSE
         RETURN ::aFieldStruct[nNum][MYSQL_FS_DECIMALS]
      ENDIF
   ENDIF

   RETURN 0


METHOD FieldType( nNum ) CLASS TMySQLQuery

   LOCAL cType := "U"

   IF nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      DO CASE
      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
         cType := "L"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE.OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
         cType := "N"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
         cType := "D"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
         cType := "M"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR.;
           ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
         cType := "C"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE
         cType := "N"

      CASE ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
         cType := "M"

      OTHERWISE
         cType := "U"

      ENDCASE
   ENDIF

   RETURN cType


/* ----------------------------------------------------------------------------------------*/

// A Table is a query without joins; this way I can Insert() e Delete() rows.
// NOTE: it's always a SELECT result, so it will contain a full table only if
//       SELECT * FROM ... was issued
CREATE CLASS TMySQLTable FROM TMySQLQuery

   DATA  cTable               // name of table
   DATA  aOldValue         //  keeps a copy of old value

   METHOD   New( nSocket, cQuery, cTableName )
   METHOD   GetRow( nRow )
   METHOD   Skip( nRow )
   METHOD   GoTop() INLINE ::GetRow( 1 )
   METHOD   GoBottom() INLINE ::GetRow( ::nNumRows )
   METHOD   GoTo( nRow ) INLINE ::GetRow( nRow )

   //DAVID: lOldRecord, lrefresh added
   METHOD   Update( oRow, lOldRecord, lRefresh )      // Gets an oRow and updates changed fields

   METHOD   Save() INLINE ::Update()

   //DAVID: lOldRecord, lRefresh added
   METHOD   Delete( oRow, lOldRecord, lRefresh )      // Deletes passed row from table
   //DAVID: lRefresh added
   METHOD   Append( oRow, lRefresh )      // Inserts passed row into table
   //DAVID: lSetValues added
   METHOD   GetBlankRow( lSetValues )     // Returns an empty row with all available fields empty
   //DAVID:
   METHOD   SetBlankRow() INLINE ::GetBlankRow( .T. )    // Compatibility

   METHOD   Blank() INLINE ::GetBlankRow()
   METHOD   FieldPut( cnField, Value )   // field identifier, not only a number
   METHOD   Refresh()
   METHOD   MakePrimaryKeyWhere()    // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New( nSocket, cQuery, cTableName ) CLASS TMySQLTable

   LOCAL i

   super:New( nSocket, AllTrim( cQuery ) )

   ::cTable := Lower( cTableName )
   ::aOldValue := {}

   FOR i := 1 TO ::nNumFields
      AAdd( ::aOldValue, ::fieldget( i ) )
   NEXT

   RETURN Self


METHOD GetRow( nRow ) CLASS TMySQLTable

   LOCAL oRow := super:GetRow( nRow ), i

   IF oRow != NIL
      oRow:cTable := ::cTable
   ENDIF

   ::aOldvalue:={}
   FOR i := 1 TO ::nNumFields
      // ::aOldValue[i] := ::FieldGet( i )
      AAdd( ::aOldvalue, ::fieldget( i ) )
   NEXT

   RETURN oRow


METHOD Skip( nRow ) CLASS TMySQLTable
   LOCAL i

   super:skip( nRow )

   FOR i := 1 TO ::nNumFields
      ::aOldValue[i] := ::FieldGet( i )
   NEXT

//DAVID: DBSKIP() RETURN NIL  RETURN Self
   RETURN NIL


/* Creates an update query for changed fields and submits it to server */
//DAVID: lOldRecord, lRefresh added
METHOD Update( oRow, lOldRecord, lRefresh ) CLASS TMySQLTable

   LOCAL cUpdateQuery := "UPDATE " + ::cTable + " SET "
   LOCAL i
   //DAVID:
   LOCAL ni, cWhere := " WHERE "
   DEFAULT lOldRecord TO .F.
   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   DEFAULT lRefresh TO .T.

   ::lError := .F.

   DO CASE
   CASE oRow == NIL // default Current row

      FOR i := 1 TO  ::nNumFields

         IF !( ::aOldValue[i] == ::FieldGet( i ) )
            cUpdateQuery += ::aFieldStruct[i][MYSQL_FS_NAME] + "=" + ClipValue2SQL( ::FieldGet( i ) ) + ","
         ENDIF
      NEXT

      // no Change
      IF Right( cUpdateQuery, 4 ) == "SET "
         RETURN !::lError
      ENDIF

      // remove last comma
      cUpdateQuery := Left( cUpdateQuery, Len( cUpdateQuery ) - 1 )

      //DAVID:
      IF lOldRecord
         // based in matching of ALL fields of old record
         // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

         FOR nI := 1 TO Len( ::aFieldStruct )
            cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="
            // use original value
            cWhere += ClipValue2SQL( ::aOldValue[nI] )
            cWhere += " AND "
         NEXT
         // remove last " AND "
         cWhere := Left( cWhere, Len( cWhere ) - 5 )
         cUpdateQuery += cWhere

      ELSE
         //MakePrimaryKeyWhere is based in fields part of a primary key
         cUpdateQuery += ::MakePrimaryKeyWhere()
      ENDIF

      IF mysql_query( ::nSocket, cUpdateQuery ) == 0
         //DAVID: Clipper maintain same record pointer

         //DAVID: after refresh(), position of current record is often unpredictable
         IF lRefresh
            ::refresh()
         ELSE
            //DAVID: just reset values (?)
            FOR i := 1 TO ::nNumFields
               ::aOldValue[i] := ::FieldGet( i )
            NEXT
         ENDIF

      ELSE
         ::lError := .T.
      ENDIF

   CASE oRow != NIL

      IF oRow:cTable == ::cTable

         FOR i := 1 TO Len( oRow:aRow )
            IF oRow:aDirty[i]
               cUpdateQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + "=" + ClipValue2SQL( oRow:aRow[i] ) + ","
            ENDIF
         NEXT

         // remove last comma
         cUpdateQuery := Left( cUpdateQuery, Len( cUpdateQuery ) -1 )

         //DAVID:
         IF lOldRecord
            // based in matching of ALL fields of old record
            // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

            FOR nI := 1 TO Len( oRow:aFieldStruct )
               cWhere += oRow:aFieldStruct[nI][MYSQL_FS_NAME] + "="
               // use original value
               cWhere += ClipValue2SQL( oRow:aOriValue[nI] )
               cWhere += " AND "
            NEXT
            // remove last " AND "
            cWhere := Left( cWhere, Len( cWhere ) - 5 )
            cUpdateQuery += cWhere

         ELSE
            //MakePrimaryKeyWhere is based in fields part of a primary key
            cUpdateQuery += oRow:MakePrimaryKeyWhere()
         ENDIF

         IF mysql_query( ::nSocket, cUpdateQuery ) == 0

            // All values are commited
            Afill( oRow:aDirty, .F. )
            Afill( oRow:aOldValue, NIL )

            //DAVID:
            oRow:aOriValue := AClone( oRow:aRow )

            //DAVID: Clipper maintain same record pointer

            //DAVID: after refresh(), position of current record is often unpredictable
            IF lRefresh
               ::refresh()
            ENDIF

         ELSE
            ::lError := .T.
         ENDIF
      ENDIF
   ENDCASE

   RETURN !::lError


//DAVID: lOldRecord, lRefresh added
METHOD Delete( oRow, lOldRecord, lRefresh ) CLASS TMySQLTable

   LOCAL cDeleteQuery := "DELETE FROM " + ::cTable , i

   //DAVID:
   LOCAL ni, cWhere := " WHERE "
   DEFAULT lOldRecord TO .F.
   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   DEFAULT lRefresh TO .T.

   // is this a row of this table ?
   DO CASE
   CASE orow == NIL

      //DAVID:
      IF lOldRecord
         // based in matching of ALL fields of old record
         // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

         FOR nI := 1 TO Len( ::aFieldStruct )
            cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="
            // use original value
            cWhere += ClipValue2SQL( ::aOldValue[nI] )
            cWhere += " AND "
         NEXT
         // remove last " AND "
         cWhere := Left( cWhere, Len( cWhere ) - 5 )
         cDeleteQuery += cWhere

      ELSE
         //MakePrimaryKeyWhere is based in fields part of a primary key
         cDeleteQuery += ::MakePrimaryKeyWhere()
      ENDIF

      IF mysql_query( ::nSocket, cDeleteQuery ) == 0
         ::lError := .F.
         //DAVID: Clipper maintain same record pointer
         //DAVID: ::nCurRow--

         //DAVID: after refresh(), position of current record is often unpredictable
         IF lRefresh
            ::refresh()
         ELSE
            //DAVID: just reset values (?)
            FOR i := 1 TO ::nNumFields
                ::aOldValue[i] := ::FieldGet( i )
            NEXT
         ENDIF
      ELSE
         ::lError := .T.
      ENDIF

   CASE oRow != NIL
      IF oRow:cTable == ::cTable

         //DAVID:
         IF lOldRecord
            // based in matching of ALL fields of old record
            // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

            FOR nI := 1 TO Len( oRow:aFieldStruct )
               cWhere += oRow:aFieldStruct[nI][MYSQL_FS_NAME] + "="
               // use original value
               cWhere += ClipValue2SQL( oRow:aOriValue[nI] )
               cWhere += " AND "
            NEXT
            // remove last " AND "
            cWhere := Left( cWhere, Len( cWhere ) - 5 )
            cDeleteQuery += cWhere

         ELSE
            //MakePrimaryKeyWhere is based in fields part of a primary key
            cDeleteQuery += oRow:MakePrimaryKeyWhere()
         ENDIF

         IF mysql_query( ::nSocket, cDeleteQuery ) == 0
            ::lError := .F.

            //DAVID: after refresh(), position of current record is often unpredictable
            IF lRefresh
               ::refresh()
            ENDIF

         ELSE
            ::lError := .T.
         ENDIF
      ENDIF
   ENDCASE

   RETURN !::lError


// Adds a row with values passed into oRow
//DAVID: lRefresh added
METHOD Append( oRow, lRefresh ) CLASS TMySQLTable

   LOCAL cInsertQuery := "INSERT INTO " + ::cTable + " ("
   LOCAL i

   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   DEFAULT lRefresh TO .T.

   DO CASE
   CASE oRow == NIL // default Current row

      // field names
      FOR i := 1 TO ::nNumFields
         IF ::aFieldStruct[i][MYSQL_FS_FLAGS] != AUTO_INCREMENT_FLAG
            cInsertQuery += ::aFieldStruct[i][MYSQL_FS_NAME] + ","
         ENDIF
      NEXT
      // remove last comma from list
      cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ") VALUES ("

      // field values
      FOR i := 1 TO ::nNumFields
         IF ::aFieldStruct[i][MYSQL_FS_FLAGS] != AUTO_INCREMENT_FLAG
            cInsertQuery += ClipValue2SQL( ::FieldGet( i ) ) + ","
         ENDIF
      NEXT

      // remove last comma from list of values and add closing parenthesis
      cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ")"

      IF mysql_query( ::nSocket, cInsertQuery ) == 0
         ::lError := .F.
         //DAVID: Clipper add record at end
         ::nCurRow := ::lastrec() + 1

         //DAVID: after refresh(), position of current record is often unpredictable
         IF lRefresh
            ::refresh()
         ELSE
            //DAVID: just reset values in memory (?)
            /* was same values from fieldget( i ) !
            FOR i := 1 TO ::nNumFields
                ::aOldValue[i] := ::FieldGet( i )
            NEXT
            */
         ENDIF

         RETURN .T.
      ELSE
         ::lError := .T.
      ENDIF

   CASE oRow != NIL

      IF oRow:cTable == ::cTable

         // field names
         FOR i := 1 TO Len( oRow:aRow )
            IF oRow:aFieldStruct[i][MYSQL_FS_FLAGS] != AUTO_INCREMENT_FLAG
               cInsertQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + ","
            ENDIF
         NEXT
         // remove last comma from list
         cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ") VALUES ("

         // field values
         FOR i := 1 TO Len( oRow:aRow )
            IF oRow:aFieldStruct[i][MYSQL_FS_FLAGS] != AUTO_INCREMENT_FLAG
               cInsertQuery += ClipValue2SQL( oRow:aRow[i] ) + ","
            ENDIF
         NEXT

         // remove last comma from list of values and add closing parenthesis
         cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ")"

         IF mysql_query( ::nSocket, cInsertQuery ) == 0
            //DAVID:
            ::lError := .F.

            //DAVID:
            // All values are commited
            AFill( oRow:aDirty, .F. )
            AFill( oRow:aOldValue, NIL )

            //DAVID:
            oRow:aOriValue := AClone( oRow:aRow )

            //DAVID: Clipper add record at end
            ::nCurRow := ::lastrec() + 1

            //DAVID: after refresh(), position of current record is often unpredictable
            IF lRefresh
               ::refresh()
            ENDIF

            RETURN .T.
         ELSE
            ::lError := .T.
         ENDIF
      ENDIF

   ENDCASE

   RETURN .F.


//DAVID: lSetValues added  METHOD GetBlankRow() CLASS TMySQLTable
METHOD GetBlankRow( lSetValues ) CLASS TMySQLTable

   LOCAL i
   LOCAL aRow := Array( ::nNumFields )

   //DAVID: It is not current row, so do not change it
   DEFAULT lSetValues TO .F.

   // crate an array of empty fields
   FOR i := 1 TO ::nNumFields

      DO CASE
      CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE       .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
         aRow[i] := ""

      CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE      .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
         aRow[i] := 0

      CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
         aRow[i] := .F.

      CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
         aRow[i] := 0.0

      CASE ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
         aRow[i] := hb_SToD( "" )

      OTHERWISE
         aRow[i] := NIL

      ENDCASE
   NEXT

   //DAVID:
   IF lSetValues   //Assign values as current row values
      FOR i := 1 TO ::nNumFields
         ::FieldPut( i, aRow[i] )
         ::aOldValue[i] := aRow[i]
      NEXT
   ENDIF

   RETURN TMySQLRow():New( aRow, ::aFieldStruct, ::cTable, .F. )


METHOD FieldPut( cnField, Value ) CLASS TMySQLTable

   LOCAL nNum

   IF ISCHARACTER( cnField )
      nNum := ::FieldPos( cnField )
   ELSE
      nNum := cnField
   ENDIF

   IF nNum > 0 .AND. nNum <= ::nNumFields

//DAVID:      IF Valtype( Value ) == Valtype( ::FieldGet( nNum ) ) .OR. Empty( ::Fieldget( nNum ) )
      IF Valtype( Value ) == Valtype( ::aRow[nNum] ) .OR. ::aRow[nNum] == NIL

         // if it is a char field remove trailing spaces
         IF ISCHARACTER( Value )
            Value := RTrim( Value )
         ENDIF

         //DAVID:
         ::aRow[ nNum ] := Value
         IF ::lFieldAsData
            __objsetValueList( Self, { { ::aFieldStruct[nNum][MYSQL_FS_NAME], Value } } )
         ENDIF

         RETURN Value
      ENDIF
   ENDIF

   RETURN NIL


METHOD Refresh() CLASS TMySQLTABLE

   // free present result handle
   mysql_free_result( ::nResultHandle )

   ::lError := .F.

   IF mysql_query( ::nSocket, ::cQuery ) == 0

      // save result set
      ::nResultHandle := mysql_store_result( ::nSocket )
      ::nNumRows := mysql_num_rows( ::nResultHandle )

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      IF ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      ENDIF

      ::getRow( ::nCurRow )

   ELSE
/*      ::aFieldStruct := {}
      ::nResultHandle := NIL
      ::nNumFields := 0
      ::nNumRows := 0

      ::aOldValue := {}
      */
      ::lError := .T.
   ENDIF

   RETURN !::lError


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLTable

   LOCAL ni, cWhere := " WHERE "

   FOR nI := 1 TO Len( ::aFieldStruct )

      // search for fields part of a primary key
      IF hb_bitAnd( ::aFieldStruct[nI][MYSQL_FS_FLAGS], PRI_KEY_FLAG ) == PRI_KEY_FLAG .OR.;
         hb_bitAnd( ::aFieldStruct[nI][MYSQL_FS_FLAGS], MULTIPLE_KEY_FLAG ) == MULTIPLE_KEY_FLAG

         cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="

         // if a part of a primary key has been changed, use original value

            cWhere += ClipValue2SQL( ::aOldValue[nI] )

         cWhere += " AND "
      ENDIF

   NEXT

   // remove last " AND "
   cWhere := Left( cWhere, Len( cWhere ) - 5 )

   RETURN cWhere



/* ----------------------------------------------------------------------------------------*/

// Every available MySQL server
CREATE CLASS TMySQLServer

   DATA  nSocket                 // connection handle to server (currently pointer to a MYSQL structure)
   DATA  cServer                 // server name
   DATA  cDBName                 // Selected DB
   DATA  cUser                   // user accessing db
   DATA  cPassword               // his/her password
   DATA  lError                  // .T. if occurred an error
   DATA  cCreateQuery

   METHOD   New( cServer, cUser, cPassword )   // Opens connection to a server, returns a server object
   METHOD   Destroy()                         // Closes connection to server

   METHOD   SelectDB( cDBName )    // Which data base I will use for subsequent queries

   METHOD   CreateTable( cTable, aStruct, cPrimaryKey, cUniqueKey, cAuto )  // Create new table using the same syntax of dbCreate()
   METHOD   DeleteTable( cTable )           // delete table
   METHOD   TableStruct( cTable )           // returns a structure array compatible with clipper's dbStruct() ones
   METHOD   CreateIndex( cName, cTable, aFNames, lUnique ) // Create an index (unique) on field name(s) passed as an array of strings aFNames
   METHOD   DeleteIndex( cName, cTable )                   // Delete index cName from cTable

   METHOD   ListDBs()            // returns an array with list of data bases available
   METHOD   ListTables()         // returns an array with list of available tables in current database

   METHOD   Query( cQuery )      // Gets a textual query and returns a TMySQLQuery or TMySQLTable object

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error
   METHOD   CreateDatabase( cDataBase )      // Create an New Mysql Database
//Mitja
   METHOD   sql_Commit()      // Commits transaction
   METHOD   sql_Rollback()    // Rollbacks transaction
   METHOD   sql_Version()     // server version as numeric
ENDCLASS


METHOD New( cServer, cUser, cPassword ) CLASS TMySQLServer

   ::cServer := cServer
   ::cUser := cUser
   ::cPassword := cPassword
   ::nSocket := mysql_real_connect( cServer, cUser, cPassword )
   ::lError := .F.

   IF Empty( ::nSocket )
      ::lError := .T.
   ENDIF

   RETURN Self

METHOD Destroy() CLASS TMySQLServer
   mysql_close( ::nSocket )
   RETURN Self

METHOD sql_commit() CLASS TMySQLServer
   RETURN mysql_commit( ::nSocket ) == 0

METHOD sql_rollback() CLASS TMySQLServer
   RETURN mysql_rollback( ::nSocket ) == 0

METHOD sql_version() CLASS TMySQLServer
   RETURN mysql_get_server_version( ::nSocket )



*METHOD SelectDB( cDBName ) CLASS TMySQLServer
*
*   IF mysql_select_db( ::nSocket, cDBName ) == 0
*      ::cDBName := cDBName
*      RETURN .T.
*   ELSE
*      ::cDBName := ""
*   ENDIF
*
*   RETURN .F.


*****************alterado
METHOD SelectDB( cDBName ) CLASS TMySQLServer

   ::lError := .F.

   IF mysql_select_db( ::nSocket, cDBName ) != 0     /* tabela nao existe */
      ::cDBName := ""
      ::lError := .T.
   ELSE                                       /* tabela existe */
      ::cDBName := cDBName
      ::lError := .F.
      RETURN .T.
   ENDIF

   RETURN .F.


METHOD CreateDatabase( cDataBase ) CLASS TMySQLServer
   LOCAL cCreateQuery := "CREATE DATABASE " + Lower( cDatabase )

   IF mysql_query( ::nSocket, cCreateQuery ) == 0
      RETURN .T.
   ENDIF

   RETURN .F.


// NOTE: OS/2 port of MySQL is picky about table names, that is if you create a table with
// an upper case name you cannot alter it (for example) using a lower case name, this violates
// OS/2 case insensibility about names
METHOD CreateTable( cTable, aStruct, cPrimaryKey, cUniqueKey, cAuto ) CLASS TMySQLServer

   /* NOTE: all table names are created with lower case */

   LOCAL i

   // returns NOT NULL if extended structure has DBS_NOTNULL field to true
   LOCAL cNN := {| aArr | iif( Len( aArr ) > DBS_DEC, iif( aArr[DBS_NOTNULL], " NOT NULL ", "" ), "" ) }

   ::cCreateQuery := "CREATE TABLE " + Lower( cTable ) + " ("

   FOR i := 1 TO Len( aStruct )
      DO CASE
      CASE aStruct[i][DBS_TYPE] == "C"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " char(" + hb_NToS( aStruct[i][DBS_LEN]) + ")" + Eval( cNN, aStruct[i] ) + iif( aStruct[i][DBS_NAME] == cPrimaryKey, " NOT NULL ", "" ) + ","

      CASE aStruct[i][DBS_TYPE] == "M"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " text" + Eval( cNN, aStruct[i] ) + ","

      CASE aStruct[i][DBS_TYPE] == "N"
         /*
         IF aStruct[i][DBS_DEC] == 0
            ::cCreateQuery += aStruct[i][DBS_NAME] + " int(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")" + Eval( cNN, aStruct[i] ) + iif( aStruct[i][DBS_NAME] == cPrimaryKey, " NOT NULL ", "" ) + iif( aStruct[i][DBS_NAME] == cAuto, " auto_increment ", "" ) + ","
         ELSE
            ::cCreateQuery += aStruct[i][DBS_NAME] + " real(" + hb_NToS( aStruct[i][DBS_LEN] ) + "," + hb_NToS( aStruct[i][DBS_DEC] ) + ")" + Eval( cNN, aStruct[i] ) + ","
         ENDIF
         */
         IF aStruct[i][DBS_DEC] == 0 .AND. aStruct[i][DBS_LEN] <= 18
            DO CASE
            CASE aStruct[i][DBS_LEN] <= 4
               ::cCreateQuery += aStruct[i][DBS_NAME] + " smallint(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")"
            CASE aStruct[i][DBS_LEN] <= 6
               ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumint(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")"
            CASE aStruct[i][DBS_LEN] <= 9
               ::cCreateQuery += aStruct[i][DBS_NAME] + " int(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")"
            OTHERWISE
               ::cCreateQuery += aStruct[i][DBS_NAME] + " bigint(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")"
            ENDCASE
            ::cCreateQuery += Eval(cNN, aStruct[i]) + iif( aStruct[i][DBS_NAME] == cPrimaryKey, " NOT NULL ", "" ) + iif( aStruct[i][DBS_NAME] == cAuto, " auto_increment ", "" ) + ","
         ELSE
            ::cCreateQuery += aStruct[i][DBS_NAME] + " real(" + hb_NToS( aStruct[i][DBS_LEN] ) + "," + hb_NToS( aStruct[i][DBS_DEC]) + ")" + Eval( cNN, aStruct[i] ) + ","
         ENDIF
      CASE aStruct[i][DBS_TYPE] == "D"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " date " + Eval( cNN, aStruct[i] ) + ","

      CASE aStruct[i][DBS_TYPE] == "L"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " tinyint "  + Eval( cNN, aStruct[i] ) + ","

      CASE aStruct[i][DBS_TYPE] == "B"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumblob "  + Eval( cNN, aStruct[i] ) + ","

      CASE aStruct[i][DBS_TYPE] == "I"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumint " + Eval( cNN, aStruct[i] ) + ","

      OTHERWISE
         ::cCreateQuery += aStruct[i][DBS_NAME] + " char(" + hb_NToS( aStruct[i][DBS_LEN] ) + ")" + Eval( cNN, aStruct[i] ) + ","

      ENDCASE

   NEXT
   IF cPrimarykey != NIL
      ::cCreateQuery += ' PRIMARY KEY (' + cPrimaryKey + '),'
   ENDIF
   IF cUniquekey != NIL
      ::cCreateQuery += ' UNIQUE ' + cUniquekey + ' (' + cUniqueKey + '),'
   ENDIF

   // remove last comma from list
   ::cCreateQuery := Left( ::cCreateQuery, Len( ::cCreateQuery ) - 1 ) + ");"
   IF mysql_query( ::nSocket, ::cCreateQuery ) == 0
      RETURN .T.
   ELSE
      ::lError := .T.
   ENDIF

   RETURN .F.


METHOD CreateIndex( cName, cTable, aFNames, lUnique ) CLASS TMySQLServer

   LOCAL cCreateQuery := "CREATE "
   LOCAL i

   DEFAULT lUnique TO .F.

   IF lUnique
      cCreateQuery += "UNIQUE INDEX "
   ELSE
      cCreateQuery += "INDEX "
   ENDIF

   cCreateQuery += cName + " ON " + Lower( cTable ) + " ("

   FOR i := 1 TO Len( aFNames )
      cCreateQuery += aFNames[i] + ","
   NEXT

   // remove last comma from list
   cCreateQuery := Left( cCreateQuery, Len( cCreateQuery ) - 1 ) + ")"

   IF mysql_query( ::nSocket, cCreateQuery ) == 0
      RETURN .T.
   ENDIF

   RETURN .F.


METHOD DeleteIndex( cName, cTable ) CLASS TMySQLServer

   LOCAL cDropQuery := "DROP INDEX " + cName + " FROM " + Lower( cTable )

   IF mysql_query( ::nSocket, cDropQuery ) == 0
      RETURN .T.
   ENDIF

   RETURN .F.


METHOD DeleteTable( cTable ) CLASS TMySQLServer

   LOCAL cDropQuery := "DROP TABLE " + Lower( cTable )

   IF mysql_query( ::nSocket, cDropQuery ) == 0
      RETURN .T.
   ENDIF

   RETURN .F.


METHOD Query( cQuery ) CLASS TMySQLServer

   LOCAL oQuery, cTableName, i, cUpperQuery, nNumTables, cToken

   DEFAULT cQuery TO ""


   cUpperQuery := Upper( AllTrim( cQuery ) )
   i := 1
   nNumTables := 1

   DO WHILE !( ( cToken := __StrToken( cUpperQuery, i++, " " ) ) == "FROM" ) .AND. !Empty( cToken )
   ENDDO

   // first token after "FROM" is a table name
   // NOTE: SubSelects ?
   cTableName := __StrToken( cUpperQuery, i++, " " )

   DO WHILE !( ( cToken := __StrToken( cUpperQuery, i++, " " ) ) == "WHERE" ) .AND. !Empty( cToken )
      // do we have more than one table referenced ?
      IF cToken == "," .OR. cToken == "JOIN"
         nNumTables++
      ENDIF
   ENDDO

   IF nNumTables == 1
      oQuery := TMySQLTable():New( ::nSocket, cQuery, cTableName )
   ELSE
      oQuery := TMySQLQuery():New( ::nSocket, cQuery )
   ENDIF

   IF oQuery:NetErr()
      ::lError := .T.
   ENDIF

   RETURN oQuery


METHOD Error() CLASS TMySQLServer

   ::lError := .F.

   RETURN iif( Empty( ::nSocket ), "No connection to server", mysql_error( ::nSocket ) )


METHOD ListDBs() CLASS TMySQLServer

   RETURN mysql_list_dbs( ::nSocket )


METHOD ListTables() CLASS TMySQLServer

   RETURN mysql_list_tables( ::nSocket )


/* TOFIX: Conversion creates a .dbf with fields of wrong dimension (often) */
METHOD TableStruct( cTable ) CLASS TMySQLServer

   LOCAL aStruct := {}

   HB_SYMBOL_UNUSED( cTable )

   /* TODO: rewrite for MySQL
   LOCAL nRes, aField, aStruct, aSField, i

   aStruct := {}
   nRes := mysql_list_fields( ::nSocket, cTable )

   IF ! Empty( nRes )
      FOR i := 1 TO mysql_num_fields( nRes )

         aField := mysql_fetch_field( nRes )
         aSField := Array( DBS_DEC )

         // don't count indexes as real fields
         IF aField[MSQL_FS_TYPE] <= MSQL_LAST_REAL_TYPE

            aSField[DBS_NAME] := Left( aField[MSQL_FS_NAME], 10 )
            aSField[DBS_DEC] := 0

            DO CASE
            CASE aField[MSQL_FS_TYPE] == MSQL_INT_TYPE
               aSField[DBS_TYPE] := "N"
               aSField[DBS_LEN] := 11

            CASE aField[MSQL_FS_TYPE] == MSQL_UINT_TYPE
               aSField[DBS_TYPE] := "L"
               aSField[DBS_LEN] := 1

            CASE aField[MSQL_FS_TYPE] == MSQL_CHAR_TYPE
               aSField[DBS_TYPE] := "C"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            CASE aField[MSQL_FS_TYPE] == MSQL_DATE_TYPE
               aSField[DBS_TYPE] := "D"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            CASE aField[MSQL_FS_TYPE] == MSQL_REAL_TYPE
               aSField[DBS_TYPE] := "N"
               aSField[DBS_LEN] := 12
               aSFIeld[DBS_DEC] := 8

            CASE aField[MSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
               aSField[DBS_TYPE] := "B"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            CASE aField[MSQL_FS_TYPE] == FIELD_TYPE_INT24
               aSField[DBS_TYPE] := "I"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]
               aSFIeld[DBS_DEC] := aField[MYSQL_FS_DECIMALS]

            ENDCASE

            AAdd( aStruct, aSField )
         ENDIF
      NEXT

      mysql_free_result( nRes )

   ENDIF*/

   RETURN aStruct


// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
STATIC FUNCTION ClipValue2SQL( Value )

   LOCAL cValue

   DO CASE
   CASE ISNUMBER( Value )
      cValue := hb_NToS( Value )

   CASE ISDATE( Value )
      IF ! Empty( Value )
         /* MySQL dates are like YYYY-MM-DD */
         cValue := "'" + StrZero( Year( Value ), 4 ) + "-" + StrZero( Month( Value ), 2 ) + "-" + StrZero( Day( Value ), 2 ) + "'"
      ELSE
         cValue := "''"
      ENDIF

   CASE Valtype( Value ) $ "CM"
      IF Empty( Value )
         cValue := "''"
      ELSE
         cValue := "'"
         Value := mysql_escape_string( value )
         cValue += value + "'"
      ENDIF

   CASE ISLOGICAL( Value )
      cValue := iif( Value, "1", "0" )

   OTHERWISE
      cValue := "''"       // NOTE: Here we lose values we cannot convert

   ENDCASE

   RETURN cValue
