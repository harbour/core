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
#include "mysql.ch"
#include "set.ch"


// Every single row of an answer
CLASS TMySQLRow

   DATA  aRow              // a single row of answer
   DATA  aDirty            // array of booleans set to .T. if corresponding field of aRow has been changed
   DATA  aOldValue         // If aDirty[n] is .T. aOldValue[n] keeps a copy of changed value if aRow[n] is part of a primary key
   //DAVID:
   DATA  aOriValue         // Original values ( same as TMySQLtable:aOldValue )

   DATA  aFieldStruct      // type of each field
   DATA  cTable            // Name of table containing this row, empty if TMySQLQuery returned this row

   METHOD   New(aRow, aFStruct, cTableName)     // Create a new Row object

   METHOD   FieldGet(cnField)          // Same as clipper ones, but FieldGet() and FieldPut() accept a string as
   METHOD   FieldPut(cnField, Value)   // field identifier, not only a number
   METHOD   FieldName(nNum)
   METHOD   FieldPos(cFieldName)

   METHOD   FieldLen(nNum)             // Length of field N
   METHOD   FieldDec(nNum)             // How many decimals in field N
   METHOD   FieldType(nNum)            // Clipper type of field N

   METHOD   MakePrimaryKeyWhere()    // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New(aRow, aFStruct, cTableName) CLASS TMySQLRow

   default cTableName to ""
   default aFStruct to {}

   ::aRow := aRow
   //DAVID:
   ::aOriValue := ACLONE( aRow )    // Original values ( same as TMySQLtable:aOldValue )

   ::aFieldStruct := aFStruct
   ::cTable := cTableName

   ::aDirty := Array(Len(::aRow))
   ::aOldValue := Array(Len(::aRow))

   AFill(::aDirty, .F.)

return Self


METHOD FieldGet(cnField) CLASS TMySQLRow

   local nNum := iif( ISCHARACTER( cnField ), ::FieldPos(cnField), cnField )

   if nNum > 0 .AND. nNum <= Len(::aRow)

      // Char fields are padded with spaces since a real .dbf field would be
      if ::FieldType(nNum) == "C"
         return PadR(::aRow[nNum], ::aFieldStruct[nNum][MYSQL_FS_LENGTH])
      else
         return ::aRow[nNum]
      endif

   endif

return nil


METHOD FieldPut(cnField, Value) CLASS TMySQLRow

   local nNum := iif( ISCHARACTER( cnField ), ::FieldPos(cnField), cnField )

   if nNum > 0 .AND. nNum <= Len(::aRow)

      if Valtype(Value) == Valtype(::aRow[nNum]) .OR. ::aRow[nNum]==NIL

         // if it is a char field remove trailing spaces
         if ISCHARACTER(Value)
            Value := RTrim(Value)
         endif

         // Save starting value for this field
         if !::aDirty[nNum]
            ::aOldValue[nNum] := ::aRow[nNum]
            ::aDirty[nNum] := .T.
         endif

         ::aRow[nNum] := Value

         return Value
      endif
   endif

return nil


// Given a field name returns it's position
METHOD FieldPos(cFieldName) CLASS TMySQLRow

   local cUpperName := Upper(cFieldName)

return AScan(::aFieldStruct, {|aItem| (Upper(aItem[MYSQL_FS_NAME]) == cUpperName)})


// Returns name of field N
METHOD FieldName(nNum) CLASS TMySQLRow

return iif( nNum >=1 .AND. nNum <= Len(::aFieldStruct), ::aFieldStruct[nNum][MYSQL_FS_NAME], "" )


METHOD FieldLen(nNum) CLASS TMySQLRow

return iif( nNum >=1 .AND. nNum <= Len(::aFieldStruct), ::aFieldStruct[nNum][MYSQL_FS_LENGTH], 0 )

METHOD FieldDec(nNum) CLASS TMySQLRow

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      if ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .or. ;
         ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE 
       return set(_SET_DECIMALS)
      else
       return ::aFieldStruct[nNum][MYSQL_FS_DECIMALS]
      endif
   endif

return 0


METHOD FieldType(nNum) CLASS TMySQLRow

   local cType := "U"

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)

      do case
         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
            cType := "L"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
            cType := "N"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
            cType := "D"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
            cType := "M"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
            cType := "C"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE
            cType := "N"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
            cType := "M"

         otherwise
            cType := "U"

      endcase
   endif

return cType


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLRow

   local ni, cWhere := " WHERE "

   for nI := 1 to Len(::aFieldStruct)

      // search for fields part of a primary key
      if (hb_bitAnd(::aFieldStruct[nI][MYSQL_FS_FLAGS], PRI_KEY_FLAG) == PRI_KEY_FLAG) .OR.;
         (hb_bitAnd(::aFieldStruct[nI][MYSQL_FS_FLAGS], MULTIPLE_KEY_FLAG) == MULTIPLE_KEY_FLAG)

         cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="

         // if a part of a primary key has been changed, use original value
         if ::aDirty[nI]
            cWhere += ClipValue2SQL(::aOldValue[nI])
         else
            cWhere += ClipValue2SQL(::aRow[nI])
         endif

         cWhere += " AND "
      endif

   next

   // remove last " AND "
   cWhere := Left(cWhere, Len(cWhere) - 5)

return cWhere

/* ----------------------------------------------------------------------------------------*/

// Every single query submitted to MySQL server
CLASS TMySQLQuery

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

   METHOD   New(nSocket, cQuery)       // New query object
   METHOD   Destroy()
   METHOD   End() INLINE ::Destroy()
   METHOD   Refresh()                  // ReExecutes the query (cQuery) so that changes to table are visible

   METHOD   GetRow(nRow)               // return Row n of answer

   METHOD   Skip(nRows)                // Same as clipper ones

   METHOD   Bof() INLINE ::lBof    //DAVID:  ::nCurRow == 1
   METHOD   Eof() INLINE ::lEof    //DAVID:  ::nCurRow == ::nNumRows
   METHOD   RecNo() INLINE ::nCurRow
   METHOD   LastRec() INLINE ::nNumRows
   METHOD   GoTop() INLINE ::GetRow(1)
   METHOD   GoBottom() INLINE ::GetRow(::nNumRows)
   METHOD   GoTO(nRow) INLINE ::GetRow(nRow)

   METHOD   FCount()

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                           // Returns textual description of last error and clears ::lError

   METHOD   FieldName(nNum)
   METHOD   FieldPos(cFieldName)
   METHOD   FieldGet(cnField)

   METHOD   FieldLen(nNum)             // Length of field N
   METHOD   FieldDec(nNum)             // How many decimals in field N
   METHOD   FieldType(nNum)            // Clipper type of field N

ENDCLASS


METHOD New(nSocket, cQuery) CLASS TMySQLQuery

   local nI, aField

   ::nSocket := nSocket
   ::cQuery := cQuery

   ::lError := .F.
   ::aFieldStruct := {}
   ::nCurRow := 1
   ::nResultHandle := nil
   ::nNumFields := 0
   ::nNumRows := 0
   //DAVID:
   ::lBof := .T.
   ::lEof := .T.

   ::lFieldAsData := .T.     //Use fields as object DATA. For compatibility
   ::aRow := {}              //Values of fields of current row

   if mysql_query(nSocket, cQuery) == 0

      // save result set
      if !Empty(::nResultHandle := mysql_store_result(nSocket))

         ::nNumRows := mysql_num_rows(::nResultHandle)
         ::nNumFields := mysql_num_fields(::nResultHandle)
         //DAVID:
         ::aRow       := Array( ::nNumFields )

         for nI := 1 to ::nNumFields

            aField := mysql_fetch_field(::nResultHandle)
            AAdd(::aFieldStruct, aField)
            //DAVID:
            if ::lFieldAsData
               __ObjAddData(Self,::aFieldStruct[nI][MYSQL_FS_NAME])
            endif

         next

         ::getRow(::nCurRow)

      else
         // Should query have returned rows? (Was it a SELECT like query?)

         if (::nNumFields := mysql_num_fields(nSocket)) == 0

            // Was not a SELECT so reset ResultHandle changed by previous mysql_store_result()
            ::nResultHandle := nil

         else
            ::lError := .T.

         endif
      endif

   else
      ::lError := .T.
   endif

return Self


METHOD Refresh() CLASS TMySQLQuery

   // free present result handle
   mysql_free_result(::nResultHandle)

   ::lError := .F.

   if mysql_query(::nSocket, ::cQuery) == 0

      // save result set
      ::nResultHandle := mysql_store_result(::nSocket)
      ::nNumRows := mysql_num_rows(::nResultHandle)

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      if ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      endif

      ::getRow(::nCurRow)

   else
    /*  ::aFieldStruct := {}
      ::nResultHandle := nil
      ::nNumFields := 0
      ::nNumRows := 0
      */
      ::lError := .T.

   endif

return !::lError


METHOD Skip(nRows) CLASS TMySQLQuery
//DAVID:
   local lbof

   // NOTE: MySQL row count starts from 0
   default nRows to 1

   //DAVID:
   ::lBof := ( EMPTY( ::LastRec() ) )

   if nRows == 0
      // No move

   elseif nRows < 0
      // Negative movement
      //DAVID: ::nCurRow := Max(::nCurRow + nRows, 1)
      if ( ( ::recno() + nRows ) + 0 ) < 1
         nRows := - ::recno() + 1
         //Clipper: only SKIP movement can set BOF() to .T.
         ::lBof := .T.  //Try to skip before first record
      endif

   else
      // positive movement
      //DAVID: ::nCurRow := Min(::nCurRow + nRows, ::nNumRows)
      if ( ( ::recno() + nRows ) + 0 ) > ::lastrec()
         nRows := ::lastrec() - ::recno() + 1
      endif

   endif

   //DAVID:
   ::nCurRow := ::nCurRow + nRows

   //DAVID: maintain ::bof() true until next movement
   //Clipper: only SKIP movement can set BOF() to .T.
   lbof := ::bof()

//   mysql_data_seek(::nResultHandle, ::nCurRow - 1)
   ::getRow(::nCurrow)

   if lbof
      ::lBof := .T.
   endif

//DAVID: DBSKIP() return NIL  return ::nCurRow
return nil


/* Given a three letter month name gives back month number as two char string (ie. Apr -> 04) */
static function NMonth(cMonthValue)

   static cMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec" }
   local nMonth

   nMonth := AScan(cMonths, cMonthValue)

return PadL(nMonth, 2, "0")


// Get row n of a query and return it as a TMySQLRow object
METHOD GetRow(nRow) CLASS TMySQLQuery

   //DAVID: replaced by ::aRow   local aRow := NIL
   local oRow := NIL
   local i

   //DAVID: use current row  default nRow to 0
   default nRow to ::nCurRow

   if ::nResultHandle != NIL

      //DAVID:
      ::lBof := ( EMPTY( ::LastRec() ) )

      if nRow < 1 .or. nRow > ::lastrec()  //Out of range
         // Equal to Clipper behaviour
         nRow := ::lastrec() + 1  //LASTREC()+1
         ::nCurRow := ::lastrec() + 1
         // ::lEof := .t.
      endif

      if nRow >= 1 .AND. nRow <= ::nNumRows

         // NOTE: row count starts from 0
         mysql_data_seek(::nResultHandle, nRow - 1)
         ::nCurRow := nRow
//DAVID:      else
         //DAVID: use current row  ::nCurRow++
      endif

      //DAVID:
      ::lEof := ( ::Recno() > ::LastRec() )
      ::aRow := NIL

      if ::eof()
         // Phantom record with empty fields
         ::aRow := Array( Len( ::aFieldStruct ) )
         Afill( ::aRow, "" )

      else
         ::aRow := mysql_fetch_row(::nResultHandle)
      endif

      if ::aRow != NIL

         // Convert answer from text field to correct clipper types
         for i := 1 to ::nNumFields
            do case
               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
                  //DAVID:
                  if ::aRow[i]==NIL
                     ::aRow[i] := "0"
                  endif
                  ::aRow[i] := iif(Val(::aRow[i]) == 0, .F., .T.)

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE .OR. ;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
                  //DAVID:
                  if ::aRow[i]==NIL
                     ::aRow[i] := "0"
                  endif
                  ::aRow[i] := Val(::aRow[i])

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
                  //DAVID:
                  if ::aRow[i]==NIL
                     ::aRow[i] := "0"
                  endif
                  ::aRow[i] := Val(::aRow[i])

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
                  if Empty(::aRow[i])
                     ::aRow[i] := hb_SToD("")
                  else
                     // Date format YYYY-MM-DD
                     ::aRow[i] := hb_SToD(Left(::aRow[i], 4) + SubStr(::aRow[i], 6, 2) + Right(::aRow[i], 2))
                  endif

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
                  // Memo field

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE
                  // char field

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
                  // DateTime field

               otherwise

                  //DAVID: Alert("Unknown type from SQL Server Field: " + hb_NToS(i)+" is type "+hb_NToS(::aFieldStruct[i][MYSQL_FS_TYPE]))
                  // QOUT("Unknown type from SQL Server Field: " + hb_NToS(i)+" is type "+hb_NToS(::aFieldStruct[i][MYSQL_FS_TYPE]))

            endcase

            //DAVID:
            if ::lFieldAsData
               __objsetValuelist(Self,{{::aFieldStruct[i][MYSQL_FS_NAME],::aRow[i]}})
            endif

         next

         oRow := TMySQLRow():New(::aRow, ::aFieldStruct)

      endif

   endif
  //DAVID: if ::arow==nil; msginfo("::arow nil"); end

return iif(::aRow == NIL, NIL, oRow)


// Free result handle and associated resources
METHOD Destroy() CLASS TMySQLQuery

   mysql_free_result(::nResultHandle)

return Self


METHOD FCount() CLASS TMySQLQuery

return ::nNumFields


METHOD Error() CLASS TMySQLQuery

   ::lError := .F.

return mysql_error(::nSocket)

// Given a field name returns it's position
METHOD FieldPos(cFieldName) CLASS TMySQLQuery

   local cUpperName, nPos

   cUpperName := Upper(cFieldName)

   //DAVID: nPos := AScan(::aFieldStruct, {|aItem| iif(Upper(aItem[MYSQL_FS_NAME]) == cUpperName, .T., .F.)})
   nPos := AScan(::aFieldStruct, {|aItem| (Upper(aItem[MYSQL_FS_NAME]) == cUpperName)})

   /*
   nPos := 0
   while ++nPos <= Len(::aFieldStruct)
      if Upper(::aFieldStruct[nPos][MYSQL_FS_NAME]) == cUpperName
         exit
      endif
   enddo

   // I haven't found field name
   if nPos > Len(::aFieldStruct)
      nPos := 0
   endif
   */

return nPos


// Returns name of field N
METHOD FieldName(nNum) CLASS TMySQLQuery

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      return ::aFieldStruct[nNum][MYSQL_FS_NAME]
   endif

return ""

METHOD FieldGet(cnField) CLASS TMySQLQuery

   local nNum,Value

   if ISCHARACTER(cnField)
      nNum := ::FieldPos(cnField)
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= ::nNumfields
      //DAVID: Value :=  __objsendmsg(Self,::aFieldStruct[nNum][MYSQL_FS_NAME])
      Value := ::aRow[ nNum ]

      // Char fields are padded with spaces since a real .dbf field would be
      if ::FieldType(nNum) == "C"
         return PadR(Value,::aFieldStruct[nNum][MYSQL_FS_LENGTH])
      else
         return  Value
      endif

   endif

return nil


METHOD FieldLen(nNum) CLASS TMySQLQuery

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      return ::aFieldStruct[nNum][MYSQL_FS_LENGTH]
   endif

return 0

METHOD FieldDec(nNum) CLASS TMySQLQuery

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      if ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .or. ;
         ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE 
       return set(_SET_DECIMALS)
      else
       return ::aFieldStruct[nNum][MYSQL_FS_DECIMALS]
      endif
   endif

return 0


METHOD FieldType(nNum) CLASS TMySQLQuery

   local cType := "U"

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      do case
         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
            cType := "L"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE.OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
            cType := "N"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
            cType := "D"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
            cType := "M"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
            cType := "C"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE
            cType := "N"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
            cType := "M"

         otherwise
            cType := "U"

      endcase
   endif

return cType




/* ----------------------------------------------------------------------------------------*/

// A Table is a query without joins; this way I can Insert() e Delete() rows.
// NOTE: it's always a SELECT result, so it will contain a full table only if
//       SELECT * FROM ... was issued
CLASS TMySQLTable FROM TMySQLQuery

   DATA  cTable               // name of table
   DATA  aOldValue         //  keeps a copy of old value

   METHOD   New(nSocket, cQuery, cTableName)
   METHOD   GetRow(nRow)
   METHOD   Skip(nRow)
   METHOD   GoTop() INLINE ::GetRow(1)
   METHOD   GoBottom() INLINE ::GetRow(::nNumRows)
   METHOD   GoTo(nRow) INLINE ::GetRow(nRow)

   //DAVID: lOldRecord, lrefresh added
   METHOD   Update(oRow, lOldRecord, lRefresh)      // Gets an oRow and updates changed fields

   METHOD   Save() INLINE ::Update()

   //DAVID: lOldRecord, lRefresh added
   METHOD   Delete(oRow, lOldRecord, lRefresh)      // Deletes passed row from table
   //DAVID: lRefresh added
   METHOD   Append(oRow, lRefresh)      // Inserts passed row into table
   //DAVID: lSetValues added
   METHOD   GetBlankRow( lSetValues )     // Returns an empty row with all available fields empty
   //DAVID:
   METHOD   SetBlankRow() INLINE ::GetBlankRow( .T. )    //Compatibility

   METHOD   Blank() INLINE ::GetBlankRow()
   METHOD   FieldPut(cnField, Value)   // field identifier, not only a number
   METHOD   Refresh()
   METHOD   MakePrimaryKeyWhere()    // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New(nSocket, cQuery, cTableName) CLASS TMySQLTable

   local i

   super:New(nSocket, AllTrim(cQuery))

   ::cTable := Lower(cTableName)
   ::aOldValue:={}

   for i := 1 to ::nNumFields
      aadd(::aOldValue, ::fieldget(i))
   next


return Self


METHOD GetRow(nRow) CLASS TMySQLTable

   local oRow := super:GetRow(nRow), i

   if oRow != NIL
      oRow:cTable := ::cTable
   endif

   ::aOldvalue:={}
   for i := 1 to ::nNumFields
       // ::aOldValue[i] := ::FieldGet(i)
       aadd(::aOldvalue,::fieldget(i))
   next

return oRow


METHOD Skip(nRow) CLASS TMySQLTable
   local i
   super:skip(nRow)

   for i := 1 to ::nNumFields
      ::aOldValue[i] := ::FieldGet(i)
   next

//DAVID: DBSKIP() return NIL  return Self
return nil


/* Creates an update query for changed fields and submits it to server */
//DAVID: lOldRecord, lRefresh added
METHOD Update(oRow, lOldRecord, lRefresh ) CLASS TMySQLTable

   local cUpdateQuery := "UPDATE " + ::cTable + " SET "
   local i
   //DAVID:
   local ni, cWhere := " WHERE "
   default lOldRecord to .F.
   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   default lRefresh to .T.

   ::lError := .F.

   Do case

          // default Current row
       case oRow==nil

         for i := 1 to  ::nNumFields

            if !( ::aOldValue[i] == ::FieldGet(i) )
               cUpdateQuery += ::aFieldStruct[i][MYSQL_FS_NAME] + "=" + ClipValue2SQL(::FieldGet(i)) + ","
            endif
         next

         // no Change
         if right(cUpdateQuery,4)=="SET "; return !::lError; end

         // remove last comma
         cUpdateQuery := Left(cUpdateQuery, Len(cUpdateQuery) -1)

         //DAVID:
         if lOldRecord
            // based in matching of ALL fields of old record
            // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

            for nI := 1 to Len(::aFieldStruct)
                  cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="
                  // use original value
                  cWhere += ClipValue2SQL(::aOldValue[nI])
                  cWhere += " AND "
            next
            // remove last " AND "
            cWhere := Left(cWhere, Len(cWhere) - 5)
            cUpdateQuery += cWhere

         else
            //MakePrimaryKeyWhere is based in fields part of a primary key
            cUpdateQuery += ::MakePrimaryKeyWhere()
         endif

         if mysql_query(::nSocket, cUpdateQuery) == 0
            //DAVID: Clipper maintain same record pointer

            //DAVID: after refresh(), position of current record is often unpredictable
            if lRefresh
               ::refresh()
            else
               //DAVID: just reset values (?)
               for i := 1 to ::nNumFields
                   ::aOldValue[i] := ::FieldGet(i)
               next
            endif

         else
            ::lError := .T.

         endif

      Case oRow!=nil

         if oRow:cTable == ::cTable

            for i := 1 to Len(oRow:aRow)
               if oRow:aDirty[i]
                  cUpdateQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + "=" + ClipValue2SQL(oRow:aRow[i]) + ","
               endif
            next

            // remove last comma
            cUpdateQuery := Left(cUpdateQuery, Len(cUpdateQuery) -1)

            //DAVID:
            if lOldRecord
               // based in matching of ALL fields of old record
               // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

               for nI := 1 to Len(oRow:aFieldStruct)
                     cWhere += oRow:aFieldStruct[nI][MYSQL_FS_NAME] + "="
                     // use original value
                     cWhere += ClipValue2SQL(oRow:aOriValue[nI])
                     cWhere += " AND "
               next
               // remove last " AND "
               cWhere := Left(cWhere, Len(cWhere) - 5)
               cUpdateQuery += cWhere

            else
               //MakePrimaryKeyWhere is based in fields part of a primary key
               cUpdateQuery += oRow:MakePrimaryKeyWhere()
            endif

            if mysql_query(::nSocket, cUpdateQuery) == 0

               // All values are commited
               Afill(oRow:aDirty, .F.)
               Afill(oRow:aOldValue, nil)

               //DAVID:
               oRow:aOriValue := ACLONE( oRow:aRow )

               //DAVID: Clipper maintain same record pointer

               //DAVID: after refresh(), position of current record is often unpredictable
               if lRefresh
                  ::refresh()
               endif

            else
               ::lError := .T.

            endif

         endif
   endCase

return !::lError


//DAVID: lOldRecord, lRefresh added
METHOD Delete(oRow, lOldRecord, lRefresh) CLASS TMySQLTable

   local cDeleteQuery := "DELETE FROM " + ::cTable , i

   //DAVID:
   local ni, cWhere := " WHERE "
   default lOldRecord to .F.
   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   default lRefresh to .T.

   // is this a row of this table ?
   Do Case
      Case orow==nil

         //DAVID:
         if lOldRecord
            // based in matching of ALL fields of old record
            // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

            for nI := 1 to Len(::aFieldStruct)
                  cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="
                  // use original value
                  cWhere += ClipValue2SQL(::aOldValue[nI])
                  cWhere += " AND "
            next
            // remove last " AND "
            cWhere := Left(cWhere, Len(cWhere) - 5)
            cDeleteQuery += cWhere

         else
            //MakePrimaryKeyWhere is based in fields part of a primary key
            cDeleteQuery += ::MakePrimaryKeyWhere()
         endif

         if mysql_query(::nSocket, cDeleteQuery) == 0
            ::lError := .F.
            //DAVID: Clipper maintain same record pointer
            //DAVID: ::nCurRow--

            //DAVID: after refresh(), position of current record is often unpredictable
            if lRefresh
               ::refresh()
            else
               //DAVID: just reset values (?)
               for i := 1 to ::nNumFields
                   ::aOldValue[i] := ::FieldGet(i)
               next
            endif

         else
            ::lError := .T.

         endif

      Case oRow!=nil
         if oRow:cTable == ::cTable

            //DAVID:
            if lOldRecord
               // based in matching of ALL fields of old record
               // WARNING: if there are more than one record of ALL fields matching, all of those records will be changed

               for nI := 1 to Len(oRow:aFieldStruct)
                     cWhere += oRow:aFieldStruct[nI][MYSQL_FS_NAME] + "="
                     // use original value
                     cWhere += ClipValue2SQL(oRow:aOriValue[nI])
                     cWhere += " AND "
               next
               // remove last " AND "
               cWhere := Left(cWhere, Len(cWhere) - 5)
               cDeleteQuery += cWhere

            else
               //MakePrimaryKeyWhere is based in fields part of a primary key
               cDeleteQuery += oRow:MakePrimaryKeyWhere()
            endif

            if mysql_query(::nSocket, cDeleteQuery) == 0
               ::lError := .F.

               //DAVID: after refresh(), position of current record is often unpredictable
               if lRefresh
                  ::refresh()
               endif

            else
               ::lError := .T.

            endif

          endif
  EndCase

return !::lError


// Adds a row with values passed into oRow
//DAVID: lRefresh added
METHOD Append(oRow, lRefresh) CLASS TMySQLTable

   local cInsertQuery := "INSERT INTO " + ::cTable + " ("
   local i
   //DAVID: too many ::refresh() can slow some processes, so we can desactivate it by parameter
   default lRefresh to .T.

   Do Case
           // default Current row
      Case oRow==nil

            // field names
            for i := 1 to ::nNumFields
               if ::aFieldStruct[i][MYSQL_FS_FLAGS]!=AUTO_INCREMENT_FLAG
                  cInsertQuery += ::aFieldStruct[i][MYSQL_FS_NAME] + ","
               endif
            next
            // remove last comma from list
            cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ") VALUES ("

            // field values
            for i := 1 to ::nNumFields
               if ::aFieldStruct[i][MYSQL_FS_FLAGS]!=AUTO_INCREMENT_FLAG
                  cInsertQuery += ClipValue2SQL(::FieldGet(i)) + ","
               endif
            next

            // remove last comma from list of values and add closing parenthesis
            cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ")"

            if mysql_query(::nSocket, cInsertQuery) == 0
               ::lError := .F.
               //DAVID: Clipper add record at end
               ::nCurRow := ::lastrec() + 1

               //DAVID: after refresh(), position of current record is often unpredictable
               if lRefresh
                  ::refresh()
               else
                  //DAVID: just reset values in memory (?)
                  /* was same values from fieldget(i) !
                  for i := 1 to ::nNumFields
                      ::aOldValue[i] := ::FieldGet(i)
                  next
                  */
               endif

               return .T.
            else
               ::lError := .T.
            endif

    Case oRow!=nil

         if oRow:cTable == ::cTable

            // field names
            for i := 1 to Len(oRow:aRow)
               if oRow:aFieldStruct[i][MYSQL_FS_FLAGS]!=AUTO_INCREMENT_FLAG
                  cInsertQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + ","
               endif
            next
            // remove last comma from list
            cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ") VALUES ("

            // field values
            for i := 1 to Len(oRow:aRow)
               if oRow:aFieldStruct[i][MYSQL_FS_FLAGS]!=AUTO_INCREMENT_FLAG
                  cInsertQuery += ClipValue2SQL(oRow:aRow[i]) + ","
               endif
            next

            // remove last comma from list of values and add closing parenthesis
            cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ")"

            if mysql_query(::nSocket, cInsertQuery) == 0
               //DAVID:
               ::lError := .F.

               //DAVID:
               // All values are commited
               Afill(oRow:aDirty, .F.)
               Afill(oRow:aOldValue, nil)

               //DAVID:
               oRow:aOriValue := ACLONE( oRow:aRow )

               //DAVID: Clipper add record at end
               ::nCurRow := ::lastrec() + 1

               //DAVID: after refresh(), position of current record is often unpredictable
               if lRefresh
                  ::refresh()
               endif

               return .T.
            else
               ::lError := .T.
            endif

         endif

    Endcase
return .F.


//DAVID: lSetValues added  METHOD GetBlankRow() CLASS TMySQLTable
METHOD GetBlankRow( lSetValues ) CLASS TMySQLTable

   local i
   local aRow := Array(::nNumFields)

   //DAVID: It is not current row, so do not change it
   default lSetValues to .F.

   // crate an array of empty fields
   for i := 1 to ::nNumFields

      do case
      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE     .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE       .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATETIME_TYPE
         aRow[i] := ""

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE      .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONGLONG_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_INT24_TYPE .OR. ;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DECIMAL_TYPE
         aRow[i] := 0

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
         aRow[i] := .F.

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE     .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
         aRow[i] := 0.0

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
         aRow[i] := hb_SToD("")

      otherwise
         aRow[i] := nil

      endcase
   next

   //DAVID:
   if lSetValues   //Assign values as current row values
      for i := 1 to ::nNumFields
            ::FieldPut(i, aRow[i])
            ::aOldValue[i] := aRow[i]
      next
   endif

return TMySQLRow():New(aRow, ::aFieldStruct, ::cTable, .F.)


METHOD FieldPut(cnField, Value) CLASS TMySQLTable

   local nNum

   if ISCHARACTER(cnField)
      nNum := ::FieldPos(cnField)
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= ::nNumFields

//DAVID:      if Valtype(Value) == Valtype(::FieldGet(nNum)) .OR. Empty(::Fieldget(nNum))
      if Valtype(Value) == Valtype(::aRow[nNum]) .OR. ::aRow[nNum]==NIL

         // if it is a char field remove trailing spaces
         if ISCHARACTER(Value)
            Value := RTrim(Value)
         endif

         //DAVID:
         ::aRow[ nNum ] := Value
         if ::lFieldAsData
            __objsetValueList(Self,{{::aFieldStruct[nNum][MYSQL_FS_NAME],Value}})
         endif

         return Value
      endif
   endif

return nil


METHOD Refresh() CLASS TMySQLTABLE

   // free present result handle
   mysql_free_result(::nResultHandle)

   ::lError := .F.

   if mysql_query(::nSocket, ::cQuery) == 0

      // save result set
      ::nResultHandle := mysql_store_result(::nSocket)
      ::nNumRows := mysql_num_rows(::nResultHandle)

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      if ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      endif

      ::getRow(::nCurRow)

   else
/*      ::aFieldStruct := {}
      ::nResultHandle := nil
      ::nNumFields := 0
      ::nNumRows := 0

      ::aOldValue:={}
      */
      ::lError := .T.
   endif

return !::lError


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLTable

   local ni, cWhere := " WHERE "

   for nI := 1 to Len(::aFieldStruct)

      // search for fields part of a primary key
      if (hb_bitAnd(::aFieldStruct[nI][MYSQL_FS_FLAGS], PRI_KEY_FLAG) == PRI_KEY_FLAG) .OR.;
         (hb_bitAnd(::aFieldStruct[nI][MYSQL_FS_FLAGS], MULTIPLE_KEY_FLAG) == MULTIPLE_KEY_FLAG)

         cWhere += ::aFieldStruct[nI][MYSQL_FS_NAME] + "="

         // if a part of a primary key has been changed, use original value

            cWhere += ClipValue2SQL(::aOldValue[nI])


         cWhere += " AND "
      endif

   next

   // remove last " AND "
   cWhere := Left(cWhere, Len(cWhere) - 5)

return cWhere



/* ----------------------------------------------------------------------------------------*/

// Every available MySQL server
CLASS TMySQLServer

   DATA  nSocket                 // connection handle to server (currently pointer to a MYSQL structure)
   DATA  cServer                 // server name
   DATA  cDBName                 // Selected DB
   DATA  cUser                   // user accessing db
   DATA  cPassword               // his/her password
   DATA  lError                  // .T. if occurred an error
   DATA  cCreateQuery

   METHOD   New(cServer, cUser, cPassword)   // Opens connection to a server, returns a server object
   METHOD   Destroy()                        // Closes connection to server

   METHOD   SelectDB(cDBName)    // Which data base I will use for subsequent queries

   METHOD   CreateTable(cTable, aStruct,cPrimaryKey,cUniqueKey,cAuto)  // Create new table using the same syntax of dbCreate()
   METHOD   DeleteTable(cTable)           // delete table
   METHOD   TableStruct(cTable)           // returns a structure array compatible with clipper's dbStruct() ones
   METHOD   CreateIndex(cName, cTable, aFNames, lUnique) // Create an index (unique) on field name(s) passed as an array of strings aFNames
   METHOD   DeleteIndex(cName, cTable)                   // Delete index cName from cTable

   METHOD   ListDBs()            // returns an array with list of data bases available
   METHOD   ListTables()         // returns an array with list of available tables in current database

   METHOD   Query(cQuery)        // Gets a textual query and returns a TMySQLQuery or TMySQLTable object

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error
   METHOD   CreateDatabase( cDataBase )      // Create an New Mysql Database
//Mitja
   METHOD   sql_Commit()      // Commits transaction
   METHOD   sql_Rollback()    // Rollbacks transaction
   METHOD   sql_Version()     // server version as numeric
ENDCLASS


METHOD New(cServer, cUser, cPassword) CLASS TMySQLServer

   ::cServer := cServer
   ::cUser := cUser
   ::cPassword := cPassword
   ::nSocket := mysql_real_connect(cServer, cUser, cPassword)
   ::lError := .F.

   if Empty( ::nSocket )
      ::lError := .T.
   endif

return Self



METHOD Destroy() CLASS TMySQLServer
   mysql_close(::nSocket)
return Self



METHOD sql_commit() CLASS TMySQLServer
  if mysql_commit(::nSocket) == 0
    Return .T.
  endif
return .F.



METHOD sql_rollback() CLASS TMySQLServer
  if mysql_rollback(::nSocket) == 0
    Return .T.
  endif
return .F.


METHOD sql_version() CLASS TMySQLServer
local nVer
  nVer:=mysql_get_server_version(::nSocket)
return nVer



*METHOD SelectDB(cDBName) CLASS TMySQLServer
*
*   if mysql_select_db(::nSocket, cDBName) == 0
*      ::cDBName := cDBName
*      return .T.
*   else
*      ::cDBName := ""
*   endif
*
*return .F.


*****************alterado
METHOD SelectDB(cDBName) CLASS TMySQLServer

   ::lError := .F.

   if mysql_select_db(::nSocket, cDBName) != 0     /* tabela nao existe */
      ::cDBName :=""
      ::lError := .T.
   else                                       /* tabela existe */
      ::cDBName :=cDBName
      ::lError := .F.
      return .T.
   endif

return .F.


METHOD CreateDatabase ( cDataBase ) CLASS TMySQLServer
   local cCreateQuery := "CREATE DATABASE "+ lower(cDatabase)

   if mysql_query(::nSocket, cCreateQuery) == 0
      return .T.
   endif

return .F.


// NOTE: OS/2 port of MySQL is picky about table names, that is if you create a table with
// an upper case name you cannot alter it (for example) using a lower case name, this violates
// OS/2 case insensibility about names
METHOD CreateTable(cTable, aStruct,cPrimaryKey,cUniqueKey,cAuto) CLASS TMySQLServer

   /* NOTE: all table names are created with lower case */

   local i

   // returns NOT NULL if extended structure has DBS_NOTNULL field to true
   local cNN := {|aArr| iif(Len(aArr) > DBS_DEC, iif(aArr[DBS_NOTNULL], " NOT NULL ", ""), "")}
   ::cCreateQuery := "CREATE TABLE " + Lower(cTable) + " ("
   for i := 1 to Len(aStruct)
      do case
      case aStruct[i][DBS_TYPE] == "C"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " char(" + hb_NToS(aStruct[i][DBS_LEN]) + ")" + Eval(cNN, aStruct[i])+ iif(aStruct[i][DBS_NAME]==cPrimaryKey," NOT NULL ",'' )+ ","

      case aStruct[i][DBS_TYPE] == "M"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " text" + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "N"
         /*
         if aStruct[i][DBS_DEC] == 0
            ::cCreateQuery += aStruct[i][DBS_NAME] + " int(" + hb_NToS(aStruct[i][DBS_LEN]) + ")" + Eval(cNN, aStruct[i]) + iif(aStruct[i][DBS_NAME]==cPrimaryKey," NOT NULL ",'' )+ iif(aStruct[i][DBS_NAME]==cAuto," auto_increment ",'' ) + ","
         else
            ::cCreateQuery += aStruct[i][DBS_NAME] + " real(" + hb_NToS(aStruct[i][DBS_LEN]) + "," + hb_NToS(aStruct[i][DBS_DEC]) + ")" + Eval(cNN, aStruct[i]) + ","
         endif
         */
         if (aStruct[i][DBS_DEC] == 0) .and. (aStruct[i][DBS_LEN] <= 18)
            do case
               case (aStruct[i][DBS_LEN] <= 4)
                  ::cCreateQuery += aStruct[i][DBS_NAME] + " smallint(" + hb_NToS(aStruct[i][DBS_LEN]) + ")"
               case (aStruct[i][DBS_LEN] <= 6)
                  ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumint(" + hb_NToS(aStruct[i][DBS_LEN]) + ")"
               case (aStruct[i][DBS_LEN] <= 9)
                  ::cCreateQuery += aStruct[i][DBS_NAME] + " int(" + hb_NToS(aStruct[i][DBS_LEN]) + ")"
               otherwise
                  ::cCreateQuery += aStruct[i][DBS_NAME] + " bigint(" + hb_NToS(aStruct[i][DBS_LEN]) + ")"
            endcase
            ::cCreateQuery += Eval(cNN, aStruct[i]) + iif(aStruct[i][DBS_NAME]==cPrimaryKey," NOT NULL ",'' )+ iif(aStruct[i][DBS_NAME]==cAuto," auto_increment ",'' ) + ","
         else
            ::cCreateQuery += aStruct[i][DBS_NAME] + " real(" + hb_NToS(aStruct[i][DBS_LEN]) + "," + hb_NToS(aStruct[i][DBS_DEC]) + ")" + Eval(cNN, aStruct[i]) + ","
         endif
      case aStruct[i][DBS_TYPE] == "D"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " date " + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "L"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " tinyint "  + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "B"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumblob "  + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "I"
         ::cCreateQuery += aStruct[i][DBS_NAME] + " mediumint " + Eval(cNN, aStruct[i]) + ","

      otherwise
         ::cCreateQuery += aStruct[i][DBS_NAME] + " char(" + hb_NToS(aStruct[i][DBS_LEN]) + ")" + Eval(cNN, aStruct[i]) + ","

      endcase

   next
   if cPrimarykey != NIL
        ::cCreateQuery += ' PRIMARY KEY ('+cPrimaryKey+'),'
   endif
   if cUniquekey != NIL
        ::cCreateQuery += ' UNIQUE '+cUniquekey +' ('+cUniqueKey+'),'
   endif

   // remove last comma from list
   ::cCreateQuery := Left(::cCreateQuery, Len(::cCreateQuery) -1) + ");"
   if mysql_query(::nSocket, ::cCreateQuery) == 0
      return .T.
   else
      ::lError := .T.
   endif

return .F.


METHOD CreateIndex(cName, cTable, aFNames, lUnique) CLASS TMySQLServer

   local cCreateQuery := "CREATE "
   local i

   default lUnique to .F.

   if lUnique
      cCreateQuery += "UNIQUE INDEX "
   else
      cCreateQuery += "INDEX "
   endif

   cCreateQuery += cName + " ON " + Lower(cTable) + " ("

   for i := 1 to Len(aFNames)
      cCreateQuery += aFNames[i] + ","
   next

   // remove last comma from list
   cCreateQuery := Left(cCreateQuery, Len(cCreateQuery) -1) + ")"

   if mysql_query(::nSocket, cCreateQuery) == 0
      return .T.

   endif

return .F.


METHOD DeleteIndex(cName, cTable) CLASS TMySQLServer

   local cDropQuery := "DROP INDEX " + cName + " FROM " + Lower(cTable)

   if mysql_query(::nSocket, cDropQuery) == 0
      return .T.
   endif

return .F.


METHOD DeleteTable(cTable) CLASS TMySQLServer

   local cDropQuery := "DROP TABLE " + Lower(cTable)

   if mysql_query(::nSocket, cDropQuery) == 0
      return .T.

   endif

return .F.


METHOD Query(cQuery) CLASS TMySQLServer

   local oQuery, cTableName, i, cUpperQuery, nNumTables, cToken

   default cQuery to ""


   cUpperQuery := Upper(AllTrim(cQuery))
   i := 1
   nNumTables := 1

   while !( (cToken := __StrToken(cUpperQuery, i++, " ")) == "FROM" ) .AND. !Empty(cToken)
   enddo

   // first token after "FROM" is a table name
   // NOTE: SubSelects ?
   cTableName := __StrToken(cUpperQuery, i++, " ")

   while !( (cToken := __StrToken(cUpperQuery, i++, " ")) == "WHERE" ) .AND. !Empty(cToken)
      // do we have more than one table referenced ?
      if cToken == "," .OR. cToken == "JOIN"
         nNumTables++
      endif
   enddo

   if nNumTables == 1
      oQuery := TMySQLTable():New(::nSocket, cQuery, cTableName)
   else
      oQuery := TMymysql_query():New(::nSocket, cQuery)
   endif

   if oQuery:NetErr()
      ::lError := .T.
   endif

return oQuery


METHOD Error() CLASS TMySQLServer

   ::lError := .F.

return iif(Empty( ::nSocket ), "No connection to server", mysql_error(::nSocket))


METHOD ListDBs() CLASS TMySQLServer

   local aList

   aList := mysql_list_dbs(::nSocket)

return aList


METHOD ListTables() CLASS TMySQLServer

   local aList

   aList := mysql_list_tables(::nSocket)

return aList


/* TOFIX: Conversion creates a .dbf with fields of wrong dimension (often) */
METHOD TableStruct(cTable) CLASS TMySQLServer

   local aStruct := {}

   HB_SYMBOL_UNUSED( cTable )

   /* TODO: rewrite for MySQL
   local nRes, aField, aStruct, aSField, i

   aStruct := {}
   nRes := mysql_list_fields(::nSocket, cTable)

   if !Empty( nRes )
      for i := 1 to mysql_num_fields(nRes)

         aField := mysql_fetch_field(nRes)
         aSField := Array(DBS_DEC)

         // don't count indexes as real fields
         if aField[MSQL_FS_TYPE] <= MSQL_LAST_REAL_TYPE

            aSField[DBS_NAME] := Left(aField[MSQL_FS_NAME], 10)
            aSField[DBS_DEC] := 0

            do case
            case aField[MSQL_FS_TYPE] == MSQL_INT_TYPE
               aSField[DBS_TYPE] := "N"
               aSField[DBS_LEN] := 11

            case aField[MSQL_FS_TYPE] == MSQL_UINT_TYPE
               aSField[DBS_TYPE] := "L"
               aSField[DBS_LEN] := 1

            case aField[MSQL_FS_TYPE] == MSQL_CHAR_TYPE
               aSField[DBS_TYPE] := "C"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            case aField[MSQL_FS_TYPE] == MSQL_DATE_TYPE
               aSField[DBS_TYPE] := "D"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            case aField[MSQL_FS_TYPE] == MSQL_REAL_TYPE
               aSField[DBS_TYPE] := "N"
               aSField[DBS_LEN] := 12
               aSFIeld[DBS_DEC] := 8

            case aField[MSQL_FS_TYPE] == MYSQL_MEDIUM_BLOB_TYPE
               aSField[DBS_TYPE] := "B"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]

            case aField[MSQL_FS_TYPE] == FIELD_TYPE_INT24
               aSField[DBS_TYPE] := "I"
               aSField[DBS_LEN] := aField[MSQL_FS_LENGTH]
               aSFIeld[DBS_DEC] := aField[MYSQL_FS_DECIMALS]
            otherwise

            endcase

            AAdd(aStruct, aSField)
         endif
      next

      mysql_free_result(nRes)

   endif*/

return aStruct


// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
static function ClipValue2SQL(Value)

   local cValue

   do case
      case ISNUMBER(Value)
         cValue := hb_NToS(Value)

      case ISDATE(Value)
         if !Empty(Value)
            // MySQL dates are like YYYY-MM-DD
            cValue := "'"+StrZero(Year(Value), 4) + "-" + StrZero(Month(Value), 2) + "-" + StrZero(Day(Value), 2) + "'"
         else
            cValue := "''"
         endif

      case Valtype(Value) $ "CM"
         IF Empty( Value )
            cValue="''"
         ELSE
            cValue := "'"
            Value:=mysql_escape_string(value)
            cValue+= value+ "'"
         ENDIF

      case ISLOGICAL(Value)
         cValue := iif(Value, "1", "0")

      otherwise
         cValue := "''"       // NOTE: Here we lose values we cannot convert

   endcase

return cValue
