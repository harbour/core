
/*
 * Harbour Project source code:
 * mSQL DBMS classes.
 * These classes try to emulate clipper dbXXXX functions on a SQL query
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbclass.ch"
#include "common.ch"
#include "dbstruct.ch"
#include "msql.ch"


// Every single row of an answer
CLASS TmSQLRow

   DATA  aRow              // a single row of answer
   DATA  aDirty            // array of booleans set to .T. if corresponding field of aRow has been changed
   DATA  aFieldStruct      // type of each field
   DATA  cTable            // Name of table containing this row, empty if TmSQLQuery returned this row
   DATA  nRowID            // Record Number inside mSQL table. Used by Update() method of TmSQLTable

   METHOD   New(aRow, aFStruct, cTableName, lRowID)     // Create a new Row object and handle hidden _rowid field

   METHOD   FieldGet(nNum)                      // Same as clipper ones
   METHOD   FieldPut(nNum, Value)

   METHOD   FieldName(nPosition)
   METHOD   FieldPos(cFieldName)

ENDCLASS


METHOD New(aRow, aFStruct, cTableName, lRowID) CLASS TmSQLRow

   default cTableName to ""
   default aFStruct to {}
   default lRowID to .F.

   ::cTable := cTableName
   ::aFieldStruct := aFStruct

   // if first field is _rowid I'm inside a TmSQLTable, so remove this low level
   // info from list of fields.
   if lRowID
      ::nRowID := aRow[1]

      // remove first field (copying from second)
      ::aRow := Array(Len(aRow) - 1)
      ACopy(aRow, ::aRow, 2)

   else
      ::aRow := aRow
      ::aFieldStruct := aFStruct
      ::nRowID := -1
   endif

   ::aDirty := Array(Len(::aRow))
   AFill(::aDirty, .F.)

return Self


METHOD FieldGet(nNum) CLASS TmSQLRow

   if nNum > 0 .AND. nNum <= Len(::aRow)
      return ::aRow[nNum]
   else
      return nil
   endif

return


METHOD FieldPut(nNum, Value) CLASS TmSQLRow

   if nNum > 0 .AND. nNum <= Len(::aRow)
      if Valtype(Value) == Valtype(::aRow[nNum]) .OR. Empty(::aRow[nNum])
         ::aRow[nNum] := Value
         ::aDirty[nNum] := .T.
         return Value
      endif
   endif

return nil


// Given a field name returns it's position
METHOD FieldPos(cFieldName) CLASS TmSQLRow

   local cUpperName, nPos := 1

   cUpperName := Upper(cFieldName)

   /* NOTE: this code block kills harbour if called a few thousand times
   nPos := AScan(::aFieldStruct, {|aItem| iif(Upper(aItem[MSQL_FS_NAME]) == cUpperName, .T., .F.)})
   */

   while nPos <= Len(::aFieldStruct)
      if Upper(::aFieldStruct[nPos][MSQL_FS_NAME]) == cUpperName
         exit
      endif
      nPos++
   enddo

return nPos


// Returns name of field N
METHOD FieldName(nPosition) CLASS TmSQLRow

   if nPosition >=1 .AND. nPosition <= Len(::aFieldStruct)
      return ::aFieldStruct[nPosition][MSQL_FS_NAME]
   else
      return ""
   endif

return

/* ----------------------------------------------------------------------------------------*/

// Every single query submitted to mSQL server
CLASS TmSQLQuery

   DATA  nSocket           // connection handle to mSQL server
   DATA  nResultHandle     // result handle received from mSQL

   DATA  cQuery            // copy of query that generated this object

   DATA  nNumRows          // number of rows available on answer NOTE msql is 0 based
   DATA  nCurRow           // I'm currently over row number

   DATA  nNumFields        // how many fields per row (including hidden _rowID)
   DATA  aFieldStruct      // type of each field, a copy is here a copy inside each row

   DATA  lRowID            // .T. if query has field _rowID
   DATA  lError            // .T. if last operation failed

   METHOD   New(nSocket, cQuery)       // New query object
   METHOD   Destroy()

   METHOD   GetRow(nRow)               // return Row n of answer

   METHOD   Skip(nRows)                // Same as clipper ones

   METHOD   Bof() INLINE ::nCurRow == 1
   METHOD   Eof() INLINE ::nCurRow > ::nNumRows
   METHOD   RecNo() INLINE ::nCurRow
   METHOD   LastRec() INLINE ::nNumRows

   METHOD   FCount()

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error and clears ::lError

ENDCLASS


METHOD New(nSocket, cQuery) CLASS TmSQLQuery

   local nI, aField

   ::aFieldStruct := {}
   ::nSocket := nSocket
   ::cQuery := cQuery
   ::nNumRows := msqlQuery(nSocket, cQuery)
   ::nCurRow := 1
   ::lRowID := .F.
   ::lError := .F.

   if ::nNumRows >= 0

      ::nResultHandle := msqlStoreR()
      ::nNumFields := msqlNumFie(::nResultHandle)

      for nI := 1 to ::nNumFields

         aField := msqlFetchF(::nResultHandle)

         if Upper(aField[MSQL_FS_NAME]) == "_ROWID"
            ::lRowID := .T.
         else
            AAdd(::aFieldStruct, aField)
         endif
      next
   else

      ::nResultHandle := nil
      ::nNumFields := 0
      ::lError := .T.
   endif

return Self



METHOD Skip(nRows) CLASS TmSQLQuery

   // NOTE: mSQL row count starts from 0
   default nRows to 1

   if nRows == 0
      // No move
   elseif nRows < 0
      // Negative movement
      ::nCurRow := Max(::nCurRow - nRows, 1)
   else
      // positive movement
      ::nCurRow := Min(::nCurRow + nRows, ::nNumRows + 1)
   endif

   msqlDataSe(::nResultHandle, ::nCurRow - 1)

return Self


/* Given a three letter month name gives back month number as two char string (ie. Apr -> 04) */
static function NMonth(cMonthValue)

   static cMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec" }

   nMonth := AScan(cMonths, cMonthValue)

return PadL(nMonth, 2, "0")


// Get row n of a query and return it as a TmSQLRow object
METHOD GetRow(nRow) CLASS TmSQLQuery

   local aRow := NIL
   local oRow := NIL
   local i

   default nRow to 0

   if ::nResultHandle <> NIL
      if nRow >= 1 .AND. nRow <= ::nNumRows

         // NOTE: row count starts from 0
         msqlDataSe(::nResultHandle, nRow - 1)
         ::nCurRow := nRow
      else
         ::nCurRow++
      endif

      aRow := msqlFetchR(::nResultHandle, ::nNumFields)

      if aRow <> NIL

         if ::lRowID
            aRow[1] := Val(aRow[1])
         endif

         // Convert answer from text field to correct clipper types
         for i := iif(::lRowID, 2, 1) to ::nNumFields
             do case
             case ::aFieldStruct[iif(::lRowID, i - 1, i)][MSQL_FS_TYPE] == MSQL_UINT_TYPE
               aRow[i] := iif(Val(aRow[i]) == 0, .F., .T.)

             case ::aFieldStruct[iif(::lRowID, i - 1, i)][MSQL_FS_TYPE] == MSQL_INT_TYPE
               aRow[i] := Val(aRow[i])

             case ::aFieldStruct[iif(::lRowID, i - 1, i)][MSQL_FS_TYPE] == MSQL_REAL_TYPE
               aRow[i] := Val(aRow[i])

             case ::aFieldStruct[iif(::lRowID, i - 1, i)][MSQL_FS_TYPE] == MSQL_DATE_TYPE
               if Empty(aRow[i])
                  aRow[i] := CToD("")
               else
                  // Date format MM/DD/YYYY
                  aRow[i] := CToD(NMonth(__StrToken(aRow[i], 2, "-")) + "/" + __StrToken(aRow[i], 1, "-") + "/" + __StrToken(aRow[i], 3, "-"))
               endif
             otherwise
             endcase
         next

         oRow := TmSQLRow():New(aRow, ::aFieldStruct,, ::lRowID)
      endif
   endif

return iif(aRow == NIL, NIL, oRow)


// Free result handle and associated resources
METHOD Destroy() CLASS TmSQLQuery

   msqlFreeR(::nResultHandle)

return Self


METHOD FCount() CLASS TmSQLQuery

return ::nNumFields - iif(::lRowID, 1, 0)


METHOD Error() CLASS TmSQLQuery

   ::lError := .F.

return msqlGetErr()

/* ----------------------------------------------------------------------------------------*/

// A Table is a query without joins; this way I can Insert() e Delete() rows.
// NOTE: it's always a SELECT result, so it will contain a full table only if
//       SELECT * FROM ... was issued
CLASS TmSQLTable FROM TmSQLQuery

   DATA  cTable               // name of table

   METHOD   New(nSocket, cQuery, cTableName)
   METHOD   GetRow(nRow)

   METHOD   Update(oRow)      // Gets an oRow and updates changed fields
   METHOD   Delete(oRow)      // Deletes passed row from table
   METHOD   Append(oRow)      // Inserts passed row into table
   METHOD   GetBlankRow()     // Returns an empty row with all available fields empty

ENDCLASS


METHOD New(nSocket, cQuery, cTableName) CLASS TmSQLTable

   local cTrimmedQuery, nRes, i, cFieldList := "SELECT _rowid, ", aField

   ::cTable := cTableName

   // Add _rowid to query, Update() method needs this field
   cTrimmedQuery := AllTrim(cQuery)

   // if it's a SELECT * I cannot add _rowid to query; I need to expand * to the full list of fields
   if __StrToken(cTrimmedQuery, 2, " ") == "*"

      nRes := msqlListFi(nSocket, cTableName)

      if nRes > 0
         for i := 1 to msqlNumFie(nRes)
            aField := msqlFetchF(nRes)
            // don't count indexes as real fields
            if aField[MSQL_FS_TYPE] <= MSQL_LAST_REAL_TYPE
               cFieldList += aField[MSQL_FS_NAME] + ","
            endif
         next
         // remove last comma
         cFieldList := Left(cFieldList, Len(cFieldList) - 1)

         // remove SELECT
         cTrimmedQuery := StrTran(cTrimmedQuery, __StrToken(cTrimmedQuery, 1, " "), "")
         // remove *
         cTrimmedQuery := StrTran(cTrimmedQuery, __StrToken(cTrimmedQuery, 1, " "), "")
         cTrimmedQuery := cFieldList + cTrimmedQuery
         msqlFreeR(nRes)
      endif
   else
      cTrimmedQuery := StrTran(cTrimmedQuery, __StrToken(cTrimmedQuery, 1, " "), "SELECT _rowid, ")
   endif

   super:New(nSocket, cTrimmedQuery)

return Self


METHOD GetRow(nRow) CLASS TmSQLTable

   local oRow := super:GetRow(nRow)

   if oRow <> NIL
      oRow:cTable := ::cTable
   endif

return oRow


/* Creates an update query for changed fields and submits it to server */
METHOD Update(oRow) CLASS TmSQLTable

   local cUpdateQuery := "UPDATE " + ::cTable + " SET "
   local i, cField

   // is this a row of this table ?
   if oRow:cTable == ::cTable
      for i := 1 to Len(oRow:aRow)
         if oRow:aDirty[i]
            do case
            case Valtype(oRow:aRow[i]) == "N"
               cField := AllTrim(Str(oRow:aRow[i]))
               cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "=" + cField + ","

            case Valtype(oRow:aRow[i]) == "D"
               if !Empty(oRow:aRow[i])
                  // mSQL dates are like this 1-Oct-1900
                  cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "=" + "'" + Str(Day(oRow:aRow[i]), 2) + "-" + Left(CMonth(oRow:aRow[i]), 3) + "-" + Str(Year(oRow:aRow[i]), 4) +  "',"
               else
                  cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "=" + "'',"
               endif

            case Valtype(oRow:aRow[i]) == "C"
               cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "='" + oRow:aRow[i] + "',"

            case Valtype(oRow:aRow[i]) == "L"
               cField := AllTrim(Str(iif(oRow:aRow[i] == .F., 0, 1)))
               cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "=" + cField + ","

            otherwise
               cUpdateQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + "='',"

            endcase
         endif
      next

      // remove last comma
      cUpdateQuery := Left(cUpdateQuery, Len(cUpdateQuery) -1)

      cUpdateQuery += " WHERE _rowid=" + AllTrim(Str(oRow:nRowID))

      if msqlQuery(::nSocket, cUpdateQuery) == 1
         // All values are commited
         Afill(oRow:aDirty, .F.)
         return .T.
      else
         ::lError := .T.
      endif
   endif

return .F.


METHOD Delete(oRow) CLASS TmSQLTable

   local cDeleteQuery := "DELETE FROM " + ::cTable + " WHERE _rowid="

   // is this a row of this table ?
   if oRow:cTable == ::cTable

      cDeleteQuery += AllTrim(Str(oRow:nRowID))

      if msqlQuery(::nSocket, cDeleteQuery) == 1
         return .T.
      else
         ::lError := .T.
      endif
  endif

return .F.


// Adds a row with values passed into oRow
METHOD Append(oRow) CLASS TmSQLTable

   local cInsertQuery := "INSERT INTO " + ::cTable + " ("
   local i, cField

   // is this a row of this table ?
   if oRow:cTable == ::cTable

      // field names
      for i := 1 to Len(oRow:aRow)
         cInsertQuery += oRow:aFieldStruct[i][MSQL_FS_NAME] + ","
      next
      // remove last comma from list
      cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ") VALUES ("

      // field values
      for i := 1 to Len(oRow:aRow)

         do case
         case Valtype(oRow:aRow[i]) == "N"
            cField := AllTrim(Str(oRow:aRow[i]))
            cInsertQuery += cField + ","

         case Valtype(oRow:aRow[i]) == "C"
            cInsertQuery += "'" + oRow:aRow[i] + "',"

         case Valtype(oRow:aRow[i]) == "D"
            if !Empty(oRow:aRow[i])
               // mSQL dates have this form " 1-Oct-1990"
               /* NOTE: current implementation CANNOT retrieve from mSQL dates BEFORE 1st January 1970 */
               cInsertQuery += "'" + Str(Day(oRow:aRow[i]), 2) + "-" + Left(CMonth(oRow:aRow[i]), 3) + "-" + Str(Year(oRow:aRow[i]), 4) +  "',"
            else
               cInsertQuery += "'',"
            endif

        case Valtype(oRow:aRow[i]) == "L"
            cField := AllTrim(Str(iif(oRow:aRow[i] == .F., 0, 1)))
            cInsertQuery += cField + ","

         otherwise
            cInsertQuery += "'',"

         endcase
      next

      // remove last comma from list of values and add closing parenthesis
      cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ")"

      if msqlQuery(::nSocket, cInsertQuery) == 1
         return .T.
      else
         ::lError := .T.
      endif

   endif

return .F.


METHOD GetBlankRow() CLASS TmSQLTable

   local i
   local aRow := {}

   // crate an array of empty fields
   for i := 1 to ::FCount()
      do case
      case ::aFieldStruct[i][MSQL_FS_TYPE] == MSQL_CHAR_TYPE
         AAdd(aRow, "")

      case ::aFieldStruct[i][MSQL_FS_TYPE] == MSQL_INT_TYPE
         AAdd(aRow, 0)

      case ::aFieldStruct[i][MSQL_FS_TYPE] == MSQL_UINT_TYPE
         AAdd(aRow, .F.)

      case ::aFieldStruct[i][MSQL_FS_TYPE] == MSQL_REAL_TYPE
         AAdd(aRow, 0.0)

      case ::aFieldStruct[i][MSQL_FS_TYPE] == MSQL_DATE_TYPE
         AAdd(aRow, CToD(""))

      otherwise
         AAdd(aRow, nil)

      endcase
   next

   return TmSQLRow():New(aRow, ::aFieldStruct, ::cTable, .F.)

return nil


/* ----------------------------------------------------------------------------------------*/

// Every available mSQL server
CLASS TmSQLServer

   DATA  nSocket                 // connection handle to server
   DATA  cServer                 // server name
   DATA  cDBName                 // Selected DB
   DATA  lError                  // .T. if occurred an error

   METHOD   New(cServer)         // Open connection to a server, returns a server object
   METHOD   Destroy()            // Closes connection to server

   METHOD   SelectDB(cDBName)    // Which data base I will use for subsequent queries

   METHOD   CreateTable(cName, aStruct)         // Create new table using the same syntax of dbCreate()
   METHOD   DeleteTable(cName)                  // delete table
   METHOD   CreateIndex(cName, cTable, aFNames, lUnique) // Create an index (unique) on field name(s) passed as an array of strings aFNames
   METHOD   DeleteIndex(cName, cTable)                   // Delete index cName from cTable

   METHOD   Query(cQuery)        // Gets a textual query and returns a TmSQLQuery or TmSQLTable object

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error

ENDCLASS


METHOD New(cServer) CLASS TmSQLServer

   ::cServer := cServer
   ::cDBName := ""
   ::nSocket := msqlConnec(cServer)
   ::lError := .F.

   if ::nSocket == -1
      ::lError := .T.
   endif

return Self


METHOD SelectDB(cDBName) CLASS TmSQLServer

   if msqlSelect(::nSocket, cDBName) >= 0
      ::cDBName := cDBName
      return .T.
   else
      ::cDBName := ""
   endif

return .F.


METHOD CreateTable(cName, aStruct) CLASS TmSQLServer

   local cCreateQuery := "CREATE TABLE " + cName + " ("
   local i

   for i := 1 to Len(aStruct)
      do case
      case aStruct[i][DBS_TYPE] == "C"
         cCreateQuery += aStruct[i][DBS_NAME] + " char(" + AllTrim(Str(aStruct[i][DBS_LEN])) + "),"

      case aStruct[i][DBS_TYPE] == "N"
         if aStruct[i][DBS_DEC] == 0
            cCreateQuery += aStruct[i][DBS_NAME] + " int,"
         else
            cCreateQuery += aStruct[i][DBS_NAME] + " real,"
         endif

      case aStruct[i][DBS_TYPE] == "D"
         cCreateQuery += aStruct[i][DBS_NAME] + " date,"

      case aStruct[i][DBS_TYPE] == "L"
         cCreateQuery += aStruct[i][DBS_NAME] + " uint,"

      otherwise
         cCreateQuery += aStruct[i][DBS_NAME] + " char(" + AllTrim(Str(aStruct[i][DBS_LEN])) + "),"

      endcase

   next

   // remove last comma from list
   cCreateQuery := Left(cCreateQuery, Len(cCreateQuery) -1) + ")"

   if msqlQuery(::nSocket, cCreateQuery) == 1
      return .T.
   else
      ::lError := .T.
   endif

return .F.


METHOD CreateIndex(cName, cTable, aFNames, lUnique) CLASS TmSQLServer

   local cCreateQuery := "CREATE "
   local i

   default lUnique to .F.

   if lUnique
      cCreateQuery += "UNIQUE INDEX "
   else
      cCreateQuery += "INDEX "
   endif

   cCreateQuery += cName + " ON " + cTable + " ("

   for i := 1 to Len(aFNames)
      cCreateQuery += aFNames[i] + ","
   next

   // remove last comma from list
   cCreateQuery := Left(cCreateQuery, Len(cCreateQuery) -1) + ")"

   if msqlQuery(::nSocket, cCreateQuery) == 1
      return .T.
   endif

return .F.


METHOD DeleteIndex(cName, cTable) CLASS TmSQLServer

   local cDropQuery := "DROP INDEX " + cName + " FROM " + cTable

   if msqlQuery(::nSocket, cDropQuery) == 1
      return .T.
   endif

return .F.


METHOD DeleteTable(cName) CLASS TmSQLServer

   local cDropQuery := "DROP TABLE " + cName

   if msqlQuery(::nSocket, cDropQuery) == 1
      return .T.
   endif

return .F.


METHOD Query(cQuery) CLASS TmSQLServer

   local oQuery, cTableName, i, cUpperQuery, nNumTables, cToken

   cUpperQuery := Upper(AllTrim(cQuery))
   i := 1
   nNumTables := 0

   while __StrToken(cUpperQuery, i++, " ") <> "FROM"
   enddo
   while (cToken := __StrToken(cUpperQuery, i++, " ")) <> "WHERE" .AND. !Empty(cToken)
      cTableName := __StrToken(cQuery, i - 1, " ")
      nNumTables++
   enddo

   if nNumTables == 1
      oQuery := TmSQLTable():New(::nSocket, cQuery, cTableName)
   else
      oQuery := TmSQLQuery():New(::nSocket, cQuery)
   endif

   if oQuery:nNumRows < 0
      ::lError := .T.
   endif

return oQuery


METHOD Destroy() CLASS TmSQLServer

   msqlClose(::nSocket)

return Self


METHOD Error() CLASS TmSQLServer

   ::lError := .F.

return msqlGetErr()

