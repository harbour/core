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
#include "mysql.ch"



// Every single row of an answer
CLASS TMySQLRow

   DATA  aRow              // a single row of answer
   DATA  aDirty            // array of booleans set to .T. if corresponding field of aRow has been changed
   DATA  aOldValue         // If aDirty[n] is .T. aOldValue[n] keeps a copy of changed value if aRow[n] is part of a primary key

   DATA  aFieldStruct      // type of each field
   DATA  cTable            // Name of table containing this row, empty if TMySQLQuery returned this row

   METHOD   New(aRow, aFStruct, cTableName)     // Create a new Row object

   METHOD   FieldGet(nNum)             // Same as clipper ones
   METHOD   FieldPut(nNum, Value)
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
   ::aFieldStruct := aFStruct
   ::cTable := cTableName

   ::aDirty := Array(Len(::aRow))
   ::aOldValue := Array(Len(::aRow))

   AFill(::aDirty, .F.)

return Self


METHOD FieldGet(nNum) CLASS TMySQLRow

   if nNum > 0 .AND. nNum <= Len(::aRow)

      // Char fields are padded with spaces since a real .dbf field would be
      if ValType(::aRow[nNum]) == "C"
         return PadR(::aRow[nNum], ::aFieldStruct[nNum][MYSQL_FS_LENGTH])
      else
         return ::aRow[nNum]
      endif

   endif

return nil


METHOD FieldPut(nNum, Value) CLASS TMySQLRow

   if nNum > 0 .AND. nNum <= Len(::aRow)

      if Valtype(Value) == Valtype(::aRow[nNum]) .OR. Empty(::aRow[nNum])

         // if it is a char field remove trailing spaces
         if ValType(Value) == "C"
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

   local cUpperName, nPos := 1

   cUpperName := Upper(cFieldName)

   /* NOTE: this code block kills harbour if called a few thousand times
   nPos := AScan(::aFieldStruct, {|aItem| iif(Upper(aItem[MYSQL_FS_NAME]) == cUpperName, .T., .F.)})
   */

   while nPos <= Len(::aFieldStruct)
      if Upper(::aFieldStruct[nPos][MYSQL_FS_NAME]) == cUpperName
         exit
      endif
      nPos++
   enddo

return nPos


// Returns name of field N
METHOD FieldName(nNum) CLASS TMySQLRow

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      return ::aFieldStruct[nNum][MYSQL_FS_NAME]
   endif

return ""


METHOD FieldLen(nNum) CLASS TMySQLRow

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      return ::aFieldStruct[nNum][MYSQL_FS_LENGTH]
   endif

return ""


METHOD FieldDec(nNum) CLASS TMySQLRow

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      return ::aFieldStruct[nNum][MYSQL_FS_DECIMALS]
   endif

return ""


METHOD FieldType(nNum) CLASS TMySQLRow

   local cType := "U"

   if nNum >=1 .AND. nNum <= Len(::aFieldStruct)
      do case
         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
            cType := "L"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE
            cType := "N"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
            cType := "D"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
            cType := "M"

         case ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
              ::aFieldStruct[nNum][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE
            cType := "C"

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
      if (sqlAND(::aFieldStruct[nI][MYSQL_FS_FLAGS], PRI_KEY_FLAG) == PRI_KEY_FLAG) .OR.;
         (sqlAND(::aFieldStruct[nI][MYSQL_FS_FLAGS], MULTIPLE_KEY_FLAG) == MULTIPLE_KEY_FLAG)

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

   DATA  nNumFields        // how many fields per row
   DATA  aFieldStruct      // type of each field, a copy is here a copy inside each row

   DATA  lError            // .T. if last operation failed

   METHOD   New(nSocket, cQuery)       // New query object
   METHOD   Destroy()
   METHOD   Refresh()                  // ReExecutes the query (cQuery) so that changes to table are visible

   METHOD   GetRow(nRow)               // return Row n of answer

   METHOD   Skip(nRows)                // Same as clipper ones

   METHOD   Bof() INLINE ::nCurRow == 1
   METHOD   Eof() INLINE ::nCurRow == ::nNumRows
   METHOD   RecNo() INLINE ::nCurRow
   METHOD   LastRec() INLINE ::nNumRows

   METHOD   FCount()

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error and clears ::lError

ENDCLASS


METHOD New(nSocket, cQuery) CLASS TMySQLQuery

   local nI, aField, rc

   ::aFieldStruct := {}
   ::nSocket := nSocket
   ::cQuery := cQuery
   ::nCurRow := 1
   ::lError := .F.

   if (rc := sqlQuery(nSocket, cQuery)) == 0

      // save result set
      ::nResultHandle := sqlStoreR(nSocket)
      ::nNumRows := sqlNRows(::nResultHandle)
      ::nNumFields := sqlNumFi(::nResultHandle)

      for nI := 1 to ::nNumFields

         aField := sqlFetchF(::nResultHandle)
         AAdd(::aFieldStruct, aField)

      next

   else
      ::nResultHandle := nil
      ::nNumFields := 0
      ::nNumRows := 0
      ::lError := .T.

   endif

return Self


METHOD Refresh() CLASS TMySQLQuery

   local rc

   // free present result handle
   sqlFreeR(::nResultHandle)

   ::lError := .F.

   if (rc := sqlQuery(::nSocket, ::cQuery)) == 0

      // save result set
      ::nResultHandle := sqlStoreR(::nSocket)
      ::nNumRows := sqlNRows(::nResultHandle)

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      if ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      endif

   else
      ::aFieldStruct := {}
      ::nResultHandle := nil
      ::nNumFields := 0
      ::nNumRows := 0
      ::lError := .T.

   endif

return !::lError


METHOD Skip(nRows) CLASS TMySQLQuery

   // NOTE: MySQL row count starts from 0
   default nRows to 1

   if nRows == 0
      // No move

   elseif nRows < 0
      // Negative movement
      ::nCurRow := Max(::nCurRow + nRows, 1)

   else
      // positive movement
      ::nCurRow := Min(::nCurRow + nRows, ::nNumRows + 1)

   endif

   sqlDataS(::nResultHandle, ::nCurRow - 1)

return Self


/* Given a three letter month name gives back month number as two char string (ie. Apr -> 04) */
static function NMonth(cMonthValue)

   static cMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec" }
   local nMonth

   nMonth := AScan(cMonths, cMonthValue)

return PadL(nMonth, 2, "0")


// Get row n of a query and return it as a TMySQLRow object
METHOD GetRow(nRow) CLASS TMySQLQuery

   local aRow := NIL
   local oRow := NIL
   local i

   default nRow to 0

   if ::nResultHandle <> NIL
      if nRow >= 1 .AND. nRow <= ::nNumRows

         // NOTE: row count starts from 0
         sqlDataS(::nResultHandle, nRow - 1)
         ::nCurRow := nRow
      else
         ::nCurRow++
      endif

      aRow := sqlFetchR(::nResultHandle)

      if aRow <> NIL

         // Convert answer from text field to correct clipper types
         for i := 1 to ::nNumFields
            do case
               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
                  aRow[i] := iif(Val(aRow[i]) == 0, .F., .T.)

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE
                  aRow[i] := Val(aRow[i])

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
                  aRow[i] := Val(aRow[i])

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
                  if Empty(aRow[i])
                     aRow[i] := CToD("")
                  else
                     // Date format YYYY-MM-DD
                     aRow[i] := CToD(SubStr(aRow[i], 6, 2) + "/" + Right(aRow[i], 2) + "/" + Left(aRow[i], 4))
                  endif

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
                  // Memo field

               case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE .OR.;
                    ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE
                  // char field

               otherwise
                  Alert("Unknown type from SQL Server" + Str(::aFieldStruct[i][MYSQL_FS_TYPE]))

            endcase
         next

         oRow := TMySQLRow():New(aRow, ::aFieldStruct)
      endif
   endif

return iif(aRow == NIL, NIL, oRow)


// Free result handle and associated resources
METHOD Destroy() CLASS TMySQLQuery

   sqlFreeR(::nResultHandle)

return Self


METHOD FCount() CLASS TMySQLQuery

return ::nNumFields


METHOD Error() CLASS TMySQLQuery

   ::lError := .F.

return sqlGetErr(::nSocket)

/* ----------------------------------------------------------------------------------------*/

// A Table is a query without joins; this way I can Insert() e Delete() rows.
// NOTE: it's always a SELECT result, so it will contain a full table only if
//       SELECT * FROM ... was issued
CLASS TMySQLTable FROM TMySQLQuery

   DATA  cTable               // name of table

   METHOD   New(nSocket, cQuery, cTableName)
   METHOD   GetRow(nRow)

   METHOD   Update(oRow)      // Gets an oRow and updates changed fields
   METHOD   Delete(oRow)      // Deletes passed row from table
   METHOD   Append(oRow)      // Inserts passed row into table
   METHOD   GetBlankRow()     // Returns an empty row with all available fields empty

ENDCLASS


METHOD New(nSocket, cQuery, cTableName) CLASS TMySQLTable

   super:New(nSocket, AllTrim(cQuery))

   ::cTable := Lower(cTableName)

return Self


METHOD GetRow(nRow) CLASS TMySQLTable

   local oRow := super:GetRow(nRow)

   if oRow <> NIL
      oRow:cTable := ::cTable
   endif

return oRow


/* Creates an update query for changed fields and submits it to server */
METHOD Update(oRow) CLASS TMySQLTable

   local cUpdateQuery := "UPDATE " + ::cTable + " SET "
   local i, cField

   ::lError := .F.

   // is this a row of this table ?
   if oRow:cTable == ::cTable

      for i := 1 to Len(oRow:aRow)

         if oRow:aDirty[i]
            cUpdateQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + "=" + ClipValue2SQL(oRow:aRow[i]) + ","
         endif
      next

      // remove last comma
      cUpdateQuery := Left(cUpdateQuery, Len(cUpdateQuery) -1)

      cUpdateQuery += oRow:MakePrimaryKeyWhere()

      if sqlQuery(::nSocket, cUpdateQuery) == 0

         // All values are commited
         Afill(oRow:aDirty, .F.)
         Afill(oRow:aOldValue, nil)

      else
         ::lError := .T.

      endif

   endif

return !::lError


METHOD Delete(oRow) CLASS TMySQLTable

   local cDeleteQuery := "DELETE FROM " + ::cTable

   // is this a row of this table ?
   if oRow:cTable == ::cTable

      cDeleteQuery += oRow:MakePrimaryKeyWhere()

      if sqlQuery(::nSocket, cDeleteQuery) == 0
         ::lError := .F.

      else
         ::lError := .T.

      endif

  endif

return !::lError


// Adds a row with values passed into oRow
METHOD Append(oRow) CLASS TMySQLTable

   local cInsertQuery := "INSERT INTO " + ::cTable + " ("
   local i, cField

   // is this a row of this table ?
   if oRow:cTable == ::cTable

      // field names
      for i := 1 to Len(oRow:aRow)
         cInsertQuery += oRow:aFieldStruct[i][MYSQL_FS_NAME] + ","
      next
      // remove last comma from list
      cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ") VALUES ("

      // field values
      for i := 1 to Len(oRow:aRow)
         cInsertQuery += ClipValue2SQL(oRow:aRow[i]) + ","

      next

      // remove last comma from list of values and add closing parenthesis
      cInsertQuery := Left(cInsertQuery, Len(cInsertQuery) -1) + ")"

      if sqlQuery(::nSocket, cInsertQuery) == 0
         return .T.
      else
         ::lError := .T.
      endif

   endif

return .F.


METHOD GetBlankRow() CLASS TMySQLTable

   local i
   local aRow := Array(::FCount())

   // crate an array of empty fields
   for i := 1 to ::FCount()

      do case
      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_STRING_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_VAR_STRING_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_BLOB_TYPE
         aRow[i] := ""

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_SHORT_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_LONG_TYPE
         aRow[i] := 0

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_TINY_TYPE
         aRow[i] := .F.

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DOUBLE_TYPE .OR.;
           ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_FLOAT_TYPE
         aRow[i] := 0.0

      case ::aFieldStruct[i][MYSQL_FS_TYPE] == MYSQL_DATE_TYPE
         aRow[i] := CToD("")

      otherwise
         aRow[i] := nil

      endcase
   next

   return TMySQLRow():New(aRow, ::aFieldStruct, ::cTable, .F.)

return nil


/* ----------------------------------------------------------------------------------------*/

// Every available MySQL server
CLASS TMySQLServer

   DATA  nSocket                 // connection handle to server (currently pointer to a MYSQL structure)
   DATA  cServer                 // server name
   DATA  cDBName                 // Selected DB
   DATA  cUser                   // user accessing db
   DATA  cPassword               // his/her password
   DATA  lError                  // .T. if occurred an error

   METHOD   New(cServer, cUser, cPassword)   // Opens connection to a server, returns a server object
   METHOD   Destroy()                        // Closes connection to server

   METHOD   SelectDB(cDBName)    // Which data base I will use for subsequent queries

   METHOD   CreateTable(cTable, aStruct)  // Create new table using the same syntax of dbCreate()
   METHOD   DeleteTable(cTable)           // delete table
   METHOD   TableStruct(cTable)           // returns a structure array compatible with clipper's dbStruct() ones
   METHOD   CreateIndex(cName, cTable, aFNames, lUnique) // Create an index (unique) on field name(s) passed as an array of strings aFNames
   METHOD   DeleteIndex(cName, cTable)                   // Delete index cName from cTable

   METHOD   ListDBs()            // returns an array with list of data bases available
   METHOD   ListTables()         // returns an array with list of available tables in current database

   METHOD   Query(cQuery)        // Gets a textual query and returns a TMySQLQuery or TMySQLTable object

   METHOD   NetErr() INLINE ::lError         // Returns .T. if something went wrong
   METHOD   Error()                          // Returns textual description of last error

ENDCLASS


METHOD New(cServer, cUser, cPassword) CLASS TMySQLServer

   ::cServer := cServer
   ::cUser := cUser
   ::cPassword := cPassword
   ::nSocket := sqlConnect(cServer, cUser, cPassword)
   ::lError := .F.

   if ::nSocket == 0
      ::lError := .T.
   endif

return Self

METHOD Destroy() CLASS TMySQLServer

   sqlClose(::nSocket)

return Self


METHOD SelectDB(cDBName) CLASS TMySQLServer

   if sqlSelectD(::nSocket, cDBName) == 0
      ::cDBName := cDBName
      return .T.
   else
      ::cDBName := ""
   endif

return .F.


// NOTE: OS/2 port of MySQL is picky about table names, that is if you create a table with
// an upper case name you cannot alter it (for example) using a lower case name, this violates
// OS/2 case insensibility about names
METHOD CreateTable(cTable, aStruct) CLASS TMySQLServer

   /* NOTE: all table names are created with lower case */
   local cCreateQuery := "CREATE TABLE " + Lower(cTable) + " ("
   local i

   // returns NOT NULL if extended structure has DBS_NOTNULL field to true
   local cNN := {|aArr| iif(Len(aArr) > DBS_DEC, iif(aArr[DBS_NOTNULL], " NOT NULL ", ""), "")}

   for i := 1 to Len(aStruct)
      do case
      case aStruct[i][DBS_TYPE] == "C"
         cCreateQuery += aStruct[i][DBS_NAME] + " char(" + AllTrim(Str(aStruct[i][DBS_LEN])) + ")" + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "M"
         cCreateQuery += aStruct[i][DBS_NAME] + " text" + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "N"
         if aStruct[i][DBS_DEC] == 0
            cCreateQuery += aStruct[i][DBS_NAME] + " int(" + AllTrim(Str(aStruct[i][DBS_LEN])) + ")" + Eval(cNN, aStruct[i]) + ","
         else
            cCreateQuery += aStruct[i][DBS_NAME] + " real(" + AllTrim(Str(aStruct[i][DBS_LEN])) + "," + AllTrim(Str(aStruct[i][DBS_DEC])) + ")" + Eval(cNN, aStruct[i]) + ","
         endif

      case aStruct[i][DBS_TYPE] == "D"
         cCreateQuery += aStruct[i][DBS_NAME] + " date " + Eval(cNN, aStruct[i]) + ","

      case aStruct[i][DBS_TYPE] == "L"
         cCreateQuery += aStruct[i][DBS_NAME] + " tinyint "  + Eval(cNN, aStruct[i]) + ","

      otherwise
         cCreateQuery += aStruct[i][DBS_NAME] + " char(" + AllTrim(Str(aStruct[i][DBS_LEN])) + ")" + Eval(cNN, aStruct[i]) + ","

      endcase

   next

   // remove last comma from list
   cCreateQuery := Left(cCreateQuery, Len(cCreateQuery) -1) + ")"

   if sqlQuery(::nSocket, cCreateQuery) == 0
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

   if sqlQuery(::nSocket, cCreateQuery) == 0
      return .T.

   endif

return .F.


METHOD DeleteIndex(cName, cTable) CLASS TMySQLServer

   local cDropQuery := "DROP INDEX " + cName + " FROM " + Lower(cTable)

   if sqlQuery(::nSocket, cDropQuery) == 0
      return .T.
   endif

return .F.


METHOD DeleteTable(cTable) CLASS TMySQLServer

   local cDropQuery := "DROP TABLE " + Lower(cTable)

   if sqlQuery(::nSocket, cDropQuery) == 0
      return .T.

   endif

return .F.


METHOD Query(cQuery) CLASS TMySQLServer

   local oQuery, cTableName, i, cUpperQuery, nNumTables, cToken

   cUpperQuery := Upper(AllTrim(cQuery))
   i := 1
   nNumTables := 1

   while (cToken := __StrToken(cUpperQuery, i++, " ")) <> "FROM" .AND. !Empty(cToken)
   enddo

   // first token after "FROM" is a table name
   // NOTE: SubSelects ?
   cTableName := __StrToken(cUpperQuery, i++, " ")

   while (cToken := __StrToken(cUpperQuery, i++, " ")) <> "WHERE" .AND. !Empty(cToken)
      // do we have more than one table referenced ?
      if cToken == "," .OR. cToken == "JOIN"
         nNumTables++
      endif
   enddo

   if nNumTables == 1
      oQuery := TMySQLTable():New(::nSocket, cQuery, cTableName)
   else
      oQuery := TMySQLQuery():New(::nSocket, cQuery)
   endif

   if oQuery:NetErr()
      ::lError := .T.
   endif

return oQuery


METHOD Error() CLASS TMySQLServer

   ::lError := .F.

return sqlGetErr(::nSocket)


METHOD ListDBs() CLASS TMySQLServer

   local aList

   aList := sqlListDB(::nSocket)

return aList


METHOD ListTables() CLASS TMySQLServer

   local aList

   aList := sqlListTbl(::nSocket)

return aList


/* TOFIX: Conversion creates a .dbf with fields of wrong dimension (often) */
METHOD TableStruct(cTable) CLASS TMySQLServer

   local nRes, aField, aStruct, aSField, i


   aStruct := {}

   /* TODO: rewrite for MySQL
   nRes := sqlListF(::nSocket, cTable)

   if nRes > 0
      for i := 1 to sqlNumFi(nRes)

         aField := sqlFetchF(nRes)
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

            otherwise

            endcase

            AAdd(aStruct, aSField)
         endif
      next

      sqlFreeR(nRes)

   endif*/

return aStruct


// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
static function ClipValue2SQL(Value)

   local cValue := ""

   do case
      case Valtype(Value) == "N"
         cValue := AllTrim(Str(Value))

      case Valtype(Value) == "D"
         if !Empty(Value)
            // MySQL dates are like YYYY-MM-DD
            cValue := "'" + Str(Year(Value), 4) + "-" + PadL(Month(Value), 2, "0") + "-" + PadL(Day(Value), 2, "0") + "'"
         else
            cValue := "''"
         endif

      case Valtype(Value) $ "CM"
         cValue := "'" + StrTran(Value, "'", "\'") + "'"

      case Valtype(Value) == "L"
         cValue := AllTrim(Str(iif(Value == .F., 0, 1)))

      otherwise
         cValue := "''"       // NOTE: Here we lose values we cannot convert

   endcase

return cValue

