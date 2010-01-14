/*
 * $Id$
 */

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
 * See COPYING for licensing terms.
 *
 */

#include "common.ch"
#include "hbclass.ch"
#include "postgres.ch"

CREATE CLASS TPQServer
    VAR      pDb
    VAR      lTrans
    VAR      lallCols  INIT .T.
    VAR      Schema    INIT "public"
    VAR      lError    INIT .F.
    VAR      cError    INIT ""
    VAR      lTrace    INIT .F.
    VAR      pTrace

    METHOD   New( cHost, cDatabase, cUser, cPass, nPort, Schema )
    METHOD   Destroy()
    METHOD   Close()              INLINE ::Destroy()

    METHOD   StartTransaction()
    METHOD   TransactionStatus()  INLINE PQtransactionstatus(::pDb)
    METHOD   Commit()
    METHOD   Rollback()

    METHOD   Query( cQuery )
    METHOD   Execute( cQuery )    INLINE ::Query(cQuery)
    METHOD   SetSchema( cSchema )

    METHOD   NetErr()             INLINE ::lError
    METHOD   ErrorMsg()           INLINE ::cError

    METHOD   TableExists( cTable )
    METHOD   ListTables()
    METHOD   TableStruct( cTable )
    METHOD   CreateTable( cTable, aStruct )
    METHOD   DeleteTable( cTable  )
    METHOD   TraceOn(cFile)
    METHOD   TraceOff()
    METHOD   SetVerbosity(num)    INLINE PQsetErrorVerbosity( ::pDb, iif( num >= 0 .and. num <= 2, num, 1 )  )

    //DESTRUCTOR Destroy
ENDCLASS


METHOD New( cHost, cDatabase, cUser, cPass, nPort, Schema ) CLASS TPQserver
    Local res
    DEFAULT nPort TO 5432

    ::pDB := PQconnect(cDatabase, cHost, cUser, cPass, nPort)

    if PQstatus(::pDb) != CONNECTION_OK
        ::lError := .T.
        ::cError := PQerrormessage(::pDb)

    else
        if ! Empty(Schema)
            ::SetSchema(Schema)
        else
            res := PQexec( ::pDB, "SELECT current_schema()" )
            if PQresultStatus(res) == PGRES_TUPLES_OK
                ::Schema := PQgetvalue( res, 1, 1 )
            endif
            PQclear(res)
        endif
    endif

RETURN self


METHOD Destroy() CLASS TPQserver
    ::TraceOff()
    PQClose(::pDb)
RETURN nil


METHOD SetSchema( cSchema ) CLASS TPQserver
    Local res
    Local result := .F.

    if PQstatus(::pDb) == CONNECTION_OK
        ::Schema := cSchema
        res := PQexec( ::pDB, "SET search_path TO " + cSchema )
        result := (PQresultStatus(res) == PGRES_COMMAND_OK)
        PQclear(res)
    endif
RETURN result


METHOD StartTransaction() CLASS TPQserver
    Local res, lError

    res    := PQexec( ::pDB, "BEGIN" )
    lError := PQresultstatus(res) != PGRES_COMMAND_OK

    if lError
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    else
        ::lError := .F.
        ::cError := ""
    endif
    PQclear(res)
RETURN lError


METHOD Commit() CLASS TPQserver
    Local res, lError

    res    := PQexec( ::pDB, "COMMIT" )
    lError := PQresultstatus(res) != PGRES_COMMAND_OK

    if lError
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    else
        ::lError := .F.
        ::cError := ""
    endif
    PQclear(res)
RETURN lError


METHOD Rollback() CLASS TPQserver
    Local res, lError

    res    := PQexec( ::pDB, "ROLLBACK" )
    lError := PQresultstatus(res) != PGRES_COMMAND_OK

    if lError
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    else
        ::lError := .F.
        ::cError := ""
    endif
    PQclear(res)
RETURN lError


METHOD Query( cQuery ) CLASS TPQserver
    Local oQuery

    oQuery := TPQquery():New(::pDB, cQuery, ::lallCols, ::Schema)
RETURN oQuery


METHOD TableExists( cTable ) CLASS TPQserver
    Local result := .F.
    Local cQuery
    Local res

    cQuery := "select table_name "
    cQuery += "  from information_schema.tables "
    cQuery += " where table_type = 'BASE TABLE' and table_schema = " + DataToSql(::Schema) + " and table_name = " + DataToSql(lower(cTable))

    res := PQexec( ::pDB, cQuery )

    if PQresultstatus(res) == PGRES_TUPLES_OK
        result := (PQlastrec(res) != 0)
        ::lError := .F.
        ::cError := ""
    else
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    endif

    PQclear(res)
RETURN result


METHOD ListTables() CLASS TPQserver
    Local result := {}
    Local cQuery
    Local res
    Local i

    cQuery := "select table_name "
    cQuery += "  from information_schema.tables "
    cQuery += " where table_schema = " + DataToSql(::Schema) + " and table_type = 'BASE TABLE' "

    res := PQexec( ::pDB, cQuery )

    if PQresultstatus(res) == PGRES_TUPLES_OK
        For i := 1 to PQlastrec(res)
            aadd( result, PQgetvalue( res, i, 1 ) )
        Next
        ::lError := .F.
        ::cError := ""
    else
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    endif

    PQclear(res)
RETURN result

METHOD TableStruct( cTable ) CLASS TPQserver
    Local result := {}
    Local cQuery
    Local res
    Local i
    Local cField
    Local cType
    Local nSize
    Local nDec

    cQuery := "SELECT column_name, data_type, character_maximum_length, numeric_precision, numeric_scale "
    cQuery += "  FROM information_schema.columns "
    cQuery += " WHERE table_schema = " + DataToSql(::Schema) + " and table_name = " + DataToSql(lower(cTable))
    cQuery += "ORDER BY ordinal_position "

    res := PQexec( ::pDB, cQuery )

    if PQresultstatus(res) == PGRES_TUPLES_OK
        For i := 1 to PQlastrec(res)
            cField    := PQgetvalue(res, i, 1)
            cType     := PQgetvalue(res, i, 2)
            nSize     := PQgetvalue(res, i, 4)
            nDec      := PQgetvalue(res, i, 5)

            if "char" $ cType
                cType := "C"
                nSize := Val(PQgetvalue(res, i, 3))
                nDec  := 0

            elseif "text" $ cType
                cType := "M"
                nSize := 10
                nDec := 0

            elseif "boolean" $ cType
                cType := "L"
                nSize := 1
                nDec  := 0

            elseif "smallint" $ cType
                cType := "N"
                nSize := 5
                nDec  := 0

            elseif "integer" $ cType .or. "serial" $ cType
                cType := "N"
                nSize := 9
                nDec  := 0

            elseif "bigint" $ cType .or. "bigserial" $ cType
                cType := "N"
                nSize := 19
                nDec  := 0

            elseif "decimal" $ cType .or. "numeric" $ cType
                cType := "N"
                nDec  := val(nDec)
                // Postgres don't store ".", but .dbf does, it can cause data width problem
                nSize := val(nSize) + iif( nDec > 0, 1, 0 )

                // Numeric/Decimal without scale/precision can genarete big values, so, i limit this to 10,5

                if nDec > 100
                    nDec := 5
                endif

                if nSize > 100
                    nSize := 15
                endif

            elseif "real" $ cType .or. "float4" $ cType
                cType := "N"
                nSize := 15
                nDec  :=  4

            elseif "double precision" $ cType .or. "float8" $ cType
                cType := "N"
                nSize := 19
                nDec  := 9

            elseif "money" $ cType
                cType := "N"
                nSize := 9
                nDec  := 2

            elseif "timestamp" $ cType
                cType := "C"
                nSize := 20
                nDec  := 0

            elseif "date" $ cType
                cType := "D"
                nSize := 8
                nDec  := 0

            elseif "time" $ cType
                cType := "C"
                nSize := 10
                nDec  := 0

            else
                // Unsuported
                cType := "U"
                nSize := 0
                nDec  := -1

            endif

            if !( cType == "U" )
                aadd( result, { cField, cType, nSize, nDec } )
            endif

        Next
        ::lError := .F.
        ::cError := ""
    else
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    endif

    PQclear(res)
RETURN result

METHOD CreateTable( cTable, aStruct ) CLASS TPQserver
    Local result := .T.
    Local cQuery
    Local res
    Local i

    cQuery := "CREATE TABLE " + ::Schema + "." + cTable + "( "

    For i := 1 to Len(aStruct)

        cQuery += aStruct[i, 1]

        if aStruct[ i, 2 ] == "C"
            cQuery += " Char(" + hb_NToS(aStruct[i, 3]) + ")"

        elseif aStruct[ i, 2 ] == "D"
            cQuery += " Date "

        elseif aStruct[ i, 2 ] == "N"
            cQuery += " Numeric(" + hb_NToS(aStruct[i, 3]) + "," + hb_NToS(aStruct[i,4]) + ")"

        elseif aStruct[ i, 2 ] == "L"
            cQuery += " boolean "

        elseif aStruct[ i, 2 ] == "M"
            cQuery += " text "
        endif

        if i == Len(aStruct)
            cQuery += ")"
        else
            cQuery += ","
        endif
    Next

    res := PQexec( ::pDB, cQuery )

    if PQresultstatus(res) != PGRES_COMMAND_OK
        result := .F.
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    else
        ::lError := .F.
        ::cError := ""
    endif

    PQclear(res)
RETURN result


METHOD DeleteTable( cTable ) CLASS TPQserver
    Local result := .T.
    Local res

    res := PQexec( ::pDB, "DROP TABLE " + ::Schema + "." + cTable  )

    if PQresultstatus(res) != PGRES_COMMAND_OK
        result := .F.
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    else
        ::lError := .F.
        ::cError := ""
    endif

    PQclear(res)
RETURN result


METHOD TraceOn( cFile ) CLASS TPQserver
    ::pTrace := PQcreatetrace( cFile )

    if ::pTrace != NIL
        PQtrace( ::pDb, ::pTrace )
        ::lTrace := .t.
    endif
RETURN nil


METHOD TraceOff() CLASS TPQserver
    if ::pTrace != NIL
        PQuntrace( ::pDb )
        PQclosetrace( ::pTrace )
    endif

    ::lTrace := .f.
RETURN nil



CREATE CLASS TPQQuery
    VAR      pQuery
    VAR      pDB

    VAR      nResultStatus

    VAR      lBof
    VAR      lEof
    VAR      lRead
    VAR      lAllCols INIT .T.

    VAR      lError   INIT .F.
    VAR      cError   INIT ""

    VAR      cQuery
    VAR      nRecno
    VAR      nFields
    VAR      nLastrec

    VAR      aStruct
    VAR      aKeys
    VAR      TableName
    VAR      Schema
    VAR      rows     INIT 0

    METHOD   New( pDB, cQuery, lallCols, cSchema, res )
    METHOD   Destroy()
    METHOD   Close()            INLINE ::Destroy()

    METHOD   Refresh( lQuery, lMeta )
    METHOD   Fetch()            INLINE ::Skip()
    METHOD   Read()
    METHOD   Skip( nRecno )

    METHOD   Bof()              INLINE ::lBof
    METHOD   Eof()              INLINE ::lEof
    METHOD   RecNo()            INLINE ::nRecno
    METHOD   Lastrec()          INLINE ::nLastrec
    METHOD   Goto(nRecno)

    METHOD   NetErr()           INLINE ::lError
    METHOD   ErrorMsg()         INLINE ::cError

    METHOD   FCount()           INLINE ::nFields
    METHOD   FieldName( nField )
    METHOD   FieldPos( cField )
    METHOD   FieldLen( nField )
    METHOD   FieldDec( nField )
    METHOD   FieldType( nField )
    METHOD   Update( oRow )
    METHOD   Delete( oRow )
    METHOD   Append( oRow )
    METHOD   SetKey()

    METHOD   Changed(nField)    INLINE ::aRow[nField] != ::aOld[nField]
    METHOD   Blank()            INLINE ::GetBlankRow()

    METHOD   Struct()

    METHOD   FieldGet( nField, nRow )
    METHOD   GetRow( nRow )
    METHOD   GetBlankRow()

    //DESTRUCTOR Destroy
ENDCLASS


METHOD New( pDB, cQuery, lallCols, cSchema, res ) CLASS TPQquery
    ::pDB      := pDB
    ::nResultStatus := -1
    ::cQuery   := cQuery
    ::lallCols := lallCols
    ::Schema   := cSchema

    if res != NIL
        ::pQuery := res
    endif

    ::Refresh( res == NIL )
RETURN self


METHOD Destroy() CLASS TPQquery
    if !( ::nResultStatus == -1 )
        PQclear( ::pQuery )
        ::nResultStatus := -1
    endif
RETURN .T.


METHOD Refresh(lQuery,lMeta) CLASS TPQquery
    Local res
    Local aStruct := {}
    Local aTemp
    Local i
    Local cType, nDec, nSize

    Default lQuery To .T.
    Default lMeta To .T.

    ::Destroy()

    ::lBof     := .T.
    ::lEof     := .T.
    ::lRead    := .F.
    ::nRecno   := 0
    ::nLastrec := 0
    ::Rows     := 0

    if lQuery
        res := PQexec( ::pDB, ::cQuery )
    else
        res := ::pQuery
    endif

    ::nResultStatus := PQresultstatus(res)

    if ::nResultStatus == PGRES_TUPLES_OK

       if lMeta
          ::aStruct  := {}
          ::nFields  := 0
          // Get some information about metadata
           aTemp := PQmetadata(res)
           if ISARRAY(aTemp)
              For i := 1 to Len(aTemp)
                   cType := aTemp[ i, 2 ]
                   nSize := aTemp[ i, 3 ]
                   nDec  := aTemp[ i, 4 ]

                   if "char" $ cType
                       cType := "C"

                   elseif "numeric" $ cType .or. "decimal" $ cType
                       cType := "N"

                       // Postgres don't store ".", but .dbf does, it can cause data width problem
                       if nDec > 0
                          nSize++
                          // Numeric/Decimal without scale/precision can genarete big values, so, i limit this to 10,5
                          if nDec > 100
                             nDec := 5
                          endif
                       endif

                       if nSize > 100
                          nSize := 15
                       endif

                   elseif "date" $ cType
                       cType := "D"
                       nSize := 8

                   elseif "text" $ cType
                       cType := "M"

                   elseif "boolean" $ cType
                       cType := "L"
                       nSize := 1

                   elseif "smallint" $ cType
                       cType := "N"
                       nSize := 5

                   elseif "integer" $ cType .or. "serial" $ cType
                       cType := "N"
                       nSize := 9

                   elseif "bigint" $ cType .or. "bigserial" $ cType
                       cType := "N"
                       nSize := 19

                   elseif "real" $ cType .or. "float4" $ cType
                       cType := "N"
                       nSize := 15
                       nDec  :=  4

                   elseif "double precision" $ cType .or. "float8" $ cType
                       cType := "N"
                       nSize := 19
                       nDec  := 9

                   elseif "money" $ cType
                       cType := "N"
                       nSize := 10
                       nDec  := 2

                   elseif "timestamp" $ cType
                       cType := "C"
                       nSize := 20

                   elseif "time" $ cType
                       cType := "C"
                       nSize := 10

                   else
                       // Unsuported
                       cType := "K"
                   endif

                   aadd( aStruct, {aTemp[ i, 1 ], cType, nSize, nDec, aTemp[i, 5], aTemp[i, 6]} )
               Next

               ::nFields := PQfcount(res)

               ::aStruct := aStruct

           endif
        endif

        ::nLastrec := PQlastrec(res)
        ::lError := .F.
        ::cError := ""

        if ::nLastrec != 0
           ::nRecno := 1
           ::lBof := .F.
           ::lEof := .F.
        endif

    elseif ::nResultStatus == PGRES_COMMAND_OK
        ::lError := .F.
        ::cError := ""
        ::rows   := val(PQcmdTuples(res))

    else
        ::lError := .T.
        ::cError := PQresultErrormessage(res)
    endif

    ::pQuery := res

RETURN ! ::lError


METHOD Struct() CLASS TPQquery
    Local result := {}
    Local i

    For i := 1 to Len(::aStruct)
        aadd( result, { ::aStruct[i, 1], ::aStruct[i, 2], ::aStruct[i, 3], ::aStruct[i, 4] })
    Next
RETURN result

METHOD Read() CLASS TPQquery

   if !::lEof
      if !::lRead
       ::lRead := .T.
      else
       ::Skip( 1 )
      endif
   endif

RETURN !::lEof

METHOD Skip( nrecno ) CLASS TPQquery
    DEFAULT nRecno TO 1

    if ::nRecno + nRecno > 0 .and. ::nRecno + nRecno <= ::nLastrec
        ::nRecno := ::nRecno + nRecno
        ::lEof := .F.
        ::lBof := .F.

    else
        if ::nRecno + nRecno > ::nLastRec
            ::nRecno := ::nLastRec + 1
            ::lEof := .T.
        endif

        if ::nRecno + nRecno < 1
            ::nRecno := 1
            ::lBof := .T.
        endif
    endif
RETURN .T.


METHOD Goto( nRecno ) CLASS TPQquery
    if nRecno > 0 .and. nRecno <= ::nLastrec
        ::nRecno := nRecno
        ::lEof := .F.
    endif
RETURN .T.


METHOD FieldPos( cField ) CLASS TPQquery

    cField := trim(Lower(cField))

RETURN AScan( ::aStruct, {|x| x[1] == cField })

METHOD FieldName( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    elseif nField < 1 .or. nField > len(::aStruct)
        nField := 0
    endif

    if nField > 0
        result := ::aStruct[nField, 1]
    endif
RETURN result


METHOD FieldType( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    elseif nField < 1 .or. nField > len(::aStruct)
        nField := 0
    endif

    if nField > 0
        result := ::aStruct[nField, 2]
    endif
RETURN result


METHOD FieldLen( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    elseif nField < 1 .or. nField > len(::aStruct)
        nField := 0
    endif

    if nField > 0
        result := ::aStruct[nField, 3]
    endif
RETURN result


METHOD FieldDec( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    elseif nField < 1 .or. nField > len(::aStruct)
        nField := 0
    endif

    if nField > 0
        result := ::aStruct[nField, 4]
    endif
RETURN result


METHOD Delete(oRow) CLASS TPQquery
    Local res
    Local i
    Local nField
    Local xField
    Local cQuery
    Local cWhere := ""
    Local aParams := {}

    ::SetKey()

    if ! Empty(::Tablename) .and. ! Empty(::aKeys)
        For i := 1 to len(::aKeys)
            nField := oRow:Fieldpos(::aKeys[i])
            xField := oRow:FieldGetOld(nField)

            cWhere += ::aKeys[i] + " = $" + hb_NToS(i)

            AADD( aParams, ValueToString(xField) )

            if i != len(::aKeys)
                cWhere += " and "
            endif
        Next

        if ! (cWhere == "")
            cQuery := "DELETE FROM " + ::Schema + "." + ::Tablename + " WHERE " + cWhere
            res := PQexecParams( ::pDB, cQuery, aParams)

            if PQresultstatus(res) != PGRES_COMMAND_OK
                ::lError := .T.
                ::cError := PQresultErrormessage(res)
                ::rows   := 0
            else
                ::lError := .F.
                ::cError := ""
                ::rows   := val(PQcmdTuples(res))
            endif
            PQclear(res)
        endif
    else
        ::lError := .T.
        ::cError := "There is no primary keys or query is a joined table"
    endif
RETURN ! ::lError


METHOD Append( oRow ) CLASS TPQquery
    Local cQuery
    Local i
    Local res
    Local lChanged := .f.
    Local aParams := {}
    Local nParams := 0

    ::SetKey()

    if ! Empty(::Tablename)
        cQuery := "INSERT INTO " + ::Schema + "." + ::Tablename + "("
        For i := 1 to oRow:FCount()
            if ::lallCols .or. oRow:changed(i)
                lChanged := .t.
                cQuery += oRow:Fieldname(i) + ","
            endif
        Next

        cQuery := Left( cQuery, len(cQuery) - 1 ) +  ") VALUES ("

        For i := 1 to oRow:FCount()
            if ::lallCols .or. oRow:Changed(i)
                nParams++
                cQuery += "$" + hb_NToS(nParams) + ","
                aadd( aParams, ValueToString(oRow:FieldGet(i)) )
            endif
        Next

        cQuery := Left( cQuery, len(cQuery) - 1  ) + ")"

        if lChanged
            res := PQexecParams( ::pDB, cQuery, aParams)

            if PQresultstatus(res) != PGRES_COMMAND_OK
                ::lError := .T.
                ::cError := PQresultErrormessage(res)
                ::rows   := 0
            else
                ::lError := .F.
                ::cError := ""
                ::rows   := val(PQcmdTuples(res))
            endif

            PQclear(res)
        endif
    else
        ::lError := .T.
        ::cError := "Cannot insert in a joined table, or unknown error"
    endif
RETURN ! ::lError


METHOD Update(oRow) CLASS TPQquery
    Local cQuery
    Local i
    Local nField
    Local xField
    Local cWhere
    Local res
    Local lChanged := .f.
    Local aParams := {}
    Local nParams := 0

    ::SetKey()

    if ! Empty(::Tablename) .and. ! Empty(::aKeys)
        cWhere := ""
        For i := 1 to len(::aKeys)

            nField := oRow:Fieldpos(::aKeys[i])
            xField := oRow:FieldGetOld(nField)

            cWhere += ::aKeys[i] + "=" + DataToSql(xField)

            if i != len(::aKeys)
                cWhere += " and "
            endif
        Next

        cQuery := "UPDATE " + ::Schema + "." + ::Tablename + " SET "
        For i := 1 to oRow:FCount()
            if ::lallcols .or. oRow:Changed(i)
                lChanged := .t.
                nParams++
                cQuery += oRow:Fieldname(i) + " = $" + hb_NToS(nParams) + ","
                aadd( aParams, ValueToString(oRow:FieldGet(i)) )
            endif
        Next

        if ! (cWhere == "") .and. lChanged

            cQuery := Left( cQuery, len(cQuery) - 1 ) + " WHERE " + cWhere

            res := PQexecParams( ::pDB, cQuery, aParams)

            if PQresultstatus(res) != PGRES_COMMAND_OK
                ::lError := .T.
                ::cError := PQresultErrormessage(res)
                ::rows   := 0
            else
                ::lError := .F.
                ::cError := ""
                ::rows   := val(PQcmdTuples(res))
            endif

            PQclear(res)
        endif
    else
        ::lError := .T.
        ::cError := "Cannot insert in a joined table, or unknown error"
    endif
RETURN ! ::lError


METHOD FieldGet( nField, nRow ) CLASS TPQquery
    Local result
    Local cType

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    elseif nField < 1 .or. nField > ::nFields
        nField := 0
    endif

    if nField > 0 .and. ::nResultStatus == PGRES_TUPLES_OK

        if nRow == NIL
            nRow := ::nRecno
        endif

        result := PQgetvalue( ::pQuery, nRow, nField)
        cType := ::aStruct[ nField, 2 ]
        //nSize := ::aStruct[ nField, 3 ]

        if cType == "C"
            if result == NIL
                result := ""
            else
                result := result
            endif

        elseif cType == "N"
            if result != NIL
                result := val(result)
            else
                result := 0
            endif

        elseif cType == "D"
            if result != NIL
                result := hb_SToD( strtran( result, "-", "" ) )
            else
                result := hb_SToD("")
            endif

        elseif cType == "L"
            if result != NIL
                result := (result == "t")
            else
                result := .F.
            endif

        elseif cType == "M"
            if result == NIL
                result := ""
            else
                result := result
            endif

        endif
    endif
RETURN result


METHOD Getrow( nRow ) CLASS TPQquery
    Local result, aRow := {}, aOld := {}, nCol

    DEFAULT nRow TO ::nRecno

    if ::nResultStatus == PGRES_TUPLES_OK

        if nRow > 0 .and. nRow <= ::nLastRec

            ASize(aRow, ::nFields)
            ASize(aOld, ::nFields)

            For nCol := 1 to ::nFields
                aRow[nCol] := ::Fieldget(nCol, nRow)
                aOld[nCol] := ::Fieldget(nCol, nRow)
            Next

            result := TPQRow():New( aRow, aOld, ::aStruct )

        elseif nRow > ::nLastrec
            result := ::GetBlankRow()
        endif
    endif
RETURN result


METHOD GetBlankRow() CLASS TPQquery
    Local result, aRow := {}, aOld := {}, i

    ASize(aRow, ::nFields)
    ASize(aOld, ::nFields)

    For i := 1 to ::nFields
        if ::aStruct[i, 2] == "C"
            aRow[i] := ""
            aOld[i] := ""
        elseif ::aStruct[i, 2] == "N"
            aRow[i] := 0
            aOld[i] := 0
        elseif ::aStruct[i, 2] == "L"
            aRow[i] := .F.
            aOld[i] := .F.
        elseif ::aStruct[i, 2] == "D"
            aRow[i] := hb_StoD("")
            aOld[i] := hb_StoD("")
        elseif ::aStruct[i, 2] == "M"
            aRow[i] := ""
            aOld[i] := ""
        endif
    Next

    result := TPQRow():New( aRow, aOld, ::aStruct )
RETURN result


METHOD SetKey() CLASS TPQquery
    Local cQuery
    Local i, x
    Local nTableId, xTableId := -1
    Local nCount := 0
    Local res
    Local nPos

    if ::nResultStatus == PGRES_TUPLES_OK
        if ::Tablename == NIL
            /* set the table name looking for table oid */
            for i := 1 to len(::aStruct)
                /* Store table codes oid */
                nTableId := ::aStruct[i, 5]

                if nTableId != xTableId
                    xTableId := nTableId
                    nCount++
                endif
            next

            if nCount == 1
                /* first, try get the table name from select, else get from pg_catalog */
                if (npos := at("FROM ", Upper(::cQuery))) != 0
                    cQuery := lower(ltrim(substr( ::cQuery, nPos + 5 )))

                    if (npos := at(".", cQuery)) != 0
                        ::Schema := alltrim(left(cQuery,npos-1))
                        cQuery := substr(cQuery, nPos + 1)
                    endif

                    if (npos := at(" ", cQuery)) != 0
                        ::Tablename := trim(Left(cQuery, npos))
                    else
                        ::Tablename := cQuery
                    endif
                endif

                if empty(::Tablename)
                    cQuery := "select relname from pg_class where oid = " + str(xTableId)

                    res := PQexec(::pDB, cQuery)

                    if PQresultstatus(res) == PGRES_TUPLES_OK .and. PQlastrec(res) != 0
                        ::Tablename := trim(PQgetvalue(res, 1, 1))
                    endif

                    PQclear(res)
                endif
            endif
        endif

        if ::aKeys == NIL .and. ! empty(::Tablename)
            /* Set the table primary keys */
            cQuery := "SELECT c.attname "
            cQuery += "  FROM pg_class a, pg_class b, pg_attribute c, pg_index d, pg_namespace e "
            cQuery += " WHERE a.oid = d.indrelid "
            cQuery += "   AND a.relname = '" + ::Tablename + "'"
            cQuery += "   AND b.oid = d.indexrelid "
            cQuery += "   AND c.attrelid = b.oid "
            cQuery += "   AND d.indisprimary "
            cQuery += "   AND e.oid = a.relnamespace "
            cQuery += "   AND e.nspname = " + DataToSql(::Schema)

            res := PQexec(::pDB, cQuery)

            if PQresultstatus(res) == PGRES_TUPLES_OK .and. PQlastrec(res) != 0
                ::aKeys := {}

                For x := 1 To PQlastrec(res)
                    aadd( ::aKeys, PQgetvalue( res, x, 1 ) )
                Next
            endif

            PQclear(res)
        endif
    endif

RETURN nil

CREATE CLASS TPQRow
   VAR      aRow
   VAR      aOld
   VAR      aStruct

   METHOD   New( row, old, struct )

   METHOD   FCount()           INLINE Len(::aRow)
   METHOD   FieldGet( nField )
   METHOD   FieldPut( nField, Value )
   METHOD   FieldName( nField )
   METHOD   FieldPos( cField )
   METHOD   FieldLen( nField )
   METHOD   FieldDec( nField )
   METHOD   FieldType( nField )
   METHOD   Changed( nField )     INLINE !(::aRow[nField] == ::aOld[nField])
   METHOD   FieldGetOld( nField ) INLINE ::aOld[nField]
ENDCLASS


METHOD new( row, old, struct) CLASS TPQrow
    ::aRow := row
    ::aOld := old
    ::aStruct := struct
RETURN self


METHOD FieldGet( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField]
    endif

RETURN result


METHOD FieldPut( nField, Value ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField] := Value
    endif
RETURN result


METHOD FieldName( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]
    endif

RETURN result


METHOD FieldPos( cField ) CLASS TPQrow
    Local result

    result := AScan( ::aStruct, {|x| x[1] == trim(lower(cField)) })

RETURN result


METHOD FieldType( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]
    endif

RETURN result


METHOD FieldLen( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]
    endif
RETURN result


METHOD FieldDec( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]
    endif
RETURN result


Static Function DataToSql(xField)
   Local cType, result := "NULL"

   cType := ValType(xField)

   if cType == "C" .or. cType == "M"
      result := "'"+ strtran(xField, "'", " ") + "'"
   elseif cType == "D"
      result := dtos( xField)
   elseif cType == "N"
      result := str(xField)
   elseif cType == "L"
      result := iif( xField, "'t'", "'f'" )
   endif
return result

Static Function ValueToString(xField)
   Local cType, result := nil

   cType := ValType(xField)

   if cType == "D"
      result := dtos( xField )
   elseif cType == "N"
      result := str(xField)
   elseif cType == "L"
      result := iif( xField, "t", "f" )
   elseif cType == "C" .or. cType == "M"
      result := xField
   endif
return result
