/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Firebird RDBMS low level (client api) interface code.
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

#define SQL_TEXT                           452
#define SQL_VARYING                        448
#define SQL_SHORT                          500
#define SQL_LONG                           496
#define SQL_FLOAT                          482
#define SQL_DOUBLE                         480
#define SQL_D_FLOAT                        530
#define SQL_TIMESTAMP                      510
#define SQL_BLOB                           520
#define SQL_ARRAY                          540
#define SQL_QUAD                           550
#define SQL_TYPE_TIME                      560
#define SQL_TYPE_DATE                      570
#define SQL_INT64                          580
#define SQL_DATE                        SQL_TIMESTAMP


CREATE CLASS TFbServer
    VAR      db
    VAR      trans
    VAR      StartedTrans
    VAR      nError
    VAR      lError
    VAR      dialect

    METHOD   New( cServer, cUser, cPassword, nDialect )
    METHOD   Destroy()  INLINE FBClose(::db)
    METHOD   Close()    INLINE FBClose(::db)

    METHOD   TableExists( cTable )
    METHOD   ListTables()
    METHOD   TableStruct( cTable )

    METHOD   StartTransaction()
    METHOD   Commit()
    METHOD   Rollback()

    METHOD   Execute( cQuery )
    METHOD   Query( cQuery )

    METHOD   Update( oRow, cWhere )
    METHOD   Delete( oRow, cWhere )
    METHOD   Append( oRow )

    METHOD   NetErr()   INLINE ::lError
    METHOD   Error()    INLINE FBError(::nError)
    METHOD   ErrorNo()  INLINE ::nError
ENDCLASS


METHOD New( cServer, cUser, cPassword, nDialect ) CLASS TFbServer

    Default nDialect TO 1

    ::lError := .F.
    ::nError := 0
    ::StartedTrans := .F.
    ::Dialect := nDialect

    ::db := FBConnect(cServer, cUser, cPassword)

    if ISNUMBER(::db)
        ::lError := .T.
        ::nError := ::db
    endif
RETURN self


METHOD StartTransaction() CLASS TFbServer
    Local result := .F.

    ::trans := FBStartTransaction(::db)

    if ISNUMBER(::trans)
        ::lError := .T.
        ::nError := ::trans
    else
        result := .T.
        ::lError := .F.
        ::lnError := 0
        ::StartedTrans := .T.
    endif
RETURN result


METHOD Rollback() CLASS TFbServer
    Local result := .F., n

    if ::StartedTrans
        if (n := FBRollback(::trans)) < 0
            ::lError := .T.
            ::nError := n
        else
            ::lError := .F.
            ::nError := 0
            result := .T.
            ::StartedTrans := .F.
        endif
    endif
RETURN result


METHOD Commit() CLASS TFbServer
    Local result := .F., n

    if ::StartedTrans
        if (n := FBCommit(::trans)) < 0
            ::lError := .T.
            ::nError := n
        else
            ::lError := .F.
            ::nError := 0
            result := .T.
            ::StartedTrans := .F.
        endif
    endif
RETURN result


METHOD Execute( cQuery ) CLASS TFbServer
    Local result, n

    cQuery := RemoveSpaces(cQuery)

    if ::StartedTrans
        n := FBExecute( ::db, cQuery, ::dialect, ::trans )
    else
        n := FBExecute( ::db, cQuery, ::dialect )
    endif

    if n < 0
        ::lError := .T.
        ::nError := n
        result := .F.
    else
        ::lError := .F.
        ::nError := 0
        result := .T.
    endif
RETURN result


METHOD Query( cQuery ) CLASS TFbServer
    Local oQuery

    oQuery := TFbQuery():New(::db, cQuery, ::dialect)
RETURN oQuery


METHOD TableExists( cTable ) CLASS TFbServer
    Local cQuery, result := .F., qry

    cQuery := 'select rdb$relation_name from rdb$relations where rdb$relation_name = "' + Upper(cTable) + '"'

    qry := FBQuery(::db, cQuery, ::dialect)

    if ISARRAY(qry)
        result := (FBFetch(qry) == 0)

        FBFree(qry)
    endif

RETURN result


METHOD ListTables() CLASS TFbServer
    Local result := {}, cQuery, qry

    cQuery := 'select rdb$relation_name '
    cQuery += '  from rdb$relations '
    cQuery += ' where rdb$relation_name not like "RDB$%" '
    cQuery += '   and rdb$view_blr is null '
    cQuery += ' order by 1 '

    qry := FBQuery(::db, RemoveSpaces(cQuery), ::dialect)

    if ISARRAY(qry)
        do while (FBFetch(qry)) == 0
            aadd( result, FBGetdata(qry, 1) )
        enddo

        FBFree(qry)
    endif
RETURN result


METHOD TableStruct( cTable ) CLASS TFbServer
    Local result := {}, cQuery, cType, nSize, cDomain, cField, nType, nDec
    Local qry


    cQuery := 'select '
    cQuery += '  a.rdb$field_name,'
    cQuery += '  b.rdb$field_type,'
    cQuery += '  b.rdb$field_length,'
    cQuery += '  b.rdb$field_scale * -1,'
    cQuery += '  a.rdb$field_source '
    cQuery += 'from '
    cQuery += '  rdb$relation_fields a, rdb$fields b '
    cQuery += 'where '
    cQuery += '  a.rdb$field_source = b.rdb$field_name '
    cQuery += '  and a.rdb$relation_name = "' + Upper(ctable) + '" '
    cQuery += 'order by '
    cQuery += '  a.rdb$field_position '

    qry := FBQuery(::db, RemoveSpaces(cQuery), ::dialect)

    if ISARRAY(qry)
        do while (FBFetch(qry)) == 0
            cField  := FBGetData(qry, 1)
            nType   := val(FBGetData(qry, 2))
            nSize   := val(FBGetData(qry, 3))
            nDec    := val(FBGetData(qry, 4))
            cDomain := FBGetData(qry, 5)

            switch nType
                case 7 // SMALLINT
                    if "BOOL" $ cDomain
                        cType := "L"
                        nSize := 1
                        nDec := 0
                    else
                        cType := 'N'
                        nSize := 5
                    endif

                    exit

                case 8 // INTEGER
                case 9
                    cType := 'N'
                    nSize := 9
                    exit

                case 10 // FLOAT
                case 11
                    cType := 'N'
                    nSize := 15
                    exit

                case 12 // DATE
                    cType := 'D'
                    nSize := 8
                    exit

                case 13 // TIME
                    cType := 'C'
                    nSize := 10
                    exit

                case 14 // CHAR
                    cType := 'C'
                    exit

                case 16 // INT64
                    cType := 'N'
                    nSize := 9
                    exit

                case 27 // DOUBLE
                    cType := 'N'
                    nSize := 15
                    exit

                case 35 // TIMESTAMP
                    cType := 'D'
                    nSize := 8
                    exit

                case 37 // VARCHAR
                case 40
                    cType := 'C'
                    exit

                case 261 // BLOB
                    cType := 'M'
                    nSize := 10
                    exit

                otherwise
                    cType := 'C'
                    nDec := 0
            endswitch

            aadd( result, { cField, cType, nSize, nDec } )

        enddo

        FBFree(qry)
    endif
RETURN result


METHOD Delete( oRow, cWhere ) CLASS TFbServer
    Local result := .F., aKeys, i, nField, xField, cQuery, aTables

    aTables := oRow:GetTables()

    if ! ISNUMBER(::db) .and. len(aTables) == 1
        // Cannot delete joined tables

        if cWhere == NIL
            aKeys := oRow:GetKeyField()

            cWhere := ''
            For i := 1 to len(aKeys)
                nField := oRow:Fieldpos(aKeys[i])
                xField := oRow:Fieldget(nField)

                cWhere += aKeys[i] + '=' + DataToSql(xField)

                if i != len(aKeys)
                    cWhere += ','
                endif
            Next
        endif

        if ! (cWhere == '')
            cQuery := 'DELETE FROM ' + aTables[1] + ' WHERE ' + cWhere

            result := ::Execute(cQuery)
        endif
    endif
RETURN result


METHOD Append( oRow ) CLASS TFbServer
    Local result := .F., cQuery, i, aTables

    aTables := oRow:GetTables()

    if ! ISNUMBER(::db)  .and. len(aTables) == 1
        // Can insert only one table, not in joined tables

        cQuery := 'INSERT INTO ' + aTables[1] + '('
        For i := 1 to oRow:FCount()
            if oRow:Changed(i)
                // Send only changed field
                cQuery += oRow:Fieldname(i) + ','
            endif
        Next

        cQuery := Left( cQuery, len(cQuery) - 1 ) +  ') VALUES ('

        For i := 1 to oRow:FCount()
            if oRow:Changed(i)
                cQuery += DataToSql(oRow:FieldGet(i)) + ','
            endif
        Next

        cQuery := Left( cQuery, len(cQuery) - 1  ) + ')'

        result := ::Execute(cQuery)
    endif
RETURN result


METHOD Update( oRow, cWhere ) CLASS TFbServer
    Local result := .F., aKeys, cQuery, i, nField, xField, aTables

    aTables := oRow:GetTables()

    if ! ISNUMBER(::db)  .and. len(aTables) == 1
         // Can't insert joined tables

        if cWhere == NIL
            aKeys := oRow:GetKeyField()

            cWhere := ''
            For i := 1 to len(aKeys)
                nField := oRow:Fieldpos(aKeys[i])
                xField := oRow:Fieldget(nField)

                cWhere += aKeys[i] + '=' + DataToSql(xField)

                if i != len(aKeys)
                    cWhere += ', '
                endif
            Next
        endif

        cQuery := 'UPDATE ' + aTables[1] + ' SET '
        For i := 1 to oRow:FCount()
            if oRow:Changed(i)
                cQuery += oRow:Fieldname(i) + ' = ' + DataToSql(oRow:FieldGet(i)) + ','
            endif
        Next

        if ! (cWhere == '')
            cQuery := Left( cQuery, len(cQuery) - 1 ) + ' WHERE ' + cWhere

            result := ::Execute(cQuery)
        endif
    endif
RETURN result


CREATE CLASS TFbQuery
    VAR      nError
    VAR      lError
    VAR      Dialect
    VAR      lBof
    VAR      lEof
    VAR      nRecno
    VAR      qry
    VAR      aStruct
    VAR      numcols
    VAR      closed
    VAR      db
    VAR      query
    VAR      aKeys
    VAR      aTables

    METHOD   New( db, cQuery, nDialect )
    METHOD   Destroy()
    METHOD   Close()            INLINE ::Destroy()

    METHOD   Refresh()
    METHOD   Fetch()
    METHOD   Skip()             INLINE ::Fetch()

    METHOD   Bof()              INLINE ::lBof
    METHOD   Eof()              INLINE ::lEof
    METHOD   RecNo()            INLINE ::nRecno

    METHOD   NetErr()           INLINE ::lError
    METHOD   Error()            INLINE FBError(::nError)
    METHOD   ErrorNo()          INLINE ::nError

    METHOD   FCount()           INLINE ::numcols
    METHOD   Struct()
    METHOD   FieldName( nField )
    METHOD   FieldPos( cField )
    METHOD   FieldLen( nField )
    METHOD   FieldDec( nField )
    METHOD   FieldType( nField )

    METHOD   FieldGet( nField )
    METHOD   GetRow()
    METHOD   GetBlankRow()
    METHOD   Blank()            INLINE ::GetBlankRow()
    METHOD   GetKeyField()

ENDCLASS


METHOD New( nDB, cQuery, nDialect ) CLASS TFbQuery
    ::db := nDb
    ::query := RemoveSpaces(cQuery)
    ::dialect := nDialect
    ::closed := .T.
    ::aKeys := NIL

    ::Refresh()

RETURN self


METHOD Refresh() CLASS TFbQuery
    Local qry, result, i, aTable := {}

    if ! ::closed
        ::Destroy()
    endif

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

    if ISARRAY(qry)
        ::numcols := qry[4]

        ::aStruct := StructConvert(qry[6], ::db, ::dialect)

        ::lError := .F.
        ::nError := 0
        ::qry := qry

        /* Tables in query */
        For i := 1 To len(::aStruct)
            if (ASCAN(aTable, ::aStruct[i,5]) == 0)
                aadd( aTable, ::aStruct[i,5])
            endif
        Next

        ::aTables := aTable

    else
        ::lError := .T.
        ::nError := qry
    endif

RETURN result


METHOD Destroy() CLASS TFbQuery
    Local result := .T., n

    if (! ::lError) .and. ((n := FBFree(::qry)) < 0)
        ::lError := .T.
        ::nError := n
    endif

    ::closed := .T.

RETURN result


METHOD Fetch() CLASS TFbQuery
    Local result := .F., fetch_stat

    if ! ::lError .and. ! ::lEof

        if ! ::Closed
            fetch_stat := FBFetch(::qry)

            ::nRecno++

            if fetch_stat == 0
                ::lBof := .F.
                result := .T.

            else
                ::lEof := .T.

            endif
        endif
    endif
RETURN result


METHOD Struct() CLASS TFbQuery
    Local result := {}, i

    if ! ::lError
        for i := 1 to Len(::aStruct)
            aadd( result, { ::aStruct[i,1], ::aStruct[i,2], ::aStruct[i,3], ::aStruct[i,4] } )
        next
    endif

RETURN result


METHOD FieldPos( cField ) CLASS TFbQuery
    Local result  := 0

    if ! ::lError
        result := AScan( ::aStruct, {|x| x[1] == trim(Upper(cField)) })
    endif

RETURN result


METHOD FieldName( nField ) CLASS TFbQuery
    Local result

    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]
    endif

RETURN result


METHOD FieldType( nField ) CLASS TFbQuery
    Local result

    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]
    endif

RETURN result


METHOD FieldLen( nField ) CLASS TFbQuery
    Local result

    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]
    endif
RETURN result


METHOD FieldDec( nField ) CLASS TFbQuery
    Local result

    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]
    endif
RETURN result


METHOD FieldGet( nField ) CLASS TFbQuery
    Local result, aBlob, i, cType

    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct) .and. ! ::closed

        /* TODO: Convert to right data type */

        result := FBGetData(::qry, nField)
        cType := ::aStruct[ nField, 2 ]

        if cType == "M"
            /* Blob */

            if result != NIL
                aBlob := FBGetBlob( ::db, result)

                result := ''
                For i := 1 to Len(aBlob)
                    result += aBlob[i]
                Next

                //result := FBGetBlob( ::db, result)
             else
                result := ''
             endif

        elseif cType == "N"
            if result != NIL
                result := val(result)
            else
                result := 0
            endif

        elseif cType == "D"
            if result != NIL
                result := StoD(left(result,4) + substr(result, 5, 2) + substr(result, 7, 2))
            else
                result := CtoD('')
            endif

        elseif cType == "L"
            if result != NIL
                result := (val(result) == 1)
            else
                result := .F.
            endif
        endif
    endif
RETURN result


METHOD Getrow() CLASS TFbQuery
    Local result, aRow := {}, i

    if ! ::lError .and. ! ::closed
        ASize(aRow, ::numcols)

        For i := 1 to ::numcols
            aRow[i] := ::Fieldget(i)
        Next

        result := TFbRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
    endif
RETURN result


METHOD GetBlankRow() CLASS TFbQuery
    Local result, aRow := {}, i

    if ! ::lError
        ASize(aRow, ::numcols)

        For i := 1 to ::numcols
            if ::aStruct[i, 2] == 'C'
                aRow[i] := ''
            elseif ::aStruct[i, 2] == 'N'
                aRow[i] := 0
            elseif ::aStruct[i, 2] == 'L'
                aRow[i] := .F.
            elseif ::aStruct[i, 2] == 'D'
                aRow[i] := CtoD('')
            elseif ::aStruct[i, 2] == 'M'
                aRow[i] := ''
            endif
        Next

        result := TFbRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
    endif
RETURN result


METHOD GetKeyField() CLASS TFbQuery

    if ::aKeys == NIL
       ::aKeys := KeyField( ::aTables, ::db, ::dialect )
    endif

RETURN ::aKeys


CREATE CLASS TFbRow
   VAR      aRow
   VAR      aStruct
   VAR      aChanged
   VAR      aKeys
   VAR      db
   VAR      dialect
   VAR      aTables

   METHOD   New( row, struct, db, dialect )
   METHOD   Changed(nField)
   METHOD   GetTables()        INLINE ::aTables
   METHOD   FCount()           INLINE Len(::aRow)
   METHOD   FieldGet( nField )
   METHOD   FieldPut( nField, Value )
   METHOD   FieldName( nField )
   METHOD   FieldPos( cFieldName )
   METHOD   FieldLen( nField )
   METHOD   FieldDec( nField )
   METHOD   FieldType( nField )
   METHOD   GetKeyField()
ENDCLASS


METHOD new( row, struct, nDb, nDialect, aTable ) CLASS TFbRow
    ::aRow := row
    ::aStruct := struct
    ::db := nDB
    ::dialect := nDialect
    ::aTables := aTable
    ::aChanged := Array(len(row))
RETURN self


METHOD Changed( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aChanged[nField] != NIL
    endif

RETURN result


METHOD FieldGet( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField]
    endif

RETURN result


METHOD FieldPut( nField, Value ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aRow)
        ::aChanged[nField] := .T.
        result := ::aRow[nField] := Value
    endif

RETURN result


METHOD FieldName( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]
    endif

RETURN result


METHOD FieldPos( cField ) CLASS TFbRow
    Local result

    result := AScan( ::aStruct, {|x| x[1] == trim(Upper(cField)) })

RETURN result


METHOD FieldType( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]
    endif

RETURN result


METHOD FieldLen( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]
    endif
RETURN result


METHOD FieldDec( nField ) CLASS TFbRow
    Local result

    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]
    endif
RETURN result


METHOD GetKeyField() CLASS TFbRow

    if ::aKeys == NIL
       ::aKeys := KeyField( ::aTables, ::db, ::dialect )
    endif
RETURN ::aKeys



Static Function KeyField( aTables, db, dialect )
    Local cTable, cQuery
    Local qry
    Local aKeys := {}

    /* Check row, many tables exists in current query, so we must have only one table */

    if Len(aTables) = 1
        cTable := aTables[1]

        cQuery := ' select                                      '
        cQuery += '   a.rdb$field_name                          '
        cQuery += ' from                                        '
        cQuery += '   rdb$index_segments a,                     '
        cQuery += '   rdb$relation_constraints b                '
        cQuery += ' where                                       '
        cQuery += '   a.rdb$index_name = b.rdb$index_name and   '
        cQuery += '   b.rdb$constraint_type = "PRIMARY KEY" and '
        cQuery += '   b.rdb$relation_name = ' + DataToSql(cTable)
        cQuery += ' order by                                    '
        cQuery += '   b.rdb$relation_name,                      '
        cQuery += '   a.rdb$field_position                      '

        qry := FBQuery(db, RemoveSpaces(cQuery), dialect)

        if ISARRAY(qry)
            do while (FBFetch(qry)) == 0
                aadd(aKeys, trim(FBGetdata(qry, 1)))
            enddo

            FBFree(qry)
        endif
    endif

RETURN aKeys


Static Function DataToSql(xField)
        Local cType, result

        cType := ValType(xField)

        if cType == "C"
                result := '"' + strtran(xField, '"', ' ') + '"'
        elseif cType == "D"
                result := '"' + StrZero(month(xField),2) + '/' + StrZero(day(xField),2) + '/' + StrZero(Year(xField),4) + '"'
        elseif cType == "N"
                result := str(xField)
        elseif cType == "L"
                result := iif( xField, '1', '0' )
        endif

return result


Static Function StructConvert( aStru, db, dialect)
    Local aNew := {}
    Local cField
    Local nType
    Local cType
    Local nSize
    Local nDec
    Local cTable
    Local cDomain
    Local i
    Local qry
    Local cQuery
    Local aDomains := {}
    Local nVal

    Local xTables := ''
    Local xFields := ''

    /* create table list and field list */

    For i := 1 to Len(aStru)
        xtables += DataToSql(aStru[i, 5])
        xfields += DataToSql(aStru[i, 1])

        if i != len(aStru)
            xtables += ','
            xfields += ','
        endif
    Next

    /* Look for domains */
    cQuery := 'select rdb$relation_name, rdb$field_name, rdb$field_source '
    cQuery += '  from rdb$relation_fields '
    cQuery += ' where rdb$field_name not like "RDB$%" '
    cQuery += '   and rdb$relation_name in (' + xtables + ')'
    cQuery += '   and rdb$field_name in (' + xfields + ')'

    qry := FBQuery(db, RemoveSpaces(cQuery), dialect)

    if ISARRAY(qry)

        do while (FBFetch(qry)) == 0
            aadd( aDomains, { FBGetdata(qry, 1), FBGetdata(qry,2), FBGetdata(qry,3) } )
        end

        FBFree(qry)

        For i := 1 to Len(aStru)
            cField := trim(aStru[i,1])
            nType := aStru[i,2]
            nSize := aStru[i,3]
            nDec := aStru[i,4] * -1
            cTable := trim(aStru[i,5])

            nVal := AScan(aDomains, {|x| trim(x[1]) == cTable .and. trim(x[2]) == cField})

            if nVal != 0
                cDomain := aDomains[ nVal, 3 ]
            else
                cDomain := ''
            endif

            switch nType
                case SQL_TEXT
                    cType := "C"
                    exit
                case SQL_VARYING
                    cType := "C"
                    exit
                case SQL_SHORT
                    /* Firebird doesn't have boolean field, so if you define domain with BOOL then i will consider logical, ex:
                   create domain boolean_field as smallint default 0 not null check (value in (0,1)) */

                    if "BOOL" $ cDomain
                        cType := "L"
                        nSize := 1
                        nDec := 0
                    else
                        cType := "N"
                        nSize := 5
                    end
                    exit
                case SQL_LONG
                    cType := "N"
                    nSize := 9
                    exit
                case SQL_INT64
                    cType := "N"
                    nSize := 9
                    exit
                case SQL_FLOAT
                    cType := "N"
                    nSize := 15
                    exit
                case SQL_DOUBLE
                    cType := "N"
                    nSize := 15
                    exit
                case SQL_TIMESTAMP
                    cType := "D"
                    nSize := 8
                    exit
                case SQL_TYPE_DATE
                    cType := "D"
                    nSize := 8
                    exit
                case SQL_TYPE_TIME
                    cType := "C"
                    nSize := 8
                    exit
                case SQL_BLOB
                    cType := "M"
                    nSize := 10
                    exit
                otherwise
                    cType := "C"
                    nDec := 0
            endswitch

            aadd( aNew, { cField, cType, nSize, nDec, cTable, cDomain } )
        next
    endif

return aNew

Static Function RemoveSpaces( cQuery )
    Do While AT("  ", cQuery) != 0
        cQuery := Strtran(cQuery, "  ", " ")
    enddo
Return cQuery
