/*
 * $Id$
 */

#include "../postgres.ch"

Function main()
    Local conn, res, aTemp, x, y, pFile
    Local cDb := 'test'
    Local cUser := 'user'
    Local cPass := 'pass'

    CLEAR SCREEN

    conn := PQsetdbLogin( 'localhost', "5432", NIL, NIL, cDb, cUser, cPass)
    ? PQdb(conn), PQuser(conn), PQpass(conn), PQhost(conn), PQport(conn), PQtty(conn), PQoptions(conn)
    ? PQClose(conn)

    conn := PQConnect(cDb, 'localhost', cuser, cpass, 5432)

    ? PQstatus(conn), PQerrormessage(conn)

    if PQstatus(conn) != CONNECTION_OK
        quit
    endif

    ? "Blocking: ", PQisnonblocking(conn), PQsetnonblocking(conn, .t.), PQisnonblocking(conn)

    pFile := PQcreatetrace( 'trace.log' )
    PQtrace( conn, pFile )

    ? "Verbose: ", PQsetErrorVerbosity(conn, 2)

    ? "Protocol: ", PQprotocolVersion(conn), ;
      " Server Version: ", PQserverVersion(conn), ;
      " Client Encoding: ", PQsetClientEncoding(conn, "ASCII"), ;
      "New encode: ", PQclientEncoding(conn)

    ? PQdb(conn), PQuser(conn), PQpass(conn), PQhost(conn), PQport(conn), PQtty(conn), PQoptions(conn)

    res := PQexec('drop table products')
    ? PQresultStatus(res), PQresultErrorMessage(res)
    PQclear(res)

    res := PQexec('create table products ( product_no numeric(10), name varchar(20), price numeric(10,2) )')
    ? PQresultStatus(res), PQresultErrorMessage(res)
    PQclear(res)

    res := PQexecParams(conn, 'insert into products(product_no, name, price) values ($1, $2, $3)', {'2', 'bread', '10.95'})
    ? "Oid Row: ", PQoidValue(res), PQoidStatus(res)

    if PQresultStatus(res) != PGRES_COMMAND_OK
        ? PQresultStatus(res), PQresultErrorMessage(res)
    endif
    PQclear(res)

    res := PQexec(conn, 'select price, name, product_no as "produto" from products')

    if PQresultStatus(res) != PGRES_TUPLES_OK
        ? PQresultStatus(res), PQresultErrorMessage(res)
    endif

    ? "Binary: ", PQbinaryTuples(res)
    ? "Rows: ", PQntuples(res), "Cols: ", PQnfields(res)
    ? PQfname(res, 1), PQftable(res, 1), PQftype(res, 1), PQfnumber(res, "name"), PQfmod(res, 1), PQfsize(res, 1), PQgetisnull(res,1,1)

    aTemp := PQmetadata(res)

    for x := 1 to len(aTemp)
        ? "Linha 1: "
        for y := 1 to 6
            ?? aTemp[x,y], ", "
        next
    next

    ? PQFcount(res)

    ? PQlastrec(res)

    ? PQGetvalue(res,1, 2)

    ? PQclear(res)

    ? "Large Objects, always should be in a transaction..."

    res := PQexec(conn, 'begin')
    PQclear(res)

    ? (x := lo_Import( conn, 'test.prg' ))
    ? lo_Export( conn, x, 'test.new' )
    ? lo_Unlink( conn, x )

    res := PQexec(conn, 'commit')
    PQclear(res)

    PQuntrace( conn )
    PQclosetrace( pFile )
    PQClose(conn)
    return nil
