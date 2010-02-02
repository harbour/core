/*
 * $Id$
 */

/*
 * VERY IMPORTANT: Don't use this querys as sample, they are used for stress tests !!!
 */

#include "common.ch"
#include "../postgres.ch"

Function Main( cServer, cDatabase, cUser, cPass )
    Local conn, res, i, x

    Local cQuery

    CLEAR SCREEN

    ? 'Connecting....'
    conn := PQconnect(cDatabase, cServer, cUser, cPass, 5432)

    ? PQstatus(conn), PQerrormessage(conn)

    if PQstatus(conn) != CONNECTION_OK
        quit
    endif

    ? 'Dropping table...'

    res := PQexec(conn, 'DROP TABLE test')
    res := NIL

    ? 'Creating test table...'
    cQuery := 'CREATE TABLE test('
    cQuery += '     Code integer not null primary key, '
    cQuery += '     dept Integer, '
    cQuery += '     Name Varchar(40), '
    cQuery += '     Sales boolean, '
    cQuery += '     Tax Float4, '
    cQuery += '     Salary Double Precision, '
    cQuery += '     Budget Numeric(12,2), '
    cQuery += '     Discount Numeric (5,2), '
    cQuery += '     Creation Date, '
    cQuery += '     Description text ) '

    res := PQexec(conn, cQuery)
    res := NIL

    res := PQexec(conn, 'SELECT code, dept, name, sales, salary, creation FROM test')
    res := NIL

    res := PQexec(conn, 'BEGIN')
    res := NIL

    For i := 1 to 10000
        @ 15,0 say 'Inserting values....' + str(i)

        cQuery := 'INSERT INTO test(code, dept, name, sales, salary, creation) '
        cQuery += 'VALUES( ' + str(i) + ',' + str(i+1) + ", 'DEPARTMENT NAME " + strzero(i) + "', 'y', " + str(300.49+i) + ", '2003-12-28' )"

        res := PQexec(conn, cQuery)
        res := NIL

        if mod(i,100) == 0
            ? res := PQexec(conn, 'COMMIT')
            ? res := NIL

            ? res := PQexec(conn, 'BEGIN')
            ? res := NIL
        endif
    Next

    For i := 5000 to 7000
        @ 16,0 say 'Deleting values....' + str(i)

        cQuery := 'DELETE FROM test WHERE code = ' + str(i)
        res := PQexec(conn, cQuery)
        res := NIL

        if mod(i,100) == 0
            res := PQexec(conn, 'COMMIT')
            res := NIL

            res := PQexec(conn, 'BEGIN')
            res := NIL
        endif
    Next

    For i := 2000 to 3000
        @ 17,0 say 'Updating values....' + str(i)

        cQuery := 'UPDATE FROM test SET salary = 400 WHERE code = ' + str(i)
        res := PQexec(conn, cQuery)
        res := NIL

        if mod(i,100) == 0
            res := PQexec(conn, 'COMMIT')
            res := NIL

            res := PQexec(conn, 'BEGIN')
            res := NIL
        endif
    Next

    res := PQexec(conn, 'SELECT sum(salary) as sum_salary FROM test WHERE code between 1 and 4000')

    if PQresultStatus(res) == PGRES_TUPLES_OK
        @ 18,0 say 'Sum values....' + PQgetvalue(res, 1, 1)
    endif

    res := NIL

    x := 0
    For i := 1 to 4000
        res := PQexec(conn, 'SELECT salary FROM test WHERE code = ' + str(i))

        if PQresultStatus(res) == PGRES_TUPLES_OK
            x += val(PQgetvalue(res, 1, 1))

            @ 19,0 say 'Sum values....' + str(x)
        endif
    Next

    ? "Closing..."
    conn := NIL

return nil
