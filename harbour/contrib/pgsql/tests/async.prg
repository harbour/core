/* 
 * $Id$
 *
 * This sample show howto use asynchronous/nonblocking queries
 *
 */

Function main()
    Local conn
    
    CLEAR SCREEN
        
    ? "Connect", conn := PQConnect('test', 'localhost', 'user', 'pass', 5432)
                
    ? "Conection status", PQerrorMessage(conn), PQstatus(conn)
                
    Query( conn, 'SELECT codigo, descri FROM client limit 100', .f. )                
    Query( conn, 'SELECT codigo, descri FROM fornec limit 100', .f. )                
    Query( conn, 'SELECT pedido, vlrped FROM pedido', .t. )                

    PQclose(conn)

    return nil

Procedure Query( conn, cQuery, lCancel )    
    Local pCancel, cErrMsg := space(30)
    Local res, aTemp, i, x, y, xTime
    
    ? "PQSendQuery", PQsendQuery(conn, cQuery)

    xTime := time()
    CLEAR TYPEAHEAD
    
    do while inkey() != 27
        DevPos(Row(), 20)
        DevOut("Processing: " + Elaptime(xtime, time()))

        inkey(1)
        
        if lCancel
            if .t.
                pCancel := PQgetCancel(conn)
                ? "Canceled: ", PQcancel( pCancel, @cErrMsg ), cErrMsg
                PQfreeCancel(pCancel)
                
            else 
                ? PQrequestCancel(conn) // Deprecated
            endif
        endif
        
        if PQconsumeInput(conn)
            if ! PQisBusy(conn)
                exit
            endif                    
        endif                
    enddo        
    
    if inkey() != 27
        ? "PQgetResult", valtoprg(res := PQgetResult(conn))
    
        for x := 1 to PQlastrec(res)
            ? 
            for y := 1 to PQfcount(res)
                ?? PQgetvalue(res, x, y), " "
            next            
        next
    
        PQclear(res)
    else        
        ? "Canceling Query", PQrequestCancel(conn)
    endif
Return