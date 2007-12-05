/*
 * $Id$
 */

#include "common.ch"

Function Main()

 nDialect := 1

 if File('test.gdb')
     FErase('test.gdb')
 end

 ? FBCreateDB('test.gdb','sysdba', 'masterkey', 1024, 'ASCII', nDialect )

 
 /* Connect rdbms */
 db := FBConnect("127.0.0.1:d:\firebird\test\test.gdb", "sysdba", "masterkey")

 if ISNUMBER(db)
    ? 'Error'
    quit
 end
 
 ? FBExecute(db, 'sldjfs;ldjs;djf', dialect)
 
 ? FBClose(db) 

 trans := FBStartTransaction(db) 
 qry := FBQuery(db, 'create table teste (code smallint)', dialect, trans)
 FBCommit(trans)


 ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 1")', dialect, trans)

 ? "Status no Rollback: ", FBRollback(trans) 

 trans := FBStartTransaction(db)
 ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 2")', dialect, trans )  
 ? "Status commit: ", FBCommit(trans)


 ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 3")', dialect )

 // FIX WINDOWS GPF BELOW

 qry := FBQuery(db, "SELECT * FROM sales", dialect) 
 
 num_cols := qry[4] 
 columns := qry[6] 
 
 For x := 1 to num_cols
    ? x, "> "
    For y := 1 to len(columns[x])
        ?? columns[x,y], ' '
    Next
 Next
  
 ? '---'
 
 do while (fetch_stat := FBFetch(qry)) == 0
    ? fetch_stat
    for x := 1 to num_cols
        ?? FBGetData(qry,x), ', '    
    next 
 end
 
 ? 'Fetch code:', fetch_stat
 
 ? "Status Free sql: ", FBFree(qry)

   
 /* Close connection with rdbms */
 ? "Status Fechar Database: ", FBClose(db)
 
 Return Nil
