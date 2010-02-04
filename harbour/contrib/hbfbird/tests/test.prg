/*
 * $Id$
 */

#include "common.ch"

Function Main()
   LOCAL cDir, cName
   LOCAL cDBName
   LOCAL nDialect := 1

   LOCAL trans, qry

   LOCAL db, x, y
   LOCAL num_cols
   LOCAL columns
   LOCAL fetch_stat

   hb_FNameSplit( hb_argv( 0 ), @cDir, @cName, NIL )
   cDBName := hb_FNameMerge( cDir, cName, ".gdb" )

   if hb_FileExists( cDBName )
       FErase( cDBName )
   endif

   ? FBCreateDB( cDBName, 'sysdba', 'masterkey', 1024, 'ASCII', nDialect )

   /* Connect rdbms */
   db := FBConnect( "127.0.0.1:" + cDBName, "sysdba", "masterkey" )

   if ISNUMBER( db )
      ? 'Error:', FBError( db )
      quit
   endif

   ? FBExecute( db, 'sldjfs;ldjs;djf', nDialect )

   ? FBClose( db )

   trans := FBStartTransaction( db )
   FBQuery( db, 'create table teste (code smallint)', nDialect, trans )
   FBCommit( trans )


   ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 1")', nDialect, trans )

   ? "Status no Rollback: ", FBRollback( trans )

   trans := FBStartTransaction( db )
   ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 2")', nDialect, trans )
   ? "Status commit: ", FBCommit( trans )


   ? "Status Execute: ", FBExecute( db, 'insert into customer(customer) values ("test 3")', nDialect )

   // FIX WINDOWS GPF BELOW

   qry := FBQuery( db, "SELECT * FROM sales", nDialect )

   num_cols := qry[ 4 ]
   columns := qry[ 6 ]

   For x := 1 to num_cols
      ? x, "> "
      For y := 1 to len( columns[ x ] )
          ?? columns[ x, y ], ' '
      Next
   Next

   ? '---'

   do while ( fetch_stat := FBFetch( qry ) ) == 0
      ? fetch_stat
      for x := 1 to num_cols
          ?? FBGetData( qry, x ), ', '
      next
   enddo

   ? 'Fetch code:', fetch_stat

   ? "Status Free sql: ", FBFree( qry )


   /* Close connection with rdbms */
   ? "Status Fechar Database: ", FBClose( db )

   Return Nil
