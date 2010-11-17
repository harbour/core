/*
 * $Id$
 */

#include "common.ch"
#include "simpleio.ch"

FUNCTION Main()

   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 1024
   LOCAL cCharSet := "ASCII"
   LOCAL nDialect := 1
   LOCAL cName

   LOCAL trans, qry

   LOCAL db, x, y
   LOCAL num_cols
   LOCAL columns
   LOCAL fetch_stat
   LOCAL tmp

   hb_FNameSplit( hb_argv( 0 ), NIL, @cName, NIL )
   cDatabase := hb_DirTemp() + cName + ".fdb"

   IF hb_FileExists( cDatabase )
      FErase( cDatabase )
   ENDIF

   ? tmp := FBCreateDB( cServer + cDatabase, cUser, cPass, nPageSize, cCharSet, nDialect ), FBError( tmp )


   /* Connect rdbms */
   db := FBConnect( cServer + cDatabase, cUser, cPass )
   IF ISNUMBER( db )
      ? "Error:", db, FBError( db )
      QUIT
   ENDIF

   ? "Testing invalid request"
   ? tmp := FBExecute( db, "sldjfs;ldjs;djf", nDialect ), FBError( tmp )

   trans := FBStartTransaction( db )
   IF ISNUMBER( trans )
      ? "Error:", trans, FBError( trans )
   ELSE
      ? tmp := FBQuery( db, "create table teste (code smallint)", nDialect, trans ), FBError( tmp )
      ? tmp := FBCommit( trans ), FBError( tmp )
   ENDIF

   ? "==="
   trans := FBStartTransaction( db )
   IF ISNUMBER( trans )
      ? "Error:", trans, FBError( trans )
   ELSE
      ? tmp := FBQuery( db, "CREATE TABLE customer( customer VARCHAR(20) )", nDialect, trans ), FBError( tmp )
      ? tmp := FBCommit( trans ), FBError( tmp )
   ENDIF
   ? "==="

   trans := FBStartTransaction( db )
   IF ISNUMBER( trans )
      ? "Error:", trans, FBError( trans )
   ELSE
      ? "Status Execute: ", tmp := FBExecute( db, 'insert into customer(customer) values ("test 1")', nDialect, trans ), FBError( tmp )
      ? "Status Rollback: ", tmp := FBRollback( trans ), FBError( tmp )
   ENDIF

   trans := FBStartTransaction( db )
   IF ISNUMBER( trans )
      ? "Error:", trans, FBError( trans )
   ELSE
      ? "Status Execute: ", tmp := FBExecute( db, 'insert into customer(customer) values ("test 2")', nDialect, trans ), FBError( tmp )
      ? "Status Commit: ", tmp := FBCommit( trans ), FBError( tmp )
   ENDIF

   ? "Status Execute: ", tmp := FBExecute( db, 'insert into customer(customer) values ("test 3")', nDialect ), FBError( tmp )

   // FIX WINDOWS GPF BELOW

   qry := FBQuery( db, "SELECT * FROM customer", nDialect )
   IF ISNUMBER( qry )
      ? "Error:", qry, FBError( qry )
   ELSE
      num_cols := qry[ 4 ]
      columns := qry[ 6 ]

      FOR x := 1 TO num_cols
         ? x, "> "
         FOR y := 1 TO Len( columns[ x ] )
            ?? columns[ x, y ], " "
         NEXT
      NEXT

      ? "---"

      DO WHILE ( fetch_stat := FBFetch( qry ) ) == 0
         ? fetch_stat
         FOR x := 1 TO num_cols
            ?? FBGetData( qry, x ), ", "
         NEXT
      ENDDO

      ? "Fetch code:", fetch_stat

      ? "Status Free Query: ", FBFree( qry )
   ENDIF

   /* Close connection with rdbms */
   ? "Status Close Database: ", tmp := FBClose( db ), FBError( tmp )

   RETURN NIL
