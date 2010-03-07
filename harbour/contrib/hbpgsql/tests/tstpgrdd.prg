/*
 * $Id$
 */

/* The aim of this test is to check the same RDD functions and statements against a dbf file
   and the same dbf imported into a PostgreSQL database.
   Replace <...> with your data and configuration.
*/

#if 0

PROCEDURE Main()

   LOCAL nConn

   SET DELETED ON

   REQUEST DBFCDX
   REQUEST PGRDD

   USE <table> INDEX <index> EXCLUSIVE VIA "dbfcdx"

   SET ORDER TO 1

   test_code( "DBF" )

   USE

   nConn := dbpgconnection( "<host>;<database>;<user>;<password>;<port>;<scheme>" )

   /* if you want to update and insert data you need at least a primary key */

   USE "select <fields,...> FROM <table> ORDER BY <same order as dbf>" ALIAS <table> VIA "pgrdd" CONNECTION NCONN

   test_code( "SQL" )

   USE

   dbpgclearconnection( nConn )

   RETURN

PROCEDURE test_code( cMode )

   LOCAL xTemp

   <table>->( dbGoTop() )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   <table>->( dbGoBottom() )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   /* goto 100 has different meaning */
   <table>->( dbGoto( 100 ) )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   xTemp := <table>-><field3>
   REPLACE <table>-><field3> WITH "*** replaced ***"
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   REPLACE <table>-><field3> WITH xTemp
   dbCommit() // the real write is made via dbCommit() so it is needed
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   ? Eof()
   dbGoBottom()
   dbSkip()
   ? Eof()
   ? Bof()
   dbGoTop()
   dbSkip( -1 )
   ? Bof()

   dbAppend()
   REPLACE <table>-><field1> WITH <"9">
   REPLACE <table>-><field2> WITH <"999999">
   REPLACE <table>-><field3> WITH <"APPENDED">
   dbCommit()

   // RecNo() has different meaning, in SQL it is the number of the row and change for every select */
   ? <table>->( RecNo() ), <table>-><field1>, <table>-><field2>, <table>-><field3>

   WAIT

   CLS

   Browse()

   dbGoBottom()
   REPLACE <table>-><field3> WITH "REPLACED"
   dbCommit()
   dbGoBottom()

   Browse()

   DELETE FOR <table>-><field1> = "9"

   RETURN

#endif
