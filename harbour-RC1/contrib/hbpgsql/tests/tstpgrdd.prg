/* The aim of this test is to check the same RDD functions and statements against a dbf file 
   and the same dbf imported into a PostgreSQL database.
   Replace <...> with your data and configuration.
*/


procedure main()

   local nConn

   set deleted on

   request DBFCDX
   request PGRDD

   use <table> index <index> exclusive via "dbfcdx"

   set order to 1 

   test_code( "DBF" )

   use

   nConn := dbpgconnection( "<host>;<database>;<user>;<password>;<port>;<scheme>" )

   /* if you want to update and insert data you need at least a primary key */

   use "select <fields,...> from <table> order by <same order as dbf>" alias <table> via "pgrdd" connection nConn 

   test_code( "SQL" )

   use

   dbpgclearconnection( nConn )

   return 

function test_code( cMode )

   local xTemp

   <table>->( dbgotop() )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   <table>->( dbgobottom() )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   /* goto 100 has different meaning */
   <table>->( dbgoto( 100 ) )
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   xTemp := <table>-><field3>
   replace <table>-><field3> with "*** replaced ***"
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   replace <table>-><field3> with xTemp
   dbcommit() // the real write is made via dbcommit() so it is needed
   ? <table>-><field1>, <table>-><field2>, <table>-><field3>
   ? eof()
   dbgobottom()
   dbskip()
   ? eof()
   ? bof()
   dbgotop()
   dbskip(-1)
   ? bof()

   dbappend()
   replace <table>-><field1> with <"9">
   replace <table>-><field2> with <"999999">
   replace <table>-><field3> with <"APPENDED">
   dbcommit()

   // recno() has different meaning, in SQL it is the number of the row and change for every select */
   ? <table>->( recno() ), <table>-><field1>, <table>-><field2>, <table>-><field3>

   wait

   cls

   BROWSE()

   dbgobottom()
   replace <table>-><field3> with "REPLACED"
   dbcommit()
   dbgobottom()

   BROWSE()

   delete for <table>-><field1> = "9"

   return nil


