/*
 * $Id$
 */

REQUEST DBTCDX
REQUEST FPTCDX
REQUEST SMTCDX

PROCEDURE Main()

   DBCREATE( "table1", { { "F1", "M", 4, 0 } }, "DBTCDX" )
   DBCREATE( "table2", { { "F1", "M", 4, 0 } }, "FPTCDX" )
   DBCREATE( "table3", { { "F1", "M", 4, 0 } }, "SMTCDX" )

   RETURN
