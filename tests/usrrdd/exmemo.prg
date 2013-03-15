
REQUEST DBTCDX
REQUEST FPTCDX
REQUEST SMTCDX

PROCEDURE Main()

   dbCreate( "table1", { { "F1", "M", 4, 0 } }, "DBTCDX" )
   dbCreate( "table2", { { "F1", "M", 4, 0 } }, "FPTCDX" )
   dbCreate( "table3", { { "F1", "M", 4, 0 } }, "SMTCDX" )

   RETURN
