function main()

   local aStruct := { { "CHARACTER", "C", 25, 0 }, ;
		      { "NUMERIC",   "N",  8, 0 }, ;
		      { "DOUBLE",    "N",  8, 2 }, ;
		      { "DATE",      "D",  8, 0 }, ;
		      { "LOGICAL",   "L",  1, 0 } }

   QQOut( "Testing the DBF RDD driver" )
   QOut()
   QOut()
   dbCreate( "testdbf.dbf", aStruct )

return nil
