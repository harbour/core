function main()

   local aStruct := { { "CHARACTER", "C", 25, 0 }, ;
		      { "NUMERIC",   "N",  8, 0 }, ;
		      { "DOUBLE",    "N",  8, 2 }, ;
		      { "DATE",      "D",  8, 0 }, ;
		      { "LOGICAL",   "L",  1, 0 } }

   local aRdd := rddList()

   QOut( "Registered RDD's:", LTrim( Str( Len( aRdd ) ) ), "=>" )
   aEval( aRdd, { | cDriver | QQOut( "", cDriver ) } )
   QOut()
   QOut( "Creating a DBF file" )
   dbCreate( "testdbf.dbf", aStruct )

return nil
