function main()

   local aRdd := rddList()

   QOut( "Testing RDD's" )
   QOut( "=============" )
   QOut()
   QOut( "Registered RDD's:", LTrim( Str( Len( aRdd ) ) ), "=>" )
   aEval( aRdd, { | cDriver | QQOut( "", cDriver ) } )
   QOut()
   QOut()
   dbCreate( "File.dbf", {} )
   dbSelectArea( 1 )
   dbUseArea(,, "TestRdd.dbf" )
   Bof()
   rddSetDefault("DBF")
   dbUseArea(,, "TestRdd.dbf" )
   Eof()
   Found()
   dbGoBottom()
   dbGoTo( 1 )
   dbUseArea(, "DBFNTX", "TestRdd.dbf" )
   dbGoTop()
   dbSkip()
   dbCloseArea()

return nil

