function main()

   local aRdd := rddList()

   QOut( "Testing RDD's" )
   QOut( "=============" )
   QOut()
   QOut( "Registered RDD's:", LTrim( Str( Len( aRdd ) ) ), "=>" )
   aEval( aRdd, { | aDriver | QQOut( "", aDriver[ 1 ] ) } )
   QOut()
   QOut()
   dbCreate( "File.dbf", {} )

   dbUseArea( .T., "dbfntx", "AnyFile" )
   Bof()
   Eof()
   Found()
   dbGoBottom()
   dbGoTo(1)
   dbGoTop()
   dbSkip()
   dbCloseArea()

return nil

