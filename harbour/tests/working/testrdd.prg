function main()

   QOut( "Testing RDD's" )
   QOut( "=============" )
   QOut()

   dbUseArea( .T., "dbfntx", "AnyFile" )
   Bof()
   Eof()
   Found()
   dbGoBottom()
   dbGoTo(1)
   dbGoTop()
   dbCloseArea()

return nil

