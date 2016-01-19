/* Test creation of a .dbf file bigger than 2GB */


function main()

   local aStruct := { { "FIELD1", "C", 256, 0 },;
                      { "FIELD2", "C", 256, 0 },;
                      { "FIELD3", "C", 256, 0 },;
                      { "FIELD4", "C", 256, 0 } }
   local nRec
   local i
   local cFileName := "testdbf"
   local nTotalRecs := 5000000

   REQUEST DBFCDX
   RddSetDefault( "DBFCDX" )

   dbCreate( cFileName, aStruct )
   dbUseArea( .T.,, cFileName, "long", .F. )
   OrdCreate( cFileName + IndexExt(),, "Left( field->FIELD1, 30 )" )

   for nRec := 1 to nTotalRecs
      long->( dbAppend() )
      long->FIELD1 := PadL( nRec, 30, Space( 1 ) )
   next

   ? "File size is : " + Transform( hb_fSize( cFileName + ".dbf" ), "@ZE ###,###,###,###" )
   ? "Index size is: " + Transform( hb_fSize( cFileName + IndexExt() ), "@ZE ###,###,###,###" )

   ? "Testing index..."
   for nRec := 1 to nTotalRecs / 10
      if ! long->( dbSeek( PadL( nRec, 30, Space( 1 ) ) ) )
         ? "Seek on value " + LTrim( Str( nRec ) ) + " failed!!"
      endif
   next
   ? "Index test ended"
   ? "Press any key..."
   Inkey( 0 )

   long->( dbCloseArea() )
   fErase( cFileName + ".dbf" )
   fErase( cFileName + IndexExt() )

return 0
