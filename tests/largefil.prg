/* Test creation of a .dbf file bigger than 2GB */

procedure Main()

   local aStruct := { ;
      { "FIELD1", "C", 256, 0 }, ;
      { "FIELD2", "C", 256, 0 }, ;
      { "FIELD3", "C", 256, 0 }, ;
      { "FIELD4", "C", 256, 0 } }

   local nRec
   local i
   local cFileName := hb_FNameName( __FILE__ )
   local nTotalRecs := 5000000

   request DBFCDX
   rddSetDefault( "DBFCDX" )

   dbCreate( cFileName + ".dbf", aStruct )
   dbUseArea( .T.,, cFileName + ".dbf", "long", .F. )
   ordCreate( cFileName + IndexExt(),, "Left( FIELD->FIELD1, 30 )" )

   for nRec := 1 to nTotalRecs
      long->( dbAppend() )
      long->FIELD1 := PadL( nRec, 30 )
   next

   ? "Testing index..."
   for nRec := 1 to nTotalRecs / 10
      if ! long->( dbSeek( PadL( nRec, 30 ) ) )
         ? "Seek on value", hb_ntos( nRec ), "failed!!"
      endif
   next
   ? "Index test ended"

   long->( dbCloseArea() )

   ? "File size is :", Transform( hb_vfSize( cFileName + ".dbf" ), "@ZE ###,###,###,###" )
   ? "Index size is:", Transform( hb_vfSize( cFileName + IndexExt() ), "@ZE ###,###,###,###" )

   ? "Press any key..."
   Inkey( 0 )

   hb_dbDrop( cFileName + ".dbf" )

   return
