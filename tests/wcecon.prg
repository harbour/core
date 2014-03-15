/*
 * Harbour Project source code:
 *    demonstration/test code for WinCE console program
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "hbgtinfo.ch"

request DBFCDX

procedure main()

   local nMaxScrRow, nMaxScrCol
   local cPath, cName, cExt, cDrive
   local i, j, k

   /* Set EN CP-437 encoding */
   hb_cdpSelect( "EN" )
   hb_SetTermCP( "EN" )

   /* Set font size */
   hb_gtInfo( HB_GTI_FONTWIDTH, 6 )
   hb_gtInfo( HB_GTI_FONTSIZE, 12 )

   /* resize console window using new font size */
   SetMode( MaxRow() + 1, MaxCol() + 1 )

   /* get screen dimensions */
   nMaxScrRow := hb_gtInfo( HB_GTI_DESKTOPROWS )
   nMaxScrCol := hb_gtInfo( HB_GTI_DESKTOPCOLS )

   /* resize console window to the screen size */
   SetMode( nMaxScrRow, nMaxScrCol )

   /* display console window size */
   ? "rows =", hb_ntos( MaxRow() + 1 )
   ? "cols =", hb_ntos( MaxCol() + 1 )
   Inkey( 0 )

   /* display infomration aboout used OS, harbour version and GT driver */
   Alert( OS() + ";" + Version() + ";GT" + hb_gtVersion() )

   /* database test */
   hb_FNameSplit( hb_ProgName(), @cPath, @cName, @cExt, @cDrive )
   cPath += "data" + hb_ps()

   Alert( "Database path:;;" + cPath )

   rddSetDefault( "DBFCDX" )
   if ! hb_DirExists( cPath )
      hb_DirCreate( cPath )
   endif
   dbCreate( cPath + "mydata.dbf", { ;
      { "F1", "C", 10, 0 }, ;
      { "F2", "=",  8, 0 }, ;
      { "FX", "M",  4, 0 } } )
   use ( cPath + "mydata.dbf" )
   index on field->F1 tag T1
   index on field->F2 tag T2
   while LastRec() < 10
      dbAppend()
      field->F1 := "rec:" + Str( RecNo(), 3 )
      field->FX := "[rec:" + Str( RecNo(), 3 ) + "]"
   enddo
   dbCommit()
   dbGoTop()
   Browse()

   /* display all characters */
   cls
   for i := 0 to 15
      for j := 0 to 15
         DispOut( " " + Chr( i * 16 + j ) )
      next
      ?
   next
   Inkey( 0 )

   /* display boxes */
   ?; DevOut( hb_UTF8ToStr( "┌─┬─┐  ╔═╦═╗  ╒═╤═╕  ╓─╥─╖  ▄▄▄" ) )
   ?; DevOut( hb_UTF8ToStr( "│ │ │  ║ ║ ║  ├─┼─┤  ╟─╫─╢  ▌■▐" ) )
   ?; DevOut( hb_UTF8ToStr( "├─┼─┤  ╠═╬═╣  │ │ │  ║ ║ ║  ▌█▐" ) )
   ?; DevOut( hb_UTF8ToStr( "│ │ │  ║ ║ ║  ╞═╪═╡  ╠═╬═╣  ▌■▐" ) )
   ?; DevOut( hb_UTF8ToStr( "└─┴─┘  ╚═╩═╝  ╘═╧═╛  ╙─╨─╜  ▀▀▀" ) )
   Inkey( 0 )

   ?
   ? "@ - interrupt, keycodes test "
   while ( k := Inkey( 0 ) ) != hb_keyCode( "@" )
      ? ; DevOut( "key=" + Str( k, 12 ) + ", char='" + hb_keyChar( k ) + "'" )
   enddo

   dbCloseAll()

   hb_dbDrop( cPath + "mydata.dbf" )
   DirRemove( cPath )

   return
