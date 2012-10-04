/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for WinCE console program
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

/* UTF-8 */

#include "hbgtinfo.ch"

request DBFCDX

proc main()

   field F1, F2, FX
   local nMaxScrRow, nMaxScrCol
   local cPath, cName, cExt, cDrive
   local i, j, k

   /* set OEM font encoding for non unicode modes */
   hb_gtInfo( HB_GTI_CODEPAGE, 255 )

   /* Set EN CP-437 encoding */
   hb_cdpSelect( "EN" )
   hb_setTermCP( "EN" )

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
   ? "rows =", ltrim( str( maxrow() + 1 ) )
   ? "cols =", ltrim( str( maxcol() + 1 ) )
   inkey( 0 )

   /* display infomration aboout used OS, harbour version and GT driver */
   alert( OS() + ";" + Version() + ";" + hb_gtVersion() )

   /* database test */
   hb_FNameSplit( hb_argv( 0 ), @cPath, @cName, @cExt, @cDrive )
   cPath += "data" + hb_ps()

   Alert( "Database path:;;" + cPath )

   rddSetDefault("DBFCDX")
   if !hb_dirExists( cPath )
      hb_dirCreate( cPath )
   endif
   dbCreate( cPath + "mydata", { { "F1", "C", 10, 0 }, ;
                                 { "F2", "=",  8, 0 }, ;
                                 { "FX", "M",  4, 0 } } )
   use ( cPath + "mydata" )
   index on F1 tag T1
   index on F2 tag T2
   while lastrec() < 10
      dbappend()
      F1 := "rec:" + str( recno(), 3 )
      FX := "[rec:" + str( recno(), 3 ) + "]"
   enddo
   dbCommit()
   dbGoTop()
   browse()

   /* display all characters */
   CLS
   for i := 0 to 15
      for j := 0 to 15
         dispout( " " + chr( i * 16 + j ) )
      next
      ?
   next
   inkey( 0 )

   /* display boxes */
   ?; devout( hb_UTF8ToStr( "┌─┬─┐  ╔═╦═╗  ╒═╤═╕  ╓─╥─╖  ▄▄▄" ) )
   ?; devout( hb_UTF8ToStr( "│ │ │  ║ ║ ║  ├─┼─┤  ╟─╫─╢  ▌■▐" ) )
   ?; devout( hb_UTF8ToStr( "├─┼─┤  ╠═╬═╣  │ │ │  ║ ║ ║  ▌█▐" ) )
   ?; devout( hb_UTF8ToStr( "│ │ │  ║ ║ ║  ╞═╪═╡  ╠═╬═╣  ▌■▐" ) )
   ?; devout( hb_UTF8ToStr( "└─┴─┘  ╚═╩═╝  ╘═╧═╛  ╙─╨─╜  ▀▀▀" ) )
   inkey( 0 )

   ?
   ? "@ - interrupt, keycodes test "
   while ( k := inkey( 0 ) ) != hb_keyCode( "@" )
      ? ; devout( "key=" + str( k, 4 ) + ", char='" + chr( k ) + "'" )
   enddo

   dbCloseAll()

   hb_dbDrop( cPath + "mydata" )
   dirRemove( cPath )

return
