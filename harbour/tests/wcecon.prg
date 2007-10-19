/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for WinCE console program
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */

#include "hbgtinfo.ch"

proc main()

   local nMaxScrRow, nMaxScrCol
   local i, j, k

   /* set OEM font encoding for non unicode modes */
   hb_gtInfo( GTI_CODEPAGE, 255 )

   /* Set EN CP-437 encoding */
   HB_SETCODEPAGE( "EN" )
   HB_SETTERMCP( "EN" )

   /* Set font size */
   hb_gtInfo( GTI_FONTSIZE, 12 )
   hb_gtInfo( GTI_FONTWIDTH, 6 )

   /* resize console window using new font size */
   SetMode( MaxRow() + 1, MaxCol() + 1 )

   /* get screen dimensions */
   nMaxScrRow = hb_gtInfo( GTI_DESKTOPROWS )
   nMaxScrCol = hb_gtInfo( GTI_DESKTOPCOLS )

   /* resize console window to the screen size */
   SetMode( nMaxScrRow, nMaxScrCol )

   /* display console window size */
   ? "rows =", ltrim( str( maxrow() + 1 ) )
   ? "cols =", ltrim( str( maxcol() + 1 ) )
   inkey( 0 )

   /* display infomration aboout used OS, harbour version and GT driver */
   alert( OS() + ";" + VERSION() + ";" + HB_GTVERSION() )

   /* display all characters */
   ?
   for i := 0 to 15
      for j := 0 to 15
         dispout( " " + chr( i * 16 + j ) )
      next
      ?
   next
   inkey( 0 )

   /* display boxes */
   ?; devout( "ÚÄÂÄ¿  ÉÍËÍ»  ÕÍÑÍ¸  ÖÄÒÄ·  ÜÜÜ" )
   ?; devout( "³ ³ ³  º º º  ÃÄÅÄ´  ÇÄ×Ä¶  İşŞ" )
   ?; devout( "ÃÄÅÄ´  ÌÍÎÍ¹  ³ ³ ³  º º º  İÛŞ" )
   ?; devout( "³ ³ ³  º º º  ÆÍØÍµ  ÌÍÎÍ¹  İşŞ" )
   ?; devout( "ÀÄÁÄÙ  ÈÍÊÍ¼  ÔÍÏÍ¾  ÓÄĞÄ½  ßßß" )
   inkey( 0 )

   ?
   ? "@ - interrupt, keycodes test "
   while ( k := inkey( 0 ) ) != 64
      ? ; devout( "key=" + str( k, 4 ) + ", char='" + chr( k ) + "'" )
   enddo

return
