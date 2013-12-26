/*
 * Harbour Project source code:
 *    demonstration/test code for changing font in X-Window GTs
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "hbgtinfo.ch"

#if ! defined( __HBSCRIPT__HBSHELL )
   #if defined( __PLATFORM__UNIX )
      REQUEST HB_GT_XWC_DEFAULT
   #endif
#endif

proc main()
   local cChars, i, j, n

#if defined( __HBSCRIPT__HBSHELL )
   #if defined( __PLATFORM__UNIX )
      hbshell_gtSelect( "GTXWC" )
   #endif
#endif

   cChars := ""
   for i := 0 to 7
      cChars += ";"
      for j := 0 to 31
         n := i * 32 + j
         cChars += iif( n == Asc( ";" ), ",", ;
                   iif( n == 10, " ", Chr( n ) ) )
      next
   next

   n := 2
   ? "GT" + hb_gtVersion(), hb_gtVersion( 1 )
   hb_gtInfo( HB_GTI_FONTATTRIBUTE, HB_GTI_FONTA_FIXMETRIC + ;
              HB_GTI_FONTA_CLRBKG + HB_GTI_FONTA_DRAWBOX )
   while n == 2
      ? hb_gtInfo( HB_GTI_FONTSEL )
      hb_gtInfo( HB_GTI_FONTSEL, xFontSel() )
      n := Alert( "What do you think about this font;;" + ;
                  hb_gtInfo( HB_GTI_FONTSEL ) + ";" + cChars, ;
                  { "FINE", "CHANGE" } )
   enddo
   ? "current font:"
   ? hb_gtInfo( HB_GTI_FONTSEL )
   OutStd( hb_gtInfo( HB_GTI_FONTSEL ) )
   wait
   return

function xfontsel()
   local cStdOut
   hb_processRun( "xfontsel -print",, @cStdOut )
   return cStdOut
