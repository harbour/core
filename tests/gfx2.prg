/*
 * Harbour Project source code:
 *    demonstration/test code for GFX operations
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "hbgtinfo.ch"
#include "hbgfx.ch"

PROCEDURE Main()

   LOCAL nRed, nGreen, nBlue

#if defined( __HBSCRIPT__HBSHELL )
#  if defined( __PLATFORM__WINDOWS )
      hbshell_gtSelect( "GTWVT" )
#  elif defined( __PLATFORM__UNIX )
      hbshell_gtSelect( "GTXWC" )
#  endif
#else
#  if defined( __PLATFORM__WINDOWS )
     REQUEST HB_GT_WVT_DEFAULT
#  elif defined( __PLATFORM__UNIX )
     REQUEST HB_GT_XWC_DEFAULT
#  endif
#endif

   ? "GT" + hb_gtVersion()
   IF ! hb_gtInfo( HB_GTI_ISGRAPHIC )
      ? "You are using a non graphics capable gt"
      QUIT
   ENDIF

// SetMode( 30, 80 )

   nRed   := hb_gfxMakeColor( 200, 32, 32 )
   nGreen := hb_gfxMakeColor( 32, 200, 32 )
   nBlue  := hb_gfxMakeColor( 32, 32, 200 )

   hb_gfxLine( 100, 300, 200, 400, nRed )
   WAIT
   hb_gfxLine( 200, 300, 100, 400, nBlue )
   WAIT
   hb_gfxLine( 100, 420, 200, 320, nGreen )
   WAIT
   hb_gfxLine( 200, 420, 100, 320, nGreen )
   WAIT
   hb_gfxRect( 100, 300, 200, 420, nRed )
   WAIT
   hb_gfxRect(  90, 290, 210, 430, nBlue )
   WAIT
   hb_gfxLine( 100, 450, 100, 550, nRed )
   hb_gfxLine( 100, 550, 200, 550, nRed )
   hb_gfxLine( 200, 550, 200, 450, nRed )
   hb_gfxLine( 200, 450, 100, 450, nRed )
   WAIT
   hb_gfxRect( 100, 450, 200, 550, nGreen )
   WAIT

   hb_gfxFilledRect( 220, 300, 320, 400, nRed )
   WAIT
   hb_gfxRect( 219, 449, 321, 551, nRed )
   WAIT
   hb_gfxFilledRect( 320, 550, 220, 450, nGreen )
   WAIT
   hb_gfxRect( 230, 310, 310, 390, nGreen )
   WAIT
   hb_gfxCircle( 270, 350, 40, nBlue )
   WAIT
   hb_gfxFilledCircle( 270, 500, 50, nBlue )
   WAIT

   hb_gfxRect( 329, 299, 391, 501, nRed )
   hb_gfxFilledRect( 330, 300, 390, 500, nBlue )
   WAIT
   hb_gfxFilledEllipse( 360, 400, 30, 100, nGreen )
   WAIT
   hb_gfxEllipse( 360, 400, 15, 50, nRed )
   WAIT

   hb_gfxRect( 0, 0, hb_gtInfo( HB_GTI_SCREENHEIGHT ) - 1, ;
                     hb_gtInfo( HB_GTI_SCREENWIDTH ) - 1, nGreen )
   WAIT

   RETURN
