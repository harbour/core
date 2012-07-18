/*
 * $Id$
 */

#include "hbgtinfo.ch"
#include "hbgfx.ch"

#define WELCOME "Welcome to the World of Harbour multiplatform Graphics!"

PROCEDURE Main()

   LOCAL nFontHeight, nFontWidth
   LOCAL nTop, nLeft, nHeight, nWidth, nColor, nSec := Seconds()

   IF ! hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "You are using a non graphics capable gt:" + hb_eol() )
      OutStd( hb_gtVersion() + hb_eol() )
      QUIT
   ENDIF

   IF hb_gtInfo( HB_GTI_DESKTOPWIDTH ) > 1000
      hb_gtInfo( HB_GTI_FONTWIDTH, 12 )
      hb_gtInfo( HB_GTI_FONTSIZE, 24 )
   ENDIF

   OutStd( hb_gtVersion( 1 ) + hb_eol() )

   nFontHeight := hb_gtInfo( HB_GTI_FONTSIZE )
   nFontWidth := hb_gtInfo( HB_GTI_FONTWIDTH )

   SetColor( "n/w" )
   @ 0, 0 SAY Space( MaxCol() + 1 )
   @ 1, 0 SAY PadC( WELCOME, MaxCol() + 1 )
   @ 2, 0 SAY Space( MaxCol() + 1 )

   hb_gtInfo( HB_GTI_WINTITLE, "Cross-GT, multiplatform graphics demo" )

   PutFrame( nFontHeight / 2, ;
      MaxCol() / 2 * nFontWidth - Len( WELCOME ) / 2 * nFontWidth - nFontWidth, ;
      nFontHeight * 2 + nFontHeight / 2, ;
      nFontWidth + MaxCol() / 2 * nFontWidth + Len( WELCOME ) / 2 * nFontWidth, ;
      hb_gfxMakeColor( 0, 0, 0 ), hb_gfxMakeColor( 255, 255, 255 ) )

   DO WHILE Inkey() == 0
      nTop := Int( hb_Random( 3.1 * nFontHeight, hb_gtInfo(HB_GTI_SCREENHEIGHT ) ) )
      nLeft := Int( hb_Random( hb_gtInfo(HB_GTI_SCREENWIDTH ) ) )
      nHeight := Int( hb_Random( 251 ) )
      nWidth := Int( hb_Random( 251 ) )
      nColor := hb_gfxMakeColor( Int( hb_Random(32, 256 ) ), Int( hb_Random(32, 256 ) ), Int( hb_Random(32, 256 ) ) )

      SWITCH Int( hb_Random( 1, 9 ) )
      CASE 1
         hb_gfxLine( nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor )
         EXIT
      CASE 2
         hb_gfxRect( nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor )
         EXIT
      CASE 3
         hb_gfxFilledRect( nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor )
         EXIT
      CASE 4
         nTop += nHeight
         hb_gfxCircle( nTop, nLeft, nHeight, nColor )
         EXIT
      CASE 5
         nTop += nHeight
         hb_gfxFilledCircle( nTop, nLeft, nHeight, nColor )
         EXIT
      CASE 6
         nTop += nHeight
         hb_gfxEllipse( nTop, nLeft, nHeight, nWidth, nColor )
         EXIT
      CASE 7
         nTop += nHeight
         hb_gfxFilledEllipse( nTop, nLeft, nHeight, nWidth, nColor )
         EXIT
      CASE 8
         nHeight %= 64
         IF nHeight % 2 == 1
            nHeight ++
         ENDIF
         hb_gfxText( nTop, nLeft, "Hello", nColor, nHeight )
         EXIT
      ENDSWITCH
      IF Seconds() - nSec > 3
         hb_gfxFloodFill( 0, 0, nColor )
         nSec := Seconds()
      ENDIF
   ENDDO

   RETURN

FUNCTION PutFrame( nTop, nLeft, nBottom, nRight, nColor1, nColor2 )

   hb_gfxRect( ntop, nLeft, nBottom, nRight, nColor1 )
   hb_gfxRect( ntop + 1, nLeft + 1, nBottom - 1, nRight - 1, nColor2 )
/* hb_gfxLine( nTop + 1, nLeft + 1, nTop + 1, nRight - 1, nColor2 )
   hb_gfxLine( nTop + 2, nLeft + 1, nBottom - 1, nLeft + 1, nColor2 ) */

   RETURN NIL
