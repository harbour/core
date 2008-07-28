/*
 * $Id$
 */

#include "hbgtinfo.ch"
#include "hbgfx.ch"

#define WELCOME "Welcome to the World of xHarbour multiplatform Graphics!"

Function Main
Local nScreenWidth, nFontHeight, nFontWidth
Local nTop, nLeft, nHeight, nWidth, nColor, nSec := Seconds()

  If !hb_gtInfo(HB_GTI_ISGRAPHIC)
     ?
     ? "You are using a non graphics capable gt:"
     ? hb_gtVersion()
     ?
     Quit
  End

  If hb_gtInfo(HB_GTI_DESKTOPWIDTH) > 1000
    hb_gtInfo(HB_GTI_FONTWIDTH, 12)
    hb_gtInfo(HB_GTI_FONTSIZE, 24)
  End

  ? hb_gtVersion(1)

  nScreenWidth := hb_gtInfo(HB_GTI_SCREENWIDTH)
  nFontHeight := hb_gtInfo(HB_GTI_FONTSIZE)
  nFontWidth := hb_gtInfo(HB_GTI_FONTWIDTH)

  SetColor("n/w")
  @ 0, 0 Say Space(MaxCol() + 1)
  @ 1, 0 Say PadC(WELCOME, MaxCol() + 1)
  @ 2, 0 Say Space(MaxCol() + 1)

  hb_gtInfo(HB_GTI_WINTITLE, "Cross-GT, multiplatform graphics demo")

  PutFrame(nFontHeight / 2,;
           MaxCol() / 2 * nFontWidth - Len(WELCOME) / 2 * nFontWidth - nFontWidth,;
           nFontHeight * 2 + nFontHeight / 2,;
           nFontWidth + MaxCol() / 2 * nFontWidth + Len(WELCOME) / 2 * nFontWidth,;
           hb_gfxMakeColor(0, 0, 0), hb_gfxMakeColor(255, 255, 255))
  
  While Inkey() == 0
    nTop := Int(hb_Random(3.1 * nFontHeight, hb_gtInfo(HB_GTI_SCREENHEIGHT)))
    nLeft := Int(hb_Random(hb_gtInfo(HB_GTI_SCREENWIDTH)))
    nHeight := Int(hb_Random(251))
    nWidth := Int(hb_Random(251))
    nColor := hb_gfxMakeColor(Int(hb_Random(32, 256)), Int(hb_Random(32, 256)), Int(hb_Random(32, 256)))

    Switch Int(hb_Random(1, 9))
      Case 1
        hb_gfxLine(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 2
        hb_gfxRect(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 3
        hb_gfxFilledRect(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 4
        nTop += nHeight
        hb_gfxCircle(nTop, nLeft, nHeight, nColor)
        Exit
      Case 5
        nTop += nHeight
        hb_gfxFilledCircle(nTop, nLeft, nHeight, nColor)
        Exit
      Case 6
        nTop += nHeight
        hb_gfxEllipse(nTop, nLeft, nHeight, nWidth, nColor)
        Exit
      Case 7
        nTop += nHeight
        hb_gfxFilledEllipse(nTop, nLeft, nHeight, nWidth, nColor)
        Exit
      Case 8
        nHeight %= 64
        If nHeight % 2 == 1
          nHeight++
        End
        hb_gfxText(nTop, nLeft, "Hello", nColor, nHeight)
        Exit
    End
    If Seconds() - nSec > 3
      hb_gfxFloodFill(0, 0, nColor)
      nSec := Seconds()
    End
  End
Return Nil

Function PutFrame(nTop, nLeft, nBottom, nRight, nColor1, nColor2)

  hb_gfxRect(ntop, nLeft, nBottom, nRight, nColor1)
  hb_gfxRect(ntop + 1, nLeft + 1, nBottom - 1, nRight - 1, nColor2)
/*  hb_gfxLine(nTop + 1, nLeft + 1, nTop + 1, nRight - 1, nColor2)
  hb_gfxLine(nTop + 2, nLeft + 1, nBottom - 1, nLeft + 1, nColor2) */
Return Nil
