/*
 * Harbour Project source code:
 *
 * Copyright 2004 Pritpal Bedi <bedipritpal@hotmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 *            Routines to manage Wvt*Classes Gui Painting
 */

#include "wvtwin.ch"

THREAD STATIC t_paint_ := { { "", {} } }

/*
 * This function must have to be defined in your appls
 */
#if 0

FUNCTION Wvt_Paint()

   /* Call this function from this funtion */
   WvtPaintObjects()

   RETURN NIL

#endif

FUNCTION WvtPaintObjects()

   LOCAL i, lExe, nLeft, nRight, b, tlbr_, aBlocks, nBlocks

   aBlocks := WvtSetPaint()

   IF ( nBlocks := Len( aBlocks ) ) > 0
      tlbr_ := Wvt_GetPaintRect()

      FOR i := 1 TO nBlocks
         lExe := .T.

         IF aBlocks[ i, 3 ] != NIL .AND. ! Empty( aBlocks[ i, 3 ] )
            /*  Check parameters against tlbr_ depending upon the
             *  type of object and attributes contained in aAttr
             */
            DO CASE
            CASE aBlocks[ i, 3, 1 ] == WVT_BLOCK_GRID_V
               b := aBlocks[ i, 3, 6 ]
               IF Len( b:aColumnsSep ) == 0
                  lExe := .F.
               ELSE
                  nLeft  := b:aColumnsSep[ 1 ]
                  nRight := b:aColumnsSep[ Len( b:aColumnsSep ) ]
                  IF !( tlbr_[ 1 ] <= aBlocks[ i, 3, 4 ] .AND. ; /* top   < bottom */
                     tlbr_[ 3 ] >= aBlocks[ i, 3, 2 ] .AND. ; /* bootm > top    */
                     tlbr_[ 2 ] <= nRight + 1       .AND. ; /* left  < right  */
                     tlbr_[ 4 ] >= nLeft  - 2             ) /* right > left   */
                     lExe := .F.
                  ENDIF
               ENDIF

            CASE aBlocks[ i, 3, 1 ] == WVT_BLOCK_GETS
               IF !( tlbr_[ 1 ] <= aBlocks[ i, 3, 4 ] .AND. ; /* top   < bott  */
                  tlbr_[ 3 ] >= aBlocks[ i, 3, 2 ] .AND. ; /* bootm > top   */
                  tlbr_[ 2 ] <= aBlocks[ i, 3, 5 ] .AND. ; /* left  < righ  */
                  tlbr_[ 4 ] >= aBlocks[ i, 3, 3 ]       ) /* right > left  */
                  lExe := .F.
               ENDIF

            OTHERWISE
               /* If refreshing rectangle's top is less than objects' bottom    */
               /* and left is less than objects' right                          */
               /*                                                               */
               IF !( tlbr_[ 1 ] <= aBlocks[ i, 3, 4 ] .AND. ; /* top   <= bottom  */
                  tlbr_[ 3 ] >= aBlocks[ i, 3, 2 ] .AND. ; /* bootm >= top     */
                  tlbr_[ 2 ] <= aBlocks[ i, 3, 5 ] .AND. ; /* left  < right    */
                  tlbr_[ 4 ] >= aBlocks[ i, 3, 3 ]       ) /* right > left     */
                  lExe := .F.
               ENDIF
            ENDCASE
         ENDIF

         IF lExe
            Eval( aBlocks[ i, 2 ] )
         ENDIF
      NEXT
   ENDIF

   RETURN 0

FUNCTION WvtSetPaint( a_ )

   LOCAL o

   THREAD STATIC t

   IF t == nil
      t := {}
   ENDIF

   o := t

   IF a_ != nil
      t := a_
   ENDIF

   RETURN o

FUNCTION Wvg_SetPaint( cID, nAction, xData, aAttr )

   LOCAL n, n1, oldData

   IF xData != nil
      IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
         IF ( n1 := AScan( t_paint_[ n, 2 ], {| e_ | e_[ 1 ] == nAction } ) ) > 0
            oldData := t_paint_[ n, 2, n1, 2 ]
            t_paint_[ n, 2, n1, 2 ] := xData
            t_paint_[ n, 2, n1, 3 ] := aAttr
         ELSE
            AAdd( t_paint_[ n, 2 ], { nAction, xData, aAttr } )
         ENDIF
      ELSE
         AAdd( t_paint_, { cID, {} } )
         n := Len( t_paint_ )
         AAdd( t_paint_[ n, 2 ], { nAction, xData, aAttr } )
      ENDIF
   ENDIF

   RETURN oldData

FUNCTION Wvg_GetPaint( cID )

   LOCAL n

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      RETURN t_paint_[ n, 2 ]
   ENDIF

   RETURN {}

FUNCTION Wvg_DelPaint( cID, nAction )

   LOCAL xData, n1, n

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      IF ( n1 := AScan( t_paint_[ n, 2 ], {| e_ | e_[ 1 ] == nAction } ) ) > 0
         xData := t_paint_[ n, 2, n1, 2 ]
         t_paint_[ n, 2, n1, 2 ] := {|| .T. }
      ENDIF
   ENDIF

   RETURN xData

FUNCTION Wvg_PurgePaint( cID, lDummy )

   LOCAL n, aPaint

   __defaultNIL( @lDummy, .F. )

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      aPaint := t_paint_[ n ]
      hb_ADel( t_paint_, n, .T. )
   ENDIF

   IF lDummy
      WvtSetPaint( {} )
   ENDIF

   RETURN aPaint

FUNCTION Wvg_InsertPaint( cID, aPaint, lSet )

   LOCAL n

   __defaultNIL( @lSet, .F. )

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      t_paint_[ n ] := aPaint
   ELSE
      AAdd( t_paint_, aPaint )
   ENDIF

   IF lSet
      WvtSetPaint( aPaint )
   ENDIF

   RETURN NIL

/*
 *               RunTime Dialog Generation Routines
 *
 *                      Courtesy What32.lib
 */
/* nMode : 0 == Rows/cols - DEFAULT    1 == DlagUnits as from any standard dialog definition */
FUNCTION Wvt_SetDlgCoMode( nMode )

   LOCAL nOldMode

   STATIC sMode := 0

   nOldMode := sMode
   IF HB_ISNUMERIC( nMode ) .AND. nMode <= 1 .AND. nMode >= 0
      sMode := nMode
   ENDIF

   RETURN nOldMode

FUNCTION Wvt_MakeDlgTemplate( nTop, nLeft, nRows, nCols, aOffSet, cTitle, nStyle, ;
      cFaceName, nPointSize, nWeight, lItalic, nHelpId, nExStyle )

   LOCAL aDlg := { {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL aFont
   LOCAL nMode := Wvt_SetDlgCoMode()

   aFont := Wvt_GetFontInfo()

   IF nMode == 0
      __defaultNIL( @aOffSet, {} )
      ASize( aOffSet, 4 )
      __defaultNIL( @aOffSet[ 1 ], 0 )
      __defaultNIL( @aOffSet[ 2 ], 0 )
      __defaultNIL( @aOffSet[ 3 ], 0 )
      __defaultNIL( @aOffSet[ 4 ], 0 )

      nBaseUnits  := Wvg_GetDialogBaseUnits()
      nBaseUnitsX := Wvg_LOWORD( nBaseUnits )
      nBaseUnitsY := Wvg_HIWORD( nBaseUnits )

      nW := aFont[ 7 ] * nCols + aOffSet[ 4 ]
      nH := aFont[ 6 ] * nRows + aOffSet[ 3 ]

      /* Position it exactly where user has requested */

      aXY := Wvt_ClientToScreen( nTop, nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      /* MSDN says DlgBaseUnits and Screen Coordinates has multiplier of 4,8 for X and Y.
       * But in my practice, the values below are 99% accurate.
       * I have tested it on many fonts but on 1280/800 resolution.
       * Please feel free to experiment if you find thses values inappropriate.
       */
      nXM :=  5.25
      nYM := 10.25

      nX  := ( nX * nXM / nBaseUnitsX )
      nY  := ( nY * nYM / nBaseUnitsY )
      nW  := ( nW * nXM / nBaseUnitsX )
      nH  := ( nH * nYM / nBaseUnitsY )
   ELSE
      nX  := nLeft
      nY  := nTop
      nW  := nCols
      nH  := nRows
   ENDIF

   IF ! HB_ISNUMERIC( nStyle )
      nStyle := ;
         WS_CAPTION + WS_SYSMENU + WS_GROUP      + ;
         WS_TABSTOP + DS_SETFONT + WS_THICKFRAME + ;
         WS_VISIBLE + WS_POPUP   + DS_3DLOOK
   ENDIF

   AAdd( aDlg[ 1 ], iif( Empty( nHelpId  ), 0, nHelpId  ) )
   AAdd( aDlg[ 1 ], iif( Empty( nExStyle ), 0, nExStyle ) )
   AAdd( aDlg[ 1 ], nStyle  )
   AAdd( aDlg[ 1 ], 0       )
   AAdd( aDlg[ 1 ], nX      )
   AAdd( aDlg[ 1 ], nY      )
   AAdd( aDlg[ 1 ], nW      )
   AAdd( aDlg[ 1 ], nH      )
   AAdd( aDlg[ 1 ], 0       )
   AAdd( aDlg[ 1 ], 0       )
   AAdd( aDlg[ 1 ], iif( HB_ISSTRING( cTitle ), cTitle, "" ) )

   IF hb_bitAnd( nStyle, DS_SETFONT ) == DS_SETFONT
      AAdd( aDlg[ 1 ], iif( HB_ISNUMERIC( nPointSize ), nPointSize, 8               ) )
      AAdd( aDlg[ 1 ], iif( HB_ISNUMERIC( nWeight    ), nWeight, 400             ) )
      AAdd( aDlg[ 1 ], iif( HB_ISLOGICAL( lItalic    ), lItalic, .F.             ) )
      AAdd( aDlg[ 1 ], iif( HB_ISSTRING(  cFaceName  ), cFaceName, "MS Sans Serif" ) )
   ENDIF

   RETURN aDlg

FUNCTION Wvt_AddDlgItem( aDlg, nTop, nLeft, nRows, nCols, aOffSet, ;
      cnId, cnDlgClass, nStyle, cText, nHelpId, nExStyle )

   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL nBottom, nRight
   LOCAL nMode := Wvt_SetDlgCoMode()

   IF nMode == 0
      nBottom := nTop  + nRows - 1
      nRight  := nLeft + nCols - 1

      __defaultNIL( @aOffSet, {} )

      ASize( aOffSet, 4 )

      __defaultNIL( @aOffSet[ 1 ], 0 )
      __defaultNIL( @aOffSet[ 2 ], 0 )
      __defaultNIL( @aOffSet[ 3 ], 0 )
      __defaultNIL( @aOffSet[ 4 ], 0 )

      nBaseUnits  := Wvg_GetDialogBaseUnits()
      nBaseUnitsX := Wvg_LOWORD( nBaseUnits )
      nBaseUnitsY := Wvg_HIWORD( nBaseUnits )

      aXY := Wvt_GetXYFromRowCol( nTop, nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      aXY := Wvt_GetXYFromRowCol( nBottom + 1, nRight + 1 )
      nW  := aXY[ 1 ] + aOffSet[ 4 ] - nX
      nH  := aXY[ 2 ] + aOffSet[ 3 ] - nY

      nXM :=  5.25
      nYM := 10.25

      nX  := ( nX * nXM / nBaseUnitsX )
      nY  := ( nY * nYM / nBaseUnitsY )
      nW  := ( nW * nXM / nBaseUnitsX )
      nH  := ( nH * nYM / nBaseUnitsY )
   ELSE
      nX  := nLeft
      nY  := nTop
      nW  := nCols
      nH  := nRows
   ENDIF

   aDlg[ 1, 4 ]++      /* item count */

   AAdd( aDlg[  2 ], iif( HB_ISNUMERIC( nHelpId  ), nHelpId, 0                     ) )
   AAdd( aDlg[  3 ], iif( HB_ISNUMERIC( nExStyle ), nExStyle, 0                     ) )
   AAdd( aDlg[  4 ], iif( HB_ISNUMERIC( nStyle   ), nStyle, WS_CHILD + WS_VISIBLE ) )
   AAdd( aDlg[  5 ], nX         )
   AAdd( aDlg[  6 ], nY         )
   AAdd( aDlg[  7 ], nW         )
   AAdd( aDlg[  8 ], nH         )
   AAdd( aDlg[  9 ], cnId       )
   AAdd( aDlg[ 10 ], cnDlgClass )
   AAdd( aDlg[ 11 ], iif( HB_ISSTRING( cText ), cText, iif( HB_ISNUMERIC( cText ), cText, "" ) ) )
   AAdd( aDlg[ 12 ], 0 )

   RETURN aDlg

FUNCTION Wvt_CreateDialog( acnDlg, lOnTop, cbDlgProc, ncIcon, nTimerTicks, hMenu )

   LOCAL hDlg, cType, xTemplate, nDlgMode

   IF HB_ISSTRING( cbDlgProc )
      cbDlgProc := Upper( cbDlgProc )
   ENDIF

   cType    := ValType( acnDlg )
   nDlgMode := iif( cType == "C", 0, iif( cType == "N", 1, 2 ) )

   IF cType == "A"
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ], acnDlg[  2 ], acnDlg[  3 ], acnDlg[  4 ], ;
         acnDlg[ 5 ], acnDlg[  6 ], acnDlg[  7 ], acnDlg[  8 ], ;
         acnDlg[ 9 ], acnDlg[ 10 ], acnDlg[ 11 ], acnDlg[ 12 ] )
   ELSE
      xTemplate := acnDlg
   ENDIF

   hDlg := Wvt_CreateDialogDynamic( xTemplate, lOnTop, cbDlgProc, nDlgMode )

   IF hDlg != 0
      IF ncIcon != nil
         Wvt_DlgSetIcon( hDlg, ncIcon )

      ENDIF

      IF HB_ISNUMERIC( nTimerTicks )
         Wvg_SetTimer( hDlg, 1001, nTimerTicks )

      ENDIF

      IF hMenu != nil
         Wvg_SetMenu( hDlg, hMenu )

      ENDIF

   ENDIF

   RETURN hDlg

FUNCTION Wvt_DialogBox( acnDlg, cbDlgProc, hWndParent )

   LOCAL nResult, cType, xTemplate, nDlgMode

   IF HB_ISSTRING( cbDlgProc )
      cbDlgProc := Upper( cbDlgProc )
   ENDIF

   cType    := ValType( acnDlg )
   nDlgMode := iif( cType == "C", 0, iif( cType == "N", 1, 2 ) )

   IF cType == "A"
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ], acnDlg[  2 ], acnDlg[  3 ], acnDlg[  4 ], ;
         acnDlg[ 5 ], acnDlg[  6 ], acnDlg[  7 ], acnDlg[  8 ], ;
         acnDlg[ 9 ], acnDlg[ 10 ], acnDlg[ 11 ], acnDlg[ 12 ] )
   ELSE
      xTemplate := acnDlg
   ENDIF

   nResult := Wvt_CreateDialogModal( xTemplate, .F., cbDlgProc, nDlgMode, hWndParent )

   Wvg_SetFocus( hWndParent )

   RETURN nResult

/*
Wvt_GetOpenFileName( hWnd, @cPath, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex )

hWnd:     Handle to parent window
cPath:    (optional) if OFN_ALLOWMULTISELECT the path is stored
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {"Data Bases","*.dbf"},{"Clipper","*.prg"} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types
cDefName: DEFAULT file name

Returns:  If OFN_ALLOWMULTISELECT ?  Array of files selected : FileName.
*/
FUNCTION Wvt_GetOpenFileName( hWnd, cPath, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex, cDefName )

   LOCAL cRet, aTmp, xRet, i

   HB_SYMBOL_UNUSED( hWnd )

   __defaultNIL( @cPath, "" )
   __defaultNIL( @nFlags, OFN_EXPLORER + OFN_NOCHANGEDIR )

/* win_GetOpenFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := win_GetOpenFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /*nBufferSize*/, cDefName )

   IF Wvg_And( nFlags, OFN_ALLOWMULTISELECT ) > 0
      xRet := {}
      IF ! Empty( aTmp := hb_ATokens( cRet, Chr( 0 ) ) )
         cPath := aTmp[ 1 ]
         FOR i := 2 TO Len( aTmp )
            AAdd( xRet, cPath + "\" + aTmp[ i ] )
         NEXT
      ENDIF
   ELSE
      xRet := cRet
   ENDIF

   RETURN xRet

/*
Wvt_GetSaveFileName( hWnd, cDefFile, cTitle, acFilter, nFlags, cInitDir, cDefExt, @nFilterIndex )

hWnd:     Handle to parent window
cDefName: (optional) Default FileName
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {"Data Bases","*.dbf"},{"Clipper","*.prg"} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types

Returns:  FileName.
*/
FUNCTION Wvt_GetSaveFileName( hWnd, cDefName, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex )

   LOCAL cRet, aTmp, xRet, i, cPath

   HB_SYMBOL_UNUSED( hWnd )

   __defaultNIL( @nFlags, OFN_EXPLORER + OFN_NOCHANGEDIR )

/* win_GetSaveFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := win_GetSaveFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /*nBufferSize*/, cDefName )

   IF Wvg_And( nFlags, OFN_ALLOWMULTISELECT ) > 0
      xRet := {}
      IF ! Empty( aTmp := hb_ATokens( cRet, Chr( 0 ) ) )
         cPath := aTmp[ 1 ]
         FOR i := 2 TO Len( aTmp )
            AAdd( xRet, cPath + "\" + aTmp[ i ] )
         NEXT
      ENDIF
   ELSE
      xRet := cRet
   ENDIF

   RETURN xRet

/*
 * C Functions to PRG Ports
 */

#include "hbgtinfo.ch"
#include "hbgtwvg.ch"

#ifdef HB_LEGACY_LEVEL4

FUNCTION Wvt_SetTitle( cTitle )
   RETURN hb_gtInfo( HB_GTI_WINTITLE, cTitle )

FUNCTION Wvt_GetTitle()
   RETURN hb_gtInfo( HB_GTI_WINTITLE )

#endif

FUNCTION Wvt_SetIcon( ncIconRes, cIconName )

   IF HB_ISNUMERIC( ncIconRes )
      hb_gtInfo( HB_GTI_ICONRES, ncIconRes )
   ELSEIF HB_ISSTRING( cIconName )
      hb_gtInfo( HB_GTI_ICONRES, cIconName )
   ELSEIF HB_ISSTRING( ncIconRes )
      hb_gtInfo( HB_GTI_ICONFILE, ncIconRes )
   ENDIF

   RETURN NIL

FUNCTION Wvt_SetFont( cFontName, nSize, nWidth, nWeight, nQuality )

   __defaultNIL( @cFontName, hb_gtInfo( HB_GTI_FONTNAME ) )
   __defaultNIL( @nWidth, hb_gtInfo( HB_GTI_FONTWIDTH ) )
   __defaultNIL( @nWeight, hb_gtInfo( HB_GTI_FONTWEIGHT ) )
   __defaultNIL( @nQuality, hb_gtInfo( HB_GTI_FONTQUALITY ) )
   __defaultNIL( @nSize, hb_gtInfo( HB_GTI_FONTSIZE ) )

   RETURN hb_gtInfo( HB_GTI_SETFONT, { cFontName, nSize, nWidth, nWeight, nQuality } )

#ifdef HB_LEGACY_LEVEL4

FUNCTION Wvt_SetCodepage( nCodePage )
   RETURN hb_gtInfo( HB_GTI_CODEPAGE, nCodePage )

FUNCTION Wvt_GetPalette()
   RETURN hb_gtInfo( HB_GTI_PALETTE )

FUNCTION Wvt_SetPalette( aRGB )
   RETURN hb_gtInfo( HB_GTI_PALETTE, aRGB )

FUNCTION Wvt_GetRGBColor( nIndex )
   RETURN hb_gtInfo( HB_GTI_PALETTE, nIndex )

#endif

#define BLACK                                     RGB( 0x00, 0x00, 0x00 )
#define BLUE                                      RGB( 0x00, 0x00, 0x85 )
#define GREEN                                     RGB( 0x00, 0x85, 0x00 )
#define CYAN                                      RGB( 0x00, 0x85, 0x85 )
#define RED                                       RGB( 0x85, 0x00, 0x00 )
#define MAGENTA                                   RGB( 0x85, 0x00, 0x85 )
#define BROWN                                     RGB( 0x85, 0x85, 0x00 )
#define LIGHT_GRAY                                RGB( 0xC6, 0xC6, 0xC6 )
#define GRAY                                      RGB( 0x60, 0x60, 0x60 )
#define BRIGHT_BLUE                               RGB( 0x00, 0x00, 0xFF )
#define BRIGHT_GREEN                              RGB( 0x60, 0xFF, 0x60 )
#define BRIGHT_CYAN                               RGB( 0x60, 0xFF, 0xFF )
#define BRIGHT_RED                                RGB( 0xF8, 0x00, 0x26 )
#define BRIGHT_MAGENTA                            RGB( 0xFF, 0x60, 0xFF )
#define YELLOW                                    RGB( 0xFF, 0xFF, 0x00 )
#define WHITE                                     RGB( 0xFF, 0xFF, 0xFF )

FUNCTION Wvt_GetRGBColorByString( cColor, nForeBack )

   LOCAL s, n, lEnh
   LOCAL nIndex := 0
   LOCAL a_ := { "N", "B", "G", "BG", "R", "RB", "GR", "W" }

   nForeBack := iif( HB_ISNUMERIC( nForeBack ), nForeBack, 0 )

   IF HB_ISSTRING( cColor )
      IF ( n := At( "/", cColor ) ) > 0
         IF nForeBack == 0
            s := SubStr( cColor, 1, n - 1 )
         ELSE
            s := SubStr( cColor, n + 1 )
         ENDIF
      ELSE
         s := cColor
      ENDIF
      s := Upper( s )
      lEnh := ( "*" $ s ) .OR. ( "+" $ s )
      IF lEnh
         s := StrTran( s, "*" )
         s := StrTran( s, "+" )
      ENDIF
      nIndex := AScan( a_, {| e | e == s } )
      IF nIndex > 0
         IF lEnh
            nIndex += 8
         ENDIF
         nIndex--
      ENDIF
   ENDIF

   RETURN hb_gtInfo( HB_GTI_PALETTE, nIndex )

#ifdef HB_LEGACY_LEVEL4

FUNCTION Wvt_SetAltF4Close( lSetClose )
   RETURN hb_gtInfo( HB_GTI_CLOSABLE, lSetClose )

FUNCTION Wvt_GetScreenWidth()
   RETURN hb_gtInfo( HB_GTI_DESKTOPWIDTH )

FUNCTION Wvt_GetScreenHeight()
   RETURN hb_gtInfo( HB_GTI_DESKTOPHEIGHT )

#endif

FUNCTION Wvt_GetWindowHandle()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )

FUNCTION Wvt_CenterWindow( lCenter, lRePaint )

   __defaultNIL( @lCenter, .T. )
   __defaultNIL( @lRePaint, .F. )

   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

FUNCTION Wvt_WindowCentre( lCenter, lRePaint )

   __defaultNIL( @lCenter, .T. )
   __defaultNIL( @lRePaint, .F. )

   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

FUNCTION Wvt_ProcessMessages()

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_PROCESSMESSAGES )

   RETURN .T.

FUNCTION Wvt_Keyboard( nKey )

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_KEYBOARD, nKey )

   RETURN NIL

#ifdef HB_LEGACY_LEVEL4

FUNCTION Wvt_GetClipboard()
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION Wvt_SetClipboard( cText )
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA, cText )

#endif

FUNCTION Wvt_PasteFromClipboard()

   LOCAL cText, nLen, i

   cText := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
   IF ( nLen := Len( cText ) ) > 0
      FOR i := 1 TO nLen
         Wvt_Keyboard( Asc( SubStr( cText, i, 1 ) ) )
      NEXT
   ENDIF

   RETURN NIL

FUNCTION Wvt_ResetWindow()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_RESETWINDOW )

FUNCTION Wvt_SetTimer( nTimerID, nMiliSeconds )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_SETTIMER, { nTimerID, nMiliSeconds } )

FUNCTION Wvt_KillTimer( nTimerID )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_KILLTIMER, nTimerID )

FUNCTION Wvt_SetOnTop()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETONTOP )

FUNCTION Wvt_SetAsNormal()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETASNORMAL )

FUNCTION Wvt_Minimize()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MINIMIZED )

FUNCTION Wvt_Maximize()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

FUNCTION Wvt_Hide()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_HIDDEN )

FUNCTION Wvt_Show()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_NORMAL )

FUNCTION Wvt_SetWindowPos( nX, nY )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_SETPOSITION, { nX, nY } )

FUNCTION Wvt_ShowWindow( nState )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, nState )

FUNCTION Wvt_Update()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_UPDATEWINDOW )
