/*
 * Routines to manage Wvt*Classes Gui Painting
 *
 * Copyright 2004 Pritpal Bedi <bedipritpal@hotmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "wvtwin.ch"

#include "hbgtinfo.ch"

THREAD STATIC t_paint_ := { { "", {} } }

/* This function must have to be defined in your applications */
#if 0

PROCEDURE wvt_Paint()

   WvtPaintObjects()  /* Call this function from this function */

   RETURN

#endif

FUNCTION WvtPaintObjects()

   LOCAL blk, lExe, nLeft, nRight, b, tlbr_

   LOCAL aBlocks := WvtSetPaint()

   IF Len( aBlocks ) > 0
      tlbr_ := wvt_GetPaintRect()

      FOR EACH blk IN aBlocks
         lExe := .T.

         IF blk[ 3 ] != NIL .AND. ! Empty( blk[ 3 ] )
            /* Check parameters against tlbr_ depending upon the
               type of object and attributes contained in aAttr */
            DO CASE
            CASE blk[ 3, 1 ] == WVT_BLOCK_GRID_V
               b := blk[ 3, 6 ]
               IF Len( b:aColumnsSep ) == 0
                  lExe := .F.
               ELSE
                  nLeft  := b:aColumnsSep[ 1 ]
                  nRight := ATail( b:aColumnsSep )
                  IF !( tlbr_[ 1 ] <= blk[ 3, 4 ] .AND. ; /* top    < bottom */
                     tlbr_[ 3 ] >= blk[ 3, 2 ] .AND. ;    /* bottom > top    */
                     tlbr_[ 2 ] <= nRight + 1 .AND. ;     /* left   < right  */
                     tlbr_[ 4 ] >= nLeft  - 2 )           /* right  > left   */
                     lExe := .F.
                  ENDIF
               ENDIF

            CASE blk[ 3, 1 ] == WVT_BLOCK_GETS
               IF !( tlbr_[ 1 ] <= blk[ 3, 4 ] .AND. ; /* top    < bottom */
                  tlbr_[ 3 ] >= blk[ 3, 2 ] .AND. ;    /* bottom > top    */
                  tlbr_[ 2 ] <= blk[ 3, 5 ] .AND. ;    /* left   < right  */
                  tlbr_[ 4 ] >= blk[ 3, 3 ] )          /* right  > left   */
                  lExe := .F.
               ENDIF

            OTHERWISE
               /* If refreshing rectangle's top is less than objects' bottom
                * and left is less than objects' right
                */
               IF !( tlbr_[ 1 ] <= blk[ 3, 4 ] .AND. ; /* top    <= bottom */
                  tlbr_[ 3 ] >= blk[ 3, 2 ] .AND. ;    /* bottom >= top    */
                  tlbr_[ 2 ] <= blk[ 3, 5 ] .AND. ;    /* left   <  right  */
                  tlbr_[ 4 ] >= blk[ 3, 3 ] )          /* right  >  left   */
                  lExe := .F.
               ENDIF
            ENDCASE
         ENDIF

         IF lExe
            Eval( blk[ 2 ] )
         ENDIF
      NEXT
   ENDIF

   RETURN 0

FUNCTION WvtSetPaint( a_ )

   THREAD STATIC t

   LOCAL o

   IF t == NIL
      t := {}
   ENDIF

   o := t

   IF a_ != NIL
      t := a_
   ENDIF

   RETURN o

FUNCTION wvg_SetPaint( cID, nAction, xData, aAttr )

   LOCAL n, n1, oldData

   IF xData != NIL
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
         AAdd( ATail( t_paint_ )[ 2 ], { nAction, xData, aAttr } )
      ENDIF
   ENDIF

   RETURN oldData

FUNCTION wvg_GetPaint( cID )

   LOCAL n

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      RETURN t_paint_[ n, 2 ]
   ENDIF

   RETURN {}

FUNCTION wvg_DelPaint( cID, nAction )

   LOCAL xData, n1, n

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      IF ( n1 := AScan( t_paint_[ n, 2 ], {| e_ | e_[ 1 ] == nAction } ) ) > 0
         xData := t_paint_[ n, 2, n1, 2 ]
         t_paint_[ n, 2, n1, 2 ] := {|| .T. }
      ENDIF
   ENDIF

   RETURN xData

FUNCTION wvg_PurgePaint( cID, lDummy )

   LOCAL n, aPaint

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      aPaint := t_paint_[ n ]
      hb_ADel( t_paint_, n, .T. )
   ENDIF

   IF hb_defaultValue( lDummy, .F. )
      WvtSetPaint( {} )
   ENDIF

   RETURN aPaint

PROCEDURE wvg_InsertPaint( cID, aPaint, lSet )

   LOCAL n

   IF ( n := AScan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      t_paint_[ n ] := aPaint
   ELSE
      AAdd( t_paint_, aPaint )
   ENDIF

   IF hb_defaultValue( lSet, .F. )
      WvtSetPaint( aPaint )
   ENDIF

   RETURN

/* RunTime Dialog Generation Routines
 * Courtesy hbwhat library
 */
/* nMode : 0 == Rows/cols - DEFAULT    1 == DlagUnits as from any standard dialog definition */
FUNCTION wvt_SetDlgCoMode( nMode )

   STATIC sMode := 0

   LOCAL nOldMode

   nOldMode := sMode
   IF HB_ISNUMERIC( nMode ) .AND. nMode <= 1 .AND. nMode >= 0
      sMode := nMode
   ENDIF

   RETURN nOldMode

FUNCTION wvt_MakeDlgTemplate( nTop, nLeft, nRows, nCols, aOffSet, cTitle, nStyle, ;
      cFaceName, nPointSize, nWeight, lItalic, nHelpId, nExStyle )

   LOCAL aDlg := { {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL aFont := wvt_GetFontInfo()
   LOCAL nMode := wvt_SetDlgCoMode()

   IF nMode == 0
      hb_default( @aOffSet, {} )
      ASize( aOffSet, 4 )
      hb_default( @aOffSet[ 1 ], 0 )
      hb_default( @aOffSet[ 2 ], 0 )
      hb_default( @aOffSet[ 3 ], 0 )
      hb_default( @aOffSet[ 4 ], 0 )

      nBaseUnits  := wapi_GetDialogBaseUnits()
      nBaseUnitsX := wapi_LOWORD( nBaseUnits )
      nBaseUnitsY := wapi_HIWORD( nBaseUnits )

      nW := aFont[ 7 ] * nCols + aOffSet[ 4 ]
      nH := aFont[ 6 ] * nRows + aOffSet[ 3 ]

      /* Position it exactly where user has requested */

      aXY := wvt_ClientToScreen( nTop, nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      /* MSDN says DlgBaseUnits and Screen Coordinates has multiplier of 4,8 for X and Y.
       * But in my practice, the values below are 99% accurate.
       * I have tested it on many fonts but on 1280/800 resolution.
       * Please feel free to experiment if you find thses values inappropriate.
       */
      nXM :=  5.25
      nYM := 10.25

      nX := nX * nXM / nBaseUnitsX
      nY := nY * nYM / nBaseUnitsY
      nW := nW * nXM / nBaseUnitsX
      nH := nH * nYM / nBaseUnitsY
   ELSE
      nX := nLeft
      nY := nTop
      nW := nCols
      nH := nRows
   ENDIF

   hb_default( @nStyle, WIN_WS_CAPTION + WIN_WS_SYSMENU + WIN_WS_GROUP + WIN_WS_TABSTOP + DS_SETFONT + WIN_WS_THICKFRAME + WIN_WS_VISIBLE + WIN_WS_POPUP + DS_3DLOOK )

   AAdd( aDlg[ 1 ], hb_defaultValue( nHelpId, 0 ) )
   AAdd( aDlg[ 1 ], hb_defaultValue( nExStyle, 0 ) )
   AAdd( aDlg[ 1 ], nStyle )
   AAdd( aDlg[ 1 ], 0 )
   AAdd( aDlg[ 1 ], nX )
   AAdd( aDlg[ 1 ], nY )
   AAdd( aDlg[ 1 ], nW )
   AAdd( aDlg[ 1 ], nH )
   AAdd( aDlg[ 1 ], 0 )
   AAdd( aDlg[ 1 ], 0 )
   AAdd( aDlg[ 1 ], hb_defaultValue( cTitle, "" ) )

   IF hb_bitAnd( nStyle, DS_SETFONT ) != 0
      AAdd( aDlg[ 1 ], hb_defaultValue( nPointSize, 8 ) )
      AAdd( aDlg[ 1 ], hb_defaultValue( nWeight, 400 ) )
      AAdd( aDlg[ 1 ], hb_defaultValue( lItalic, .F. ) )
      AAdd( aDlg[ 1 ], hb_defaultValue( cFaceName, "MS Sans Serif" ) )
   ENDIF

   RETURN aDlg

FUNCTION wvt_AddDlgItem( aDlg, nTop, nLeft, nRows, nCols, aOffSet, ;
      cnId, cnDlgClass, nStyle, cText, nHelpId, nExStyle )

   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL nBottom, nRight
   LOCAL nMode := wvt_SetDlgCoMode()

   IF nMode == 0
      nBottom := nTop  + nRows - 1
      nRight  := nLeft + nCols - 1

      hb_default( @aOffSet, {} )

      ASize( aOffSet, 4 )

      hb_default( @aOffSet[ 1 ], 0 )
      hb_default( @aOffSet[ 2 ], 0 )
      hb_default( @aOffSet[ 3 ], 0 )
      hb_default( @aOffSet[ 4 ], 0 )

      nBaseUnits  := wapi_GetDialogBaseUnits()
      nBaseUnitsX := wapi_LOWORD( nBaseUnits )
      nBaseUnitsY := wapi_HIWORD( nBaseUnits )

      aXY := wvt_GetXYFromRowCol( nTop, nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      aXY := wvt_GetXYFromRowCol( nBottom + 1, nRight + 1 )
      nW  := aXY[ 1 ] + aOffSet[ 4 ] - nX
      nH  := aXY[ 2 ] + aOffSet[ 3 ] - nY

      nXM :=  5.25
      nYM := 10.25

      nX := nX * nXM / nBaseUnitsX
      nY := nY * nYM / nBaseUnitsY
      nW := nW * nXM / nBaseUnitsX
      nH := nH * nYM / nBaseUnitsY
   ELSE
      nX := nLeft
      nY := nTop
      nW := nCols
      nH := nRows
   ENDIF

   aDlg[ 1, 4 ]++      /* item count */

   AAdd( aDlg[  2 ], hb_defaultValue( nHelpId, 0 ) )
   AAdd( aDlg[  3 ], hb_defaultValue( nExStyle, 0 ) )
   AAdd( aDlg[  4 ], hb_defaultValue( nStyle, WIN_WS_CHILD + WIN_WS_VISIBLE ) )
   AAdd( aDlg[  5 ], nX )
   AAdd( aDlg[  6 ], nY )
   AAdd( aDlg[  7 ], nW )
   AAdd( aDlg[  8 ], nH )
   AAdd( aDlg[  9 ], cnId )
   AAdd( aDlg[ 10 ], cnDlgClass )
   AAdd( aDlg[ 11 ], iif( HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText ), cText, "" ) )
   AAdd( aDlg[ 12 ], 0 )

   RETURN aDlg

FUNCTION wvt_CreateDialog( acnDlg, lOnTop, cbDlgProc, ncIcon, nTimerTicks, hMenu )

   LOCAL hDlg, xTemplate, nDlgMode

   IF HB_ISSTRING( cbDlgProc )
      cbDlgProc := Upper( cbDlgProc )
   ENDIF

   nDlgMode := iif( HB_ISSTRING( acnDlg ), 0, iif( HB_ISNUMERIC( acnDlg ), 1, 2 ) )

   IF HB_ISARRAY( acnDlg )
      xTemplate := __wapi_DLGTEMPLATE_Raw_New( hb_ArrayToParams( acnDlg ) )
   ELSE
      xTemplate := acnDlg
   ENDIF

   hDlg := wvt_CreateDialogDynamic( xTemplate, lOnTop, cbDlgProc, nDlgMode )

   IF ! Empty( hDlg )
      IF ncIcon != NIL
         wvt_DlgSetIcon( hDlg, ncIcon )
      ENDIF

      IF HB_ISNUMERIC( nTimerTicks )
         wvg_SetTimer( hDlg, 1001, nTimerTicks )
      ENDIF

      IF hMenu != NIL
         wvg_SetMenu( hDlg, hMenu )
      ENDIF
   ENDIF

   RETURN hDlg

FUNCTION wvt_DialogBox( acnDlg, cbDlgProc, hWndParent )

   LOCAL nResult, xTemplate, nDlgMode

   IF HB_ISSTRING( cbDlgProc )
      cbDlgProc := Upper( cbDlgProc )
   ENDIF

   nDlgMode := iif( HB_ISSTRING( acnDlg ), 0, iif( HB_ISNUMERIC( acnDlg ), 1, 2 ) )

   IF HB_ISARRAY( acnDlg )
      xTemplate := __wapi_DLGTEMPLATE_Raw_New( hb_ArrayToParams( acnDlg ) )
   ELSE
      xTemplate := acnDlg
   ENDIF

   nResult := wvt_CreateDialogModal( xTemplate, .F., cbDlgProc, nDlgMode, hWndParent )

   wapi_SetFocus( hWndParent )

   RETURN nResult

/* wvt_GetOpenFileName( hWnd, @cPath, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex )

hWnd:     Handle to parent window
cPath:    (optional) if WIN_OFN_ALLOWMULTISELECT the path is stored
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { { "Databases", "*.dbf" }, { "Harbour", "*.prg" } }
nFlags:   WIN_OFN_* values default to WIN_OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types
cDefName: DEFAULT file name

Returns:  If WIN_OFN_ALLOWMULTISELECT ?  Array of files selected : FileName.
*/
FUNCTION wvt_GetOpenFileName( hWnd, cPath, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex, cDefName )

   LOCAL cRet, aTmp, xRet, i

   HB_SYMBOL_UNUSED( hWnd )

   hb_default( @cPath, "" )
   hb_default( @nFlags, WIN_OFN_EXPLORER + WIN_OFN_NOCHANGEDIR )

/* win_GetOpenFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>], ;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := win_GetOpenFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /* nBufferSize */, cDefName )

   IF hb_bitAnd( nFlags, WIN_OFN_ALLOWMULTISELECT ) != 0
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

/* wvt_GetSaveFileName( hWnd, cDefFile, cTitle, acFilter, nFlags, cInitDir, cDefExt, @nFilterIndex )

hWnd:     Handle to parent window
cDefName: (optional) Default FileName
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { { "Databases", "*.dbf" }, { "Harbour", "*.prg" } }
nFlags:   WIN_OFN_* values default to WIN_OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types

Returns:  FileName.
*/
FUNCTION wvt_GetSaveFileName( hWnd, cDefName, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex )

   LOCAL cRet, aTmp, xRet, i, cPath

   HB_SYMBOL_UNUSED( hWnd )

   hb_default( @nFlags, WIN_OFN_EXPLORER + WIN_OFN_NOCHANGEDIR )

/* win_GetSaveFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>], ;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := win_GetSaveFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /*nBufferSize*/, cDefName )

   IF hb_bitAnd( nFlags, WIN_OFN_ALLOWMULTISELECT ) != 0
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

/* C functions to PRG Ports */

#include "hbgtinfo.ch"
#include "hbgtwvg.ch"

#ifdef HB_LEGACY_LEVEL4

FUNCTION wvt_SetTitle( cTitle )
   RETURN hb_gtInfo( HB_GTI_WINTITLE, cTitle )

FUNCTION wvt_GetTitle()
   RETURN hb_gtInfo( HB_GTI_WINTITLE )

#endif

PROCEDURE wvt_SetIcon( ncIconRes, cIconName )

   DO CASE
   CASE HB_ISNUMERIC( ncIconRes ) ; hb_gtInfo( HB_GTI_ICONRES, ncIconRes )
   CASE HB_ISSTRING( cIconName )  ; hb_gtInfo( HB_GTI_ICONRES, cIconName )
   CASE HB_ISSTRING( ncIconRes )  ; hb_gtInfo( HB_GTI_ICONFILE, ncIconRes )
   ENDCASE

   RETURN

FUNCTION wvt_SetFont( cFontName, nSize, nWidth, nWeight, nQuality )
   RETURN hb_gtInfo( HB_GTI_SETFONT, { ;
      hb_defaultValue( cFontName, hb_gtInfo( HB_GTI_FONTNAME ) ), ;
      hb_defaultValue( nSize, hb_gtInfo( HB_GTI_FONTSIZE ) ), ;
      hb_defaultValue( nWidth, hb_gtInfo( HB_GTI_FONTWIDTH ) ), ;
      hb_defaultValue( nWeight, hb_gtInfo( HB_GTI_FONTWEIGHT ) ), ;
      hb_defaultValue( nQuality, hb_gtInfo( HB_GTI_FONTQUALITY ) ) } )

#ifdef HB_LEGACY_LEVEL4

FUNCTION wvt_SetCodepage( nCodePage )
   RETURN hb_gtInfo( HB_GTI_CODEPAGE, nCodePage )

FUNCTION wvt_GetPalette()
   RETURN hb_gtInfo( HB_GTI_PALETTE )

FUNCTION wvt_SetPalette( aRGB )
   RETURN hb_gtInfo( HB_GTI_PALETTE, aRGB )

FUNCTION wvt_GetRGBColor( nIndex )
   RETURN hb_gtInfo( HB_GTI_PALETTE, nIndex )

#endif

#define BLACK               WIN_RGB( 0x00, 0x00, 0x00 )
#define BLUE                WIN_RGB( 0x00, 0x00, 0x85 )
#define GREEN               WIN_RGB( 0x00, 0x85, 0x00 )
#define CYAN                WIN_RGB( 0x00, 0x85, 0x85 )
#define RED                 WIN_RGB( 0x85, 0x00, 0x00 )
#define MAGENTA             WIN_RGB( 0x85, 0x00, 0x85 )
#define BROWN               WIN_RGB( 0x85, 0x85, 0x00 )
#define LIGHT_GRAY          WIN_RGB( 0xC6, 0xC6, 0xC6 )
#define GRAY                WIN_RGB( 0x60, 0x60, 0x60 )
#define BRIGHT_BLUE         WIN_RGB( 0x00, 0x00, 0xFF )
#define BRIGHT_GREEN        WIN_RGB( 0x60, 0xFF, 0x60 )
#define BRIGHT_CYAN         WIN_RGB( 0x60, 0xFF, 0xFF )
#define BRIGHT_RED          WIN_RGB( 0xF8, 0x00, 0x26 )
#define BRIGHT_MAGENTA      WIN_RGB( 0xFF, 0x60, 0xFF )
#define YELLOW              WIN_RGB( 0xFF, 0xFF, 0x00 )
#define WHITE               WIN_RGB( 0xFF, 0xFF, 0xFF )

FUNCTION wvt_GetRGBColorByString( cColor, nForeBack )

   LOCAL s, n, lEnh
   LOCAL nIndex := 0

   IF HB_ISSTRING( cColor )
      IF ( n := At( "/", cColor ) ) > 0
         IF hb_defaultValue( nForeBack, 0 ) == 0
            s := Left( cColor, n - 1 )
         ELSE
            s := SubStr( cColor, n + 1 )
         ENDIF
      ELSE
         s := cColor
      ENDIF
      s := Upper( s )
      lEnh := "*" $ s .OR. "+" $ s
      IF lEnh
         s := hb_StrReplace( s, "*+" )
      ENDIF
      IF ( nIndex := hb_AScan( { "N", "B", "G", "BG", "R", "RB", "GR", "W" }, s,,, .T. ) ) > 0
         IF lEnh
            nIndex += 8
         ENDIF
         nIndex--
      ENDIF
   ENDIF

   RETURN hb_gtInfo( HB_GTI_PALETTE, nIndex )

#ifdef HB_LEGACY_LEVEL4

FUNCTION wvt_SetAltF4Close( lSetClose )
   RETURN hb_gtInfo( HB_GTI_CLOSABLE, lSetClose )

FUNCTION wvt_GetScreenWidth()
   RETURN hb_gtInfo( HB_GTI_DESKTOPWIDTH )

FUNCTION wvt_GetScreenHeight()
   RETURN hb_gtInfo( HB_GTI_DESKTOPHEIGHT )

#endif

#ifdef HB_LEGACY_LEVEL5

FUNCTION wvt_GetWindowHandle()
   RETURN hb_gtInfo( HB_GTI_WINHANDLE )

#endif

FUNCTION wvt_CenterWindow( lCenter, lRePaint )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { hb_defaultValue( lCenter, .T. ), hb_defaultValue( lRePaint, .F. ) } )

FUNCTION wvt_WindowCentre( lCenter, lRePaint )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { hb_defaultValue( lCenter, .T. ), hb_defaultValue( lRePaint, .F. ) } )

FUNCTION wvt_ProcessMessages()

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_PROCESSMESSAGES )

   RETURN .T.

PROCEDURE wvt_Keyboard( nKey )

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_KEYBOARD, nKey )

   RETURN

#ifdef HB_LEGACY_LEVEL4

FUNCTION wvt_GetClipboard()
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION wvt_SetClipboard( cText )
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA, cText )

#endif

PROCEDURE wvt_PasteFromClipboard()

   LOCAL cText := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
   LOCAL nLen := Len( cText )
   LOCAL i

   FOR i := 1 TO nLen
      wvt_Keyboard( hb_keyCode( SubStr( cText, i, 1 ) ) )
   NEXT

   RETURN

FUNCTION wvt_ResetWindow()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_RESETWINDOW )

PROCEDURE wvt_SetTimer( nTimerID, nMiliSeconds )

   LOCAL hWnd := hb_gtInfo( HB_GTI_WINHANDLE )

   IF ! Empty( hWnd )
      wapi_SetTimer( hWnd, nTimerID, nMiliSeconds )
   ENDIF

   RETURN

PROCEDURE wvt_KillTimer( nTimerID )

   LOCAL hWnd := hb_gtInfo( HB_GTI_WINHANDLE )

   IF ! Empty( hWnd )
      wapi_KillTimer( hWnd, nTimerID )
   ENDIF

   RETURN

FUNCTION wvt_SetOnTop()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETONTOP )

FUNCTION wvt_SetAsNormal()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETASNORMAL )

FUNCTION wvt_Minimize()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MINIMIZED )

FUNCTION wvt_Maximize()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

FUNCTION wvt_Hide()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_HIDDEN )

FUNCTION wvt_Show()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_NORMAL )

FUNCTION wvt_SetWindowPos( nX, nY )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_SETPOSITION, { nX, nY } )

FUNCTION wvt_ShowWindow( nState )
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, nState )

FUNCTION wvt_Update()
   RETURN hb_gtInfo( HB_GTI_SPEC, HB_GTS_UPDATEWINDOW )

FUNCTION wvt_GetCursorPos()

   LOCAL xy

   wapi_GetCursorPos( @xy )

   RETURN xy

FUNCTION wvg_GetWindowRect( hWnd )

   LOCAL rc

   wapi_GetWindowRect( hWnd, @rc )

   RETURN rc

FUNCTION wvg_GetClientRect( hWnd )

   LOCAL rc

   wapi_GetClientRect( hWnd, @rc )

   RETURN rc

FUNCTION wvg_SetTimer( ... )
   RETURN ! Empty( wapi_SetTimer( ... ) )

FUNCTION wvg_InvalidateRect( w, r, e )
   RETURN wapi_InvalidateRect( w, r, hb_defaultValue( e, .T. ) )

FUNCTION wvg_GetMessageText( w, p1, p2 )

   LOCAL cText := Replicate( hb_BChar( 0 ), 64000 )

   wapi_SendMessage( w, p1, p2, @cText )

   RETURN Left( cText, At( Chr( 0 ), cText + Chr( 0 ) ) - 1 )

FUNCTION wvt_IsLButtonPressed()
   RETURN hb_bitAnd( wapi_GetKeyState( WIN_VK_LBUTTON ), 0x8000 ) != 0

FUNCTION wvg_FindWindow( cTitle )
   RETURN wapi_FindWindow( , cTitle )

#if 0
FUNCTION wvg_CreateBrush( ... )  /* TOFIX: causes problems due to the GC collected pointer is returns */
   RETURN wapi_CreateBrushIndirect( { ... } )
#endif

FUNCTION wvg_SetDCBrushColor( hDC, nRGB )
   RETURN wapi_SetDCBrushColor( hDC, nRGB )

FUNCTION wvg_SetDCPenColor( hDC, nRGB )
   RETURN wapi_SetDCPenColor( hDC, nRGB )

FUNCTION wvg_SetWindowPosToBack( hWnd )
   RETURN wapi_SetWindowPos( hWnd, WIN_HWND_BOTTOM,,,,, ;
      hb_bitOr( WIN_SWP_NOSIZE, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) )

FUNCTION wvg_SetWindowPosToTop( hWnd )
   RETURN wapi_SetWindowPos( hWnd, WIN_HWND_TOP,,,,, ;
      hb_bitOr( WIN_SWP_NOSIZE, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) )

FUNCTION wvg_SetWindowSize( hWnd, w, h, lPaint )
   RETURN wapi_SetWindowPos( hWnd,, 0, 0, w, h, ;
      iif( hb_defaultValue( lPaint, .F. ),, ;
      hb_bitOr( WIN_SWP_NOREDRAW, WIN_SWP_NOZORDER, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) ) )

FUNCTION wvg_SetWindowPosition( hWnd, x, y, lPaint )
   RETURN wapi_SetWindowPos( hWnd,, x, y, 0, 0, ;
      iif( hb_defaultValue( lPaint, .F. ),, ;
      hb_bitOr( WIN_SWP_NOREDRAW, WIN_SWP_NOZORDER, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) ) )

FUNCTION wvg_SetWindowPosAndSize( hWnd, x, y, w, h, lPaint )
   RETURN wapi_SetWindowPos( hWnd,, x, y, w, h, ;
      iif( hb_defaultValue( lPaint, .F. ),, ;
      hb_bitOr( WIN_SWP_NOREDRAW, WIN_SWP_NOZORDER, WIN_SWP_NOACTIVATE, WIN_SWP_FRAMECHANGED ) ) )

PROCEDURE wvg_ForceWindowToTop( hWnd )

   wapi_SetWindowPos( hWnd, WIN_HWND_TOPMOST,,,,, hb_bitOr( WIN_SWP_NOMOVE, WIN_SWP_NOSIZE ) )
   wapi_SetWindowPos( hWnd, WIN_HWND_NOTOPMOST,,,,, hb_bitOr( WIN_SWP_NOMOVE, WIN_SWP_NOSIZE ) )

   RETURN

FUNCTION wvg_SetWndProc( hWnd, pWndProc )
   RETURN wapi_SetWindowLongPtr( hWnd, WIN_GWLP_WNDPROC, pWndProc )

STATIC FUNCTION wvg_hWnd()
   RETURN hb_gtInfo( HB_GTI_WINHANDLE )

/* wvt_MessageBox( cMessage, cTitle, nIcon, hWnd ) */
FUNCTION wvt_MessageBox( ... )

   LOCAL hWnd := wvg_hWnd()

   IF Empty( hWnd )
      RETURN 0
   ENDIF

   RETURN wapi_MessageBox( hWnd, ... )

/* wvt_DlgSetIcon( hDlg, ncIcon ) */
FUNCTION wvt_DlgSetIcon( hDlg, ncIcon )

   LOCAL hIcon

   IF HB_ISNUMERIC( ncIcon )
      hIcon := wapi_LoadIcon( wapi_GetModuleHandle(), ncIcon )
   ELSEIF Empty( hIcon := wapi_LoadImage( , ncIcon, WIN_IMAGE_ICON,,, WIN_LR_LOADFROMFILE ) )
      hIcon := wapi_LoadImage( wapi_GetModuleHandle(), ncIcon, WIN_IMAGE_ICON )
   ENDIF

   IF ! Empty( hIcon )
      wapi_SendMessage( hDlg, WIN_WM_SETICON, WIN_ICON_SMALL, hIcon )  /* Titlebar icon */
      wapi_SendMessage( hDlg, WIN_WM_SETICON, WIN_ICON_BIG  , hIcon )  /* Tasklist icon */
   ENDIF

   RETURN hIcon

FUNCTION wvg_LoadIcon( ncIcon )

   IF HB_ISNUMERIC( ncIcon )
      RETURN wapi_LoadIcon( wapi_GetModuleHandle(), ncIcon )
   ENDIF

   RETURN wapi_LoadImage( , ncIcon, WIN_IMAGE_ICON,,, WIN_LR_LOADFROMFILE )

/* https://msdn.microsoft.com/en-us/library/windows/desktop/ms648045.aspx
   W2000 and Upper, wapi_LoadImage() can resize image

   nSource: 0 ResourceIdByNumber
   nSource: 1 ResourceIdByName
   nSource: 2 ImageFromDiskFile */
FUNCTION wvg_LoadImage( ncImage, nSource, nBmpOrIcon, nWidth, nHeight )

   hb_defaultValue( @nBmpOrIcon, WIN_IMAGE_BITMAP )

   RETURN iif( hb_defaultValue( nSource, 0 ) == 2, ;
      wapi_LoadImage( , ncImage, nBmpOrIcon, nWidth, nHeight, WIN_LR_LOADFROMFILE + WIN_LR_DEFAULTSIZE ), ;
      wapi_LoadImage( wapi_GetModuleHandle(), ncImage, nBmpOrIcon, nWidth, nHeight, WIN_LR_DEFAULTSIZE ) )

FUNCTION wvg_TrackPopupMenu( hMenu, nFlags, x, y, hWnd )

   LOCAL xy

   IF ! HB_ISNUMERIC( x ) .OR. ! HB_ISNUMERIC( y )
      xy := { => }
      wapi_GetCursorPos( @xy )
      x := xy[ "x" ]
      y := xy[ "y" ]
   ENDIF

   RETURN wapi_TrackPopupMenu( hMenu, ;
      hb_defaultValue( nFlags, hb_bitOr( WIN_TPM_CENTERALIGN, WIN_TPM_RETURNCMD ) ), ;
      x, y, 0, ;
      hb_defaultValue( hWnd, wapi_GetActiveWindow() ) )

FUNCTION wvt_TrackPopupMenu( hMenu )

   LOCAL hWnd := wvg_hWnd()
   LOCAL xy

   IF Empty( hWnd )
      RETURN 0
   ENDIF

   xy := { => }
   wapi_GetCursorPos( @xy )

   RETURN wapi_TrackPopupMenu( hMenu, ;
      hb_bitOr( WIN_TPM_CENTERALIGN, WIN_TPM_RETURNCMD ), ;
      xy[ "x" ], xy[ "y" ], 0, hWnd )

FUNCTION wvt_GetMenu()

   LOCAL hWnd := wvg_hWnd()

   RETURN iif( Empty( hWnd ),, wapi_GetMenu( hWnd ) )

FUNCTION wvg_ChooseColor( nColor, aColor, nFlags, hWnd )
   RETURN win_ChooseColor( hWnd,, nColor, ;
      hb_defaultValue( aColor, AFill( Array( 16 ), wapi_GetSysColor( WIN_COLOR_BTNFACE ) ) ), ;
      hb_defaultValue( nFlags, hb_bitOr( WIN_CC_ANYCOLOR, WIN_CC_RGBINIT, WIN_CC_FULLOPEN ) ) )

FUNCTION wvt_ChooseColor( nColor, aColor, nFlags )

   LOCAL hWnd := wvg_hWnd()

   IF Empty( hWnd )
      RETURN -1
   ENDIF

   RETURN win_ChooseColor( hWnd,, nColor, ;
      hb_defaultValue( aColor, AFill( Array( 16 ), wapi_GetSysColor( WIN_COLOR_BTNFACE ) ) ), ;
      hb_defaultValue( nFlags, hb_bitOr( WIN_CC_ANYCOLOR, WIN_CC_RGBINIT, WIN_CC_FULLOPEN ) ) )
