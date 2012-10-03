/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
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
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
//
//
//
/*
 *                           WvtPaint.prg
 *
 *            Routines to manage Wvt*Classes Gui Painting
 */
//
//
//

#include "wvtwin.ch"
#include "common.ch"

//

thread static t_paint_:= { { "", {} } }

//
/*
 *        This function must have to be defined in your appls
 */
#if 0
function Wvt_Paint()

   /* Call this function from this funtion */
   WvtPaintObjects()

   return nil
#endif
//

function WvtPaintObjects()
   LOCAL i, lExe, nLeft, nRight, b, tlbr_, aBlocks, nBlocks

   aBlocks := WvtSetPaint()

   if ( nBlocks := len( aBlocks ) ) > 0
      tlbr_:= Wvt_GetPaintRect()

      for i := 1 to nBlocks
         lExe := .T.

         if aBlocks[ i,3 ] != nil .and. !empty( aBlocks[ i,3 ] )
            /*  Check parameters against tlbr_ depending upon the
             *  type of object and attributes contained in aAttr
             */
            do case
            case aBlocks[ i,3,1 ] == WVT_BLOCK_GRID_V
               b := aBlocks[ i,3,6 ]
               if len( b:aColumnsSep ) == 0
                  lExe := .F.
               else
                  nLeft  := b:aColumnsSep[ 1 ]
                  nRight := b:aColumnsSep[ len( b:aColumnsSep ) ]
                  if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   < bottom */
                        tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm > top    */
                        tlbr_[ 2 ] <= nRight + 1       .and. ; /* left  < right  */
                        tlbr_[ 4 ] >= nLeft  - 2             ) /* right > left   */
                     lExe := .F.
                  endif
               endif

            case aBlocks[ i,3,1 ] == WVT_BLOCK_GETS
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   < bott  */
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm > top   */
                     tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; /* left  < righ  */
                     tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) /* right > left  */
                  lExe := .F.
               endif

            otherwise
               /* If refreshing rectangle's top is less than objects' bottom    */
               /* and left is less than objects' right                          */
               /*                                                               */
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   <= bottom  */
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm >= top     */
                     tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; /* left  < right    */
                     tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) /* right > left     */
                  lExe := .F.
               endif
            endcase
         endif

         if lExe
            eval( aBlocks[ i,2 ] )
         endif
      next
   endif

   return 0

//

function WvtSetPaint( a_ )
   local o
   thread static t

   IF t == nil
      t := {}
   ENDIF

   o := t

   if a_ != nil
      t := a_
   endif

   return o

//

function wvg_SetPaint( cID, nAction, xData, aAttr )
   local n, n1, oldData

   if xData != nil
      if ( n := ascan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
         if ( n1 := ascan( t_paint_[ n, 2 ], {| e_ | e_[ 1 ] == nAction } ) ) > 0
            oldData := t_paint_[ n,2,n1,2 ]
            t_paint_[ n,2,n1,2 ] := xData
            t_paint_[ n,2,n1,3 ] := aAttr
         else
            aadd( t_paint_[ n,2 ], { nAction,xData,aAttr } )
         endif
      else
         aadd( t_paint_, { cID, {} } )
         n := len( t_paint_ )
         aadd( t_paint_[ n,2 ], { nAction, xData, aAttr } )
      endif
   endif

   return oldData

//

function wvg_GetPaint( cID )
   local n

   if ( n := ascan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      return t_paint_[ n,2 ]
   endif

   return {}

//

function wvg_DelPaint( cID, nAction )
   local xData, n1, n

   if ( n := ascan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      if ( n1 := ascan( t_paint_[ n, 2 ], {| e_ | e_[ 1 ] == nAction } ) ) > 0
         xData := t_paint_[ n,2,n1,2 ]
         t_paint_[ n,2,n1,2 ] := {|| .T. }
      endif
   endif

   return xData

//

function wvg_PurgePaint( cID,lDummy )
   local n, aPaint

   DEFAULT lDummy TO .F.

   if ( n := ascan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      aPaint := t_paint_[ n ]
      ADel( t_paint_, n )
      aSize( t_paint_, len( t_paint_ ) - 1 )
   endif

   if lDummy
      WvtSetPaint( {} )
   endif

   return aPaint

//

function wvg_InsertPaint( cID, aPaint, lSet )
   local n

   DEFAULT lSet TO .F.

   if ( n := ascan( t_paint_, {| e_ | e_[ 1 ] == cID } ) ) > 0
      t_paint_[ n ] := aPaint
   else
      aadd( t_paint_, aPaint )
   endif

   if lSet
      WvtSetPaint( aPaint )
   endif

   return nil

//
//
//
/*
 *               RunTime Dialog Generation Routines
 *
 *                      Courtesy What32.lib
 */
//
//
//
/* nMode : 0 == Rows/cols - DEFAULT    1 == DlagUnits as from any standard dialog definition */
FUNCTION Wvt_SetDlgCoMode( nMode )
   LOCAL nOldMode

   STATIC sMode := 0

   nOldMode := sMode
   IF HB_ISNUMERIC( nMode ) .and. nMode <= 1 .and. nMode >= 0
      sMode := nMode
   ENDIF

   RETURN nOldMode

//


FUNCTION Wvt_MakeDlgTemplate( nTop, nLeft, nRows, nCols, aOffSet, cTitle, nStyle, ;
                              cFaceName, nPointSize, nWeight, lItalic, nHelpId, nExStyle )

   LOCAL aDlg := { {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL aFont
   LOCAL nMode := Wvt_SetDlgCoMode()

   aFont := Wvt_GetFontInfo()

   IF nMode == 0
      DEFAULT aOffSet TO {}
      aSize( aOffSet,4 )
      DEFAULT aOffSet[ 1 ] TO 0
      DEFAULT aOffSet[ 2 ] TO 0
      DEFAULT aOffSet[ 3 ] TO 0
      DEFAULT aOffSet[ 4 ] TO 0

      nBaseUnits  := WVG_GetDialogBaseUnits()
      nBaseUnitsX := WVG_LOWORD( nBaseUnits )
      nBaseUnitsY := WVG_HIWORD( nBaseUnits )

      nW := aFont[ 7 ] * nCols + aOffSet[ 4 ]
      nH := aFont[ 6 ] * nRows + aOffSet[ 3 ]

      /* Position it exactly where user has requested */

      aXY := Wvt_ClientToScreen( nTop,nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      /* MSDN says DlgBaseUnits and Screen Coordinates has multiplier of 4,8 for x & Y.
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

   If !HB_ISNUMERIC( nStyle )
      nStyle := + WS_CAPTION    + WS_SYSMENU              ;
                + WS_GROUP      + WS_TABSTOP + DS_SETFONT ;
                + WS_THICKFRAME + WS_VISIBLE + WS_POPUP   ;
                + DS_3DLOOK
   EndIf

   aAdd( aDlg[ 1 ] , iif( Empty( nHelpId  ), 0, nHelpId  ) )
   aAdd( aDlg[ 1 ] , iif( Empty( nExStyle ), 0, nExStyle ) )
   aAdd( aDlg[ 1 ] , nStyle  )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , nX      )
   aAdd( aDlg[ 1 ] , nY      )
   aAdd( aDlg[ 1 ] , nW      )
   aAdd( aDlg[ 1 ] , nH      )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , iif( ValType( cTitle ) == "C", cTitle, "" ) )

   if hb_bitAnd( nStyle, DS_SETFONT ) == DS_SETFONT
      aAdd( aDlg[ 1 ], iif( ValType( nPointSize ) == "N", nPointSize, 8               ) )
      aAdd( aDlg[ 1 ], iif( ValType( nWeight    ) == "N", nWeight   , 400             ) )
      aAdd( aDlg[ 1 ], iif( ValType( lItalic    ) == "L", lItalic   , .F.             ) )
      aAdd( aDlg[ 1 ], iif( ValType( cFaceName  ) == "C", cFaceName , "MS Sans Serif" ) )
   EndIf

   Return aDlg

//

Function Wvt_AddDlgItem( aDlg, nTop, nLeft, nRows, nCols, aOffSet,;
                         cnId, cnDlgClass, nStyle, cText, nHelpId, nExStyle )
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL nBottom, nRight
   LOCAL nMode := Wvt_SetDlgCoMode()

   IF nMode == 0
      nBottom := nTop  + nRows - 1
      nRight  := nLeft + nCols - 1

      DEFAULT aOffSet TO {}

      aSize( aOffSet,4 )

      DEFAULT aOffSet[ 1 ] TO 0
      DEFAULT aOffSet[ 2 ] TO 0
      DEFAULT aOffSet[ 3 ] TO 0
      DEFAULT aOffSet[ 4 ] TO 0

      nBaseUnits  := WVG_GetDialogBaseUnits()
      nBaseUnitsX := WVG_LOWORD( nBaseUnits )
      nBaseUnitsY := WVG_HIWORD( nBaseUnits )

      aXY := Wvt_GetXYFromRowCol( nTop, nLeft )
      nX  := aXY[ 1 ] + aOffSet[ 2 ]
      nY  := aXY[ 2 ] + aOffSet[ 1 ]

      aXY := Wvt_GetXYFromRowCol( nBottom+1, nRight+1 )
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

   aDlg[ 1,4 ]++      /* item count */

   aAdd( aDlg[  2 ] , iif( ValType( nHelpId  ) == "N", nHelpId , 0                     ) )
   aAdd( aDlg[  3 ] , iif( ValType( nExStyle ) == "N", nExStyle, 0                     ) )
   aAdd( aDlg[  4 ] , iif( ValType( nStyle   ) == "N", nStyle  , WS_CHILD + WS_VISIBLE ) )
   aAdd( aDlg[  5 ] , nX         )
   aAdd( aDlg[  6 ] , nY         )
   aAdd( aDlg[  7 ] , nW         )
   aAdd( aDlg[  8 ] , nH         )
   aAdd( aDlg[  9 ] , cnId       )
   aAdd( aDlg[ 10 ] , cnDlgClass )
   aAdd( aDlg[ 11 ] , iif( HB_ISSTRING( cText ), cText, iif( HB_ISNUMERIC( cText ), cText, "" ) ) )
   aAdd( aDlg[ 12 ] , 0 )

   Return aDlg

//

Function Wvt_CreateDialog( acnDlg, lOnTop, cbDlgProc, ncIcon, nTimerTicks, hMenu )
   LOCAL hDlg, cType, xTemplate, nDlgMode

   if valtype( cbDlgProc ) == "C"
      cbDlgProc := upper( cbDlgProc )
   endif

   cType    := Valtype( acnDlg )
   nDlgMode := iif( cType == "C", 0, iif( cType == "N", 1, 2 ) )

   if cType == "A"
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[  2 ] , acnDlg[  3 ] , acnDlg[  4 ] , ;
                                         acnDlg[ 5 ] , acnDlg[  6 ] , acnDlg[  7 ] , acnDlg[  8 ] , ;
                                         acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )
   else
      xTemplate := acnDlg
   endif

   hDlg := Wvt_CreateDialogDynamic( xTemplate, lOnTop, cbDlgProc, nDlgMode )

   if hDlg != 0
      if ncIcon != nil
         Wvt_DlgSetIcon( hDlg, ncIcon )

      endif

      if valtype( nTimerTicks ) == "N"
         WVG_SetTimer( hDlg, 1001, nTimerTicks )

      endif

      if hMenu != nil
         WVG_SetMenu( hDlg, hMenu )

      endif

   endif

   Return hDlg

//

Function Wvt_DialogBox( acnDlg, cbDlgProc, hWndParent )
   LOCAL nResult, cType, xTemplate, nDlgMode

   if valtype( cbDlgProc ) == "C"
      cbDlgProc := upper( cbDlgProc )
   endif

   cType    := Valtype( acnDlg )
   nDlgMode := iif( cType == "C", 0, iif( cType == "N", 1, 2 ) )

   if cType == "A"
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[  2 ] , acnDlg[  3 ] , acnDlg[  4 ] , ;
                                         acnDlg[ 5 ] , acnDlg[  6 ] , acnDlg[  7 ] , acnDlg[  8 ] , ;
                                         acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )
   else
      xTemplate := acnDlg
   endif

   nResult := Wvt_CreateDialogModal( xTemplate, .F., cbDlgProc, nDlgMode, hWndParent )

   Wvg_SetFocus( hWndParent )

   Return nResult

//
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
FUNCTION WVT_GetOpenFileName( hWnd, cPath, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex, cDefName )
   LOCAL cRet, aTmp, xRet, i

   HB_SYMBOL_UNUSED( hWnd )

   DEFAULT cPath  TO ""
   DEFAULT nFlags TO OFN_EXPLORER + OFN_NOCHANGEDIR

/* WIN_GETOPENFILENAME( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := WIN_GetOpenFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /*nBufferSize*/, cDefName )

   IF WVG_And( nFlags, OFN_ALLOWMULTISELECT ) > 0
      xRet := {}
      IF ! empty( aTmp := hb_aTokens( cRet, chr( 0 ) ) )
         cPath := aTmp[ 1 ]
         FOR i := 2 TO len( aTmp )
            aadd( xRet, cPath + "\" + aTmp[ i ] )
         NEXT
      ENDIF
   ELSE
      xRet := cRet
   ENDIF

   RETURN xRet

//
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

FUNCTION WVT_GetSaveFileName( hWnd, cDefName, cTitle, acFilter, nFlags, cInitDir, cDefExt, nFilterIndex )
   LOCAL cRet, aTmp, xRet, i, cPath

   HB_SYMBOL_UNUSED( hWnd )

   DEFAULT nFlags TO OFN_EXPLORER + OFN_NOCHANGEDIR

/* WIN_GETSAVEFILENAME( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 */
   cRet := WIN_GetSaveFileName( @nFlags, cTitle, cInitDir, cDefExt, acFilter, @nFilterIndex, /*nBufferSize*/, cDefName )

   IF WVG_And( nFlags, OFN_ALLOWMULTISELECT ) > 0
      xRet := {}
      IF ! empty( aTmp := hb_aTokens( cRet, chr( 0 ) ) )
         cPath := aTmp[ 1 ]
         FOR i := 2 TO len( aTmp )
            aadd( xRet, cPath + "\" + aTmp[ i ] )
         NEXT
      ENDIF
   ELSE
      xRet := cRet
   ENDIF

   RETURN xRet

//
//
//
/*
 *                      C Functions to PRG Ports
 */
//
//
//

#include "hbgtinfo.ch"
#include "hbgtwvg.ch"

//

FUNCTION Wvt_SetTitle( cTitle )

   RETURN Hb_GtInfo( HB_GTI_WINTITLE, cTitle )

//

FUNCTION Wvt_GetTitle()

   RETURN Hb_GtInfo( HB_GTI_WINTITLE )

//

FUNCTION Wvt_SetIcon( ncIconRes, cIconName )

   if     valtype( ncIconRes ) == "N"
      Hb_GtInfo( HB_GTI_ICONRES, ncIconRes )

   elseif valtype( cIconName ) == "C"
      Hb_GtInfo( HB_GTI_ICONRES, cIconName )

   elseif valtype( ncIconRes ) == "C"
      Hb_GtInfo( HB_GTI_ICONFILE, ncIconRes )

   endif

   RETURN NIL

//

FUNCTION Wvt_SetFont( cFontName, nSize, nWidth, nWeight, nQuality )

   DEFAULT cFontName TO Hb_GtInfo( HB_GTI_FONTNAME    )
   DEFAULT nWidth    TO Hb_GtInfo( HB_GTI_FONTWIDTH   )
   DEFAULT nWeight   TO Hb_GtInfo( HB_GTI_FONTWEIGHT  )
   DEFAULT nQuality  TO Hb_GtInfo( HB_GTI_FONTQUALITY )
   DEFAULT nSize     TO Hb_GtInfo( HB_GTI_FONTSIZE    )

   RETURN Hb_GtInfo( HB_GTI_SETFONT, { cFontName, nSize, nWidth, nWeight, nQuality } )

//

FUNCTION Wvt_SetCodePage( nCodePage )

   RETURN Hb_GtInfo( HB_GTI_CODEPAGE, nCodePage )

//

FUNCTION Wvt_GetPalette()

   RETURN Hb_GtInfo( HB_GTI_PALETTE )

//

FUNCTION Wvt_SetPalette( aRGB )

   RETURN Hb_GtInfo( HB_GTI_PALETTE, aRGB )

//

FUNCTION Wvt_GetRGBColor( nIndex )

   RETURN Hb_GtInfo( HB_GTI_PALETTE, nIndex )

//
#define BLACK          RGB( 0x0 ,0x0 ,0x0  )
#define BLUE           RGB( 0x0 ,0x0 ,0x85 )
#define GREEN          RGB( 0x0 ,0x85,0x0  )
#define CYAN           RGB( 0x0 ,0x85,0x85 )
#define RED            RGB( 0x85,0x0 ,0x0  )
#define MAGENTA        RGB( 0x85,0x0 ,0x85 )
#define BROWN          RGB( 0x85,0x85,0x0  )
#define LIGHT_GRAY     RGB( 0xC6,0xC6,0xC6 )
#define GRAY           RGB( 0x60,0x60,0x60 )
#define BRIGHT_BLUE    RGB( 0x00,0x00,0xFF )
#define BRIGHT_GREEN   RGB( 0x60,0xFF,0x60 )
#define BRIGHT_CYAN    RGB( 0x60,0xFF,0xFF )
#define BRIGHT_RED     RGB( 0xF8,0x00,0x26 )
#define BRIGHT_MAGENTA RGB( 0xFF,0x60,0xFF )
#define YELLOW         RGB( 0xFF,0xFF,0x00 )
#define WHITE          RGB( 0xFF,0xFF,0xFF )

FUNCTION Wvt_GetRGBColorByString( cColor, nForeBack )
   LOCAL s, n, lEnh
   LOCAL nIndex := 0
   LOCAL a_:= { "N", "B", "G", "BG", "R", "RB", "GR", "W" }

   nForeBack := iif( HB_ISNUMERIC( nForeBack ), nForeBack, 0 )

   IF HB_ISSTRING( cColor )
      IF ( n := at( "/", cColor ) ) > 0
         IF nForeBack == 0
            s := substr( cColor, 1, n-1 )
         ELSE
            s := substr( cColor, n+1 )
         ENDIF
      ELSE
         s := cColor
      ENDIF
      s := upper( s )
      lEnh := ( "*" $ s ) .OR. ( "+" $ s )
      IF lEnh
         s := strtran( s, "*" )
         s := strtran( s, "+" )
      ENDIF
      nIndex := ascan( a_, {| e | e == s } )
      IF nIndex > 0
         IF lEnh
            nIndex += 8
         ENDIF
         nIndex--
      ENDIF
   ENDIF

   RETURN Hb_GtInfo( HB_GTI_PALETTE, nIndex )

//

FUNCTION Wvt_SetAltF4Close( lSetClose )

   RETURN Hb_GtInfo( HB_GTI_CLOSABLE, lSetClose )

//

FUNCTION Wvt_GetScreenWidth()

   RETURN Hb_GtInfo( HB_GTI_DESKTOPWIDTH )

//

FUNCTION Wvt_GetScreenHeight()

   RETURN Hb_GtInfo( HB_GTI_DESKTOPHEIGHT )

//

FUNCTION Wvt_GetWindowHandle()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )

//

FUNCTION Wvt_CenterWindow( lCenter, lRePaint )

   DEFAULT lCenter  TO .T.
   DEFAULT lRePaint TO .F.

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

//

FUNCTION Wvt_WindowCentre( lCenter, lRePaint )

   DEFAULT lCenter  TO .T.
   DEFAULT lRePaint TO .F.

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

//

FUNCTION Wvt_ProcessMessages()

   Hb_GtInfo( HB_GTI_SPEC, HB_GTS_PROCESSMESSAGES )

   RETURN .T.

//

FUNCTION Wvt_KeyBoard( nKey )

   Hb_GtInfo( HB_GTI_SPEC, HB_GTS_KEYBOARD, nKey )

   RETURN NIL

//

FUNCTION Wvt_GetClipboard()

   RETURN Hb_GtInfo( HB_GTI_CLIPBOARDDATA )

//

FUNCTION Wvt_SetClipboard( cText )

   RETURN Hb_GtInfo( HB_GTI_CLIPBOARDDATA, cText )

//

FUNCTION Wvt_PasteFromClipboard()
   Local cText, nLen, i

   cText := Hb_GtInfo( HB_GTI_CLIPBOARDDATA )
   if ( nLen := Len( cText ) ) > 0
      for i := 1 to nLen
         Wvt_KeyBoard( asc( substr( cText, i, 1 ) ) )
      next
   endif

   RETURN NIL

//

FUNCTION Wvt_ResetWindow()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_RESETWINDOW )

//

FUNCTION Wvt_SetTimer( nTimerID, nMiliSeconds )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SETTIMER, { nTimerID, nMiliSeconds } )

//

FUNCTION Wvt_KillTimer( nTimerID )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_KILLTIMER, nTimerID )

//

FUNCTION Wvt_SetOnTop()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETONTOP )

//

FUNCTION Wvt_SetAsNormal()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETASNORMAL )

//

FUNCTION Wvt_Minimize()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MINIMIZED )

//

FUNCTION Wvt_Maximize()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

//

FUNCTION Wvt_Hide()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_HIDDEN )

//

FUNCTION Wvt_Show()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_NORMAL )

//

FUNCTION Wvt_SetWindowPos( nX, nY )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SETPOSITION, { nX, nY } )

//

FUNCTION Wvt_ShowWindow( nState )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, nState )

//

FUNCTION Wvt_Update()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_UPDATEWINDOW )

//
