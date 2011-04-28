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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                           WvtPaint.prg
 *
 *            Routines to manage Wvt*Classes Gui Painting
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "wvtwin.ch"
#include "common.ch"

/*----------------------------------------------------------------------*/

thread static t_paint_:= { { "", {} } }

/*----------------------------------------------------------------------*/
/*
 *        This function must have to be defined in your appls
 */
#if 0
function Wvt_Paint()

   /* Call this function from this funtion */
   WvtPaintObjects()

   return nil
#endif
/*----------------------------------------------------------------------*/

function WvtPaintObjects()
   LOCAL i, lExe, nLeft, nRight, b, tlbr_, aBlocks, nBlocks

   aBlocks := WvtSetPaint()

   if ( nBlocks := len( aBlocks ) ) > 0
      tlbr_:= Wvt_GetPaintRect()

      for i := 1 to nBlocks
         lExe := .t.

         if aBlocks[ i,3 ] <> nil .and. !empty( aBlocks[ i,3 ] )
            /*  Check parameters against tlbr_ depending upon the
             *  type of object and attributes contained in aAttr
             */
            do case
            case aBlocks[ i,3,1 ] == WVT_BLOCK_GRID_V
               b := aBlocks[ i,3,6 ]
               if len( b:aColumnsSep ) == 0
                  lExe := .f.
               else
                  nLeft  := b:aColumnsSep[ 1 ]
                  nRight := b:aColumnsSep[ len( b:aColumnsSep ) ]
                  if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   < bottom */
                        tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm > top    */
                        tlbr_[ 2 ] <= nRight + 1       .and. ; /* left  < right  */
                        tlbr_[ 4 ] >= nLeft  - 2             ) /* right > left   */
                     lExe := .f.
                  endif
               endif

            case aBlocks[ i,3,1 ] == WVT_BLOCK_GETS
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   < bott  */
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm > top   */
                     tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; /* left  < righ  */
                     tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) /* right > left  */
                  lExe := .f.
               endif

            otherwise
               /* If refreshing rectangle's top is less than objects' bottom    */
               /* and left is less than objects' right                          */
               /*                                                               */
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; /* top   <= bottom  */
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; /* bootm >= top     */
                     tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; /* left  < right    */
                     tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) /* right > left     */
                  lExe := .f.
               endif
            endcase
         endif

         if lExe
            eval( aBlocks[ i,2 ] )
         endif
      next
   endif

   return 0

/*----------------------------------------------------------------------*/

function WvtSetPaint( a_ )
   local o
   thread static t

   IF t == nil
      t := {}
   ENDIF

   o := t

   if a_ <> nil
      t := a_
   endif

   return o

/*----------------------------------------------------------------------*/

function SetPaint( cID, nAction, xData, aAttr )
   local n, n1, oldData

   if xData <> nil
      if ( n := ascan( t_paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
         if ( n1 := ascan( t_paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
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

/*----------------------------------------------------------------------*/

function GetPaint( cID )
   local n

   if ( n := ascan( t_paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      return t_paint_[ n,2 ]
   endif

   return {}

/*----------------------------------------------------------------------*/

function DelPaint( cID, nAction )
   local xData, n1, n

   if ( n := ascan( t_paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      if ( n1 := ascan( t_paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
         xData := t_paint_[ n,2,n1,2 ]
         t_paint_[ n,2,n1,2 ] := {|| .t. }
      endif
   endif

   return xData

/*----------------------------------------------------------------------*/

function PurgePaint( cID,lDummy )
   local n, aPaint

   DEFAULT lDummy TO .f.

   if ( n := ascan( t_paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      aPaint := t_paint_[ n ]
      ADel( t_paint_, n )
      aSize( t_paint_, len( t_paint_ ) - 1 )
   endif

   if lDummy
      WvtSetPaint( {} )
   endif

   return aPaint

/*----------------------------------------------------------------------*/

function InsertPaint( cID, aPaint, lSet )
   local n

   DEFAULT lSet TO .f.

   if ( n := ascan( t_paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      t_paint_[ n ] := aPaint
   else
      aadd( t_paint_, aPaint )
   endif

   if lSet
      WvtSetPaint( aPaint )
   endif

   return nil

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *               RunTime Dialog Generation Routines
 *
 *                      Courtesy What32.lib
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

FUNCTION Wvt_MakeDlgTemplate( nTop, nLeft, nRows, nCols, aOffSet, cTitle, nStyle, ;
                              cFaceName, nPointSize, nWeight, lItalic, nHelpId, nExStyle )

   LOCAL aDlg := { {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL aFont

   aFont := Wvt_GetFontInfo()

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

   If !ISNUMBER( nStyle )
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

/*----------------------------------------------------------------------*/

Function Wvt_AddDlgItem( aDlg, nTop, nLeft, nRows, nCols, aOffSet,;
                         cnId, cnDlgClass, nStyle, cText, nHelpId, nExStyle )
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL nBottom, nRight

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
   aAdd( aDlg[ 11 ] , iif( ISCHARACTER( cText ), cText, iif( ISNUMBER( cText ), cText, "" ) ) )
   aAdd( aDlg[ 12 ] , 0 )

   Return aDlg

/*----------------------------------------------------------------------*/

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

   if hDlg <> 0
      if ncIcon <> nil
         Wvt_DlgSetIcon( hDlg, ncIcon )

      endif

      if valtype( nTimerTicks ) == "N"
         WVG_SetTimer( hDlg, 1001, nTimerTicks )

      endif

      if hMenu <> nil
         WVG_SetMenu( hDlg, hMenu )

      endif

   endif

   Return hDlg

/*----------------------------------------------------------------------*/

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

   nResult := Wvt_CreateDialogModal( xTemplate, .f., cbDlgProc, nDlgMode, hWndParent )

   Return nResult

/*----------------------------------------------------------------------*/
/*                       Borrowed from What32

Wvt_GetOpenFileName( hWnd, @cPath, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex )

hWnd:     Handle to parent window
cPath:    (optional) if OFN_ALLOWMULTISELECT the path is stored
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {"Data Bases","*.dbf"},{"Clipper","*.prg"} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types

Returns:  If OFN_ALLOWMULTISELECT
              Array of files selected
          else
              FileName.
          endif
*/
FUNCTION WVT_GetOpenFileName( hWnd, cPath, cTitle, aFilter, nFlags, cIniDir, cDefExt, nIndex )
   local aFiles, cRet, cFile, n, x, c := ""

   IF aFilter == nil
      aFilter := {}
   END
   IF ValType( aFilter ) == "A"
      FOR n := 1 TO LEN( aFilter )
          c += aFilter[n][1] + chr(0) + aFilter[n][2] + chr(0)
      NEXT
      c += chr( 0 )
   ENDIF
   if WVG_And( nFlags,OFN_ALLOWMULTISELECT ) > 0
      cFile := space( 32000 )
     ELSE
      cFile := padr( trim( cPath ), 255, chr( 0 ) )
   END

   cRet := WVT__GetOpenFileName( hWnd, @cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )

   if WVG_And( nFlags,OFN_ALLOWMULTISELECT ) > 0
      n := AT( CHR(0)+ CHR(0), cFile )
      cFile  := LEFT( cFile,n )
      aFiles := {}
      IF n == 0 /* no double chr(0) user must have pressed cancel */
         RETURN aFiles
      END
      x := AT( CHR( 0 ),cFile ) /* fist null */
      cPath := LEFT( cFile,x )

      cFile := STRTRAN( cFile,cPath )
      IF !EMPTY( cFile ) /* user selected more than 1 file */
         c := ""
         FOR n := 1 TO LEN( cFile )
             IF SUBSTR( cFile,n,1 ) == CHR( 0 )
                AADD( aFiles,STRTRAN( cPath, CHR( 0 ) ) +"\"+ c )
                c:=""
                LOOP
             END
             c += SUBSTR( cFile,n,1 )
         NEXT
        ELSE
         /*
         cFile:=cPath
         x:=RAT("\",cFile)
         cPath:=LEFT(cFile,x-1)
         */
         aFiles := { STRTRAN( cPath, CHR( 0 ) ) }
      END
      Return aFiles
   else
      /* cRet := left( cRet, at( chr( 0 ), cRet ) -1 ) */

   end

   Return cRet

/*----------------------------------------------------------------------*/
/*
Wvt_GetSaveFileName( hWnd, cFile, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex)

hWnd:     Handle to parent window
cFile:    (optional) Default FileName
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {"Data Bases","*.dbf"},{"Clipper","*.prg"} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. "DBF"
nIndex:   Index position of types

Returns:  FileName.
*/

FUNCTION WVT_GetSaveFileName( hWnd, cFile, cTitle, aFilter, nFlags, cIniDir, cDefExt, nIndex )
   local n,c:=""

   IF aFilter == nil
      aFilter := {}
   END

   FOR n := 1 TO LEN( aFilter )
       c += aFilter[ n ][ 1 ]+chr( 0 )+aFilter[ n ][ 2 ]+chr( 0 )
   NEXT
   cFile := WVT__GetSaveFileName( hWnd, cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )

   Return cFile

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      C Functions to PRG Ports
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbgtinfo.ch"
#include "hbgtwvg.ch"

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetTitle( cTitle )

   RETURN Hb_GtInfo( HB_GTI_WINTITLE, cTitle )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetTitle()

   RETURN Hb_GtInfo( HB_GTI_WINTITLE )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetIcon( ncIconRes, cIconName )

   if     valtype( ncIconRes ) == "N"
      Hb_GtInfo( HB_GTI_ICONRES, ncIconRes )

   elseif valtype( cIconName ) == "C"
      Hb_GtInfo( HB_GTI_ICONRES, cIconName )

   elseif valtype( ncIconRes ) == "C"
      Hb_GtInfo( HB_GTI_ICONFILE, ncIconRes )

   endif

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetFont( cFontName, nSize, nWidth, nWeight, nQuality )

   DEFAULT cFontName TO Hb_GtInfo( HB_GTI_FONTNAME    )
   DEFAULT nWidth    TO Hb_GtInfo( HB_GTI_FONTWIDTH   )
   DEFAULT nWeight   TO Hb_GtInfo( HB_GTI_FONTWEIGHT  )
   DEFAULT nQuality  TO Hb_GtInfo( HB_GTI_FONTQUALITY )
   DEFAULT nSize     TO Hb_GtInfo( HB_GTI_FONTSIZE    )

   RETURN Hb_GtInfo( HB_GTI_SETFONT, { cFontName, nSize, nWidth, nWeight, nQuality } )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetCodePage( nCodePage )

   RETURN Hb_GtInfo( HB_GTI_CODEPAGE, nCodePage )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetPalette()

   RETURN Hb_GtInfo( HB_GTI_PALETTE )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetPalette( aRGB )

   RETURN Hb_GtInfo( HB_GTI_PALETTE, aRGB )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetRGBColor( nIndex )

   RETURN Hb_GtInfo( HB_GTI_PALETTE, nIndex )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetAltF4Close( lSetClose )

   RETURN Hb_GtInfo( HB_GTI_CLOSABLE, lSetClose )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetScreenWidth()

   RETURN Hb_GtInfo( HB_GTI_DESKTOPWIDTH )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetScreenHeight()

   RETURN Hb_GtInfo( HB_GTI_DESKTOPHEIGHT )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetWindowHandle()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_CenterWindow( lCenter, lRePaint )

   DEFAULT lCenter  TO .t.
   DEFAULT lRePaint TO .f.

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_WindowCentre( lCenter, lRePaint )

   DEFAULT lCenter  TO .t.
   DEFAULT lRePaint TO .f.

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_CENTERWINDOW, { lCenter, lRePaint } )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_ProcessMessages()

   Hb_GtInfo( HB_GTI_SPEC, HB_GTS_PROCESSMESSAGES )

   RETURN .t.

/*----------------------------------------------------------------------*/

FUNCTION Wvt_KeyBoard( nKey )

   Hb_GtInfo( HB_GTI_SPEC, HB_GTS_KEYBOARD, nKey )

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Wvt_GetClipboard()

   RETURN Hb_GtInfo( HB_GTI_CLIPBOARDDATA )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetClipboard( cText )

   RETURN Hb_GtInfo( HB_GTI_CLIPBOARDDATA, cText )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_PasteFromClipboard()
   Local cText, nLen, i

   cText := Hb_GtInfo( HB_GTI_CLIPBOARDDATA )
   if ( nLen := Len( cText ) ) > 0
      for i := 1 to nLen
         Wvt_KeyBoard( asc( substr( cText, i, 1 ) ) )
      next
   endif

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Wvt_ResetWindow()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_RESETWINDOW )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetTimer( nTimerID, nMiliSeconds )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SETTIMER, { nTimerID, nMiliSeconds } )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_KillTimer( nTimerID )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_KILLTIMER, nTimerID )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetOnTop()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETONTOP )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetAsNormal()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_SETASNORMAL )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Minimize()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MINIMIZED )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Maximize()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Hide()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_HIDDEN )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Show()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_NORMAL )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_SetWindowPos( nX, nY )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SETPOSITION, { nX, nY } )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_ShowWindow( nState )

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, nState )

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Update()

   RETURN Hb_GtInfo( HB_GTI_SPEC, HB_GTS_UPDATEWINDOW )

/*----------------------------------------------------------------------*/
