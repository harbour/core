/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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
/*-*/
/*-*/
/*-*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpPartHandler Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               08Nov2008
 */
/*-*/
/*-*/
/*-*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*-*/

CLASS WvgSysWindow INHERIT WvgPartHandler


   METHOD   new( oParent, oOwner, aPos )
   METHOD   create( oParent, oOwner, aPos )
   METHOD   configure()
   METHOD   destroy()

   METHOD   disable()
   METHOD   enable()
   METHOD   hide()
   METHOD   show()
   METHOD   setPos( aPos )

   METHOD   currentPos()
   METHOD   currentSize()

   DATA     aPos                                  INIT    { 0, 0 }

   DATA     hWnd                                  PROTECTED
   DATA     nOldProc                              PROTECTED
   DATA     nWndProc                              PROTECTED


   DATA     sl_helpRequest
   ACCESS   helpRequest                           INLINE  ::sl_helpRequest
   ASSIGN   helpRequest( bBlock )                 INLINE  ::sl_helpRequest := bBlock

   DATA     sl_move
   ACCESS   move                                  INLINE  ::sl_move
   ASSIGN   move( bBlock )                        INLINE  ::sl_move := bBlock

   DATA     sl_quit
   ACCESS   quit                                  INLINE  ::sl_quit
   ASSIGN   quit( bBlock )                        INLINE  ::sl_quit := bBlock

   ENDCLASS

/*-*/

METHOD WvgSysWindow:new( oParent, oOwner, aPos )

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner
   DEFAULT aPos    TO ::aPos

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:new( oParent, oOwner )

   RETURN Self

/*-*/

METHOD WvgSysWindow:create( oParent, oOwner, aPos )

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner
   DEFAULT aPos    TO ::aPos

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:create( oParent, oOwner )

   RETURN Self

/*-*/

METHOD WvgSysWindow:configure()

   RETURN Self

/*-*/

METHOD WvgSysWindow:destroy()

   RETURN Self

/*-*/

METHOD WvgSysWindow:disable()

   RETURN Self

/*-*/

METHOD WvgSysWindow:enable()

   RETURN Self

/*-*/

METHOD WvgSysWindow:hide()

   RETURN Self

/*-*/

METHOD WvgSysWindow:show()

   RETURN Self

/*-*/

METHOD WvgSysWindow:setPos( aPos )

   WVG_SetWindowPosition( ::hWnd, aPos[ 1 ], aPos[ 2 ], .f. )

   RETURN Self

/*-*/

METHOD WvgSysWindow:currentPos()
   LOCAL aRect

   aRect := WVG_GetWindowRect( ::hWnd )

   RETURN { aRect[ 1 ], aRect[ 2 ] }

/*-*/

METHOD WvgSysWindow:currentSize()
   LOCAL aRect

   aRect := WVG_GetClientRect( ::hWnd )

   RETURN { aRect[ 3 ] - aRect[ 1 ], aRect[ 4 ] - aRect[ 2 ] }

/*-*/
/*-*/
/*-*/
/*
 *                       Class WvgFontDialog()
 */
/*-*/
/*-*/
/*-*/

CLASS WvgFontDialog INHERIT WvgSysWindow

   /* Appearance */
   DATA     title                                 INIT   ""
   DATA     buttonApply                           INIT   .F.
   DATA     buttonCancel                          INIT   .T.
   DATA     buttonHelp                            INIT   .F.
   DATA     buttonOk                              INIT   .T.
   DATA     buttonReset                           INIT   .F.
   DATA     strikeOut                             INIT   .T.
   DATA     underscore                            INIT   .T.

   DATA     name                                  INIT   .T.
   DATA     style                                 INIT   .T.
   DATA     size                                  INIT   .T.

   DATA     displayFilter                         INIT   .T.
   DATA     printerFilter                         INIT   .T.

   DATA     familyName                            INIT   " "
   DATA     nominalPointSize                      INIT   0

   DATA     bitmapOnly                            INIT   .F.
   DATA     fixedOnly                             INIT   .F.
   DATA     proportionalOnly                      INIT   .T.


   DATA     outLine                               INIT   .T.
   DATA     previewBGClr                          INIT   RGB( 255,255,255 )
   DATA     previewFGClr                          INIT   RGB( 0,0,0 )
   DATA     previewString                         INIT   " "
   DATA     printerPS                             INIT   NIL
   DATA     screenPS                              INIT   NIL

   DATA     synthesizeFonts                       INIT   .T.

   DATA     vectorOnly                            INIT   .F.
   DATA     vectorSizes                           INIT   {}

   DATA     viewPrinterFonts                      INIT   .F.
   DATA     viewScreenFonts                       INIT   .T.

   METHOD   new( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD   create( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD   destroy()
   METHOD   display( nMode )

   DATA     sl_activateApply
   ACCESS   activateApply                         INLINE ::sl_activateApply
   ASSIGN   activateApply( bBlock )               INLINE ::sl_activateApply := bBlock

   DATA     sl_activateCancel
   ACCESS   activateCancel                        INLINE ::sl_activateCancel
   ASSIGN   activateCancel( bBlock )              INLINE ::sl_activateCancel := bBlock

   DATA     sl_activateOk
   ACCESS   activateOk                            INLINE ::sl_activateOk
   ASSIGN   activateOk( bBlock )                  INLINE ::sl_activateOk := bBlock

   DATA     sl_activateReset
   ACCESS   activateReset                         INLINE ::sl_activateReset
   ASSIGN   activateReset( bBlock )               INLINE ::sl_activateReset := bBlock

   DATA     oScreenPS
   DATA     oPrinterPS
   DATA     aPos                                  INIT   { 0, 0 }
   DATA     ok                                    INIT   .f.

   METHOD   wndProc( hWnd, nMessage, nwParam, nlParam )
   METHOD   GetWvgFont( aFont )                   PROTECTED

   ENDCLASS

/*-*/

METHOD new( oParent, oOwner, oScreenPS, oPrinterPS, aPos ) CLASS WvgFontDialog

   DEFAULT oParent    TO ::oParent
   DEFAULT oOwner     TO ::oOwner
   DEFAULT oScreenPS  TO ::oScreenPS
   DEFAULT oPrinterPS TO ::oPrinterPS
   DEFAULT aPos       TO ::aPos

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   ::WvgSysWindow:new( oParent, oOwner )

   RETURN Self

/*-*/

METHOD create( oParent, oOwner, oScreenPS, oPrinterPS, aPos ) CLASS WvgFontDialog

   DEFAULT oParent    TO ::oParent
   DEFAULT oOwner     TO ::oOwner
   DEFAULT oScreenPS  TO ::oScreenPS
   DEFAULT oPrinterPS TO ::oPrinterPS
   DEFAULT aPos       TO ::aPos

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   IF ::viewPrinterFonts .and. ::oPrinterPS == NIL
      ::viewPrinterFonts := .f.
   ENDIF
   IF ( ! ::viewScreenFonts .and. ! ::viewPrinterFonts )
      ::viewScreenFonts := .t.
   ENDIF

   ::WvgSysWindow:create( oParent, oOwner )

   /* ::nWndProc := hb_AsCallBack( "WNDPROC", Self ) */

   RETURN Self

/*-*/

METHOD wndProc( hWnd, nMessage, nwParam, nlParam ) CLASS WvgFontDialog
   LOCAL aRect, nL, nH

   HB_SYMBOL_UNUSED( nlParam )

   DO CASE

   CASE nMessage == WM_INITDIALOG
      ::hWnd := hWnd

      IF !empty( ::title )
         WVG_SetWindowText( ::hWnd, ::title )
      ENDIF
      IF !( ::buttonCancel )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,IDCANCEL ), .f. )
      ENDIF
      IF !( ::buttonApply )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1026 ), .f. )
      ENDIF
      IF !( ::buttonHelp )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1038 ), .f. )
      ENDIF
      IF !( ::strikeOut )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1040 ), .f. )
      ENDIF
      IF !( ::underscore )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1041 ), .f. )
      ENDIF
      IF !( ::name )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1136 ), .f. )
      ENDIF
      IF !( ::style )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1137 ), .f. )
      ENDIF
      IF !( ::size )
         WVG_EnableWindow( WVG_GetDlgItem( ::hWnd,1138 ), .f. )
      ENDIF

      IF ::aPos[ 1 ] > 0 .OR. ::aPos[ 2 ] > 0
         aRect := WVG_GetWindowRect( ::hWnd )
         WVG_MoveWindow( ::hWnd, ::aPos[ 1 ], ::aPos[ 2 ], aRect[3]-aRect[1], aRect[4]-aRect[2], .f. )
      ENDIF

      RETURN 1

   CASE nMessage == WM_COMMAND
      nL := WVG_LOWORD( nwParam )
      nH := WVG_HIWORD( nwParam )

      HB_SYMBOL_UNUSED( nH )

      DO CASE

      CASE nL == IDOK
         ::ok := .t.
         IF HB_ISBLOCK( ::sl_activateOk )
            eval( ::sl_activateOk, ::GetWvgFont(), NIL, Self )
         ENDIF

      CASE nL == IDCANCEL
         IF HB_ISBLOCK( ::sl_activateCancel )
            eval( ::sl_activateCancel, NIL, NIL, Self )
         ENDIF

      CASE nL == 1026
         IF HB_ISBLOCK( ::sl_activateApply )
            eval( ::sl_activateApply, ::GetWvgFont(), NIL, Self )
         ENDIF

      CASE nL == 1038  /* Help */

      ENDCASE

   ENDCASE

   RETURN 0

/*-*/

METHOD display( nMode ) CLASS WvgFontDialog
   LOCAL hWnd, aInfo

   IF nMode == 0
      hWnd := ::oParent:hWnd
   ELSE
      hWnd := WVG_GetDesktopWindow()
   ENDIF

   ::ok := .f.
   aInfo := Wvg_ChooseFont( hWnd, {|h,m,w,l| ::wndProc( h,m,w,l ) }, ::familyName, ;
                            ::nominalPointSize, ::viewScreenFonts, ::viewPrinterFonts )
   IF !( ::ok )
      RETURN NIL
   ENDIF

   RETURN ::GetWvgFont( aInfo )

/*-*/

METHOD destroy() CLASS WvgFontDialog

   /* hb_FreeCallBack( ::nWndProc ) */

   RETURN Self

/*-*/
/*
 * Only callable from ::activateOK and ::activateApply
 */
METHOD GetWvgFont( aFont ) CLASS WvgFontDialog
   LOCAL oWvgFont

   DEFAULT aFont TO Wvg_ChooseFont_GetLogFont( ::hWnd )

   oWvgFont := WvgFont():new()

   oWvgFont:familyName       := aFont[ 1 ]
   oWvgFont:height           := aFont[ 2 ]
   oWvgFont:nominalPointSize := Wvg_HeightToPointSize( /* hdc */, oWvgFont:height )
   oWvgFont:width            := aFont[ 3 ]
   oWvgFont:bold             := aFont[ 4 ] > 400
   oWvgFont:italic           := aFont[ 5 ]
   oWvgFont:underscore       := aFont[ 6 ]
   oWvgFont:strikeOut        := aFont[ 7 ]
   oWvgFont:codePage         := aFont[ 8 ]
   oWvgFont:setCompoundName( trim( aFont[ 1 ] +" "+ iif( oWvgFont:bold, "Bold ", "" ) + ;
                                                    iif( oWvgFont:italic, "Italic", "" ) ) )
   oWvgFont:create()

   RETURN oWvgFont

/*-*/
/*-*/
/*-*/
/*
                            Class WvgFont()
*/
/*-*/
/*-*/
/*-*/

CLASS WvgFont

   DATA     hFont
   DATA     oPS
   DATA     hdc

   DATA     familyName                            INIT   ""
   DATA     height                                INIT   0
   DATA     nominalPointSize                      INIT   0

   DATA     width                                 INIT   0
   DATA     widthClass                            INIT   .F.

   DATA     bold                                  INIT   .F.
   DATA     weightClass                           INIT   FW_DONTCARE

   DATA     italic                                INIT   .F.
   DATA     strikeout                             INIT   .F.
   DATA     underscore                            INIT   .F.
   DATA     codePage                              INIT   DEFAULT_CHARSET

   DATA     fixed                                 INIT   .F.
   DATA     antiAliased                           INIT   .F.

   DATA     compoundName                          INIT   ""
   METHOD   setCompoundName( cName )              INLINE ::compoundName := cName

   DATA     generic                               INIT   .T.

   DATA     baseLine                              INIT   0                READONLY
   DATA     dbcs                                  INIT   .F.
   DATA     kerning                               INIT   .F.
   DATA     mbcs                                  INIT   .F.
   DATA     vector                                INIT   .F.
   DATA     outlined                              INIT   .F.

   DATA     aFontInfo                             INIT   {}

   METHOD   new( oPS )
   METHOD   create( cFontName )
   METHOD   configure( cFontName )
   METHOD   list()
   METHOD   createFont()

   DESTRUCTOR destroy()

   ENDCLASS

/*-*/

METHOD new( oPS ) CLASS WvgFont

   DEFAULT oPS TO ::oPS

   ::oPS := oPS

   RETURN Self

/*-*/

METHOD create( cFontName ) CLASS WvgFont

   DEFAULT cFontName TO ::familyName

   ::familyName := cFontName

   ::createFont()

   RETURN Self

/*-*/

METHOD configure( cFontName ) CLASS WvgFont

   DEFAULT cFontName TO ::familyName

   ::familyName := cFontName

   ::createFont()

   RETURN Self

/*-*/

METHOD destroy() CLASS WvgFont

   IF ::hFont != NIL
      WVG_DeleteObject( ::hFont )
   ENDIF

   RETURN Self

/*-*/

METHOD list() CLASS WvgFont
   LOCAL aList := {}

   RETURN aList

/*-*/

METHOD createFont() CLASS WvgFont
   LOCAL aFont

   IF ::hFont != NIL
      WVG_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::oPS != NIL
      ::height := Wvg_PointSizeToHeight( ::oPS:hdc, ::nominalPointSize )
   ENDIF

   ::aFontInfo := array( 15 )

   ::aFontInfo[  1 ] := ::familyName
   ::aFontInfo[  2 ] := ::height
   ::aFontInfo[  3 ] := ::width
   ::aFontInfo[  4 ] := iif( ::bold, FW_BOLD, 0 )
   ::aFontInfo[  5 ] := ::italic
   ::aFontInfo[  6 ] := ::underscore
   ::aFontInfo[  7 ] := ::strikeout
   ::aFontInfo[  8 ] := ::codePage
   ::aFontInfo[  9 ] := 0
   ::aFontInfo[ 10 ] := 0
   ::aFontInfo[ 11 ] := 0
   ::aFontInfo[ 12 ] := 0
   ::aFontInfo[ 13 ] := DEFAULT_QUALITY
   ::aFontInfo[ 14 ] := NIL

   aFont := Wvg_FontCreate( ::aFontInfo )

   IF empty( aFont[ 1 ] )
      RETURN nil
   ENDIF

   ::hFont     := aFont[ 15 ]
   ::aFontInfo := aFont

   RETURN ::hFont

/*-*/
