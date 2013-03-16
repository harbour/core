/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpPartHandler Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               08Nov2008
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgSysWindow INHERIT WvgPartHandler

   METHOD new( oParent, oOwner, aPos )
   METHOD create( oParent, oOwner, aPos )
   METHOD configure()
   METHOD destroy()

   METHOD disable()
   METHOD enable()
   METHOD hide()
   METHOD show()
   METHOD SetPos( aPos )

   METHOD currentPos()
   METHOD currentSize()

   VAR    aPos                                  INIT    { 0, 0 }

   VAR    hWnd                                  PROTECTED
   VAR    nOldProc                              PROTECTED
   VAR    nWndProc                              PROTECTED


   VAR    sl_helpRequest
   ACCESS helpRequest                           INLINE  ::sl_helpRequest
   ASSIGN helpRequest( bBlock )                 INLINE  ::sl_helpRequest := bBlock

   VAR    sl_move
   ACCESS move                                  INLINE  ::sl_move
   ASSIGN move( bBlock )                        INLINE  ::sl_move := bBlock

   VAR    sl_quit
   ACCESS quit                                  INLINE  ::sl_quit
   ASSIGN quit( bBlock )                        INLINE  ::sl_quit := bBlock

ENDCLASS

METHOD WvgSysWindow:new( oParent, oOwner, aPos )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )
   __defaultNIL( @aPos, ::aPos )

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:new( oParent, oOwner )

   RETURN Self

METHOD WvgSysWindow:create( oParent, oOwner, aPos )

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )
   __defaultNIL( @aPos, ::aPos )

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:create( oParent, oOwner )

   RETURN Self

METHOD WvgSysWindow:configure()

   RETURN Self

METHOD WvgSysWindow:destroy()

   RETURN Self

METHOD WvgSysWindow:disable()

   RETURN Self

METHOD WvgSysWindow:enable()

   RETURN Self

METHOD WvgSysWindow:hide()

   RETURN Self

METHOD WvgSysWindow:show()

   RETURN Self

METHOD WvgSysWindow:SetPos( aPos )

   Wvg_SetWindowPosition( ::hWnd, aPos[ 1 ], aPos[ 2 ], .F. )

   RETURN Self

METHOD WvgSysWindow:currentPos()

   LOCAL aRect

   aRect := Wvg_GetWindowRect( ::hWnd )

   RETURN { aRect[ 1 ], aRect[ 2 ] }

METHOD WvgSysWindow:currentSize()

   LOCAL aRect

   aRect := Wvg_GetClientRect( ::hWnd )

   RETURN { aRect[ 3 ] - aRect[ 1 ], aRect[ 4 ] - aRect[ 2 ] }

/*
 *                       Class WvgFontDialog()
 */
CREATE CLASS WvgFontDialog INHERIT WvgSysWindow

   /* Appearance */
   VAR    title                                 INIT   ""
   VAR    buttonApply                           INIT   .F.
   VAR    buttonCancel                          INIT   .T.
   VAR    buttonHelp                            INIT   .F.
   VAR    buttonOk                              INIT   .T.
   VAR    buttonReset                           INIT   .F.
   VAR    strikeOut                             INIT   .T.
   VAR    underscore                            INIT   .T.

   VAR    name                                  INIT   .T.
   VAR    style                                 INIT   .T.
   VAR    size                                  INIT   .T.

   VAR    displayFilter                         INIT   .T.
   VAR    printerFilter                         INIT   .T.

   VAR    familyName                            INIT   " "
   VAR    nominalPointSize                      INIT   0

   VAR    bitmapOnly                            INIT   .F.
   VAR    fixedOnly                             INIT   .F.
   VAR    proportionalOnly                      INIT   .T.


   VAR    outLine                               INIT   .T.
   VAR    previewBGClr                          INIT   RGB( 255, 255, 255 )
   VAR    previewFGClr                          INIT   RGB( 0, 0, 0 )
   VAR    previewString                         INIT   " "
   VAR    printerPS                             INIT   NIL
   VAR    screenPS                              INIT   NIL

   VAR    synthesizeFonts                       INIT   .T.

   VAR    vectorOnly                            INIT   .F.
   VAR    vectorSizes                           INIT   {}

   VAR    viewPrinterFonts                      INIT   .F.
   VAR    viewScreenFonts                       INIT   .T.

   METHOD new( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD create( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD destroy()
   METHOD display( nMode )

   VAR    sl_activateApply
   ACCESS activateApply                         INLINE ::sl_activateApply
   ASSIGN activateApply( bBlock )               INLINE ::sl_activateApply := bBlock

   VAR    sl_activateCancel
   ACCESS activateCancel                        INLINE ::sl_activateCancel
   ASSIGN activateCancel( bBlock )              INLINE ::sl_activateCancel := bBlock

   VAR    sl_activateOk
   ACCESS activateOk                            INLINE ::sl_activateOk
   ASSIGN activateOk( bBlock )                  INLINE ::sl_activateOk := bBlock

   VAR    sl_activateReset
   ACCESS activateReset                         INLINE ::sl_activateReset
   ASSIGN activateReset( bBlock )               INLINE ::sl_activateReset := bBlock

   VAR    oScreenPS
   VAR    oPrinterPS
   VAR    aPos                                  INIT   { 0, 0 }
   VAR    ok                                    INIT   .F.

   METHOD wndProc( hWnd, nMessage, nwParam, nlParam )
   METHOD GetWvgFont( aFont )                   PROTECTED

ENDCLASS

METHOD new( oParent, oOwner, oScreenPS, oPrinterPS, aPos ) CLASS WvgFontDialog

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )
   __defaultNIL( @oScreenPS, ::oScreenPS )
   __defaultNIL( @oPrinterPS, ::oPrinterPS )
   __defaultNIL( @aPos, ::aPos )

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   ::WvgSysWindow:new( oParent, oOwner )

   RETURN Self

METHOD create( oParent, oOwner, oScreenPS, oPrinterPS, aPos ) CLASS WvgFontDialog

   __defaultNIL( @oParent, ::oParent )
   __defaultNIL( @oOwner, ::oOwner )
   __defaultNIL( @oScreenPS, ::oScreenPS )
   __defaultNIL( @oPrinterPS, ::oPrinterPS )
   __defaultNIL( @aPos, ::aPos )

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   IF ::viewPrinterFonts .AND. ::oPrinterPS == NIL
      ::viewPrinterFonts := .F.
   ENDIF
   IF ! ::viewScreenFonts .AND. ! ::viewPrinterFonts
      ::viewScreenFonts := .T.
   ENDIF

   ::WvgSysWindow:create( oParent, oOwner )

   /* ::nWndProc := hb_AsCallBack( "WNDPROC", Self ) */

   RETURN Self

METHOD wndProc( hWnd, nMessage, nwParam, nlParam ) CLASS WvgFontDialog

   LOCAL aRect, nL, nH

   HB_SYMBOL_UNUSED( nlParam )

   DO CASE

   CASE nMessage == WM_INITDIALOG
      ::hWnd := hWnd

      IF ! Empty( ::title )
         Wvg_SetWindowText( ::hWnd, ::title )
      ENDIF
      IF ! ::buttonCancel
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, IDCANCEL ), .F. )
      ENDIF
      IF ! ::buttonApply
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1026 ), .F. )
      ENDIF
      IF ! ::buttonHelp
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1038 ), .F. )
      ENDIF
      IF ! ::strikeOut
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1040 ), .F. )
      ENDIF
      IF ! ::underscore
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1041 ), .F. )
      ENDIF
      IF ! ::name
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1136 ), .F. )
      ENDIF
      IF ! ::style
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1137 ), .F. )
      ENDIF
      IF ! ::size
         Wvg_EnableWindow( Wvg_GetDlgItem( ::hWnd, 1138 ), .F. )
      ENDIF

      IF ::aPos[ 1 ] > 0 .OR. ::aPos[ 2 ] > 0
         aRect := Wvg_GetWindowRect( ::hWnd )
         Wvg_MoveWindow( ::hWnd, ::aPos[ 1 ], ::aPos[ 2 ], aRect[ 3 ] - aRect[ 1 ], aRect[ 4 ] - aRect[ 2 ], .F. )
      ENDIF

      RETURN 1

   CASE nMessage == WM_COMMAND
      nL := Wvg_LOWORD( nwParam )
      nH := Wvg_HIWORD( nwParam )

      HB_SYMBOL_UNUSED( nH )

      DO CASE

      CASE nL == IDOK
         ::ok := .T.
         IF HB_ISBLOCK( ::sl_activateOk )
            Eval( ::sl_activateOk, ::GetWvgFont(), NIL, Self )
         ENDIF

      CASE nL == IDCANCEL
         IF HB_ISBLOCK( ::sl_activateCancel )
            Eval( ::sl_activateCancel, NIL, NIL, Self )
         ENDIF

      CASE nL == 1026
         IF HB_ISBLOCK( ::sl_activateApply )
            Eval( ::sl_activateApply, ::GetWvgFont(), NIL, Self )
         ENDIF

      CASE nL == 1038  /* Help */

      ENDCASE

   ENDCASE

   RETURN 0

METHOD display( nMode ) CLASS WvgFontDialog

   LOCAL hWnd, aInfo

   IF nMode == 0
      hWnd := ::oParent:hWnd
   ELSE
      hWnd := Wvg_GetDesktopWindow()
   ENDIF

   ::ok := .F.
   aInfo := Wvg_ChooseFont( hWnd, {| h, m, w, l | ::wndProc( h, m, w, l ) }, ::familyName, ;
      ::nominalPointSize, ::viewScreenFonts, ::viewPrinterFonts )
   IF ! ::ok
      RETURN NIL
   ENDIF

   RETURN ::GetWvgFont( aInfo )

METHOD destroy() CLASS WvgFontDialog

   /* hb_FreeCallBack( ::nWndProc ) */

   RETURN Self

/*
 * Only callable from ::activateOK and ::activateApply
 */
METHOD GetWvgFont( aFont ) CLASS WvgFontDialog

   LOCAL oWvgFont

   IF ! HB_ISARRAY( aFont )
      aFont := Wvg_ChooseFont_GetLogFont( ::hWnd )
   ENDIF

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
   oWvgFont:setCompoundName( RTrim( aFont[ 1 ] + " " + iif( oWvgFont:bold, "Bold ", "" ) + ;
      iif( oWvgFont:italic, "Italic", "" ) ) )
   oWvgFont:create()

   RETURN oWvgFont

/*
                            Class WvgFont()
*/
CREATE CLASS WvgFont

   VAR    hFont
   VAR    oPS
   VAR    hdc

   VAR    familyName                            INIT   ""
   VAR    height                                INIT   0
   VAR    nominalPointSize                      INIT   0

   VAR    width                                 INIT   0
   VAR    widthClass                            INIT   .F.

   VAR    bold                                  INIT   .F.
   VAR    weightClass                           INIT   FW_DONTCARE

   VAR    italic                                INIT   .F.
   VAR    strikeout                             INIT   .F.
   VAR    underscore                            INIT   .F.
   VAR    codePage                              INIT   DEFAULT_CHARSET

   VAR    fixed                                 INIT   .F.
   VAR    antiAliased                           INIT   .F.

   VAR    compoundName                          INIT   ""
   METHOD setCompoundName( cName )              INLINE ::compoundName := cName

   VAR    generic                               INIT   .T.

   VAR    baseLine                              INIT   0                READONLY
   VAR    dbcs                                  INIT   .F.
   VAR    kerning                               INIT   .F.
   VAR    mbcs                                  INIT   .F.
   VAR    vector                                INIT   .F.
   VAR    outlined                              INIT   .F.

   VAR    aFontInfo                             INIT   {}

   METHOD new( oPS )
   METHOD create( cFontName )
   METHOD configure( cFontName )
   METHOD list()
   METHOD createFont()

   DESTRUCTOR destroy()

ENDCLASS

METHOD new( oPS ) CLASS WvgFont

   __defaultNIL( @oPS, ::oPS )

   ::oPS := oPS

   RETURN Self

METHOD create( cFontName ) CLASS WvgFont

   __defaultNIL( @cFontName, ::familyName )

   ::familyName := cFontName

   ::createFont()

   RETURN Self

METHOD configure( cFontName ) CLASS WvgFont

   __defaultNIL( @cFontName, ::familyName )

   ::familyName := cFontName

   ::createFont()

   RETURN Self

METHOD destroy() CLASS WvgFont

   IF ::hFont != NIL
      Wvg_DeleteObject( ::hFont )
   ENDIF

   RETURN Self

METHOD list() CLASS WvgFont

   LOCAL aList := {}

   RETURN aList

METHOD createFont() CLASS WvgFont

   LOCAL aFont

   IF ::hFont != NIL
      Wvg_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::oPS != NIL
      ::height := Wvg_PointSizeToHeight( ::oPS:hdc, ::nominalPointSize )
   ENDIF

   ::aFontInfo := Array( 15 )

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

   IF Empty( aFont[ 1 ] )
      RETURN NIL
   ENDIF

   ::hFont     := aFont[ 15 ]
   ::aFontInfo := aFont

   RETURN ::hFont
