/*
 * Harbour Project source code:
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    :Move()
 *
 * See COPYING.txt for licensing terms.
 *
 */

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

#include "hbmemvar.ch"

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbWindow // Debugger windows and dialogs

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR cCaption
   VAR cBackImage
   VAR cColor
   VAR lFocused     INIT .F.
   VAR bGotFocus
   VAR bLostFocus
   VAR bKeyPressed
   VAR bPainted
   VAR bLButtonDown
   VAR bLDblClick
   VAR lShadow      INIT .F.
   VAR lVisible     INIT .F.
   VAR Cargo
   VAR Browser

   METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor )

   METHOD Hide()
   METHOD IsOver( nRow, nCol )
   METHOD nWidth() INLINE ::nRight - ::nLeft + 1
   METHOD Clear()
   METHOD ScrollUp( nLines )
   METHOD SetCaption( cCaption )
   METHOD ShowCaption()
   METHOD SetFocus( lOnOff )
   METHOD Show( lFocused )
   METHOD ShowModal()
   METHOD LButtonDown( nMRow, nMCol )
   METHOD LDblClick( nMRow, nMCol )
   METHOD LoadColors()

   METHOD Move()
   METHOD KeyPressed( nKey )
   METHOD Refresh()
   METHOD Resize( nTop, nLeft, nBottom, nRight )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor ) CLASS HBDbWindow

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight
   ::cCaption := cCaption
   ::cColor   := hb_defaultValue( cColor, __dbgColors()[ 1 ] )

   RETURN Self

METHOD Clear() CLASS HBDbWindow

   SetColor( ::cColor )
   hb_Scroll( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1 )

   RETURN NIL

METHOD Hide() CLASS HBDbWindow

   RestScreen( ::nTop, ::nLeft, ::nBottom + iif( ::lShadow, 1, 0 ), ;
      ::nRight + iif( ::lShadow, 2, 0 ), ::cBackImage )
   ::cBackImage := NIL
   ::lVisible := .F.

   RETURN NIL

METHOD IsOver( nRow, nCol ) CLASS HBDbWindow

   RETURN nRow >= ::nTop .AND. nRow <= ::nBottom .AND. ;
      nCol >= ::nLeft .AND. nCol <= ::nRight

METHOD ScrollUp( nLines ) CLASS HBDbWindow

   SetColor( ::cColor )
   hb_Scroll( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1, hb_defaultValue( nLines, 1 ) )

   RETURN NIL

METHOD SetCaption( cCaption ) CLASS HBDbWindow

   ::cCaption := cCaption

   RETURN NIL

METHOD ShowCaption() CLASS HBDbWindow

   IF ! Empty( ::cCaption )
      hb_DispOutAt( ::nTop, ::nLeft + ( ( ::nRight - ::nLeft ) / 2 ) - ;
         ( ( Len( ::cCaption ) + 2 ) / 2 ), ;
         " " + ::cCaption + " ", ::cColor )
   ENDIF

   RETURN NIL

METHOD SetFocus( lOnOff ) CLASS HBDbWindow

   IF ! lOnOff .AND. HB_ISEVALITEM( ::bLostFocus )
      Eval( ::bLostFocus, Self )
   ENDIF

   ::lFocused := lOnOff

   IF lOnOff .AND. HB_ISEVALITEM( ::bGotFocus )
      Eval( ::bGotFocus, Self )
   ENDIF

   RETURN NIL

METHOD Refresh() CLASS HBDbWindow

   DispBegin()

   hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, iif( ::lFocused, HB_B_DOUBLE_UNI, HB_B_SINGLE_UNI ), ::cColor )
   hb_DispOutAtBox( ::nTop, ::nLeft + 1, hb_UTF8ToStrBox( "[■]" ), ::cColor )

   ::ShowCaption( ::cCaption )

   IF HB_ISEVALITEM( ::bPainted )
      Eval( ::bPainted, Self )
   ENDIF

   DispEnd()

   RETURN NIL

METHOD Show( lFocused ) CLASS HBDbWindow

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   ::cBackImage := SaveScreen( ::nTop, ::nLeft, ::nBottom + iif( ::lShadow, 1, 0 ), ;
      ::nRight + iif( ::lShadow, 2, 0 ) )
   SetColor( ::cColor )
   hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ::SetFocus( hb_defaultValue( lFocused, ::lFocused ) )

   IF ::lShadow
      hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ENDIF

   ::Refresh()
   ::lVisible := .T.

   SetPos( nRow, nCol )

   RETURN NIL

METHOD ShowModal() CLASS HBDbWindow

   LOCAL lExit := .F.
   LOCAL nKey

   ::lShadow := .T.
   ::Show()

   DO WHILE ! lExit
      nKey := Inkey( 0, INKEY_ALL )

      IF HB_ISEVALITEM( ::bKeyPressed )
         Eval( ::bKeyPressed, nKey )
      ENDIF

      DO CASE
      CASE nKey == K_ESC
         lExit := .T.

      CASE nKey == K_LBUTTONDOWN
         IF MRow() == ::nTop .AND. MCol() >= ::nLeft + 1 .AND. ;
            MCol() <= ::nLeft + 3
            lExit := .T.
         ENDIF
      ENDCASE
   ENDDO

   ::Hide()

   RETURN NIL

METHOD LButtonDown( nMRow, nMCol ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bLButtonDown )
      Eval( ::bLButtonDown, nMRow, nMCol )
   ENDIF

   RETURN NIL

METHOD LDblClick( nMRow, nMCol ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bLDblClick )
      Eval( ::bLDblClick, nMRow, nMCol )
   ENDIF

   RETURN NIL

METHOD Move() CLASS HBDbWindow

   LOCAL nOldTop    := ::nTop
   LOCAL nOldLeft   := ::nLeft
   LOCAL nOldBottom := ::nbottom
   LOCAL nOldRight  := ::nright
   LOCAL nKey

   DO WHILE .T.
      RestScreen( ,,,, ::cBackImage )
      hb_DispBox( ::nTop, ::nLeft, ::nRight, ::nBottom, Replicate( hb_UTF8ToStrBox( "░" ), 8 ) + " " )

      nKey := Inkey( 0, INKEY_ALL )

      DO CASE
      CASE nKey == K_UP

         IF ::nTop != 0
            ::nTop--
            ::nBottom--
         ENDIF

      CASE nKey == K_DOWN

         IF ::nBottom != MaxRow()
            ::nTop++
            ::nBottom++
         ENDIF

      CASE nKey == K_LEFT

         IF ::nLeft != 0
            ::nLeft--
            ::nRight--
         ENDIF

      CASE nKey == K_RIGHT

         IF ::nBottom != MaxRow()
            ::nLeft++
            ::nRight++
         ENDIF

      CASE nKey == K_ESC

         ::nTop    := nOldTop
         ::nLeft   := nOldLeft
         ::nBottom := nOldBottom
         ::nRight  := nOldRight

      ENDCASE

      IF nKey == K_ESC .OR. nKey == K_ENTER
         EXIT
      ENDIF
   ENDDO

#if 0
   hb_keyPut( 0 )
   Inkey()
#endif

   RETURN NIL

METHOD KeyPressed( nKey ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bKeyPressed )
      Eval( ::bKeyPressed, nKey, Self )
   ENDIF

   RETURN NIL

METHOD LoadColors() CLASS HBDbWindow

   LOCAL aClr := __dbgColors()

   ::cColor := aClr[ 1 ]

   IF ::Browser != NIL
      ::Browser:ColorSpec := aClr[ 2 ] + "," + aClr[ 5 ] + "," + aClr[ 3 ] + "," + aClr[ 6 ]
   ENDIF

   RETURN NIL

METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBDbWindow

   LOCAL lShow

   IF ( ! HB_ISNUMERIC( nTop ) .OR. nTop == ::nTop ) .AND. ;
      ( ! HB_ISNUMERIC( nLeft ) .OR. nLeft == ::nLeft ) .AND. ;
      ( ! HB_ISNUMERIC( nBottom ) .OR. nBottom == ::nBottom ) .AND. ;
      ( ! HB_ISNUMERIC( nRight ) .OR. nRight == ::nRight )
      RETURN Self
   ENDIF

   IF ( lShow := ::lVisible )
      ::Hide()
   ENDIF

   IF HB_ISNUMERIC( nTop )
      ::nTop := nTop
   ENDIF
   IF HB_ISNUMERIC( nBottom )
      ::nBottom := nBottom
   ENDIF
   IF HB_ISNUMERIC( nLeft )
      ::nLeft := nLeft
   ENDIF
   IF HB_ISNUMERIC( nRight )
      ::nRight := nRight
   ENDIF

   IF ::Browser != NIL
      ::Browser:Resize( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1 )
   ENDIF

   IF lShow
      ::Show( ::lFocused )
   ENDIF

   RETURN self
