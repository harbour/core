/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Win32
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Alexander Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
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

#include "common.ch"
#include "hbclass.ch"

#define WM_CLOSE        0x0010
#define WM_COMMAND      0x0111
#define WM_DESTROY      0x0002
#define WM_LBUTTONDOWN  0x0201
#define WS_VISIBLE      0x10000000

static aForms := {}

CLASS TForm FROM TPersistent

   DATA      hWnd
   DATA      oMainMenu

   DATA      OnClick  PROPERTY

   CLASSDATA lRegistered

   METHOD    New()
   METHOD    Close() INLINE SendMessage( ::hWnd, WM_CLOSE )
   METHOD    Command( nNotifyCode, nId, hWndCtl )
   METHOD    HandleEvent( nMsg, nParam1, nParam2 )
   METHOD    LButtonDown( nKeyFlags, nXPos, nYPos )
   METHOD    ShowModal()

   ACCESS    Caption() INLINE WinGetText( ::hWnd ) PROPERTY
   ASSIGN    Caption( cNewCaption ) INLINE ;
                WinSetWindowText( ::hWnd, cNewCaption )

   ACCESS    Top()    INLINE WinGetTop( ::hWnd )    PROPERTY
   ASSIGN    Top( nNewTop ) INLINE WinSetTop( ::hWnd, nNewTop )

   ACCESS    Left()   INLINE WinGetLeft( ::hWnd )   PROPERTY
   ASSIGN    Left( nNewLeft ) INLINE WinSetLeft( ::hWnd, nNewLeft )

   ACCESS    Height() INLINE WinGetHeight( ::hWnd ) PROPERTY
   ASSIGN    Height( nNewHeight ) INLINE WinSetHeight( ::hWnd, nNewHeight )

   ACCESS    Width()  INLINE WinGetWidth( ::hWnd )  PROPERTY
   ASSIGN    Width( nNewWidth ) INLINE WinSetWidth( ::hWnd, nNewWidth )

   ACCESS    Menu() INLINE ::oMainMenu PROPERTY
   ASSIGN    Menu( oNewMenu )

ENDCLASS


METHOD New() CLASS TForm

   DEFAULT ::lRegistered TO .f.

   Super:New()

   if ! ::lRegistered
      WinRegisterClass( "HB_TFORM" )
      ::lRegistered = .t.
   endif

   ::hWnd  = WinCreateStdWindow( , WS_VISIBLE,, "HB_TFORM", "Harbour TForm" )

   AAdd( aForms, Self )

return Self

METHOD Command( nNotifyCode, nId, hWndCtl ) CLASS TForm

   local oMenuItem

   if nNotifyCode == 0  // Menu command
      if ::Menu != nil
         if( oMenuItem := ::Menu:FindItem( nId ) ) != nil
            if oMenuItem:OnClick != nil
               __ObjSendMsg( Self, oMenuItem:OnClick, oMenuItem )
            endif
         endif
      endif
   endif

return nil

METHOD LButtonDown( nKeyFlags, nXPos, nYPos ) CLASS TForm

   if ::OnClick != nil
      return __ObjSendMsg( Self, ::OnClick, Self, nXPos, nYPos )
   endif

return nil

METHOD HandleEvent( nMsg, nParam1, nParam2 ) CLASS TForm

   do case
      case nMsg == WM_COMMAND
           return ::Command( nHiWord( nParam1 ), nLoWord( nParam1 ), nParam2 )

      case nMsg == WM_LBUTTONDOWN
           return ::LButtonDown( nParam1, nLoWord( nParam2 ), nHiWord( nParam2 ) )

      case nMsg == WM_DESTROY
           PostQuitMessage( 0 )
           return 0
   endcase

return nil

METHOD ShowModal() CLASS TForm

   HB_FormShowModal( ::hWnd )

return nil


ASSIGN Menu( oNewMenu ) CLASS TForm

   ::oMainMenu = oNewMenu

   SetMenu( ::hWnd, oNewMenu:nHandle )

return nil

function HB_GUI( hWnd, nMsg, nParam1, nParam2 ) // messages entry point

   local nForm := AScan( aForms, { | oForm | oForm:hWnd == hWnd } )

   static aReturn := { nil, nil }

   if nForm != 0
      aReturn[ 1 ] = aForms[ nForm ]:HandleEvent( nMsg, nParam1, nParam2 )
   endif

return aReturn