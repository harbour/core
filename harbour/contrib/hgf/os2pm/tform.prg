/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for IBM OS/2 Presentation Manager
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Maurilio Longo <maurilio.longo@libero.it>
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
#include "os2pm.ch"   // Needed to store some OS/2 PM constant values


// Win32 compatibility Message redefinition
#define WM_LBUTTONDOWN  WM_BUTTON1DOWN


static aForms := {}


CLASS HBForm FROM HBWinControl

   DATA     oMainMenu
   DATA     OnClick     PROPERTY
   DATA     aControls   PROPERTY

   CLASSDATA lRegistered

   METHOD   New()
   METHOD   Close() INLINE SendMessage( ::hWnd, WM_CLOSE )
   METHOD   Command( nNotifyCode, nId, hWndCtl )
   METHOD   HandleEvent( nMsg, nParam1, nParam2 )
   METHOD   InsertControl( oControl )
   METHOD   LButtonDown( nKeyFlags, nXPos, nYPos )
   METHOD   ShowModal()

   ACCESS   Menu() INLINE ::oMainMenu PROPERTY
   ASSIGN   Menu( oNewMenu )

ENDCLASS


METHOD New() CLASS HBForm

   local hWC

   DEFAULT ::lRegistered TO .f.


   if ! ::lRegistered
      // Notice that this code may be moved to a method Register()
      // so we hide again the OS API details
      WinRegisterClass("HB_HBForm", CS_SIZEREDRAW, 0, 0 )
      ::lRegistered = .t.
   endif

   // Again this code may be moved to a method Create() to hide the
   // OS API details
   ::hWnd = WinCreateStdWindow( HWND_DESKTOP,;
                                WS_VISIBLE,;
                                (FCF_TITLEBAR + FCF_SYSMENU +;
                                FCF_SIZEBORDER + FCF_TASKLIST +;
                                FCF_MINMAX + FCF_SHELLPOSITION ),;
                                "HB_HBForm", "Harbour HBForm",;
                                (WS_SYNCPAINT + WS_VISIBLE ),,,;
                                @hWC )

   ::hWndClient := hWC

   AAdd(aForms, Self)
return Self


METHOD Command( nNotifyCode, nId, hWndCtl ) CLASS HBForm

   local oMenuItem, nAt, oControl

   do case
      case nNotifyCode == CMDSRC_MENU   // Menu command
         if ::Menu != nil
            if( oMenuItem := ::Menu:FindItem( nId ) ) != nil
               if oMenuItem:OnClick != nil
                  __ObjSendMsg( Self, oMenuItem:OnClick, oMenuItem )
               endif
            endif
         endif

      case nNotifyCode == CMDSRC_PUSHBUTTON
         nAt = AScan( ::aControls, { | o | o:nId == nId } )
         if nAt != 0
            oControl = ::aControls[ nAt ]
            if oControl:OnClick != nil
               __ObjSendMsg( Self, oControl:OnClick, oControl )
            endif
         endif

      otherwise
   endcase

return nil


METHOD InsertControl( oControl ) CLASS HBForm

   DEFAULT ::aControls TO {}

   AAdd( ::aControls, oControl )
   oControl:Show()

return nil


METHOD LButtonDown( nKeyFlags, nXPos, nYPos ) CLASS HBForm

   if ::OnClick != nil
      return __ObjSendMsg( Self, ::OnClick, Self, nXPos, nYPos )
   endif

return nil


METHOD HandleEvent( nMsg, nParam1, nParam2 ) CLASS HBForm

   do case
      case nMsg == WM_COMMAND
         /*
         param1
            USHORT  uscmd       //  Command value.
         param2
            USHORT  ussource    //  Source type.
            USHORT  uspointer   //  Pointer-device indicator.
         returns
            ULONG   ulReserved  //  Reserved value, should be 0.
         */
         ::Command( nLoWord( nParam2 ), nLoWord( nParam1 ), nil )
         return 0

      case nMsg == WM_LBUTTONDOWN
         /*
         param1
            POINTS  ptspointerpos  //  Pointer position.
         param2
            USHORT  fsHitTestres   //  Hit-test result.
            USHORT  fsflags        //  Keyboard control codes.
         returns
            BOOL    rc             //  Processed indicator.
         */
         return ::LButtonDown( nHiWord(nParam2), nLoWord( nParam1 ), nHiWord( nParam1 ) )

      case nMsg == WM_DESTROY
         PostQuitMessage( 0 )
         return 0
   endcase

return nil

METHOD ShowModal() CLASS HBForm

   HB_FormShowModal(::hWnd)

return nil


ASSIGN Menu( oNewMenu ) CLASS HBForm

   ::oMainMenu := oNewMenu

   WinSetParent( oNewMenu:nHandle, ::hWnd, .t. )
   WinSetOwner( oNewMenu:nHandle, ::hWnd )
   WinSendMsg( ::hWnd, WM_UPDATEFRAME, FCF_MENU, 0 )

return nil


function HB_GUI( hWnd, nMsg, nParam1, nParam2 ) // messages entry point

   static aReturn := { nil, nil }

   local nForm := AScan( aForms, { | oForm | oForm:hWndClient == hWnd } )

   if nForm != 0
      aReturn[ 1 ] = aForms[ nForm ]:HandleEvent( nMsg, nParam1, nParam2 )
   endif

return aReturn
