/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Win32
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

CLASS TForm

   DATA      hWnd
   DATA     oMainMenu

   CLASSDATA lRegistered

   METHOD   New()
   METHOD   ShowModal()
   METHOD   cCaption() INLINE WinGetText( ::hWnd )

   METHOD   _cCaption( cNewCaption ) INLINE ;
               WinSetWindowText( ::hWnd, cNewCaption )

   METHOD   oMenu() INLINE ::oMainMenu
   METHOD   _oMenu( oNewMenu )

ENDCLASS


METHOD New() CLASS TForm

   local hWndClient, res

   DEFAULT ::lRegistered TO .f.

   if ! ::lRegistered

      // Notice that this code may be moved to a method Register()
      // so we hide again the OS API details

      res := WinRegisterClass( "HB_TFORM",;
                        (CS_SIZEREDRAW + 0x2000001), 0 )
      ::lRegistered = .t.
      Writelog( "Register: "+Iif( res,"Ok","No" ) )
   endif

   // Again this code may be moved to a method Create() to hide the
   // OS API details

   ::hWnd := WinCreateStdWindow( HWND_DESKTOP,;
                                WS_VISIBLE,;
                                (FCF_TITLEBAR + FCF_SYSMENU +;
                                FCF_SIZEBORDER + FCF_TASKLIST +;
                                FCF_MINMAX + FCF_SHELLPOSITION ),;
                                "HB_TFORM", "Harbour TForm",;
                                (WS_SYNCPAINT + WS_VISIBLE ),,,;
                                @hWndClient ) // Not used yet

   Writelog( "Create: "+Str( ::hWnd ) )

return Self


METHOD ShowModal() CLASS TForm

   HB_PM_ShowModal( ::hWnd )

return nil


METHOD _oMenu( oNewMenu ) CLASS TForm

   ::oMainMenu = oNewMenu

   res := SetMenu( ::hWnd, oNewMenu:nHandle )

return nil
