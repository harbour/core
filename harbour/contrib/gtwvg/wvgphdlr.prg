/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                               EkOnkar
//                         ( The LORD is ONE )
//
//                Xbase++ Compatible xbpPartHandler Class
//
//                  Pritpal Bedi <pritpal@vouchcac.com>
//                               08Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'hbgtinfo.ch'
#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//

CLASS WvgPartHandler

   DATA     cargo

   METHOD   init( oParent, oOwner )
   METHOD   create( oParent, oOwner )
   METHOD   configure( oParent, oOwner )
   METHOD   destroy()
   METHOD   handleEvent( hEvent, mp1, mp2 )
   METHOD   status()

   METHOD   addChild( oWvg )
   METHOD   childFromName( nNameId )
   METHOD   childList()
   METHOD   delChild( oWvg )
   METHOD   setName( nNameId )
   METHOD   setOwner( oWvg )
   METHOD   setParent( oWvg )

   METHOD   notifier( nEvent, xParams )

   DATA     hChildren                             INIT    hb_hash()
   DATA     nNameId
   DATA     oParent
   DATA     oOwner
   DATA     nStatus                               INIT    0

   ENDCLASS

//----------------------------------------------------------------------//

METHOD init( oParent, oOwner ) CLASS WvgPartHandler

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

//----------------------------------------------------------------------//

METHOD create( oParent, oOwner ) CLASS WvgPartHandler

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

//----------------------------------------------------------------------//

METHOD configure( oParent, oOwner ) CLASS WvgPartHandler

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

//----------------------------------------------------------------------//

METHOD destroy() CLASS WvgPartHandler

   ::hChildren  := NIL
   ::nNameId    := NIL
   ::oParent    := NIL
   ::oOwner     := NIL

   RETURN Self

//----------------------------------------------------------------------//

METHOD handleEvent( hEvent, mp1, mp2 ) CLASS WvgPartHandler

   HB_SYMBOL_UNUSED( hEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN Self

//----------------------------------------------------------------------//

METHOD status() CLASS WvgPartHandler

   RETURN ::nStatus

//----------------------------------------------------------------------//

METHOD addChild( oWvg ) CLASS WvgPartHandler

   HB_SYMBOL_UNUSED( oWvg )

   RETURN Self

//----------------------------------------------------------------------//

METHOD childFromName( nNameId ) CLASS WvgPartHandler
   LOCAL oWvg

   if ::hChildren[ nNameId ] <> NIL
      oWvg := ::hChildren[ nNameId ]           // ???
   endif

   RETURN oWvg

//----------------------------------------------------------------------//

METHOD childList() CLASS WvgPartHandler
   LOCAL aChildList := {}

   RETURN aChildList

//----------------------------------------------------------------------//

METHOD delChild( oWvg ) CLASS WvgPartHandler

   HB_SYMBOL_UNUSED( oWvg )

   RETURN Self

//----------------------------------------------------------------------//

METHOD setName( nNameId ) CLASS WvgPartHandler
   LOCAL nOldNameId := ::nNameId

   if Valtype( nNameId ) == 'N'
      ::nNameId := nNameId
   endif

   RETURN nOldNameId

//----------------------------------------------------------------------//

METHOD setOwner( oWvg ) CLASS WvgPartHandler
   LOCAL oOldXbp := ::oOwner

   if valtype( oWvg ) == 'O'
      ::oOwner := oWvg
   endif

   RETURN oOldXbp

//----------------------------------------------------------------------//

METHOD setParent( oWvg ) CLASS WvgPartHandler
   LOCAL oOldXbp := ::oParent

   if valtype( oWvg ) == 'O'
      ::oParent := oWvg
   endif

   RETURN oOldXbp

//----------------------------------------------------------------------//

METHOD notifier( nEvent, xParams ) CLASS WvgPartHandler
   Local aPos, nReturn := 0

   DO CASE

   CASE nEvent == HB_GTE_MOUSE
      if     xParams[ 1 ] == WM_MOUSEHOVER
         aPos := { xParams[ 3 ], xParams[ 4 ] }
      elseif xParams[ 1 ] == WM_MOUSELEAVE
         // Nothing
      else
         aPos := if( ::mouseMode == 2, { xParams[ 3 ], xParams[ 4 ] }, { xParams[ 5 ], xParams[ 6 ] } )
      endif

      SWITCH xParams[ 1 ]

      case WM_MOUSEHOVER
         if hb_isBlock( ::sl_enter )
            eval( ::sl_enter, aPos, NIL, self )
         endif
         EXIT
      case WM_MOUSELEAVE
         if hb_isBlock( ::sl_leave )
            eval( ::sl_leave, aPos, NIL, self )
         endif
         EXIT
      case WM_RBUTTONDOWN
         if hb_isBlock( ::sl_rbDown )
            eval( ::sl_rbDown, aPos, NIL, self )
         endif
         EXIT
      case WM_LBUTTONDOWN
         if hb_isBlock( ::sl_lbDown )
            eval( ::sl_lbDown, aPos, NIL, self )
         endif
         EXIT
      case WM_RBUTTONUP      ////
         if hb_isBlock( ::sl_rbUp )
            eval( ::sl_rbUp, aPos, NIL, self )
         endif
         EXIT
      case WM_LBUTTONUP      ////
         if hb_isBlock( ::sl_lbUp )
            eval( ::sl_lbUp, aPos, NIL, self )
         endif
         EXIT
      case WM_RBUTTONDBLCLK
         if hb_isBlock( ::sl_rbDblClick )
            eval( ::sl_rbDblClick, aPos, NIL, self )
         endif
         EXIT
      case WM_LBUTTONDBLCLK
         if hb_isBlock( ::sl_lbDblClick )
            eval( ::sl_lbDblClick, aPos, NIL, self )
         endif
         EXIT
      case WM_MBUTTONDOWN
         if hb_isBlock( ::sl_mbDown )
            eval( ::sl_mbDown, aPos, NIL, self )
         endif
         EXIT
      case WM_MBUTTONUP       ////
         if hb_isBlock( ::sl_mbClick )
            eval( ::sl_mbClick, aPos, NIL, self )
         endif
         EXIT
      case WM_MBUTTONDBLCLK
         if hb_isBlock( ::sl_mbDblClick )
            eval( ::sl_mbDblClick, aPos, NIL, self )
         endif
         EXIT
      case WM_MOUSEMOVE
         if hb_isBlock( ::sl_motion )
            eval( ::sl_motion, aPos, NIL, self )
         endif
         EXIT
      case WM_MOUSEWHEEL
         if hb_isBlock( ::sl_wheel )
            eval( ::sl_wheel, aPos, NIL, self )
         endif
         EXIT
      case WM_NCMOUSEMOVE
         EXIT
      END

   CASE nEvent == HB_GTE_KEYBOARD
      if hb_isBlock( ::keyboard )
         eval( ::keyboard, xParams, NIL, Self )
      endif

   CASE nEvent == HB_GTE_SETFOCUS
      if hb_isBlock( ::setInputFocus )
         eval( ::setInputFocus, NIL, NIL, Self )
      endif
      ::lHasInputFocus := .t.

   CASE nEvent == HB_GTE_KILLFOCUS
      if hb_isBlock( ::killInputFocus )
         eval( ::killInputFocus, NIL, NIL, Self )
      endif
      ::lHasInputFocus := .f.

   CASE nEvent == HB_GTE_RESIZED
      if hb_isBlock( ::sl_resize )
         eval( ::sl_resize, { xParams[ 1 ], xParams[ 2 ] }, { xParams[ 3 ], xParams[ 4 ] }, Self )
      endif

   CASE nEvent == HB_GTE_CLOSE
      if hb_isBlock( ::close )
         nReturn := eval( ::close, NIL, NIL, Self )
      endif

   ENDCASE

   RETURN nReturn

//----------------------------------------------------------------------//
