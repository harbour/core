/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Andy Wos
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *              Xbase++ Compatible xbpActiveXControl Class
 *
 *                 Pritpal Bedi  <pritpal@vouchcac.com>
 *                              08Nov2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

CLASS WvgActiveXControl FROM WvgWindow

   DATA   oOLE
   DATA   CLSID                              INIT ""
   DATA   server                             INIT NIL
   DATA   license                            INIT NIL
   DATA   controlFlags                       INIT 0
   DATA   visible                            INIT .T.
   DATA   default                            INIT .F.
   DATA   cancel                             INIT .F.

   DATA   interface
   DATA   interfaceName

   DATA   lSubStdEvents                      INIT .f.

   DATA   hEvents                            INIT hb_hash()
   DATA   hContainer

   DATA   ClassName

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible, cCLSID, cLicense )
   METHOD Destroy()
   METHOD execEvent( nEvent, ... )
   METHOD handleEvent( nEvent, aInfo )
   METHOD mapEvent( nEvent, bBlock )

   METHOD inheritPresParams()
   METHOD presParamsChanged()
   METHOD setInputFocus()
   METHOD subscribeStdEvents()
   METHOD unsubscribeStdEvents()
   METHOD keyDown()
   METHOD click()
   METHOD dblClick()
   METHOD mouseDown()
   METHOD mouseUp()
   METHOD mouseMove()
   METHOD activate()
   ERROR HANDLER OnError

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style      := WS_CHILD + WS_VISIBLE + WS_CLIPCHILDREN + WS_CLIPSIBLINGS
   ::objType    := objTypeActiveX
   ::className  := 'WIN_OLEAUTO'

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:Create( oParent, oOwner, aPos, aSize, aPresParams, lVisible, cCLSID, cLicense )
   LOCAL hObj, hWnd

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT cCLSID   TO ::CLSID
   DEFAULT cLicense TO ::license

   ::CLSID      := cCLSID
   ::license    := cLicense
   ::hContainer := ::oParent:getHWND()

   IF ValType( ::hContainer ) + ValType( ::CLSID ) != "NC"
      RETURN NIL
   ENDIF

   ::hWnd := NIL
   ::nID  := ::oParent:GetControlId()
   ::oOLE := Win_OleAuto()

   Win_AxInit()

   hWnd := WAPI_CreateWindowEX( 0, "AtlAxWin", ::CLSID, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
                                    ::aSize[ 1 ], ::aSize[ 2 ], Win_N2P( ::hContainer ), 0 )
   IF empty( hWnd )
      RETURN NIL
   ENDIF
   ::hWnd := Win_P2N( hWnd )

   hObj := __AxGetControl( Win_N2P( ::hWnd ) )
   if empty( hObj )
      RETURN NIL
   ENDIF
   ::oOLE:__hObj := hObj
   __AxDoVerb( Win_N2P( ::hWnd ), -4 )

   IF !Empty( ::hEvents )
      ::oOle:__hSink := __AxRegisterHandler( ::oOle:__hObj, {|nEvent,...| ::execEvent( nEvent, ... ) } )
   ENDIF

   ::oParent:addChild( SELF )

   RETURN Self

/*----------------------------------------------------------------------*/

PROCEDURE execEvent( nEvent, ... ) CLASS WvgActiveXControl
#if 0
   LOCAL cEvents := HB_ValToStr( nEvent ) + ", "
   LOCAL aEvents := { ... }
   aEval( aEvents, { | xEvent | cEvents += HB_ValToStr( xEvent ) + ", " } )
   WAPI_OutputDebugString( cEvents )
#endif

   IF hb_hHaskey( ::hEvents, nEvent )
      eval( ::hEvents[ nEvent ], ... )
   ENDIF

   RETURN

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:handleEvent( nEvent, aInfo )
   LOCAL nHandled := 0

   HB_SYMBOL_UNUSED( aInfo )

   SWITCH nEvent

   CASE WM_SIZE
      IF hb_isBlock( ::sl_resize )
         eval( ::sl_resize, NIL, NIL, self )
      ENDIF
      EXIT
   END

   RETURN nHandled

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:OnError()
#if 0
   WAPI_OutputDebugString( "HI: " + HB_ValToStr( __GetMessage() ) + " : " + str( len( HB_AParams() ) ) )
#endif
   RETURN HB_ExecFromArray( ::oOLE, __GetMessage(), HB_AParams() )

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:Destroy()
#if 0
   WAPI_OutputDebugString( "WvgActiveXControl:Destroy()" )
#endif
   IF !empty( ::oOLE:__hObj )
      IF WAPI_IsWindow( Win_N2P( ::hWnd ) )
         WAPI_DestroyWindow( Win_N2P( ::hWnd ) )
      ENDIF
      ::oOle := NIL
      ::hWnd := NIL
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:mapEvent( nEvent, bBlock )

   if hb_isNumeric( nEvent ) .and. hb_isBlock( bBlock )
      ::hEvents[ nEvent ] := bBlock
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:inheritPresParams()
   Local lSuccess := .t.

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:presParamsChanged()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:setInputFocus()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:subscribeStdEvents()
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:unsubscribeStdEvents()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:keyDown()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:click()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:dblClick()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:mouseDown()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:mouseUp()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:mouseMove()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgActiveXControl:activate()
   RETURN Self

/*----------------------------------------------------------------------*/
