/*
 * Xbase++ Compatible xbpActiveXControl Class
 *
 * Copyright 2008 Andy Wos
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

#ifndef __DBG_PARTS__
#xtranslate hb_traceLog( [<x,...>] ) =>
#endif

CREATE CLASS WvgActiveXControl INHERIT WvgWindow

   VAR    oOLE
   VAR    CLSID                              INIT ""
   VAR    server                             INIT NIL
   VAR    license                            INIT NIL
   VAR    controlFlags                       INIT 0
   VAR    default                            INIT .F.
   VAR    cancel                             INIT .F.

   VAR    interface
   VAR    interfaceName

   VAR    lSubStdEvents                      INIT .F.

   VAR    hEvents                            INIT { => }
   VAR    hContainer

   VAR    ClassName

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible, cCLSID, cLicense )
   METHOD Destroy()
   METHOD execEvent( nEvent, ... )
   METHOD handleEvent( nEvent, aNM )
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

   ERROR HANDLER OnError()

ENDCLASS

METHOD WvgActiveXControl:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style      := WS_CHILD + WS_VISIBLE + WS_CLIPCHILDREN + WS_CLIPSIBLINGS
   ::exStyle    := WS_EX_CLIENTEDGE
   ::objType    := objTypeActiveX
   ::className  := "WIN_OLEAUTO"

   RETURN Self

METHOD WvgActiveXControl:Create( oParent, oOwner, aPos, aSize, aPresParams, lVisible, cCLSID, cLicense )

   LOCAL hObj, hWnd

   ::WvgWindow:create( @oParent, @oOwner, aPos, aSize, aPresParams, lVisible )

   __defaultNIL( @cCLSID, ::CLSID )
   __defaultNIL( @cLicense, ::license )

   ::CLSID      := cCLSID
   ::license    := cLicense
   ::hContainer := iif( HB_ISOBJECT( ::oParent ), ::oParent:getHWND(), ::oParent )

   IF ! HB_ISNUMERIC( ::hContainer ) .OR. ! HB_ISSTRING( ::CLSID )
      RETURN NIL
   ENDIF

   ::hWnd := NIL
   ::nID  := iif( HB_ISOBJECT( ::oParent ), ::oParent:GetControlId(), ::getControlID() )
   ::oOLE := win_oleAuto()

   win_axInit()

   hWnd := wapi_CreateWindowEx( ::exStyle, "AtlAxWin", ::CLSID, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
                                ::aSize[ 1 ], ::aSize[ 2 ], win_N2P( ::hContainer ), 0, NIL, NIL )
   IF Empty( hWnd )
      RETURN NIL
   ENDIF
   ::hWnd := win_P2N( hWnd )
   ::pWnd := hWnd

   hObj := __axGetControl( ::pWnd )
   IF Empty( hObj )
      RETURN NIL
   ENDIF
   ::oOLE:__hObj := hObj
   __axDoVerb( ::pWnd, -4 )

   IF ! Empty( ::hEvents )
      ::oOle:__hSink := __axRegisterHandler( ::oOle:__hObj, {| nEvent, ... | ::execEvent( nEvent, ... ) } )
   ENDIF

#if 0
   ::SetWindowProcCallback()  /* Is this needed to catch windowing events ? - NO */
#endif

   IF HB_ISOBJECT( ::oParent )
      ::oParent:addChild( Self )
   ENDIF

   ::setPosAndSize()
   IF ::visible
      ::show()
   ELSE
      ::hide()
   ENDIF
   IF ::isParentCrt()
      ::oParent:setFocus()
   ENDIF

   RETURN Self

METHOD PROCEDURE WvgActiveXControl:execEvent( nEvent, ... )

#if 0
   LOCAL cEvents := hb_ValToStr( nEvent ) + ", "
   LOCAL aEvents := { ... }

   AEval( aEvents, {| xEvent | cEvents += hb_ValToStr( xEvent ) + ", " } )
   hb_traceLog( cEvents )
#endif

   IF hb_HHasKey( ::hEvents, nEvent )
      Eval( ::hEvents[ nEvent ], ... )
   ENDIF

   RETURN

METHOD WvgActiveXControl:handleEvent( nEvent, aNM )

   LOCAL nHandled := 0

   HB_SYMBOL_UNUSED( aNM )

   SWITCH nEvent

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      IF HB_ISBLOCK( ::sl_resize )
         Eval( ::sl_resize, , , Self )
      ENDIF
      EXIT

   CASE HB_GTE_ANY
      EXIT

   ENDSWITCH

   RETURN nHandled

METHOD WvgActiveXControl:OnError()

#if 0
   hb_traceLog( "HI: " + hb_ValToStr( __GetMessage() ) + " : " + Str( Len( hb_AParams() ) ) )
#endif

   RETURN hb_ExecFromArray( ::oOLE, __GetMessage(), hb_AParams() )

METHOD PROCEDURE WvgActiveXControl:Destroy()

   IF ! Empty( ::oOLE )
      IF ! Empty( ::oOLE:__hObj )
         IF wapi_IsWindow( ::pWnd )
            wapi_DestroyWindow( ::pWnd )
         ENDIF
         ::oOle := NIL
         ::hWnd := NIL
      ENDIF
   ENDIF
   RETURN

METHOD WvgActiveXControl:mapEvent( nEvent, bBlock )

   IF HB_ISNUMERIC( nEvent ) .AND. HB_ISBLOCK( bBlock )
      ::hEvents[ nEvent ] := bBlock
   ENDIF

   RETURN Self

METHOD WvgActiveXControl:inheritPresParams()
   RETURN .T.

METHOD WvgActiveXControl:presParamsChanged()
   RETURN Self

METHOD WvgActiveXControl:setInputFocus()
   RETURN Self

METHOD WvgActiveXControl:subscribeStdEvents()
   RETURN NIL

METHOD WvgActiveXControl:unsubscribeStdEvents()
   RETURN Self

METHOD WvgActiveXControl:keyDown()
   RETURN Self

METHOD WvgActiveXControl:click()
   RETURN Self

METHOD WvgActiveXControl:dblClick()
   RETURN Self

METHOD WvgActiveXControl:mouseDown()
   RETURN Self

METHOD WvgActiveXControl:mouseUp()
   RETURN Self

METHOD WvgActiveXControl:mouseMove()
   RETURN Self

METHOD WvgActiveXControl:activate()
   RETURN Self
