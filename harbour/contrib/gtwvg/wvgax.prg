/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Andy Wos
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

#include 'hbclass.ch'
#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'

#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'wvgparts.ch'

/*----------------------------------------------------------------------*/

STATIC  nRef := 0

/*----------------------------------------------------------------------*/

CLASS WvgActiveXControl FROM TOleAuto, WvgWindow

   DATA   CLSID                              INIT ''
   DATA   server                             INIT NIL
   DATA   license                            INIT NIL
   DATA   controlFlags                       INIT 0
   DATA   visible                            INIT .T.
   DATA   default                            INIT .F.
   DATA   cancel                             INIT .F.
   DATA   hObj

   DATA   interface
   DATA   interfaceName

   DATA   lSubStdEvents                      INIT .f.

   DATA   hEvents                            INIT hb_hash()
   DATA   hContainer
   DATA   hSink


   DATA   hhOBJ

   METHOD New()
   METHOD Create()
   METHOD Destroy()

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

   METHOD mapEvent( nEvent, bBlock )

   METHOD handleEvent()

PROTECTED:
   METHOD adviseEvents()

   ENDCLASS

/*----------------------------------------------------------------------*/
METHOD New( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgActiveXControl

   ::wvgWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style      := WS_CHILD + WS_VISIBLE + WS_CLIPCHILDREN + WS_CLIPSIBLINGS
   ::objType    := objTypeActiveX
   ::className  := 'WVGACTIVEX'

   RETURN Self
/*----------------------------------------------------------------------*/
METHOD Create( oParent, oOwner, aPos, aSize, aPresParams, lVisible, cCLSID, cLicense ) CLASS WvgActiveXControl
   LOCAL hx, pUnk

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT cCLSID   TO ::CLSID
   DEFAULT cLicense TO ::license

   ::CLSID      := cCLSID
   ::license    := cLicense

   ::hObj       := 0
   ::hSink      := 0

   ::hContainer := ::oParent:getHWND()

   IF ValType( ::hContainer ) + ValType( ::CLSID ) != "NC"
      RETURN( NIL )
   ELSEIF HB_AX_AtlAxWinInit()
      nRef++
   ENDIF

   ::nID := ::oParent:GetControlId()

   #if 1
   ::hObj := HB_AX_AtlAxGetControl( "ATLAXWin", ::hContainer, ::CLSID, ::nID, ;
                                    ::aPos[ 1 ], ::aPos[ 2 ], ::aSize[ 1 ], ::aSize[ 2 ], ;
                                    ::style, ::exStyle, NIL, @hx, @pUnk )
   #else
   ::hObj := HB_AX_AtlAxCreateControl( "ATLAXWin", ::hContainer, ::CLSID, ::nID, ;
                                    ::aPos[ 1 ], ::aPos[ 2 ], ::aSize[ 1 ], ::aSize[ 2 ], ;
                                    ::style, ::exStyle, NIL, @hx, @pUnk )
   #endif
   if ::hObj == 0
      Return NIL
   endif

   /* Required as to AddRef() to self */
   TOleAuto():New( ::hObj )

   ::oParent:addChild( SELF )
   ::hWnd := hx

   HB_AX_AtlSetVerb( ::hWnd, pUnk, -4 )

   IF ::hObj <> 0  .AND. !Empty( ::hEvents )
      ::AdviseEvents()
   ENDIF

   #if 0
   HB_AX_LoadTypeInfo( ::hObj )
   #endif

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD handleEvent( nEvent, aInfo ) CLASS WvgActiveXControl
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

METHOD Destroy() CLASS WvgActiveXControl
   LOCAL bError := ErrorBlock()

   BEGIN SEQUENCE

   IF hb_IsNumeric( ::hObj ) .and. ::hObj <> 0

      IF Win_IsWindow( ::hWnd )
         Win_DestroyWindow( ::hWnd )
      ENDIF

      IF ::hSink <> 0
         HB_AX_ShutDownConnectionPoint( ::hSink )
         ::hSink := NIL
      ENDIF

      IF --nRef == 0
        /*  HB_AX_AtlAxWinTerm()  */
      ENDIF
   ENDIF

   ENDSEQUENCE
   ErrorBlock( bError )
   RETURN NIL
/*----------------------------------------------------------------------*/
METHOD adviseEvents() CLASS WvgActiveXControl
   LOCAL  n, hSink, xRet

   xRet := HB_AX_SetupConnectionPoint( ::hObj, @hSink, @n, ::hEvents )
   ::hSink := hSink

   RETURN xRet
/*----------------------------------------------------------------------*/
METHOD mapEvent( nEvent, bBlock )

   if hb_isNumeric( nEvent ) .and. hb_isBlock( bBlock )
      ::hEvents[ nEvent ] := { bBlock }
   endif

   RETURN Self
/*----------------------------------------------------------------------*/
METHOD inheritPresParams() CLASS WvgActiveXControl
   Local lSuccess := .t.

   RETURN lSuccess
/*----------------------------------------------------------------------*/
METHOD presParamsChanged() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD setInputFocus() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD subscribeStdEvents() CLASS WvgActiveXControl
   RETURN NIL
/*----------------------------------------------------------------------*/
METHOD unsubscribeStdEvents() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD keyDown() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD click() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD dblClick() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD mouseDown() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD mouseUp() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD mouseMove() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD activate() CLASS WvgActiveXControl
   RETURN Self
/*----------------------------------------------------------------------*/
/*                       Class AutomationObject                         */
/*----------------------------------------------------------------------*/
#if 0
CLASS AutomationObject

   DATA   interface             AS NUMERIC   READONLY
   DATA   interfaceName         AS CHARACTER READONLY
   DATA   CLSID                 AS CHARACTER READONLY INIT ' '
   DATA   server                AS CHARACTER READONLY INIT ' '
   DATA   license               AS CHARACTER READONLY INIT ' '
   DATA   cargo

   METHOD create( cProgID, cServerName, cLicense )
   METHOD destroy()
   METHOD dynamicCast()
   METHOD addRef()
   METHOD release()
   METHOD queryInterface( cGUID )
   METHOD getIDsOfNames( aNames )
   METHOD invoke( cNameORnID, nNamedArgs /*, ...*/ )
   METHOD loadTypeLib( cTypeLib )
   METHOD getProperty( cNameORnID )
   METHOD setProperty( cNameORnID, xValue /*, ...*/ )
   METHOD callMethod( cNameORnID /*, ...*/ )
   METHOD onError( oError )

   ENDCLASS

/*----------------------------------------------------------------------*/
METHOD create( cProgID, cServerName, cLicense )CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD destroy() CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD dynamicCast() CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD addRef() CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD release() CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD queryInterface( cGUID ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD getIDsOfNames( aNames ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD invoke( cNameORnID, nNamedArgs, ... ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD loadTypeLib( cTypeLib ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD getProperty( cNameORnID ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD setProperty( cNameORnID, xValue, ... ) CLASS AutomationObject
   Local lValue

   RETURN lValue
/*----------------------------------------------------------------------*/
METHOD callMethod( cNameORnID, ... ) CLASS AutomationObject
   Local xValue

   RETURN xValue
/*----------------------------------------------------------------------*/
METHOD onError( oError ) CLASS AutomationObject
   Local xValue

   RETURN xValue
#endif
/*----------------------------------------------------------------------*/

