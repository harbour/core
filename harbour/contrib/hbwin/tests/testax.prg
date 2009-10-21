/*
 * $Id$
 */

#include "hbgtinfo.ch"
#include "hbclass.ch"

REQUEST HB_GT_WVT_DEFAULT

#define WS_CHILD            1073741824
#define WS_VISIBLE          268435456
#define WS_CLIPCHILDREN     33554432

PROCEDURE Main()
   LOCAL oMSCAL

   WAIT "Make sure we are 'Active Window'"
   oMSCAL := HActiveX():Init( WAPI_GetActiveWindow(), "MSCAL.Calendar", 0, 0, 300, 300 )
   WAIT "Press any key to exit"
   oMSCAL:Close()
   RETURN

CLASS HActiveX
   DATA oOLE
   METHOD Init
   METHOD Event
   ERROR HANDLER OnError
// DESTRUCTOR Close
   METHOD Close
ENDCLASS

METHOD Init( hWnd, cProgId, nTop, nLeft, nWidth, nHeight, cID ) CLASS HActiveX
   LOCAL nStyle := WS_CHILD + WS_VISIBLE + WS_CLIPCHILDREN
   win_AxInit()
   hWnd := WAPI_CreateWindowEX( 0, "AtlAxWin", cProgId, nStyle, nLeft, nTop, nWidth, nHeight, hWnd, 0 )
   ::oOLE := WIN_AxGetControl( hWnd, { | event, ... | ::Event( event, ... ) }, cID )
   RETURN self

PROCEDURE Event( ... ) CLASS HActiveX
   LOCAL cEvents := ""
   LOCAL aEvents := { ... }
   aEval( aEvents, { | xEvent | cEvents += HB_ValToStr( xEvent ) + ", " } )
   wapi_OutputDebugString( cEvents )
   RETURN

METHOD OnError() CLASS HActiveX
   RETURN HB_ExecFromArray( ::oOLE, __GetMessage(), HB_AParams() )

METHOD Close() CLASS HActiveX
   wapi_OutputDebugString( "Close" )
   ::oOLE := NIL
   wapi_OutputDebugString( "After Close" )
   RETURN NIL
