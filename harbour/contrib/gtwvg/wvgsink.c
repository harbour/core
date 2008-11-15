/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Andy Wos
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
//                     Active-X Interface Functions
//
//                       Contributed by Andy Wos
//                   A little tweaked by Pritpal Bedi
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
/*
   Reference material:

   http://codeguru.earthweb.com/mfc/comments/55024.shtml

   Several examples here:
   http://www.john.findlay1.btinternet.co.uk/OLE/ole.htm

   ADO example here:
   http://www.john.findlay1.btinternet.co.uk/DataBase/database.htm

   HOWTO: Use OLE Automation from a C Application Rather Than C++:
   http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/q181/4/73.asp&NoWebContent=1

   The Microsoft B2C.EXE utility, which converts Microsoft Visual Basic Automation code into Microsoft Visual C++ code.
   http://support.microsoft.com/default.aspx?scid=kb;EN-US;216388
*/
//----------------------------------------------------------------------//

#ifdef __XHARBOUR__
   #include "hbvmopt.h"
#else
   #include "hbvmint.h"
#endif

#include <windows.h>
#include <oaidl.h>
#include "hbapi.h"
#include "item.api"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"

#include <shlobj.h>
#include <objbase.h>
#include <ocidl.h>
#include <olectl.h>
#include <ole2.h>
#include <oleauto.h>

//----------------------------------------------------------------------//
static HRESULT  s_nOleError;
static HMODULE  hLib = NULL;

typedef BOOL    ( CALLBACK *PATLAXWININIT )( void );
typedef BOOL    ( CALLBACK *PATLAXWINTERM )( void );
typedef HRESULT ( CALLBACK *PATLAXGETCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXATTACHCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXCREATECONTROL )( LPCOLESTR, HWND, IStream*, IUnknown** );
//----------------------------------------------------------------------//
HB_EXPORT void hb_oleItemToVariant( VARIANT *pVariant, PHB_ITEM pItem );

HRESULT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

static int s_ev_iid_[ 100 ];
//----------------------------------------------------------------------//
static int ThisThreadId( void )
{
   PHB_THREADSTATE pThread;

   pThread = ( PHB_THREADSTATE ) hb_vmThreadState();
   if( pThread )
      return( ( int ) pThread->th_no );
   else
      return( 0 );
}
//----------------------------------------------------------------------//
// these 2 functions are required to send parameters by reference
//
static void hb_itemPushList( ULONG ulRefMask, ULONG ulPCount, PHB_ITEM** pItems )
{
   HB_ITEM itmRef;
   ULONG   ulParam;

   if( ulPCount )
   {
      // initialize the reference item
      itmRef.type = HB_IT_BYREF;
      itmRef.item.asRefer.offset = -1;
      itmRef.item.asRefer.BasePtr.itemsbasePtr = pItems;

      for( ulParam = 0; ulParam < ulPCount; ulParam++ )
      {
         if( ulRefMask & ( 1L << ulParam ) )
         {
            // when item is passed by reference then we have to put
            // the reference on the stack instead of the item itself
            itmRef.item.asRefer.value = ulParam+1;
            hb_vmPush( &itmRef );
         }
         else
         {
            hb_vmPush( ( *pItems )[ ulParam ] );
         }
      }
   }
}
//----------------------------------------------------------------------//
//this is a macro which defines our IEventHandler struct as so:
//
// typedef struct {
//    IEventHandlerVtbl  *lpVtbl;
// } IEventHandler;

#undef  INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_ ( INTERFACE, IDispatch )
{
   // IUnknown functions
   STDMETHOD  ( QueryInterface ) ( THIS_ REFIID, void ** ) PURE;
   STDMETHOD_ ( ULONG, AddRef )  ( THIS ) PURE;
   STDMETHOD_ ( ULONG, Release ) ( THIS ) PURE;
   // IDispatch functions
   STDMETHOD_ ( ULONG, GetTypeInfoCount ) ( THIS_ UINT * ) PURE;
   STDMETHOD_ ( ULONG, GetTypeInfo ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;
   STDMETHOD_ ( ULONG, GetIDsOfNames ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;
   STDMETHOD_ ( ULONG, Invoke ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};
//----------------------------------------------------------------------//
//
// In other words, it defines our IEventHandler to have nothing
// but a pointer to its VTable. And of course, every COM object must
// start with a pointer to its VTable.
//
// But we actually want to add some more members to our IEventHandler.
// We just don't want any app to be able to know about, and directly
// access, those members. So here we'll define a MyRealIEventHandler that
// contains those extra members. The app doesn't know that we're
// really allocating and giving it a MyRealIEventHAndler object. We'll
// lie and tell it we're giving a plain old IEventHandler. That's ok
// because a MyRealIEventHandler starts with the same VTable pointer.
//
// We add a DWORD reference count so that this IEventHandler
// can be allocated (which we do in our IClassFactory object's
// CreateInstance()) and later freed. And, we have an extra
// BSTR (pointer) string, which is used by some of the functions we'll
// add to IEventHandler
//----------------------------------------------------------------------//

typedef struct {
   DISPID     dispid;
   PHB_ITEM   pSelf;
   PHB_DYNS   pSymbol;
} EventMap;


typedef struct {
   IEventHandler*          lpVtbl;
   DWORD                   count;
   IConnectionPoint*       pIConnectionPoint;  // Ref counted of course.
   DWORD                   dwEventCookie;
   char*                   parent_on_invoke;
   IID                     device_event_interface_iid;
   PHB_ITEM                pSelf;        // object to handle the events (optional)
   EventMap*               pEventMap;    // event map
   int                     iEventMapLen; // length of the eventMap
   PHB_ITEM                pEvents;
} MyRealIEventHandler;

//----------------------------------------------------------------------//
// Here are IEventHandler's functions.
//----------------------------------------------------------------------//
//
// Every COM object's interface must have the 3 functions QueryInterface(),
// AddRef(), and Release().

// IEventHandler's QueryInterface()
static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *this, REFIID vTableGuid, void **ppv )
{
   // Check if the GUID matches IEvenetHandler VTable's GUID. We gave the C variable name
   // IID_IEventHandler to our VTable GUID. We can use an OLE function called
   // IsEqualIID to do the comparison for us. Also, if the caller passed a
   // IUnknown GUID, then we'll likewise return the IEventHandler, since it can
   // masquerade as an IUnknown object too. Finally, if the called passed a
   // IDispatch GUID, then we'll return the IExample3, since it can masquerade
   // as an IDispatch too

   if ( IsEqualIID( vTableGuid, &IID_IUnknown ) )
   {
      *ppv = ( IUnknown * ) this;

      /* Increment the count of callers who have an outstanding pointer to this object */
      this->lpVtbl->AddRef( this );
      return S_OK;
   }

   if ( IsEqualIID( vTableGuid, &IID_IDispatch ) )
   {
      *ppv = ( IDispatch * ) this;
      this->lpVtbl->AddRef( this );
      return S_OK;
   }

   if ( IsEqualIID( vTableGuid, &( ( ( MyRealIEventHandler * ) this )->device_event_interface_iid ) ) )
   {
      if( s_ev_iid_[ ThisThreadId() ] == 0 )
      {
         s_ev_iid_[ ThisThreadId() ]++;
         *ppv = ( IDispatch* ) this;
         this->lpVtbl->AddRef( this );
      }
      return S_OK;
   }

   // We don't recognize the GUID passed to us. Let the caller know this,
   // by clearing his handle, and returning E_NOINTERFACE.
   *ppv = 0;
   return( E_NOINTERFACE );
}
//----------------------------------------------------------------------//
//
// IEventHandler's AddRef()

static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *this )
{
   // Increment IEventHandler's reference count, and return the updated value.
   // NOTE: We have to typecast to gain access to any data members. These
   // members are not defined  (so that an app can't directly access them).
   // Rather they are defined only above in our MyRealIEventHandler
   // struct. So typecast to that in order to access those data members

   return( ++( ( MyRealIEventHandler * ) this )->count );

}
//----------------------------------------------------------------------//
//
// IEventHandler's Release()

static ULONG STDMETHODCALLTYPE Release( IEventHandler *this )
{
   if ( --( ( MyRealIEventHandler * ) this )->count == 0 )
   {

      if( ( ( MyRealIEventHandler * ) this)->pSelf )
         hb_itemRelease( ( ( MyRealIEventHandler * ) this )->pSelf );

      if( ( ( MyRealIEventHandler * ) this )->pEventMap )
         hb_xfree( ( ( MyRealIEventHandler * ) this )->pEventMap );

      GlobalFree( this );
      return( 0 );
   }
   return( ( ( MyRealIEventHandler * ) this )->count );
}
//----------------------------------------------------------------------//
//
// IEventHandler's GetTypeInfoCount()

static ULONG STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *this, UINT *pCount )
{
   HB_SYMBOL_UNUSED( this );
   HB_SYMBOL_UNUSED( pCount );

   return ( ULONG ) E_NOTIMPL;
}
//----------------------------------------------------------------------//
//
// IEventHandler's GetTypeInfo()

static ULONG STDMETHODCALLTYPE GetTypeInfo( IEventHandler *this, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( this );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );

   return ( ULONG ) E_NOTIMPL;
}
//----------------------------------------------------------------------//
//
// IEventHandler's GetIDsOfNames()

static ULONG STDMETHODCALLTYPE GetIDsOfNames( IEventHandler *this, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid )
{

   HB_SYMBOL_UNUSED( this );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgdispid );

   return ( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------

static ULONG STDMETHODCALLTYPE Invoke( IEventHandler *this, DISPID dispid, REFIID riid,
                                       LCID lcid, WORD wFlags, DISPPARAMS *params,
                                       VARIANT *result, EXCEPINFO *pexcepinfo, UINT *puArgErr )
{
   int        iArg;
   int        i;
   ULONG      ulRefMask = 0;
   ULONG      ulPos;
   PHB_ITEM   pItem;
   PHB_ITEM   pItemArray[ 32 ]; // max 32 parameters?
   PHB_ITEM   *pItems;
   PHB_ITEM   Key;

   // We implement only a "default" interface
   if ( !IsEqualIID( riid, &IID_NULL ) )
   {
      return( ( ULONG ) DISP_E_UNKNOWNINTERFACE );
   }
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   Key = hb_itemNew( NULL );
   if ( hb_hashScan( ( ( MyRealIEventHandler * ) this )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) this )->pEvents, ulPos );
      PHB_ITEM pExec  = hb_arrayGetItemPtr( pArray, 1 );

      if ( pExec )
      {
         hb_vmPushState();

         switch ( hb_itemType( pExec ) )
         {
         case HB_IT_BLOCK:
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pExec );
            break;

         case HB_IT_STRING:
            {
               PHB_ITEM pObject = hb_arrayGetItemPtr( pArray, 3 );

               hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( hb_itemGetCPtr( pExec ) ) ) );
               if ( HB_IS_OBJECT( pObject ) )
                  hb_vmPush( pObject );
               else
                  hb_vmPushNil();
            }
            break;

         case HB_IT_POINTER:
            hb_vmPushSymbol( hb_dynsymSymbol( ( ( PHB_SYMB ) pExec )->pDynSym ) );
            hb_vmPushNil();
            break;
         }

         iArg = params->cArgs;
         for( i = 1; i <= iArg; i++ )
         {
            pItem = hb_itemNew( NULL );
            hb_oleVariantToItem( pItem, &( params->rgvarg[ iArg-i ] ) ); // VARIANT *pVariant )
            pItemArray[ i-1 ] = pItem;
            ulRefMask |= ( 1L << (i-1) );                                // set bit i
         }

         if( iArg )
         {
            pItems = pItemArray;
            hb_itemPushList( ulRefMask, iArg, &pItems );
         }
         // execute
         //
         hb_vmDo( iArg );

         for( i=iArg; i > 0; i-- )
         {
            if( ( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.vt & VT_BYREF ) == VT_BYREF )
            {
               hb_oleItemToVariant( &( params->rgvarg[ iArg-i ] ), pItemArray[ iArg-i ] );

               #if 0
               switch( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.vt )
               {
               case VT_I2|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.piVal )    = ( short ) hb_itemGetNI( pItemArray[i-1] );
                  break;
               case VT_I4|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.plVal )    = ( long ) hb_itemGetNL( pItemArray[i-1] );
                  break;
               case VT_R4|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.pfltVal )  = ( float ) hb_itemGetND( pItemArray[i-1] );
                  break;
               case VT_R8|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.pdblVal )  = ( double ) hb_itemGetND( pItemArray[i-1] );
                  break;
               case VT_BOOL|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.pboolVal ) = hb_itemGetL( pItemArray[i-1] ) ? 0xFFFF : 0;
                  break;
               case VT_DATE|VT_BYREF:
                  *( ( &( params->rgvarg[ iArg-i ] ) )->n1.n2.n3.pdate )    = ( DATE ) ( double ) ( hb_itemGetDL( pItemArray[i-1] )-2415019 );
                  break;
               }
               #endif
            }
         }

         // Pritpal
         if ( iArg )
         {
            for( i=iArg; i > 0; i-- )
            {
               hb_itemRelease( pItemArray[ i-1 ] );
            }
         }
         hb_vmPopState();
      }
   }
   hb_itemRelease( Key );   // Pritpal

   return ( ULONG ) S_OK;
}

//----------------------------------------------------------------------//
// Here's IEventHandler's VTable. It never changes so we can declare it static
//
static const IEventHandlerVtbl IEventHandler_Vtbl = {
   QueryInterface,
   AddRef,
   Release,
   GetTypeInfoCount,
   GetTypeInfo,
   GetIDsOfNames,
   Invoke
};

#include <ocidl.h>

//----------------------------------------------------------------------//
// constructor : params:
// device_interface        - refers to the interface type of the COM object (whose event we are trying to receive).
// device_event_interface  - indicates the interface type of the outgoing interface supported by the COM object.
//                           This will be the interface that must be implemented by the Sink object.
//                           is essentially derived from IDispatch, our Sink object (this IEventHandler)
//                           is also derived from IDispatch.

typedef IEventHandler device_interface;

//----------------------------------------------------------------------//
static HRESULT SetupConnectionPoint( device_interface* pdevice_interface, REFIID riid, void** pThis, int* pn )
{
   IConnectionPointContainer*  pIConnectionPointContainerTemp = NULL;
   IUnknown*                   pIUnknown;
   IConnectionPoint*           m_pIConnectionPoint = NULL;
   IEnumConnectionPoints*      m_pIEnumConnectionPoints;
   HRESULT                     hr;
   IID                         rriid;
   register IEventHandler*     thisobj;
   DWORD                       dwCookie = 0;

   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( pn );

   if( !( thisobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) ) ) )
   {
      hr = E_OUTOFMEMORY;
   }
   else
   {
      thisobj->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;

      ( ( MyRealIEventHandler * ) thisobj )->count = 0;

      hr = thisobj->lpVtbl->QueryInterface( thisobj, &IID_IUnknown, (void**) &pIUnknown);
      if (hr == S_OK && pIUnknown)
      {
         hr = pdevice_interface->lpVtbl->QueryInterface( pdevice_interface, &IID_IConnectionPointContainer, (void**) &pIConnectionPointContainerTemp);
         if ( hr == S_OK && pIConnectionPointContainerTemp )
         {
            hr = pIConnectionPointContainerTemp->lpVtbl->EnumConnectionPoints( pIConnectionPointContainerTemp, &m_pIEnumConnectionPoints );
            if ( hr == S_OK && m_pIEnumConnectionPoints )
            {
               do
               {
                  hr = m_pIEnumConnectionPoints->lpVtbl->Next( m_pIEnumConnectionPoints, 1, &m_pIConnectionPoint, NULL );

                  if( hr == S_OK )
                  {
                     hr = m_pIConnectionPoint->lpVtbl->GetConnectionInterface( m_pIConnectionPoint, &rriid );

                     if ( hr == S_OK )
                     {
                        ( ( MyRealIEventHandler* ) thisobj )->device_event_interface_iid = rriid;

                        hr = m_pIConnectionPoint->lpVtbl->Advise( m_pIConnectionPoint, pIUnknown, &dwCookie );
                        if ( hr == S_OK )
                        {
                           ( ( MyRealIEventHandler* ) thisobj )->pIConnectionPoint = m_pIConnectionPoint;
                           ( ( MyRealIEventHandler* ) thisobj )->dwEventCookie     = dwCookie;
                        }
                        else
                        {
                           hr = S_OK;
                        }
                     }
                     else
                     {
                        hr = S_OK;
                     }
                  }
               } while( hr == S_OK );

               m_pIEnumConnectionPoints->lpVtbl->Release( m_pIEnumConnectionPoints );
               m_pIEnumConnectionPoints = NULL;
            }

            pIConnectionPointContainerTemp->lpVtbl->Release( pIConnectionPointContainerTemp );
            pIConnectionPointContainerTemp = NULL;
         }

         pIUnknown->lpVtbl->Release( pIUnknown );
         pIUnknown = NULL;
      }
   }

   *pThis = ( void * ) thisobj;

   return hr;
}
//----------------------------------------------------------------------//
static void ShutdownConnectionPoint( MyRealIEventHandler *this )
{
    if ( this->pIConnectionPoint )
    {
       this->dwEventCookie = 0;
       this->pIConnectionPoint->lpVtbl->Release( this->pIConnectionPoint );
       this->pIConnectionPoint = NULL;
    }
}
//----------------------------------------------------------------------//
HB_FUNC( HB_AX_SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler* hSink = ( MyRealIEventHandler * ) hb_parnl( 1 );
   ShutdownConnectionPoint( hSink );
   hb_itemRelease( hSink->pEvents );

   s_ev_iid_[ ThisThreadId() ] = 0;
}
//----------------------------------------------------------------------//
HB_FUNC( HB_AX_SETUPCONNECTIONPOINT )
{
   HRESULT              hr;
   MyRealIEventHandler* hSink = NULL;
   LPIID                riid  = ( LPIID ) &IID_IDispatch;
   int                  n;

   hr = SetupConnectionPoint( ( device_interface* ) hb_parnint( 1 ), ( REFIID ) riid, ( void** ) &hSink, &n ) ;

   hSink->pEvents = hb_itemNew( hb_param( 4, HB_IT_ANY ) );
   hb_stornl( ( LONG ) hSink, 2 );
   hb_storni( n, 3 );
   hb_retnl( hr );
}
//----------------------------------------------------------------------//
//                ActiveX Container Management Interface
//----------------------------------------------------------------------//
HB_FUNC( HB_AX_ATLAXWININIT )
{
   PATLAXWININIT AtlAxWinInit;
   char szLibName[ MAX_PATH + 1 ] = { 0 } ;
   BOOL bRet = FALSE;

   if( !hLib )
   {
      GetSystemDirectory( szLibName, MAX_PATH );
      hb_strncat( szLibName, "\\atl.dll", MAX_PATH -1 );
      hLib = LoadLibrary( ( LPCSTR ) szLibName );

      if( hLib )
      {
         AtlAxWinInit = ( PATLAXWININIT ) GetProcAddress( hLib, "AtlAxWinInit" );
         if ( AtlAxWinInit )
         {
            if ( ( AtlAxWinInit )() )
            {
               bRet = TRUE;
            }
         }

         if( !bRet )
         {
            FreeLibrary( hLib );
            hLib = NULL;
         }
      }
   }
   else
   {
      bRet = TRUE;
   }

   hb_retl( bRet);
}
//---------------------------------------------------------------------------//
HB_FUNC( HB_AX_ATLAXCREATECONTROL )
{
   IUnknown  *pUnk;
   IDispatch *obj;
   BSTR  wString;
   UINT  uLen;
   PATLAXCREATECONTROL AtlAxCreateControl;
   HWND  hContainer;
   RECT  rc;
   char  *class   = hb_parc( 1 );
   HWND  hParent  = ( HWND ) hb_parnl(2);
   char  *Caption = ISNIL(  4 ) ? "" : hb_parc( 3 );
   HMENU id       = ISNIL(  4 ) ? ( HMENU )-1 : ( HMENU ) hb_parni( 4 );
   int   x        = ISNIL(  5 ) ? 0 : hb_parni( 5 );
   int   y        = ISNIL(  6 ) ? 0 : hb_parni( 6 );
   int   w        = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   int   h        = ISNIL(  8 ) ? 0 : hb_parni( 8 );
   int   Style    = ISNIL(  9 ) ? WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS : hb_parni( 9 );
   int   Exstyle  = ISNIL( 10 ) ? 0 : hb_parni( 10 );

   AtlAxCreateControl = ( PATLAXCREATECONTROL ) GetProcAddress( hLib, "AtlAxCreateControl" );
   if ( AtlAxCreateControl )
   {
      LPTSTR cCaption = HB_TCHAR_CONVTO( Caption );
      LPTSTR cClass = HB_TCHAR_CONVTO( class );
      hContainer = ( HWND ) CreateWindowEx( Exstyle, cClass, cCaption, Style, x, y, w, h, hParent, id, GetModuleHandle( NULL ), NULL );
      HB_TCHAR_FREE( cCaption );
      HB_TCHAR_FREE( cClass );

      if( hContainer )
      {
         SendMessage( ( HWND ) hContainer, ( UINT ) WM_SETFONT, ( WPARAM ) GetStockObject( DEFAULT_GUI_FONT ), ( LPARAM ) ( MAKELPARAM( FALSE, 0 ) ) );
         uLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Caption, strlen( Caption )+1, NULL, 0 );
         wString = ( BSTR ) malloc( uLen * sizeof( WCHAR ) );
         MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Caption, strlen( Caption )+1, wString, uLen );

         ( AtlAxCreateControl ) ( wString, hContainer, NULL, &pUnk );

         free( wString );

         pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, ( void** ) &obj );
         pUnk->lpVtbl->Release( pUnk );
         hb_retnl( ( long ) obj );

         GetClientRect( hContainer, &rc );
         MoveWindow( GetDlgItem( hContainer, ( int ) id ), 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
      }
      else
      {
         hb_retnl( 0 );
      }
   }
   else
   {
      hb_retnl( 0 );
   }

   // return the container handle
   if ISBYREF( 11 )
   {
      hb_stornl( ( long ) hContainer, 11 );
   }
}
//---------------------------------------------------------------------------//
HB_FUNC( HB_AX_ATLAXGETCONTROL ) // HWND hWnd = handle of control container window
{
   IUnknown  *pUnk = NULL;
   IDispatch *obj;
   PATLAXGETCONTROL AtlAxGetControl;
   RECT  rc;
   HWND  hWnd = NULL;
   char  *lpcclass = hb_parc( 1 );
   HWND  hParent   = ( HWND ) hb_parnl( 2 );
   char  *Caption  = ISNIL(  4 ) ? "" : hb_parc( 3 );
   HMENU id        = ISNIL(  4 ) ? ( HMENU )-1 : ( HMENU ) hb_parni( 4 );
   int   x         = ISNIL(  5 ) ? 0 : hb_parni( 5 );
   int   y         = ISNIL(  6 ) ? 0 : hb_parni( 6 );
   int   w         = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   int   h         = ISNIL(  8 ) ? 0 : hb_parni( 8 );
   int   Style     = ISNIL(  9 ) ? WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS : hb_parni( 9 );
   int   Exstyle   = ISNIL( 10 ) ? 0 : hb_parni( 10 );

   AtlAxGetControl = ( PATLAXGETCONTROL ) GetProcAddress( hLib, "AtlAxGetControl" );
   if( AtlAxGetControl )
   {
      LPTSTR cCaption = HB_TCHAR_CONVTO( Caption );
      LPTSTR cClass = HB_TCHAR_CONVTO( lpcclass );
      hWnd = ( HWND ) CreateWindowEx( Exstyle, cClass, cCaption, Style, x, y, w, h, hParent, id, GetModuleHandle( NULL ), NULL );
      HB_TCHAR_FREE( cCaption );
      HB_TCHAR_FREE( cClass );

      if( hWnd )
      {
         SendMessage( hWnd,
                      ( ( UINT ) WM_SETFONT ),
                      ( ( WPARAM ) GetStockObject( DEFAULT_GUI_FONT ) ),
                      ( ( LPARAM ) ( MAKELPARAM( FALSE, 0 ) ) ) );
         ( AtlAxGetControl )( hWnd, &pUnk );

         if( pUnk )
         {
            pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, ( void** ) &obj );
            pUnk->lpVtbl->Release( pUnk );
            GetClientRect( hParent, &rc );
            MoveWindow( GetDlgItem( hParent, ( int ) id ), 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
            hb_retnl( ( long ) obj );
         }
         else
         {
            hb_retnl( 0 );
         }
      }
      else
      {
         hb_retnl( 0 );
      }
   }
   else
   {
      hb_retnl( 0 );
   }

   // return the control handle
   if ISBYREF( 11 )
   {
      hb_stornl( ( long ) hWnd, 11 );
   }
}
//---------------------------------------------------------------------------//
//
// (hOleObject)
//
HB_FUNC( HB_AX_AXRELEASEOBJECT )
{
   IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
   s_nOleError = pDisp->lpVtbl->Release( pDisp );
}
//---------------------------------------------------------------------------//
//
// terminate activex suport, free the library
//
HB_FUNC( HB_AX_ATLAXWINTERM )
{
   PATLAXWINTERM AtlAxWinTerm;
   BOOL          bRet = FALSE;

   if( hLib )
   {
      AtlAxWinTerm = ( PATLAXWINTERM ) GetProcAddress( hLib, "AtlAxWinTerm" );
      if( AtlAxWinTerm )
      {
         if( AtlAxWinTerm() )
         {
            FreeLibrary( hLib );
            hLib = NULL;
            bRet = TRUE;
         }
      }
   }
   hb_retl( bRet );
}
//----------------------------------------------------------------------//

