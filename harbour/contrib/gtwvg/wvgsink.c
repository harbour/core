/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Andy Wos
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
/*
 *                     Active-X Interface Functions
 *
 *                       Contributed by Andy Wos
 *                   A little tweaked by Pritpal Bedi
 */
/*----------------------------------------------------------------------*/

#include "hbvmint.h"
#include "hbwinole.h"

/*----------------------------------------------------------------------*/

#if defined( __BORLANDC__ ) && !defined( HB_ARCH_64BIT )
    #undef MAKELONG
    #define MAKELONG(a,b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | \
                          (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

/*----------------------------------------------------------------------*/

#undef  INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_ ( INTERFACE, IDispatch )
{
   STDMETHOD  ( QueryInterface   ) ( THIS_ REFIID, void ** ) PURE;
   STDMETHOD_ ( ULONG, AddRef    ) ( THIS ) PURE;
   STDMETHOD_ ( ULONG, Release   ) ( THIS ) PURE;
   STDMETHOD  ( GetTypeInfoCount ) ( THIS_ UINT * ) PURE;
   STDMETHOD  ( GetTypeInfo      ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;
   STDMETHOD  ( GetIDsOfNames    ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;
   STDMETHOD  ( Invoke           ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};

#if !defined( HB_OLE_C_API )
typedef struct
{
   HRESULT ( STDMETHODCALLTYPE * QueryInterface   ) ( IEventHandler*, REFIID, void** );
   ULONG   ( STDMETHODCALLTYPE * AddRef           ) ( IEventHandler* );
   ULONG   ( STDMETHODCALLTYPE * Release          ) ( IEventHandler* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfoCount ) ( IEventHandler*, UINT* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfo      ) ( IEventHandler*, UINT, LCID, ITypeInfo** );
   HRESULT ( STDMETHODCALLTYPE * GetIDsOfNames    ) ( IEventHandler*, REFIID, LPOLESTR*, UINT, LCID, DISPID* );
   HRESULT ( STDMETHODCALLTYPE * Invoke           ) ( IEventHandler*, DISPID, REFIID, LCID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*, UINT* );
} IEventHandlerVtbl;
#endif

typedef struct
{
   IEventHandlerVtbl*      lpVtbl;
   int                     count;
   IConnectionPoint*       pIConnectionPoint;  /* Ref counted of course. */
   DWORD                   dwEventCookie;
   char*                   parent_on_invoke;
   IID                     device_event_interface_iid;
   PHB_ITEM                pSelf;              /* object to handle the events (optional) */
   PHB_ITEM                pEvents;
   int                     iID_riid;
} MyRealIEventHandler;

/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   if( IsEqualIID( vTableGuid, HB_ID_REF( IID_IUnknown ) ) )
   {
      *ppv = ( IUnknown * ) self;
      HB_VTBL( self )->AddRef( HB_THIS( self ) );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, HB_ID_REF( IID_IDispatch ) ) )
   {
      *ppv = ( IDispatch * ) self;
      HB_VTBL( self )->AddRef( HB_THIS( self ) );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, HB_ID_REF( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      if( ++( ( ( MyRealIEventHandler * ) self )->iID_riid ) == 1 )
      {
         *ppv = ( IDispatch* ) self;
         HB_VTBL( self )->AddRef( HB_THIS( self ) );
      }
      return S_OK;
   }
   *ppv = 0;
   return E_NOINTERFACE;
}

/*----------------------------------------------------------------------*/

static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   return ++( ( MyRealIEventHandler * ) self )->count;
}

/*----------------------------------------------------------------------*/

static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      if( ( ( MyRealIEventHandler * ) self )->pSelf )
         hb_itemRelease( ( ( MyRealIEventHandler * ) self )->pSelf );
#if 0
      if( ( MyRealIEventHandler * ) self )
         GlobalFree( ( MyRealIEventHandler * ) self );
#endif
      return ( ULONG ) 0;
   }
   return ( ULONG ) ( ( MyRealIEventHandler * ) self )->count;
}

/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );

   return ( HRESULT ) E_NOTIMPL;
}

/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );

   return ( HRESULT ) E_NOTIMPL;
}

/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE GetIDsOfNames( IEventHandler *self, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid )
{

   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgdispid );

   return ( HRESULT ) E_NOTIMPL;
}

/*----------------------------------------------------------------------*/

typedef struct
{
   PHB_ITEM item;
   VARIANT* variant;
}
HB_OLE_PARAM_REF;

/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE Invoke( IEventHandler *lpThis, DISPID dispid, REFIID riid,
                                         LCID lcid, WORD wFlags, DISPPARAMS* pParams,
                                         VARIANT* pVarResult, EXCEPINFO* pExcepInfo, UINT* puArgErr )
{
   int i, iCount, ii, iRefs;
   PHB_ITEM pAction, pKey = NULL;

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( pExcepInfo );
   HB_SYMBOL_UNUSED( puArgErr );

   Sleep( 10 );

   if( ! IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      return DISP_E_UNKNOWNINTERFACE;

   if( ! ( ( MyRealIEventHandler * ) lpThis )->pEvents )
      return S_OK;

   pAction = ( ( MyRealIEventHandler * ) lpThis )->pEvents;

   if( HB_IS_HASH( pAction ) )
   {
      pKey = hb_itemPutNL( pKey, ( long ) dispid );
      pAction = hb_hashGetItemPtr( pAction, pKey, 0 );
      hb_itemRelease( pKey );
   }

   if( pAction && hb_vmRequestReenter() )
   {
      HB_OLE_PARAM_REF refArray[ 32 ];

      iCount = pParams->cArgs;

      for( i = iRefs = 0; i < iCount && iRefs < 32; i++ )
      {
         if( V_VT( &pParams->rgvarg[ i ] ) & VT_BYREF )
            refArray[ iRefs++ ].item = hb_stackAllocItem();
      }

      hb_vmPushEvalSym();
      hb_vmPush( pAction );
      if( pKey == NULL )
         hb_vmPushLong( ( long ) dispid );

      for( i = 1, ii = 0; i <= iCount; i++ )
      {
         if( V_VT( &pParams->rgvarg[ iCount - i ] ) & VT_BYREF )
         {
            refArray[ ii ].variant = &pParams->rgvarg[ iCount - i ];
            hb_oleVariantToItem( refArray[ ii ].item, refArray[ ii ].variant );
            hb_vmPushItemRef( refArray[ ii++ ].item );
         }
         else
            hb_oleVariantToItem( hb_stackAllocItem(),
                                 &pParams->rgvarg[ iCount - i ] );
      }

      hb_vmSend( ( HB_USHORT ) ( iCount + ( pKey == NULL ? 1 : 0 ) ) );

      if( pVarResult )
         hb_oleItemToVariant( pVarResult, hb_stackReturnItem() );

      for( i = 0; i < iRefs; i++ )
         hb_oleVariantUpdate( refArray[ i ].variant, refArray[ i ].item, NULL );

      for( i = 0; i < iRefs; i++ )
         hb_stackPop();

      hb_vmRequestRestore();
   }

   return S_OK;
}

/*----------------------------------------------------------------------*/

static const IEventHandlerVtbl IEventHandler_Vtbl =
{
   QueryInterface,
   AddRef,
   Release,
   GetTypeInfoCount,
   GetTypeInfo,
   GetIDsOfNames,
   Invoke
};

/*----------------------------------------------------------------------*/

typedef IEventHandler device_interface;

HB_FUNC( WVG_AXSETUPCONNECTIONPOINT )
{
   HRESULT                     hr;
   MyRealIEventHandler*        hSink;
   device_interface*           pdevice_interface = ( device_interface * ) hb_oleParam( 1 );

   IConnectionPointContainer*  pCPC = NULL;
   IUnknown*                   pIUnknown;
   IConnectionPoint*           pCP = NULL;
   IEnumConnectionPoints*      pEnumCPs;
   IID                         rriid;
   IEventHandler*              thisobj;
   DWORD                       dwCookie = 0;

   thisobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) );
   if( thisobj )
   {
      ( ( MyRealIEventHandler * ) thisobj )->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;
      ( ( MyRealIEventHandler * ) thisobj )->pSelf = NULL;
      ( ( MyRealIEventHandler * ) thisobj )->count = 0;
      ( ( MyRealIEventHandler * ) thisobj )->iID_riid = 0;

      hr = HB_VTBL( thisobj )->QueryInterface( HB_THIS_( thisobj ) HB_ID_REF( IID_IUnknown ), (void **) (void*) &pIUnknown );
      if( hr == S_OK && pIUnknown )
      {
         hr = HB_VTBL( pdevice_interface )->QueryInterface( HB_THIS_( pdevice_interface ) HB_ID_REF( IID_IConnectionPointContainer ), (void**) (void*) &pCPC);
         if( hr == S_OK && pCPC )
         {
            hr = HB_VTBL( pCPC )->EnumConnectionPoints( HB_THIS_( pCPC ) &pEnumCPs );
            if( hr == S_OK && pEnumCPs )
            {
               do
               {
                  hr = HB_VTBL( pEnumCPs )->Next( HB_THIS_( pEnumCPs ) 1, &pCP, NULL );
                  if( hr == S_OK )
                  {
                     hr = HB_VTBL( pCP )->GetConnectionInterface( HB_THIS_( pCP ) &rriid );
                     if( hr == S_OK )
                     {
                        /**************           This has to be review         *******************
                               PellesC was generating GPF at this point
                               After commenting it out, I could not see any difference in objects
                               I play with. Cannot say why did I retained it so long.            */
                        #if 1
                        ( ( MyRealIEventHandler* ) thisobj )->device_event_interface_iid = rriid;
                        #endif

                        hr = HB_VTBL( pCP )->Advise( HB_THIS_( pCP ) pIUnknown, &dwCookie );
                        if( hr == S_OK )
                        {
                           ( ( MyRealIEventHandler* ) thisobj )->pIConnectionPoint = pCP;
                           ( ( MyRealIEventHandler* ) thisobj )->dwEventCookie     = dwCookie;
                        }
                        else
                           hr = S_OK;
                     }
                     else
                        hr = S_OK;
                  }
               } while( hr == S_OK );
               HB_VTBL( pEnumCPs )->Release( HB_THIS( pEnumCPs ) );
               pEnumCPs = NULL;
            }
            HB_VTBL( pCPC )->Release( HB_THIS( pCPC ) );
            pCPC = NULL;
         }
         HB_VTBL( pIUnknown )->Release( HB_THIS( pIUnknown ) );
         pIUnknown = NULL;
      }
   }
   else
      hr = E_OUTOFMEMORY;

   hSink = ( MyRealIEventHandler * ) thisobj;

   hSink->pEvents = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

   hb_storptr( hSink, 2 );
   hb_retnl( hr );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_AXSHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler* pSink = ( MyRealIEventHandler * ) hb_parptr( 1 );

   if( pSink && pSink->pIConnectionPoint )
   {
      HB_VTBL( pSink->pIConnectionPoint )->Unadvise( HB_THIS_( pSink->pIConnectionPoint ) pSink->dwEventCookie );
      HB_VTBL( pSink->pIConnectionPoint )->Release( HB_THIS( pSink->pIConnectionPoint ) );
      pSink->pIConnectionPoint = NULL;
      pSink->dwEventCookie = 0;
   }
   if( pSink && pSink->pEvents )
      hb_itemRelease( pSink->pEvents );

   /* in CPP mode following line GPFs on any OLE whose count becomes 0 */
   HB_VTBL( ( IEventHandler* ) pSink )->Release( HB_THIS( ( IEventHandler* ) pSink ) );
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_AXCREATEWINDOW ) /* ( hWndContainer, CLSID, menuID=0, x, y, w, h, style, exstyle ) --> pWnd */
{
   LPTSTR cCaption = HB_TCHAR_CONVTO( hb_parcx( 2 ) );

   hb_retptr( ( void * ) ( HB_PTRDIFF ) CreateWindowEx(
               hb_parnl( 9 ) /* Exstyle */,
               TEXT( "ATLAXWin" ),
               cCaption,
               hb_parnidef( 8, WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS ) /* Style */,
               hb_parni( 4 ) /* x */,
               hb_parni( 5 ) /* y */,
               hb_parni( 6 ) /* w */,
               hb_parni( 7 ) /* h */,
               ( HWND ) hb_parptr( 1 ) /* hParent */,
               HB_ISPOINTER( 3 ) ? ( HMENU ) hb_parptr( 3 ) : ( HMENU ) ( HB_PTRDIFF ) -1 /* id */,
               GetModuleHandle( NULL ),
               NULL ) );

   HB_TCHAR_FREE( cCaption );
}

/*----------------------------------------------------------------------*/
