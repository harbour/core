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

#if defined(__BORLANDC__) && !defined(HB_ARCH_64BIT)
    #undef MAKELONG
    #define MAKELONG(a,b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | \
                          (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

#if defined( HB_OS_WIN_CE )
   #define HBTEXT( x ) TEXT( x )
#else
   #define HBTEXT( x ) x
#endif

#if 0
#define __HBTOOUT__
#endif
#ifdef __HBTOOUT__
void extern hb_ToOutDebug( const char * sTraceMsg, ... );
#endif

/*----------------------------------------------------------------------*/

static HMODULE  s_hLib = NULL;

typedef BOOL      ( CALLBACK * PHB_AX_WININIT )( void );
typedef BOOL      ( CALLBACK * PHB_AX_WINTERM )( void );
typedef HRESULT   ( CALLBACK * PHB_AX_GETCTRL )( HWND, IUnknown** );

static PHB_AX_WINTERM   s_pAtlAxWinTerm = NULL;
static PHB_AX_GETCTRL   s_pAtlAxGetControl = NULL;

/*----------------------------------------------------------------------*/

static void hb_ax_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_hLib )
   {
#if 0
hb_ToOutDebug( "Terminating 1" );
#endif
      if( s_pAtlAxWinTerm )
      {
#if 0
hb_ToOutDebug( "Terminating 2" );
#endif
         if( ( *s_pAtlAxWinTerm ) () )
         {
#if 0
hb_ToOutDebug( "Terminating 3" );
#endif
            s_pAtlAxWinTerm = NULL;
            s_pAtlAxGetControl = NULL;
         }
      }
      FreeLibrary( s_hLib );
      s_hLib = NULL;
   }
}

static int hb_ax_init( void )
{
   if( s_hLib == NULL )
   {
      PHB_AX_WININIT pAtlAxWinInit;

      s_hLib = LoadLibrary( TEXT( "atl.dll" ) );
      if( ( unsigned long ) s_hLib <= 32 )
      {
         s_hLib = NULL;
         return 0;
      }
      pAtlAxWinInit      = ( PHB_AX_WININIT ) GetProcAddress( s_hLib, HBTEXT( "AtlAxWinInit" ) );
      s_pAtlAxWinTerm    = ( PHB_AX_WINTERM ) GetProcAddress( s_hLib, HBTEXT( "AtlAxWinTerm" ) );
      s_pAtlAxGetControl = ( PHB_AX_GETCTRL ) GetProcAddress( s_hLib, HBTEXT( "AtlAxGetControl" ) );
      if( pAtlAxWinInit )
         ( *pAtlAxWinInit )();

      hb_vmAtQuit( hb_ax_exit, NULL );
   }
   return 1;
}


HB_FUNC( WVG_AXINIT )
{
   hb_retl( hb_ax_init() );
}

/*----------------------------------------------------------------------*/

static void hb_itemPushList( ULONG ulRefMask, ULONG ulPCount, PHB_ITEM** pItems )
{
   HB_ITEM itmRef;
   ULONG   ulParam;

   if( ulPCount )
   {
      /* initialize the reference item */
      itmRef.type = HB_IT_BYREF;
      itmRef.item.asRefer.offset = -1;
      itmRef.item.asRefer.BasePtr.itemsbasePtr = pItems;

      for( ulParam = 0; ulParam < ulPCount; ulParam++ )
      {
         if( ulRefMask & ( 1L << ulParam ) )
         {
            /* when item is passed by reference then we have to put
               the reference on the stack instead of the item itself */
            itmRef.item.asRefer.value = ulParam+1;
            hb_vmPush( &itmRef );
         }
         else
            hb_vmPush( ( *pItems )[ ulParam ] );
      }
   }
}

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
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfo      ) ( IEventHandler*,  UINT, LCID, ITypeInfo** );
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
/*               Here are IEventHandler's functions.                    */
/*----------------------------------------------------------------------*/

static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   if( IsEqualIID( vTableGuid, HB_ID_REF( IID_IUnknown ) ) )
   {
      *ppv = ( IUnknown * ) self;
#ifdef __HBTOOUT__
hb_ToOutDebug( "..................if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IUnknown ) ) )" );
#endif
      HB_VTBL( self )->AddRef( HB_THIS( self ) );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, HB_ID_REF( IID_IDispatch ) ) )
   {
      *ppv = ( IDispatch * ) self;
#ifdef __HBTOOUT__
hb_ToOutDebug( "..................if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IDispatch ) ) )" );
#endif
      HB_VTBL( self )->AddRef( HB_THIS( self ) );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, HB_ID_REF( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      if( ++( ( ( MyRealIEventHandler * ) self )->iID_riid ) == 1 )
      {
         *ppv = ( IDispatch* ) self;
#ifdef __HBTOOUT__
hb_ToOutDebug( "..................if ( IsEqualIID( vTableGuid, HB_ID_REF( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )" );
#endif
         HB_VTBL( self )->AddRef( HB_THIS( self ) );
      }
      return S_OK;
   }
   *ppv = 0;
   return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   return ++( ( MyRealIEventHandler * ) self )->count;
}


static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
#ifdef __HBTOOUT__
hb_ToOutDebug( "Release %i", ( ( MyRealIEventHandler * ) self )->count );
#endif

   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      if( ( ( MyRealIEventHandler * ) self)->pSelf )
         hb_itemRelease( ( ( MyRealIEventHandler * ) self )->pSelf );

      if( ( MyRealIEventHandler * ) self )
         GlobalFree( ( MyRealIEventHandler * ) self );

      return ( ULONG ) 0;
   }
   else
      return ( ULONG ) ( ( MyRealIEventHandler * ) self )->count;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );

   return ( HRESULT ) E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );

   return ( HRESULT ) E_NOTIMPL;
}

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

static HRESULT STDMETHODCALLTYPE Invoke( IEventHandler *self, DISPID dispid, REFIID riid,
                                      LCID lcid, WORD wFlags, DISPPARAMS *params,
                                      VARIANT *result, EXCEPINFO *pexcepinfo, UINT *puArgErr )
{
   int        iArg;
   int        i;
   ULONG      ulRefMask = 0;
   ULONG      ulPos;
   PHB_ITEM   pItem;
   PHB_ITEM   pItemArray[ 32 ]; /* max 32 parameters? */
   PHB_ITEM   *pItems;
   PHB_ITEM   Key;

#if 0
hb_ToOutDebug( "event = %i",(int)dispid );
#endif

   /* We implement only a "default" interface */
   if( !IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      return( ( HRESULT ) DISP_E_UNKNOWNINTERFACE );

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   /* Don't know but this speed ups the OLE load time */
   Sleep( 10 );

   Key = hb_itemNew( NULL );
   if( hb_hashScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) self )->pEvents, ulPos );
      PHB_ITEM pExec  = hb_arrayGetItemPtr( pArray, 1 );

      if( pExec )
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
               if( HB_IS_OBJECT( pObject ) )
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

         if( iArg > ( int ) HB_SIZEOFARRAY( pItemArray ) )
            iArg = HB_SIZEOFARRAY( pItemArray );

         for( i = 1; i <= iArg; i++ )
         {
            pItem = hb_itemNew( NULL );
            hb_oleVariantToItem( pItem, &( params->rgvarg[ iArg - i ] ) );
            pItemArray[ i - 1 ] = pItem;
            ulRefMask |= ( 1L << ( i - 1 ) );                                /* set bit i */
         }

         if( iArg )
         {
            pItems = pItemArray;
            hb_itemPushList( ulRefMask, iArg, &pItems );
         }

         /* execute */
         hb_vmDo( ( USHORT ) iArg );

         for( i = iArg; i > 0; i-- )
         {
            if( HB_IS_BYREF( pItemArray[ iArg - i ] ) )
               hb_oleItemToVariant( &( params->rgvarg[ iArg - i ] ), pItemArray[ iArg - i ] );
         }

         /* Pritpal */
         if( iArg )
         {
            for( i = iArg; i > 0; i-- )
               hb_itemRelease( pItemArray[ i - 1 ] );
         }
         hb_vmPopState();
      }
   }
   hb_itemRelease( Key );   /* Pritpal */

   return ( HRESULT ) S_OK;
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

typedef IEventHandler device_interface;

/*----------------------------------------------------------------------*/

static HRESULT SetupConnectionPoint( device_interface* pdevice_interface, REFIID riid, void** pThis )
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
         hr = HB_VTBL( pdevice_interface )->QueryInterface( HB_THIS_( pdevice_interface ) HB_ID_REF( IID_IConnectionPointContainer ), (void**) (void*) &pIConnectionPointContainerTemp);
         if( hr == S_OK && pIConnectionPointContainerTemp )
         {
            hr = HB_VTBL( pIConnectionPointContainerTemp )->EnumConnectionPoints( HB_THIS_( pIConnectionPointContainerTemp ) &m_pIEnumConnectionPoints );
            if( hr == S_OK && m_pIEnumConnectionPoints )
            {
               do
               {
                  hr = HB_VTBL( m_pIEnumConnectionPoints )->Next( HB_THIS_( m_pIEnumConnectionPoints ) 1, &m_pIConnectionPoint, NULL );
                  if( hr == S_OK )
                  {
                     hr = HB_VTBL( m_pIConnectionPoint )->GetConnectionInterface( HB_THIS_( m_pIConnectionPoint ) &rriid );
                     if( hr == S_OK )
                     {
                        /**************           This has to be review         *******************
                               PellesC was generating GPF at this point
                               After commenting it out, I could not see any difference in objects
                               I play with. Cannot say why did I retained it so long.            */
                        #if 1
                        ( ( MyRealIEventHandler* ) thisobj )->device_event_interface_iid = rriid;
                        #endif

                        hr = HB_VTBL( m_pIConnectionPoint )->Advise( HB_THIS_( m_pIConnectionPoint ) pIUnknown, &dwCookie );
                        if( hr == S_OK )
                        {
                           ( ( MyRealIEventHandler* ) thisobj )->pIConnectionPoint = m_pIConnectionPoint;
                           ( ( MyRealIEventHandler* ) thisobj )->dwEventCookie     = dwCookie;
                        }
                        else
                           hr = S_OK;
                     }
                     else
                        hr = S_OK;
                  }
               } while( hr == S_OK );
               HB_VTBL( m_pIEnumConnectionPoints )->Release( HB_THIS( m_pIEnumConnectionPoints ) );
               m_pIEnumConnectionPoints = NULL;
            }
            HB_VTBL( pIConnectionPointContainerTemp )->Release( HB_THIS( pIConnectionPointContainerTemp ) );
            pIConnectionPointContainerTemp = NULL;
         }
         HB_VTBL( pIUnknown )->Release( HB_THIS( pIUnknown ) );
         pIUnknown = NULL;
      }
   }
   else
      hr = E_OUTOFMEMORY;

   *pThis = ( void * ) thisobj;

   return hr;
}
/*----------------------------------------------------------------------*/

HB_FUNC( WVG_AXSETUPCONNECTIONPOINT )
{
   HRESULT              hr;
   MyRealIEventHandler* hSink = NULL;
   LPIID                riid  = ( LPIID ) &IID_IDispatch;

   hr = SetupConnectionPoint( ( device_interface* ) hb_oleParam( 1 ), ( REFIID ) riid, ( void** ) (void*) &hSink ) ;

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
               HB_ISNUM( 9 ) ? hb_parnl( 9 ) : 0 /* Exstyle */,
               TEXT( "ATLAXWin" ),
               cCaption,
               HB_ISNUM( 8 ) ? hb_parni( 8 ) : WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS /* Style */,
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

HB_FUNC( WVG_AXGETCONTROL ) /* HWND hWnd = handle of control container window */
{
   IDispatch   *obj;
   IUnknown    *pUnk = NULL;
   HWND        hWnd = ( HWND ) hb_parptr( 1 );

   if( ! s_pAtlAxGetControl )
   {
      hb_oleSetError( S_OK );
      hb_errRT_BASE_SubstR( EG_UNSUPPORTED, 3012, "ActiveX not initialized", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( hWnd )
   {
      ( *s_pAtlAxGetControl )( hWnd, &pUnk );

      if( pUnk )
      {
         HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IDispatch ), ( void** ) (void*) &obj );
         HB_VTBL( pUnk )->Release( HB_THIS( pUnk ) );

         hb_itemReturnRelease( hb_oleItemPut( NULL, obj ) );
      }
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( WVG_AXDOVERB ) /* ( hWndAx, iVerb ) --> hResult */
{
   HWND        hWnd = ( HWND ) hb_parptr( 1 );
   IUnknown*   pUnk = NULL;
   HRESULT     lOleError;

   if( ! s_pAtlAxGetControl )
   {
      hb_oleSetError( S_OK );
      hb_errRT_BASE_SubstR( EG_UNSUPPORTED, 3012, "ActiveX not initialized", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   lOleError = ( *s_pAtlAxGetControl )( hWnd, &pUnk );

   if( lOleError == S_OK )
   {
      IOleObject *lpOleObject = NULL;

      lOleError = HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IOleObject ), ( void** ) ( void* ) &lpOleObject );
      if( lOleError == S_OK )
      {
         IOleClientSite* lpOleClientSite;

         lOleError = HB_VTBL( lpOleObject )->GetClientSite( HB_THIS_( lpOleObject ) &lpOleClientSite );
         if( lOleError == S_OK )
         {
            MSG Msg;
            RECT rct;

            memset( &Msg, 0, sizeof( MSG ) );
            GetClientRect( hWnd, &rct );
            HB_VTBL( lpOleObject )->DoVerb( HB_THIS_( lpOleObject ) hb_parni( 2 ), &Msg, lpOleClientSite, 0, hWnd, &rct );
         }
      }
   }

   hb_oleSetError( lOleError );

   hb_retnl( lOleError );
}

/*----------------------------------------------------------------------*/
