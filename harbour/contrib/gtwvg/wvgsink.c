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

/*----------------------------------------------------------------------*/

static HMODULE  hLib = NULL;

typedef BOOL    ( CALLBACK *PATLAXWININIT )( void );
typedef BOOL    ( CALLBACK *PATLAXWINTERM )( void );
typedef HRESULT ( CALLBACK *PATLAXGETCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXATTACHCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXCREATECONTROL )( LPCOLESTR, HWND, IStream*, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXCREATECONTROLEX )( LPCOLESTR, HWND, IStream*, IUnknown**, IUnknown**, REFIID, IUnknown* );

/*----------------------------------------------------------------------*/

#if 0
#define __HBTOOUT__
#endif

#ifdef __HBTOOUT__
void hb_ToOutDebug( const char * sTraceMsg, ... );
#endif

/*----------------------------------------------------------------------*/

#if !defined( StringCchCat )
   #ifdef UNICODE
      #define StringCchCat(d,n,s)   hb_wcncpy( (d), (s), (n) - 1 )
   #else
      #define StringCchCat(d,n,s)   hb_xstrncpy( (d), (s), (n) - 1 )
   #endif
#endif

/*
 * This function copies szText to destination buffer.
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
char * hb_xstrncpy( char * pDest, const char * pSource, ULONG ulLen )
{
   ULONG ulDst, ulSrc;

   pDest[ ulLen ] = 0;
   ulDst = strlen( pDest );
   if( ulDst < ulLen )
   {
      ulSrc = strlen( pSource );
      if( ulDst + ulSrc > ulLen )
         ulSrc = ulLen - ulDst;

      memcpy( &pDest[ ulDst ], pSource, ulSrc );
      pDest[ ulDst + ulSrc ] = 0;
   }
   return pDest;
}


wchar_t *hb_wcncpy( wchar_t *dstW, const wchar_t *srcW, unsigned long ulLen )
{
   ULONG ulDst, ulSrc;

   dstW[ ulLen ] = 0;
   ulDst = lstrlenW( dstW );
   if( ulDst < ulLen )
   {
      ulSrc = lstrlenW( srcW );
      if( ulDst + ulSrc > ulLen )
         ulSrc = ulLen - ulDst;
      memcpy( &dstW[ ulDst ], srcW, ulSrc * sizeof( wchar_t ) );
      dstW[ ulDst + ulSrc ] = 0;
   }
   return dstW;
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
         {
            hb_vmPush( ( *pItems )[ ulParam ] );
         }
      }
   }
}

/*----------------------------------------------------------------------*/

#undef  INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_ ( INTERFACE, IDispatch )
{
   /* IUnknown functions */
   STDMETHOD  ( QueryInterface ) ( THIS_ REFIID, void ** ) PURE;
   STDMETHOD_ ( ULONG, AddRef )  ( THIS ) PURE;
   STDMETHOD_ ( ULONG, Release ) ( THIS ) PURE;
   /* IDispatch functions */
   STDMETHOD ( GetTypeInfoCount ) ( THIS_ UINT * ) PURE;
   STDMETHOD ( GetTypeInfo ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;
   STDMETHOD ( GetIDsOfNames ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;
   STDMETHOD ( Invoke ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};

#if !defined( HB_OLE_C_API )
typedef struct
{
   HRESULT ( STDMETHODCALLTYPE * QueryInterface ) ( IEventHandler*, REFIID, void** );
   ULONG   ( STDMETHODCALLTYPE * AddRef ) ( IEventHandler* );
   ULONG   ( STDMETHODCALLTYPE * Release ) ( IEventHandler* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfoCount ) ( IEventHandler*, UINT* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfo ) ( IEventHandler*,  UINT, LCID, ITypeInfo** );
   HRESULT ( STDMETHODCALLTYPE * GetIDsOfNames ) ( IEventHandler*, REFIID, LPOLESTR*, UINT, LCID, DISPID* );
   HRESULT ( STDMETHODCALLTYPE * Invoke ) ( IEventHandler*, DISPID, REFIID, LCID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*, UINT* );
} IEventHandlerVtbl;
#endif

typedef struct {
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
   return( E_NOINTERFACE );
}

static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   return( ++( ( MyRealIEventHandler * ) self )->count );
}


static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      if( ( ( MyRealIEventHandler * ) self)->pSelf )
      {
         hb_itemRelease( ( ( MyRealIEventHandler * ) self )->pSelf );
      }

      if( ( MyRealIEventHandler * ) self )
      {
         GlobalFree( ( MyRealIEventHandler * ) self );
      }
      return( ( ULONG ) 0 );
   }
   return( ( ULONG ) ( ( MyRealIEventHandler * ) self )->count );
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
         for( i = 1; i <= iArg; i++ )
         {
            pItem = hb_itemNew( NULL );
            hb_oleVariantToItem( pItem, &( params->rgvarg[ iArg-i ] ) );
            pItemArray[ i-1 ] = pItem;
            ulRefMask |= ( 1L << (i-1) );                                /* set bit i */
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
            if( HB_IS_BYREF( pItemArray[ iArg-i ] ) )
               hb_oleItemToVariant( &( params->rgvarg[ iArg-i ] ), pItemArray[ iArg-i ] );
         }

         /* Pritpal */
         if( iArg )
         {
            for( i = iArg; i > 0; i-- )
               hb_itemRelease( pItemArray[ i-1 ] );
         }
         hb_vmPopState();
      }
   }
   hb_itemRelease( Key );   /* Pritpal */

   return ( HRESULT ) S_OK;
}

/*----------------------------------------------------------------------*/

static const IEventHandlerVtbl IEventHandler_Vtbl = {
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

   thisobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) );
   if( !( thisobj ) )
   {
      hr = E_OUTOFMEMORY;
   }
   else
   {
      ( ( MyRealIEventHandler* ) thisobj)->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;
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

   *pThis = ( void * ) thisobj;

   return hr;
}
/*----------------------------------------------------------------------*/

HB_FUNC( HB_AX_SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler* hSink = ( MyRealIEventHandler * ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( hSink && hSink->pIConnectionPoint )
   {
      hSink->dwEventCookie = 0;
      HB_VTBL( hSink->pIConnectionPoint )->Release( HB_THIS( hSink->pIConnectionPoint ) );
      hSink->pIConnectionPoint = NULL;
   }

   if( hSink && hSink->pEvents )
   {
      hb_itemRelease( hSink->pEvents );
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( HB_AX_SETUPCONNECTIONPOINT )
{
   HRESULT              hr;
   MyRealIEventHandler* hSink = NULL;
   LPIID                riid  = ( LPIID ) &IID_IDispatch;
   int                  n = 0;

   hr = SetupConnectionPoint( ( device_interface* ) hb_oleParam( 1 ), ( REFIID ) riid, ( void** ) (void*) &hSink, &n ) ;

   hSink->pEvents = hb_itemNew( hb_param( 4, HB_IT_ANY ) );

   hb_stornint( ( HB_PTRDIFF ) hSink, 2 );
   hb_storni( n, 3 );
   hb_retnl( hr );
}

/*----------------------------------------------------------------------*/
/*                ActiveX Container Management Interface                */
/*----------------------------------------------------------------------*/
HB_FUNC( HB_AX_ATLAXWININIT )
{
   BOOL bRet = FALSE;

   if( !hLib )
   {
      PATLAXWININIT AtlAxWinInit;

      TCHAR szLibName[ MAX_PATH + 1 ] = { 0 };

      /* please always check if given function need size in TCHARs or bytes
       * in MS documentation.
       */
      GetSystemDirectory( szLibName, MAX_PATH );

      /* TEXT() macro can be used for literal (and only for literal) string
       * values. It creates array of TCHAR items with given text. It cannot
       * be used to ecapsulate non literal values. In different [x]Harbour
       * source code you may find things like TEXT( hb_parc( 1 ) ) - it's
       * a technical nonsense written by someone who has no idea what this
       * macro does.
       * Use new string functions (StringCchCat() in this case) which always
       * set trailing 0 in the given buffer just like hb_strn*() functions.
       * [l]str[n]cat() is absolute and should not be used by new code. It does
       * not guarantee buffer overflow protection and/or setting trailing 0.
       * StringCch*() functions operate on TCHAR types.
       */
      StringCchCat( szLibName, MAX_PATH + 1, TEXT( "\\atl.dll" ) );

       /* Please note that I intentionally removed any casting when szLibName
        * is passed to WinAPI functions. Such casting can pacify warnings so
        * program will be compiled but code will be still wrong so it does not
        * fix anything and only makes much harder later fixing when someone
        * will look for wrong code which is not UNICODE ready. The wrong casting
        * related to different character representations used only to pacify
        * warnings is the biggest problem in MS-Win 3-rd party code written
        * for [x]Harbour because it only hides bugs and then people have to
        * look for the code line by line to fix it. I dedicated above note to
        * developers of few well known MS-Win GUI projects for [x]Harbour.
        * Please remember about it.
        */
      hLib = LoadLibrary( szLibName );

      if( hLib )
      {
#if defined( UNICODE ) && defined( GetProcAddress )
         AtlAxWinInit = ( PATLAXWININIT ) GetProcAddressW( hLib, TEXT( "AtlAxWinInit" ) );
#else
         AtlAxWinInit = ( PATLAXWININIT ) GetProcAddress( hLib, "AtlAxWinInit" );
#endif
         if( AtlAxWinInit )
         {
            if( ( AtlAxWinInit )() )
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

   hb_retl( bRet );
}
/*----------------------------------------------------------------------*/

HB_FUNC( HB_AX_ATLAXWINTERM )
{
   PATLAXWINTERM AtlAxWinTerm;
   BOOL          bRet = FALSE;

   if( hLib )
   {
#if defined( UNICODE ) && defined( GetProcAddress )
      AtlAxWinTerm = ( PATLAXWINTERM ) GetProcAddressW( hLib, TEXT( "AtlAxWinTerm" ) );
#else
      AtlAxWinTerm = ( PATLAXWINTERM ) GetProcAddress( hLib, "AtlAxWinTerm" );
#endif
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

/*----------------------------------------------------------------------*/
/*
 *    ::hObj := HB_AX_AtlAxGetControl( "ATLAXWin", ::hContainer, ::CLSID, ::nID, ;
 *                   ::aPos[ 1 ], ::aPos[ 2 ], ::aSize[ 1 ], ::aSize[ 2 ], ::style, ::exStyle, @hx )
 */
HB_FUNC( HB_AX_ATLAXGETCONTROL ) /* HWND hWnd = handle of control container window */
{
   IUnknown  *pUnk = NULL;
   IDispatch *obj;
   PATLAXGETCONTROL AtlAxGetControl;
   RECT  rc;
   HWND  hWnd = NULL;
   char  *lpcclass = hb_parc( 1 );
   HWND  hParent   = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 2 );
   char  *Caption  = ISNIL(  4 ) ? "" : hb_parc( 3 );
   HMENU id        = ISNIL(  4 ) ? ( HMENU ) ( HB_PTRDIFF ) -1 : ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 4 );
   int   x         = ISNIL(  5 ) ? 0 : hb_parni( 5 );
   int   y         = ISNIL(  6 ) ? 0 : hb_parni( 6 );
   int   w         = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   int   h         = ISNIL(  8 ) ? 0 : hb_parni( 8 );
   int   Style     = ISNIL(  9 ) ? WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS : hb_parni( 9 );
   int   Exstyle   = ISNIL( 10 ) ? 0 : hb_parni( 10 );
   char *lpLic     = ISNIL( 11 ) ? NULL : hb_parc( 11 );

#if defined( UNICODE ) && defined( GetProcAddress )
   AtlAxGetControl = ( PATLAXGETCONTROL ) GetProcAddressW( hLib, TEXT( "AtlAxGetControl" ) );
#else
   AtlAxGetControl = ( PATLAXGETCONTROL ) GetProcAddress( hLib, "AtlAxGetControl" );
#endif
   if( AtlAxGetControl )
   {
      LPTSTR cCaption = HB_TCHAR_CONVTO( Caption );
      LPTSTR cClass = HB_TCHAR_CONVTO( lpcclass );
      LPTSTR cLic = HB_TCHAR_CONVTO( lpLic );

      if( ISNIL( 11 ) )
         hWnd = ( HWND ) CreateWindowEx( Exstyle, cClass, cCaption, Style, x, y, w, h, hParent, id,
                                      GetModuleHandle( NULL ), NULL );
      else
         hWnd = ( HWND ) CreateWindowEx( Exstyle, cClass, cCaption, Style, x, y, w, h, hParent, id,
                                      GetModuleHandle( NULL ), NULL /*cLic*/ );

      HB_TCHAR_FREE( cCaption );
      HB_TCHAR_FREE( cClass );
      HB_TCHAR_FREE( cLic );

      if( hWnd )
      {
#if ! defined( HB_OS_WIN_CE )
         SendMessage( hWnd,
                      ( ( UINT ) WM_SETFONT ),
                      ( ( WPARAM ) GetStockObject( DEFAULT_GUI_FONT ) ),
                      ( ( LPARAM ) ( MAKELPARAM( FALSE, 0 ) ) ) );
#else
         SendMessage( hWnd,
                      ( ( UINT ) WM_SETFONT ),
                      ( ( WPARAM ) GetStockObject( OEM_FIXED_FONT ) ),
                      ( ( LPARAM ) ( MAKELPARAM( FALSE, 0 ) ) ) );
#endif
         ( AtlAxGetControl )( hWnd, &pUnk );

         if( pUnk )
         {
            HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IDispatch ), ( void** ) (void*) &obj );
            HB_VTBL( pUnk )->Release( HB_THIS( pUnk ) );
            GetClientRect( hWnd, &rc );
            MoveWindow( hWnd, 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
            hb_itemReturnRelease( hb_oleItemPut( NULL, obj ) );
         }
         else
         {
            hb_ret();
         }
      }
      else
      {
         hb_ret();
      }
   }
   else
   {
      hb_ret();
   }

   /* return the control handle */
   if ISBYREF( 12 )
   {
      hb_stornint( ( HB_PTRDIFF ) hWnd, 12 );
   }
   if ISBYREF( 13 )
   {
      hb_stornint( ( HB_PTRDIFF ) pUnk, 13 );
   }
}

/*----------------------------------------------------------------------*/

HB_FUNC( HB_AX_ATLSETVERB )
{
   HWND hwnd = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( hwnd )
   {
      IUnknown *pUnk = ( IUnknown* ) ( HB_PTRDIFF ) hb_parnint( 2 );

      IOleObject *lpOleObject = NULL;
      if( SUCCEEDED( HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IOleObject ), ( void** ) ( void* ) &lpOleObject ) ) )
      {
         IOleClientSite* lpOleClientSite;

         HB_VTBL( pUnk )->Release( HB_THIS( pUnk ) );

         if( SUCCEEDED( HB_VTBL( lpOleObject )->GetClientSite( HB_THIS_( lpOleObject ) &lpOleClientSite ) ) )
         {
            MSG Msg;
            RECT rct;

            memset( &Msg, 0, sizeof( MSG ) );
            GetClientRect( hwnd, &rct );

            HB_VTBL( lpOleObject )->DoVerb( HB_THIS_( lpOleObject ) hb_parni( 3 ), &Msg, lpOleClientSite, 0, hwnd, &rct );
         }
      }
   }
}

/*----------------------------------------------------------------------*/
