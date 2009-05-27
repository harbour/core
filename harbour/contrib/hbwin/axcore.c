/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ActiveX core module
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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


#include "hbwinole.h"

#if defined( HB_OS_WIN_CE )
   #define HBTEXT( x ) TEXT( x )
#else
   #define HBTEXT( x ) x
#endif

typedef BOOL      ( CALLBACK * PHB_AX_WININIT )( void );
typedef BOOL      ( CALLBACK * PHB_AX_WINTERM )( void );
typedef HRESULT   ( CALLBACK * PHB_AX_GETCTRL )( HWND, IUnknown** );

static HMODULE s_hLib = NULL;

static PHB_AX_WINTERM   s_pAtlAxWinTerm = NULL;
static PHB_AX_GETCTRL   s_pAtlAxGetControl = NULL;


static void hb_ax_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_hLib )
   {
      ( *s_pAtlAxWinTerm )();

      s_pAtlAxWinTerm = NULL;
      s_pAtlAxWinTerm = NULL;

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


HB_FUNC( WIN_AXINIT )
{
   hb_retl( hb_ax_init() );
}


HB_FUNC( __AXGETCONTROL ) /* ( hWnd ) --> pDisp */
{
   IUnknown*   pUnk = NULL;
   IDispatch*  pDisp = NULL;
   HRESULT     lOleError;

   if( ! s_pAtlAxGetControl )
   {
      hb_oleSetError( S_OK );
      hb_errRT_BASE_SubstR( EG_UNSUPPORTED, 3012, "ActiveX not initialized", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   lOleError = ( *s_pAtlAxGetControl )( ( HWND ) hb_parptr( 1 ), &pUnk );

   if( lOleError == S_OK )
   {
      lOleError = HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IDispatch ), ( void** ) ( void * ) &pDisp );
      HB_VTBL( pUnk )->Release( HB_THIS( pUnk ) );
   }

   hb_oleSetError( lOleError );

   if( lOleError == S_OK )
      hb_itemReturnRelease( hb_oleItemPut( NULL, pDisp ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_AXRELEASEOBJECT )
{
   IDispatch * pDisp = hb_oleParam( 1 );
   HB_VTBL( pDisp )->Release( HB_THIS( pDisp ) );
}

/* ======================== Event handler support ======================== */

#if !defined( HB_OLE_C_API )
typedef struct
{
   HRESULT ( STDMETHODCALLTYPE * QueryInterface ) ( IDispatch*, REFIID, void** );
   ULONG   ( STDMETHODCALLTYPE * AddRef ) ( IDispatch* );
   ULONG   ( STDMETHODCALLTYPE * Release ) ( IDispatch* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfoCount ) ( IDispatch*, UINT* );
   HRESULT ( STDMETHODCALLTYPE * GetTypeInfo ) ( IDispatch*, UINT, LCID, ITypeInfo** );
   HRESULT ( STDMETHODCALLTYPE * GetIDsOfNames ) ( IDispatch*, REFIID, LPOLESTR*, UINT, LCID, DISPID* );
   HRESULT ( STDMETHODCALLTYPE * Invoke ) ( IDispatch*, DISPID, REFIID, LCID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*, UINT* );
} IDispatchVtbl;
#endif


typedef struct {
   IDispatchVtbl*          lpVtbl;
   DWORD                   count;
   IConnectionPoint*       pConnectionPoint;
   DWORD                   dwCookie;
   PHB_ITEM                pItemHandler;
} ISink;


static HB_GARBAGE_FUNC( hb_sink_destructor )
{
   ISink * pSink = ( ISink * ) Cargo;

   if( pSink->pItemHandler )
   {
      hb_itemRelease( pSink->pItemHandler );
      pSink->pItemHandler = NULL;
   }
   if( pSink->pConnectionPoint )
   {
      HB_VTBL( pSink->pConnectionPoint )->Unadvise( HB_THIS_( pSink->pConnectionPoint ) pSink->dwCookie );
      HB_VTBL( pSink->pConnectionPoint )->Release( HB_THIS( pSink->pConnectionPoint ) );
      pSink->pConnectionPoint = NULL;
      pSink->dwCookie = 0;
   }
   HB_VTBL( ( IDispatch* ) pSink )->Release( HB_THIS( ( IDispatch* ) pSink ) );
}


static HRESULT STDMETHODCALLTYPE QueryInterface( IDispatch* lpThis, REFIID riid, void** ppRet )
{
   if( IsEqualIID( riid, HB_ID_REF( IID_IUnknown ) ) || IsEqualIID( riid, HB_ID_REF( IID_IDispatch ) ) )
   {
      *ppRet = ( void* ) lpThis;
      HB_VTBL( lpThis )->AddRef( HB_THIS( lpThis ) );
      return S_OK;
   }
   *ppRet = NULL;
   return E_NOINTERFACE;
}


static ULONG STDMETHODCALLTYPE AddRef( IDispatch* lpThis )
{
   return ++( ( ISink* ) lpThis )->count;
}


static ULONG STDMETHODCALLTYPE Release( IDispatch* lpThis )
{
   if( --( ( ISink* ) lpThis )->count == 0 )
   {
      hb_xfree( lpThis );
      return 0;
   }
   return ( ( ISink* ) lpThis )->count;
}


static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IDispatch* lpThis, UINT* pInfoCount )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( pInfoCount );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE GetTypeInfo( IDispatch* lpThis, UINT iTInfo, LCID lcid, ITypeInfo** ppTypeInfo )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( iTInfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( ppTypeInfo );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE GetIDsOfNames( IDispatch* lpThis, REFIID riid, LPOLESTR* rgszNames, UINT cNames, LCID lcid, DISPID* rgDispId )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgDispId );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE Invoke( IDispatch* lpThis, DISPID dispid, REFIID riid,
                                         LCID lcid, WORD wFlags, DISPPARAMS* pParams,
                                         VARIANT* pVarResult, EXCEPINFO* pExcepInfo, UINT* puArgErr )
{
   PHB_ITEM  pItem;
   int       i, iCount;

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( pVarResult );
   HB_SYMBOL_UNUSED( pExcepInfo );
   HB_SYMBOL_UNUSED( puArgErr );

   if( ! IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      return DISP_E_UNKNOWNINTERFACE;

   if( ! ( ( ISink* ) lpThis)->pItemHandler )
      return S_OK;

   hb_vmPushState();
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( ( ( ISink* ) lpThis )->pItemHandler );
   hb_vmPushLong( dispid );

   iCount = pParams->cArgs;
   for( i = 1; i <= iCount; i++ )
   {
      pItem = hb_itemNew( NULL );
      hb_oleVariantToItem( pItem, &( pParams->rgvarg[ iCount - i ] ) );
      hb_vmPush( pItem );
      hb_itemRelease( pItem );
   }

   hb_vmDo( ( USHORT ) iCount + 1 );

   if( pVarResult )
      hb_oleItemToVariant( pVarResult, hb_stackReturnItem() );

   hb_vmPopState();
   return S_OK;
}


static const IDispatchVtbl ISink_Vtbl = {
   QueryInterface,
   AddRef,
   Release,
   GetTypeInfoCount,
   GetTypeInfo,
   GetIDsOfNames,
   Invoke
};


HB_FUNC( __AXREGISTERHANDLER )  /* ( pDisp, bHandler ) --> pSink */
{
   IDispatch * pDisp = hb_oleParam( 1 );

   if( pDisp )
   {
      PHB_ITEM pItemBlock = hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL );

      if( pItemBlock )
      {
         IConnectionPointContainer*  pCPC = NULL;
         IConnectionPoint*           pCP = NULL;
         HRESULT                     lOleError;

         lOleError = HB_VTBL( pDisp )->QueryInterface( HB_THIS_( pDisp ) HB_ID_REF( IID_IConnectionPointContainer ), ( void** ) ( void* ) &pCPC );
         if( lOleError == S_OK )
         {
            lOleError = HB_VTBL( pCPC )->FindConnectionPoint( HB_THIS_( pCPC ) HB_ID_REF( IID_IDispatch ), &pCP );
            if( lOleError == S_OK )
            {
               DWORD dwCookie = 0;
               ISink * pSink = ( ISink* ) hb_gcAlloc( sizeof( ISink ), hb_sink_destructor ); /* TODO: GlobalAlloc GMEM_FIXED ??? */
               pSink->lpVtbl = ( IDispatchVtbl * ) &ISink_Vtbl;
               pSink->count = 2; /* 1 for pCP->Advice() param and 1 for Harbour collectible pointer [Mindaugas] */
               pSink->pItemHandler = hb_itemNew( pItemBlock );

               lOleError = HB_VTBL( pCP )->Advise( HB_THIS_( pCP ) ( IUnknown* ) pSink, &dwCookie );
               pSink->pConnectionPoint = pCP;
               pSink->dwCookie = dwCookie;

               hb_retptrGC( pSink );
            }
            HB_VTBL( pCPC )->Release( HB_THIS( pCPC ) );
         }

         hb_oleSetError( lOleError );
         if( lOleError != S_OK )
            hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to obtain connection point", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
