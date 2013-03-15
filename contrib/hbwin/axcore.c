/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ActiveX core module
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbwapi.h"
#include "hbwinole.h"
#include <olectl.h>

#include "hbapistr.h"
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

typedef BOOL      ( CALLBACK * PHB_AX_WININIT )( void );
typedef HRESULT   ( CALLBACK * PHB_AX_GETCTRL )( HWND, IUnknown** );

static HMODULE s_hLib = NULL;

static PHB_AX_GETCTRL s_pAtlAxGetControl = NULL;


static void hb_errRT_OLE( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode, const char * szDescription, const char * szOperation )
{
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "WINOLE", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );

   if( hb_pcount() != 0 )
   {
      /* HB_ERR_ARGS_BASEPARAMS */
      PHB_ITEM pArray = hb_arrayBaseParams();
      hb_errPutArgsArray( pError, pArray );
      hb_itemRelease( pArray );
   }
   hb_errLaunch( pError );
   hb_errRelease( pError );
}


static void hb_oleAxExit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_hLib )
   {
      s_pAtlAxGetControl = NULL;

      FreeLibrary( s_hLib );

      s_hLib = NULL;
   }
}

HB_BOOL hb_oleAxInit( void )
{
   hb_oleInit();

   if( s_hLib == NULL )
   {
      PHB_AX_WININIT pAtlAxWinInit;

      s_hLib = hbwapi_LoadLibrarySystem( TEXT( "atl.dll" ) );
      if( ( unsigned long ) ( HB_PTRDIFF ) s_hLib <= 32 )
      {
         s_hLib = NULL;
         return HB_FALSE;
      }
      pAtlAxWinInit      = ( PHB_AX_WININIT ) GetProcAddress( s_hLib, "AtlAxWinInit" );
      s_pAtlAxGetControl = ( PHB_AX_GETCTRL ) GetProcAddress( s_hLib, "AtlAxGetControl" );

      if( pAtlAxWinInit )
         ( *pAtlAxWinInit )();

      hb_vmAtQuit( hb_oleAxExit, NULL );
   }
   return HB_TRUE;
}


HB_FUNC( WIN_AXINIT )
{
   hb_retl( hb_oleAxInit() );
}


PHB_ITEM hb_oleAxControlNew( PHB_ITEM pItem, HWND hWnd )
{
   IUnknown *  pUnk  = NULL;
   IDispatch * pDisp = NULL;
   HRESULT     lOleError;

   if( pItem )
      hb_itemClear( pItem );

   if( ! hb_oleAxInit() || ! s_pAtlAxGetControl )
   {
      hb_oleSetError( S_OK );
      hb_errRT_OLE( EG_UNSUPPORTED, 1010, 0, "ActiveX not initialized", HB_ERR_FUNCNAME );
   }
   else
   {
      lOleError = ( *s_pAtlAxGetControl )( hWnd, &pUnk );

      if( lOleError == S_OK )
      {
         lOleError = HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IDispatch ), ( void ** ) ( void * ) &pDisp );

         if( lOleError == S_OK )
            pItem = hb_oleItemPut( pItem, pDisp );

         HB_VTBL( pUnk )->Release( HB_THIS( pUnk ) );
      }

      hb_oleSetError( lOleError );

      if( lOleError != S_OK )
         hb_errRT_OLE( EG_ARG, 1011, ( HB_ERRCODE ) lOleError, NULL, HB_ERR_FUNCNAME );
   }

   return pItem;
}


HB_FUNC( __AXGETCONTROL ) /* ( hWnd ) --> pDisp */
{
   HWND hWnd = ( HWND ) hb_parptr( 1 );

   if( ! hWnd )
      hb_errRT_OLE( EG_ARG, 1012, 0, NULL, HB_ERR_FUNCNAME );
   else
      hb_oleAxControlNew( hb_stackReturnItem(), hWnd );
}

HB_FUNC( __AXDOVERB ) /* ( hWndAx, iVerb ) --> hResult */
{
   HWND       hWnd = ( HWND ) hb_parptr( 1 );
   IUnknown * pUnk = NULL;
   HRESULT    lOleError;

   if( ! hb_oleAxInit() || ! s_pAtlAxGetControl )
   {
      hb_oleSetError( S_OK );
      hb_errRT_OLE( EG_UNSUPPORTED, 1013, 0, "ActiveX not initialized", HB_ERR_FUNCNAME );
      return;
   }

   lOleError = ( *s_pAtlAxGetControl )( hWnd, &pUnk );

   if( lOleError == S_OK )
   {
      IOleObject * lpOleObject = NULL;

      lOleError = HB_VTBL( pUnk )->QueryInterface( HB_THIS_( pUnk ) HB_ID_REF( IID_IOleObject ), ( void ** ) ( void * ) &lpOleObject );
      if( lOleError == S_OK )
      {
         IOleClientSite * lpOleClientSite;

         lOleError = HB_VTBL( lpOleObject )->GetClientSite( HB_THIS_( lpOleObject ) & lpOleClientSite );
         if( lOleError == S_OK )
         {
            MSG Msg;
            RECT rct;

            memset( &Msg, 0, sizeof( Msg ) );
            GetClientRect( hWnd, &rct );
            HB_VTBL( lpOleObject )->DoVerb( HB_THIS_( lpOleObject ) hb_parni( 2 ), &Msg, lpOleClientSite, 0, hWnd, &rct );
         }
         HB_VTBL( lpOleObject )->Release( HB_THIS( lpOleObject ) );
      }
   }

   hb_oleSetError( lOleError );

   hb_retnl( lOleError );
}


/* ======================== Event handler support ======================== */


#if ! defined( HB_OLE_C_API )
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


typedef struct
{
   const IDispatchVtbl * lpVtbl;
   DWORD                 count;
   IConnectionPoint *    pConnectionPoint;
   DWORD                 dwCookie;
   IID                   rriid;
   PHB_ITEM              pItemHandler;
   HB_USHORT             uiClass;
} ISink;


static HRESULT STDMETHODCALLTYPE QueryInterface( IDispatch * lpThis, REFIID riid, void ** ppRet )
{
   if( IsEqualIID( riid, HB_ID_REF( IID_IUnknown ) ) ||
       IsEqualIID( riid, HB_ID_REF( IID_IDispatch ) ) ||
       IsEqualIID( riid, HB_ID_REF( ( ( ISink * ) lpThis )->rriid ) ) )
   {
      *ppRet = ( void * ) lpThis;
      HB_VTBL( lpThis )->AddRef( HB_THIS( lpThis ) );
      return S_OK;
   }
   *ppRet = NULL;
   return E_NOINTERFACE;
}


static ULONG STDMETHODCALLTYPE AddRef( IDispatch * lpThis )
{
   return ++( ( ISink * ) lpThis )->count;
}


static ULONG STDMETHODCALLTYPE Release( IDispatch * lpThis )
{
   ISink * pSink = ( ISink * ) lpThis;

   if( --pSink->count == 0 )
   {
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
      hb_xfree( pSink );      /* TODO: GlobalAlloc/Free GMEM_FIXED ??? */
      return 0;
   }
   return pSink->count;
}


static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IDispatch * lpThis, UINT * pInfoCount )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( pInfoCount );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE GetTypeInfo( IDispatch * lpThis, UINT iTInfo, LCID lcid, ITypeInfo ** ppTypeInfo )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( iTInfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( ppTypeInfo );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE GetIDsOfNames( IDispatch * lpThis, REFIID riid, LPOLESTR * rgszNames, UINT cNames, LCID lcid, DISPID * rgDispId )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgDispId );
   return E_NOTIMPL;
}


static HRESULT STDMETHODCALLTYPE Invoke( IDispatch * lpThis, DISPID dispid, REFIID riid,
                                         LCID lcid, WORD wFlags, DISPPARAMS * pParams,
                                         VARIANT * pVarResult, EXCEPINFO * pExcepInfo,
                                         UINT * puArgErr )
{
   PHB_ITEM pAction;
   HRESULT hr;

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( pExcepInfo );
   HB_SYMBOL_UNUSED( puArgErr );

   if( ! IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      return DISP_E_UNKNOWNINTERFACE;

   hr = DISP_E_MEMBERNOTFOUND;

   pAction = ( ( ISink * ) lpThis )->pItemHandler;
   if( pAction )
   {
      PHB_ITEM pKey = hb_itemPutNL( hb_stackAllocItem(), ( long ) dispid );

      if( pAction && HB_IS_HASH( pAction ) )
      {
         pAction = hb_hashGetItemPtr( pAction, pKey, 0 );
         pKey = NULL;
      }

      if( pAction && hb_oleDispInvoke( NULL, pAction, pKey,
                                       pParams, pVarResult, NULL,
                                       ( ( ISink * ) lpThis )->uiClass ) )
         hr = S_OK;

      hb_stackPop();
   }

   return hr;
}


static const IDispatchVtbl ISink_Vtbl =
{
   QueryInterface,
   AddRef,
   Release,
   GetTypeInfoCount,
   GetTypeInfo,
   GetIDsOfNames,
   Invoke
};


#if 0
/* Debug helper function */
static char * GUID2String( GUID * pID )
{
   static char strguid[ 128 ];
   wchar_t     olestr[ 128 ];
   int         iLen;

   StringFromGUID2( pID, olestr, HB_SIZEOFARRAY( olestr ) );
   iLen = WideCharToMultiByte( CP_ACP, 0, olestr, -1, strguid, sizeof( strguid ), NULL, NULL );
   if( iLen )
      strguid[ iLen - 1 ] = 0;
   return strguid;
}
#endif

static HRESULT _get_default_sink( IDispatch * iDisp, const char * szEvent, IID * piid )
{
   ITypeInfo * iTI;
   ITypeInfo * iTISink;
   TYPEATTR *  pTypeAttr;
   HREFTYPE    hRefType;
   HRESULT     hr;
   int         iFlags, i, j;

   if( ! szEvent )
   {
      IProvideClassInfo2 * iPCI2;
      IProvideClassInfo *  iPCI;

      /* Method 1: using IProvideClassInfo2 */

      hr = HB_VTBL( iDisp )->QueryInterface( HB_THIS_( iDisp ) HB_ID_REF( IID_IProvideClassInfo2 ), ( void ** ) ( void * ) &iPCI2 );
      if( hr == S_OK )
      {
         HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink IProvideClassInfo2 OK" ) );
         hr = HB_VTBL( iPCI2 )->GetGUID( HB_THIS_( iPCI2 ) GUIDKIND_DEFAULT_SOURCE_DISP_IID, piid );
         HB_VTBL( iPCI2 )->Release( HB_THIS( iPCI2 ) );

         if( hr == S_OK )
            return S_OK;
      }
      else
         HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink IProvideClassInfo2 obtain error %08lX", hr ) );


      /* Method 2: using IProvideClassInfo and searching for default source in ITypeInfo */

      hr = HB_VTBL( iDisp )->QueryInterface( HB_THIS_( iDisp ) HB_ID_REF( IID_IProvideClassInfo ), ( void ** ) ( void * ) &iPCI );
      if( hr == S_OK )
      {
         HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink IProvideClassInfo OK" ) );

         hr = HB_VTBL( iPCI )->GetClassInfo( HB_THIS_( iPCI ) & iTI );
         if( hr == S_OK )
         {
            hr = HB_VTBL( iTI )->GetTypeAttr( HB_THIS_( iTI ) & pTypeAttr );
            if( hr == S_OK )
            {
               for( i = 0; i < pTypeAttr->cImplTypes; i++ )
               {
                  hr = HB_VTBL( iTI )->GetImplTypeFlags( HB_THIS_( iTI ) i, &iFlags );
                  if( hr == S_OK && ( iFlags & IMPLTYPEFLAG_FDEFAULT ) && ( iFlags & IMPLTYPEFLAG_FSOURCE ) )
                  {
                     if( HB_VTBL( iTI )->GetRefTypeOfImplType( HB_THIS_( iTI ) i, &hRefType ) == S_OK &&
                         HB_VTBL( iTI )->GetRefTypeInfo( HB_THIS_( iTI ) hRefType, &iTISink ) == S_OK )
                     {
                        HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink Method 2: default source is found" ) );

                        hr = HB_VTBL( iTISink )->GetTypeAttr( HB_THIS_( iTISink ) & pTypeAttr );
                        if( hr == S_OK )
                        {
                           *piid = pTypeAttr->guid;
                           HB_VTBL( iTISink )->ReleaseTypeAttr( HB_THIS_( iTISink ) pTypeAttr );

                           HB_VTBL( iTI )->ReleaseTypeAttr( HB_THIS_( iTI ) pTypeAttr );
                           HB_VTBL( iPCI )->Release( HB_THIS( iPCI ) );
                           return S_OK;
                        }
                     }
                  }
               }
               HB_VTBL( iTI )->ReleaseTypeAttr( HB_THIS_( iTI ) pTypeAttr );
            }
         }
         HB_VTBL( iPCI )->Release( HB_THIS( iPCI ) );
      }
      else
         HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink IProvideClassInfo obtain error %08lX", hr ) );
   }


   /* Method 3: using CoClass */

   hr = HB_VTBL( iDisp )->GetTypeInfo( HB_THIS_( iDisp ) 0, LOCALE_SYSTEM_DEFAULT, &iTI );
   if( hr == S_OK )
   {
      ITypeLib * iTL;
      TYPEATTR * pTypeAttr2;

      hr = HB_VTBL( iTI )->GetContainingTypeLib( HB_THIS_( iTI ) & iTL, NULL );
      HB_VTBL( iTI )->Release( HB_THIS( iTI ) );

      if( hr == S_OK )
      {
         int iCount = HB_VTBL( iTL )->GetTypeInfoCount( HB_THIS( iTL ) );
         for( i = 0; i < iCount; i++ )
         {
            hr = HB_VTBL( iTL )->GetTypeInfo( HB_THIS_( iTL ) i, &iTI );
            if( hr == S_OK )
            {
               hr = HB_VTBL( iTI )->GetTypeAttr( HB_THIS_( iTI ) & pTypeAttr );
               if( hr == S_OK )
               {
                  if( pTypeAttr->typekind == TKIND_COCLASS )
                  {
                     for( j = 0; j < pTypeAttr->cImplTypes; j++ )
                     {
                        if( szEvent )
                        {
                           if( HB_VTBL( iTI )->GetRefTypeOfImplType( HB_THIS_( iTI ) j, &hRefType ) == S_OK &&
                               HB_VTBL( iTI )->GetRefTypeInfo( HB_THIS_( iTI ) hRefType, &iTISink ) == S_OK )
                           {
                              BSTR bstr;

                              hr = HB_VTBL( iTISink )->GetDocumentation( HB_THIS_( iTISink ) - 1, &bstr, NULL, NULL, NULL );
                              if( hr == S_OK )
                              {
                                 char str[ 256 ];
                                 int  iLen;

                                 iLen = WideCharToMultiByte( CP_ACP, 0, bstr, -1, str, sizeof( str ), NULL, NULL );
                                 str[ iLen - 1 ] = '\0';
                                 if( ! strcmp( szEvent, str ) )
                                 {
                                    hr = HB_VTBL( iTISink )->GetTypeAttr( HB_THIS_( iTISink ) & pTypeAttr2 );
                                    if( hr == S_OK )
                                    {
                                       *piid = pTypeAttr2->guid;
                                       HB_VTBL( iTISink )->ReleaseTypeAttr( HB_THIS_( iTISink ) pTypeAttr2 );

                                       HB_VTBL( iTISink )->Release( HB_THIS( iTISink ) );
                                       HB_VTBL( iTI )->ReleaseTypeAttr( HB_THIS_( iTI ) pTypeAttr );
                                       HB_VTBL( iTI )->Release( HB_THIS( iTI ) );
                                       HB_VTBL( iTL )->Release( HB_THIS( iTL ) );
                                       return S_OK;
                                    }
                                 }

                              }
                              HB_VTBL( iTISink )->Release( HB_THIS( iTISink ) );
                           }
                        }
                        else /* szEvent == NULL */
                        {
                           hr = HB_VTBL( iTI )->GetImplTypeFlags( HB_THIS_( iTI ) j, &iFlags );
                           if( hr == S_OK && ( iFlags & IMPLTYPEFLAG_FDEFAULT ) && ( iFlags & IMPLTYPEFLAG_FSOURCE ) )
                           {
                              if( HB_VTBL( iTI )->GetRefTypeOfImplType( HB_THIS_( iTI ) j, &hRefType ) == S_OK &&
                                  HB_VTBL( iTI )->GetRefTypeInfo( HB_THIS_( iTI ) hRefType, &iTISink ) == S_OK )
                              {
                                 hr = HB_VTBL( iTISink )->GetTypeAttr( HB_THIS_( iTISink ) & pTypeAttr2 );
                                 if( hr == S_OK )
                                 {
#if 0
/* Debug code. You can also comment out iFlags condition, to list more interfaces [Mindaugas] */
                                    BSTR bstr;
                                    char str[ 256 ];
                                    int  iLen;

                                    HB_VTBL( iTISink )->GetDocumentation( HB_THIS_( iTISink ) - 1, &bstr, NULL, NULL, NULL );
                                    iLen = WideCharToMultiByte( CP_ACP, 0, bstr, -1, str, sizeof( str ), NULL, NULL );
                                    str[ iLen - 1 ] = '\0';
                                    HB_TRACE( HB_TR_DEBUG, ( "_get_default_sink Method 3: iFlags=%d guid=%s class=%s", iFlags, GUID2String( &( pTypeAttr2->guid ) ), str ) );
#endif
                                    *piid = pTypeAttr2->guid;
                                    HB_VTBL( iTISink )->ReleaseTypeAttr( HB_THIS_( iTISink ) pTypeAttr2 );

                                    HB_VTBL( iTI )->ReleaseTypeAttr( HB_THIS_( iTI ) pTypeAttr );
                                    HB_VTBL( iTI )->Release( HB_THIS( iTI ) );
                                    HB_VTBL( iTL )->Release( HB_THIS( iTL ) );
                                    return S_OK;
                                 }
                              }
                           }
                        }
                     }
                  }
                  HB_VTBL( iTI )->ReleaseTypeAttr( HB_THIS_( iTI ) pTypeAttr );
               }
               HB_VTBL( iTI )->Release( HB_THIS( iTI ) );
            }
         }
         HB_VTBL( iTL )->Release( HB_THIS( iTL ) );
      }
   }
   return E_NOINTERFACE;
}


static void hb_sink_destruct( void * cargo )
{
   ISink * pSink = ( ISink * ) cargo;

   if( pSink->pConnectionPoint )
   {
      IConnectionPoint * pConnectionPoint = pSink->pConnectionPoint;
      DWORD dwCookie = pSink->dwCookie;

      /* Unadvise() may activate pSink destructor so clear these
       * items as protection against recursive Unadvise call.
       */
      pSink->pConnectionPoint = NULL;
      pSink->dwCookie = 0;

      HB_VTBL( pConnectionPoint )->Unadvise( HB_THIS_( pConnectionPoint ) dwCookie );
      HB_VTBL( pConnectionPoint )->Release( HB_THIS( pConnectionPoint ) );
   }
}


HB_FUNC( __AXREGISTERHANDLER )  /* ( pDisp, bHandler [, cIID] ) --> pSink */
{
   IDispatch * pDisp = hb_oleParam( 1 );

   if( pDisp )
   {
      PHB_ITEM pItemBlock = hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL | HB_IT_HASH );

      if( pItemBlock )
      {
         IConnectionPointContainer* pCPC = NULL;
         IConnectionPoint*          pCP = NULL;
         HRESULT                    lOleError;
         IID                        rriid;
         void*                      hCLSID;
         const char *               szIID;

         hb_oleInit();

         szIID = hb_parc( 3 );
         if( szIID && szIID[ 0 ] == '{' )
         {
            const wchar_t * wCLSID;

            wCLSID = hb_parstr_u16( 3, HB_CDP_ENDIAN_NATIVE, &hCLSID, NULL );
            lOleError = CLSIDFromString( ( wchar_t * ) wCLSID, &rriid );
            hb_strfree( hCLSID );
         }
         else
            lOleError = _get_default_sink( pDisp, szIID, &rriid );

         if( lOleError == S_OK )
         {
#if 0
            HB_TRACE( HB_TR_DEBUG, ( "__AXREGISTERHANDLER using sink %s", GUID2String( &rriid ) ) );
#endif
            lOleError = HB_VTBL( pDisp )->QueryInterface( HB_THIS_( pDisp ) HB_ID_REF( IID_IConnectionPointContainer ), ( void ** ) ( void * ) &pCPC );

            if( lOleError == S_OK )
            {
               lOleError = HB_VTBL( pCPC )->FindConnectionPoint( HB_THIS_( pCPC ) HB_ID_REF( rriid ), &pCP );

               if( lOleError == S_OK )
               {
                  PHB_ITEM pOleItem;
                  DWORD dwCookie = 0;
                  ISink * pSink;

                  pSink = ( ISink * ) hb_xgrab( sizeof( ISink ) );    /* TODO: GlobalAlloc/Free GMEM_FIXED ??? */

                  pSink->lpVtbl = &ISink_Vtbl;
                  pSink->count = 0;
                  pSink->pItemHandler = hb_itemNew( pItemBlock );
                  pSink->rriid = rriid;
                  pSink->uiClass = 0;
                  lOleError = HB_VTBL( pCP )->Advise( HB_THIS_( pCP ) ( IUnknown* ) pSink, &dwCookie );
                  pSink->pConnectionPoint = pCP;
                  pSink->dwCookie = dwCookie;

                  HB_VTBL( pDisp )->AddRef( HB_THIS( pDisp ) );
                  pOleItem = hb_oleItemPut( hb_stackReturnItem(), ( IDispatch * ) pDisp );
                  /* bind call back handler item with returned object */
                  hb_oleItemSetCallBack( pOleItem, &pSink->pItemHandler );
                  /* add additional destructor */
                  hb_oleItemSetDestructor( pOleItem, hb_sink_destruct, ( void * ) pSink );
               }
               HB_VTBL( pCPC )->Release( HB_THIS( pCPC ) );
            }
         }

         hb_oleSetError( lOleError );
         if( lOleError != S_OK )
            hb_ret();
      }
      else
         hb_errRT_OLE( EG_ARG, 1015, 0, NULL, HB_ERR_FUNCNAME );
   }
}
