/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    OLE server
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if ! defined( HB_DYNLIB )

#include "hbapi.h"

#include "hbwinuni.h"
#include "hbwinole.h"
#include <olectl.h>

#define MAX_CLSID_SIZE    64
#define MAX_CLSNAME_SIZE  256
#define MAX_REGSTR_SIZE   ( MAX_CLSNAME_SIZE + 64 )

static const LPCTSTR s_regTable[][ 3 ] =
{
   { TEXT( "CLSID\\@" ),                 NULL,                     TEXT( "$" )         },
   { TEXT( "CLSID\\@\\InprocServer32" ), NULL,                     ( LPCTSTR ) -1      },
   { TEXT( "CLSID\\@\\InprocServer32" ), TEXT( "ThreadingModel" ), TEXT( "Apartment" ) },
   { TEXT( "CLSID\\@\\ProgId" ),         NULL,                     TEXT( "$" )         },
   { TEXT( "$" ),                        NULL,                     TEXT( "$" )         },
   { TEXT( "$\\CLSID" ),                 NULL,                     TEXT( "@" )         }
};

static LONG s_lLockCount;
static LONG s_lObjectCount;

static GUID s_IID_IHbOleServer;

static TCHAR s_lpClsId[ MAX_CLSID_SIZE ] = TEXT( "" );
static TCHAR s_lpClsName[ MAX_CLSNAME_SIZE ] = TEXT( "" );

static HB_BOOL  s_fServerReady = HB_FALSE;
static HB_BOOL  s_fHashClone   = HB_FALSE;
static PHB_ITEM s_pAction      = NULL;
static PHB_ITEM s_pMsgHash     = NULL;

static HINSTANCE s_hInstDll;

static HB_BOOL s_objItemToVariant( VARIANT * pVariant, PHB_ITEM pItem );

/* helper functions
 */
static DISPID hb_dynsymToDispId( PHB_DYNS pDynSym )
{
   return ( DISPID ) hb_dynsymToNum( pDynSym );
}

static PHB_DYNS hb_dispIdToDynsym( DISPID dispid )
{
   if( ( LONG ) dispid > 0 )
      return hb_dynsymFromNum( ( int ) dispid );
   else
      return NULL;
}

static void hb_errRT_OLESRV( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode,
                             const char * szDescription, const char * szOperation )
{
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "OLESERVER", errGenCode, errSubCode,
                          szDescription, szOperation, errOsCode, EF_NONE );
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

static HB_BOOL s_hashWithNumKeys( PHB_ITEM pHash )
{
   HB_SIZE nLen = hb_hashLen( pHash ), n;

   for( n = 1; n <= nLen; ++n )
   {
      PHB_ITEM pKey = hb_hashGetKeyAt( pHash, n );
      if( ! pKey || ! HB_IS_NUMERIC( pKey ) )
         return HB_FALSE;
   }

   return HB_TRUE;
}

static int s_WideToAnsiBuffer( const wchar_t * wszString, char * szBuffer, int iLen )
{
   int iResult = WideCharToMultiByte( CP_ACP, 0, wszString, -1, szBuffer, iLen, NULL, NULL );

   szBuffer[ iLen - 1 ] = '\0';
   return iResult;
}

static HB_BOOL s_getKeyValue( LPCTSTR lpKey, LPTSTR lpBuffer, int iLen )
{
   LPTSTR lpPtr;
   int iSize, iPos, iCount;

   if( lpKey == ( LPCTSTR ) -1 )
      return GetModuleFileName( s_hInstDll, lpBuffer, iLen );

   lpPtr = lpBuffer;
   iSize = iLen - 1;
   iPos = 0;
   for( ;; )
   {
      char c = lpKey[ iPos++ ];
      if( c == TEXT( '$' ) || c == TEXT( '@' ) || c == TEXT( '\0' ) )
      {
         if( --iPos )
         {
            iCount = HB_MIN( iPos, iSize );
            memcpy( lpPtr, lpKey, iCount * sizeof( TCHAR ) );
            lpKey += iPos;
            lpPtr += iCount;
            iSize -= iCount;
            if( iSize == 0 )
               break;
            iPos = 0;
         }
         if( c == TEXT( '\0' ) )
            break;
         else
         {
            LPCTSTR lpVal = c == TEXT( '$' ) ? s_lpClsName : s_lpClsId;
            iCount = ( int ) HB_STRNLEN( lpVal, iSize );
            memcpy( lpPtr, lpVal, iCount * sizeof( TCHAR ) );
            lpKey++;
            lpPtr += iCount;
            iSize -= iCount;
            if( iSize == 0 )
               break;
         }
      }
   }
   *lpPtr = TEXT( '\0' );

   return iSize != 0;
}


/* IHbOleServer
 */
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
   DWORD    count;
   PHB_ITEM pAction;
   HB_BOOL  fGuids;
} IHbOleServer;


static HRESULT STDMETHODCALLTYPE QueryInterface( IDispatch * lpThis,
                                                 REFIID riid, void ** ppRet )
{
   if( IsEqualIID( riid, HB_ID_REF( IID_IUnknown ) ) ||
       IsEqualIID( riid, HB_ID_REF( IID_IDispatch ) ) )
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
   return ++( ( IHbOleServer * ) lpThis )->count;
}

static ULONG STDMETHODCALLTYPE Release( IDispatch * lpThis )
{
   IHbOleServer * pHbOleServer = ( IHbOleServer * ) lpThis;

   if( --pHbOleServer->count == 0 )
   {
      if( pHbOleServer->pAction )
      {
         hb_itemRelease( pHbOleServer->pAction );
         pHbOleServer->pAction = NULL;
      }
      hb_xfree( pHbOleServer );
      InterlockedDecrement( &s_lObjectCount );
      return 0;
   }
   return pHbOleServer->count;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IDispatch * lpThis,
                                                   UINT * pInfoCount )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( pInfoCount );
   return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfo( IDispatch * lpThis, UINT iTInfo,
                                              LCID lcid, ITypeInfo ** ppTypeInfo )
{
   HB_SYMBOL_UNUSED( lpThis );
   HB_SYMBOL_UNUSED( iTInfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( ppTypeInfo );
   return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE GetIDsOfNames( IDispatch * lpThis, REFIID riid,
                                                LPOLESTR * rgszNames,
                                                UINT cNames, LCID lcid,
                                                DISPID * rgDispId )
{
   HRESULT hr = S_OK;

   HB_SYMBOL_UNUSED( lcid );

   if( ! IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      hr = DISP_E_UNKNOWNINTERFACE;
   else if( ( ( IHbOleServer * ) lpThis )->fGuids )
      return E_NOTIMPL;
   else if( cNames > 0 )
   {
      char szName[ HB_SYMBOL_NAME_LEN + 1 ];
      DISPID dispid = 0;
      UINT ui;

      if( s_WideToAnsiBuffer( rgszNames[ 0 ], szName,
                              ( int ) sizeof( szName ) ) != 0 )
      {
         PHB_ITEM pAction;

         pAction = ( ( IHbOleServer * ) lpThis )->pAction;
         if( ! pAction )
            pAction = s_pAction;
         if( pAction )
         {
            if( s_pMsgHash )
            {
               HB_SIZE nPos = hb_hashGetCItemPos( s_pMsgHash, szName );

               if( nPos == 0 )
               {
                  PHB_ITEM pKey = hb_itemPutC( hb_stackAllocItem(), szName );
                  if( hb_hashAdd( s_pMsgHash, pKey, NULL ) )
                     hb_hashScan( s_pMsgHash, pKey, &nPos );
                  hb_stackPop();
               }
               dispid = ( DISPID ) nPos;
            }
            else if( HB_IS_HASH( pAction ) )
            {
               HB_SIZE nPos = hb_hashGetCItemPos( pAction, szName );

               if( nPos )
                  dispid = ( DISPID ) nPos;
            }
            else if( HB_IS_OBJECT( pAction ) )
            {
               PHB_DYNS pDynSym = hb_dynsymFindName( szName );

               if( pDynSym && hb_objHasMessage( pAction, pDynSym ) )
                  dispid = hb_dynsymToDispId( pDynSym );
            }
         }
         else
         {
            PHB_DYNS pDynSym = hb_dynsymFindName( szName );

            if( pDynSym && ( hb_dynsymIsFunction( pDynSym ) ||
                             hb_dynsymIsMemvar( pDynSym ) ) )
               dispid = hb_dynsymToDispId( pDynSym );
         }
      }

      for( ui = 0; ui < cNames; ++ui )
         rgDispId[ ui ] = DISPID_UNKNOWN;

      hr = DISP_E_UNKNOWNNAME;
      if( dispid )
      {
         rgDispId[ 0 ] = dispid;
         if( cNames == 1 )
            hr = S_OK;
      }
   }

   return hr;
}

static HRESULT STDMETHODCALLTYPE Invoke( IDispatch * lpThis, DISPID dispid, REFIID riid,
                                         LCID lcid, WORD wFlags, DISPPARAMS * pParams,
                                         VARIANT * pVarResult, EXCEPINFO * pExcepInfo,
                                         UINT * puArgErr )
{
   PHB_DYNS pDynSym;
   PHB_ITEM pAction;
   HB_USHORT uiClass = 0;

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pExcepInfo );
   HB_SYMBOL_UNUSED( puArgErr );

   if( ! IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
      return DISP_E_UNKNOWNINTERFACE;

   pAction = ( ( IHbOleServer * ) lpThis )->pAction;
   if( ! pAction )
      pAction = s_pAction;

   if( pAction )
   {
      HB_BOOL fResult = HB_FALSE;

      if( s_pMsgHash )
      {
         if( ( wFlags & DISPATCH_METHOD ) != 0 ||
             ( ( wFlags & DISPATCH_PROPERTYGET ) != 0 && pParams->cArgs == 0 ) ||
             ( ( wFlags & DISPATCH_PROPERTYPUT ) != 0 && pParams->cArgs == 1 ) )
         {
            fResult = hb_oleDispInvoke( NULL, pAction,
                                        hb_hashGetKeyAt( s_pMsgHash, ( HB_SIZE ) dispid ),
                                        pParams, pVarResult, s_objItemToVariant, uiClass );
         }
      }
      else if( HB_IS_HASH( pAction ) )
      {
         PHB_ITEM pItem;

         if( ( ( IHbOleServer * ) lpThis )->fGuids )
         {
            PHB_ITEM pKey = hb_itemPutNL( hb_stackAllocItem(), ( long ) dispid );
            pItem = hb_hashGetItemPtr( pAction, pKey, 0 );
            hb_stackPop();
         }
         else
            pItem = hb_hashGetValueAt( pAction, ( HB_SIZE ) dispid );

         if( pItem )
         {
            if( HB_IS_EVALITEM( pItem ) )
            {
               if( ( wFlags & DISPATCH_METHOD ) != 0 )
               {
                  PHB_SYMB pSym = hb_itemGetSymbol( pItem );
                  fResult = hb_oleDispInvoke( pSym, pSym ? pAction : pItem, NULL,
                                              pParams, pVarResult,
                                              s_objItemToVariant, uiClass );
               }
            }
            else if( ( wFlags & DISPATCH_PROPERTYGET ) != 0 &&
                     pParams->cArgs == 0 )
            {
               if( pVarResult )
                  hb_oleItemToVariantEx( pVarResult, pItem, s_objItemToVariant );
               fResult = HB_TRUE;
            }
            else if( ( wFlags & DISPATCH_PROPERTYPUT ) != 0 &&
                     pParams->cArgs == 1 )
            {
               hb_oleVariantToItemEx( pItem, &pParams->rgvarg[ 0 ], uiClass );
               fResult = HB_TRUE;
            }
         }
      }
      else if( HB_IS_OBJECT( pAction ) )
      {
         pDynSym = hb_dispIdToDynsym( dispid );
         if( pDynSym && ( wFlags & DISPATCH_PROPERTYPUT ) != 0 )
         {
            if( pParams->cArgs == 1 )
            {
               char szName[ HB_SYMBOL_NAME_LEN + 1 ];
               szName[ 0 ] = '_';
               hb_strncpy( szName + 1, hb_dynsymName( pDynSym ), sizeof( szName ) - 2 );
               pDynSym = hb_dynsymFindName( szName );
            }
            else
               pDynSym = NULL;
         }
         if( pDynSym && hb_objHasMessage( pAction, pDynSym ) )
         {
            fResult = hb_oleDispInvoke( hb_dynsymSymbol( pDynSym ),
                                        pAction, NULL, pParams, pVarResult,
                                        s_objItemToVariant, uiClass );
         }
      }
      if( ! fResult )
         return DISP_E_MEMBERNOTFOUND;
   }
   else
   {
      pDynSym = hb_dispIdToDynsym( dispid );
      if( ! pDynSym )
         return DISP_E_MEMBERNOTFOUND;

      if( wFlags & DISPATCH_PROPERTYPUT )
      {
         if( pParams->cArgs == 1 && hb_dynsymIsMemvar( pDynSym ) )
         {
            PHB_ITEM pItem = hb_stackAllocItem();

            hb_oleVariantToItemEx( pItem, &pParams->rgvarg[ 0 ], uiClass );
            hb_memvarSetValue( hb_dynsymSymbol( pDynSym ), pItem );
            hb_stackPop();
            return S_OK;
         }
         else
            return DISP_E_MEMBERNOTFOUND;
      }
      else if( ( wFlags & DISPATCH_PROPERTYGET ) &&
               pParams->cArgs == 0 && hb_dynsymIsMemvar( pDynSym ) )
      {
         if( pVarResult )
         {
            PHB_ITEM pItem = hb_stackAllocItem();
            hb_memvarGet( pItem, hb_dynsymSymbol( pDynSym ) );
            hb_oleItemToVariantEx( pVarResult, pItem, s_objItemToVariant );
            hb_stackPop();
         }
         return S_OK;
      }
      else if( ( wFlags & DISPATCH_METHOD ) == 0 ||
               ! hb_dynsymIsFunction( pDynSym ) )
         return DISP_E_MEMBERNOTFOUND;
      else if( ! hb_oleDispInvoke( hb_dynsymSymbol( pDynSym ),
                                   NULL, NULL, pParams, pVarResult,
                                   s_objItemToVariant, uiClass ) )
         return DISP_E_MEMBERNOTFOUND;
   }

   return S_OK;
}

static const IDispatchVtbl IHbOleServer_Vtbl = {
   QueryInterface,
   AddRef,
   Release,
   GetTypeInfoCount,
   GetTypeInfo,
   GetIDsOfNames,
   Invoke
};


/* IClassFactory object
 */
#if ! defined( HB_OLE_C_API )
typedef struct
{
   HRESULT ( STDMETHODCALLTYPE * QueryInterface ) ( IClassFactory*, REFIID, void** );
   ULONG   ( STDMETHODCALLTYPE * AddRef ) ( IClassFactory* );
   ULONG   ( STDMETHODCALLTYPE * Release ) ( IClassFactory* );
   HRESULT ( STDMETHODCALLTYPE * CreateInstance ) ( IClassFactory*, IUnknown*, REFIID, void** );
   HRESULT ( STDMETHODCALLTYPE * LockServer) ( IClassFactory*, BOOL );
} IClassFactoryVtbl;
#endif

typedef struct
{
   const IClassFactoryVtbl * lpVtbl;
} IHbClassFactory;

static IHbClassFactory s_IClassFactoryObj;


static HRESULT STDMETHODCALLTYPE classQueryInterface( IClassFactory * lpThis,
                                                      REFIID riid,
                                                      void ** ppRet )
{
   if( IsEqualIID( riid, HB_ID_REF( IID_IUnknown ) ) ||
       IsEqualIID( riid, HB_ID_REF( IID_IClassFactory ) ) )
   {
      *ppRet = ( void * ) lpThis;
      HB_VTBL( lpThis )->AddRef( HB_THIS( lpThis ) );
      return S_OK;
   }
   *ppRet = NULL;
   return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE classAddRef( IClassFactory * lpThis )
{
   HB_SYMBOL_UNUSED( lpThis );

   InterlockedIncrement( &s_lObjectCount );
   return 1;
}

static ULONG STDMETHODCALLTYPE classRelease( IClassFactory * lpThis )
{
   HB_SYMBOL_UNUSED( lpThis );

   return InterlockedDecrement( &s_lObjectCount );
}

static HRESULT s_createHbOleObject( REFIID riid, void ** ppvObj,
                                    PHB_ITEM pAction, HB_BOOL fGuids )
{
   HRESULT hr;
   IHbOleServer * thisobj = ( IHbOleServer * ) hb_xalloc( sizeof( IHbOleServer ) );

   if( ! thisobj )
   {
      if( pAction )
         hb_itemRelease( pAction );
      hr = E_OUTOFMEMORY;
   }
   else
   {
      InterlockedIncrement( &s_lObjectCount );

      thisobj->lpVtbl = &IHbOleServer_Vtbl;
      thisobj->count = 1;
      thisobj->pAction = pAction;
      thisobj->fGuids = fGuids;

      hr = IHbOleServer_Vtbl.QueryInterface( ( IDispatch * ) thisobj, riid, ppvObj );
      IHbOleServer_Vtbl.Release( ( IDispatch * ) thisobj );
   }
   return hr;
}

static HB_BOOL s_objItemToVariant( VARIANT * pVariant, PHB_ITEM pItem )
{
   void * pvObj;

   VariantClear( pVariant );

   if( s_createHbOleObject( HB_ID_REF( IID_IDispatch ), &pvObj,
                            hb_itemNew( pItem ), HB_FALSE ) == S_OK )
   {
      V_VT( pVariant ) = VT_DISPATCH;
      V_DISPATCH( pVariant ) = ( IDispatch * ) pvObj;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HRESULT STDMETHODCALLTYPE classCreateInstance( IClassFactory * lpThis,
                                                      IUnknown * punkOuter,
                                                      REFIID riid,
                                                      void ** ppvObj )
{
   HRESULT hr;

   HB_SYMBOL_UNUSED( lpThis );

   *ppvObj = NULL;

   if( punkOuter )
      hr = CLASS_E_NOAGGREGATION;
   else
   {
      PHB_ITEM pAction = NULL;
      HB_BOOL fGuids = HB_FALSE;

      if( s_pAction )
      {
         if( HB_IS_EVALITEM( s_pAction ) )
         {
            if( hb_vmRequestReenter() )
            {
               hb_vmPushEvalSym();
               hb_vmPush( s_pAction );
               hb_vmProc( 0 );
               pAction = hb_itemNew( hb_stackReturnItem() );
               hb_vmRequestRestore();
            }
         }
         else if( HB_IS_HASH( s_pAction ) )
         {
            if( s_fHashClone )
               pAction = hb_itemClone( s_pAction );
            else if( ! s_pMsgHash && s_hashWithNumKeys( s_pAction ) )
               fGuids = HB_TRUE;
         }
      }
      hr = s_createHbOleObject( riid, ppvObj, pAction, fGuids );
   }
   return hr;
}

static HRESULT STDMETHODCALLTYPE classLockServer( IClassFactory * lpThis,
                                                  BOOL fLock )
{
   HB_SYMBOL_UNUSED( lpThis );

   if( fLock )
      InterlockedIncrement( &s_lLockCount );
   else
      InterlockedDecrement( &s_lLockCount );

   return S_OK;
}

static const IClassFactoryVtbl IClassFactory_Vtbl = {
   classQueryInterface,
   classAddRef,
   classRelease,
   classCreateInstance,
   classLockServer
};


/* OLE InProc DLL server API
 */
STDAPI DllGetClassObject( REFCLSID rclsid, REFIID riid, void ** ppv )
{
   HRESULT hr;

   if( IsEqualCLSID( rclsid, HB_ID_REF( s_IID_IHbOleServer ) ) )
   {
      hr = classQueryInterface( ( IClassFactory * ) ( void * ) &s_IClassFactoryObj, riid, ppv );
   }
   else
   {
      *ppv = NULL;
      hr = CLASS_E_CLASSNOTAVAILABLE;
   }

   return hr;
}

STDAPI DllCanUnloadNow( void )
{
   return ( s_lObjectCount | s_lLockCount ) ? S_FALSE : S_OK;
}


/* server register/unregister code
 */

STDAPI DllUnregisterServer( void )
{
   TCHAR lpKeyName[ MAX_REGSTR_SIZE ];
   int i;

   for( i = ( int ) HB_SIZEOFARRAY( s_regTable ) - 1; i >= 0; --i )
   {
      if( s_getKeyValue( s_regTable[ i ][ 0 ], lpKeyName, MAX_REGSTR_SIZE ) )
         RegDeleteKey( HKEY_CLASSES_ROOT, lpKeyName );
   }

   return S_OK;
}

#ifndef SELFREG_E_CLASS
#  ifndef SELFREG_E_FIRST
#     define SELFREG_E_FIRST  MAKE_SCODE( SEVERITY_ERROR, FACILITY_ITF, 0x0200 )
#  endif
#  define SELFREG_E_CLASS     ( SELFREG_E_FIRST + 1 )
#endif

STDAPI DllRegisterServer( void )
{
   TCHAR lpKeyName[ MAX_REGSTR_SIZE ];
   TCHAR lpNameBuf[ MAX_REGSTR_SIZE ];
   TCHAR lpValue  [ MAX_REGSTR_SIZE ];
   LPCTSTR lpValName;
   HRESULT hr = S_OK;
   HKEY hKey;
   long err;
   int i;

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( s_regTable ); ++i )
   {
      s_getKeyValue( s_regTable[ i ][ 0 ], lpKeyName, MAX_REGSTR_SIZE );
      if( s_regTable[ i ][ 1 ] )
      {
         s_getKeyValue( s_regTable[ i ][ 1 ], lpNameBuf, MAX_REGSTR_SIZE );
         lpValName = lpNameBuf;
      }
      else
         lpValName = NULL;
      s_getKeyValue( s_regTable[ i ][ 2 ], lpValue, MAX_REGSTR_SIZE );

      err = RegCreateKeyEx( HKEY_CLASSES_ROOT, lpKeyName,
                            0, NULL, REG_OPTION_NON_VOLATILE,
                            KEY_SET_VALUE | KEY_CREATE_SUB_KEY,
                            NULL, &hKey, NULL );

      if( err == ERROR_SUCCESS )
      {
         err = RegSetValueEx( hKey, lpValName, 0, REG_SZ,
                              ( const BYTE * ) lpValue,
                              ( lstrlen( lpValue ) + 1 ) * sizeof( TCHAR ) );
         RegCloseKey( hKey );
      }
      if( err != ERROR_SUCCESS )
      {
         DllUnregisterServer();
         hr = SELFREG_E_CLASS;
         break;
      }
   }

   return hr;
}

#if defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) || defined( __POCC__ ) )
BOOL WINAPI DllMain( HANDLE hInstance, DWORD dwReason, PVOID pvReserved )
#else
BOOL WINAPI DllMain( HINSTANCE hInstance, DWORD dwReason, PVOID pvReserved )
#endif
{
   static HB_BOOL s_fInit = HB_FALSE;
   BOOL fResult = TRUE;

   HB_SYMBOL_UNUSED( pvReserved );

   switch( dwReason )
   {
      case DLL_PROCESS_ATTACH:
         s_hInstDll = ( HINSTANCE ) hInstance;
         s_lLockCount = s_lObjectCount = 0;
         s_IClassFactoryObj.lpVtbl = ( IClassFactoryVtbl * )
                                     &IClassFactory_Vtbl;

         DisableThreadLibraryCalls( ( HMODULE ) hInstance );

         s_fInit = ! hb_vmIsActive();
         if( s_fInit )
            hb_vmInit( HB_FALSE );

         hb_oleInit();

         if( ! s_fServerReady )
         {
            PHB_DYNS pDynSym = hb_dynsymFind( "DLLMAIN" );

            if( pDynSym && hb_dynsymIsFunction( pDynSym ) &&
                hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( pDynSym );
               hb_vmPushNil();
               hb_vmProc( 0 );
               hb_vmRequestRestore();
            }
         }
         fResult = s_fServerReady ? TRUE : FALSE;
         break;

      case DLL_PROCESS_DETACH:
         s_fServerReady = HB_FALSE;
         if( s_pAction )
         {
            hb_itemRelease( s_pAction );
            s_pAction = NULL;
         }
         if( s_pMsgHash )
         {
            hb_itemRelease( s_pMsgHash );
            s_pMsgHash = NULL;
         }
         if( s_fInit )
         {
            hb_vmQuit();
            s_fInit = HB_FALSE;
         }
         break;
   }

   return fResult;
}

/* win_oleServerInit( <cClassID>, <cServerName>, ;
 *                    [ <hAction> | <oAction> | <bAction> | <sAction> ], ;
 *                    [ <lHashClone> | <lAcceptAll> ] )
 */
HB_FUNC( WIN_OLESERVERINIT )
{
   HB_ERRCODE errCode = 0;

   if( ! s_fServerReady )
   {
      void * hClsId, * hClsName;
      LPCTSTR lpClsId, lpClsName;

      lpClsId = HB_PARSTR( 1, &hClsId, NULL );
      lpClsName = HB_PARSTR( 2, &hClsName, NULL );

      if( lpClsId && lpClsName )
      {
         void * hOleClsId;
         LPCOLESTR lpOleClsId;

         lpOleClsId = hb_parstr_u16( 1, HB_CDP_ENDIAN_NATIVE, &hOleClsId, NULL );
         if( CLSIDFromString( ( LPOLESTR ) lpOleClsId, &s_IID_IHbOleServer ) == S_OK )
         {
            PHB_ITEM pAction;

            s_fHashClone = HB_FALSE;
            if( s_pMsgHash )
            {
               hb_itemRelease( s_pMsgHash );
               s_pMsgHash = NULL;
            }

            pAction = hb_param( 3, HB_IT_HASH | HB_IT_BLOCK | HB_IT_SYMBOL );
            if( ! pAction && HB_ISOBJECT( 3 ) )
               pAction = hb_param( 3, HB_IT_OBJECT );
            if( pAction )
            {
               if( s_pAction )
                  hb_itemRelease( s_pAction );
               s_pAction = hb_itemNew( pAction );

               if( HB_ISLOG( 4 ) )
               {
                  if( hb_parl( 4 ) )
                  {
                     if( HB_IS_HASH( s_pAction ) )
                        s_fHashClone = HB_TRUE;
                     else
                     {
                        s_pMsgHash = hb_hashNew( hb_itemNew( NULL ) );
                        hb_hashSetFlags( s_pMsgHash, HB_HASH_KEEPORDER );
                     }
                  }
               }
               else if( ! HB_ISNIL( 4 ) )
                  errCode = 1001;
            }
            else if( ! HB_ISNIL( 3 ) )
               errCode = 1001;

            HB_STRNCPY( s_lpClsId, lpClsId, HB_SIZEOFARRAY( s_lpClsId ) - 1 );
            HB_STRNCPY( s_lpClsName, lpClsName, HB_SIZEOFARRAY( s_lpClsName ) - 1 );

            s_fServerReady = HB_TRUE;
         }
         else
            errCode = 1002;

         hb_strfree( hOleClsId );
      }
      else
         errCode = 1001;

      hb_strfree( hClsId );
      hb_strfree( hClsName );
   }

   if( errCode )
      hb_errRT_OLESRV( EG_ARG, errCode, 0, NULL, HB_ERR_FUNCNAME );
   else
      hb_retl( s_fServerReady );
}

#endif
