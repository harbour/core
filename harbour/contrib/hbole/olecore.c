/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OLE library
 *
 * Copyright 2000, 2003 Jose F. Gimenez (JFG) <jfgimenez@wanadoo.es>
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbinit.h"

#include <ole2.h>

static HRESULT      s_lOleError = 0;

static PHB_DYNS s_pDyns_hb_oleauto;
static PHB_DYNS s_pDyns_hObjAccess;
static PHB_DYNS s_pDyns_hObjAssign;
static PHB_DYNS s_pDyns_GetMessage;


void hb_oleInit( void );          /* TODO: move to some hbole.h */

void HB_FUN_HB_OLEAUTO( void );


static void hb_olecore_init( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_pDyns_hb_oleauto = hb_dynsymGetCase( "HB_OLEAUTO" );
   s_pDyns_hObjAccess = hb_dynsymGetCase( "__HOBJ" );
   s_pDyns_hObjAssign = hb_dynsymGetCase( "___HOBJ" );
   s_pDyns_GetMessage = hb_dynsymGetCase( "__GETMESSAGE" );

   if( s_pDyns_hObjAssign == s_pDyns_GetMessage )
   {
      /* Never executed. Just force linkage */
      HB_FUN_HB_OLEAUTO();
   }

   hb_oleInit();
}


/* Unicode string management */

static wchar_t* AnsiToWide( char* szString )
{
   int       iLen;
   wchar_t*  szWide;

   iLen  = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szString, -1, NULL, 0 );
   szWide = ( wchar_t* ) hb_xgrab( iLen * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szString, -1, szWide, iLen );
   return szWide;
}


static char* WideToAnsi( wchar_t* szWide )
{
   int    iLen;
   char*  szString;

   iLen = WideCharToMultiByte( CP_ACP, 0, szWide, -1, NULL, 0, NULL, NULL );
   szString = hb_xgrab( ( iLen + 1 ) * sizeof( char ) );
   WideCharToMultiByte( CP_ACP, 0, szWide, -1, szString, iLen, NULL, NULL );
   return szString;
}


/* Item <-> Variant conversion */

static void hb_oleItemToVariant( VARIANT* pVariant, PHB_ITEM pItem )
{
   wchar_t*  szString;

   VariantClear( pVariant );  /* pVariant->n1.n2.vt = VT_EMPTY; */

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_MEMO:
         pVariant->n1.n2.vt = VT_BSTR;
         szString = AnsiToWide( hb_itemGetCPtr( pItem ) );
         pVariant->n1.n2.n3.bstrVal = SysAllocString( szString );
         hb_xfree( szString );
         break;

      case HB_IT_LOGICAL:
         pVariant->n1.n2.vt = VT_BOOL;
         pVariant->n1.n2.n3.boolVal = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
         break;

      case HB_IT_INTEGER:
         pVariant->n1.n2.vt = VT_I4;
         pVariant->n1.n2.n3.lVal = hb_itemGetNL( pItem );
         break;

      case HB_IT_LONG:
         pVariant->n1.n2.vt = VT_I8;
         pVariant->n1.n2.n3.llVal = hb_itemGetNInt( pItem );
         break;

      case HB_IT_DOUBLE:
         pVariant->n1.n2.vt = VT_R8;
         pVariant->n1.n2.n3.dblVal = hb_itemGetND( pItem );
         break;

      case HB_IT_DATE:
         pVariant->n1.n2.vt = VT_DATE;
         pVariant->n1.n2.n3.dblVal = ( double ) ( hb_itemGetDL( pItem ) - 0x0024D9AB );
         break;

      case HB_IT_OBJECT:
         if ( hb_stricmp( hb_objGetClsName( pItem ), "HB_OLEAUTO" ) == 0 )
         {
            hb_vmPushDynSym( s_pDyns_hObjAccess );
            hb_vmPush( pItem );
            hb_vmSend( 0 );
            pVariant->n1.n2.vt = VT_DISPATCH;
            pVariant->n1.n2.n3.pdispVal = ( IDispatch* ) hb_parptr( -1 );
         }
         break;
   }
}


static void hb_oleVariantToItem( PHB_ITEM pItem, VARIANT* pVariant )
{
   char*  szString;

   hb_itemClear( pItem );

   switch( pVariant->n1.n2.vt )
   {
      case VT_BSTR:
         szString = WideToAnsi( pVariant->n1.n2.n3.bstrVal );
         hb_itemPutC( pItem, szString );
         hb_xfree( szString );
         break;

      case VT_BOOL:
         hb_itemPutL( pItem, pVariant->n1.n2.n3.boolVal );
         break;

      case VT_DISPATCH:
      {
         PHB_ITEM   pRet;

         if ( pVariant->n1.n2.n3.pdispVal )
         {
            /* TODO: save/restore stack return item */
            hb_vmPushDynSym( s_pDyns_hb_oleauto );
            hb_vmPushNil();
            hb_vmDo( 0 );
         
            pRet = hb_itemNew( NULL );
            hb_itemMove( pRet, hb_stackReturnItem() );
         
            hb_vmPushDynSym( s_pDyns_hObjAssign );
            hb_vmPush( pRet );
            hb_vmPushPointer( pVariant->n1.n2.n3.pdispVal );
            hb_vmSend( 1 );
            hb_itemMove( pItem, pRet );
            hb_itemRelease( pRet );
         }
         break;
      }

      case VT_I1:
           hb_itemPutNI( pItem, ( int ) pVariant->n1.n2.n3.cVal );
           break;

      case VT_I2:
           hb_itemPutNI( pItem, ( int ) pVariant->n1.n2.n3.iVal );
           break;

      case VT_I4:
           hb_itemPutNL( pItem, pVariant->n1.n2.n3.lVal );
           break;

      case VT_I8:
           hb_itemPutNInt( pItem, pVariant->n1.n2.n3.llVal );
           break;

      case VT_UI1:
           hb_itemPutNI( pItem, ( int ) pVariant->n1.n2.n3.bVal );
           break;

      case VT_UI2:
           hb_itemPutNI( pItem, ( int ) pVariant->n1.n2.n3.uiVal );
           break;

      case VT_UI4:
           hb_itemPutNInt( pItem, ( HB_LONG ) pVariant->n1.n2.n3.ulVal );
           break;

      case VT_UI8:  
           /* TODO: sign is lost. Convertion to double will lose significant digits. */
           hb_itemPutNInt( pItem, ( HB_LONG ) pVariant->n1.n2.n3.ullVal );
           break;

      case VT_R8:
           hb_itemPutND( pItem, pVariant->n1.n2.n3.dblVal );
           break;

      case VT_INT:
           hb_itemPutNI( pItem, pVariant->n1.n2.n3.intVal );
           break;

      case VT_UINT:
           hb_itemPutNInt( pItem, ( HB_LONG ) pVariant->n1.n2.n3.uintVal );
           break;

      case VT_DATE:
           hb_itemPutDL( pItem, ( long ) pVariant->n1.n2.n3.dblVal + 0x0024D9AB );
           break;

   }
}


/* IDispatch parameters, return value handling */

static void GetParams( DISPPARAMS* dParams )
{
   VARIANTARG*  pArgs = NULL;
   UINT         uiArgCount, uiArg;

   uiArgCount = ( UINT ) hb_pcount();

   if( uiArgCount > 0 )
   {
      pArgs = ( VARIANTARG* ) hb_xgrab( sizeof( VARIANTARG ) * uiArgCount );

      for( uiArg = 0; uiArg < uiArgCount; uiArg++ )
      {
         VariantInit( &( pArgs[ uiArg ] ) );
         hb_oleItemToVariant( & pArgs[ uiArg ], hb_param( uiArgCount - uiArg, HB_IT_ANY ) );
      }
   }

   dParams->rgvarg = pArgs;
   dParams->cArgs  = uiArgCount;
   dParams->rgdispidNamedArgs = 0;
   dParams->cNamedArgs = 0;
}


static void FreeParams( DISPPARAMS * dispparam )
{
   UINT  ui;

   if( dispparam->cArgs > 0 )
   {
      for( ui = 0; ui < dispparam->cArgs; ui++ )
         VariantClear( & ( dispparam->rgvarg[ ui ] ) );

      hb_xfree( dispparam->rgvarg );
   }
}


/* PRG level functions and methods */

HB_FUNC( OLECREATEOBJECT ) /* ( cOleName | cCLSID  [, cIID ] ) */
{
   wchar_t*    cCLSID;
   GUID        ClassID, iid;
   REFIID      riid = &IID_IDispatch;
   IDispatch*  pDisp = NULL;


   cCLSID = AnsiToWide( hb_parc( 1 ) );
   if ( hb_parc( 1 )[ 0 ] == '{' )
      s_lOleError = CLSIDFromString( (LPOLESTR) cCLSID, &ClassID );
   else
      s_lOleError = CLSIDFromProgID( (LPCOLESTR) cCLSID, &ClassID );
   hb_xfree( cCLSID );

   if ( hb_pcount() == 2 )
   {
      if ( hb_parc( 2 )[ 0 ] == '{' )
      {
         cCLSID = AnsiToWide( hb_parc( 2 ) );
         s_lOleError = CLSIDFromString( (LPOLESTR) cCLSID, &iid );
         hb_xfree( cCLSID );
      }
      else
         memcpy( (LPVOID) &iid, hb_parc( 2 ), sizeof( iid ) );

      (LPVOID) riid = &iid;
   }

   if ( s_lOleError == S_OK )
      s_lOleError = CoCreateInstance( &ClassID, NULL, CLSCTX_SERVER, riid, (LPVOID) &pDisp );
   
   hb_retptr( pDisp );
}


HB_FUNC( OLEGETACTIVEOBJECT ) /* ( cOleName | cCLSID  [, cIID ] ) */
{
   BSTR          wCLSID;
   IID           ClassID, iid;
   LPIID         riid = (LPIID) &IID_IDispatch;
   IDispatch*    pDisp = NULL;
   IUnknown*     pUnk = NULL;
   char*         cOleName = hb_parc( 1 );

   s_lOleError = S_OK;

   wCLSID = (BSTR) AnsiToWide( (LPSTR) cOleName );

   if ( cOleName[ 0 ] == '{' )
      s_lOleError = CLSIDFromString( wCLSID, (LPCLSID) &ClassID );
   else
      s_lOleError = CLSIDFromProgID( wCLSID, (LPCLSID) &ClassID );

   hb_xfree( wCLSID );

   if ( hb_pcount() == 2 )
   {
      char * cID = hb_parc( 2 );
      if ( cID[ 0 ] == '{' )
      {
         wCLSID = (BSTR) AnsiToWide( (LPSTR) cID );
         s_lOleError = CLSIDFromString( wCLSID, &iid );
         hb_xfree( wCLSID );
      }
      else
      {
         memcpy( ( LPVOID ) &iid, cID, sizeof( iid ) );
      }

      riid = &iid;
   }

   if ( s_lOleError == S_OK )
   {
      s_lOleError = GetActiveObject( &ClassID, NULL, &pUnk );

      if ( s_lOleError == S_OK )
         s_lOleError = pUnk->lpVtbl->QueryInterface( pUnk, riid, (void **) &pDisp );
   }

   hb_retptr( pDisp );
}


HB_FUNC( OLERELEASE ) 
{
   IDispatch * pDisp = ( IDispatch* ) hb_parptr( 1 );

   s_lOleError = pDisp->lpVtbl->Release( pDisp );
   hb_retl( s_lOleError == S_OK  );
}


HB_FUNC( OLEERROR )
{
   hb_retnl( s_lOleError );
}


HB_FUNC( OLEERRORTEXT )
{
   switch ( s_lOleError )
   {
      case S_OK:
         hb_retc( "" );
         break;

      case CO_E_CLASSSTRING:
         hb_retc( "CO_E_CLASSSTRING" );
         break;

      case OLE_E_WRONGCOMPOBJ:
         hb_retc( "OLE_E_WRONGCOMPOBJ" );
         break;

      case REGDB_E_CLASSNOTREG:
         hb_retc( "REGDB_E_CLASSNOTREG" );
         break;

      case REGDB_E_WRITEREGDB:
         hb_retc( "REGDB_E_WRITEREGDB" );
         break;

      case E_OUTOFMEMORY:
         hb_retc( "E_OUTOFMEMORY" );
         break;

      case E_INVALIDARG:
         hb_retc( "E_INVALIDARG" );
         break;

      case E_UNEXPECTED:
         hb_retc( "E_UNEXPECTED" );
         break;

      case DISP_E_UNKNOWNNAME:
         hb_retc( "DISP_E_UNKNOWNNAME" );
         break;

      case DISP_E_UNKNOWNLCID:
         hb_retc( "DISP_E_UNKNOWNLCID" );
         break;

      case DISP_E_BADPARAMCOUNT:
         hb_retc( "DISP_E_BADPARAMCOUNT" );
         break;

      case DISP_E_BADVARTYPE:
         hb_retc( "DISP_E_BADVARTYPE" );
         break;

      case DISP_E_EXCEPTION:
         hb_retc( "DISP_E_EXCEPTION" );
         break;

      case DISP_E_MEMBERNOTFOUND:
         hb_retc( "DISP_E_MEMBERNOTFOUND" );
         break;

      case DISP_E_NONAMEDARGS:
         hb_retc( "DISP_E_NONAMEDARGS" );
         break;

      case DISP_E_OVERFLOW:
         hb_retc( "DISP_E_OVERFLOW" );
         break;

      case DISP_E_PARAMNOTFOUND:
         hb_retc( "DISP_E_PARAMNOTFOUND" );
         break;

      case DISP_E_TYPEMISMATCH:
         hb_retc( "DISP_E_TYPEMISMATCH" );
         break;

      case DISP_E_UNKNOWNINTERFACE:
         hb_retc( "DISP_E_UNKNOWNINTERFACE" );
         break;

      case DISP_E_PARAMNOTOPTIONAL:
         hb_retc( "DISP_E_PARAMNOTOPTIONAL" );
         break;

      default:
         hb_retc( "Unknown OLE error" );
         break;
   }
}


HB_FUNC( HB_OLEAUTO___ONERROR )
{
   IDispatch*  pDisp;
   char*       szMethod;
   wchar_t*    szMethodWide;
   OLECHAR*    pMemberArray;
   DISPID      dispid;
   DISPPARAMS  dispparam;
   VARIANTARG  RetVal;
   EXCEPINFO   excep;
   UINT        uiArgErr;

   /* Get object handle */
   hb_vmPushDynSym( s_pDyns_hObjAccess );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch* ) hb_parptr( -1 );
                         
   /* TODO: implement hb_clsGetMessageName() */
   hb_vmPushDynSym( s_pDyns_GetMessage );
   hb_vmPushNil();
   hb_vmDo( 0 );

   if ( ! pDisp )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1005, "Invalid HB_OLEAUTO object", hb_parc( -1 ), HB_ERR_ARGS_SELFPARAMS );
   }

   /* Take a copy of szMethod string, because return item could be overwritten */
   szMethod = hb_strdup( hb_parc( -1 ) );
   szMethodWide = AnsiToWide( szMethod );

   /* Try property put */

   if( szMethod[ 0 ] == '_' && hb_pcount() > 0 )
   {
      pMemberArray = &szMethodWide[ 1 ];
      s_lOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, &pMemberArray, 
                                                  1, LOCALE_USER_DEFAULT, &dispid );

      if ( s_lOleError == S_OK )
      {
         DISPID     lPropPut = DISPID_PROPERTYPUT;

         memset( &excep, 0, sizeof( excep ) );
         GetParams( &dispparam );
         dispparam.rgdispidNamedArgs = &lPropPut;
         dispparam.cNamedArgs = 1;

         s_lOleError = pDisp->lpVtbl->Invoke( pDisp, dispid, &IID_NULL, 
                                              LOCALE_USER_DEFAULT,
                                              DISPATCH_PROPERTYPUT, &dispparam, 
                                              NULL, &excep, &uiArgErr );

         FreeParams( &dispparam );
         hb_xfree( szMethodWide );
         hb_xfree( szMethod );
         hb_ret();
         return;
      }
   }

   /* Try property get and invoke */

   pMemberArray = szMethodWide;
   s_lOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, &pMemberArray, 
                                               1, LOCALE_USER_DEFAULT, &dispid );
   hb_xfree( szMethodWide );

   if ( s_lOleError == S_OK )
   {
      memset( &excep, 0, sizeof( excep ) );
      VariantInit( &RetVal );
      GetParams( &dispparam );

      s_lOleError = pDisp->lpVtbl->Invoke( pDisp, dispid, &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           DISPATCH_PROPERTYGET | DISPATCH_METHOD,
                                           &dispparam, &RetVal, &excep, &uiArgErr );
      FreeParams( &dispparam );

      hb_oleVariantToItem( hb_stackReturnItem(), & RetVal );
      if( RetVal.n1.n2.vt != VT_DISPATCH )
         VariantClear( &RetVal );

      hb_xfree( szMethod );
      return;
   }

   /* TODO: add description containing TypeName of the object */
   if( szMethod[ 0 ] == '_' )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, szMethod + 1, HB_ERR_ARGS_BASEPARAMS );
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, szMethod, HB_ERR_ARGS_BASEPARAMS );

   hb_xfree( szMethod );
}


HB_FUNC( HB_OLEAUTO___DTOR )
{
   IDispatch*  pDisp;

   /* Get object handle */
   hb_vmPushDynSym( s_pDyns_hObjAccess );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );

   pDisp = ( IDispatch* ) hb_parptr( -1 );
   if( pDisp )
      pDisp->lpVtbl->Release( pDisp );
}


HB_CALL_ON_STARTUP_BEGIN( _hb_olecore_init_ )
   hb_vmAtInit( hb_olecore_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_olecore_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_olecore_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_olecore_init = _hb_olecore_init_;
   #pragma data_seg()
#endif
