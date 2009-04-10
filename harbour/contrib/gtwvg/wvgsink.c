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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                     Active-X Interface Functions
 *
 *                       Contributed by Andy Wos
 *                   A little tweaked by Pritpal Bedi
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
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
/*----------------------------------------------------------------------*/

#define HB_OS_WIN_USED

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

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
#if ! defined( HB_OS_WIN_CE )
#include <olectl.h>
#endif
#include <ole2.h>
#include <oleauto.h>


#if defined( __cplusplus ) && \
   ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
     ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
#  define HB_ID_REF( id )     id
#else
#  define HB_ID_REF( id )     ( &(id) )
#endif

/*----------------------------------------------------------------------*/

#if defined(__BORLANDC__) && !defined(HB_ARCH_64BIT)
    #undef MAKELONG
    #define MAKELONG(a,b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | \
                          (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

/*----------------------------------------------------------------------*/
static HRESULT  s_nOleError;
static HMODULE  hLib = NULL;

typedef BOOL    ( CALLBACK *PATLAXWININIT )( void );
typedef BOOL    ( CALLBACK *PATLAXWINTERM )( void );
typedef HRESULT ( CALLBACK *PATLAXGETCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXATTACHCONTROL )( HWND, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXCREATECONTROL )( LPCOLESTR, HWND, IStream*, IUnknown** );
typedef HRESULT ( CALLBACK *PATLAXCREATECONTROLEX )( LPCOLESTR, HWND, IStream*, IUnknown**, IUnknown**, REFIID, IUnknown* );
/*----------------------------------------------------------------------*/
HB_EXPORT void hb_oleItemToVariant( VARIANT *pVariant, PHB_ITEM pItem );

HRESULT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

/* Switch it on when required tracing. While committing switch it off */
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
/*
 * these 2 functions are required to send parameters by reference
 */
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
/*
   this is a macro which defines our IEventHandler struct as so:

   typedef struct {
      IEventHandlerVtbl  *lpVtbl;
   } IEventHandler;
*/

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
/*----------------------------------------------------------------------*/
/*
 * In other words, it defines our IEventHandler to have nothing
 * but a pointer to its VTable. And of course, every COM object must
 * start with a pointer to its VTable.
 *
 * But we actually want to add some more members to our IEventHandler.
 * We just don't want any app to be able to know about, and directly
 * access, those members. So here we'll define a MyRealIEventHandler that
 * contains those extra members. The app doesn't know that we're
 * really allocating and giving it a MyRealIEventHAndler object. We'll
 * lie and tell it we're giving a plain old IEventHandler. That's ok
 * because a MyRealIEventHandler starts with the same VTable pointer.
 *
 * We add a DWORD reference count so that this IEventHandler
 * can be allocated (which we do in our IClassFactory object's
 * CreateInstance()) and later freed. And, we have an extra
 * BSTR (pointer) string, which is used by some of the functions we'll
 * add to IEventHandler
 */
/*----------------------------------------------------------------------*/

typedef struct {
   DISPID     dispid;
   PHB_ITEM   pSelf;
   PHB_DYNS   pSymbol;
} EventMap;


typedef struct {
   IEventHandler*          lpVtbl;
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
/*
 * Every COM object's interface must have the 3 functions QueryInterface(),
 * AddRef(), and Release().
 *
 * IEventHandler's QueryInterface()
*/
static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   /* Check if the GUID matches IEvenetHandler VTable's GUID. We gave the C variable name
    * IID_IEventHandler to our VTable GUID. We can use an OLE function called
    * IsEqualIID to do the comparison for us. Also, if the caller passed a
    * IUnknown GUID, then we'll likewise return the IEventHandler, since it can
    * masquerade as an IUnknown object too. Finally, if the called passed a
    * IDispatch GUID, then we'll return the IExample3, since it can masquerade
    * as an IDispatch too
    */
   if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IUnknown ) ) )
   {
      *ppv = ( IUnknown * ) self;
      /* Increment the count of callers who have an outstanding pointer to self object */
#ifdef __HBTOOUT__
hb_ToOutDebug( ".................................if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IUnknown ) ) )" );
#endif
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IDispatch ) ) )
   {
      *ppv = ( IDispatch * ) self;
#ifdef __HBTOOUT__
hb_ToOutDebug( ".................................if ( IsEqualIID( vTableGuid, HB_ID_REF( IID_IDispatch ) ) )" );
#endif
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if ( IsEqualIID( vTableGuid, HB_ID_REF( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      if( ++( ( ( MyRealIEventHandler * ) self )->iID_riid ) == 1 )
      {
         *ppv = ( IDispatch* ) self;
#ifdef __HBTOOUT__
hb_ToOutDebug( ".................................if ( IsEqualIID( vTableGuid, HB_ID_REF( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )" );
#endif
         self->lpVtbl->AddRef( self );
      }
      return S_OK;
   }

   /* We don't recognize the GUID passed to us. Let the caller know this,
      by clearing his handle, and returning E_NOINTERFACE. */
   *ppv = 0;
   return( E_NOINTERFACE );
}
/*----------------------------------------------------------------------*/
/*
 * IEventHandler's AddRef()
 */
static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   /* Increment IEventHandler's reference count, and return the updated value.
    * NOTE: We have to typecast to gain access to any data members. These
    * members are not defined  (so that an app can't directly access them).
    * Rather they are defined only above in our MyRealIEventHandler
    * struct. So typecast to that in order to access those data members
    */
   #if 0
   hb_ToOutDebug( "AddRef->count=%i", ( ( MyRealIEventHandler * ) self )->count + 1 );
   #endif
   return( ++( ( MyRealIEventHandler * ) self )->count );
}
/*----------------------------------------------------------------------*/
/*
 * IEventHandler's Release()
 */
static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
#if 0
hb_ToOutDebug( "static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )->count = %i",
                                           ( ( MyRealIEventHandler * ) self )->count - 1 );
#endif
   if ( --( ( MyRealIEventHandler * ) self )->count == 0 )
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
/*----------------------------------------------------------------------*/
/*
 * IEventHandler's GetTypeInfoCount()
 */
static HRESULT STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );

   return ( HRESULT ) E_NOTIMPL;
}
/*----------------------------------------------------------------------*/
/*
 * IEventHandler's GetTypeInfo()
 */
static HRESULT STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );

   return ( HRESULT ) E_NOTIMPL;
}
/*----------------------------------------------------------------------*/
/*
 * IEventHandler's GetIDsOfNames()
 */
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

/*-----------------------------------------------------------------------------*/

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
   if ( !IsEqualIID( riid, HB_ID_REF( IID_NULL ) ) )
   {
      return( ( HRESULT ) DISP_E_UNKNOWNINTERFACE );
   }
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   Key = hb_itemNew( NULL );
   if ( hb_hashScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) self )->pEvents, ulPos );
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
         hb_vmDo( iArg );

         for( i=iArg; i > 0; i-- )
         {
            if( HB_IS_BYREF( pItemArray[ iArg-i ] ) )
            {
               hb_oleItemToVariant( &( params->rgvarg[ iArg-i ] ), pItemArray[ iArg-i ] );
            }
         }

         /* Pritpal */
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
   hb_itemRelease( Key );   /* Pritpal */

   return ( HRESULT ) S_OK;
}

/*----------------------------------------------------------------------*/
/*
 * Here's IEventHandler's VTable. It never changes so we can declare it static
 */
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

/*----------------------------------------------------------------------*/
/*
 * constructor : params:
 * device_interface        - refers to the interface type of the COM object (whose event we are trying to receive).
 * device_event_interface  - indicates the interface type of the outgoing interface supported by the COM object.
 *                           This will be the interface that must be implemented by the Sink object.
 *                           is essentially derived from IDispatch, our Sink object (this IEventHandler)
 *                           is also derived from IDispatch.
 */
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
      thisobj->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;

      ( ( MyRealIEventHandler * ) thisobj )->pSelf = NULL;
      ( ( MyRealIEventHandler * ) thisobj )->count = 0;
      ( ( MyRealIEventHandler * ) thisobj )->iID_riid = 0;

      hr = thisobj->lpVtbl->QueryInterface( thisobj, HB_ID_REF( IID_IUnknown ), (void **) (void*) &pIUnknown );
      if (hr == S_OK && pIUnknown)
      {
         hr = pdevice_interface->lpVtbl->QueryInterface( pdevice_interface, HB_ID_REF( IID_IConnectionPointContainer ), (void**) (void*) &pIConnectionPointContainerTemp);
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
                        /**************           This has to be review         *******************
                               PellesC was generating GPF at this point
                               After commenting it out, I could not see any difference in objects
                               I play with. Cannot say why did I retained it so long.            */
                        #if 0
                        ( ( MyRealIEventHandler* ) thisobj )->device_event_interface_iid = rriid;
                        #endif

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
#if 0
hb_ToOutDebug( " Do = %i", i++ );
#endif
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
/*----------------------------------------------------------------------*/
HB_FUNC( HB_AX_SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler* hSink = ( MyRealIEventHandler * ) ( HB_PTRDIFF ) hb_parnint( 1 );

#if 0
hb_ToOutDebug( "---------------------------------------------" );
#endif
   #if 1
   if ( hSink->pIConnectionPoint )
   {
      hSink->dwEventCookie = 0;
      hSink->pIConnectionPoint->lpVtbl->Release( hSink->pIConnectionPoint );
      hSink->pIConnectionPoint = NULL;
   }
   #endif

   #if 1
   if( hSink->pEvents )
   {
      hb_itemRelease( hSink->pEvents );
   }
   #endif

   #if 0
   if( ( MyRealIEventHandler * ) hSink )
   {
      GlobalFree( ( MyRealIEventHandler * ) hSink );
   }
   #endif
}
/*----------------------------------------------------------------------*/
HB_FUNC( HB_AX_RELEASEOBJECT )
{
   IDispatch * pDisp = ( IDispatch * ) ( HB_PTRDIFF ) hb_parnint( 1 );
   s_nOleError = pDisp->lpVtbl->Release( pDisp );
}
/*----------------------------------------------------------------------*/
HB_FUNC( HB_AX_SETUPCONNECTIONPOINT )
{
   HRESULT              hr;
   MyRealIEventHandler* hSink = NULL;
   LPIID                riid  = ( LPIID ) &IID_IDispatch;
   int                  n = 0;

   hr = SetupConnectionPoint( ( device_interface* ) ( HB_PTRDIFF ) hb_parnint( 1 ), ( REFIID ) riid, ( void** ) (void*) &hSink, &n ) ;

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

      #if 0
      /* the code below needs TCHAR strcat of szLibName and dll name */
      char szLibName[ MAX_PATH + 1 ] = { 0 };

      GetSystemDirectory( szLibName, MAX_PATH );
      hb_strncat( szLibName, "\\atl.dll", sizeof( szLibName ) - 1 );
      hLib = LoadLibrary( ( LPCSTR ) szLibName );
      #endif

      /*hLib = LoadLibrary( TEXT( "atl.dll" ) ); */

      #if 1
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
      #endif

      if( hLib )
      {
#if defined( UNICODE ) && defined( GetProcAddress )
         AtlAxWinInit = ( PATLAXWININIT ) GetProcAddressW( hLib, TEXT( "AtlAxWinInit" ) );
#else
         AtlAxWinInit = ( PATLAXWININIT ) GetProcAddress( hLib, "AtlAxWinInit" );
#endif
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

   hb_retl( bRet );
}
/*----------------------------------------------------------------------*/
/*
ATLAPI AtlAxCreateControlEx(
   LPCOLESTR lpszName,
   HWND hWnd,
   IStream* pStream,
   IUnknown** ppUnkContainer,
   IUnknown** ppUnkControl,
   REFIID iidSink = IID_NULL,
   IUnknown* punkSink = NULL
);
*/
HB_FUNC( HB_AX_ATLAXCREATECONTROL )
{
   IUnknown  *pUnk;
   IDispatch *obj;
   BSTR  wString;
   UINT  uLen;
   PATLAXCREATECONTROL AtlAxCreateControl;
   HWND  hContainer = NULL;
   char  *szClass = hb_parc( 1 );
   HWND  hParent  = ( HWND ) ( HB_PTRDIFF ) hb_parnint( 2 );
   char  *Caption = ISNIL(  4 ) ? "" : hb_parc( 3 );
   HMENU id       = ISNIL(  4 ) ? ( HMENU ) ( HB_PTRDIFF ) -1 : ( HMENU ) ( HB_PTRDIFF ) hb_parnint( 4 );
   int   x        = ISNIL(  5 ) ? 0 : hb_parni( 5 );
   int   y        = ISNIL(  6 ) ? 0 : hb_parni( 6 );
   int   w        = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   int   h        = ISNIL(  8 ) ? 0 : hb_parni( 8 );
   int   Style    = ISNIL(  9 ) ? WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS : hb_parni( 9 );
   int   Exstyle  = ISNIL( 10 ) ? 0 : hb_parni( 10 );

   #if 0
   char *lpLic    = ISNIL( 11 ) ? NULL : hb_parc( 11 );
   REFIID *iidSink = IID_NULL;
   IUnknown  *pUnk, *pUnkCtrl, pUnkSink;
   #endif

#if defined( UNICODE ) && defined( GetProcAddress )
   AtlAxCreateControl = ( PATLAXCREATECONTROL ) GetProcAddressW( hLib, TEXT( "AtlAxCreateControl" ) );
#else
   AtlAxCreateControl = ( PATLAXCREATECONTROL ) GetProcAddress( hLib, "AtlAxCreateControl" );
#endif
   if ( AtlAxCreateControl )
   {
      LPTSTR cCaption = HB_TCHAR_CONVTO( Caption );
      LPTSTR cClass = HB_TCHAR_CONVTO( szClass );
      #if 0
      hContainer = ( HWND ) CreateWindowEx( Exstyle, cClass, cCaption, Style, x, y, w, h,
                                            hParent, id, GetModuleHandle( NULL ), NULL );
      #else
      hContainer = ( HWND ) CreateWindowEx( Exstyle, cClass, TEXT(""), Style, x, y, w, h,
                                            hParent, id, GetModuleHandle( NULL ), NULL );
      #endif
      HB_TCHAR_FREE( cCaption );
      HB_TCHAR_FREE( cClass   );

      if( hContainer )
      {
         LPARAM lParam = MAKELPARAM( FALSE, 0 );
#if ! defined( HB_OS_WIN_CE )
         SendMessage( ( HWND ) hContainer, ( UINT ) WM_SETFONT, ( WPARAM ) GetStockObject( DEFAULT_GUI_FONT ), lParam );
#else
         SendMessage( ( HWND ) hContainer, ( UINT ) WM_SETFONT, ( WPARAM ) GetStockObject( OEM_FIXED_FONT ), lParam );
#endif
         uLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Caption, strlen( Caption )+1, NULL, 0 );
         wString = ( BSTR ) malloc( uLen * sizeof( WCHAR ) );
         MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Caption, strlen( Caption )+1, wString, uLen );

         /*( AtlAxCreateControl ) ( wString, hContainer, NULL, &pUnk, &pUnkCtrl, NULL, &pUnkSink ); */
         ( AtlAxCreateControl ) ( wString, hContainer, NULL, &pUnk );

         free( wString );

         #if 1
         if( pUnk )
         {
            pUnk->lpVtbl->QueryInterface( pUnk, HB_ID_REF( IID_IDispatch ), ( void** ) (void*) &obj );
            pUnk->lpVtbl->Release( pUnk );
            hb_retnint( ( HB_PTRDIFF ) obj );

            #if 0
            {
               RECT rc;
               GetClientRect( hContainer, &rc );
               MoveWindow( GetDlgItem( hContainer, ( int ) id ), 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
            }
            #endif
         }
         #else
         if( pUnkCtrl )
         {
            RECT rc;

            pUnkCtrl->lpVtbl->QueryInterface( pUnkCtrl, HB_ID_REF( IID_IDispatch ), ( void** ) (void*) &obj );
            pUnkCtrl->lpVtbl->Release( pUnkCtrl );
            hb_retnint( ( long ) obj );

            GetClientRect( hContainer, &rc );
            MoveWindow( GetDlgItem( hContainer, ( int ) id ), 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
         }
         #endif
         else
         {
            hb_retnint( 0 );
         }
      }
      else
      {
         hb_retnint( 0 );
      }
   }
   else
   {
      hb_retnint( 0 );
   }

   /* return the container handle */
   if ISBYREF( 12 )
   {
      hb_stornint( ( HB_PTRDIFF ) hContainer, 12 );
   }
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
            pUnk->lpVtbl->QueryInterface( pUnk, HB_ID_REF( IID_IDispatch ), ( void** ) (void*) &obj );
            pUnk->lpVtbl->Release( pUnk );
            GetClientRect( hWnd, &rc );
            /* MoveWindow( GetDlgItem( hParent, ( int ) id ), 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE ); */
            MoveWindow( hWnd, 0, 0, rc.right-rc.left, rc.bottom-rc.top, TRUE );
            hb_retnint( ( HB_PTRDIFF ) obj );
         }
         else
         {
            hb_retnint( 0 );
         }
      }
      else
      {
         hb_retnint( 0 );
      }
   }
   else
   {
      hb_retnint( 0 );
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
      if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, HB_ID_REF( IID_IOleObject ), ( void** ) ( void* ) &lpOleObject ) ) )
      {
         IOleClientSite* lpOleClientSite;

         pUnk->lpVtbl->Release( pUnk );

         if( SUCCEEDED( lpOleObject->lpVtbl->GetClientSite( lpOleObject, &lpOleClientSite ) ) )
         {
            MSG Msg;
            RECT rct;

            memset( &Msg, 0, sizeof( MSG ) );
            GetClientRect( hwnd, &rct );

            lpOleObject->lpVtbl->DoVerb( lpOleObject, hb_parni( 3 ), &Msg, lpOleClientSite, 0, hwnd, &rct );
         }
      }
   }
}
/*----------------------------------------------------------------------*/
/*
 * terminate activex suport, free the library
 */
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
/*                           Load Type Info                             */
/*----------------------------------------------------------------------*/

typedef struct  {
   DISPID   m_dispID;       /* dispatch id */
   BSTR     m_bstrName;     /* method or property name */
   WORD     m_wFlag;        /* invoke flag */
   short    m_oVft;         /* offset of virtual function */
   CALLCONV m_callconv;     /* calling convention */
   VARTYPE  m_vtOutputType; /* output type */
   VARIANT* m_pOutput;      /* output data */
   int      m_nParamCount;  /* number of parameters */
   WORD*    m_pParamTypes;  /* parameter type array */
} DispInfo;

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
#if 0


/*=================================================
   BUGGY experimental, untested and unfinished code
  =================================================*/

int LoadTypeInformation( IDispatch* pDisp ) /* pass dispatch interface */
{
   /*UINT nTypeInfoCount;
    *m_hRet = m_pDisp->GetTypeInfoCount(&nTypeInfoCount);
    *if(m_hRet!=S_OK||nTypeInfoCount==0)
    *{
    *   #ifdef XYDISPDRIVER_DEBUG
    *   _tprintf(_T("GetTypeInfoCount failed or no type info: %x\n"),m_hRet);
    *   #endif
    *}
    */
   ITypeInfo*   pTypeInfo;
   TYPEATTR*    pTypeAttr;
   HRESULT      hr;
   int          i,j;
   unsigned int nCount;
   int          m_nMethodCount;
   int          m_nVarCount;
   int          m_nDispInfoCount;
   DispInfo*    m_pDispInfo;
   UINT         ui;

   char         cBuffer[128];

   /*pDisp->lpVtbl->GetTypeInfoCount(pDisp, &i );  // i should be 1, otherwise no typeinfo present
    *wsprintf( cBuffer, "typeinfocount: %i", i );
    *OutputDebugString( cBuffer);
    */
#ifdef __HBTOOUT__
   hb_ToOutDebug( "---------------- start ------------------" );
#endif
   hr = pDisp->lpVtbl->GetTypeInfo( pDisp, 0, LOCALE_SYSTEM_DEFAULT, &pTypeInfo );
   if( hr != S_OK || pTypeInfo == NULL )
   {
      return 0;
   }

   hr = pDisp->lpVtbl->GetTypeInfoCount( pDisp, &ui );
   hb_ToOutDebug( "typeinfocount: %i, returned: %i", ui, hr );

   hr = pTypeInfo->lpVtbl->GetTypeAttr( pTypeInfo, &pTypeAttr );
   if( hr != S_OK )
   {
      pTypeInfo->lpVtbl->Release( pTypeInfo );
      return 0;
   }
   hb_ToOutDebug( "------------ got type attributes -------------" );

   if( pTypeAttr->typekind != TKIND_DISPATCH && pTypeAttr->typekind != TKIND_COCLASS && pTypeAttr->typekind != TKIND_INTERFACE )
   {
      hb_ToOutDebug( "typekind: %i", pTypeAttr->typekind );

      pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
      pTypeInfo->lpVtbl->Release( pTypeInfo );
      hr = S_FALSE;
   }
   hb_ToOutDebug( "------------ got type kind -------------" );

   if( pTypeAttr->typekind == TKIND_COCLASS )
   {
      int nFlags;
      HREFTYPE hRefType;
      ITypeInfo* pTempInfo;
      TYPEATTR* pTempAttr = NULL;

      for ( i=0 ; i < pTypeAttr->cImplTypes ; i++ )
      {
         if( pTypeInfo->lpVtbl->GetImplTypeFlags( pTypeInfo, i, &nFlags ) == S_OK && ( nFlags & IMPLTYPEFLAG_FDEFAULT ))
         {
            hr = pTypeInfo->lpVtbl->GetRefTypeOfImplType( pTypeInfo, i, &hRefType );
            if( hr == S_OK )
            {
               hr = pTypeInfo->lpVtbl->GetRefTypeInfo( pTypeInfo, hRefType, &pTempInfo );
            }
            if( hr == S_OK )
            {
               hr = pTempInfo->lpVtbl->GetTypeAttr( pTempInfo, &pTempAttr );
               if( hr != S_OK )
               {
                  pTempInfo->lpVtbl->Release( pTempInfo );
                  pTempInfo = NULL;
                  break;
               }
            }
            else break;
         }
      }

      pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
      pTypeInfo->lpVtbl->Release(pTypeInfo);

      if( pTempAttr == NULL )
      {
         if( hr == S_OK )
         {
            /* hr = S_FALSE; */
         }
         return 0;
      }
      else
      {
         pTypeInfo = pTempInfo;
         pTypeAttr = pTempAttr;
      }
   }
   #if 0
   hb_ToOutDebug( "------------------ allocating memory ---------------" );
   #endif

   m_nMethodCount = pTypeAttr->cFuncs;
   m_nVarCount = pTypeAttr->cVars;
   m_nDispInfoCount = m_nMethodCount+2*m_nVarCount;
   /* allocate <m_nDispInfoCount> structures of DispInfo */
   m_pDispInfo = ( DispInfo * ) hb_xgrab( sizeof( DispInfo ) * m_nDispInfoCount );

   hb_ToOutDebug( "methods: %i vars: %i  DispInfo: %i", m_nMethodCount, m_nVarCount, m_nDispInfoCount );

   hb_ToOutDebug( "--------------- looping through methods ----------------");

   for( i=0; i < m_nMethodCount; i++ )
   {
      FUNCDESC* pFuncDesc;
      hr = pTypeInfo->lpVtbl->GetFuncDesc( pTypeInfo, i, &pFuncDesc );
      if( hr != S_OK )
      {
         pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
         pTypeInfo->lpVtbl->Release( pTypeInfo );
         /* m_nMethodCount = m_nVarCount = m_nDispInfoCount = 0; */
         hb_xfree( m_pDispInfo );
         return 0;
      }

      m_pDispInfo[i].m_dispID = pFuncDesc->memid;

      hr = pTypeInfo->lpVtbl->GetNames( pTypeInfo, m_pDispInfo[i].m_dispID ,&m_pDispInfo[i].m_bstrName, 1, &nCount );
      if( hr != S_OK )
      {
         pTypeInfo->lpVtbl->ReleaseFuncDesc( pTypeInfo, pFuncDesc );
         pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
         pTypeInfo->lpVtbl->Release( pTypeInfo );
         /*m_nMethodCount = m_nVarCount = m_nDispInfoCount = 0;*/
         /* free memory */
         hb_xfree( m_pDispInfo );
         return 0;
      }

      /* kind of entry */
      switch( pFuncDesc->invkind )
      {
      case INVOKE_PROPERTYGET:
         m_pDispInfo[i].m_wFlag = DISPATCH_PROPERTYGET;
         break;
      case INVOKE_PROPERTYPUT:
         m_pDispInfo[i].m_wFlag = DISPATCH_PROPERTYPUT;
         break;
      case INVOKE_PROPERTYPUTREF:
         m_pDispInfo[i].m_wFlag = DISPATCH_PROPERTYPUTREF;
         break;
      case INVOKE_FUNC:
         m_pDispInfo[i].m_wFlag = DISPATCH_METHOD;
         break;
      default:
         break;
      }

      m_pDispInfo[i].m_oVft = pFuncDesc->oVft;
      m_pDispInfo[i].m_callconv = pFuncDesc->callconv;
      m_pDispInfo[i].m_pOutput = ( VARIANT* ) hb_xgrab( sizeof( VARIANT ) );

      VariantInit( m_pDispInfo[i].m_pOutput );
      m_pDispInfo[i].m_vtOutputType = pFuncDesc->elemdescFunc.tdesc.vt;
      if( m_pDispInfo[i].m_vtOutputType==VT_VOID||m_pDispInfo[i].m_vtOutputType==VT_NULL )
      {
         m_pDispInfo[i].m_vtOutputType = VT_EMPTY;
      }

      WideCharToMultiByte(  CP_ACP,        /* code page                            */
                            0,             /* performance and mapping flags        */
                            m_pDispInfo[i].m_bstrName,    /* wide-character string */
                            -1,            /* number of chars in string.           */
                            cBuffer,       /* buffer for new string                */
                            128,           /* size of buffer                       */
                            NULL,          /* default for unmappable chars         */
                            NULL           /* set when default char used           */
                         );

      hb_ToOutDebug( "%s   [id: %i kind: %i params: %i]", cBuffer, pFuncDesc->memid, pFuncDesc->invkind, pFuncDesc->cParams );
      /*hb_ToOutDebug( cBuffer );      name of the function                  */
      /*hb_ToOutDebug( m_pDispInfo[i].m_bstrName );                          */

      /*hb_ToOutDebug( "------------- looping paremeters ---------------");  */

      /* parameters                                                          */
      m_pDispInfo[i].m_nParamCount = pFuncDesc->cParams;
      m_pDispInfo[i].m_pParamTypes = (WORD*) hb_xgrab( sizeof(WORD) * (m_pDispInfo[i].m_nParamCount+1));
      for( j=0; j<m_pDispInfo[i].m_nParamCount; j++ )
      {
         if( pFuncDesc->lprgelemdescParam[j].tdesc.vt==VT_SAFEARRAY )
         {
            m_pDispInfo[i].m_pParamTypes[j] = (pFuncDesc->lprgelemdescParam[j].tdesc.lptdesc->vt)|VT_ARRAY;
         }
         else if( pFuncDesc->lprgelemdescParam[j].tdesc.vt==VT_PTR )
         {
            m_pDispInfo[i].m_pParamTypes[j] = (pFuncDesc->lprgelemdescParam[j].tdesc.lptdesc->vt)|VT_BYREF;
         }
         else
         {
            m_pDispInfo[i].m_pParamTypes[j] = pFuncDesc->lprgelemdescParam[j].tdesc.vt;
         }
      }
      m_pDispInfo[i].m_pParamTypes[m_pDispInfo[i].m_nParamCount] = 0;
      pTypeInfo->lpVtbl->ReleaseFuncDesc( pTypeInfo, pFuncDesc );
   }

   hb_ToOutDebug( "--------------- looping through var count -------------------");

   for( i=m_nMethodCount; i<m_nMethodCount+m_nVarCount; i++ )
   {
      VARDESC* pVarDesc;
      hr = pTypeInfo->lpVtbl->GetVarDesc(pTypeInfo, i-m_nMethodCount, &pVarDesc);
      if( hr != S_OK )
      {
         pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
         pTypeInfo->lpVtbl->Release( pTypeInfo );
         /*m_nMethodCount = m_nVarCount = m_nDispInfoCount = 0; */
         /* free memory                                         */
         return 0;
      }
      m_pDispInfo[i].m_dispID = pVarDesc->memid;
      m_pDispInfo[i+m_nVarCount].m_dispID = m_pDispInfo[i].m_dispID;

      hr = pTypeInfo->lpVtbl->GetNames( pTypeInfo, m_pDispInfo[i].m_dispID ,&m_pDispInfo[i].m_bstrName,1,&nCount );
      if( hr != S_OK )
      {
         pTypeInfo->lpVtbl->ReleaseVarDesc(pTypeInfo, pVarDesc);
         pTypeInfo->lpVtbl->ReleaseTypeAttr(pTypeInfo, pTypeAttr);
         pTypeInfo->lpVtbl->Release(pTypeInfo);
         /*m_nMethodCount = m_nVarCount = m_nDispInfoCount = 0; */
         /* free memory                                         */
         return 0;
      }
      m_pDispInfo[i+m_nVarCount].m_bstrName = SysAllocString( m_pDispInfo[i].m_bstrName );

      switch( pVarDesc->varkind )
      {
      case VAR_DISPATCH:
         m_pDispInfo[i].m_wFlag = DISPATCH_PROPERTYGET;
         m_pDispInfo[i+m_nVarCount].m_wFlag = DISPATCH_PROPERTYPUT;
         m_pDispInfo[i].m_vtOutputType = pVarDesc->elemdescVar.tdesc.vt;
         m_pDispInfo[i+m_nVarCount].m_vtOutputType = VT_EMPTY;
         m_pDispInfo[i+m_nVarCount].m_nParamCount = 1;
         m_pDispInfo[i+m_nVarCount].m_pParamTypes = (WORD*) hb_xgrab( 2* sizeof(WORD)); /*new WORD[2];*/
         m_pDispInfo[i+m_nVarCount].m_pParamTypes[0] = m_pDispInfo[i].m_vtOutputType;
         m_pDispInfo[i+m_nVarCount].m_pParamTypes[1] = 0;
         break;
      default:
         m_pDispInfo[i].m_wFlag = 0;
         m_pDispInfo[i+m_nVarCount].m_wFlag = 0;
         break;
      }
      m_pDispInfo[i].m_pOutput = ( VARIANT* ) hb_xgrab( sizeof( VARIANT ) );/* new VARIANT; */
      ::VariantInit( m_pDispInfo[i].m_pOutput );
      m_pDispInfo[i+m_nVarCount].m_pOutput = ( VARIANT* ) hb_xgrab( sizeof( VARIANT ) ); /* new VARIANT; */
      VariantInit( m_pDispInfo[i+m_nVarCount].m_pOutput );
      pTypeInfo->lpVtbl->ReleaseVarDesc( pTypeInfo, pVarDesc );
   }

   hb_ToOutDebug( "---------------- end ------------------" );

   pTypeInfo->lpVtbl->ReleaseTypeAttr( pTypeInfo, pTypeAttr );
   pTypeInfo->lpVtbl->Release( pTypeInfo );

   return m_nDispInfoCount;
}
/*----------------------------------------------------------------------*/
HB_FUNC( HB_AX_LOADTYPEINFO )
{
   hb_retni( LoadTypeInformation( ( IDispatch* ) hb_parnint( 1 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
#if 0
/*                                                                       */
/*  AdviseEvents( hObj, IID_* {...-...-.........}, @hSink, hashEvents )  */
/*                                                                       */
HB_FUNC( HB_AX_ADVISEEVENTS )
{
   IConnectionPointContainer*  pCPC = NULL;
   IConnectionPoint*           pCP = NULL;
   IUnknown*                   pIUnknown;
   HRESULT                     hr;
   DWORD                       dwCookie = 0;
   device_interface*           pdevice_interface = ( device_interface* ) hb_parnint( 1 );
   IID                         iid;
   BSTR                        bstrClassID;
   register IEventHandler*     thisobj;

   bstrClassID = hb_oleAnsiToSysString( hb_parcx( 2 ) );
   hr = IIDFromString( bstrClassID, ( LPIID ) &iid );
   SysFreeString( bstrClassID );
   if ( hr == S_OK )
   {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  1  >>>>>>>>>>>>" );
      thisobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof(MyRealIEventHandler ) );
      if ( !( thisobj ) )
      {
         hr = E_OUTOFMEMORY;
      }
      else
      {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  2  >>>>>>>>>>>>" );
         thisobj->lpVtbl = (IEventHandlerVtbl *) &IEventHandler_Vtbl;
         ((MyRealIEventHandler *) thisobj)->count = 0;

         hr = thisobj->lpVtbl->QueryInterface( thisobj, &IID_IUnknown, (void**) &pIUnknown );
         if( hr == S_OK )
         {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  3  >>>>>>>>>>>>");
            hr = pdevice_interface->lpVtbl->QueryInterface( pdevice_interface, &IID_IConnectionPointContainer, (void**)&pCPC);
            if( hr == S_OK )
            {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  4  >>>>>>>>>>>>");
               hr = pCPC->lpVtbl->FindConnectionPoint( pCPC, (REFIID)&iid, &pCP );
               if ( hr == S_OK )
               {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  5  >>>>>>>>>>>>");
                  hr = pCP->lpVtbl->Advise( pCP, pIUnknown, &dwCookie );
                  if ( hr == S_OK )
                  {
hb_ToOutDebug( "<<<<<<<<<<<<<<<  6  >>>>>>>>>>>>");
                     ((MyRealIEventHandler *) thisobj)->device_event_interface_iid = iid;
                     ((MyRealIEventHandler *) thisobj)->pIConnectionPoint = pCP;
                     ((MyRealIEventHandler *) thisobj)->dwEventCookie     = dwCookie;
                     ((MyRealIEventHandler *) thisobj)->pEvents           = hb_itemNew( hb_param( 4, HB_IT_ANY ) );

                     hb_stornl( (LONG) thisobj, 3 );
                  }
               }
               pCPC->lpVtbl->Release( pCPC );
               pCPC = NULL;
            }
            pIUnknown->lpVtbl->Release(pIUnknown);
            pIUnknown = NULL;
         }
      }
   }
   hb_retnl( hr );
}
#endif
/*----------------------------------------------------------------------*/
