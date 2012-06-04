/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbvmint.h"

#include "hbqt.h"

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbapicls.h"
#include "hbstack.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

/*----------------------------------------------------------------------*/

static HB_GARBAGE_FUNC( Q_release )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->ph )
      p->func( p );
}

static HB_GARBAGE_FUNC( Q_mark )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "HB_GARBAGE_FUNC( Q_mark )  p=%p  p->ph=%p  p->mark=%p", p, p->ph, p->mark ) );
   if( p && p->ph && p->mark )
      p->mark( p );
}

static const HB_GC_FUNCS QT_gcFuncs =
{
   Q_release,
   Q_mark
};

const HB_GC_FUNCS * hbqt_gcFuncs( void )
{
   return &QT_gcFuncs;
}

static void * s_hbqt_GCPointerFromItem( PHB_ITEM pObj )
{
   static PHB_DYNS s_pDyns_hPPtrAssign = NULL;

   if( hb_itemType( pObj ) == HB_IT_OBJECT )
   {
      if( ! s_pDyns_hPPtrAssign )
         s_pDyns_hPPtrAssign = hb_dynsymGetCase( "PPTR" );

      hb_vmPushDynSym( s_pDyns_hPPtrAssign );
      hb_vmPush( pObj );
      hb_vmSend( 0 );

      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_itemGetPtrGC( hb_param( -1, HB_IT_POINTER ), hbqt_gcFuncs() );
      if( p && p->ph )
         return p->ph;
   }
   return NULL;
}

void * hbqt_par_ptr( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_ptr( %d )", iParam ) );
#ifdef __HBQT_REVAMP__
   return hbqt_bindGetQtObject( hb_param( iParam, HB_IT_OBJECT ) );
#else
   return s_hbqt_GCPointerFromItem( hb_param( iParam, HB_IT_ANY ) );
#endif
}

void * hbqt_get_ptr( PHB_ITEM pObj )
{
#ifdef __HBQT_REVAMP__
   return hbqt_bindGetQtObject( pObj );
#else
   return s_hbqt_GCPointerFromItem( pObj );
#endif
}

static void s_hbqt_set_ptr( PHB_ITEM pSelf, void * ptr )
{
   static PHB_DYNS s_pDyns_hPPtrAssign = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "s_hbqt_set_ptr( pSelf=%p, ptr=%p )", pSelf, ptr ) );

   /* get the position of _PPTR member, the
      leading underscore because I want to write to it */

   if( pSelf )
   {
      if( ! s_pDyns_hPPtrAssign )
         s_pDyns_hPPtrAssign = hb_dynsymGetCase( "_PPTR" );

      /* push the _PPTR address */
      hb_vmPushDynSym( s_pDyns_hPPtrAssign );

      /* push the instance we want change _PPTR value
         it was already on the stack, but we push a pointer to it */
      hb_vmPush( pSelf );

      /* push the actual value */
      hb_vmPushPointerGC( ptr );

      /* run the command with 1 parameter */
      hb_vmSend( 1 );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "s_hbqt_set_ptr(): returns NULL" ) );
      return; /* TODO: Still better if RTE. */
   }
}

void hbqt_itemPushReturn( void* ptr, PHB_ITEM pSelf )
{
   HB_TRACE( HB_TR_DEBUG, ( "itemPushReturn( ptr =%p, pSelf=%p )", ptr, pSelf ) );

   /* get the position of _PPTR member, the
      leading underscore because I want to write to it */

   if( ! pSelf )
      pSelf = hb_stackSelfItem();

   s_hbqt_set_ptr( pSelf, ptr );

   if( hb_stackReturnItem() != pSelf )
      hb_itemReturn( pSelf );
}
#if 0
HBQT_GC_T * hbqt_par_ptrGC( int iParam )
{
   static PHB_DYNS s_pDyns_hPPtr = NULL;

   PHB_ITEM pItem = hb_param( iParam, HB_IT_OBJECT );
   if( pItem )
   {
      if( ! s_pDyns_hPPtr )
         s_pDyns_hPPtr = hb_dynsymGetCase( "PPTR" );

      hb_vmPushDynSym( s_pDyns_hPPtr );
      hb_vmPush( pItem );
      hb_vmSend( 0 );

      return ( HBQT_GC_T * ) hb_itemGetPtrGC( hb_param( -1, HB_IT_POINTER ), hbqt_gcFuncs() );
   }
   return NULL;
}
#endif
HB_FUNC( HBQT_ISOBJECT )
{
   PHB_ITEM pItem;

   if( ( pItem = hb_param( 1, HB_IT_OBJECT ) ) != NULL )
   {
      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pItem );
      hb_vmSend( 0 );

      hb_retl( hb_param( -1, HB_IT_POINTER ) != NULL );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( __HBQT_PTR )
{
   PHB_ITEM pParam = hb_param( 1, HB_IT_ANY );

   if( hb_itemType( pParam ) & HB_IT_OBJECT )
   {
      PHB_ITEM pRetVal;

      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pParam );
      hb_vmSend( 0 );

      if( ( pRetVal = hb_param( -1, HB_IT_POINTER ) ) != NULL )
         return;
   }

   hb_itemReturn( pParam );
}

void hbqt_errRT_ARG( void )
{
   hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

void hbqt_par_detach_ptrGC( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_detach_ptrGC( %d )", iParam ) );

   if( HB_ISOBJECT( iParam ) )
   {
      PHB_ITEM pObj = hb_param( iParam, HB_IT_ANY );
      HBQT_GC_T * p;

      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pObj );
      hb_vmSend( 0 );

      p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), -1 );

      if( p && p->ph )
         p->bNew = false;
   }
}

HB_FUNC( __HBQT_ISPOINTER )
{
#ifdef __HBQT_REVAMP__
   PHB_ITEM pObject = hb_stackSelfItem();
   if( pObject == NULL )
      pObject = hb_param( 1, HB_IT_OBJECT );
   if( pObject )
      hb_retl( ! ( hbqt_bindGetQtObject( pObject ) == NULL ) );
   else
      hb_retl( HB_FALSE );
#else
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   hb_retl( p && p->ph );
#endif
}

HB_FUNC( HBQT_ISEQUAL )
{
   hb_retl( hbqt_par_ptr( 1 ) == hbqt_par_ptr( 2 ) );
}

HB_FUNC( __HBQT_ERROR )
{
   HB_TRACE( HB_TR_DEBUG, ( "In __HBQT_ERROR" ));
   hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( __HBQT_SETUTF8 )
{
   PHB_CODEPAGE cdp = hb_cdpFindExt( "UTF8" );

   if( cdp )
      hb_vmSetCDP( cdp );
}

PHB_ITEM hbqt_defineClassBegin( const char * pszClsName, PHB_ITEM s_oClass, const char * pszParentClsStr )
{
   static PHB_DYNS s__CLSLOCKDEF = NULL;

   PHB_ITEM oClass = NULL;

   if( s__CLSLOCKDEF == NULL )
      s__CLSLOCKDEF = hb_dynsymGetCase( "__CLSLOCKDEF" );

   hb_vmPushDynSym( s__CLSLOCKDEF );
   hb_vmPushNil();
   hb_vmPushItemRef( s_oClass );
   hb_vmDo( 1 );

   if( hb_itemGetL( hb_stackReturnItem() ) && pszParentClsStr )
   {
      static PHB_DYNS s___HBCLASS = NULL;

      char * pszSingleClsNameBuf = ( char * ) hb_xgrab( strlen( pszParentClsStr ) + 1 );

      if( s___HBCLASS == NULL )
         s___HBCLASS = hb_dynsymGetCase( "HBCLASS" );

      PHB_ITEM pClsName = hb_itemNew( NULL );
      PHB_ITEM pSuper = hb_itemNew( NULL );
      PHB_ITEM pSym_ClsFunc = hb_itemNew( NULL );

      hb_itemPutC( pClsName, pszClsName );

      HB_SIZE nPos = 0;
      HB_SIZE nStart = 0;

      /* array with parent classes (at least ONE) */
      hb_arrayNew( pSuper, 0 );

      HB_TRACE( HB_TR_DEBUG, ("%s: dCB 3", pszClsName ) );

      while( pszParentClsStr[ nPos++ ] )
      {
         if( ! pszParentClsStr[ nPos ] || ( pszParentClsStr[ nPos ] == ',' && pszParentClsStr[ nPos + 1 ] == ' ' ) )
         {
            PHB_ITEM pItem = hb_itemNew( NULL );

            memcpy( pszSingleClsNameBuf, pszParentClsStr + nStart, nPos - nStart );
            pszSingleClsNameBuf[ nPos - nStart ] = '\0';

            hb_itemPutC( pItem, pszSingleClsNameBuf );
            hb_arrayAdd( pSuper, hb_itemPutSymbol( pItem, hb_dynsymGetCase( pszSingleClsNameBuf )->pSymbol ) );

            hb_itemRelease( pItem );

            nStart = nPos + 2;
         }
      }

      hb_xfree( pszSingleClsNameBuf );

      hb_itemPutSymbol( pSym_ClsFunc, hb_dynsymGetCase( pszClsName )->pSymbol );

      hb_vmPushDynSym( s___HBCLASS );
      hb_vmPushNil();
      hb_vmDo( 0 );

      /* TODO: change this hack */
      char test[ HB_SYMBOL_NAME_LEN + 1 ];
      hb_snprintf( test, sizeof( test ), "HB_%s", pszClsName );

      hb_itemPutSymbol( pSym_ClsFunc, hb_dynsymGetCase( test )->pSymbol );

      hb_objSendMsg( hb_stackReturnItem(), "NEW", 3, pClsName, pSuper, pSym_ClsFunc );

      oClass = hb_itemNew( hb_stackReturnItem() );

      hb_itemRelease( pSym_ClsFunc );
      hb_itemRelease( pSuper );
      hb_itemRelease( pClsName );

      hb_objSendMsg( oClass, "CREATE", 0 );
      hb_objSendMsg( oClass, "INSTANCE", 0 );
   }

   return oClass;
}

void hbqt_defineClassEnd( PHB_ITEM s_oClass, PHB_ITEM oClass )
{
   if( s_oClass && oClass )
   {
      static PHB_DYNS s__CLSUNLOCKDEF = NULL;

      if( s__CLSUNLOCKDEF == NULL )
         s__CLSUNLOCKDEF = hb_dynsymGetCase( "__CLSUNLOCKDEF" );

      hb_vmPushDynSym( s__CLSUNLOCKDEF );
      hb_vmPushNil();
      hb_vmPushItemRef( s_oClass );
      hb_vmPush( oClass );
      hb_vmDo( 2 );

      hb_itemRelease( oClass );
   }
}

PHB_ITEM hbqt_create_object( void * pObject, const char * pszObjectName )
{
   PHB_ITEM pRetVal;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "create_object %s", pszObjectName ) );

   hb_vmPushDynSym( hb_dynsymGet( pszObjectName ) );
   hb_vmPushNil();
   hb_vmDo( 0 );

   pItem = hb_itemPutPtr( NULL, pObject );
   pRetVal = hb_itemNew( hb_stackReturnItem() );
   hb_objSendMsg( pRetVal, "_PPTR", 1, pItem );
   hb_itemReturnRelease( pRetVal );
   hb_itemRelease( pItem );

   return hb_stackReturnItem();
}

PHB_ITEM hbqt_create_objectGC( void * pObject, const char * pszObjectName )
{
   PHB_ITEM pItem, pRetVal;

   //HB_TRACE( HB_TR_DEBUG, ( "create_object_GC %s", pszObjectName ) );

   hb_vmPushDynSym( hb_dynsymGet( pszObjectName ) );
   hb_vmPushNil();
   hb_vmDo( 0 );

   pRetVal = hb_itemNew( hb_stackReturnItem() );

   pItem = hb_itemPutPtrGC( NULL, pObject );
   hb_objSendMsg( pRetVal, "_PPTR", 1, pItem );
   hb_itemReturnRelease( pRetVal );
   //hb_itemReturn( pRetVal );
   hb_itemRelease( pItem );

   //HB_TRACE( HB_TR_DEBUG, ( ".............................create_object_GC %s", pszObjectName ) );

   return hb_stackReturnItem();
}

HB_BOOL hbqt_obj_isDerivedFrom( PHB_ITEM pItem, const char * pszClsName )
{
   return hb_clsIsParent( hb_objGetClass( pItem ), pszClsName );
}

/* Checks that the param iParam is an object and a subclass of pszClsName */
HB_BOOL hbqt_par_isDerivedFrom( int iParam, const char * pszClsName )
{
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_isDerivedFrom( %i, %s )", iParam, pszClsName ) );

   if( ( pItem = hb_param( iParam, HB_IT_OBJECT ) ) != NULL )
   {
#ifdef __HBQT_REVAMP__
      if( hbqt_bindGetQtObject( pItem ) == NULL )
         hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      else
         return hbqt_obj_isDerivedFrom( pItem, pszClsName );
#else
      if( s_hbqt_GCPointerFromItem( pItem ) == NULL )
         hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      else
         return hbqt_obj_isDerivedFrom( pItem, pszClsName );
#endif
   }
   return HB_FALSE;
}

int hbqt_QtConnect( QObject *sender, const char * pszSignal, QObject *receiver, const char * pszSlot )
{
   HB_TRACE( HB_TR_DEBUG, ( "_Connect %s with slot %s", pszSignal, pszSlot ) );

   int nResult = 1;

   if( sender && receiver )
   {
      QString signal = pszSignal;
      QByteArray theSignal = QMetaObject::normalizedSignature( signal.toAscii() );
      QString slot = pszSlot;
      QByteArray theSlot = QMetaObject::normalizedSignature( slot.toAscii() );

      if( QMetaObject::checkConnectArgs( theSignal, theSlot ) )
      {
         int signalId = sender->metaObject()->indexOfSignal( theSignal );
         if( signalId != -1 )
         {
            int slotId = receiver->metaObject()->indexOfMethod( theSlot );
            if( slotId != -1 )
            {
               if( QMetaObject::connect( sender, signalId, receiver, slotId, Qt::AutoConnection ) )
               {
                  nResult = 0;
                  HB_TRACE( HB_TR_DEBUG, ( "SIGNAL2SLOT ok" ) );
               }
               else
                  nResult = 8;
            }
            else
               nResult = 7;
         }
         else
            nResult = 6;
      }
      else
         nResult = 5;
   }
   else
      nResult = 9;  // Qt objects not active

   HB_TRACE( HB_TR_DEBUG, ( "_Connect returns: %d", nResult ) );
   return nResult;
}

HB_FUNC( HBQT_CONNECT )
{
   HB_BOOL ret = HB_FALSE;

   if( hb_pcount() == 4 && HB_ISCHAR( 2 ) && HB_ISCHAR( 4 ) && hbqt_par_isDerivedFrom( 1, "QOBJECT" ) && hbqt_par_isDerivedFrom( 3, "QOBJECT" ) )
   {
      void * pText01 = NULL;
      void * pText02 = NULL;
      if( hbqt_QtConnect( ( QObject* ) hbqt_par_ptr( 1 ), hb_parstr_utf8( 2, &pText01, NULL ), ( QObject* ) hbqt_par_ptr( 3 ), hb_parstr_utf8( 4, &pText02, NULL ) ) == 0 )
      {
         ret = HB_TRUE;
      }
      hb_strfree( pText01 );
      hb_strfree( pText02 );
   }
   hb_retl( ret );
}

/*----------------------------------------------------------------------*/

#endif
