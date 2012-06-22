/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper source code
 *
 * Copyright 2012 Przemyslaw Czerpak (druzus/at/poczta.onet.pl)
 * Copyright 2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbthread.h"
#include "hbvm.h"
#include "hbapierr.h"

#include "hbqt.h"
#include "hbqt_destroyer.h"
#include "hbqt_hbqslots.h"
#include "hbqt_hbqevents.h"

/* locks for MT mode, now just dummy definitions which checks
 * if all locks are correctly released.
 *
 * #define HBQT_BIND_LOCK        do {
 * #define HBQT_BIND_UNLOCK      } while( 0 );
 */
static HB_CRITICAL_NEW( s_qtMtx );
#define HBQT_BIND_LOCK     do { hb_threadEnterCriticalSection( &s_qtMtx );
#define HBQT_BIND_UNLOCK   hb_threadLeaveCriticalSection( &s_qtMtx ); } while( 0 );

void hbqt_bindDelSlots( PHB_ITEM pSenderObject );
void hbqt_bindDelEvents( PHB_ITEM pSenderObject );
int __hbqt_bindItemsInGlobalList();
int hbqt_bindIsHbObject( PHB_ITEM pObject );

static PHB_DYNS s_dynsym_NEW       = NULL;
static PHB_DYNS s_dynsym___CHILDS  = NULL;
static PHB_DYNS s_dynsym___SLOTS   = NULL;
static PHB_DYNS s_dynsym_SETSLOTS  = NULL;
static PHB_DYNS s_dynsym___EVENTS  = NULL;
static PHB_DYNS s_dynsym_SETEVENTS = NULL;

typedef struct _HBQT_BIND
{
   void *               qtObject;
   void *               hbObject;
   PHBQT_DEL_FUNC       pDelFunc;
   int                  iFlags;
   bool                 fDeleting;
   char                 szClassName[ HB_SYMBOL_NAME_LEN + 1 ];
   bool                 fEventFilterInstalled;
   int                  iThreadId;
   struct _HBQT_BIND *  next;
}
HBQT_BIND, * PHBQT_BIND;

typedef struct
{
   PHBQT_BIND s_hbqt_binds;
} HB_BIND_DATA, * PHB_BIND_DATA;

typedef struct
{
   int                  iThreadId;
   HBQDestroyer *       pDestroyer;
   HBQSlots *           pReceiverSlots;
   HBQEvents *          pReceiverEvents;
} HB_BIND_THREADDATA, * PHB_BIND_THREADDATA;

HB_FUNC_EXTERN( HB_THREADID );

static int s_bind_threadId = 0;

static int hbqt_bindGetThreadId()
{
   HBQT_BIND_LOCK
   s_bind_threadId++;
   HBQT_BIND_UNLOCK
   return s_bind_threadId;
}

#define __HBQT_WITH_MT_SUPPORT__NO
#ifdef __HBQT_WITH_MT_SUPPORT__
static void hbqt_bindInit( void * cargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindInit............0........." ) );
   ( ( PHB_BIND_DATA ) cargo )->s_hbqt_binds = NULL;
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindInit............1........." ) );
}
static void hbqt_bindRelease( void * cargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindRelease.........0........." ) );
   PHBQT_BIND hbqt_bind;

   while( ( ( PHB_BIND_DATA ) cargo )->s_hbqt_binds )
   {
      hbqt_bind = ( ( PHB_BIND_DATA ) cargo )->s_hbqt_binds;
      HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindRelease( %p, %s )", hbqt_bind->qtObject, hbqt_bind->szClassName ) );
      hbqt_bindDestroyHbObject( hb_arrayFromId( NULL, hbqt_bind->hbObject ) );
   }
   ( ( PHB_BIND_DATA ) cargo )->s_hbqt_binds = NULL;
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindRelease.........1........." ) );
}
static HB_TSD_NEW( s_bindData, sizeof( HB_BIND_DATA ), hbqt_bindInit, hbqt_bindRelease );
#define hbqt_bindGetData()       ( ( ( PHB_BIND_DATA ) hb_stackGetTSD( &s_bindData ) )->s_hbqt_binds )
#else
static PHB_BIND_DATA s_bindData = NULL;
#define hbqt_bindGetData()       ( ( ( PHB_BIND_DATA ) &s_bindData )->s_hbqt_binds )
#endif


static void hbqt_bindThreadInit( void * cargo )
{
   PHB_BIND_THREADDATA pBindThreadData = ( ( PHB_BIND_THREADDATA ) cargo );

   pBindThreadData->iThreadId = hbqt_bindGetThreadId();
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindThreadInit( %i )...STARTS",pBindThreadData->iThreadId ) );
   pBindThreadData->pDestroyer = new HBQDestroyer();
   pBindThreadData->pReceiverSlots = new HBQSlots();
   pBindThreadData->pReceiverEvents = new HBQEvents();
   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindThreadInit( %i )...ENDS",pBindThreadData->iThreadId ) );
}
static void hbqt_bindThreadRelease( void * cargo )
{
   PHB_BIND_THREADDATA pBindThreadData = ( ( PHB_BIND_THREADDATA ) cargo );
   int id = pBindThreadData->iThreadId;

   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindThreadRelease( %i )...STARTS", id ) );

   QList<PHB_ITEM> deleteIt;

   PHBQT_BIND bind;
   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();
   while( bind )
   {
      if( bind->iThreadId == id )
      {
         deleteIt << bind->hbObject;
      }
      bind = bind->next;
   }
   HBQT_BIND_UNLOCK

   while( ! deleteIt.isEmpty() )
   {
      if( hbqt_bindIsHbObject( ( PHB_ITEM ) deleteIt.at( 0 ) ) )
      {
         hbqt_bindDestroyHbObject( hb_arrayFromId( NULL, deleteIt.at( 0 ) ) );
         hb_itemRelease( ( PHB_ITEM ) deleteIt.at( 0 ) );
      }
      deleteIt.removeAt( 0 );
   }

   delete pBindThreadData->pDestroyer;
   delete pBindThreadData->pReceiverSlots;
   delete pBindThreadData->pReceiverEvents;

   HB_TRACE( HB_TR_DEBUG, ( "...hbqt_bindThreadRelease( %i )...ENDS",id ) );
}

static HB_TSD_NEW( s_bindThreadData, sizeof( HB_BIND_THREADDATA ), hbqt_bindThreadInit, hbqt_bindThreadRelease );
#define hbqt_bindGetThreadData()       ( ( PHB_BIND_THREADDATA ) hb_stackGetTSD( &s_bindThreadData ) )


static void hbqt_bind_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hbqt_bindGetData() = NULL;

   s_dynsym_NEW       = hb_dynsymGetCase( "NEW" );
   s_dynsym___CHILDS  = hb_dynsymGetCase( "__CHILDS" );
   s_dynsym___SLOTS   = hb_dynsymGetCase( "__SLOTS" );
   s_dynsym_SETSLOTS  = hb_dynsymGetCase( "SETSLOTS" );
   s_dynsym___EVENTS  = hb_dynsymGetCase( "__EVENTS" );
   s_dynsym_SETEVENTS = hb_dynsymGetCase( "SETEVENTS" );
}

static void hbqt_bind_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

PHB_ITEM hbqt_bindGetHbObject( PHB_ITEM pItem, void * qtObject, const char * szClassName, PHBQT_DEL_FUNC pDelFunc, int iFlags )
{
   int id = hbqt_bindGetThreadData()->iThreadId;

   char * pname = ( char * ) hb_xgrab( 200 );
   char * pname1 = ( char * ) hb_xgrab( 200 );
   HB_TRACE( HB_TR_DEBUG, ( "......HARBOUR_REQUEST_BIND_OBJECT( %p, %s, %i, %i, %s, %s )", qtObject, szClassName, iFlags, id, hb_procname( 0, pname, HB_TRUE ),  hb_procname( 1, pname1, HB_TRUE ) ) );
   hb_xfree( pname );
   hb_xfree( pname1 );

   PHB_ITEM pObject = NULL;

   if( qtObject == NULL )
      return pObject;

   PHB_SYMB pClassFunc = hb_dynsymGetSymbol( szClassName );
   if( pClassFunc == NULL )
      return pObject;

   PHBQT_BIND bind;

   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();
   while( bind )
   {
      if( bind->qtObject == qtObject )
      {
         HB_TRACE( HB_TR_DEBUG, ( "......hbqt_bindGetHbObject( %p ):if( bind->qtObject == qtObject )", qtObject ) );
         pObject = hb_arrayFromId( pItem, bind->hbObject );
         break;
      }
      bind = bind->next;
   }

   if( pObject == NULL && pClassFunc )
   {
      hb_vmPushSymbol( pClassFunc );
      hb_vmPushNil();
      hb_vmDo( 0 );

      if( HB_IS_OBJECT( hb_stackReturnItem() ) && hb_vmRequestQuery() == 0 )
      {
         if( pItem == NULL )
         {
            pItem = hb_itemNew( NULL );
         }
         pObject = hb_stackReturnItem();
         if( pItem != pObject )
         {
            hb_itemMove( pItem, pObject );
            pObject = pItem;
         }
         if( bind == NULL )
         {
            bind = ( PHBQT_BIND ) hb_xgrab( sizeof( HBQT_BIND ) );
            memset( bind, 0, sizeof( HBQT_BIND ) );
            bind->qtObject              = qtObject;
            bind->pDelFunc              = pDelFunc;
            bind->iFlags                = iFlags;
            bind->fDeleting             = false;
            bind->fEventFilterInstalled = false;
            bind->iThreadId             = id;
            hb_strncpy( bind->szClassName, szClassName, HB_SIZEOFARRAY( bind->szClassName ) - 1 );
            bind->next = hbqt_bindGetData();
            hbqt_bindGetData() = bind;
         }

         bind->hbObject = hb_arrayId( pObject );

         if( iFlags & HBQT_BIT_QOBJECT )
         {
            QObject * obj = ( QObject * ) qtObject;
            QString className = ( QString ) obj->metaObject()->className();

            if( pDelFunc != NULL )
            {
               QObject::connect( obj, SIGNAL( destroyed(QObject*) ), hbqt_bindGetThreadData()->pDestroyer, SLOT( destroyer() ) );
            }
            HB_TRACE( HB_TR_DEBUG, ( "......hbqt_bindGetHbObject_connected_to_destroy()( %p, %s )", qtObject, szClassName ) );

            hb_vmPushDynSym( s_dynsym_SETSLOTS );  /* initializes __Slots hash */
            hb_vmPush( pObject );
            hb_vmSend( 0 );

            hb_vmPushDynSym( s_dynsym_SETEVENTS );  /* initializes __Events hash */
            hb_vmPush( pObject );
            hb_vmSend( 0 );
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "......hbqt_bindGetHbObject( %p )", qtObject ) );
         }
      }
   }
   HBQT_BIND_UNLOCK

   return pObject;
}

PHB_ITEM hbqt_bindSetHbObject( PHB_ITEM pItem, void * qtObject, const char * szClassName, PHBQT_DEL_FUNC pDelFunc, int iFlags )
{
   int id = hbqt_bindGetThreadData()->iThreadId;

   Q_UNUSED( szClassName );
   HB_TRACE( HB_TR_DEBUG, ( "......ENTER hbqt_bindSetHbObject( %p, %s )", qtObject, szClassName ) );

   PHB_ITEM pObject = NULL;

   if( qtObject == NULL )
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return pObject;
   }

   PHBQT_BIND bind;

   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();

   pObject = hb_param( 0, HB_IT_OBJECT );
   if( pObject && hb_vmRequestQuery() == 0 )
   {
      if( pItem == NULL )
      {
         pItem = hb_itemNew( NULL );
      }
      if( pItem != pObject )
      {
         hb_itemMove( pItem, pObject );
         pObject = pItem;
      }

      bind = ( PHBQT_BIND ) hb_xgrab( sizeof( HBQT_BIND ) );
      memset( bind, 0, sizeof( HBQT_BIND ) );
      bind->qtObject              = qtObject;
      bind->pDelFunc              = pDelFunc;
      bind->iFlags                = iFlags;
      bind->fDeleting             = false;
      bind->fEventFilterInstalled = false;
      bind->iThreadId             = id;
      hb_strncpy( bind->szClassName, szClassName, HB_SIZEOFARRAY( bind->szClassName ) - 1 );
      bind->next = hbqt_bindGetData();
      hbqt_bindGetData() = bind;

      bind->hbObject = hb_arrayId( pObject );

      if( iFlags & HBQT_BIT_QOBJECT )
      {
         if( pDelFunc != NULL )
         {
            QObject::connect( ( QObject * ) qtObject, SIGNAL( destroyed(QObject*) ), hbqt_bindGetThreadData()->pDestroyer, SLOT( destroyer() ) );
         }
         HB_TRACE( HB_TR_DEBUG, ( "......hbqt_bindSetHbObject( QObject %p )...%s", qtObject, ( ( QObject * ) qtObject )->metaObject()->className() ) );
      }
   }
   HBQT_BIND_UNLOCK

   HB_TRACE( HB_TR_DEBUG, ( "......hbqt_bindSetHbObject returns PHB_ITEM = %p", pObject ) );
   return pObject;
}

void hbqt_bindDestroyHbObject( PHB_ITEM pObject )
{
   void * hbObject = hb_arrayId( pObject );

   if( hbObject )
   {
      PHBQT_BIND * bind_ptr, bind;

      HBQT_BIND_LOCK
      bind_ptr = &( hbqt_bindGetData() );
      while( ( bind = * bind_ptr ) != NULL )
      {
         if( bind->hbObject == hbObject )
         {
            bool fDelQtObject = false;

            if( bind->iFlags & HBQT_BIT_OWNER )
            {
               if( bind->iFlags & HBQT_BIT_QOBJECT )
               {
                  if( ( ( QObject * ) bind->qtObject )->metaObject()->className() != ( const char * ) "QAction" )
                     if( ( ( QObject * ) bind->qtObject )->parent() == NULL )
                        fDelQtObject = true;
               }
               else
                  fDelQtObject = true;
            }

            HB_TRACE( HB_TR_DEBUG, ( ".........HARBOUR_DESTROY_BEGINS( %p, %s, %i )", bind->qtObject, bind->szClassName, bind->iFlags ) );
            * bind_ptr = bind->next;

            if( fDelQtObject )
            {
               if( bind->pDelFunc != NULL )
               {
                  if( bind->iFlags & HBQT_BIT_QOBJECT )
                  {
                     if( bind->iThreadId == 1 && ( ( QObject * ) bind->qtObject )->objectName() == "PleaseDoNotDelete" )  /* This is a hack: will be removed a bit later */
                     {
                        HB_TRACE( HB_TR_DEBUG, ( ".........HARBOUR_NOT_DESTROYING_ACTUAL_QT_OBJECT( %p, %s, PleaseDoNotDelete )", bind->qtObject, bind->szClassName ) );
                        fDelQtObject = false;
                     }
                  }
                  if( fDelQtObject )
                  {
                     HB_TRACE( HB_TR_DEBUG, ( ".........HARBOUR_DESTROYING_ACTUAL_QT_OBJECT( %p, %s )", bind->qtObject, bind->szClassName ) );
                     bind->fDeleting = true;
                     bind->pDelFunc( bind->qtObject, bind->iFlags );
                     bind->fDeleting = false;
                  }
               }
            }
            hb_xfree( bind );
            break;
         }
         bind_ptr = &bind->next;
      }
      HBQT_BIND_UNLOCK
   }
}

void hbqt_bindDestroyQtObject( void * qtObject )
{
   HB_TRACE( HB_TR_DEBUG, ( "............QT_DESTROY_BEGINS( %p )..............", qtObject ) );
   if( qtObject && hb_vmRequestReenter() )
   {
      PHBQT_BIND * bind_ptr, bind;

      HBQT_BIND_LOCK
      bind_ptr = &( hbqt_bindGetData() );
      while( ( bind = * bind_ptr ) != NULL )
      {
         if( bind->qtObject == qtObject )
         {
            HB_TRACE( HB_TR_DEBUG, ( "............QT_DESTROYING( %p, %s )", qtObject, bind->szClassName ) );
            * bind_ptr = bind->next;
            if( ! bind->fDeleting )
            {
               hb_xfree( bind );
               HB_TRACE( HB_TR_DEBUG, ( "............QT_DESTROYED()" ) );
            }
            break;
         }
         bind_ptr = &bind->next;
      }
      HBQT_BIND_UNLOCK

      hb_vmRequestRestore();
   }
   HB_TRACE( HB_TR_DEBUG, ( "............QT_DESTROY_ENDS( %p )..............", qtObject ) );
}

HBQSlots * hbqt_bindGetReceiverSlotsByHbObject( PHB_ITEM pObject )
{
   HBQSlots * pReceiverSlot = NULL;
   if( pObject != NULL )
   {
      void * hbObject = hb_arrayId( pObject );
      PHBQT_BIND bind;

      HBQT_BIND_LOCK
      bind = hbqt_bindGetData();
      while( bind )
      {
         if( bind->hbObject == hbObject )
         {
            pReceiverSlot = hbqt_bindGetThreadData()->pReceiverSlots;
            HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindGetReceiverSlotByHbObject( %p )", bind->qtObject ) );
            break;
         }
         bind = bind->next;
      }
      HBQT_BIND_UNLOCK
   }
   return pReceiverSlot;
}

HBQEvents * hbqt_bindGetReceiverEventsByHbObject( PHB_ITEM pObject )
{
   HBQEvents * pReceiverEvents = NULL;
   if( pObject != NULL )
   {
      void * hbObject = hb_arrayId( pObject );
      PHBQT_BIND bind;

      HBQT_BIND_LOCK
      bind = hbqt_bindGetData();
      while( bind )
      {
         if( bind->hbObject == hbObject )
         {
            pReceiverEvents = hbqt_bindGetThreadData()->pReceiverEvents;
            if( ! bind->fEventFilterInstalled )
            {
               bind->fEventFilterInstalled = true;
               hbqt_bindGetThreadData()->pReceiverEvents->hbInstallEventFilter( pObject );
            }
            HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindGetReceiverEventsByHbObject( %p )", bind->qtObject ) );
            break;
         }
         bind = bind->next;
      }
      HBQT_BIND_UNLOCK
   }
   return pReceiverEvents;
}

PHB_ITEM hbqt_bindGetHbObjectByQtObject( void * qtObject )
{
   PHB_ITEM pObject = NULL;

   if( qtObject != NULL )
   {
      PHBQT_BIND bind;

      HBQT_BIND_LOCK
      bind = hbqt_bindGetData();
      while( bind )
      {
         if( bind->qtObject == qtObject )
         {
            pObject = hb_arrayFromId( NULL, bind->hbObject );
            break;
         }
         bind = bind->next;
      }
      HBQT_BIND_UNLOCK
   }
   return pObject;
}

int hbqt_bindIsHbObject( PHB_ITEM pObject )
{
   PHBQT_BIND bind;

   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();
   while( bind )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindIsHbObject( %p )", bind->qtObject ) );

      if( bind->hbObject == pObject )
      {
         return 1;
      }
      bind = bind->next;
   }
   HBQT_BIND_UNLOCK

   return 0;
}

void * hbqt_bindGetQtObject( PHB_ITEM pObject )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindGetQtObject()" ) );

   void * hbObject = hb_arrayId( pObject );
   void * qtObject = NULL;

   if( hbObject )
   {
      PHBQT_BIND bind;

      HBQT_BIND_LOCK
      bind = hbqt_bindGetData();
      while( bind )
      {
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindGetQtObject( %p )", bind->qtObject ) );

         if( bind->hbObject == hbObject )
         {
            qtObject = bind->qtObject;
            break;
         }
         bind = bind->next;
      }
      HBQT_BIND_UNLOCK
   }
   return qtObject;
}

void hbqt_bindSetOwner( void * qtObject, HB_BOOL fOwner )
{
   PHBQT_BIND bind;

   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();
   while( bind )
   {
      if( bind->qtObject == qtObject )
      {
         if( fOwner == HB_TRUE )
            bind->iFlags = bind->iFlags | HBQT_BIT_OWNER;
         else
            bind->iFlags = bind->iFlags & ~( HBQT_BIT_OWNER );
         break;
      }
      bind = bind->next;
   }
   HBQT_BIND_UNLOCK
}

void hbqt_bindAddSlot( PHB_ITEM pSenderObject, int iSignalid, PHB_ITEM pCode )
{
   if( HB_IS_BLOCK( pCode ) && hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___SLOTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iSignalid );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex,
                                     HB_HASH_AUTOADD_ASSIGN );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) )
            hb_arrayAdd( pArray, pCode );
         else
            hb_errRT_BASE( EG_BOUND, 4005, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      hb_vmRequestRestore();
   }
}

void hbqt_bindDelSlot( PHB_ITEM pSenderObject, int iSignalid, PHB_ITEM pCode )
{
   if( hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___SLOTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iSignalid );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex, 0 );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) )
         {
            if( pCode == NULL )
            {
               HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelSlot( PHB_ITEM pSenderObject, int iSignalid, PHB_ITEM pCode )" ) );
               hb_arraySize( pArray, 0 );
            }
            else
            {
               void * id = hb_codeblockId( pCode );
               HB_SIZE nPos;
               for( nPos = hb_arrayLen( pArray ); nPos; --nPos )
               {
                  if( id == hb_codeblockId( hb_arrayGetItemPtr( pArray, nPos ) ) )
                  {
                     hb_arrayDel( pArray, nPos );
                     hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
                     break;
                  }
               }
            }
         }
      }
      hb_vmRequestRestore();
   }
}

void hbqt_bindDelSlots( PHB_ITEM pSenderObject )
{
   if( pSenderObject )
   {
      if( hb_vmRequestReenter() )
      {
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelSlots( PHB_ITEM pSenderObject    0 %p )", pSenderObject ) );
         hb_vmPushDynSym( s_dynsym___SLOTS );
         hb_vmPush( pSenderObject );
         hb_vmSend( 0 );
         if( hb_vmRequestQuery() == 0 )
         {
            HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelSlots( PHB_ITEM pSenderObject )" ) );
            hb_hashClear( hb_stackReturnItem() );
         }
         hb_vmRequestRestore();
      }
   }
}

PHB_ITEM hbqt_bindGetSlots( PHB_ITEM pSenderObject, int iSignalid )
{
   PHB_ITEM pSlots = NULL;

   if( hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___SLOTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iSignalid );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex, 0 );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) && hb_arrayLen( pArray ) > 0 )
            pSlots = hb_itemNew( pArray );
      }
      hb_vmRequestRestore();
   }
   return pSlots;
}

void hbqt_bindAddEvent( PHB_ITEM pSenderObject, int iEventId, PHB_ITEM pCode )
{
   if( HB_IS_BLOCK( pCode ) && hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___EVENTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iEventId );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex, HB_HASH_AUTOADD_ASSIGN );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) )
            hb_arrayAdd( pArray, pCode );
         else
            hb_errRT_BASE( EG_BOUND, 4005, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      hb_vmRequestRestore();
   }
}

void hbqt_bindDelEvent( PHB_ITEM pSenderObject, int iEventId, PHB_ITEM pCode )
{
   if( hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___EVENTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iEventId );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex, 0 );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) )
         {
            if( pCode == NULL )
            {
               HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelEvent( %i )", iEventId ) );
               hb_arraySize( pArray, 0 );
            }
            else
            {
               void * id = hb_codeblockId( pCode );
               HB_SIZE nPos;
               for( nPos = hb_arrayLen( pArray ); nPos; --nPos )
               {
                  if( id == hb_codeblockId( hb_arrayGetItemPtr( pArray, nPos ) ) )
                  {
                     hb_arrayDel( pArray, nPos );
                     hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
                     break;
                  }
               }
            }
         }
      }
      hb_vmRequestRestore();
   }
}

void hbqt_bindDelEvents( PHB_ITEM pSenderObject )
{
   if( pSenderObject )
   {
      if( hb_vmRequestReenter() )
      {
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelEvents( PHB_ITEM pSenderObject    0 %p )", pSenderObject ) );
         hb_vmPushDynSym( s_dynsym___EVENTS );
         hb_vmPush( pSenderObject );
         hb_vmSend( 0 );
         if( hb_vmRequestQuery() == 0 )
         {
            HB_TRACE( HB_TR_DEBUG, ( "hbqt_bindDelEvents( PHB_ITEM pSenderObject )" ) );
            hb_hashClear( hb_stackReturnItem() );
         }
         hb_vmRequestRestore();
      }
   }
}

PHB_ITEM hbqt_bindGetEvents( PHB_ITEM pSenderObject, int iEventId )
{
   PHB_ITEM pEvents = NULL;

   if( hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___EVENTS );
      hb_vmPush( pSenderObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray, pIndex;

         pIndex = hb_itemPutNI( hb_stackAllocItem(), iEventId );
         pArray = hb_hashGetItemPtr( hb_stackReturnItem(), pIndex, 0 );
         hb_stackPop();

         if( pArray && HB_IS_ARRAY( pArray ) && hb_arrayLen( pArray ) > 0 )
            pEvents = hb_itemNew( pArray );
      }
      hb_vmRequestRestore();
   }
   return pEvents;
}

void hbqt_bindAddChild( PHB_ITEM pObject, PHB_ITEM pChild )
{
#ifdef HBQT_BIND_DEBUG
   QObject * qtParent = hbqt_bindGetQtObject( pObject );
   QObject * qtChild = hbqt_bindGetQtObject( pChild );

   if( qtParent == NULL || qtChild == NULL ||
       HBQT_PARENT( qtChild ) != qtParent )
   {
      hb_errRT_BASE( EG_BOUND, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
#endif

   if( hb_vmRequestReenter() )
   {
      PHB_ITEM pArray;

      hb_vmPushDynSym( s_dynsym___CHILDS );
      hb_vmPush( pObject );
      hb_vmSend( 0 );
      pArray = hb_stackReturnItem();
      if( hb_vmRequestQuery() == 0 )
      {
         if( HB_IS_ARRAY( pArray ) )
            hb_arrayAdd( pArray, pChild );
         else
            hb_errRT_BASE( EG_BOUND, 4002, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      hb_vmRequestRestore();
   }
}

void hbqt_bindDelChild( PHB_ITEM pObject, PHB_ITEM pChild )
{
#ifdef HBQT_BIND_DEBUG
   QObject * qtParent = hbqt_bindGetQtObject( pObject );
   QObject * qtChild = hbqt_bindGetQtObject( pChild );

   if( qtParent == NULL || qtChild == NULL ||
       HBQT_PARENT( qtChild ) != NULL )
   {
      hb_errRT_BASE( EG_BOUND, 4003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
#endif

   if( hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( s_dynsym___CHILDS );
      hb_vmPush( pObject );
      hb_vmSend( 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         PHB_ITEM pArray = hb_stackReturnItem();
         if( hb_arrayDel( pArray, hb_arrayScan( pArray, pChild,
                                                NULL, NULL, HB_TRUE ) ) )
            hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
         else
            hb_errRT_BASE( EG_BOUND, 4004, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      hb_vmRequestRestore();
   }
}

/* this function has to be executed from HBQT object destructor
 * with Self parameter:
 *    __hbqt_destroy( Self )
 * alternatively it can be used directly as destructor method.
 */
HB_FUNC( __HBQT_DESTROY )
{
   PHB_ITEM pObject = hb_stackSelfItem();
   if( pObject )
      hbqt_bindDestroyHbObject( pObject );
}

int __hbqt_bindItemsInGlobalList( void )
{
   int i = 0;
   PHBQT_BIND bind;

   HBQT_BIND_LOCK
   bind = hbqt_bindGetData();
   while( bind )
   {
      i++;
      HB_TRACE( HB_TR_DEBUG, ( "_____OnExit______( %p, %s )", bind->qtObject, bind->szClassName ) );
      bind = bind->next;
   }
   HBQT_BIND_UNLOCK
   return i;
}

HB_FUNC( __HBQT_ITEMSINGLOBALLIST )
{
   hb_retni( __hbqt_bindItemsInGlobalList() );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqt_bind_init_ )
   hb_vmAtInit( hbqt_bind_init, NULL );
   hb_vmAtExit( hbqt_bind_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqt_bind_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqt_bind_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqt_bind_init_ )
   #include "hbiniseg.h"
#endif
