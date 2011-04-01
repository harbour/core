/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqevents.h"

#include <QtCore/QPointer>
#include <QtCore/QVariant>

/*----------------------------------------------------------------------*/

#include <QtCore/QStringList>

/*----------------------------------------------------------------------*/

static QList<QEvent::Type> s_lstEvent;
static QList<QByteArray> s_lstCreateObj;
static QList<PHBQT_EVENT_FUNC> s_pEventAllocateCallback;


void hbqt_events_register_createobj( QEvent::Type eventtype, QByteArray szCreateObj, PHBQT_EVENT_FUNC pCallback )
{
   int iIndex = s_lstEvent.indexOf( eventtype );

   if( iIndex == -1 )
   {
      s_lstEvent << eventtype;
      s_lstCreateObj << szCreateObj.toUpper();
      s_pEventAllocateCallback << pCallback;
   }
   else
   {
      s_lstCreateObj[ iIndex ] = szCreateObj.toUpper();
      s_pEventAllocateCallback[ iIndex ] = pCallback;
   }
}

void hbqt_events_unregister_createobj( QEvent::Type eventtype )
{
   int iIndex = s_lstEvent.indexOf( eventtype );

   if( iIndex > -1 )
   {
      s_lstEvent.removeAt( iIndex );
      s_lstCreateObj.removeAt( iIndex );
      s_pEventAllocateCallback.removeAt( iIndex );
   }
}

/*----------------------------------------------------------------------*/

HBQEvents::HBQEvents( QObject * parent ) : QObject( parent )
{
}

/* QUESTION: Shouldn't all events be disconnected at this point? */
HBQEvents::~HBQEvents()
{
   HB_TRACE( HB_TR_DEBUG, ( "      HBQEvents::~HBQEvents()" ) );
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         HB_TRACE( HB_TR_DEBUG, ( "      HBQEvents::~HBQEvents() %d", i ) );

         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;

         HB_TRACE( HB_TR_DEBUG, ( "                            X" ) );
      }
   }
}

bool HBQEvents::hbConnect( PHB_ITEM pObj, int iEvent, PHB_ITEM bBlock )
{
   HB_SYMBOL_UNUSED( pObj   );
   HB_SYMBOL_UNUSED( iEvent );
   HB_SYMBOL_UNUSED( bBlock );

   QObject * object = ( QObject* ) hbqt_pPtrFromObj( 1 );          /* get sender    */

   if( object )
   {
      PHB_ITEM codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) );

      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", iEvent, "P" );  /* Make it a unique identifier */

      listBlock << codeblock;

      object->setProperty( prop, ( int ) listBlock.size() );

      return true;
   }
   return false;
}

bool HBQEvents::hbDisconnect( PHB_ITEM pObj, int iEvent )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject* ) hbqt_pPtrFromObj( 1 );

   if( object )
   {
      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", iEvent, "P" );    /* Make it a unique identifier */

      int i = object->property( prop ).toInt();
      if( i > 0 && i <= listBlock.size() )
      {
         hb_itemRelease( listBlock.at( i - 1 ) );
         listBlock[ i - 1 ] = NULL;
         object->setProperty( prop, QVariant() );

         HB_TRACE( HB_TR_DEBUG, ( "      QT_EVENTS_DISCONNECT: %i", iEvent ) );
         return true;
      }
   }
   return false;
}

bool HBQEvents::hbClear()
{
   HB_TRACE( HB_TR_DEBUG, ( "      HBQEvents::hbClear()" ) );
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   listBlock.clear();
   return true;
}

/* DO NOT Reformat */
bool HBQEvents::eventFilter( QObject * object, QEvent * event )
{
   if( object )
   {
      QEvent::Type eventtype = event->type();
      if( ( int ) eventtype > 0 )
      {
         if( eventtype == QEvent::Close )
         {
             event->ignore();
         }
         char prop[ 20 ];
         hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", eventtype, "P" );

         int found = object->property( prop ).toInt();
         if( found > 0 )
         {
            if( found <= listBlock.size() &&  listBlock.at( found - 1 ) != NULL )
            {
               int eventId = s_lstEvent.indexOf( eventtype );
               if( eventId > -1 )
               {
                  PHBQT_EVENT_FUNC pCallback = s_pEventAllocateCallback.at( eventId );
                  if( pCallback )
                  {
                     if( hb_vmRequestReenter() )
                     {
                        PHB_ITEM pItem = hb_itemNew( hbqt_create_objectGC( ( * pCallback )( event, false ), s_lstCreateObj.at( eventId ) ) );
                        hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 1, pItem );
                        hb_itemRelease( pItem );
                        hb_vmRequestRestore();
                     }
                  }
               }
            }
         }
      }
   }
   return false;
}

HB_FUNC( __HBQT_EVENTS_CONNECT )
{
   int nResult;
   HBQEvents * t_events = hbqt_par_HBQEvents( 1 );

   if( t_events )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 ); /* get sender    */

      if( object )
      {
         PHB_ITEM codeblock = hb_itemNew( hb_param( 4, HB_IT_BLOCK | HB_IT_BYREF ) );
         if( codeblock )
         {
            int type = hb_parni( 3 );
            hb_gcUnlock( codeblock );

            char prop[ 20 ];
            hb_snprintf( prop, sizeof( prop ), "P%iP", type ); /* Make it a unique identifier */

            int i = object->property( prop ).toInt();
            if( i == 0 )
            {
               t_events->listBlock << codeblock;
               object->setProperty( prop, ( int ) t_events->listBlock.size() );
            }
            else
            {
               if( t_events->listBlock.at( i - 1 ) != NULL )
               {
                  hb_itemRelease( t_events->listBlock.at( i - 1 ) );
               }
               t_events->listBlock[ i - 1 ] = codeblock;
            }
            nResult = 0;
         }
         else
            nResult = -3;
      }
      else
         nResult = -2;
   }
   else
      nResult = -1;

   hb_retni( nResult );
}

HB_FUNC( __HBQT_EVENTS_DISCONNECT )
{
   int nResult;
   HBQEvents * t_events = hbqt_par_HBQEvents( 1 );

   if( t_events )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );

      if( object )
      {
         int type = hb_parni( 3 );

         char prop[ 20 ];
         hb_snprintf( prop, sizeof( prop ), "P%iP", type );    /* Make it a unique identifier */

         int i = object->property( prop ).toInt();
         if( i > 0 && i <= t_events->listBlock.size() )
         {
            object->setProperty( prop, QVariant() );

            if( t_events->listBlock[ i - 1 ] != NULL )
            {
               HB_TRACE( HB_TR_DEBUG, ( "           __HBQT_EVENTS_DISCONNECT %d=", i-1 ) );
               hb_itemRelease( t_events->listBlock.at( i - 1 ) );
               t_events->listBlock[ i - 1 ] = NULL;
               HB_TRACE( HB_TR_DEBUG, ( "                                    X=" ) );
            }
            nResult = 0;

            HB_TRACE( HB_TR_DEBUG, ( "      QT_EVENTS_DISCONNECT: %i", type ) );
         }
         else
            nResult = -3;
      }
      else
         nResult = -2;
   }
   else
      nResult = -1;

   hb_retni( nResult );
}

HB_FUNC( __HBQT_EVENTS_NEW )
{
   HBQEvents * p;
   QObject * o;

   o = (QObject *) hbqt_pPtrFromObj( 1 );

   p = new HBQEvents();
   o->installEventFilter( p );
 
   hb_retptrGC( hbqt_gcAllocate_HBQEvents( p, true ) );
}

static void hbqt_events_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

static void hbqt_events_exit( void * cargo )
{
   int i;
   int iItems = s_lstCreateObj.size();
   HB_TRACE( HB_TR_DEBUG, ( "ENTERING hbqt_events_exit, len=%d", s_lstCreateObj.size() ) );

   for( i = 0; i < iItems; i++ )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_events_exit, deleting item %d", i ));
      s_lstEvent.removeAt( 0 );
      s_lstCreateObj.removeAt( 0 );
      s_pEventAllocateCallback.removeAt( 0 );
   }

   HB_TRACE( HB_TR_DEBUG, ( "EXITING hbqt_events_exit, len=%d", s_lstCreateObj.size() ) );
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtevents_init_ )
   hb_vmAtInit( hbqt_events_init, NULL );
   hb_vmAtExit( hbqt_events_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtevents_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtevents_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtevents_init_ )
   #include "hbiniseg.h"
#endif

#endif
