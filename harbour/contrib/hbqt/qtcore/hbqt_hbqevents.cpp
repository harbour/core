/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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
#include <QtGui/QMessageBox>

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

HBQEvents::HBQEvents( PHB_ITEM pObj ) : QObject()
{
   if( pObj )
   {
      QObject * object = ( QObject * ) hbqt_get_ptr( pObj );
      if( object )
      {
         object->installEventFilter( this );
      }
   }
}

HBQEvents::~HBQEvents()
{
   if( hb_vmRequestReenter() )
   {
      HB_TRACE( HB_TR_DEBUG, ( "HBQEvents::~HBQEvents() Size = %i", listBlock.size() ) );
   
      int i;
      for( i = 0; i < listBlock.size(); i++ )
      {
         if( listBlock[ i ] != NULL )
         {
            HB_TRACE( HB_TR_DEBUG, ( "HBQEvents::~HBQEvents() item %d", i + 1 ) );
            hb_itemRelease( listBlock.at( i ) );
            listBlock[ i ] = NULL;
         }
      }
      hb_vmRequestRestore();
   }
   listBlock.clear();
}

int HBQEvents::hbConnect( PHB_ITEM pObj, int event, PHB_ITEM bBlock )
{
   int nResult = -1;

   if( true )
   {
      QObject * object = ( QObject * ) hbqt_get_ptr( pObj );
      if( object )
      {
         PHB_ITEM pBlock = hb_itemNew( bBlock );
         if( pBlock )
         {
            hb_gcUnlock( pBlock );

            char prop[ 20 ];
            hb_snprintf( prop, sizeof( prop ), "P%iP", event ); /* Make it a unique identifier */

            int i = object->property( prop ).toInt();
            if( i == 0 )
            {
               listBlock << pBlock;
               object->setProperty( prop, ( int ) listBlock.size() );
            }
            else
            {
               if( listBlock.at( i - 1 ) != NULL )
               {
                  hb_itemRelease( listBlock.at( i - 1 ) );
               }
               listBlock[ i - 1 ] = pBlock;
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

   return nResult;
}

int HBQEvents::hbDisconnect( PHB_ITEM pObj, int event )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQEvents::hbDisconnect( %i )", event ) );
   
   int nResult = -1;

   QObject * object = ( QObject * ) hbqt_get_ptr( pObj );
   if( object )
   {
      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "P%iP", event );    /* Make it a unique identifier */

      int i = object->property( prop ).toInt();
      if( i > 0 && i <= listBlock.size() )
      {
         object->setProperty( prop, 0 );

         if( listBlock[ i - 1 ] != NULL )
         {
            hb_itemRelease( listBlock.at( i - 1 ) );
            listBlock[ i - 1 ] = NULL;
         }
         nResult = 0;
      }
      else
         nResult = -3;
   }
   else
      nResult = -2;

   return nResult;
}

bool HBQEvents::eventFilter( QObject * object, QEvent * event )
{
   bool stopTheEventChain = false;

   if( object )
   {
      QEvent::Type eventtype = event->type();
      if( ( int ) eventtype > 0 )
      {
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
                        PHB_ITEM ret = hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 1, pItem );

                        if( hb_itemType( ret ) & HB_IT_LOGICAL )
                           stopTheEventChain = ( bool ) hb_itemGetL( ret );

                        hb_itemRelease( pItem );
                        hb_vmRequestRestore();
                     }
                  }
               }
            }
         }

         if( eventtype == QEvent::Close )
            return true;
      }
   }

   return stopTheEventChain;
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
