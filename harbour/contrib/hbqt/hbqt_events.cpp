/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"

/*----------------------------------------------------------------------*/

typedef struct
{
   Events * t_events;
} HB_EVENTS, * PHB_EVENTS;

static HB_TSD_NEW( s_events, sizeof( HB_EVENTS ), NULL, NULL );

#define HB_GETQTEVENTFILTER()       ( ( PHB_EVENTS ) hb_stackGetTSD( &s_events ) )

/*----------------------------------------------------------------------*/

static void qt_setEventFilter()
{
   if( ! HB_GETQTEVENTFILTER()->t_events )
      HB_GETQTEVENTFILTER()->t_events = new Events();
}

static Events * qt_getEventFilter( void )
{
   return HB_GETQTEVENTFILTER()->t_events;
}

HB_FUNC( QT_SETEVENTFILTER )
{
   qt_setEventFilter();
}

HB_FUNC( QT_GETEVENTFILTER )
{
   hb_retptr( qt_getEventFilter() );
}

/*----------------------------------------------------------------------*/

Events::Events( QObject * parent ) : QObject( parent )
{
}

Events::~Events()
{
   listBlock.clear();
}

bool Events::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();

   if( ( int ) eventtype == 0 )
      return false;

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", eventtype, "P" );

   int found = object->property( prop ).toInt();

   if( found == 0 )
      return false;

   bool ret = true;

   if( found <= listBlock.size() && listObj.at( found - 1 ) == object && hb_vmRequestReenter() )
   {
      PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
      PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event  );
      ret = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 2, pObject, pEvent ) );
      hb_itemRelease( pObject );
      hb_itemRelease( pEvent  );

      hb_vmRequestRestore();

      if( eventtype == QEvent::Close )
         event->ignore();
   }

   return ret;
}

HB_FUNC( QT_EVENTS_DESTROY )
{
   qt_getEventFilter()->~Events();
}

HB_FUNC( QT_CONNECT_EVENT )
{
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );          /* get sender    */

   if( object )
   {
      int       type      = hb_parni( 2 );
      PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) );
      Events  * t_events  = qt_getEventFilter();

      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

      t_events->listBlock << codeblock;
      t_events->listObj   << object;

      object->setProperty( prop, ( int ) t_events->listBlock.size() );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( QT_DISCONNECT_EVENT )
{
   HB_BOOL   bRet   = HB_FALSE;
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );

   if( object )
   {
      int       type     = hb_parni( 2 );
      Events  * t_events = qt_getEventFilter();

      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

      int i = object->property( prop ).toInt();
      if( i > 0 && i <= t_events->listBlock.size() )
      {
         hb_itemRelease( t_events->listBlock.at( i - 1 ) );
         t_events->listBlock[ i - 1 ] = NULL;
         t_events->listObj[ i - 1 ]   = NULL;
         object->setProperty( prop, QVariant() );
         bRet = HB_TRUE;

         HB_TRACE( HB_TR_DEBUG, ( "      QT_DISCONNECT_EVENT: %i", type ) );
      }
   }

   hb_retl( bRet );
}

/*----------------------------------------------------------------------*/

#endif
