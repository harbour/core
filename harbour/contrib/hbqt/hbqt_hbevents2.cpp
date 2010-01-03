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

#include "hbqt_hbevents.h"

#include <QPointer>
#include <QVariant>

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< HBEvents > pq;
} QGC_POINTER_HBEvents;

static QT_G_FUNC( hbqt_release_HBEvents )
{
   QGC_POINTER_HBEvents * p = ( QGC_POINTER_HBEvents * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_HBEvents                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_HBEvents                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( HBEvents * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( HBEvents * ) p->ph )->~HBEvents();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( HBEvents * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_HBEvents                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_HBEvents                 Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_HBEvents                 Object Already deleted!" ) );
   }
}

static void * hbqt_gcAllocate_HBEvents( void * pObj )
{
   QGC_POINTER_HBEvents * p = ( QGC_POINTER_HBEvents * ) hb_gcAllocate( sizeof( QGC_POINTER_HBEvents ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_release_HBEvents;
   new( & p->pq ) QPointer< HBEvents >( ( HBEvents * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_HBEvents                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

/*----------------------------------------------------------------------*/

/* TOFIX: Possible GPF is below pointer is used by .prg after release. */
HB_FUNC( QT_EVENTS_PTR )
{
   hb_retptr( hbqt_par_HBEvents( 1 ) );
}

/*----------------------------------------------------------------------*/

HBEvents::HBEvents( QObject * parent ) : QObject( parent )
{
}

/* QUESTION: Shouldn't all events be disconnected at this point? */
HBEvents::~HBEvents()
{
   HB_TRACE( HB_TR_DEBUG, ( "      HBEvents::~HBEvents()" ) );
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
         listObj[ i ] = NULL;
      }
   }

   listBlock.clear();
}

bool HBEvents::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();

   if( ( int ) eventtype == 0 )
      return false;

   char prop[ 20 ];
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

HB_FUNC( QT_EVENTS_CONNECT )
{
   HB_BOOL  bRet       = HB_FALSE;
   HBEvents * t_events = hbqt_par_HBEvents( 1 );

   if( t_events )
   {
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );          /* get sender    */

      if( object )
      {
         int      type      = hb_parni( 3 );
         PHB_ITEM codeblock = hb_itemNew( hb_param( 4, HB_IT_BLOCK | HB_IT_BYREF ) );

         char prop[ 20 ];
         hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

         t_events->listBlock << codeblock;
         /* TOFIX: Reference to GC collected pointer is stored. */
         t_events->listObj   << object;

         object->setProperty( prop, ( int ) t_events->listBlock.size() );

         bRet = HB_TRUE;
      }
   }

   hb_retl( bRet );
}

HB_FUNC( QT_EVENTS_DISCONNECT )
{
   HB_BOOL  bRet       = HB_FALSE;
   HBEvents * t_events = hbqt_par_HBEvents( 1 );

   if( t_events )
   {
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );

      if( object )
      {
         int type = hb_parni( 3 );

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

            HB_TRACE( HB_TR_DEBUG, ( "      QT_EVENTS_DISCONNECT: %i", type ) );
         }
      }
   }

   hb_retl( bRet );
}

HB_FUNC( QT_EVENTS_NEW )
{
   void * pObj = NULL;

   pObj = ( HBEvents * ) new HBEvents();

   hb_retptrGC( hbqt_gcAllocate_HBEvents( pObj ) );
}

/*----------------------------------------------------------------------*/

#endif
