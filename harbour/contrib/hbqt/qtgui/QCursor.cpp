/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <qpixmap.h>
#include <QtGui/QCursor>

/*
 * QCursor ()
 * QCursor ( Qt::CursorShape shape )
 * QCursor ( const QBitmap & bitmap, const QBitmap & mask, int hotX = -1, int hotY = -1 )
 * QCursor ( const QPixmap & pixmap, int hotX = -1, int hotY = -1 )
 * QCursor ( const QCursor & c )
 * QCursor ( HCURSOR cursor )
 * QCursor ( Qt::HANDLE handle )
 * ~QCursor ()
 */

QT_G_FUNC( hbqt_gcRelease_QCursor )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QCursor                      p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QCursor                     ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QCursor * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QCursor                     Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QCursor                     Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QCursor( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QCursor;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QCursor                     %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QCURSOR )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = ( QCursor* ) new QCursor( ( Qt::CursorShape ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QCursor* ) new QCursor( *hbqt_par_QCursor( 1 ) ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = hbqt_par_QString( 1 );

      if( objName == ( QString ) "QPixmap" )
      {
         pObj = ( QCursor* ) new QCursor( *hbqt_par_QPixmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
      }
      else
      {
         pObj = ( QCursor* ) new QCursor() ;
      }
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = ( QCursor* ) new QCursor( *hbqt_par_QBitmap( 1 ), *hbqt_par_QBitmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
   }
   else
   {
      pObj = ( QCursor* ) new QCursor() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QCursor( pObj ) );
}
/*
 * const QBitmap * bitmap () const
 */
HB_FUNC( QT_QCURSOR_BITMAP )
{
   hb_retptr( ( QBitmap* ) hbqt_par_QCursor( 1 )->bitmap() );
}

/*
 * QPoint hotSpot () const
 */
HB_FUNC( QT_QCURSOR_HOTSPOT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QCursor( 1 )->hotSpot() ) ) );
}

/*
 * const QBitmap * mask () const
 */
HB_FUNC( QT_QCURSOR_MASK )
{
   hb_retptr( ( QBitmap* ) hbqt_par_QCursor( 1 )->mask() );
}

/*
 * QPixmap pixmap () const
 */
HB_FUNC( QT_QCURSOR_PIXMAP )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QCursor( 1 )->pixmap() ) ) );
}

/*
 * void setShape ( Qt::CursorShape shape )
 */
HB_FUNC( QT_QCURSOR_SETSHAPE )
{
   hbqt_par_QCursor( 1 )->setShape( ( Qt::CursorShape ) hb_parni( 2 ) );
}

/*
 * Qt::CursorShape shape () const
 */
HB_FUNC( QT_QCURSOR_SHAPE )
{
   hb_retni( ( Qt::CursorShape ) hbqt_par_QCursor( 1 )->shape() );
}

/*
 * QPoint pos ()
 */
HB_FUNC( QT_QCURSOR_POS )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QCursor( 1 )->pos() ) ) );
}

/*
 * void setPos ( int x, int y )
 */
HB_FUNC( QT_QCURSOR_SETPOS )
{
   hbqt_par_QCursor( 1 )->setPos( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setPos ( const QPoint & p )
 */
HB_FUNC( QT_QCURSOR_SETPOS_1 )
{
   hbqt_par_QCursor( 1 )->setPos( *hbqt_par_QPoint( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
