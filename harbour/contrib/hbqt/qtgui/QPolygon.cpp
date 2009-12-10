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

#include <QtGui/QPolygon>


/* QPolygon ()
 * QPolygon ( int size )
 * QPolygon ( const QPolygon & polygon )
 * QPolygon ( const QVector<QPoint> & points )
 * QPolygon ( const QRect & rectangle, bool closed = false )
 * ~QPolygon ()
 */

QT_G_FUNC( release_QPolygon )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QPolygon                     p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QPolygon                    ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QPolygon * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QPolygon                    Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QPolygon                    Object Already deleted!" ) );
   }
}

void * gcAllocate_QPolygon( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QPolygon;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QPolygon                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QPOLYGON )
{
   void * pObj = NULL;

   pObj = new QPolygon() ;

   hb_retptrGC( gcAllocate_QPolygon( pObj ) );
}
/*
 * QRect boundingRect () const
 */
HB_FUNC( QT_QPOLYGON_BOUNDINGRECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPolygon( 1 )->boundingRect() ) ) );
}

/*
 * bool containsPoint ( const QPoint & point, Qt::FillRule fillRule ) const
 */
HB_FUNC( QT_QPOLYGON_CONTAINSPOINT )
{
   hb_retl( hbqt_par_QPolygon( 1 )->containsPoint( *hbqt_par_QPoint( 2 ), ( Qt::FillRule ) hb_parni( 3 ) ) );
}

/*
 * QPolygon intersected ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_INTERSECTED )
{
   hb_retptrGC( gcAllocate_QPolygon( new QPolygon( hbqt_par_QPolygon( 1 )->intersected( *hbqt_par_QPolygon( 2 ) ) ) ) );
}

/*
 * void point ( int index, int * x, int * y ) const
 */
HB_FUNC( QT_QPOLYGON_POINT )
{
   int iX = 0;
   int iY = 0;

   hbqt_par_QPolygon( 1 )->point( hb_parni( 2 ), &iX, &iY );

   hb_storni( iX, 3 );
   hb_storni( iY, 4 );
}

/*
 * QPoint point ( int index ) const
 */
HB_FUNC( QT_QPOLYGON_POINT_1 )
{
   hb_retptrGC( gcAllocate_QPoint( new QPoint( hbqt_par_QPolygon( 1 )->point( hb_parni( 2 ) ) ) ) );
}

/*
 * void putPoints ( int index, int nPoints, const QPolygon & fromPolygon, int fromIndex = 0 )
 */
HB_FUNC( QT_QPOLYGON_PUTPOINTS )
{
   hbqt_par_QPolygon( 1 )->putPoints( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPolygon( 4 ), hb_parni( 5 ) );
}

/*
 * void setPoint ( int index, int x, int y )
 */
HB_FUNC( QT_QPOLYGON_SETPOINT )
{
   hbqt_par_QPolygon( 1 )->setPoint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void setPoint ( int index, const QPoint & point )
 */
HB_FUNC( QT_QPOLYGON_SETPOINT_1 )
{
   hbqt_par_QPolygon( 1 )->setPoint( hb_parni( 2 ), *hbqt_par_QPoint( 3 ) );
}

/*
 * void setPoints ( int nPoints, const int * points )
 */
HB_FUNC( QT_QPOLYGON_SETPOINTS )
{
   int iPoints = 0;

   hbqt_par_QPolygon( 1 )->setPoints( hb_parni( 2 ), &iPoints );

   hb_storni( iPoints, 3 );
}

/*
 * QPolygon subtracted ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_SUBTRACTED )
{
   hb_retptrGC( gcAllocate_QPolygon( new QPolygon( hbqt_par_QPolygon( 1 )->subtracted( *hbqt_par_QPolygon( 2 ) ) ) ) );
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QPOLYGON_TRANSLATE )
{
   hbqt_par_QPolygon( 1 )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QPOLYGON_TRANSLATE_1 )
{
   hbqt_par_QPolygon( 1 )->translate( *hbqt_par_QPoint( 2 ) );
}

/*
 * QPolygon united ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_UNITED )
{
   hb_retptrGC( gcAllocate_QPolygon( new QPolygon( hbqt_par_QPolygon( 1 )->united( *hbqt_par_QPolygon( 2 ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
