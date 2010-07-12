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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

typedef struct
{
   QPolygon * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPolygon;

QT_G_FUNC( hbqt_gcRelease_QPolygon )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPolygon   /.\\", p->ph ) );
         delete ( ( QPolygon * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPolygon   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPolygon    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPolygon    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPolygon( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QPolygon * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPolygon;
   p->type = HBQT_TYPE_QPolygon;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPolygon", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPolygon", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPOLYGON )
{
   QPolygon * pObj = NULL;

   pObj = new QPolygon() ;

   hb_retptrGC( hbqt_gcAllocate_QPolygon( ( void * ) pObj, true ) );
}

/*
 * QRect boundingRect () const
 */
HB_FUNC( QT_QPOLYGON_BOUNDINGRECT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool containsPoint ( const QPoint & point, Qt::FillRule fillRule ) const
 */
HB_FUNC( QT_QPOLYGON_CONTAINSPOINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retl( ( p )->containsPoint( *hbqt_par_QPoint( 2 ), ( Qt::FillRule ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_CONTAINSPOINT FP=hb_retl( ( p )->containsPoint( *hbqt_par_QPoint( 2 ), ( Qt::FillRule ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QPolygon intersected ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_INTERSECTED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->intersected( *hbqt_par_QPolygon( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_INTERSECTED FP=hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->intersected( *hbqt_par_QPolygon( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void point ( int index, int * x, int * y ) const
 */
HB_FUNC( QT_QPOLYGON_POINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   int iX = 0;
   int iY = 0;

   if( p )
      ( p )->point( hb_parni( 2 ), &iX, &iY );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_POINT FP=( p )->point( hb_parni( 2 ), &iX, &iY ); p is NULL" ) );
   }

   hb_storni( iX, 3 );
   hb_storni( iY, 4 );
}

/*
 * QPoint point ( int index ) const
 */
HB_FUNC( QT_QPOLYGON_POINT_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->point( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_POINT_1 FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->point( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void putPoints ( int index, int nPoints, const QPolygon & fromPolygon, int fromIndex = 0 )
 */
HB_FUNC( QT_QPOLYGON_PUTPOINTS )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->putPoints( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPolygon( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_PUTPOINTS FP=( p )->putPoints( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPolygon( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setPoint ( int index, int x, int y )
 */
HB_FUNC( QT_QPOLYGON_SETPOINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->setPoint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_SETPOINT FP=( p )->setPoint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setPoint ( int index, const QPoint & point )
 */
HB_FUNC( QT_QPOLYGON_SETPOINT_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->setPoint( hb_parni( 2 ), *hbqt_par_QPoint( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_SETPOINT_1 FP=( p )->setPoint( hb_parni( 2 ), *hbqt_par_QPoint( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPoints ( int nPoints, const int * points )
 */
HB_FUNC( QT_QPOLYGON_SETPOINTS )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   int iPoints = 0;

   if( p )
      ( p )->setPoints( hb_parni( 2 ), &iPoints );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_SETPOINTS FP=( p )->setPoints( hb_parni( 2 ), &iPoints ); p is NULL" ) );
   }

   hb_storni( iPoints, 3 );
}

/*
 * QPolygon subtracted ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_SUBTRACTED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->subtracted( *hbqt_par_QPolygon( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_SUBTRACTED FP=hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->subtracted( *hbqt_par_QPolygon( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QPOLYGON_TRANSLATE )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_TRANSLATE FP=( p )->translate( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QPOLYGON_TRANSLATE_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_TRANSLATE_1 FP=( p )->translate( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * QPolygon united ( const QPolygon & r ) const
 */
HB_FUNC( QT_QPOLYGON_UNITED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->united( *hbqt_par_QPolygon( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPOLYGON_UNITED FP=hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->united( *hbqt_par_QPolygon( 2 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
