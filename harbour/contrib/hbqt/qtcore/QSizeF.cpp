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
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QSizeF>


/*
 * QSizeF ()
 * QSizeF ( const QSize & size )
 * QSizeF ( qreal width, qreal height )
 * ~QSizeF ()
 */

typedef struct
{
   QSizeF * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QSizeF;

QT_G_FUNC( hbqt_gcRelease_QSizeF )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QSizeF   /.\\", p->ph ) );
         delete ( ( QSizeF * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QSizeF   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSizeF    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSizeF    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizeF( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QSizeF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizeF;
   p->type = HBQT_TYPE_QSizeF;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSizeF", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSizeF", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSIZEF )
{
   QSizeF * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QSizeF( hb_parnd( 1 ), hb_parnd( 2 ) ) ;
   }
   else
   {
      pObj =  new QSizeF() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSizeF( ( void * ) pObj, true ) );
}

/*
 * QSizeF boundedTo ( const QSizeF & otherSize ) const
 */
HB_FUNC( QT_QSIZEF_BOUNDEDTO )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->boundedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_BOUNDEDTO FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->boundedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QSizeF expandedTo ( const QSizeF & otherSize ) const
 */
HB_FUNC( QT_QSIZEF_EXPANDEDTO )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->expandedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_EXPANDEDTO FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->expandedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QSIZEF_HEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QSIZEF_ISEMPTY )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QSIZEF_ISNULL )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QSIZEF_ISVALID )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * qreal & rheight ()
 */
HB_FUNC( QT_QSIZEF_RHEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->rheight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_RHEIGHT FP=hb_retnd( ( p )->rheight() ); p is NULL" ) );
   }
}

/*
 * qreal & rwidth ()
 */
HB_FUNC( QT_QSIZEF_RWIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->rwidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_RWIDTH FP=hb_retnd( ( p )->rwidth() ); p is NULL" ) );
   }
}

/*
 * void scale ( qreal width, qreal height, Qt::AspectRatioMode mode )
 */
HB_FUNC( QT_QSIZEF_SCALE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ), ( Qt::AspectRatioMode ) hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_SCALE FP=( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ), ( Qt::AspectRatioMode ) hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void scale ( const QSizeF & size, Qt::AspectRatioMode mode )
 */
HB_FUNC( QT_QSIZEF_SCALE_1 )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->scale( *hbqt_par_QSizeF( 2 ), ( Qt::AspectRatioMode ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_SCALE_1 FP=( p )->scale( *hbqt_par_QSizeF( 2 ), ( Qt::AspectRatioMode ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QSIZEF_SETHEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_SETHEIGHT FP=( p )->setHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QSIZEF_SETWIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_SETWIDTH FP=( p )->setWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize toSize () const
 */
HB_FUNC( QT_QSIZEF_TOSIZE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_TOSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void transpose ()
 */
HB_FUNC( QT_QSIZEF_TRANSPOSE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->transpose();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_TRANSPOSE FP=( p )->transpose(); p is NULL" ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QSIZEF_WIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSIZEF_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
