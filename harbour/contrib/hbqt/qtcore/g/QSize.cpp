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

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QSize>


/*
 * QSize ()
 * QSize ( int width, int height )
 * ~QSize ()
 */

typedef struct
{
   QSize * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSize;

HBQT_GC_FUNC( hbqt_gcRelease_QSize )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QSize   /.\\", p->ph ) );
         delete ( ( QSize * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QSize   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSize    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSize    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSize( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSize * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSize;
   p->type = HBQT_TYPE_QSize;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSize", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSize", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSIZE )
{
   QSize * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QSize( hb_parni( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QSize( *hbqt_par_QSize( 1 ) ) ;
   }
   else
   {
      pObj =  new QSize() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSize( ( void * ) pObj, true ) );
}

/*
 * int height () const
 */
HB_FUNC( QT_QSIZE_HEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retni( ( p )->height() );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QSIZE_ISEMPTY )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QSIZE_ISNULL )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QSIZE_ISVALID )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * int & rheight ()
 */
HB_FUNC( QT_QSIZE_RHEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retni( ( p )->rheight() );
   }
}

/*
 * int & rwidth ()
 */
HB_FUNC( QT_QSIZE_RWIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retni( ( p )->rwidth() );
   }
}

/*
 * void scale ( int width, int height, Qt::AspectRatioMode mode )
 */
HB_FUNC( QT_QSIZE_SCALE )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      ( p )->scale( hb_parni( 2 ), hb_parni( 3 ), ( Qt::AspectRatioMode ) hb_parni( 4 ) );
   }
}

/*
 * void scale ( const QSize & size, Qt::AspectRatioMode mode )
 */
HB_FUNC( QT_QSIZE_SCALE_1 )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      ( p )->scale( *hbqt_par_QSize( 2 ), ( Qt::AspectRatioMode ) hb_parni( 3 ) );
   }
}

/*
 * void setHeight ( int height )
 */
HB_FUNC( QT_QSIZE_SETHEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      ( p )->setHeight( hb_parni( 2 ) );
   }
}

/*
 * void setWidth ( int width )
 */
HB_FUNC( QT_QSIZE_SETWIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      ( p )->setWidth( hb_parni( 2 ) );
   }
}

/*
 * void transpose ()
 */
HB_FUNC( QT_QSIZE_TRANSPOSE )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      ( p )->transpose();
   }
}

/*
 * int width () const
 */
HB_FUNC( QT_QSIZE_WIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retni( ( p )->width() );
   }
}

/*
 * QSize boundedTo ( const QSize & otherSize ) const
 */
HB_FUNC( QT_QSIZE_BOUNDEDTO )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->boundedTo( *hbqt_par_QSize( 2 ) ) ), true ) );
   }
}

/*
 * QSize expandedTo ( const QSize & otherSize ) const
 */
HB_FUNC( QT_QSIZE_EXPANDEDTO )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->expandedTo( *hbqt_par_QSize( 2 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
