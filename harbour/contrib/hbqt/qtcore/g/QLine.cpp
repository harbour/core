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

#include "hbqt.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QLine>


/*
 * QLine ()
 * QLine ( const QPoint & p1, const QPoint & p2 )
 * QLine ( int x1, int y1, int x2, int y2 )
 */

typedef struct
{
   QLine * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QLine;

QT_G_FUNC( hbqt_gcRelease_QLine )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QLine   /.\\", p->ph ) );
         delete ( ( QLine * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QLine   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLine    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLine    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLine( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QLine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLine;
   p->type = HBQT_TYPE_QLine;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLine", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLine", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLINE )
{
   QLine * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QLine( *hbqt_par_QLine( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QLine( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj =  new QLine( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ;
   }
   else
   {
      pObj =  new QLine() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QLine( ( void * ) pObj, true ) );
}

/*
 * QPoint p1 () const
 */
HB_FUNC( QT_QLINE_P1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p1() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_P1 FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p1() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint p2 () const
 */
HB_FUNC( QT_QLINE_P2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p2() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_P2 FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p2() ), true ) ); p is NULL" ) );
   }
}

/*
 * int x1 () const
 */
HB_FUNC( QT_QLINE_X1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->x1() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_X1 FP=hb_retni( ( p )->x1() ); p is NULL" ) );
   }
}

/*
 * int x2 () const
 */
HB_FUNC( QT_QLINE_X2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->x2() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_X2 FP=hb_retni( ( p )->x2() ); p is NULL" ) );
   }
}

/*
 * int y1 () const
 */
HB_FUNC( QT_QLINE_Y1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->y1() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_Y1 FP=hb_retni( ( p )->y1() ); p is NULL" ) );
   }
}

/*
 * int y2 () const
 */
HB_FUNC( QT_QLINE_Y2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->y2() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_Y2 FP=hb_retni( ( p )->y2() ); p is NULL" ) );
   }
}

/*
 * int dx () const
 */
HB_FUNC( QT_QLINE_DX )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->dx() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_DX FP=hb_retni( ( p )->dx() ); p is NULL" ) );
   }
}

/*
 * int dy () const
 */
HB_FUNC( QT_QLINE_DY )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->dy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_DY FP=hb_retni( ( p )->dy() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QLINE_ISNULL )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * void setP1 ( const QPoint & p1 )
 */
HB_FUNC( QT_QLINE_SETP1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setP1( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_SETP1 FP=( p )->setP1( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setP2 ( const QPoint & p2 )
 */
HB_FUNC( QT_QLINE_SETP2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setP2( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_SETP2 FP=( p )->setP2( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QLINE_SETLINE )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_SETLINE FP=( p )->setLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setPoints ( const QPoint & p1, const QPoint & p2 )
 */
HB_FUNC( QT_QLINE_SETPOINTS )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setPoints( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_SETPOINTS FP=( p )->setPoints( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) ); p is NULL" ) );
   }
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QLINE_TRANSLATE )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_TRANSLATE FP=( p )->translate( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QLINE_TRANSLATE_1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_TRANSLATE_1 FP=( p )->translate( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QLine translated ( const QPoint & offset ) const
 */
HB_FUNC( QT_QLINE_TRANSLATED )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_TRANSLATED FP=hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QLine translated ( int dx, int dy ) const
 */
HB_FUNC( QT_QLINE_TRANSLATED_1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINE_TRANSLATED_1 FP=hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
