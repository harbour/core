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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 60/60 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QRectF>


/*
 * QRectF ()
 * QRectF ( const QPointF & topLeft, const QSizeF & size )
 * QRectF ( const QPointF & topLeft, const QPointF & bottomRight )
 * QRectF ( qreal x, qreal y, qreal width, qreal height )
 * QRectF ( const QRect & rectangle )
 * ~QRectF ()
 */

typedef struct
{
   QRectF * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRectF;

HBQT_GC_FUNC( hbqt_gcRelease_QRectF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QRectF   /.\\", p->ph ) );
         delete ( ( QRectF * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QRectF   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QRectF    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QRectF    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QRectF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRectF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRectF;
   p->type = HBQT_TYPE_QRectF;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QRectF", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QRectF", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QRECTF )
{
   QRectF * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QRectF( *hbqt_par_QRectF( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QRectF( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QRectF( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ;
   }
   else
   {
      pObj = new QRectF() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRectF( ( void * ) pObj, true ) );
}

/*
 * void adjust ( qreal dx1, qreal dy1, qreal dx2, qreal dy2 )
 */
HB_FUNC( QT_QRECTF_ADJUST )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->adjust( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * QRectF adjusted ( qreal dx1, qreal dy1, qreal dx2, qreal dy2 ) const
 */
HB_FUNC( QT_QRECTF_ADJUSTED )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->adjusted( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   }
}

/*
 * qreal bottom () const
 */
HB_FUNC( QT_QRECTF_BOTTOM )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->bottom() );
   }
}

/*
 * QPointF bottomLeft () const
 */
HB_FUNC( QT_QRECTF_BOTTOMLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->bottomLeft() ), true ) );
   }
}

/*
 * QPointF bottomRight () const
 */
HB_FUNC( QT_QRECTF_BOTTOMRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->bottomRight() ), true ) );
   }
}

/*
 * QPointF center () const
 */
HB_FUNC( QT_QRECTF_CENTER )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) );
   }
}

/*
 * bool contains ( const QPointF & point ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( *hbqt_par_QPointF( 2 ) ) );
   }
}

/*
 * bool contains ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS_1 )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( hb_parnd( 2 ), hb_parnd( 3 ) ) );
   }
}

/*
 * bool contains ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS_2 )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( *hbqt_par_QRectF( 2 ) ) );
   }
}

/*
 * void getCoords ( qreal * x1, qreal * y1, qreal * x2, qreal * y2 ) const
 */
HB_FUNC( QT_QRECTF_GETCOORDS )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   qreal qrX1 = 0;
   qreal qrY1 = 0;
   qreal qrX2 = 0;
   qreal qrY2 = 0;

   if( p )
   {
      ( p )->getCoords( &qrX1, &qrY1, &qrX2, &qrY2 );
   }

   hb_stornd( qrX1, 2 );
   hb_stornd( qrY1, 3 );
   hb_stornd( qrX2, 4 );
   hb_stornd( qrY2, 5 );
}

/*
 * void getRect ( qreal * x, qreal * y, qreal * width, qreal * height ) const
 */
HB_FUNC( QT_QRECTF_GETRECT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   qreal qrX = 0;
   qreal qrY = 0;
   qreal qrWidth = 0;
   qreal qrHeight = 0;

   if( p )
   {
      ( p )->getRect( &qrX, &qrY, &qrWidth, &qrHeight );
   }

   hb_stornd( qrX, 2 );
   hb_stornd( qrY, 3 );
   hb_stornd( qrWidth, 4 );
   hb_stornd( qrHeight, 5 );
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QRECTF_HEIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->height() );
   }
}

/*
 * QRectF intersected ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_INTERSECTED )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->intersected( *hbqt_par_QRectF( 2 ) ) ), true ) );
   }
}

/*
 * bool intersects ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_INTERSECTS )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->intersects( *hbqt_par_QRectF( 2 ) ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QRECTF_ISEMPTY )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QRECTF_ISNULL )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRECTF_ISVALID )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * qreal left () const
 */
HB_FUNC( QT_QRECTF_LEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->left() );
   }
}

/*
 * void moveBottom ( qreal y )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOM )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveBottom( hb_parnd( 2 ) );
   }
}

/*
 * void moveBottomLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOMLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveBottomLeft( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void moveBottomRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOMRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveBottomRight( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void moveCenter ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVECENTER )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveCenter( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void moveLeft ( qreal x )
 */
HB_FUNC( QT_QRECTF_MOVELEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveLeft( hb_parnd( 2 ) );
   }
}

/*
 * void moveRight ( qreal x )
 */
HB_FUNC( QT_QRECTF_MOVERIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveRight( hb_parnd( 2 ) );
   }
}

/*
 * void moveTo ( qreal x, qreal y )
 */
HB_FUNC( QT_QRECTF_MOVETO )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveTo( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void moveTo ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETO_1 )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveTo( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void moveTop ( qreal y )
 */
HB_FUNC( QT_QRECTF_MOVETOP )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveTop( hb_parnd( 2 ) );
   }
}

/*
 * void moveTopLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETOPLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveTopLeft( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void moveTopRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETOPRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->moveTopRight( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * QRectF normalized () const
 */
HB_FUNC( QT_QRECTF_NORMALIZED )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->normalized() ), true ) );
   }
}

/*
 * qreal right () const
 */
HB_FUNC( QT_QRECTF_RIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->right() );
   }
}

/*
 * void setBottom ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETBOTTOM )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setBottom( hb_parnd( 2 ) );
   }
}

/*
 * void setBottomLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETBOTTOMLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setBottomLeft( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setBottomRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETBOTTOMRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setBottomRight( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setCoords ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */
HB_FUNC( QT_QRECTF_SETCOORDS )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setCoords( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QRECTF_SETHEIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setHeight( hb_parnd( 2 ) );
   }
}

/*
 * void setLeft ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setLeft( hb_parnd( 2 ) );
   }
}

/*
 * void setRect ( qreal x, qreal y, qreal width, qreal height )
 */
HB_FUNC( QT_QRECTF_SETRECT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * void setRight ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setRight( hb_parnd( 2 ) );
   }
}

/*
 * void setSize ( const QSizeF & size )
 */
HB_FUNC( QT_QRECTF_SETSIZE )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setSize( *hbqt_par_QSizeF( 2 ) );
   }
}

/*
 * void setTop ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETTOP )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setTop( hb_parnd( 2 ) );
   }
}

/*
 * void setTopLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETTOPLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setTopLeft( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setTopRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETTOPRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setTopRight( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QRECTF_SETWIDTH )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setX ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETX )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setX( hb_parnd( 2 ) );
   }
}

/*
 * void setY ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETY )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->setY( hb_parnd( 2 ) );
   }
}

/*
 * QSizeF size () const
 */
HB_FUNC( QT_QRECTF_SIZE )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size() ), true ) );
   }
}

/*
 * QRect toAlignedRect () const
 */
HB_FUNC( QT_QRECTF_TOALIGNEDRECT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->toAlignedRect() ), true ) );
   }
}

/*
 * QRect toRect () const
 */
HB_FUNC( QT_QRECTF_TORECT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->toRect() ), true ) );
   }
}

/*
 * qreal top () const
 */
HB_FUNC( QT_QRECTF_TOP )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->top() );
   }
}

/*
 * QPointF topLeft () const
 */
HB_FUNC( QT_QRECTF_TOPLEFT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->topLeft() ), true ) );
   }
}

/*
 * QPointF topRight () const
 */
HB_FUNC( QT_QRECTF_TOPRIGHT )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->topRight() ), true ) );
   }
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QRECTF_TRANSLATE )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QRECTF_TRANSLATE_1 )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      ( p )->translate( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * QRectF translated ( qreal dx, qreal dy ) const
 */
HB_FUNC( QT_QRECTF_TRANSLATED )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->translated( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   }
}

/*
 * QRectF translated ( const QPointF & offset ) const
 */
HB_FUNC( QT_QRECTF_TRANSLATED_1 )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->translated( *hbqt_par_QPointF( 2 ) ) ), true ) );
   }
}

/*
 * QRectF united ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_UNITED )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->united( *hbqt_par_QRectF( 2 ) ) ), true ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QRECTF_WIDTH )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->width() );
   }
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QRECTF_X )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->x() );
   }
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QRECTF_Y )
{
   QRectF * p = hbqt_par_QRectF( 1 );
   if( p )
   {
      hb_retnd( ( p )->y() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
