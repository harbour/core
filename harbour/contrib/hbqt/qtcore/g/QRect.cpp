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
 *  Constructed[ 59/59 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QRect>


/*
 * QRect ()
 * QRect ( const QPoint & topLeft, const QPoint & bottomRight )
 * QRect ( const QPoint & topLeft, const QSize & size )
 * QRect ( int x, int y, int width, int height )
 * ~QRect ()
 */

typedef struct
{
   QRect * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRect;

HBQT_GC_FUNC( hbqt_gcRelease_QRect )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QRect   /.\\", p->ph ) );
         delete ( ( QRect * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QRect   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QRect    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QRect    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QRect( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRect * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRect;
   p->type = HBQT_TYPE_QRect;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QRect", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QRect", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QRECT )
{
   QRect * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QRect( *hbqt_par_QRect( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QRect( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj =  new QRect( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ;
   }
   else
   {
      pObj =  new QRect() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRect( ( void * ) pObj, true ) );
}

/*
 * void adjust ( int dx1, int dy1, int dx2, int dy2 )
 */
HB_FUNC( QT_QRECT_ADJUST )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->adjust( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   }
}

/*
 * QRect adjusted ( int dx1, int dy1, int dx2, int dy2 ) const
 */
HB_FUNC( QT_QRECT_ADJUSTED )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->adjusted( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
   }
}

/*
 * int bottom () const
 */
HB_FUNC( QT_QRECT_BOTTOM )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->bottom() );
   }
}

/*
 * QPoint bottomLeft () const
 */
HB_FUNC( QT_QRECT_BOTTOMLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->bottomLeft() ), true ) );
   }
}

/*
 * QPoint bottomRight () const
 */
HB_FUNC( QT_QRECT_BOTTOMRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->bottomRight() ), true ) );
   }
}

/*
 * QPoint center () const
 */
HB_FUNC( QT_QRECT_CENTER )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->center() ), true ) );
   }
}

/*
 * bool contains ( const QPoint & point, bool proper = false ) const
 */
HB_FUNC( QT_QRECT_CONTAINS )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( *hbqt_par_QPoint( 2 ), hb_parl( 3 ) ) );
   }
}

/*
 * bool contains ( int x, int y, bool proper ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_1 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ) );
   }
}

/*
 * bool contains ( int x, int y ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_2 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( hb_parni( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * bool contains ( const QRect & rectangle, bool proper = false ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_3 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( *hbqt_par_QRect( 2 ), hb_parl( 3 ) ) );
   }
}

/*
 * void getCoords ( int * x1, int * y1, int * x2, int * y2 ) const
 */
HB_FUNC( QT_QRECT_GETCOORDS )
{
   QRect * p = hbqt_par_QRect( 1 );
   int iX1 = 0;
   int iY1 = 0;
   int iX2 = 0;
   int iY2 = 0;

   if( p )
   {
      ( p )->getCoords( &iX1, &iY1, &iX2, &iY2 );
   }

   hb_storni( iX1, 2 );
   hb_storni( iY1, 3 );
   hb_storni( iX2, 4 );
   hb_storni( iY2, 5 );
}

/*
 * void getRect ( int * x, int * y, int * width, int * height ) const
 */
HB_FUNC( QT_QRECT_GETRECT )
{
   QRect * p = hbqt_par_QRect( 1 );
   int iX = 0;
   int iY = 0;
   int iWidth = 0;
   int iHeight = 0;

   if( p )
   {
      ( p )->getRect( &iX, &iY, &iWidth, &iHeight );
   }

   hb_storni( iX, 2 );
   hb_storni( iY, 3 );
   hb_storni( iWidth, 4 );
   hb_storni( iHeight, 5 );
}

/*
 * int height () const
 */
HB_FUNC( QT_QRECT_HEIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->height() );
   }
}

/*
 * QRect intersected ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_INTERSECTED )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->intersected( *hbqt_par_QRect( 2 ) ) ), true ) );
   }
}

/*
 * bool intersects ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_INTERSECTS )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->intersects( *hbqt_par_QRect( 2 ) ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QRECT_ISEMPTY )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QRECT_ISNULL )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRECT_ISVALID )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * int left () const
 */
HB_FUNC( QT_QRECT_LEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->left() );
   }
}

/*
 * void moveBottom ( int y )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOM )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveBottom( hb_parni( 2 ) );
   }
}

/*
 * void moveBottomLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOMLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveBottomLeft( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void moveBottomRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOMRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveBottomRight( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void moveCenter ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVECENTER )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveCenter( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void moveLeft ( int x )
 */
HB_FUNC( QT_QRECT_MOVELEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveLeft( hb_parni( 2 ) );
   }
}

/*
 * void moveRight ( int x )
 */
HB_FUNC( QT_QRECT_MOVERIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveRight( hb_parni( 2 ) );
   }
}

/*
 * void moveTo ( int x, int y )
 */
HB_FUNC( QT_QRECT_MOVETO )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveTo( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void moveTo ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETO_1 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveTo( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void moveTop ( int y )
 */
HB_FUNC( QT_QRECT_MOVETOP )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveTop( hb_parni( 2 ) );
   }
}

/*
 * void moveTopLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETOPLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveTopLeft( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void moveTopRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETOPRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->moveTopRight( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * QRect normalized () const
 */
HB_FUNC( QT_QRECT_NORMALIZED )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->normalized() ), true ) );
   }
}

/*
 * int right () const
 */
HB_FUNC( QT_QRECT_RIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->right() );
   }
}

/*
 * void setBottom ( int y )
 */
HB_FUNC( QT_QRECT_SETBOTTOM )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setBottom( hb_parni( 2 ) );
   }
}

/*
 * void setBottomLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETBOTTOMLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setBottomLeft( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void setBottomRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETBOTTOMRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setBottomRight( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void setCoords ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QRECT_SETCOORDS )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setCoords( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   }
}

/*
 * void setHeight ( int height )
 */
HB_FUNC( QT_QRECT_SETHEIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setHeight( hb_parni( 2 ) );
   }
}

/*
 * void setLeft ( int x )
 */
HB_FUNC( QT_QRECT_SETLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setLeft( hb_parni( 2 ) );
   }
}

/*
 * void setRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QRECT_SETRECT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   }
}

/*
 * void setRight ( int x )
 */
HB_FUNC( QT_QRECT_SETRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setRight( hb_parni( 2 ) );
   }
}

/*
 * void setSize ( const QSize & size )
 */
HB_FUNC( QT_QRECT_SETSIZE )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setTop ( int y )
 */
HB_FUNC( QT_QRECT_SETTOP )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setTop( hb_parni( 2 ) );
   }
}

/*
 * void setTopLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETTOPLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setTopLeft( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void setTopRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETTOPRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setTopRight( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void setWidth ( int width )
 */
HB_FUNC( QT_QRECT_SETWIDTH )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setWidth( hb_parni( 2 ) );
   }
}

/*
 * void setX ( int x )
 */
HB_FUNC( QT_QRECT_SETX )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setX( hb_parni( 2 ) );
   }
}

/*
 * void setY ( int y )
 */
HB_FUNC( QT_QRECT_SETY )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->setY( hb_parni( 2 ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QRECT_SIZE )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   }
}

/*
 * int top () const
 */
HB_FUNC( QT_QRECT_TOP )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->top() );
   }
}

/*
 * QPoint topLeft () const
 */
HB_FUNC( QT_QRECT_TOPLEFT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->topLeft() ), true ) );
   }
}

/*
 * QPoint topRight () const
 */
HB_FUNC( QT_QRECT_TOPRIGHT )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->topRight() ), true ) );
   }
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QRECT_TRANSLATE )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QRECT_TRANSLATE_1 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * QRect translated ( int dx, int dy ) const
 */
HB_FUNC( QT_QRECT_TRANSLATED )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * QRect translated ( const QPoint & offset ) const
 */
HB_FUNC( QT_QRECT_TRANSLATED_1 )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * QRect united ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_UNITED )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->united( *hbqt_par_QRect( 2 ) ) ), true ) );
   }
}

/*
 * int width () const
 */
HB_FUNC( QT_QRECT_WIDTH )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->width() );
   }
}

/*
 * int x () const
 */
HB_FUNC( QT_QRECT_X )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->x() );
   }
}

/*
 * int y () const
 */
HB_FUNC( QT_QRECT_Y )
{
   QRect * p = hbqt_par_QRect( 1 );
   if( p )
   {
      hb_retni( ( p )->y() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
