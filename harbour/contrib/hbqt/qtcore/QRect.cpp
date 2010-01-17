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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QRect;

QT_G_FUNC( hbqt_gcRelease_QRect )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QRect * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QRect                      ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QRect                       Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QRect                       Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QRect( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRect;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QRect                      ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QRECT )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QRect* ) new QRect( *hbqt_par_QRect( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = ( QRect* ) new QRect( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = ( QRect* ) new QRect( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ;
   }
   else
   {
      pObj = ( QRect* ) new QRect() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRect( pObj, true ) );
}
/*
 * void adjust ( int dx1, int dy1, int dx2, int dy2 )
 */
HB_FUNC( QT_QRECT_ADJUST )
{
   hbqt_par_QRect( 1 )->adjust( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * QRect adjusted ( int dx1, int dy1, int dx2, int dy2 ) const
 */
HB_FUNC( QT_QRECT_ADJUSTED )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->adjusted( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
}

/*
 * int bottom () const
 */
HB_FUNC( QT_QRECT_BOTTOM )
{
   hb_retni( hbqt_par_QRect( 1 )->bottom() );
}

/*
 * QPoint bottomLeft () const
 */
HB_FUNC( QT_QRECT_BOTTOMLEFT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QRect( 1 )->bottomLeft() ), true ) );
}

/*
 * QPoint bottomRight () const
 */
HB_FUNC( QT_QRECT_BOTTOMRIGHT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QRect( 1 )->bottomRight() ), true ) );
}

/*
 * QPoint center () const
 */
HB_FUNC( QT_QRECT_CENTER )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QRect( 1 )->center() ), true ) );
}

/*
 * bool contains ( const QPoint & point, bool proper = false ) const
 */
HB_FUNC( QT_QRECT_CONTAINS )
{
   hb_retl( hbqt_par_QRect( 1 )->contains( *hbqt_par_QPoint( 2 ), hb_parl( 3 ) ) );
}

/*
 * bool contains ( int x, int y, bool proper ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_1 )
{
   hb_retl( hbqt_par_QRect( 1 )->contains( hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ) );
}

/*
 * bool contains ( int x, int y ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_2 )
{
   hb_retl( hbqt_par_QRect( 1 )->contains( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * bool contains ( const QRect & rectangle, bool proper = false ) const
 */
HB_FUNC( QT_QRECT_CONTAINS_3 )
{
   hb_retl( hbqt_par_QRect( 1 )->contains( *hbqt_par_QRect( 2 ), hb_parl( 3 ) ) );
}

/*
 * void getCoords ( int * x1, int * y1, int * x2, int * y2 ) const
 */
HB_FUNC( QT_QRECT_GETCOORDS )
{
   int iX1 = 0;
   int iY1 = 0;
   int iX2 = 0;
   int iY2 = 0;

   hbqt_par_QRect( 1 )->getCoords( &iX1, &iY1, &iX2, &iY2 );

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
   int iX = 0;
   int iY = 0;
   int iWidth = 0;
   int iHeight = 0;

   hbqt_par_QRect( 1 )->getRect( &iX, &iY, &iWidth, &iHeight );

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
   hb_retni( hbqt_par_QRect( 1 )->height() );
}

/*
 * QRect intersected ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_INTERSECTED )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->intersected( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/*
 * bool intersects ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_INTERSECTS )
{
   hb_retl( hbqt_par_QRect( 1 )->intersects( *hbqt_par_QRect( 2 ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QRECT_ISEMPTY )
{
   hb_retl( hbqt_par_QRect( 1 )->isEmpty() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QRECT_ISNULL )
{
   hb_retl( hbqt_par_QRect( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRECT_ISVALID )
{
   hb_retl( hbqt_par_QRect( 1 )->isValid() );
}

/*
 * int left () const
 */
HB_FUNC( QT_QRECT_LEFT )
{
   hb_retni( hbqt_par_QRect( 1 )->left() );
}

/*
 * void moveBottom ( int y )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOM )
{
   hbqt_par_QRect( 1 )->moveBottom( hb_parni( 2 ) );
}

/*
 * void moveBottomLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOMLEFT )
{
   hbqt_par_QRect( 1 )->moveBottomLeft( *hbqt_par_QPoint( 2 ) );
}

/*
 * void moveBottomRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVEBOTTOMRIGHT )
{
   hbqt_par_QRect( 1 )->moveBottomRight( *hbqt_par_QPoint( 2 ) );
}

/*
 * void moveCenter ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVECENTER )
{
   hbqt_par_QRect( 1 )->moveCenter( *hbqt_par_QPoint( 2 ) );
}

/*
 * void moveLeft ( int x )
 */
HB_FUNC( QT_QRECT_MOVELEFT )
{
   hbqt_par_QRect( 1 )->moveLeft( hb_parni( 2 ) );
}

/*
 * void moveRight ( int x )
 */
HB_FUNC( QT_QRECT_MOVERIGHT )
{
   hbqt_par_QRect( 1 )->moveRight( hb_parni( 2 ) );
}

/*
 * void moveTo ( int x, int y )
 */
HB_FUNC( QT_QRECT_MOVETO )
{
   hbqt_par_QRect( 1 )->moveTo( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void moveTo ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETO_1 )
{
   hbqt_par_QRect( 1 )->moveTo( *hbqt_par_QPoint( 2 ) );
}

/*
 * void moveTop ( int y )
 */
HB_FUNC( QT_QRECT_MOVETOP )
{
   hbqt_par_QRect( 1 )->moveTop( hb_parni( 2 ) );
}

/*
 * void moveTopLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETOPLEFT )
{
   hbqt_par_QRect( 1 )->moveTopLeft( *hbqt_par_QPoint( 2 ) );
}

/*
 * void moveTopRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_MOVETOPRIGHT )
{
   hbqt_par_QRect( 1 )->moveTopRight( *hbqt_par_QPoint( 2 ) );
}

/*
 * QRect normalized () const
 */
HB_FUNC( QT_QRECT_NORMALIZED )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->normalized() ), true ) );
}

/*
 * int right () const
 */
HB_FUNC( QT_QRECT_RIGHT )
{
   hb_retni( hbqt_par_QRect( 1 )->right() );
}

/*
 * void setBottom ( int y )
 */
HB_FUNC( QT_QRECT_SETBOTTOM )
{
   hbqt_par_QRect( 1 )->setBottom( hb_parni( 2 ) );
}

/*
 * void setBottomLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETBOTTOMLEFT )
{
   hbqt_par_QRect( 1 )->setBottomLeft( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setBottomRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETBOTTOMRIGHT )
{
   hbqt_par_QRect( 1 )->setBottomRight( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setCoords ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QRECT_SETCOORDS )
{
   hbqt_par_QRect( 1 )->setCoords( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setHeight ( int height )
 */
HB_FUNC( QT_QRECT_SETHEIGHT )
{
   hbqt_par_QRect( 1 )->setHeight( hb_parni( 2 ) );
}

/*
 * void setLeft ( int x )
 */
HB_FUNC( QT_QRECT_SETLEFT )
{
   hbqt_par_QRect( 1 )->setLeft( hb_parni( 2 ) );
}

/*
 * void setRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QRECT_SETRECT )
{
   hbqt_par_QRect( 1 )->setRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setRight ( int x )
 */
HB_FUNC( QT_QRECT_SETRIGHT )
{
   hbqt_par_QRect( 1 )->setRight( hb_parni( 2 ) );
}

/*
 * void setSize ( const QSize & size )
 */
HB_FUNC( QT_QRECT_SETSIZE )
{
   hbqt_par_QRect( 1 )->setSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setTop ( int y )
 */
HB_FUNC( QT_QRECT_SETTOP )
{
   hbqt_par_QRect( 1 )->setTop( hb_parni( 2 ) );
}

/*
 * void setTopLeft ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETTOPLEFT )
{
   hbqt_par_QRect( 1 )->setTopLeft( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setTopRight ( const QPoint & position )
 */
HB_FUNC( QT_QRECT_SETTOPRIGHT )
{
   hbqt_par_QRect( 1 )->setTopRight( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setWidth ( int width )
 */
HB_FUNC( QT_QRECT_SETWIDTH )
{
   hbqt_par_QRect( 1 )->setWidth( hb_parni( 2 ) );
}

/*
 * void setX ( int x )
 */
HB_FUNC( QT_QRECT_SETX )
{
   hbqt_par_QRect( 1 )->setX( hb_parni( 2 ) );
}

/*
 * void setY ( int y )
 */
HB_FUNC( QT_QRECT_SETY )
{
   hbqt_par_QRect( 1 )->setY( hb_parni( 2 ) );
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QRECT_SIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QRect( 1 )->size() ), true ) );
}

/*
 * int top () const
 */
HB_FUNC( QT_QRECT_TOP )
{
   hb_retni( hbqt_par_QRect( 1 )->top() );
}

/*
 * QPoint topLeft () const
 */
HB_FUNC( QT_QRECT_TOPLEFT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QRect( 1 )->topLeft() ), true ) );
}

/*
 * QPoint topRight () const
 */
HB_FUNC( QT_QRECT_TOPRIGHT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QRect( 1 )->topRight() ), true ) );
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QRECT_TRANSLATE )
{
   hbqt_par_QRect( 1 )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QRECT_TRANSLATE_1 )
{
   hbqt_par_QRect( 1 )->translate( *hbqt_par_QPoint( 2 ) );
}

/*
 * QRect translated ( int dx, int dy ) const
 */
HB_FUNC( QT_QRECT_TRANSLATED )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
}

/*
 * QRect translated ( const QPoint & offset ) const
 */
HB_FUNC( QT_QRECT_TRANSLATED_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/*
 * QRect united ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QRECT_UNITED )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QRect( 1 )->united( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/*
 * int width () const
 */
HB_FUNC( QT_QRECT_WIDTH )
{
   hb_retni( hbqt_par_QRect( 1 )->width() );
}

/*
 * int x () const
 */
HB_FUNC( QT_QRECT_X )
{
   hb_retni( hbqt_par_QRect( 1 )->x() );
}

/*
 * int y () const
 */
HB_FUNC( QT_QRECT_Y )
{
   hb_retni( hbqt_par_QRect( 1 )->y() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
