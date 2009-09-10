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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtCore/QRectF>


/*
 * QRectF ()
 * QRectF ( const QPointF & topLeft, const QSizeF & size )
 * QRectF ( const QPointF & topLeft, const QPointF & bottomRight )
 * QRectF ( qreal x, qreal y, qreal width, qreal height )
 * QRectF ( const QRect & rectangle )
 * ~QRectF ()
 */
HB_FUNC( QT_QRECTF )
{
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      hb_retptr( ( QRectF* ) new QRectF( *hbqt_par_QRectF( 1 ) ) );
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      hb_retptr( ( QRectF* ) new QRectF( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) );
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      hb_retptr( ( QRectF* ) new QRectF( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) );
   }
   else
   {
      hb_retptr( ( QRectF* ) new QRectF() );
   }
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QRECTF_DESTROY )
{
   hbqt_par_QRectF( 1 )->~QRectF();
}

/*
 * void adjust ( qreal dx1, qreal dy1, qreal dx2, qreal dy2 )
 */
HB_FUNC( QT_QRECTF_ADJUST )
{
   hbqt_par_QRectF( 1 )->adjust( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * QRectF adjusted ( qreal dx1, qreal dy1, qreal dx2, qreal dy2 ) const
 */
HB_FUNC( QT_QRECTF_ADJUSTED )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->adjusted( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ) );
}

/*
 * qreal bottom () const
 */
HB_FUNC( QT_QRECTF_BOTTOM )
{
   hb_retnd( hbqt_par_QRectF( 1 )->bottom() );
}

/*
 * QPointF bottomLeft () const
 */
HB_FUNC( QT_QRECTF_BOTTOMLEFT )
{
   hb_retptr( new QPointF( hbqt_par_QRectF( 1 )->bottomLeft() ) );
}

/*
 * QPointF bottomRight () const
 */
HB_FUNC( QT_QRECTF_BOTTOMRIGHT )
{
   hb_retptr( new QPointF( hbqt_par_QRectF( 1 )->bottomRight() ) );
}

/*
 * QPointF center () const
 */
HB_FUNC( QT_QRECTF_CENTER )
{
   hb_retptr( new QPointF( hbqt_par_QRectF( 1 )->center() ) );
}

/*
 * bool contains ( const QPointF & point ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS )
{
   hb_retl( hbqt_par_QRectF( 1 )->contains( *hbqt_par_QPointF( 2 ) ) );
}

/*
 * bool contains ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS_1 )
{
   hb_retl( hbqt_par_QRectF( 1 )->contains( hb_parnd( 2 ), hb_parnd( 3 ) ) );
}

/*
 * bool contains ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_CONTAINS_2 )
{
   hb_retl( hbqt_par_QRectF( 1 )->contains( *hbqt_par_QRectF( 2 ) ) );
}

/*
 * void getCoords ( qreal * x1, qreal * y1, qreal * x2, qreal * y2 ) const
 */
HB_FUNC( QT_QRECTF_GETCOORDS )
{
   qreal qrX1 = 0;
   qreal qrY1 = 0;
   qreal qrX2 = 0;
   qreal qrY2 = 0;

   hbqt_par_QRectF( 1 )->getCoords( &qrX1, &qrY1, &qrX2, &qrY2 );

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
   qreal qrX = 0;
   qreal qrY = 0;
   qreal qrWidth = 0;
   qreal qrHeight = 0;

   hbqt_par_QRectF( 1 )->getRect( &qrX, &qrY, &qrWidth, &qrHeight );

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
   hb_retnd( hbqt_par_QRectF( 1 )->height() );
}

/*
 * QRectF intersected ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_INTERSECTED )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->intersected( *hbqt_par_QRectF( 2 ) ) ) );
}

/*
 * bool intersects ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_INTERSECTS )
{
   hb_retl( hbqt_par_QRectF( 1 )->intersects( *hbqt_par_QRectF( 2 ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QRECTF_ISEMPTY )
{
   hb_retl( hbqt_par_QRectF( 1 )->isEmpty() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QRECTF_ISNULL )
{
   hb_retl( hbqt_par_QRectF( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRECTF_ISVALID )
{
   hb_retl( hbqt_par_QRectF( 1 )->isValid() );
}

/*
 * qreal left () const
 */
HB_FUNC( QT_QRECTF_LEFT )
{
   hb_retnd( hbqt_par_QRectF( 1 )->left() );
}

/*
 * void moveBottom ( qreal y )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOM )
{
   hbqt_par_QRectF( 1 )->moveBottom( hb_parnd( 2 ) );
}

/*
 * void moveBottomLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOMLEFT )
{
   hbqt_par_QRectF( 1 )->moveBottomLeft( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveBottomRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVEBOTTOMRIGHT )
{
   hbqt_par_QRectF( 1 )->moveBottomRight( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveCenter ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVECENTER )
{
   hbqt_par_QRectF( 1 )->moveCenter( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveLeft ( qreal x )
 */
HB_FUNC( QT_QRECTF_MOVELEFT )
{
   hbqt_par_QRectF( 1 )->moveLeft( hb_parnd( 2 ) );
}

/*
 * void moveRight ( qreal x )
 */
HB_FUNC( QT_QRECTF_MOVERIGHT )
{
   hbqt_par_QRectF( 1 )->moveRight( hb_parnd( 2 ) );
}

/*
 * void moveTo ( qreal x, qreal y )
 */
HB_FUNC( QT_QRECTF_MOVETO )
{
   hbqt_par_QRectF( 1 )->moveTo( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void moveTo ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETO_1 )
{
   hbqt_par_QRectF( 1 )->moveTo( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveTop ( qreal y )
 */
HB_FUNC( QT_QRECTF_MOVETOP )
{
   hbqt_par_QRectF( 1 )->moveTop( hb_parnd( 2 ) );
}

/*
 * void moveTopLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETOPLEFT )
{
   hbqt_par_QRectF( 1 )->moveTopLeft( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveTopRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_MOVETOPRIGHT )
{
   hbqt_par_QRectF( 1 )->moveTopRight( *hbqt_par_QPointF( 2 ) );
}

/*
 * QRectF normalized () const
 */
HB_FUNC( QT_QRECTF_NORMALIZED )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->normalized() ) );
}

/*
 * qreal right () const
 */
HB_FUNC( QT_QRECTF_RIGHT )
{
   hb_retnd( hbqt_par_QRectF( 1 )->right() );
}

/*
 * void setBottom ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETBOTTOM )
{
   hbqt_par_QRectF( 1 )->setBottom( hb_parnd( 2 ) );
}

/*
 * void setBottomLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETBOTTOMLEFT )
{
   hbqt_par_QRectF( 1 )->setBottomLeft( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setBottomRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETBOTTOMRIGHT )
{
   hbqt_par_QRectF( 1 )->setBottomRight( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setCoords ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */
HB_FUNC( QT_QRECTF_SETCOORDS )
{
   hbqt_par_QRectF( 1 )->setCoords( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QRECTF_SETHEIGHT )
{
   hbqt_par_QRectF( 1 )->setHeight( hb_parnd( 2 ) );
}

/*
 * void setLeft ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETLEFT )
{
   hbqt_par_QRectF( 1 )->setLeft( hb_parnd( 2 ) );
}

/*
 * void setRect ( qreal x, qreal y, qreal width, qreal height )
 */
HB_FUNC( QT_QRECTF_SETRECT )
{
   hbqt_par_QRectF( 1 )->setRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void setRight ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETRIGHT )
{
   hbqt_par_QRectF( 1 )->setRight( hb_parnd( 2 ) );
}

/*
 * void setSize ( const QSizeF & size )
 */
HB_FUNC( QT_QRECTF_SETSIZE )
{
   hbqt_par_QRectF( 1 )->setSize( *hbqt_par_QSizeF( 2 ) );
}

/*
 * void setTop ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETTOP )
{
   hbqt_par_QRectF( 1 )->setTop( hb_parnd( 2 ) );
}

/*
 * void setTopLeft ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETTOPLEFT )
{
   hbqt_par_QRectF( 1 )->setTopLeft( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setTopRight ( const QPointF & position )
 */
HB_FUNC( QT_QRECTF_SETTOPRIGHT )
{
   hbqt_par_QRectF( 1 )->setTopRight( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QRECTF_SETWIDTH )
{
   hbqt_par_QRectF( 1 )->setWidth( hb_parnd( 2 ) );
}

/*
 * void setX ( qreal x )
 */
HB_FUNC( QT_QRECTF_SETX )
{
   hbqt_par_QRectF( 1 )->setX( hb_parnd( 2 ) );
}

/*
 * void setY ( qreal y )
 */
HB_FUNC( QT_QRECTF_SETY )
{
   hbqt_par_QRectF( 1 )->setY( hb_parnd( 2 ) );
}

/*
 * QSizeF size () const
 */
HB_FUNC( QT_QRECTF_SIZE )
{
   hb_retptr( new QSizeF( hbqt_par_QRectF( 1 )->size() ) );
}

/*
 * QRect toAlignedRect () const
 */
HB_FUNC( QT_QRECTF_TOALIGNEDRECT )
{
   hb_retptr( new QRect( hbqt_par_QRectF( 1 )->toAlignedRect() ) );
}

/*
 * QRect toRect () const
 */
HB_FUNC( QT_QRECTF_TORECT )
{
   hb_retptr( new QRect( hbqt_par_QRectF( 1 )->toRect() ) );
}

/*
 * qreal top () const
 */
HB_FUNC( QT_QRECTF_TOP )
{
   hb_retnd( hbqt_par_QRectF( 1 )->top() );
}

/*
 * QPointF topLeft () const
 */
HB_FUNC( QT_QRECTF_TOPLEFT )
{
   hb_retptr( new QPointF( hbqt_par_QRectF( 1 )->topLeft() ) );
}

/*
 * QPointF topRight () const
 */
HB_FUNC( QT_QRECTF_TOPRIGHT )
{
   hb_retptr( new QPointF( hbqt_par_QRectF( 1 )->topRight() ) );
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QRECTF_TRANSLATE )
{
   hbqt_par_QRectF( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QRECTF_TRANSLATE_1 )
{
   hbqt_par_QRectF( 1 )->translate( *hbqt_par_QPointF( 2 ) );
}

/*
 * QRectF translated ( qreal dx, qreal dy ) const
 */
HB_FUNC( QT_QRECTF_TRANSLATED )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->translated( hb_parnd( 2 ), hb_parnd( 3 ) ) ) );
}

/*
 * QRectF translated ( const QPointF & offset ) const
 */
HB_FUNC( QT_QRECTF_TRANSLATED_1 )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->translated( *hbqt_par_QPointF( 2 ) ) ) );
}

/*
 * QRectF united ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QRECTF_UNITED )
{
   hb_retptr( new QRectF( hbqt_par_QRectF( 1 )->united( *hbqt_par_QRectF( 2 ) ) ) );
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QRECTF_WIDTH )
{
   hb_retnd( hbqt_par_QRectF( 1 )->width() );
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QRECTF_X )
{
   hb_retnd( hbqt_par_QRectF( 1 )->x() );
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QRECTF_Y )
{
   hb_retnd( hbqt_par_QRectF( 1 )->y() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
