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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 35/35 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLayoutItem>
#include <QtCore/QRectF>
#include <QtCore/QSizeF>
#include <QtCore/QPointF>


/*
 * QGraphicsLayoutItem ( QGraphicsLayoutItem * parent = 0, bool isLayout = false )
 * virtual ~QGraphicsLayoutItem ()
 */

typedef struct
{
   QGraphicsLayoutItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLayoutItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLayoutItem )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsLayoutItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLayoutItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLayoutItem;
   p->type = HBQT_TYPE_QGraphicsLayoutItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsLayoutItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsLayoutItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSLAYOUTITEM )
{
   // __HB_RETPTRGC__( new QGraphicsLayoutItem() );
}

/*
 * QRectF contentsRect () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_CONTENTSRECT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->contentsRect() ), true ) );
   }
}

/*
 * QSizeF effectiveSizeHint ( Qt::SizeHint which, const QSizeF & constraint = QSizeF() ) const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_EFFECTIVESIZEHINT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->effectiveSizeHint( ( Qt::SizeHint ) hb_parni( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QSizeF( 3 ) : QSizeF() ) ) ), true ) );
   }
}

/*
 * QRectF geometry () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
   }
}

/*
 * virtual void getContentsMargins ( qreal * left, qreal * top, qreal * right, qreal * bottom ) const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GETCONTENTSMARGINS )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   qreal qrLeft = 0;
   qreal qrTop = 0;
   qreal qrRight = 0;
   qreal qrBottom = 0;

   if( p )
   {
      ( p )->getContentsMargins( &qrLeft, &qrTop, &qrRight, &qrBottom );
   }

   hb_stornd( qrLeft, 2 );
   hb_stornd( qrTop, 3 );
   hb_stornd( qrRight, 4 );
   hb_stornd( qrBottom, 5 );
}

/*
 * QGraphicsItem * graphicsItem () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GRAPHICSITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->graphicsItem(), false ) );
   }
}

/*
 * bool isLayout () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_ISLAYOUT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isLayout() );
   }
}

/*
 * qreal maximumHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->maximumHeight() );
   }
}

/*
 * QSizeF maximumSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->maximumSize() ), true ) );
   }
}

/*
 * qreal maximumWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->maximumWidth() );
   }
}

/*
 * qreal minimumHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->minimumHeight() );
   }
}

/*
 * QSizeF minimumSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->minimumSize() ), true ) );
   }
}

/*
 * qreal minimumWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->minimumWidth() );
   }
}

/*
 * bool ownedByLayout () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_OWNEDBYLAYOUT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retl( ( p )->ownedByLayout() );
   }
}

/*
 * QGraphicsLayoutItem * parentLayoutItem () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PARENTLAYOUTITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->parentLayoutItem(), false ) );
   }
}

/*
 * qreal preferredHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->preferredHeight() );
   }
}

/*
 * QSizeF preferredSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->preferredSize() ), true ) );
   }
}

/*
 * qreal preferredWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->preferredWidth() );
   }
}

/*
 * virtual void setGeometry ( const QRectF & rect )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETGEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
   }
}

/*
 * void setMaximumHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMaximumHeight( hb_parnd( 2 ) );
   }
}

/*
 * void setMaximumSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMaximumSize( *hbqt_par_QSizeF( 2 ) );
   }
}

/*
 * void setMaximumSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMaximumSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setMaximumWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMaximumWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setMinimumHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMinimumHeight( hb_parnd( 2 ) );
   }
}

/*
 * void setMinimumSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMinimumSize( *hbqt_par_QSizeF( 2 ) );
   }
}

/*
 * void setMinimumSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMinimumSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setMinimumWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setMinimumWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setParentLayoutItem ( QGraphicsLayoutItem * parent )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPARENTLAYOUTITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setParentLayoutItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
   }
}

/*
 * void setPreferredHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setPreferredHeight( hb_parnd( 2 ) );
   }
}

/*
 * void setPreferredSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setPreferredSize( *hbqt_par_QSizeF( 2 ) );
   }
}

/*
 * void setPreferredSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setPreferredSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setPreferredWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setPreferredWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setSizePolicy ( const QSizePolicy & policy )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) );
   }
}

/*
 * void setSizePolicy ( QSizePolicy::Policy hPolicy, QSizePolicy::Policy vPolicy, QSizePolicy::ControlType controlType = QSizePolicy::DefaultType )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QSizePolicy::ControlType ) hb_parni( 4 ) : ( QSizePolicy::ControlType ) QSizePolicy::DefaultType ) );
   }
}

/*
 * QSizePolicy sizePolicy () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SIZEPOLICY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) );
   }
}

/*
 * virtual void updateGeometry ()
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_UPDATEGEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
   {
      ( p )->updateGeometry();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
