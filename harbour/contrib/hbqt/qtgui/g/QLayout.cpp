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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum SizeConstraint { SetDefaultConstraint, SetFixedSize, SetMinimumSize, SetMaximumSize, SetMinAndMaxSize, SetNoConstraint }
 */

/*
 *  Constructed[ 29/31 [ 93.55% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  }
 */

#include <QtCore/QPointer>

#include <QtGui/QLayout>


/*
 * QLayout ( QWidget * parent )
 * QLayout ()
 */

typedef struct
{
   QPointer< QLayout > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QLayout;

QT_G_FUNC( hbqt_gcRelease_QLayout )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLayout( void * pObj, bool bNew )
{
   QGC_POINTER_QLayout * p = ( QGC_POINTER_QLayout * ) hb_gcAllocate( sizeof( QGC_POINTER_QLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLayout >( ( QLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLayout;
   p->type = HBQT_TYPE_QLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLAYOUT )
{

}

/*
 * bool activate ()
 */
HB_FUNC( QT_QLAYOUT_ACTIVATE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->activate() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_ACTIVATE FP=hb_retl( ( p )->activate() ); p is NULL" ) );
   }
}

/*
 * virtual void addItem ( QLayoutItem * item )
 */
HB_FUNC( QT_QLAYOUT_ADDITEM )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QLAYOUT_ADDITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QLAYOUT_ADDITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QLayout( 1 )->addItem( hbqt_par_QLayoutItem( 2 ) );
   }
}

/*
 * void addWidget ( QWidget * w )
 */
HB_FUNC( QT_QLAYOUT_ADDWIDGET )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QLAYOUT_ADDWIDGET()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QLAYOUT_ADDWIDGET() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QLayout( 1 )->addWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * QRect contentsRect () const
 */
HB_FUNC( QT_QLAYOUT_CONTENTSRECT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_CONTENTSRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual int count () const = 0
 */
HB_FUNC( QT_QLAYOUT_COUNT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * virtual Qt::Orientations expandingDirections () const
 */
HB_FUNC( QT_QLAYOUT_EXPANDINGDIRECTIONS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_EXPANDINGDIRECTIONS FP=hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() ); p is NULL" ) );
   }
}

/*
 * void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QLAYOUT_GETCONTENTSMARGINS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_GETCONTENTSMARGINS FP=( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom ); p is NULL" ) );
   }

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/*
 * virtual int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QLAYOUT_INDEXOF )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_INDEXOF FP=hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QLAYOUT_ISENABLED )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * virtual QLayoutItem * itemAt ( int index ) const = 0
 */
HB_FUNC( QT_QLAYOUT_ITEMAT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize maximumSize () const
 */
HB_FUNC( QT_QLAYOUT_MAXIMUMSIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_MAXIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * menuBar () const
 */
HB_FUNC( QT_QLAYOUT_MENUBAR )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuBar(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_MENUBAR FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuBar(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize minimumSize () const
 */
HB_FUNC( QT_QLAYOUT_MINIMUMSIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_MINIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QLAYOUT_PARENTWIDGET )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_PARENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * void removeItem ( QLayoutItem * item )
 */
HB_FUNC( QT_QLAYOUT_REMOVEITEM )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->removeItem( hbqt_par_QLayoutItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_REMOVEITEM FP=( p )->removeItem( hbqt_par_QLayoutItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeWidget ( QWidget * widget )
 */
HB_FUNC( QT_QLAYOUT_REMOVEWIDGET )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->removeWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_REMOVEWIDGET FP=( p )->removeWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool setAlignment ( QWidget * w, Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->setAlignment( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETALIGNMENT FP=hb_retl( ( p )->setAlignment( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_1 )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETALIGNMENT_1 FP=( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool setAlignment ( QLayout * l, Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_2 )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->setAlignment( hbqt_par_QLayout( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETALIGNMENT_2 FP=hb_retl( ( p )->setAlignment( hbqt_par_QLayout( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setContentsMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QLAYOUT_SETCONTENTSMARGINS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETCONTENTSMARGINS FP=( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool enable )
 */
HB_FUNC( QT_QLAYOUT_SETENABLED )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenuBar ( QWidget * widget )
 */
HB_FUNC( QT_QLAYOUT_SETMENUBAR )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setMenuBar( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETMENUBAR FP=( p )->setMenuBar( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeConstraint ( SizeConstraint )
 */
HB_FUNC( QT_QLAYOUT_SETSIZECONSTRAINT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setSizeConstraint( ( QLayout::SizeConstraint ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETSIZECONSTRAINT FP=( p )->setSizeConstraint( ( QLayout::SizeConstraint ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSpacing ( int )
 */
HB_FUNC( QT_QLAYOUT_SETSPACING )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SETSPACING FP=( p )->setSpacing( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * SizeConstraint sizeConstraint () const
 */
HB_FUNC( QT_QLAYOUT_SIZECONSTRAINT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( QLayout::SizeConstraint ) ( p )->sizeConstraint() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SIZECONSTRAINT FP=hb_retni( ( QLayout::SizeConstraint ) ( p )->sizeConstraint() ); p is NULL" ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QLAYOUT_SPACING )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_SPACING FP=hb_retni( ( p )->spacing() ); p is NULL" ) );
   }
}

/*
 * virtual QLayoutItem * takeAt ( int index ) = 0
 */
HB_FUNC( QT_QLAYOUT_TAKEAT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->takeAt( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_TAKEAT FP=hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->takeAt( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void update ()
 */
HB_FUNC( QT_QLAYOUT_UPDATE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->update();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_UPDATE FP=( p )->update(); p is NULL" ) );
   }
}

/*
 * QSize closestAcceptableSize ( const QWidget * widget, const QSize & size )
 */
HB_FUNC( QT_QLAYOUT_CLOSESTACCEPTABLESIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->closestAcceptableSize( hbqt_par_QWidget( 2 ), *hbqt_par_QSize( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUT_CLOSESTACCEPTABLESIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->closestAcceptableSize( hbqt_par_QWidget( 2 ), *hbqt_par_QSize( 3 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
