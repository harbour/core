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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum SizeConstraint { SetDefaultConstraint, SetFixedSize, SetMinimumSize, SetMaximumSize, SetMinAndMaxSize, SetNoConstraint }
 */

#include <QtCore/QPointer>

#include <QtGui/QLayout>


/*
 * QLayout ( QWidget * parent )
 * QLayout ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QLayout > pq;
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

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLayout;

   if( bNew )
   {
      new( & p->pq ) QPointer< QLayout >( ( QLayout * ) pObj );
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
   hb_retl( hbqt_par_QLayout( 1 )->activate() );
}

/*
 * virtual void addItem ( QLayoutItem * item ) = 0
 */
HB_FUNC( QT_QLAYOUT_ADDITEM )
{
   hbqt_par_QLayout( 1 )->addItem( hbqt_par_QLayoutItem( 2 ) );
}

/*
 * void addWidget ( QWidget * w )
 */
HB_FUNC( QT_QLAYOUT_ADDWIDGET )
{
   hbqt_par_QLayout( 1 )->addWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * QRect contentsRect () const
 */
HB_FUNC( QT_QLAYOUT_CONTENTSRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QLayout( 1 )->contentsRect() ), true ) );
}

/*
 * virtual int count () const = 0
 */
HB_FUNC( QT_QLAYOUT_COUNT )
{
   hb_retni( hbqt_par_QLayout( 1 )->count() );
}

/*
 * virtual Qt::Orientations expandingDirections () const
 */
HB_FUNC( QT_QLAYOUT_EXPANDINGDIRECTIONS )
{
   hb_retni( ( Qt::Orientations ) hbqt_par_QLayout( 1 )->expandingDirections() );
}

/*
 * void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QLAYOUT_GETCONTENTSMARGINS )
{
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   hbqt_par_QLayout( 1 )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );

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
   hb_retni( hbqt_par_QLayout( 1 )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QLAYOUT_ISENABLED )
{
   hb_retl( hbqt_par_QLayout( 1 )->isEnabled() );
}

/*
 * virtual QLayoutItem * itemAt ( int index ) const = 0
 */
HB_FUNC( QT_QLAYOUT_ITEMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QLayoutItem( hbqt_par_QLayout( 1 )->itemAt( hb_parni( 2 ) ), false ) );
}

/*
 * virtual QSize maximumSize () const
 */
HB_FUNC( QT_QLAYOUT_MAXIMUMSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QLayout( 1 )->maximumSize() ), true ) );
}

/*
 * QWidget * menuBar () const
 */
HB_FUNC( QT_QLAYOUT_MENUBAR )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QLayout( 1 )->menuBar(), false ) );
}

/*
 * virtual QSize minimumSize () const
 */
HB_FUNC( QT_QLAYOUT_MINIMUMSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QLayout( 1 )->minimumSize() ), true ) );
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QLAYOUT_PARENTWIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QLayout( 1 )->parentWidget(), false ) );
}

/*
 * void removeItem ( QLayoutItem * item )
 */
HB_FUNC( QT_QLAYOUT_REMOVEITEM )
{
   hbqt_par_QLayout( 1 )->removeItem( hbqt_par_QLayoutItem( 2 ) );
}

/*
 * void removeWidget ( QWidget * widget )
 */
HB_FUNC( QT_QLAYOUT_REMOVEWIDGET )
{
   hbqt_par_QLayout( 1 )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * bool setAlignment ( QWidget * w, Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT )
{
   hb_retl( hbqt_par_QLayout( 1 )->setAlignment( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_1 )
{
   hbqt_par_QLayout( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * bool setAlignment ( QLayout * l, Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_2 )
{
   hb_retl( hbqt_par_QLayout( 1 )->setAlignment( hbqt_par_QLayout( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
}

/*
 * void setContentsMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QLAYOUT_SETCONTENTSMARGINS )
{
   hbqt_par_QLayout( 1 )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setEnabled ( bool enable )
 */
HB_FUNC( QT_QLAYOUT_SETENABLED )
{
   hbqt_par_QLayout( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
 * void setMenuBar ( QWidget * widget )
 */
HB_FUNC( QT_QLAYOUT_SETMENUBAR )
{
   hbqt_par_QLayout( 1 )->setMenuBar( hbqt_par_QWidget( 2 ) );
}

/*
 * void setSizeConstraint ( SizeConstraint )
 */
HB_FUNC( QT_QLAYOUT_SETSIZECONSTRAINT )
{
   hbqt_par_QLayout( 1 )->setSizeConstraint( ( QLayout::SizeConstraint ) hb_parni( 2 ) );
}

/*
 * void setSpacing ( int )
 */
HB_FUNC( QT_QLAYOUT_SETSPACING )
{
   hbqt_par_QLayout( 1 )->setSpacing( hb_parni( 2 ) );
}

/*
 * SizeConstraint sizeConstraint () const
 */
HB_FUNC( QT_QLAYOUT_SIZECONSTRAINT )
{
   hb_retni( ( QLayout::SizeConstraint ) hbqt_par_QLayout( 1 )->sizeConstraint() );
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QLAYOUT_SPACING )
{
   hb_retni( hbqt_par_QLayout( 1 )->spacing() );
}

/*
 * virtual QLayoutItem * takeAt ( int index ) = 0
 */
HB_FUNC( QT_QLAYOUT_TAKEAT )
{
   hb_retptrGC( hbqt_gcAllocate_QLayoutItem( hbqt_par_QLayout( 1 )->takeAt( hb_parni( 2 ) ), false ) );
}

/*
 * void update ()
 */
HB_FUNC( QT_QLAYOUT_UPDATE )
{
   hbqt_par_QLayout( 1 )->update();
}

/*
 * QSize closestAcceptableSize ( const QWidget * widget, const QSize & size )
 */
HB_FUNC( QT_QLAYOUT_CLOSESTACCEPTABLESIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QLayout( 1 )->closestAcceptableSize( hbqt_par_QWidget( 2 ), *hbqt_par_QSize( 3 ) ) ), true ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
