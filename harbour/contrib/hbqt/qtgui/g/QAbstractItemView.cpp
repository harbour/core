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
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  flags EditTriggers
 *  enum DragDropMode { NoDragDrop, DragOnly, DropOnly, DragDrop, InternalMove }
 *  enum EditTrigger { NoEditTriggers, CurrentChanged, DoubleClicked, SelectedClicked, ..., AllEditTriggers }
 *  enum ScrollHint { EnsureVisible, PositionAtTop, PositionAtBottom, PositionAtCenter }
 *  enum ScrollMode { ScrollPerItem, ScrollPerPixel }
 *  enum SelectionBehavior { SelectItems, SelectRows, SelectColumns }
 *  enum SelectionMode { SingleSelection, ContiguousSelection, ExtendedSelection, MultiSelection, NoSelection }
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractItemView>


/*
 * QAbstractItemView ( QWidget * parent = 0 )
 * ~QAbstractItemView ()
 */

typedef struct
{
   QPointer< QAbstractItemView > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QAbstractItemView;

QT_G_FUNC( hbqt_gcRelease_QAbstractItemView )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractItemView( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractItemView * p = ( QGC_POINTER_QAbstractItemView * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractItemView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractItemView >( ( QAbstractItemView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractItemView;
   p->type = HBQT_TYPE_QAbstractItemView;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractItemView  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractItemView", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTITEMVIEW )
{

}

/*
 * bool alternatingRowColors () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ALTERNATINGROWCOLORS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->alternatingRowColors() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ALTERNATINGROWCOLORS FP=hb_retl( ( p )->alternatingRowColors() ); p is NULL" ) );
   }
}

/*
 * int autoScrollMargin () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_AUTOSCROLLMARGIN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( p )->autoScrollMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_AUTOSCROLLMARGIN FP=hb_retni( ( p )->autoScrollMargin() ); p is NULL" ) );
   }
}

/*
 * void closePersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLOSEPERSISTENTEDITOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->closePersistentEditor( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_CLOSEPERSISTENTEDITOR FP=( p )->closePersistentEditor( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CURRENTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_CURRENTINDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) ); p is NULL" ) );
   }
}

/*
 * DragDropMode dragDropMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::DragDropMode ) ( p )->dragDropMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_DRAGDROPMODE FP=hb_retni( ( QAbstractItemView::DragDropMode ) ( p )->dragDropMode() ); p is NULL" ) );
   }
}

/*
 * bool dragDropOverwriteMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPOVERWRITEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->dragDropOverwriteMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_DRAGDROPOVERWRITEMODE FP=hb_retl( ( p )->dragDropOverwriteMode() ); p is NULL" ) );
   }
}

/*
 * bool dragEnabled () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGENABLED )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->dragEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_DRAGENABLED FP=hb_retl( ( p )->dragEnabled() ); p is NULL" ) );
   }
}

/*
 * EditTriggers editTriggers () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDITTRIGGERS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::EditTriggers ) ( p )->editTriggers() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_EDITTRIGGERS FP=hb_retni( ( QAbstractItemView::EditTriggers ) ( p )->editTriggers() ); p is NULL" ) );
   }
}

/*
 * bool hasAutoScroll () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HASAUTOSCROLL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->hasAutoScroll() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_HASAUTOSCROLL FP=hb_retl( ( p )->hasAutoScroll() ); p is NULL" ) );
   }
}

/*
 * ScrollMode horizontalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HORIZONTALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->horizontalScrollMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_HORIZONTALSCROLLMODE FP=hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->horizontalScrollMode() ); p is NULL" ) );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ICONSIZE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ICONSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QModelIndex indexAt ( const QPoint & point ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXAT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_INDEXAT FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * indexWidget ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXWIDGET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->indexWidget( *hbqt_par_QModelIndex( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_INDEXWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->indexWidget( *hbqt_par_QModelIndex( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ITEMDELEGATE FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE_1 )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate( *hbqt_par_QModelIndex( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ITEMDELEGATE_1 FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate( *hbqt_par_QModelIndex( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegateForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForColumn( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORCOLUMN FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForColumn( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegateForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForRow( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORROW FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForRow( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void keyboardSearch ( const QString & search )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_KEYBOARDSEARCH )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->keyboardSearch( QAbstractItemView::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_KEYBOARDSEARCH FP=( p )->keyboardSearch( QAbstractItemView::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_MODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_MODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) ); p is NULL" ) );
   }
}

/*
 * void openPersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_OPENPERSISTENTEDITOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->openPersistentEditor( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_OPENPERSISTENTEDITOR FP=( p )->openPersistentEditor( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex rootIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ROOTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->rootIndex() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_ROOTINDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->rootIndex() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible ) = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTO )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QAbstractItemView::EnsureVisible ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SCROLLTO FP=( p )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QAbstractItemView::EnsureVisible ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemView::SelectionBehavior selectionBehavior () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONBEHAVIOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::SelectionBehavior ) ( p )->selectionBehavior() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SELECTIONBEHAVIOR FP=hb_retni( ( QAbstractItemView::SelectionBehavior ) ( p )->selectionBehavior() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemView::SelectionMode selectionMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::SelectionMode ) ( p )->selectionMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SELECTIONMODE FP=hb_retni( ( QAbstractItemView::SelectionMode ) ( p )->selectionMode() ); p is NULL" ) );
   }
}

/*
 * QItemSelectionModel * selectionModel () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelectionModel( ( p )->selectionModel(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SELECTIONMODEL FP=hb_retptrGC( hbqt_gcAllocate_QItemSelectionModel( ( p )->selectionModel(), false ) ); p is NULL" ) );
   }
}

/*
 * void setAlternatingRowColors ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETALTERNATINGROWCOLORS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setAlternatingRowColors( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETALTERNATINGROWCOLORS FP=( p )->setAlternatingRowColors( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoScroll ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setAutoScroll( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETAUTOSCROLL FP=( p )->setAutoScroll( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoScrollMargin ( int margin )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLLMARGIN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setAutoScrollMargin( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETAUTOSCROLLMARGIN FP=( p )->setAutoScrollMargin( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDragDropMode ( DragDropMode behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setDragDropMode( ( QAbstractItemView::DragDropMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETDRAGDROPMODE FP=( p )->setDragDropMode( ( QAbstractItemView::DragDropMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDragDropOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPOVERWRITEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setDragDropOverwriteMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETDRAGDROPOVERWRITEMODE FP=( p )->setDragDropOverwriteMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDragEnabled ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGENABLED )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setDragEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETDRAGENABLED FP=( p )->setDragEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDropIndicatorShown ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDROPINDICATORSHOWN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setDropIndicatorShown( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETDROPINDICATORSHOWN FP=( p )->setDropIndicatorShown( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEditTriggers ( EditTriggers triggers )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETEDITTRIGGERS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setEditTriggers( ( QAbstractItemView::EditTriggers ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETEDITTRIGGERS FP=( p )->setEditTriggers( ( QAbstractItemView::EditTriggers ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETHORIZONTALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setHorizontalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETHORIZONTALSCROLLMODE FP=( p )->setHorizontalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETICONSIZE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETICONSIZE FP=( p )->setIconSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIndexWidget ( const QModelIndex & index, QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETINDEXWIDGET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setIndexWidget( *hbqt_par_QModelIndex( 2 ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETINDEXWIDGET FP=( p )->setIndexWidget( *hbqt_par_QModelIndex( 2 ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETITEMDELEGATE FP=( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemDelegateForColumn ( int column, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setItemDelegateForColumn( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORCOLUMN FP=( p )->setItemDelegateForColumn( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setItemDelegateForRow ( int row, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setItemDelegateForRow( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORROW FP=( p )->setItemDelegateForRow( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETMODEL FP=( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionBehavior ( QAbstractItemView::SelectionBehavior behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONBEHAVIOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setSelectionBehavior( ( QAbstractItemView::SelectionBehavior ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETSELECTIONBEHAVIOR FP=( p )->setSelectionBehavior( ( QAbstractItemView::SelectionBehavior ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionMode ( QAbstractItemView::SelectionMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setSelectionMode( ( QAbstractItemView::SelectionMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETSELECTIONMODE FP=( p )->setSelectionMode( ( QAbstractItemView::SelectionMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setSelectionModel ( QItemSelectionModel * selectionModel )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setSelectionModel( hbqt_par_QItemSelectionModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETSELECTIONMODEL FP=( p )->setSelectionModel( hbqt_par_QItemSelectionModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabKeyNavigation ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTABKEYNAVIGATION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setTabKeyNavigation( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETTABKEYNAVIGATION FP=( p )->setTabKeyNavigation( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextElideMode ( Qt::TextElideMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTEXTELIDEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setTextElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETTEXTELIDEMODE FP=( p )->setTextElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETVERTICALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setVerticalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETVERTICALSCROLLMODE FP=( p )->setVerticalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool showDropIndicator () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SHOWDROPINDICATOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->showDropIndicator() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SHOWDROPINDICATOR FP=hb_retl( ( p )->showDropIndicator() ); p is NULL" ) );
   }
}

/*
 * virtual int sizeHintForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( p )->sizeHintForColumn( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SIZEHINTFORCOLUMN FP=hb_retni( ( p )->sizeHintForColumn( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeHintForIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHintForIndex( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SIZEHINTFORINDEX FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHintForIndex( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual int sizeHintForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( p )->sizeHintForRow( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SIZEHINTFORROW FP=hb_retni( ( p )->sizeHintForRow( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool tabKeyNavigation () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TABKEYNAVIGATION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retl( ( p )->tabKeyNavigation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_TABKEYNAVIGATION FP=hb_retl( ( p )->tabKeyNavigation() ); p is NULL" ) );
   }
}

/*
 * Qt::TextElideMode textElideMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TEXTELIDEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( Qt::TextElideMode ) ( p )->textElideMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_TEXTELIDEMODE FP=hb_retni( ( Qt::TextElideMode ) ( p )->textElideMode() ); p is NULL" ) );
   }
}

/*
 * ScrollMode verticalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VERTICALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->verticalScrollMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_VERTICALSCROLLMODE FP=hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->verticalScrollMode() ); p is NULL" ) );
   }
}

/*
 * virtual QRect visualRect ( const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VISUALRECT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_VISUALRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLEARSELECTION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->clearSelection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_CLEARSELECTION FP=( p )->clearSelection(); p is NULL" ) );
   }
}

/*
 * void edit ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDIT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->edit( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_EDIT FP=( p )->edit( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void reset ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_RESET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->reset();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_RESET FP=( p )->reset(); p is NULL" ) );
   }
}

/*
 * void scrollToBottom ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOBOTTOM )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->scrollToBottom();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SCROLLTOBOTTOM FP=( p )->scrollToBottom(); p is NULL" ) );
   }
}

/*
 * void scrollToTop ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOTOP )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->scrollToTop();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SCROLLTOTOP FP=( p )->scrollToTop(); p is NULL" ) );
   }
}

/*
 * virtual void selectAll ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTALL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->selectAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SELECTALL FP=( p )->selectAll(); p is NULL" ) );
   }
}

/*
 * void setCurrentIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETCURRENTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETCURRENTINDEX FP=( p )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setRootIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETROOTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->setRootIndex( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_SETROOTINDEX FP=( p )->setRootIndex( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * void update ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_UPDATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
      ( p )->update( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTITEMVIEW_UPDATE FP=( p )->update( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
