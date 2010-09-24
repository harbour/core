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
 *  flags EditTriggers
 *  enum DragDropMode { NoDragDrop, DragOnly, DropOnly, DragDrop, InternalMove }
 *  enum EditTrigger { NoEditTriggers, CurrentChanged, DoubleClicked, SelectedClicked, ..., AllEditTriggers }
 *  enum ScrollHint { EnsureVisible, PositionAtTop, PositionAtBottom, PositionAtCenter }
 *  enum ScrollMode { ScrollPerItem, ScrollPerPixel }
 *  enum SelectionBehavior { SelectItems, SelectRows, SelectColumns }
 *  enum SelectionMode { SingleSelection, ContiguousSelection, ExtendedSelection, MultiSelection, NoSelection }
 */

/*
 *  Constructed[ 63/63 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractItemView;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractItemView )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractItemView( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractItemView * p = ( HBQT_GC_T_QAbstractItemView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractItemView ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->alternatingRowColors() );
   }
}

/*
 * int autoScrollMargin () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_AUTOSCROLLMARGIN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( p )->autoScrollMargin() );
   }
}

/*
 * void closePersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLOSEPERSISTENTEDITOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->closePersistentEditor( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CURRENTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
   }
}

/*
 * DragDropMode dragDropMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::DragDropMode ) ( p )->dragDropMode() );
   }
}

/*
 * bool dragDropOverwriteMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPOVERWRITEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retl( ( p )->dragDropOverwriteMode() );
   }
}

/*
 * bool dragEnabled () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGENABLED )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retl( ( p )->dragEnabled() );
   }
}

/*
 * EditTriggers editTriggers () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDITTRIGGERS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::EditTriggers ) ( p )->editTriggers() );
   }
}

/*
 * bool hasAutoScroll () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HASAUTOSCROLL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retl( ( p )->hasAutoScroll() );
   }
}

/*
 * ScrollMode horizontalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HORIZONTALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->horizontalScrollMode() );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ICONSIZE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   }
}

/*
 * virtual QModelIndex indexAt ( const QPoint & point ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXAT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * QWidget * indexWidget ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXWIDGET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->indexWidget( *hbqt_par_QModelIndex( 2 ) ), false ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE_1 )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate( *hbqt_par_QModelIndex( 2 ) ), false ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegateForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForColumn( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegateForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegateForRow( hb_parni( 2 ) ), false ) );
   }
}

/*
 * virtual void keyboardSearch ( const QString & search )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_KEYBOARDSEARCH )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      void * pText;
      ( p )->keyboardSearch( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_MODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) );
   }
}

/*
 * void openPersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_OPENPERSISTENTEDITOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->openPersistentEditor( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * QModelIndex rootIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ROOTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->rootIndex() ), true ) );
   }
}

/*
 * virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible ) = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTO )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QAbstractItemView::EnsureVisible ) );
   }
}

/*
 * QAbstractItemView::SelectionBehavior selectionBehavior () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONBEHAVIOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::SelectionBehavior ) ( p )->selectionBehavior() );
   }
}

/*
 * QAbstractItemView::SelectionMode selectionMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::SelectionMode ) ( p )->selectionMode() );
   }
}

/*
 * QItemSelectionModel * selectionModel () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QItemSelectionModel( ( p )->selectionModel(), false ) );
   }
}

/*
 * void setAlternatingRowColors ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETALTERNATINGROWCOLORS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setAlternatingRowColors( hb_parl( 2 ) );
   }
}

/*
 * void setAutoScroll ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setAutoScroll( hb_parl( 2 ) );
   }
}

/*
 * void setAutoScrollMargin ( int margin )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLLMARGIN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setAutoScrollMargin( hb_parni( 2 ) );
   }
}

/*
 * void setDragDropMode ( DragDropMode behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setDragDropMode( ( QAbstractItemView::DragDropMode ) hb_parni( 2 ) );
   }
}

/*
 * void setDragDropOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPOVERWRITEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setDragDropOverwriteMode( hb_parl( 2 ) );
   }
}

/*
 * void setDragEnabled ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGENABLED )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setDragEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setDropIndicatorShown ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDROPINDICATORSHOWN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setDropIndicatorShown( hb_parl( 2 ) );
   }
}

/*
 * void setEditTriggers ( EditTriggers triggers )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETEDITTRIGGERS )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setEditTriggers( ( QAbstractItemView::EditTriggers ) hb_parni( 2 ) );
   }
}

/*
 * void setHorizontalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETHORIZONTALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setHorizontalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
   }
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETICONSIZE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setIndexWidget ( const QModelIndex & index, QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETINDEXWIDGET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setIndexWidget( *hbqt_par_QModelIndex( 2 ), hbqt_par_QWidget( 3 ) );
   }
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
   }
}

/*
 * void setItemDelegateForColumn ( int column, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setItemDelegateForColumn( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
   }
}

/*
 * void setItemDelegateForRow ( int row, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setItemDelegateForRow( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
   }
}

/*
 * virtual void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
   }
}

/*
 * void setSelectionBehavior ( QAbstractItemView::SelectionBehavior behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONBEHAVIOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setSelectionBehavior( ( QAbstractItemView::SelectionBehavior ) hb_parni( 2 ) );
   }
}

/*
 * void setSelectionMode ( QAbstractItemView::SelectionMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setSelectionMode( ( QAbstractItemView::SelectionMode ) hb_parni( 2 ) );
   }
}

/*
 * virtual void setSelectionModel ( QItemSelectionModel * selectionModel )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODEL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setSelectionModel( hbqt_par_QItemSelectionModel( 2 ) );
   }
}

/*
 * void setTabKeyNavigation ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTABKEYNAVIGATION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setTabKeyNavigation( hb_parl( 2 ) );
   }
}

/*
 * void setTextElideMode ( Qt::TextElideMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTEXTELIDEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setTextElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
   }
}

/*
 * void setVerticalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETVERTICALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setVerticalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
   }
}

/*
 * bool showDropIndicator () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SHOWDROPINDICATOR )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retl( ( p )->showDropIndicator() );
   }
}

/*
 * virtual int sizeHintForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORCOLUMN )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( p )->sizeHintForColumn( hb_parni( 2 ) ) );
   }
}

/*
 * QSize sizeHintForIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHintForIndex( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * virtual int sizeHintForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORROW )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( p )->sizeHintForRow( hb_parni( 2 ) ) );
   }
}

/*
 * bool tabKeyNavigation () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TABKEYNAVIGATION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retl( ( p )->tabKeyNavigation() );
   }
}

/*
 * Qt::TextElideMode textElideMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TEXTELIDEMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextElideMode ) ( p )->textElideMode() );
   }
}

/*
 * ScrollMode verticalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VERTICALSCROLLMODE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retni( ( QAbstractItemView::ScrollMode ) ( p )->verticalScrollMode() );
   }
}

/*
 * virtual QRect visualRect ( const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VISUALRECT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLEARSELECTION )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->clearSelection();
   }
}

/*
 * void edit ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDIT )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->edit( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * virtual void reset ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_RESET )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->reset();
   }
}

/*
 * void scrollToBottom ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOBOTTOM )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->scrollToBottom();
   }
}

/*
 * void scrollToTop ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOTOP )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->scrollToTop();
   }
}

/*
 * virtual void selectAll ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTALL )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->selectAll();
   }
}

/*
 * void setCurrentIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETCURRENTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * virtual void setRootIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETROOTINDEX )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->setRootIndex( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * void update ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_UPDATE )
{
   QAbstractItemView * p = hbqt_par_QAbstractItemView( 1 );
   if( p )
   {
      ( p )->update( *hbqt_par_QModelIndex( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
