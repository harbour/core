/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

/*
 *  Constructed[ 62/63 [ 98.41% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void update ( const QModelIndex & index
 */


#include <QtGui/QAbstractItemView>


/*
 * QAbstractItemView ( QWidget * parent = 0 )
 * ~QAbstractItemView ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW )
{

}

/*
 * bool alternatingRowColors () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ALTERNATINGROWCOLORS )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->alternatingRowColors() );
}

/*
 * int autoScrollMargin () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_AUTOSCROLLMARGIN )
{
   hb_retni( hbqt_par_QAbstractItemView( 1 )->autoScrollMargin() );
}

/*
 * void closePersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLOSEPERSISTENTEDITOR )
{
   hbqt_par_QAbstractItemView( 1 )->closePersistentEditor( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CURRENTINDEX )
{
   hb_retptr( new QModelIndex( hbqt_par_QAbstractItemView( 1 )->currentIndex() ) );
}

/*
 * DragDropMode dragDropMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPMODE )
{
   hb_retni( ( QAbstractItemView::DragDropMode ) hbqt_par_QAbstractItemView( 1 )->dragDropMode() );
}

/*
 * bool dragDropOverwriteMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGDROPOVERWRITEMODE )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->dragDropOverwriteMode() );
}

/*
 * bool dragEnabled () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_DRAGENABLED )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->dragEnabled() );
}

/*
 * EditTriggers editTriggers () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDITTRIGGERS )
{
   hb_retni( ( QAbstractItemView::EditTriggers ) hbqt_par_QAbstractItemView( 1 )->editTriggers() );
}

/*
 * bool hasAutoScroll () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HASAUTOSCROLL )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->hasAutoScroll() );
}

/*
 * ScrollMode horizontalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_HORIZONTALSCROLLMODE )
{
   hb_retni( ( QAbstractItemView::ScrollMode ) hbqt_par_QAbstractItemView( 1 )->horizontalScrollMode() );
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ICONSIZE )
{
   hb_retptr( new QSize( hbqt_par_QAbstractItemView( 1 )->iconSize() ) );
}

/*
 * virtual QModelIndex indexAt ( const QPoint & point ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXAT )
{
   hb_retptr( new QModelIndex( hbqt_par_QAbstractItemView( 1 )->indexAt( *hbqt_par_QPoint( 2 ) ) ) );
}

/*
 * QWidget * indexWidget ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_INDEXWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QAbstractItemView( 1 )->indexWidget( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QAbstractItemView( 1 )->itemDelegate() );
}

/*
 * QAbstractItemDelegate * itemDelegate ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATE_1 )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QAbstractItemView( 1 )->itemDelegate( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * QAbstractItemDelegate * itemDelegateForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORCOLUMN )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QAbstractItemView( 1 )->itemDelegateForColumn( hb_parni( 2 ) ) );
}

/*
 * QAbstractItemDelegate * itemDelegateForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ITEMDELEGATEFORROW )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QAbstractItemView( 1 )->itemDelegateForRow( hb_parni( 2 ) ) );
}

/*
 * virtual void keyboardSearch ( const QString & search )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_KEYBOARDSEARCH )
{
   hbqt_par_QAbstractItemView( 1 )->keyboardSearch( hbqt_par_QString( 2 ) );
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_MODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QAbstractItemView( 1 )->model() );
}

/*
 * void openPersistentEditor ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_OPENPERSISTENTEDITOR )
{
   hbqt_par_QAbstractItemView( 1 )->openPersistentEditor( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * QModelIndex rootIndex () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_ROOTINDEX )
{
   hb_retptr( new QModelIndex( hbqt_par_QAbstractItemView( 1 )->rootIndex() ) );
}

/*
 * virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible ) = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTO )
{
   hbqt_par_QAbstractItemView( 1 )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QAbstractItemView::EnsureVisible ) );
}

/*
 * QAbstractItemView::SelectionBehavior selectionBehavior () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONBEHAVIOR )
{
   hb_retni( ( QAbstractItemView::SelectionBehavior ) hbqt_par_QAbstractItemView( 1 )->selectionBehavior() );
}

/*
 * QAbstractItemView::SelectionMode selectionMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODE )
{
   hb_retni( ( QAbstractItemView::SelectionMode ) hbqt_par_QAbstractItemView( 1 )->selectionMode() );
}

/*
 * QItemSelectionModel * selectionModel () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTIONMODEL )
{
   hb_retptr( ( QItemSelectionModel* ) hbqt_par_QAbstractItemView( 1 )->selectionModel() );
}

/*
 * void setAlternatingRowColors ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETALTERNATINGROWCOLORS )
{
   hbqt_par_QAbstractItemView( 1 )->setAlternatingRowColors( hb_parl( 2 ) );
}

/*
 * void setAutoScroll ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLL )
{
   hbqt_par_QAbstractItemView( 1 )->setAutoScroll( hb_parl( 2 ) );
}

/*
 * void setAutoScrollMargin ( int margin )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETAUTOSCROLLMARGIN )
{
   hbqt_par_QAbstractItemView( 1 )->setAutoScrollMargin( hb_parni( 2 ) );
}

/*
 * void setDragDropMode ( DragDropMode behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setDragDropMode( ( QAbstractItemView::DragDropMode ) hb_parni( 2 ) );
}

/*
 * void setDragDropOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGDROPOVERWRITEMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setDragDropOverwriteMode( hb_parl( 2 ) );
}

/*
 * void setDragEnabled ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDRAGENABLED )
{
   hbqt_par_QAbstractItemView( 1 )->setDragEnabled( hb_parl( 2 ) );
}

/*
 * void setDropIndicatorShown ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETDROPINDICATORSHOWN )
{
   hbqt_par_QAbstractItemView( 1 )->setDropIndicatorShown( hb_parl( 2 ) );
}

/*
 * void setEditTriggers ( EditTriggers triggers )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETEDITTRIGGERS )
{
   hbqt_par_QAbstractItemView( 1 )->setEditTriggers( ( QAbstractItemView::EditTriggers ) hb_parni( 2 ) );
}

/*
 * void setHorizontalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETHORIZONTALSCROLLMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setHorizontalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETICONSIZE )
{
   hbqt_par_QAbstractItemView( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setIndexWidget ( const QModelIndex & index, QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETINDEXWIDGET )
{
   hbqt_par_QAbstractItemView( 1 )->setIndexWidget( *hbqt_par_QModelIndex( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATE )
{
   hbqt_par_QAbstractItemView( 1 )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
}

/*
 * void setItemDelegateForColumn ( int column, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORCOLUMN )
{
   hbqt_par_QAbstractItemView( 1 )->setItemDelegateForColumn( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
}

/*
 * void setItemDelegateForRow ( int row, QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETITEMDELEGATEFORROW )
{
   hbqt_par_QAbstractItemView( 1 )->setItemDelegateForRow( hb_parni( 2 ), hbqt_par_QAbstractItemDelegate( 3 ) );
}

/*
 * virtual void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETMODEL )
{
   hbqt_par_QAbstractItemView( 1 )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
}

/*
 * void setSelectionBehavior ( QAbstractItemView::SelectionBehavior behavior )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONBEHAVIOR )
{
   hbqt_par_QAbstractItemView( 1 )->setSelectionBehavior( ( QAbstractItemView::SelectionBehavior ) hb_parni( 2 ) );
}

/*
 * void setSelectionMode ( QAbstractItemView::SelectionMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setSelectionMode( ( QAbstractItemView::SelectionMode ) hb_parni( 2 ) );
}

/*
 * virtual void setSelectionModel ( QItemSelectionModel * selectionModel )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETSELECTIONMODEL )
{
   hbqt_par_QAbstractItemView( 1 )->setSelectionModel( hbqt_par_QItemSelectionModel( 2 ) );
}

/*
 * void setTabKeyNavigation ( bool enable )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTABKEYNAVIGATION )
{
   hbqt_par_QAbstractItemView( 1 )->setTabKeyNavigation( hb_parl( 2 ) );
}

/*
 * void setTextElideMode ( Qt::TextElideMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETTEXTELIDEMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setTextElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
}

/*
 * void setVerticalScrollMode ( ScrollMode mode )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETVERTICALSCROLLMODE )
{
   hbqt_par_QAbstractItemView( 1 )->setVerticalScrollMode( ( QAbstractItemView::ScrollMode ) hb_parni( 2 ) );
}

/*
 * bool showDropIndicator () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SHOWDROPINDICATOR )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->showDropIndicator() );
}

/*
 * virtual int sizeHintForColumn ( int column ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORCOLUMN )
{
   hb_retni( hbqt_par_QAbstractItemView( 1 )->sizeHintForColumn( hb_parni( 2 ) ) );
}

/*
 * QSize sizeHintForIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORINDEX )
{
   hb_retptr( new QSize( hbqt_par_QAbstractItemView( 1 )->sizeHintForIndex( *hbqt_par_QModelIndex( 2 ) ) ) );
}

/*
 * virtual int sizeHintForRow ( int row ) const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SIZEHINTFORROW )
{
   hb_retni( hbqt_par_QAbstractItemView( 1 )->sizeHintForRow( hb_parni( 2 ) ) );
}

/*
 * bool tabKeyNavigation () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TABKEYNAVIGATION )
{
   hb_retl( hbqt_par_QAbstractItemView( 1 )->tabKeyNavigation() );
}

/*
 * Qt::TextElideMode textElideMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_TEXTELIDEMODE )
{
   hb_retni( ( Qt::TextElideMode ) hbqt_par_QAbstractItemView( 1 )->textElideMode() );
}

/*
 * ScrollMode verticalScrollMode () const
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VERTICALSCROLLMODE )
{
   hb_retni( ( QAbstractItemView::ScrollMode ) hbqt_par_QAbstractItemView( 1 )->verticalScrollMode() );
}

/*
 * virtual QRect visualRect ( const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_VISUALRECT )
{
   hb_retptr( new QRect( hbqt_par_QAbstractItemView( 1 )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ) );
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_CLEARSELECTION )
{
   hbqt_par_QAbstractItemView( 1 )->clearSelection();
}

/*
 * void edit ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_EDIT )
{
   hbqt_par_QAbstractItemView( 1 )->edit( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * virtual void reset ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_RESET )
{
   hbqt_par_QAbstractItemView( 1 )->reset();
}

/*
 * void scrollToBottom ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOBOTTOM )
{
   hbqt_par_QAbstractItemView( 1 )->scrollToBottom();
}

/*
 * void scrollToTop ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SCROLLTOTOP )
{
   hbqt_par_QAbstractItemView( 1 )->scrollToTop();
}

/*
 * virtual void selectAll ()
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SELECTALL )
{
   hbqt_par_QAbstractItemView( 1 )->selectAll();
}

/*
 * void setCurrentIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETCURRENTINDEX )
{
   hbqt_par_QAbstractItemView( 1 )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * virtual void setRootIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMVIEW_SETROOTINDEX )
{
   hbqt_par_QAbstractItemView( 1 )->setRootIndex( *hbqt_par_QModelIndex( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

