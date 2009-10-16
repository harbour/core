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

/*
 *  Constructed[ 30/32 [ 93.75% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QListWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags ) const
 *  QList<QListWidgetItem *> selectedItems () const
 */

#include <QtCore/QPointer>

#include <QtGui/QListWidget>


/*
 * QListWidget ( QWidget * parent = 0 )
 * ~QListWidget ()
 */

HB_FUNC( QT_QLISTWIDGET )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   QPointer< QListWidget > pObj = NULL;

   pObj = new QListWidget( hbqt_par_QWidget( 1 ) ) ;

   p->ph = pObj;
   p->type = 1001;
   hb_retptrGC( p );
}
/*
 * void addItem ( const QString & label )
 */
HB_FUNC( QT_QLISTWIDGET_ADDITEM )
{
   hbqt_par_QListWidget( 1 )->addItem( hbqt_par_QString( 2 ) );
}

/*
 * void addItem ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_ADDITEM_1 )
{
   hbqt_par_QListWidget( 1 )->addItem( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * void addItems ( const QStringList & labels )
 */
HB_FUNC( QT_QLISTWIDGET_ADDITEMS )
{
   hbqt_par_QListWidget( 1 )->addItems( *hbqt_par_QStringList( 2 ) );
}

/*
 * void closePersistentEditor ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_CLOSEPERSISTENTEDITOR )
{
   hbqt_par_QListWidget( 1 )->closePersistentEditor( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QLISTWIDGET_COUNT )
{
   hb_retni( hbqt_par_QListWidget( 1 )->count() );
}

/*
 * QListWidgetItem * currentItem () const
 */
HB_FUNC( QT_QLISTWIDGET_CURRENTITEM )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidget( 1 )->currentItem() );
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QLISTWIDGET_CURRENTROW )
{
   hb_retni( hbqt_par_QListWidget( 1 )->currentRow() );
}

/*
 * void editItem ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_EDITITEM )
{
   hbqt_par_QListWidget( 1 )->editItem( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * void insertItem ( int row, QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_INSERTITEM )
{
   hbqt_par_QListWidget( 1 )->insertItem( hb_parni( 2 ), hbqt_par_QListWidgetItem( 3 ) );
}

/*
 * void insertItem ( int row, const QString & label )
 */
HB_FUNC( QT_QLISTWIDGET_INSERTITEM_1 )
{
   hbqt_par_QListWidget( 1 )->insertItem( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void insertItems ( int row, const QStringList & labels )
 */
HB_FUNC( QT_QLISTWIDGET_INSERTITEMS )
{
   hbqt_par_QListWidget( 1 )->insertItems( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QLISTWIDGET_ISSORTINGENABLED )
{
   hb_retl( hbqt_par_QListWidget( 1 )->isSortingEnabled() );
}

/*
 * QListWidgetItem * item ( int row ) const
 */
HB_FUNC( QT_QLISTWIDGET_ITEM )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidget( 1 )->item( hb_parni( 2 ) ) );
}

/*
 * QListWidgetItem * itemAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QLISTWIDGET_ITEMAT )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidget( 1 )->itemAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QListWidgetItem * itemAt ( int x, int y ) const
 */
HB_FUNC( QT_QLISTWIDGET_ITEMAT_1 )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidget( 1 )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QWidget * itemWidget ( QListWidgetItem * item ) const
 */
HB_FUNC( QT_QLISTWIDGET_ITEMWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QListWidget( 1 )->itemWidget( hbqt_par_QListWidgetItem( 2 ) ) );
}

/*
 * void openPersistentEditor ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_OPENPERSISTENTEDITOR )
{
   hbqt_par_QListWidget( 1 )->openPersistentEditor( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * void removeItemWidget ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_REMOVEITEMWIDGET )
{
   hbqt_par_QListWidget( 1 )->removeItemWidget( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * int row ( const QListWidgetItem * item ) const
 */
HB_FUNC( QT_QLISTWIDGET_ROW )
{
   hb_retni( hbqt_par_QListWidget( 1 )->row( hbqt_par_QListWidgetItem( 2 ) ) );
}

/*
 * void setCurrentItem ( QListWidgetItem * item )
 */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTITEM )
{
   hbqt_par_QListWidget( 1 )->setCurrentItem( hbqt_par_QListWidgetItem( 2 ) );
}

/*
 * void setCurrentItem ( QListWidgetItem * item, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTITEM_1 )
{
   hbqt_par_QListWidget( 1 )->setCurrentItem( hbqt_par_QListWidgetItem( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/*
 * void setCurrentRow ( int row )
 */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTROW )
{
   hbqt_par_QListWidget( 1 )->setCurrentRow( hb_parni( 2 ) );
}

/*
 * void setCurrentRow ( int row, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTROW_1 )
{
   hbqt_par_QListWidget( 1 )->setCurrentRow( hb_parni( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/*
 * void setItemWidget ( QListWidgetItem * item, QWidget * widget )
 */
HB_FUNC( QT_QLISTWIDGET_SETITEMWIDGET )
{
   hbqt_par_QListWidget( 1 )->setItemWidget( hbqt_par_QListWidgetItem( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QLISTWIDGET_SETSORTINGENABLED )
{
   hbqt_par_QListWidget( 1 )->setSortingEnabled( hb_parl( 2 ) );
}

/*
 * void sortItems ( Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QLISTWIDGET_SORTITEMS )
{
   hbqt_par_QListWidget( 1 )->sortItems( ( HB_ISNUM( 2 ) ? ( Qt::SortOrder ) hb_parni( 2 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/*
 * QListWidgetItem * takeItem ( int row )
 */
HB_FUNC( QT_QLISTWIDGET_TAKEITEM )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidget( 1 )->takeItem( hb_parni( 2 ) ) );
}

/*
 * QRect visualItemRect ( const QListWidgetItem * item ) const
 */
HB_FUNC( QT_QLISTWIDGET_VISUALITEMRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QListWidget( 1 )->visualItemRect( hbqt_par_QListWidgetItem( 2 ) ) ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLISTWIDGET_CLEAR )
{
   hbqt_par_QListWidget( 1 )->clear();
}

/*
 * void scrollToItem ( const QListWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QLISTWIDGET_SCROLLTOITEM )
{
   hbqt_par_QListWidget( 1 )->scrollToItem( hbqt_par_QListWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QListWidget::EnsureVisible ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
