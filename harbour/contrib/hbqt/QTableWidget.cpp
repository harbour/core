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
 *  Constructed[ 46/49 [ 93.88% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QTableWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags ) const
 *  QList<QTableWidgetItem *> selectedItems ()
 *  QList<QTableWidgetSelectionRange> selectedRanges () const
 */


#include <QtGui/QTableWidget>


/*
 * QTableWidget ( QWidget * parent = 0 )
 * QTableWidget ( int rows, int columns, QWidget * parent = 0 )
 * ~QTableWidget ()
 */
HB_FUNC( QT_QTABLEWIDGET )
{
   if( hb_pcount() >= 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      hb_retptr( ( QTableWidget* ) new QTableWidget( hb_parni( 1 ), hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) );
   else
      hb_retptr( ( QTableWidget* ) new QTableWidget( hbqt_par_QWidget( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QTABLEWIDGET_DESTROY )
{
   delete hbqt_par_QTableWidget( 1 );
}

/*
 * QWidget * cellWidget ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_CELLWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QTableWidget( 1 )->cellWidget( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * void closePersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_CLOSEPERSISTENTEDITOR )
{
   hbqt_par_QTableWidget( 1 )->closePersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
}

/*
 * int column ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMN )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->column( hbqt_par_QTableWidgetItem( 2 ) ) );
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->columnCount() );
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTCOLUMN )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->currentColumn() );
}

/*
 * QTableWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->currentItem() );
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTROW )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->currentRow() );
}

/*
 * void editItem ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_EDITITEM )
{
   hbqt_par_QTableWidget( 1 )->editItem( hbqt_par_QTableWidgetItem( 2 ) );
}

/*
 * QTableWidgetItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_HORIZONTALHEADERITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->horizontalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QTableWidgetItem * item ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->item( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QTableWidgetItem * itemAt ( const QPoint & point ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->itemAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QTableWidgetItem * itemAt ( int ax, int ay ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT_1 )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * const QTableWidgetItem * itemPrototype () const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMPROTOTYPE )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->itemPrototype() );
}

/*
 * void openPersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_OPENPERSISTENTEDITOR )
{
   hbqt_par_QTableWidget( 1 )->openPersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
}

/*
 * void removeCellWidget ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECELLWIDGET )
{
   hbqt_par_QTableWidget( 1 )->removeCellWidget( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int row ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ROW )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->row( hbqt_par_QTableWidgetItem( 2 ) ) );
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_ROWCOUNT )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->rowCount() );
}

/*
 * void setCellWidget ( int row, int column, QWidget * widget )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCELLWIDGET )
{
   hbqt_par_QTableWidget( 1 )->setCellWidget( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCOLUMNCOUNT )
{
   hbqt_par_QTableWidget( 1 )->setColumnCount( hb_parni( 2 ) );
}

/*
 * void setCurrentCell ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL )
{
   hbqt_par_QTableWidget( 1 )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setCurrentCell ( int row, int column, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL_1 )
{
   hbqt_par_QTableWidget( 1 )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
}

/*
 * void setCurrentItem ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM )
{
   hbqt_par_QTableWidget( 1 )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ) );
}

/*
 * void setCurrentItem ( QTableWidgetItem * item, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM_1 )
{
   hbqt_par_QTableWidget( 1 )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/*
 * void setHorizontalHeaderItem ( int column, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERITEM )
{
   hbqt_par_QTableWidget( 1 )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERLABELS )
{
   hbqt_par_QTableWidget( 1 )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setItem ( int row, int column, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEM )
{
   hbqt_par_QTableWidget( 1 )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QTableWidgetItem( 4 ) );
}

/*
 * void setItemPrototype ( const QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEMPROTOTYPE )
{
   hbqt_par_QTableWidget( 1 )->setItemPrototype( hbqt_par_QTableWidgetItem( 2 ) );
}

/*
 * void setRangeSelected ( const QTableWidgetSelectionRange & range, bool select )
 */
HB_FUNC( QT_QTABLEWIDGET_SETRANGESELECTED )
{
   hbqt_par_QTableWidget( 1 )->setRangeSelected( *hbqt_par_QTableWidgetSelectionRange( 2 ), hb_parl( 3 ) );
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QTABLEWIDGET_SETROWCOUNT )
{
   hbqt_par_QTableWidget( 1 )->setRowCount( hb_parni( 2 ) );
}

/*
 * void setVerticalHeaderItem ( int row, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERITEM )
{
   hbqt_par_QTableWidget( 1 )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERLABELS )
{
   hbqt_par_QTableWidget( 1 )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/*
 * void sortItems ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QTABLEWIDGET_SORTITEMS )
{
   hbqt_par_QTableWidget( 1 )->sortItems( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/*
 * QTableWidgetItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEHORIZONTALHEADERITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->takeHorizontalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QTableWidgetItem * takeItem ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QTableWidgetItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEVERTICALHEADERITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->takeVerticalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QTableWidgetItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VERTICALHEADERITEM )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidget( 1 )->verticalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * int visualColumn ( int logicalColumn ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALCOLUMN )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->visualColumn( hb_parni( 2 ) ) );
}

/*
 * QRect visualItemRect ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALITEMRECT )
{
   hb_retptr( new QRect( hbqt_par_QTableWidget( 1 )->visualItemRect( hbqt_par_QTableWidgetItem( 2 ) ) ) );
}

/*
 * int visualRow ( int logicalRow ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALROW )
{
   hb_retni( hbqt_par_QTableWidget( 1 )->visualRow( hb_parni( 2 ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEAR )
{
   hbqt_par_QTableWidget( 1 )->clear();
}

/*
 * void clearContents ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEARCONTENTS )
{
   hbqt_par_QTableWidget( 1 )->clearContents();
}

/*
 * void insertColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTCOLUMN )
{
   hbqt_par_QTableWidget( 1 )->insertColumn( hb_parni( 2 ) );
}

/*
 * void insertRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTROW )
{
   hbqt_par_QTableWidget( 1 )->insertRow( hb_parni( 2 ) );
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECOLUMN )
{
   hbqt_par_QTableWidget( 1 )->removeColumn( hb_parni( 2 ) );
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVEROW )
{
   hbqt_par_QTableWidget( 1 )->removeRow( hb_parni( 2 ) );
}

/*
 * void scrollToItem ( const QTableWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTABLEWIDGET_SCROLLTOITEM )
{
   hbqt_par_QTableWidget( 1 )->scrollToItem( hbqt_par_QTableWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTableWidget::EnsureVisible ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
