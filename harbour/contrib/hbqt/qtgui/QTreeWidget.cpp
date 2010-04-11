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
 *  Constructed[ 37/41 [ 90.24% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void addTopLevelItems ( const QList<QTreeWidgetItem *> & items )
 *  QList<QTreeWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags, int column = 0 ) const
 *  void insertTopLevelItems ( int index, const QList<QTreeWidgetItem *> & items )
 *  QList<QTreeWidgetItem *> selectedItems () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTreeWidget>


/*
 * QTreeWidget ( QWidget * parent = 0 )
 * ~QTreeWidget ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QTreeWidget > pq;
} QGC_POINTER_QTreeWidget;

QT_G_FUNC( hbqt_gcRelease_QTreeWidget )
{
   QGC_POINTER_QTreeWidget * p = ( QGC_POINTER_QTreeWidget * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTreeWidget   /.\\   pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QTreeWidget * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTreeWidget   \\./   pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTreeWidget          pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTreeWidget    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTreeWidget    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTreeWidget( void * pObj, bool bNew )
{
   QGC_POINTER_QTreeWidget * p = ( QGC_POINTER_QTreeWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QTreeWidget ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeWidget;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTreeWidget >( ( QTreeWidget * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTreeWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTreeWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTREEWIDGET )
{
   void * pObj = NULL;

   pObj = ( QTreeWidget* ) new QTreeWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTreeWidget( pObj, true ) );
}

/*
 * void addTopLevelItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_ADDTOPLEVELITEM )
{
   hbqt_par_QTreeWidget( 1 )->addTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * void closePersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_CLOSEPERSISTENTEDITOR )
{
   hbqt_par_QTreeWidget( 1 )->closePersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTREEWIDGET_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QTreeWidget( 1 )->columnCount() );
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTCOLUMN )
{
   hb_retni( hbqt_par_QTreeWidget( 1 )->currentColumn() );
}

/*
 * QTreeWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTITEM )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->currentItem(), false ) );
}

/*
 * void editItem ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_EDITITEM )
{
   hbqt_par_QTreeWidget( 1 )->editItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
}

/*
 * QTreeWidgetItem * headerItem () const
 */
HB_FUNC( QT_QTREEWIDGET_HEADERITEM )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->headerItem(), false ) );
}

/*
 * int indexOfTopLevelItem ( QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_INDEXOFTOPLEVELITEM )
{
   hb_retni( hbqt_par_QTreeWidget( 1 )->indexOfTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) ) );
}

/*
 * void insertTopLevelItem ( int index, QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_INSERTTOPLEVELITEM )
{
   hbqt_par_QTreeWidget( 1 )->insertTopLevelItem( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) );
}

/*
 * QTreeWidgetItem * invisibleRootItem () const
 */
HB_FUNC( QT_QTREEWIDGET_INVISIBLEROOTITEM )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->invisibleRootItem(), false ) );
}

/*
 * bool isFirstItemColumnSpanned ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ISFIRSTITEMCOLUMNSPANNED )
{
   hb_retl( hbqt_par_QTreeWidget( 1 )->isFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ) ) );
}

/*
 * QTreeWidgetItem * itemAbove ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMABOVE )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->itemAbove( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
}

/*
 * QTreeWidgetItem * itemAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/*
 * QTreeWidgetItem * itemAt ( int x, int y ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/*
 * QTreeWidgetItem * itemBelow ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMBELOW )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->itemBelow( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
}

/*
 * QWidget * itemWidget ( QTreeWidgetItem * item, int column ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMWIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QTreeWidget( 1 )->itemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ), false ) );
}

/*
 * void openPersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_OPENPERSISTENTEDITOR )
{
   hbqt_par_QTreeWidget( 1 )->openPersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
}

/*
 * void removeItemWidget ( QTreeWidgetItem * item, int column )
 */
HB_FUNC( QT_QTREEWIDGET_REMOVEITEMWIDGET )
{
   hbqt_par_QTreeWidget( 1 )->removeItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTREEWIDGET_SETCOLUMNCOUNT )
{
   hbqt_par_QTreeWidget( 1 )->setColumnCount( hb_parni( 2 ) );
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM )
{
   hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_1 )
{
   hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_2 )
{
   hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
}

/*
 * void setFirstItemColumnSpanned ( const QTreeWidgetItem * item, bool span )
 */
HB_FUNC( QT_QTREEWIDGET_SETFIRSTITEMCOLUMNSPANNED )
{
   hbqt_par_QTreeWidget( 1 )->setFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ), hb_parl( 3 ) );
}

/*
 * void setHeaderItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERITEM )
{
   hbqt_par_QTreeWidget( 1 )->setHeaderItem( hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * void setHeaderLabel ( const QString & label )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABEL )
{
   hbqt_par_QTreeWidget( 1 )->setHeaderLabel( QTreeWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABELS )
{
   hbqt_par_QTreeWidget( 1 )->setHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setItemWidget ( QTreeWidgetItem * item, int column, QWidget * widget )
 */
HB_FUNC( QT_QTREEWIDGET_SETITEMWIDGET )
{
   hbqt_par_QTreeWidget( 1 )->setItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/*
 * int sortColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_SORTCOLUMN )
{
   hb_retni( hbqt_par_QTreeWidget( 1 )->sortColumn() );
}

/*
 * void sortItems ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEWIDGET_SORTITEMS )
{
   hbqt_par_QTreeWidget( 1 )->sortItems( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/*
 * QTreeWidgetItem * takeTopLevelItem ( int index )
 */
HB_FUNC( QT_QTREEWIDGET_TAKETOPLEVELITEM )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->takeTopLevelItem( hb_parni( 2 ) ), false ) );
}

/*
 * QTreeWidgetItem * topLevelItem ( int index ) const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEM )
{
   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( hbqt_par_QTreeWidget( 1 )->topLevelItem( hb_parni( 2 ) ), false ) );
}

/*
 * int topLevelItemCount () const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEMCOUNT )
{
   hb_retni( hbqt_par_QTreeWidget( 1 )->topLevelItemCount() );
}

/*
 * QRect visualItemRect ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_VISUALITEMRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QTreeWidget( 1 )->visualItemRect( hbqt_par_QTreeWidgetItem( 2 ) ) ), true ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTREEWIDGET_CLEAR )
{
   hbqt_par_QTreeWidget( 1 )->clear();
}

/*
 * void collapseItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_COLLAPSEITEM )
{
   hbqt_par_QTreeWidget( 1 )->collapseItem( hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * void expandItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_EXPANDITEM )
{
   hbqt_par_QTreeWidget( 1 )->expandItem( hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * void scrollToItem ( const QTreeWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTREEWIDGET_SCROLLTOITEM )
{
   hbqt_par_QTreeWidget( 1 )->scrollToItem( hbqt_par_QTreeWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTreeWidget::EnsureVisible ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
