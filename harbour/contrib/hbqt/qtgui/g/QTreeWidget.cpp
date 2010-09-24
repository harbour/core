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
 *  Constructed[ 39/41 [ 95.12% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void addTopLevelItems ( const QList<QTreeWidgetItem *> & items )
 *  void insertTopLevelItems ( int index, const QList<QTreeWidgetItem *> & items )
 */

#include <QtCore/QPointer>

#include <QtGui/QTreeWidget>


/*
 * QTreeWidget ( QWidget * parent = 0 )
 * ~QTreeWidget ()
 */

typedef struct
{
   QPointer< QTreeWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTreeWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QTreeWidget )
{
   QTreeWidget  * ph = NULL ;
   HBQT_GC_T_QTreeWidget * p = ( HBQT_GC_T_QTreeWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTreeWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTreeWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTreeWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTreeWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTreeWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTreeWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QTreeWidget * p = ( HBQT_GC_T_QTreeWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTreeWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTreeWidget >( ( QTreeWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeWidget;
   p->type = HBQT_TYPE_QTreeWidget;

   if( bNew )
   {
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
   QTreeWidget * pObj = NULL;

   pObj =  new QTreeWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTreeWidget( ( void * ) pObj, true ) );
}

/*
 * void addTopLevelItem ( QTreeWidgetItem * item )   [*D=1*]
 */
HB_FUNC( QT_QTREEWIDGET_ADDTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->addTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void closePersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_CLOSEPERSISTENTEDITOR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->closePersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTREEWIDGET_COLUMNCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount() );
   }
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTCOLUMN )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->currentColumn() );
   }
}

/*
 * QTreeWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->currentItem(), false ) );
   }
}

/*
 * void editItem ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_EDITITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->editItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * QList<QTreeWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags, int column = 0 ) const
 */
HB_FUNC( QT_QTREEWIDGET_FINDITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->findItems( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::MatchFlags ) hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTreeWidgetItem * headerItem () const
 */
HB_FUNC( QT_QTREEWIDGET_HEADERITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->headerItem(), false ) );
   }
}

/*
 * int indexOfTopLevelItem ( QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_INDEXOFTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOfTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) ) );
   }
}

/*
 * void insertTopLevelItem ( int index, QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_INSERTTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->insertTopLevelItem( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) );
   }
}

/*
 * QTreeWidgetItem * invisibleRootItem () const
 */
HB_FUNC( QT_QTREEWIDGET_INVISIBLEROOTITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->invisibleRootItem(), false ) );
   }
}

/*
 * bool isFirstItemColumnSpanned ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ISFIRSTITEMCOLUMNSPANNED )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retl( ( p )->isFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ) ) );
   }
}

/*
 * QTreeWidgetItem * itemAbove ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMABOVE )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAbove( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
   }
}

/*
 * QTreeWidgetItem * itemAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
   }
}

/*
 * QTreeWidgetItem * itemAt ( int x, int y ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT_1 )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QTreeWidgetItem * itemBelow ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMBELOW )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemBelow( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
   }
}

/*
 * QWidget * itemWidget ( QTreeWidgetItem * item, int column ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->itemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * void openPersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_OPENPERSISTENTEDITOR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->openPersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void removeItemWidget ( QTreeWidgetItem * item, int column )
 */
HB_FUNC( QT_QTREEWIDGET_REMOVEITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->removeItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * QList<QTreeWidgetItem *> selectedItems () const
 */
HB_FUNC( QT_QTREEWIDGET_SELECTEDITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->selectedItems() ), true ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTREEWIDGET_SETCOLUMNCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->setColumnCount( hb_parni( 2 ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item )   [*D=1*]
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column )   [*D=1*]
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_1 )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column, QItemSelectionModel::SelectionFlags command )   [*D=1*]
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_2 )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
   }
}

/*
 * void setFirstItemColumnSpanned ( const QTreeWidgetItem * item, bool span )
 */
HB_FUNC( QT_QTREEWIDGET_SETFIRSTITEMCOLUMNSPANNED )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->setFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setHeaderItem ( QTreeWidgetItem * item )   [*D=1*]
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setHeaderItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void setHeaderLabel ( const QString & label )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABEL )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHeaderLabel( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABELS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->setHeaderLabels( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setItemWidget ( QTreeWidgetItem * item, int column, QWidget * widget )
 */
HB_FUNC( QT_QTREEWIDGET_SETITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->setItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   }
}

/*
 * int sortColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_SORTCOLUMN )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->sortColumn() );
   }
}

/*
 * void sortItems ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEWIDGET_SORTITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->sortItems( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   }
}

/*
 * QTreeWidgetItem * takeTopLevelItem ( int index )
 */
HB_FUNC( QT_QTREEWIDGET_TAKETOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->takeTopLevelItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTreeWidgetItem * topLevelItem ( int index ) const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->topLevelItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * int topLevelItemCount () const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEMCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->topLevelItemCount() );
   }
}

/*
 * QRect visualItemRect ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_VISUALITEMRECT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTreeWidgetItem( 2 ) ) ), true ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTREEWIDGET_CLEAR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void collapseItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_COLLAPSEITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->collapseItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void expandItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_EXPANDITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->expandItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void scrollToItem ( const QTreeWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTREEWIDGET_SCROLLTOITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
   {
      ( p )->scrollToItem( hbqt_par_QTreeWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTreeWidget::EnsureVisible ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
