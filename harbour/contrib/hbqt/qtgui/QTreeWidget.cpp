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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 39/45 [ 86.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void addTopLevelItems ( const QList<QTreeWidgetItem *> & items )
 *  void insertTopLevelItems ( int index, const QList<QTreeWidgetItem *> & items )
 *  }
 *  }
 *  }
 *  }
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTreeWidget;

QT_G_FUNC( hbqt_gcRelease_QTreeWidget )
{
   QTreeWidget  * ph = NULL ;
   QGC_POINTER_QTreeWidget * p = ( QGC_POINTER_QTreeWidget * ) Cargo;

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
   QGC_POINTER_QTreeWidget * p = ( QGC_POINTER_QTreeWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QTreeWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTreeWidget >( ( QTreeWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeWidget;
   p->type = QT_TYPE_QTreeWidget;

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
 * void addTopLevelItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_ADDTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->addTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ADDTOPLEVELITEM FP=( p )->addTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void closePersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_CLOSEPERSISTENTEDITOR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->closePersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_CLOSEPERSISTENTEDITOR FP=( p )->closePersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTREEWIDGET_COLUMNCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retni( ( p )->columnCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_COLUMNCOUNT FP=hb_retni( ( p )->columnCount() ); p is NULL" ) );
   }
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTCOLUMN )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retni( ( p )->currentColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_CURRENTCOLUMN FP=hb_retni( ( p )->currentColumn() ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTREEWIDGET_CURRENTITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->currentItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_CURRENTITEM FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->currentItem(), false ) ); p is NULL" ) );
   }
}

/*
 * void editItem ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_EDITITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->editItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_EDITITEM FP=( p )->editItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QList<QTreeWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags, int column = 0 ) const
 */
HB_FUNC( QT_QTREEWIDGET_FINDITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->findItems( QTreeWidget::tr( hb_parc( 2 ) ), ( Qt::MatchFlags ) hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_FINDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->findItems( QTreeWidget::tr( hb_parc( 2 ) ), ( Qt::MatchFlags ) hb_parni( 3 ), hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * headerItem () const
 */
HB_FUNC( QT_QTREEWIDGET_HEADERITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->headerItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_HEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->headerItem(), false ) ); p is NULL" ) );
   }
}

/*
 * int indexOfTopLevelItem ( QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_INDEXOFTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retni( ( p )->indexOfTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_INDEXOFTOPLEVELITEM FP=hb_retni( ( p )->indexOfTopLevelItem( hbqt_par_QTreeWidgetItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void insertTopLevelItem ( int index, QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_INSERTTOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->insertTopLevelItem( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_INSERTTOPLEVELITEM FP=( p )->insertTopLevelItem( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * invisibleRootItem () const
 */
HB_FUNC( QT_QTREEWIDGET_INVISIBLEROOTITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->invisibleRootItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_INVISIBLEROOTITEM FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->invisibleRootItem(), false ) ); p is NULL" ) );
   }
}

/*
 * bool isFirstItemColumnSpanned ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ISFIRSTITEMCOLUMNSPANNED )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retl( ( p )->isFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ISFIRSTITEMCOLUMNSPANNED FP=hb_retl( ( p )->isFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * itemAbove ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMABOVE )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAbove( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ITEMABOVE FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAbove( hbqt_par_QTreeWidgetItem( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * itemAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * itemAt ( int x, int y ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMAT_1 )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ITEMAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * itemBelow ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMBELOW )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemBelow( hbqt_par_QTreeWidgetItem( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ITEMBELOW FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->itemBelow( hbqt_par_QTreeWidgetItem( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * itemWidget ( QTreeWidgetItem * item, int column ) const
 */
HB_FUNC( QT_QTREEWIDGET_ITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->itemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_ITEMWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->itemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void openPersistentEditor ( QTreeWidgetItem * item, int column = 0 )
 */
HB_FUNC( QT_QTREEWIDGET_OPENPERSISTENTEDITOR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->openPersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_OPENPERSISTENTEDITOR FP=( p )->openPersistentEditor( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeItemWidget ( QTreeWidgetItem * item, int column )
 */
HB_FUNC( QT_QTREEWIDGET_REMOVEITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->removeItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_REMOVEITEMWIDGET FP=( p )->removeItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QList<QTreeWidgetItem *> selectedItems () const
 */
HB_FUNC( QT_QTREEWIDGET_SELECTEDITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->selectedItems() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SELECTEDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->selectedItems() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTREEWIDGET_SETCOLUMNCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->setColumnCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SETCOLUMNCOUNT FP=( p )->setColumnCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTREEWIDGET_SETCURRENTITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTREEWIDGET_SETCURRENTITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_1 )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTREEWIDGET_SETCURRENTITEM_1()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTREEWIDGET_SETCURRENTITEM_1() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setCurrentItem ( QTreeWidgetItem * item, int column, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTREEWIDGET_SETCURRENTITEM_2 )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTREEWIDGET_SETCURRENTITEM_2()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTREEWIDGET_SETCURRENTITEM_2() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTreeWidget( 1 )->setCurrentItem( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
   }
}

/*
 * void setFirstItemColumnSpanned ( const QTreeWidgetItem * item, bool span )
 */
HB_FUNC( QT_QTREEWIDGET_SETFIRSTITEMCOLUMNSPANNED )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->setFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SETFIRSTITEMCOLUMNSPANNED FP=( p )->setFirstItemColumnSpanned( hbqt_par_QTreeWidgetItem( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHeaderItem ( QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERITEM )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * q = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTREEWIDGET_SETHEADERITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTREEWIDGET_SETHEADERITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTreeWidget( 1 )->setHeaderItem( hbqt_par_QTreeWidgetItem( 2 ) );
   }
}

/*
 * void setHeaderLabel ( const QString & label )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABEL )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->setHeaderLabel( QTreeWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SETHEADERLABEL FP=( p )->setHeaderLabel( QTreeWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTREEWIDGET_SETHEADERLABELS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->setHeaderLabels( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SETHEADERLABELS FP=( p )->setHeaderLabels( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemWidget ( QTreeWidgetItem * item, int column, QWidget * widget )
 */
HB_FUNC( QT_QTREEWIDGET_SETITEMWIDGET )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->setItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SETITEMWIDGET FP=( p )->setItemWidget( hbqt_par_QTreeWidgetItem( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * int sortColumn () const
 */
HB_FUNC( QT_QTREEWIDGET_SORTCOLUMN )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retni( ( p )->sortColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SORTCOLUMN FP=hb_retni( ( p )->sortColumn() ); p is NULL" ) );
   }
}

/*
 * void sortItems ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEWIDGET_SORTITEMS )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->sortItems( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SORTITEMS FP=( p )->sortItems( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * takeTopLevelItem ( int index )
 */
HB_FUNC( QT_QTREEWIDGET_TAKETOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->takeTopLevelItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_TAKETOPLEVELITEM FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->takeTopLevelItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * topLevelItem ( int index ) const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->topLevelItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_TOPLEVELITEM FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->topLevelItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * int topLevelItemCount () const
 */
HB_FUNC( QT_QTREEWIDGET_TOPLEVELITEMCOUNT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retni( ( p )->topLevelItemCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_TOPLEVELITEMCOUNT FP=hb_retni( ( p )->topLevelItemCount() ); p is NULL" ) );
   }
}

/*
 * QRect visualItemRect ( const QTreeWidgetItem * item ) const
 */
HB_FUNC( QT_QTREEWIDGET_VISUALITEMRECT )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTreeWidgetItem( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_VISUALITEMRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTreeWidgetItem( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTREEWIDGET_CLEAR )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void collapseItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_COLLAPSEITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->collapseItem( hbqt_par_QTreeWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_COLLAPSEITEM FP=( p )->collapseItem( hbqt_par_QTreeWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void expandItem ( const QTreeWidgetItem * item )
 */
HB_FUNC( QT_QTREEWIDGET_EXPANDITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->expandItem( hbqt_par_QTreeWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_EXPANDITEM FP=( p )->expandItem( hbqt_par_QTreeWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void scrollToItem ( const QTreeWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTREEWIDGET_SCROLLTOITEM )
{
   QTreeWidget * p = hbqt_par_QTreeWidget( 1 );
   if( p )
      ( p )->scrollToItem( hbqt_par_QTreeWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTreeWidget::EnsureVisible ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGET_SCROLLTOITEM FP=( p )->scrollToItem( hbqt_par_QTreeWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTreeWidget::EnsureVisible ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
