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
 *  Constructed[ 49/55 [ 89.09% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  }
 *  }
 *  }
 *  }
 *  }
 */

#include <QtCore/QPointer>

#include <QtGui/QTableWidget>


/*
 * QTableWidget ( QWidget * parent = 0 )
 * QTableWidget ( int rows, int columns, QWidget * parent = 0 )
 * ~QTableWidget ()
 */

typedef struct
{
   QPointer< QTableWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTableWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QTableWidget )
{
   QTableWidget  * ph = NULL ;
   HBQT_GC_T_QTableWidget * p = ( HBQT_GC_T_QTableWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTableWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTableWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTableWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTableWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTableWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTableWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QTableWidget * p = ( HBQT_GC_T_QTableWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTableWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTableWidget >( ( QTableWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableWidget;
   p->type = HBQT_TYPE_QTableWidget;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTableWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTableWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTABLEWIDGET )
{
   QTableWidget * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      pObj =  new QTableWidget( hb_parni( 1 ), hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) ;
   else
      pObj =  new QTableWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTableWidget( ( void * ) pObj, true ) );
}

/*
 * QWidget * cellWidget ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_CELLWIDGET )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->cellWidget( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CELLWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->cellWidget( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void closePersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_CLOSEPERSISTENTEDITOR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->closePersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CLOSEPERSISTENTEDITOR FP=( p )->closePersistentEditor( hbqt_par_QTableWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * int column ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->column( hbqt_par_QTableWidgetItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_COLUMN FP=hb_retni( ( p )->column( hbqt_par_QTableWidgetItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMNCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->columnCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_COLUMNCOUNT FP=hb_retni( ( p )->columnCount() ); p is NULL" ) );
   }
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->currentColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CURRENTCOLUMN FP=hb_retni( ( p )->currentColumn() ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->currentItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CURRENTITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->currentItem(), false ) ); p is NULL" ) );
   }
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->currentRow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CURRENTROW FP=hb_retni( ( p )->currentRow() ); p is NULL" ) );
   }
}

/*
 * void editItem ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_EDITITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->editItem( hbqt_par_QTableWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_EDITITEM FP=( p )->editItem( hbqt_par_QTableWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<QTableWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags ) const
 */
HB_FUNC( QT_QTABLEWIDGET_FINDITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->findItems( QTableWidget::tr( hb_parc( 2 ) ), ( Qt::MatchFlags ) hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_FINDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->findItems( QTableWidget::tr( hb_parc( 2 ) ), ( Qt::MatchFlags ) hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_HORIZONTALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_HORIZONTALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * item ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * itemAt ( const QPoint & point ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * itemAt ( int ax, int ay ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT_1 )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ITEMAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * const QTableWidgetItem * itemPrototype () const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMPROTOTYPE )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( new QTableWidgetItem( *( ( p )->itemPrototype() ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ITEMPROTOTYPE FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( new QTableWidgetItem( *( ( p )->itemPrototype() ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void openPersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_OPENPERSISTENTEDITOR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->openPersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_OPENPERSISTENTEDITOR FP=( p )->openPersistentEditor( hbqt_par_QTableWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeCellWidget ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECELLWIDGET )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->removeCellWidget( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_REMOVECELLWIDGET FP=( p )->removeCellWidget( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int row ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->row( hbqt_par_QTableWidgetItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ROW FP=hb_retni( ( p )->row( hbqt_par_QTableWidgetItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_ROWCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->rowCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_ROWCOUNT FP=hb_retni( ( p )->rowCount() ); p is NULL" ) );
   }
}

/*
 * QList<QTableWidgetItem *> selectedItems ()
 */
HB_FUNC( QT_QTABLEWIDGET_SELECTEDITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->selectedItems() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SELECTEDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->selectedItems() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QTableWidgetSelectionRange> selectedRanges () const
 */
HB_FUNC( QT_QTABLEWIDGET_SELECTEDRANGES )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetSelectionRange>( ( p )->selectedRanges() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SELECTEDRANGES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetSelectionRange>( ( p )->selectedRanges() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCellWidget ( int row, int column, QWidget * widget )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCELLWIDGET )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 4 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETCELLWIDGET()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETCELLWIDGET() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setCellWidget( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCOLUMNCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setColumnCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETCOLUMNCOUNT FP=( p )->setColumnCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentCell ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETCURRENTCELL FP=( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentCell ( int row, int column, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL_1 )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETCURRENTCELL_1 FP=( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentItem ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETCURRENTITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETCURRENTITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * void setCurrentItem ( QTableWidgetItem * item, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM_1 )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETCURRENTITEM_1()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETCURRENTITEM_1() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setHorizontalHeaderItem ( int column, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERITEM )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 3 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETHORIZONTALHEADERITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETHORIZONTALHEADERITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
   }
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERLABELS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETHORIZONTALHEADERLABELS FP=( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItem ( int row, int column, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEM )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 4 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QTableWidgetItem( 4 ) );
   }
}

/*
 * void setItemPrototype ( const QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEMPROTOTYPE )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setItemPrototype( hbqt_par_QTableWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETITEMPROTOTYPE FP=( p )->setItemPrototype( hbqt_par_QTableWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRangeSelected ( const QTableWidgetSelectionRange & range, bool select )
 */
HB_FUNC( QT_QTABLEWIDGET_SETRANGESELECTED )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setRangeSelected( *hbqt_par_QTableWidgetSelectionRange( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETRANGESELECTED FP=( p )->setRangeSelected( *hbqt_par_QTableWidgetSelectionRange( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QTABLEWIDGET_SETROWCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setRowCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETROWCOUNT FP=( p )->setRowCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalHeaderItem ( int row, QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERITEM )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 3 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTABLEWIDGET_SETVERTICALHEADERITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTABLEWIDGET_SETVERTICALHEADERITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QTableWidget( 1 )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
   }
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERLABELS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SETVERTICALHEADERLABELS FP=( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void sortItems ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QTABLEWIDGET_SORTITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->sortItems( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SORTITEMS FP=( p )->sortItems( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEHORIZONTALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_TAKEHORIZONTALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * takeItem ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_TAKEITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEVERTICALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_TAKEVERTICALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTableWidgetItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VERTICALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_VERTICALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * int visualColumn ( int logicalColumn ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->visualColumn( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_VISUALCOLUMN FP=hb_retni( ( p )->visualColumn( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QRect visualItemRect ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALITEMRECT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTableWidgetItem( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_VISUALITEMRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTableWidgetItem( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int visualRow ( int logicalRow ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      hb_retni( ( p )->visualRow( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_VISUALROW FP=hb_retni( ( p )->visualRow( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEAR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void clearContents ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEARCONTENTS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->clearContents();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_CLEARCONTENTS FP=( p )->clearContents(); p is NULL" ) );
   }
}

/*
 * void insertColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->insertColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_INSERTCOLUMN FP=( p )->insertColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_INSERTROW FP=( p )->insertRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->removeColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_REMOVECOLUMN FP=( p )->removeColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVEROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->removeRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_REMOVEROW FP=( p )->removeRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void scrollToItem ( const QTableWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTABLEWIDGET_SCROLLTOITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
      ( p )->scrollToItem( hbqt_par_QTableWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTableWidget::EnsureVisible ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGET_SCROLLTOITEM FP=( p )->scrollToItem( hbqt_par_QTableWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTableWidget::EnsureVisible ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
