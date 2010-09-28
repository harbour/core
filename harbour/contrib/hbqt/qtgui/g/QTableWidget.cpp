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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 49/49 [ 100.00% ] ]
 *
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
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->cellWidget( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * void closePersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_CLOSEPERSISTENTEDITOR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->closePersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * int column ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->column( hbqt_par_QTableWidgetItem( 2 ) ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_COLUMNCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount() );
   }
}

/*
 * int currentColumn () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->currentColumn() );
   }
}

/*
 * QTableWidgetItem * currentItem () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->currentItem(), false ) );
   }
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QTABLEWIDGET_CURRENTROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->currentRow() );
   }
}

/*
 * void editItem ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_EDITITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->editItem( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * QList<QTableWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags ) const
 */
HB_FUNC( QT_QTABLEWIDGET_FINDITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->findItems( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::MatchFlags ) hb_parni( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTableWidgetItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_HORIZONTALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * item ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * itemAt ( const QPoint & point ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * itemAt ( int ax, int ay ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMAT_1 )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * const QTableWidgetItem * itemPrototype () const
 */
HB_FUNC( QT_QTABLEWIDGET_ITEMPROTOTYPE )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( new QTableWidgetItem( *( ( p )->itemPrototype() ) ), true ) );
   }
}

/*
 * void openPersistentEditor ( QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_OPENPERSISTENTEDITOR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->openPersistentEditor( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * void removeCellWidget ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECELLWIDGET )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->removeCellWidget( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int row ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_ROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->row( hbqt_par_QTableWidgetItem( 2 ) ) );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QTABLEWIDGET_ROWCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->rowCount() );
   }
}

/*
 * QList<QTableWidgetItem *> selectedItems ()
 */
HB_FUNC( QT_QTABLEWIDGET_SELECTEDITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetItem *>( ( p )->selectedItems() ), true ) );
   }
}

/*
 * QList<QTableWidgetSelectionRange> selectedRanges () const
 */
HB_FUNC( QT_QTABLEWIDGET_SELECTEDRANGES )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTableWidgetSelectionRange>( ( p )->selectedRanges() ), true ) );
   }
}

/*
 * void setCellWidget ( int row, int column, QWidget * widget )   [*D=3*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETCELLWIDGET )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 4 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCellWidget( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCOLUMNCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setColumnCount( hb_parni( 2 ) );
   }
}

/*
 * void setCurrentCell ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setCurrentCell ( int row, int column, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTCELL_1 )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setCurrentCell( hb_parni( 2 ), hb_parni( 3 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 4 ) );
   }
}

/*
 * void setCurrentItem ( QTableWidgetItem * item )   [*D=1*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * void setCurrentItem ( QTableWidgetItem * item, QItemSelectionModel::SelectionFlags command )   [*D=1*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETCURRENTITEM_1 )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setCurrentItem( hbqt_par_QTableWidgetItem( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
   }
}

/*
 * void setHorizontalHeaderItem ( int column, QTableWidgetItem * item )   [*D=2*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 3 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
   }
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETHORIZONTALHEADERLABELS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setItem ( int row, int column, QTableWidgetItem * item )   [*D=3*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 4 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QTableWidgetItem( 4 ) );
   }
}

/*
 * void setItemPrototype ( const QTableWidgetItem * item )
 */
HB_FUNC( QT_QTABLEWIDGET_SETITEMPROTOTYPE )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setItemPrototype( hbqt_par_QTableWidgetItem( 2 ) );
   }
}

/*
 * void setRangeSelected ( const QTableWidgetSelectionRange & range, bool select )
 */
HB_FUNC( QT_QTABLEWIDGET_SETRANGESELECTED )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setRangeSelected( *hbqt_par_QTableWidgetSelectionRange( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QTABLEWIDGET_SETROWCOUNT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setRowCount( hb_parni( 2 ) );
   }
}

/*
 * void setVerticalHeaderItem ( int row, QTableWidgetItem * item )   [*D=2*]
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 3 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QTableWidgetItem( 3 ) );
   }
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QTABLEWIDGET_SETVERTICALHEADERLABELS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void sortItems ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QTABLEWIDGET_SORTITEMS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->sortItems( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
   }
}

/*
 * QTableWidgetItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEHORIZONTALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * takeItem ( int row, int column )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_TAKEVERTICALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTableWidgetItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VERTICALHEADERITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * int visualColumn ( int logicalColumn ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->visualColumn( hb_parni( 2 ) ) );
   }
}

/*
 * QRect visualItemRect ( const QTableWidgetItem * item ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALITEMRECT )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QTableWidgetItem( 2 ) ) ), true ) );
   }
}

/*
 * int visualRow ( int logicalRow ) const
 */
HB_FUNC( QT_QTABLEWIDGET_VISUALROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      hb_retni( ( p )->visualRow( hb_parni( 2 ) ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEAR )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void clearContents ()
 */
HB_FUNC( QT_QTABLEWIDGET_CLEARCONTENTS )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->clearContents();
   }
}

/*
 * void insertColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTCOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->insertColumn( hb_parni( 2 ) );
   }
}

/*
 * void insertRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_INSERTROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->insertRow( hb_parni( 2 ) );
   }
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVECOLUMN )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->removeColumn( hb_parni( 2 ) );
   }
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QTABLEWIDGET_REMOVEROW )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->removeRow( hb_parni( 2 ) );
   }
}

/*
 * void scrollToItem ( const QTableWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTABLEWIDGET_SCROLLTOITEM )
{
   QTableWidget * p = hbqt_par_QTableWidget( 1 );
   if( p )
   {
      ( p )->scrollToItem( hbqt_par_QTableWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QTableWidget::EnsureVisible ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
