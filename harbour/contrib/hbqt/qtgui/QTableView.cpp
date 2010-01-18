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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTableView>

/*
 * QTableView ( QWidget * parent = 0 )
 * ~QTableView ()
 */


typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QTableView > pq;
} QGC_POINTER_QTableView;

QT_G_FUNC( hbqt_gcRelease_QTableView )
{
   QGC_POINTER_QTableView * p = ( QGC_POINTER_QTableView * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QTableView * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTableView                 ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QTableView                 ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTableView                  Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTableView                  Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTableView( void * pObj, bool bNew )
{
   QGC_POINTER_QTableView * p = ( QGC_POINTER_QTableView * ) hb_gcAllocate( sizeof( QGC_POINTER_QTableView ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableView;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTableView >( ( QTableView * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTableView                 ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTABLEVIEW )
{
   void * pObj = NULL;

   pObj = ( QTableView* ) new QTableView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTableView( pObj, true ) );
}

/*
 * void clearSpans ()
 */
HB_FUNC( QT_QTABLEVIEW_CLEARSPANS )
{
   hbqt_par_QTableView( 1 )->clearSpans();
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNAT )
{
   hb_retni( hbqt_par_QTableView( 1 )->columnAt( hb_parni( 2 ) ) );
}

/*
 * int columnSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNSPAN )
{
   hb_retni( hbqt_par_QTableView( 1 )->columnSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNVIEWPORTPOSITION )
{
   hb_retni( hbqt_par_QTableView( 1 )->columnViewportPosition( hb_parni( 2 ) ) );
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNWIDTH )
{
   hb_retni( hbqt_par_QTableView( 1 )->columnWidth( hb_parni( 2 ) ) );
}

/*
 * Qt::PenStyle gridStyle () const
 */
HB_FUNC( QT_QTABLEVIEW_GRIDSTYLE )
{
   hb_retni( ( Qt::PenStyle ) hbqt_par_QTableView( 1 )->gridStyle() );
}

/*
 * QHeaderView * horizontalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_HORIZONTALHEADER )
{
   hb_retptrGC( hbqt_gcAllocate_QHeaderView( hbqt_par_QTableView( 1 )->horizontalHeader(), false ) );
}

/*
 * virtual QModelIndex indexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTABLEVIEW_INDEXAT )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QTableView( 1 )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISCOLUMNHIDDEN )
{
   hb_retl( hbqt_par_QTableView( 1 )->isColumnHidden( hb_parni( 2 ) ) );
}

/*
 * bool isCornerButtonEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISCORNERBUTTONENABLED )
{
   hb_retl( hbqt_par_QTableView( 1 )->isCornerButtonEnabled() );
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISROWHIDDEN )
{
   hb_retl( hbqt_par_QTableView( 1 )->isRowHidden( hb_parni( 2 ) ) );
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISSORTINGENABLED )
{
   hb_retl( hbqt_par_QTableView( 1 )->isSortingEnabled() );
}

/*
 * int rowAt ( int y ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWAT )
{
   hb_retni( hbqt_par_QTableView( 1 )->rowAt( hb_parni( 2 ) ) );
}

/*
 * int rowHeight ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWHEIGHT )
{
   hb_retni( hbqt_par_QTableView( 1 )->rowHeight( hb_parni( 2 ) ) );
}

/*
 * int rowSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWSPAN )
{
   hb_retni( hbqt_par_QTableView( 1 )->rowSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * int rowViewportPosition ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWVIEWPORTPOSITION )
{
   hb_retni( hbqt_par_QTableView( 1 )->rowViewportPosition( hb_parni( 2 ) ) );
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNHIDDEN )
{
   hbqt_par_QTableView( 1 )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNWIDTH )
{
   hbqt_par_QTableView( 1 )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setCornerButtonEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETCORNERBUTTONENABLED )
{
   hbqt_par_QTableView( 1 )->setCornerButtonEnabled( hb_parl( 2 ) );
}

/*
 * void setGridStyle ( Qt::PenStyle style )
 */
HB_FUNC( QT_QTABLEVIEW_SETGRIDSTYLE )
{
   hbqt_par_QTableView( 1 )->setGridStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/*
 * void setHorizontalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETHORIZONTALHEADER )
{
   hbqt_par_QTableView( 1 )->setHorizontalHeader( hbqt_par_QHeaderView( 2 ) );
}

/*
 * void setRowHeight ( int row, int height )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHEIGHT )
{
   hbqt_par_QTableView( 1 )->setRowHeight( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHIDDEN )
{
   hbqt_par_QTableView( 1 )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETSORTINGENABLED )
{
   hbqt_par_QTableView( 1 )->setSortingEnabled( hb_parl( 2 ) );
}

/*
 * void setSpan ( int row, int column, int rowSpanCount, int columnSpanCount )
 */
HB_FUNC( QT_QTABLEVIEW_SETSPAN )
{
   hbqt_par_QTableView( 1 )->setSpan( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setVerticalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETVERTICALHEADER )
{
   hbqt_par_QTableView( 1 )->setVerticalHeader( hbqt_par_QHeaderView( 2 ) );
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTABLEVIEW_SETWORDWRAP )
{
   hbqt_par_QTableView( 1 )->setWordWrap( hb_parl( 2 ) );
}

/*
 * bool showGrid () const
 */
HB_FUNC( QT_QTABLEVIEW_SHOWGRID )
{
   hb_retl( hbqt_par_QTableView( 1 )->showGrid() );
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTABLEVIEW_SORTBYCOLUMN )
{
   hbqt_par_QTableView( 1 )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/*
 * QHeaderView * verticalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_VERTICALHEADER )
{
   hb_retptrGC( hbqt_gcAllocate_QHeaderView( hbqt_par_QTableView( 1 )->verticalHeader(), false ) );
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTABLEVIEW_WORDWRAP )
{
   hb_retl( hbqt_par_QTableView( 1 )->wordWrap() );
}

/*
 * void hideColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_HIDECOLUMN )
{
   hbqt_par_QTableView( 1 )->hideColumn( hb_parni( 2 ) );
}

/*
 * void hideRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_HIDEROW )
{
   hbqt_par_QTableView( 1 )->hideRow( hb_parni( 2 ) );
}

/*
 * void resizeColumnToContents ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNTOCONTENTS )
{
   hbqt_par_QTableView( 1 )->resizeColumnToContents( hb_parni( 2 ) );
}

/*
 * void resizeColumnsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNSTOCONTENTS )
{
   hbqt_par_QTableView( 1 )->resizeColumnsToContents();
}

/*
 * void resizeRowToContents ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWTOCONTENTS )
{
   hbqt_par_QTableView( 1 )->resizeRowToContents( hb_parni( 2 ) );
}

/*
 * void resizeRowsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWSTOCONTENTS )
{
   hbqt_par_QTableView( 1 )->resizeRowsToContents();
}

/*
 * void selectColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTCOLUMN )
{
   hbqt_par_QTableView( 1 )->selectColumn( hb_parni( 2 ) );
}

/*
 * void selectRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTROW )
{
   hbqt_par_QTableView( 1 )->selectRow( hb_parni( 2 ) );
}

/*
 * void setShowGrid ( bool show )
 */
HB_FUNC( QT_QTABLEVIEW_SETSHOWGRID )
{
   hbqt_par_QTableView( 1 )->setShowGrid( hb_parl( 2 ) );
}

/*
 * void showColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWCOLUMN )
{
   hbqt_par_QTableView( 1 )->showColumn( hb_parni( 2 ) );
}

/*
 * void showRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWROW )
{
   hbqt_par_QTableView( 1 )->showRow( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
