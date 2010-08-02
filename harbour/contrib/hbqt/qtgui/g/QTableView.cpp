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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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
   QPointer< QTableView > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTableView;

QT_G_FUNC( hbqt_gcRelease_QTableView )
{
   QTableView  * ph = NULL ;
   QGC_POINTER_QTableView * p = ( QGC_POINTER_QTableView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTableView   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTableView   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTableView          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTableView    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTableView    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTableView( void * pObj, bool bNew )
{
   QGC_POINTER_QTableView * p = ( QGC_POINTER_QTableView * ) hb_gcAllocate( sizeof( QGC_POINTER_QTableView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTableView >( ( QTableView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableView;
   p->type = HBQT_TYPE_QTableView;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTableView  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTableView", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTABLEVIEW )
{
   QTableView * pObj = NULL;

   pObj =  new QTableView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTableView( ( void * ) pObj, true ) );
}

/*
 * void clearSpans ()
 */
HB_FUNC( QT_QTABLEVIEW_CLEARSPANS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->clearSpans();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_CLEARSPANS FP=( p )->clearSpans(); p is NULL" ) );
   }
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnAt( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_COLUMNAT FP=hb_retni( ( p )->columnAt( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int columnSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_COLUMNSPAN FP=hb_retni( ( p )->columnSpan( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_COLUMNVIEWPORTPOSITION FP=hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_COLUMNWIDTH FP=hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * Qt::PenStyle gridStyle () const
 */
HB_FUNC( QT_QTABLEVIEW_GRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( Qt::PenStyle ) ( p )->gridStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_GRIDSTYLE FP=hb_retni( ( Qt::PenStyle ) ( p )->gridStyle() ); p is NULL" ) );
   }
}

/*
 * QHeaderView * horizontalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_HORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->horizontalHeader(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_HORIZONTALHEADER FP=hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->horizontalHeader(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QModelIndex indexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTABLEVIEW_INDEXAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_INDEXAT FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ISCOLUMNHIDDEN FP=hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isCornerButtonEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isCornerButtonEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ISCORNERBUTTONENABLED FP=hb_retl( ( p )->isCornerButtonEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ISROWHIDDEN FP=hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isSortingEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ISSORTINGENABLED FP=hb_retl( ( p )->isSortingEnabled() ); p is NULL" ) );
   }
}

/*
 * int rowAt ( int y ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowAt( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ROWAT FP=hb_retni( ( p )->rowAt( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int rowHeight ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowHeight( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ROWHEIGHT FP=hb_retni( ( p )->rowHeight( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int rowSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ROWSPAN FP=hb_retni( ( p )->rowSpan( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int rowViewportPosition ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowViewportPosition( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_ROWVIEWPORTPOSITION FP=hb_retni( ( p )->rowViewportPosition( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETCOLUMNHIDDEN FP=( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETCOLUMNWIDTH FP=( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setCornerButtonEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setCornerButtonEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETCORNERBUTTONENABLED FP=( p )->setCornerButtonEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGridStyle ( Qt::PenStyle style )
 */
HB_FUNC( QT_QTABLEVIEW_SETGRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setGridStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETGRIDSTYLE FP=( p )->setGridStyle( ( Qt::PenStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETHORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setHorizontalHeader( hbqt_par_QHeaderView( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETHORIZONTALHEADER FP=( p )->setHorizontalHeader( hbqt_par_QHeaderView( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRowHeight ( int row, int height )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setRowHeight( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETROWHEIGHT FP=( p )->setRowHeight( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETROWHIDDEN FP=( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setSortingEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETSORTINGENABLED FP=( p )->setSortingEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSpan ( int row, int column, int rowSpanCount, int columnSpanCount )
 */
HB_FUNC( QT_QTABLEVIEW_SETSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setSpan( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETSPAN FP=( p )->setSpan( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETVERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setVerticalHeader( hbqt_par_QHeaderView( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETVERTICALHEADER FP=( p )->setVerticalHeader( hbqt_par_QHeaderView( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTABLEVIEW_SETWORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setWordWrap( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETWORDWRAP FP=( p )->setWordWrap( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool showGrid () const
 */
HB_FUNC( QT_QTABLEVIEW_SHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SHOWGRID FP=hb_retl( ( p )->showGrid() ); p is NULL" ) );
   }
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTABLEVIEW_SORTBYCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SORTBYCOLUMN FP=( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QHeaderView * verticalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_VERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->verticalHeader(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_VERTICALHEADER FP=hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->verticalHeader(), false ) ); p is NULL" ) );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTABLEVIEW_WORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->wordWrap() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_WORDWRAP FP=hb_retl( ( p )->wordWrap() ); p is NULL" ) );
   }
}

/*
 * void hideColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_HIDECOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->hideColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_HIDECOLUMN FP=( p )->hideColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void hideRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_HIDEROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->hideRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_HIDEROW FP=( p )->hideRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resizeColumnToContents ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeColumnToContents( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_RESIZECOLUMNTOCONTENTS FP=( p )->resizeColumnToContents( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resizeColumnsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeColumnsToContents();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_RESIZECOLUMNSTOCONTENTS FP=( p )->resizeColumnsToContents(); p is NULL" ) );
   }
}

/*
 * void resizeRowToContents ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeRowToContents( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_RESIZEROWTOCONTENTS FP=( p )->resizeRowToContents( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resizeRowsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeRowsToContents();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_RESIZEROWSTOCONTENTS FP=( p )->resizeRowsToContents(); p is NULL" ) );
   }
}

/*
 * void selectColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->selectColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SELECTCOLUMN FP=( p )->selectColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void selectRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->selectRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SELECTROW FP=( p )->selectRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setShowGrid ( bool show )
 */
HB_FUNC( QT_QTABLEVIEW_SETSHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SETSHOWGRID FP=( p )->setShowGrid( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->showColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SHOWCOLUMN FP=( p )->showColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->showRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEVIEW_SHOWROW FP=( p )->showRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
