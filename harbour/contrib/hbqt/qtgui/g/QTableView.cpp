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
 *  Constructed[ 42/42 [ 100.00% ] ]
 *
 */

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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTableView;

HBQT_GC_FUNC( hbqt_gcRelease_QTableView )
{
   QTableView  * ph = NULL ;
   HBQT_GC_T_QTableView * p = ( HBQT_GC_T_QTableView * ) Cargo;

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
   HBQT_GC_T_QTableView * p = ( HBQT_GC_T_QTableView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTableView ), hbqt_gcFuncs() );

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

   pObj = new QTableView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTableView( ( void * ) pObj, true ) );
}

/*
 * void clearSpans ()
 */
HB_FUNC( QT_QTABLEVIEW_CLEARSPANS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->clearSpans();
   }
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnAt( hb_parni( 2 ) ) );
   }
}

/*
 * int columnSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) );
   }
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_COLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) );
   }
}

/*
 * Qt::PenStyle gridStyle () const
 */
HB_FUNC( QT_QTABLEVIEW_GRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( Qt::PenStyle ) ( p )->gridStyle() );
   }
}

/*
 * QHeaderView * horizontalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_HORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->horizontalHeader(), false ) );
   }
}

/*
 * virtual QModelIndex indexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTABLEVIEW_INDEXAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) );
   }
}

/*
 * bool isCornerButtonEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->isCornerButtonEnabled() );
   }
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ISROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) );
   }
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTABLEVIEW_ISSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->isSortingEnabled() );
   }
}

/*
 * int rowAt ( int y ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->rowAt( hb_parni( 2 ) ) );
   }
}

/*
 * int rowHeight ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->rowHeight( hb_parni( 2 ) ) );
   }
}

/*
 * int rowSpan ( int row, int column ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->rowSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int rowViewportPosition ( int row ) const
 */
HB_FUNC( QT_QTABLEVIEW_ROWVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retni( ( p )->rowViewportPosition( hb_parni( 2 ) ) );
   }
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setCornerButtonEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setCornerButtonEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setGridStyle ( Qt::PenStyle style )
 */
HB_FUNC( QT_QTABLEVIEW_SETGRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setGridStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
   }
}

/*
 * void setHorizontalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETHORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setHorizontalHeader( hbqt_par_QHeaderView( 2 ) );
   }
}

/*
 * void setRowHeight ( int row, int height )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setRowHeight( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QTABLEVIEW_SETROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTABLEVIEW_SETSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setSortingEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setSpan ( int row, int column, int rowSpanCount, int columnSpanCount )
 */
HB_FUNC( QT_QTABLEVIEW_SETSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setSpan( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   }
}

/*
 * void setVerticalHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTABLEVIEW_SETVERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setVerticalHeader( hbqt_par_QHeaderView( 2 ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTABLEVIEW_SETWORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setWordWrap( hb_parl( 2 ) );
   }
}

/*
 * bool showGrid () const
 */
HB_FUNC( QT_QTABLEVIEW_SHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->showGrid() );
   }
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTABLEVIEW_SORTBYCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   }
}

/*
 * QHeaderView * verticalHeader () const
 */
HB_FUNC( QT_QTABLEVIEW_VERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->verticalHeader(), false ) );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTABLEVIEW_WORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      hb_retl( ( p )->wordWrap() );
   }
}

/*
 * void hideColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_HIDECOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->hideColumn( hb_parni( 2 ) );
   }
}

/*
 * void hideRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_HIDEROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->hideRow( hb_parni( 2 ) );
   }
}

/*
 * void resizeColumnToContents ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->resizeColumnToContents( hb_parni( 2 ) );
   }
}

/*
 * void resizeColumnsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->resizeColumnsToContents();
   }
}

/*
 * void resizeRowToContents ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->resizeRowToContents( hb_parni( 2 ) );
   }
}

/*
 * void resizeRowsToContents ()
 */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->resizeRowsToContents();
   }
}

/*
 * void selectColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->selectColumn( hb_parni( 2 ) );
   }
}

/*
 * void selectRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SELECTROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->selectRow( hb_parni( 2 ) );
   }
}

/*
 * void setShowGrid ( bool show )
 */
HB_FUNC( QT_QTABLEVIEW_SETSHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->setShowGrid( hb_parl( 2 ) );
   }
}

/*
 * void showColumn ( int column )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->showColumn( hb_parni( 2 ) );
   }
}

/*
 * void showRow ( int row )
 */
HB_FUNC( QT_QTABLEVIEW_SHOWROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
   {
      ( p )->showRow( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
