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
 *  #      These enums are defined in QAbstractItemView class
 *  #
 *  enum DragDropMode { NoDragDrop, DragOnly, DropOnly, DragDrop, InternalMove }
 *  enum EditTrigger { NoEditTriggers, CurrentChanged, DoubleClicked, SelectedClicked, ..., AllEditTriggers }
 *  enum ScrollHint { EnsureVisible, PositionAtTop, PositionAtBottom, PositionAtCenter }
 *  enum ScrollMode { ScrollPerItem, ScrollPerPixel }
 *  enum SelectionBehavior { SelectItems, SelectRows, SelectColumns }
 *  enum SelectionMode { SingleSelection, ContiguousSelection, ExtendedSelection, MultiSelection, NoSelection }
 *  flags EditTriggers
 */

/*
 *  Constructed[ 49/49 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTreeView>


/*
 * QTreeView ( QWidget * parent = 0 )
 * ~QTreeView ()
 */

typedef struct
{
   QPointer< QTreeView > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTreeView;

HBQT_GC_FUNC( hbqt_gcRelease_QTreeView )
{
   QTreeView  * ph = NULL ;
   HBQT_GC_T_QTreeView * p = ( HBQT_GC_T_QTreeView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTreeView   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTreeView   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTreeView          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTreeView    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTreeView    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTreeView( void * pObj, bool bNew )
{
   HBQT_GC_T_QTreeView * p = ( HBQT_GC_T_QTreeView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTreeView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTreeView >( ( QTreeView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeView;
   p->type = HBQT_TYPE_QTreeView;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTreeView  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTreeView", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTREEVIEW )
{
   QTreeView * pObj = NULL;

   pObj =  new QTreeView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTreeView( ( void * ) pObj, true ) );
}

/*
 * bool allColumnsShowFocus () const
 */
HB_FUNC( QT_QTREEVIEW_ALLCOLUMNSSHOWFOCUS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->allColumnsShowFocus() );
   }
}

/*
 * int autoExpandDelay () const
 */
HB_FUNC( QT_QTREEVIEW_AUTOEXPANDDELAY )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retni( ( p )->autoExpandDelay() );
   }
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNAT )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnAt( hb_parni( 2 ) ) );
   }
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNVIEWPORTPOSITION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) );
   }
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNWIDTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) );
   }
}

/*
 * bool expandsOnDoubleClick () const
 */
HB_FUNC( QT_QTREEVIEW_EXPANDSONDOUBLECLICK )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->expandsOnDoubleClick() );
   }
}

/*
 * QHeaderView * header () const
 */
HB_FUNC( QT_QTREEVIEW_HEADER )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->header(), false ) );
   }
}

/*
 * int indentation () const
 */
HB_FUNC( QT_QTREEVIEW_INDENTATION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retni( ( p )->indentation() );
   }
}

/*
 * QModelIndex indexAbove ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_INDEXABOVE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAbove( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * QModelIndex indexBelow ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_INDEXBELOW )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexBelow( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * bool isAnimated () const
 */
HB_FUNC( QT_QTREEVIEW_ISANIMATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isAnimated() );
   }
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_ISCOLUMNHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) );
   }
}

/*
 * bool isExpanded ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_ISEXPANDED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isExpanded( *hbqt_par_QModelIndex( 2 ) ) );
   }
}

/*
 * bool isFirstColumnSpanned ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QTREEVIEW_ISFIRSTCOLUMNSPANNED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * bool isHeaderHidden () const
 */
HB_FUNC( QT_QTREEVIEW_ISHEADERHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isHeaderHidden() );
   }
}

/*
 * bool isRowHidden ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QTREEVIEW_ISROWHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTREEVIEW_ISSORTINGENABLED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->isSortingEnabled() );
   }
}

/*
 * bool itemsExpandable () const
 */
HB_FUNC( QT_QTREEVIEW_ITEMSEXPANDABLE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->itemsExpandable() );
   }
}

/*
 * bool rootIsDecorated () const
 */
HB_FUNC( QT_QTREEVIEW_ROOTISDECORATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->rootIsDecorated() );
   }
}

/*
 * virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTREEVIEW_SCROLLTO )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QTreeView::ScrollHint ) hb_parni( 3 ) : ( QTreeView::ScrollHint ) QTreeView::EnsureVisible ) );
   }
}

/*
 * void setAllColumnsShowFocus ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETALLCOLUMNSSHOWFOCUS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setAllColumnsShowFocus( hb_parl( 2 ) );
   }
}

/*
 * void setAnimated ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETANIMATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setAnimated( hb_parl( 2 ) );
   }
}

/*
 * void setAutoExpandDelay ( int delay )
 */
HB_FUNC( QT_QTREEVIEW_SETAUTOEXPANDDELAY )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setAutoExpandDelay( hb_parni( 2 ) );
   }
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNWIDTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setExpanded ( const QModelIndex & index, bool expanded )
 */
HB_FUNC( QT_QTREEVIEW_SETEXPANDED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setExpanded( *hbqt_par_QModelIndex( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setExpandsOnDoubleClick ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETEXPANDSONDOUBLECLICK )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setExpandsOnDoubleClick( hb_parl( 2 ) );
   }
}

/*
 * void setFirstColumnSpanned ( int row, const QModelIndex & parent, bool span )
 */
HB_FUNC( QT_QTREEVIEW_SETFIRSTCOLUMNSPANNED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
   }
}

/*
 * void setHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADER )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setHeader( hbqt_par_QHeaderView( 2 ) );
   }
}

/*
 * void setHeaderHidden ( bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADERHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setHeaderHidden( hb_parl( 2 ) );
   }
}

/*
 * void setIndentation ( int i )
 */
HB_FUNC( QT_QTREEVIEW_SETINDENTATION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setIndentation( hb_parni( 2 ) );
   }
}

/*
 * void setItemsExpandable ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETITEMSEXPANDABLE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setItemsExpandable( hb_parl( 2 ) );
   }
}

/*
 * void setRootIsDecorated ( bool show )
 */
HB_FUNC( QT_QTREEVIEW_SETROOTISDECORATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setRootIsDecorated( hb_parl( 2 ) );
   }
}

/*
 * void setRowHidden ( int row, const QModelIndex & parent, bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETROWHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
   }
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETSORTINGENABLED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setSortingEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setUniformRowHeights ( bool uniform )
 */
HB_FUNC( QT_QTREEVIEW_SETUNIFORMROWHEIGHTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setUniformRowHeights( hb_parl( 2 ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTREEVIEW_SETWORDWRAP )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->setWordWrap( hb_parl( 2 ) );
   }
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEVIEW_SORTBYCOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   }
}

/*
 * bool uniformRowHeights () const
 */
HB_FUNC( QT_QTREEVIEW_UNIFORMROWHEIGHTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->uniformRowHeights() );
   }
}

/*
 * virtual QRect visualRect ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_VISUALRECT )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTREEVIEW_WORDWRAP )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      hb_retl( ( p )->wordWrap() );
   }
}

/*
 * void collapse ( const QModelIndex & index )
 */
HB_FUNC( QT_QTREEVIEW_COLLAPSE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->collapse( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * void collapseAll ()
 */
HB_FUNC( QT_QTREEVIEW_COLLAPSEALL )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->collapseAll();
   }
}

/*
 * void expand ( const QModelIndex & index )
 */
HB_FUNC( QT_QTREEVIEW_EXPAND )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->expand( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * void expandAll ()
 */
HB_FUNC( QT_QTREEVIEW_EXPANDALL )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->expandAll();
   }
}

/*
 * void expandToDepth ( int depth )
 */
HB_FUNC( QT_QTREEVIEW_EXPANDTODEPTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->expandToDepth( hb_parni( 2 ) );
   }
}

/*
 * void hideColumn ( int column )
 */
HB_FUNC( QT_QTREEVIEW_HIDECOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->hideColumn( hb_parni( 2 ) );
   }
}

/*
 * void resizeColumnToContents ( int column )
 */
HB_FUNC( QT_QTREEVIEW_RESIZECOLUMNTOCONTENTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->resizeColumnToContents( hb_parni( 2 ) );
   }
}

/*
 * void showColumn ( int column )
 */
HB_FUNC( QT_QTREEVIEW_SHOWCOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
   {
      ( p )->showColumn( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
