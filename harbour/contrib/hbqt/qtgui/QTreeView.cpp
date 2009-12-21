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
#include "../hbqt.h"

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

#include <QtCore/QPointer>

#include <QtGui/QTreeView>


/*
 * QTreeView ( QWidget * parent = 0 )
 * ~QTreeView ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QTreeView > pq;
} QGC_POINTER_QTreeView;

QT_G_FUNC( hbqt_gcRelease_QTreeView )
{
   QGC_POINTER_QTreeView * p = ( QGC_POINTER_QTreeView * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QTreeView                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QTreeView                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QTreeView * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QTreeView * ) p->ph )->~QTreeView();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QTreeView * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QTreeView                   Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QTreeView                   Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QTreeView                   Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QTreeView( void * pObj )
{
   QGC_POINTER_QTreeView * p = ( QGC_POINTER_QTreeView * ) hb_gcAllocate( sizeof( QGC_POINTER_QTreeView ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QTreeView;
   new( & p->pq ) QPointer< QTreeView >( ( QTreeView * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QTreeView                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QTREEVIEW )
{
   void * pObj = NULL;

   pObj = ( QTreeView* ) new QTreeView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTreeView( pObj ) );
}
/*
 * bool allColumnsShowFocus () const
 */
HB_FUNC( QT_QTREEVIEW_ALLCOLUMNSSHOWFOCUS )
{
   hb_retl( hbqt_par_QTreeView( 1 )->allColumnsShowFocus() );
}

/*
 * int autoExpandDelay () const
 */
HB_FUNC( QT_QTREEVIEW_AUTOEXPANDDELAY )
{
   hb_retni( hbqt_par_QTreeView( 1 )->autoExpandDelay() );
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNAT )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnAt( hb_parni( 2 ) ) );
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNVIEWPORTPOSITION )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnViewportPosition( hb_parni( 2 ) ) );
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNWIDTH )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnWidth( hb_parni( 2 ) ) );
}

/*
 * bool expandsOnDoubleClick () const
 */
HB_FUNC( QT_QTREEVIEW_EXPANDSONDOUBLECLICK )
{
   hb_retl( hbqt_par_QTreeView( 1 )->expandsOnDoubleClick() );
}

/*
 * QHeaderView * header () const
 */
HB_FUNC( QT_QTREEVIEW_HEADER )
{
   hb_retptr( ( QHeaderView* ) hbqt_par_QTreeView( 1 )->header() );
}

/*
 * int indentation () const
 */
HB_FUNC( QT_QTREEVIEW_INDENTATION )
{
   hb_retni( hbqt_par_QTreeView( 1 )->indentation() );
}

/*
 * QModelIndex indexAbove ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_INDEXABOVE )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QTreeView( 1 )->indexAbove( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * QModelIndex indexBelow ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_INDEXBELOW )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QTreeView( 1 )->indexBelow( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * bool isAnimated () const
 */
HB_FUNC( QT_QTREEVIEW_ISANIMATED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isAnimated() );
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_ISCOLUMNHIDDEN )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isColumnHidden( hb_parni( 2 ) ) );
}

/*
 * bool isExpanded ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_ISEXPANDED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isExpanded( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool isFirstColumnSpanned ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QTREEVIEW_ISFIRSTCOLUMNSPANNED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * bool isHeaderHidden () const
 */
HB_FUNC( QT_QTREEVIEW_ISHEADERHIDDEN )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isHeaderHidden() );
}

/*
 * bool isRowHidden ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QTREEVIEW_ISROWHIDDEN )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTREEVIEW_ISSORTINGENABLED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isSortingEnabled() );
}

/*
 * bool itemsExpandable () const
 */
HB_FUNC( QT_QTREEVIEW_ITEMSEXPANDABLE )
{
   hb_retl( hbqt_par_QTreeView( 1 )->itemsExpandable() );
}

/*
 * bool rootIsDecorated () const
 */
HB_FUNC( QT_QTREEVIEW_ROOTISDECORATED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->rootIsDecorated() );
}

/*
 * virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible )
 */
HB_FUNC( QT_QTREEVIEW_SCROLLTO )
{
   hbqt_par_QTreeView( 1 )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QTreeView::ScrollHint ) hb_parni( 3 ) : ( QTreeView::ScrollHint ) QTreeView::EnsureVisible ) );
}

/*
 * void setAllColumnsShowFocus ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETALLCOLUMNSSHOWFOCUS )
{
   hbqt_par_QTreeView( 1 )->setAllColumnsShowFocus( hb_parl( 2 ) );
}

/*
 * void setAnimated ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETANIMATED )
{
   hbqt_par_QTreeView( 1 )->setAnimated( hb_parl( 2 ) );
}

/*
 * void setAutoExpandDelay ( int delay )
 */
HB_FUNC( QT_QTREEVIEW_SETAUTOEXPANDDELAY )
{
   hbqt_par_QTreeView( 1 )->setAutoExpandDelay( hb_parni( 2 ) );
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNHIDDEN )
{
   hbqt_par_QTreeView( 1 )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNWIDTH )
{
   hbqt_par_QTreeView( 1 )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setExpanded ( const QModelIndex & index, bool expanded )
 */
HB_FUNC( QT_QTREEVIEW_SETEXPANDED )
{
   hbqt_par_QTreeView( 1 )->setExpanded( *hbqt_par_QModelIndex( 2 ), hb_parl( 3 ) );
}

/*
 * void setExpandsOnDoubleClick ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETEXPANDSONDOUBLECLICK )
{
   hbqt_par_QTreeView( 1 )->setExpandsOnDoubleClick( hb_parl( 2 ) );
}

/*
 * void setFirstColumnSpanned ( int row, const QModelIndex & parent, bool span )
 */
HB_FUNC( QT_QTREEVIEW_SETFIRSTCOLUMNSPANNED )
{
   hbqt_par_QTreeView( 1 )->setFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
}

/*
 * void setHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADER )
{
   hbqt_par_QTreeView( 1 )->setHeader( hbqt_par_QHeaderView( 2 ) );
}

/*
 * void setHeaderHidden ( bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADERHIDDEN )
{
   hbqt_par_QTreeView( 1 )->setHeaderHidden( hb_parl( 2 ) );
}

/*
 * void setIndentation ( int i )
 */
HB_FUNC( QT_QTREEVIEW_SETINDENTATION )
{
   hbqt_par_QTreeView( 1 )->setIndentation( hb_parni( 2 ) );
}

/*
 * void setItemsExpandable ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETITEMSEXPANDABLE )
{
   hbqt_par_QTreeView( 1 )->setItemsExpandable( hb_parl( 2 ) );
}

/*
 * void setRootIsDecorated ( bool show )
 */
HB_FUNC( QT_QTREEVIEW_SETROOTISDECORATED )
{
   hbqt_par_QTreeView( 1 )->setRootIsDecorated( hb_parl( 2 ) );
}

/*
 * void setRowHidden ( int row, const QModelIndex & parent, bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETROWHIDDEN )
{
   hbqt_par_QTreeView( 1 )->setRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETSORTINGENABLED )
{
   hbqt_par_QTreeView( 1 )->setSortingEnabled( hb_parl( 2 ) );
}

/*
 * void setUniformRowHeights ( bool uniform )
 */
HB_FUNC( QT_QTREEVIEW_SETUNIFORMROWHEIGHTS )
{
   hbqt_par_QTreeView( 1 )->setUniformRowHeights( hb_parl( 2 ) );
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTREEVIEW_SETWORDWRAP )
{
   hbqt_par_QTreeView( 1 )->setWordWrap( hb_parl( 2 ) );
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEVIEW_SORTBYCOLUMN )
{
   hbqt_par_QTreeView( 1 )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/*
 * bool uniformRowHeights () const
 */
HB_FUNC( QT_QTREEVIEW_UNIFORMROWHEIGHTS )
{
   hb_retl( hbqt_par_QTreeView( 1 )->uniformRowHeights() );
}

/*
 * virtual QRect visualRect ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QTREEVIEW_VISUALRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QTreeView( 1 )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTREEVIEW_WORDWRAP )
{
   hb_retl( hbqt_par_QTreeView( 1 )->wordWrap() );
}

/*
 * void collapse ( const QModelIndex & index )
 */
HB_FUNC( QT_QTREEVIEW_COLLAPSE )
{
   hbqt_par_QTreeView( 1 )->collapse( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * void collapseAll ()
 */
HB_FUNC( QT_QTREEVIEW_COLLAPSEALL )
{
   hbqt_par_QTreeView( 1 )->collapseAll();
}

/*
 * void expand ( const QModelIndex & index )
 */
HB_FUNC( QT_QTREEVIEW_EXPAND )
{
   hbqt_par_QTreeView( 1 )->expand( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * void expandAll ()
 */
HB_FUNC( QT_QTREEVIEW_EXPANDALL )
{
   hbqt_par_QTreeView( 1 )->expandAll();
}

/*
 * void expandToDepth ( int depth )
 */
HB_FUNC( QT_QTREEVIEW_EXPANDTODEPTH )
{
   hbqt_par_QTreeView( 1 )->expandToDepth( hb_parni( 2 ) );
}

/*
 * void hideColumn ( int column )
 */
HB_FUNC( QT_QTREEVIEW_HIDECOLUMN )
{
   hbqt_par_QTreeView( 1 )->hideColumn( hb_parni( 2 ) );
}

/*
 * void resizeColumnToContents ( int column )
 */
HB_FUNC( QT_QTREEVIEW_RESIZECOLUMNTOCONTENTS )
{
   hbqt_par_QTreeView( 1 )->resizeColumnToContents( hb_parni( 2 ) );
}

/*
 * void showColumn ( int column )
 */
HB_FUNC( QT_QTREEVIEW_SHOWCOLUMN )
{
   hbqt_par_QTreeView( 1 )->showColumn( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
