/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

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
   HBQT_GC_T_QTreeView * p = ( HBQT_GC_T_QTreeView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QTreeView * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTreeView( void * pObj, bool bNew )
{
   HBQT_GC_T_QTreeView * p = ( HBQT_GC_T_QTreeView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTreeView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTreeView >( ( QTreeView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeView;
   p->type = HBQT_TYPE_QTreeView;

   return p;
}

HB_FUNC( QT_QTREEVIEW )
{
   QTreeView * pObj = NULL;

   pObj = new QTreeView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTreeView( ( void * ) pObj, true ) );
}

/* bool allColumnsShowFocus () const */
HB_FUNC( QT_QTREEVIEW_ALLCOLUMNSSHOWFOCUS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->allColumnsShowFocus() );
}

/* int autoExpandDelay () const */
HB_FUNC( QT_QTREEVIEW_AUTOEXPANDDELAY )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retni( ( p )->autoExpandDelay() );
}

/* int columnAt ( int x ) const */
HB_FUNC( QT_QTREEVIEW_COLUMNAT )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retni( ( p )->columnAt( hb_parni( 2 ) ) );
}

/* int columnViewportPosition ( int column ) const */
HB_FUNC( QT_QTREEVIEW_COLUMNVIEWPORTPOSITION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) );
}

/* int columnWidth ( int column ) const */
HB_FUNC( QT_QTREEVIEW_COLUMNWIDTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) );
}

/* bool expandsOnDoubleClick () const */
HB_FUNC( QT_QTREEVIEW_EXPANDSONDOUBLECLICK )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->expandsOnDoubleClick() );
}

/* QHeaderView * header () const */
HB_FUNC( QT_QTREEVIEW_HEADER )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->header(), false ) );
}

/* int indentation () const */
HB_FUNC( QT_QTREEVIEW_INDENTATION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retni( ( p )->indentation() );
}

/* QModelIndex indexAbove ( const QModelIndex & index ) const */
HB_FUNC( QT_QTREEVIEW_INDEXABOVE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAbove( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* QModelIndex indexBelow ( const QModelIndex & index ) const */
HB_FUNC( QT_QTREEVIEW_INDEXBELOW )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexBelow( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* bool isAnimated () const */
HB_FUNC( QT_QTREEVIEW_ISANIMATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isAnimated() );
}

/* bool isColumnHidden ( int column ) const */
HB_FUNC( QT_QTREEVIEW_ISCOLUMNHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) );
}

/* bool isExpanded ( const QModelIndex & index ) const */
HB_FUNC( QT_QTREEVIEW_ISEXPANDED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isExpanded( *hbqt_par_QModelIndex( 2 ) ) );
}

/* bool isFirstColumnSpanned ( int row, const QModelIndex & parent ) const */
HB_FUNC( QT_QTREEVIEW_ISFIRSTCOLUMNSPANNED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* bool isHeaderHidden () const */
HB_FUNC( QT_QTREEVIEW_ISHEADERHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isHeaderHidden() );
}

/* bool isRowHidden ( int row, const QModelIndex & parent ) const */
HB_FUNC( QT_QTREEVIEW_ISROWHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* bool isSortingEnabled () const */
HB_FUNC( QT_QTREEVIEW_ISSORTINGENABLED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->isSortingEnabled() );
}

/* bool itemsExpandable () const */
HB_FUNC( QT_QTREEVIEW_ITEMSEXPANDABLE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->itemsExpandable() );
}

/* bool rootIsDecorated () const */
HB_FUNC( QT_QTREEVIEW_ROOTISDECORATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->rootIsDecorated() );
}

/* virtual void scrollTo ( const QModelIndex & index, ScrollHint hint = EnsureVisible ) */
HB_FUNC( QT_QTREEVIEW_SCROLLTO )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->scrollTo( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? ( QTreeView::ScrollHint ) hb_parni( 3 ) : ( QTreeView::ScrollHint ) QTreeView::EnsureVisible ) );
}

/* void setAllColumnsShowFocus ( bool enable ) */
HB_FUNC( QT_QTREEVIEW_SETALLCOLUMNSSHOWFOCUS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setAllColumnsShowFocus( hb_parl( 2 ) );
}

/* void setAnimated ( bool enable ) */
HB_FUNC( QT_QTREEVIEW_SETANIMATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setAnimated( hb_parl( 2 ) );
}

/* void setAutoExpandDelay ( int delay ) */
HB_FUNC( QT_QTREEVIEW_SETAUTOEXPANDDELAY )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setAutoExpandDelay( hb_parni( 2 ) );
}

/* void setColumnHidden ( int column, bool hide ) */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setColumnWidth ( int column, int width ) */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNWIDTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setExpanded ( const QModelIndex & index, bool expanded ) */
HB_FUNC( QT_QTREEVIEW_SETEXPANDED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setExpanded( *hbqt_par_QModelIndex( 2 ), hb_parl( 3 ) );
}

/* void setExpandsOnDoubleClick ( bool enable ) */
HB_FUNC( QT_QTREEVIEW_SETEXPANDSONDOUBLECLICK )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setExpandsOnDoubleClick( hb_parl( 2 ) );
}

/* void setFirstColumnSpanned ( int row, const QModelIndex & parent, bool span ) */
HB_FUNC( QT_QTREEVIEW_SETFIRSTCOLUMNSPANNED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setFirstColumnSpanned( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
}

/* void setHeader ( QHeaderView * header ) */
HB_FUNC( QT_QTREEVIEW_SETHEADER )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setHeader( hbqt_par_QHeaderView( 2 ) );
}

/* void setHeaderHidden ( bool hide ) */
HB_FUNC( QT_QTREEVIEW_SETHEADERHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setHeaderHidden( hb_parl( 2 ) );
}

/* void setIndentation ( int i ) */
HB_FUNC( QT_QTREEVIEW_SETINDENTATION )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setIndentation( hb_parni( 2 ) );
}

/* void setItemsExpandable ( bool enable ) */
HB_FUNC( QT_QTREEVIEW_SETITEMSEXPANDABLE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setItemsExpandable( hb_parl( 2 ) );
}

/* void setRootIsDecorated ( bool show ) */
HB_FUNC( QT_QTREEVIEW_SETROOTISDECORATED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setRootIsDecorated( hb_parl( 2 ) );
}

/* void setRowHidden ( int row, const QModelIndex & parent, bool hide ) */
HB_FUNC( QT_QTREEVIEW_SETROWHIDDEN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setRowHidden( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ), hb_parl( 4 ) );
}

/* void setSortingEnabled ( bool enable ) */
HB_FUNC( QT_QTREEVIEW_SETSORTINGENABLED )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setSortingEnabled( hb_parl( 2 ) );
}

/* void setUniformRowHeights ( bool uniform ) */
HB_FUNC( QT_QTREEVIEW_SETUNIFORMROWHEIGHTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setUniformRowHeights( hb_parl( 2 ) );
}

/* void setWordWrap ( bool on ) */
HB_FUNC( QT_QTREEVIEW_SETWORDWRAP )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->setWordWrap( hb_parl( 2 ) );
}

/* void sortByColumn ( int column, Qt::SortOrder order ) */
HB_FUNC( QT_QTREEVIEW_SORTBYCOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/* bool uniformRowHeights () const */
HB_FUNC( QT_QTREEVIEW_UNIFORMROWHEIGHTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->uniformRowHeights() );
}

/* virtual QRect visualRect ( const QModelIndex & index ) const */
HB_FUNC( QT_QTREEVIEW_VISUALRECT )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* bool wordWrap () const */
HB_FUNC( QT_QTREEVIEW_WORDWRAP )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      hb_retl( ( p )->wordWrap() );
}

/* void collapse ( const QModelIndex & index ) */
HB_FUNC( QT_QTREEVIEW_COLLAPSE )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->collapse( *hbqt_par_QModelIndex( 2 ) );
}

/* void collapseAll () */
HB_FUNC( QT_QTREEVIEW_COLLAPSEALL )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->collapseAll();
}

/* void expand ( const QModelIndex & index ) */
HB_FUNC( QT_QTREEVIEW_EXPAND )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->expand( *hbqt_par_QModelIndex( 2 ) );
}

/* void expandAll () */
HB_FUNC( QT_QTREEVIEW_EXPANDALL )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->expandAll();
}

/* void expandToDepth ( int depth ) */
HB_FUNC( QT_QTREEVIEW_EXPANDTODEPTH )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->expandToDepth( hb_parni( 2 ) );
}

/* void hideColumn ( int column ) */
HB_FUNC( QT_QTREEVIEW_HIDECOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->hideColumn( hb_parni( 2 ) );
}

/* void resizeColumnToContents ( int column ) */
HB_FUNC( QT_QTREEVIEW_RESIZECOLUMNTOCONTENTS )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->resizeColumnToContents( hb_parni( 2 ) );
}

/* void showColumn ( int column ) */
HB_FUNC( QT_QTREEVIEW_SHOWCOLUMN )
{
   QTreeView * p = hbqt_par_QTreeView( 1 );
   if( p )
      ( p )->showColumn( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
