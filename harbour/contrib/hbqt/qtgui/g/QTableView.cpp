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
   QTableView  * ph = NULL;
   HBQT_GC_T_QTableView * p = ( HBQT_GC_T_QTableView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QTableView( void * pObj, bool bNew )
{
   HBQT_GC_T_QTableView * p = ( HBQT_GC_T_QTableView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTableView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTableView >( ( QTableView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableView;
   p->type = HBQT_TYPE_QTableView;

   return p;
}

HB_FUNC( QT_QTABLEVIEW )
{
   QTableView * pObj = NULL;

   pObj = new QTableView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTableView( ( void * ) pObj, true ) );
}

/* void clearSpans () */
HB_FUNC( QT_QTABLEVIEW_CLEARSPANS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->clearSpans();
}

/* int columnAt ( int x ) const */
HB_FUNC( QT_QTABLEVIEW_COLUMNAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnAt( hb_parni( 2 ) ) );
}

/* int columnSpan ( int row, int column ) const */
HB_FUNC( QT_QTABLEVIEW_COLUMNSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* int columnViewportPosition ( int column ) const */
HB_FUNC( QT_QTABLEVIEW_COLUMNVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnViewportPosition( hb_parni( 2 ) ) );
}

/* int columnWidth ( int column ) const */
HB_FUNC( QT_QTABLEVIEW_COLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->columnWidth( hb_parni( 2 ) ) );
}

/* Qt::PenStyle gridStyle () const */
HB_FUNC( QT_QTABLEVIEW_GRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( Qt::PenStyle ) ( p )->gridStyle() );
}

/* QHeaderView * horizontalHeader () const */
HB_FUNC( QT_QTABLEVIEW_HORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->horizontalHeader(), false ) );
}

/* virtual QModelIndex indexAt ( const QPoint & pos ) const */
HB_FUNC( QT_QTABLEVIEW_INDEXAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexAt( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* bool isColumnHidden ( int column ) const */
HB_FUNC( QT_QTABLEVIEW_ISCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isColumnHidden( hb_parni( 2 ) ) );
}

/* bool isCornerButtonEnabled () const */
HB_FUNC( QT_QTABLEVIEW_ISCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isCornerButtonEnabled() );
}

/* bool isRowHidden ( int row ) const */
HB_FUNC( QT_QTABLEVIEW_ISROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) );
}

/* bool isSortingEnabled () const */
HB_FUNC( QT_QTABLEVIEW_ISSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->isSortingEnabled() );
}

/* int rowAt ( int y ) const */
HB_FUNC( QT_QTABLEVIEW_ROWAT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowAt( hb_parni( 2 ) ) );
}

/* int rowHeight ( int row ) const */
HB_FUNC( QT_QTABLEVIEW_ROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowHeight( hb_parni( 2 ) ) );
}

/* int rowSpan ( int row, int column ) const */
HB_FUNC( QT_QTABLEVIEW_ROWSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowSpan( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* int rowViewportPosition ( int row ) const */
HB_FUNC( QT_QTABLEVIEW_ROWVIEWPORTPOSITION )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retni( ( p )->rowViewportPosition( hb_parni( 2 ) ) );
}

/* void setColumnHidden ( int column, bool hide ) */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setColumnHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setColumnWidth ( int column, int width ) */
HB_FUNC( QT_QTABLEVIEW_SETCOLUMNWIDTH )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setColumnWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setCornerButtonEnabled ( bool enable ) */
HB_FUNC( QT_QTABLEVIEW_SETCORNERBUTTONENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setCornerButtonEnabled( hb_parl( 2 ) );
}

/* void setGridStyle ( Qt::PenStyle style ) */
HB_FUNC( QT_QTABLEVIEW_SETGRIDSTYLE )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setGridStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/* void setHorizontalHeader ( QHeaderView * header ) */
HB_FUNC( QT_QTABLEVIEW_SETHORIZONTALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setHorizontalHeader( hbqt_par_QHeaderView( 2 ) );
}

/* void setRowHeight ( int row, int height ) */
HB_FUNC( QT_QTABLEVIEW_SETROWHEIGHT )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setRowHeight( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setRowHidden ( int row, bool hide ) */
HB_FUNC( QT_QTABLEVIEW_SETROWHIDDEN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setSortingEnabled ( bool enable ) */
HB_FUNC( QT_QTABLEVIEW_SETSORTINGENABLED )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setSortingEnabled( hb_parl( 2 ) );
}

/* void setSpan ( int row, int column, int rowSpanCount, int columnSpanCount ) */
HB_FUNC( QT_QTABLEVIEW_SETSPAN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setSpan( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setVerticalHeader ( QHeaderView * header ) */
HB_FUNC( QT_QTABLEVIEW_SETVERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setVerticalHeader( hbqt_par_QHeaderView( 2 ) );
}

/* void setWordWrap ( bool on ) */
HB_FUNC( QT_QTABLEVIEW_SETWORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setWordWrap( hb_parl( 2 ) );
}

/* bool showGrid () const */
HB_FUNC( QT_QTABLEVIEW_SHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
}

/* void sortByColumn ( int column, Qt::SortOrder order ) */
HB_FUNC( QT_QTABLEVIEW_SORTBYCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->sortByColumn( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/* QHeaderView * verticalHeader () const */
HB_FUNC( QT_QTABLEVIEW_VERTICALHEADER )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( p )->verticalHeader(), false ) );
}

/* bool wordWrap () const */
HB_FUNC( QT_QTABLEVIEW_WORDWRAP )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      hb_retl( ( p )->wordWrap() );
}

/* void hideColumn ( int column ) */
HB_FUNC( QT_QTABLEVIEW_HIDECOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->hideColumn( hb_parni( 2 ) );
}

/* void hideRow ( int row ) */
HB_FUNC( QT_QTABLEVIEW_HIDEROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->hideRow( hb_parni( 2 ) );
}

/* void resizeColumnToContents ( int column ) */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeColumnToContents( hb_parni( 2 ) );
}

/* void resizeColumnsToContents () */
HB_FUNC( QT_QTABLEVIEW_RESIZECOLUMNSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeColumnsToContents();
}

/* void resizeRowToContents ( int row ) */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeRowToContents( hb_parni( 2 ) );
}

/* void resizeRowsToContents () */
HB_FUNC( QT_QTABLEVIEW_RESIZEROWSTOCONTENTS )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->resizeRowsToContents();
}

/* void selectColumn ( int column ) */
HB_FUNC( QT_QTABLEVIEW_SELECTCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->selectColumn( hb_parni( 2 ) );
}

/* void selectRow ( int row ) */
HB_FUNC( QT_QTABLEVIEW_SELECTROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->selectRow( hb_parni( 2 ) );
}

/* void setShowGrid ( bool show ) */
HB_FUNC( QT_QTABLEVIEW_SETSHOWGRID )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
}

/* void showColumn ( int column ) */
HB_FUNC( QT_QTABLEVIEW_SHOWCOLUMN )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->showColumn( hb_parni( 2 ) );
}

/* void showRow ( int row ) */
HB_FUNC( QT_QTABLEVIEW_SHOWROW )
{
   QTableView * p = hbqt_par_QTableView( 1 );
   if( p )
      ( p )->showRow( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
