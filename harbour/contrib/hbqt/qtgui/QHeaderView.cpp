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

/*
 *  enum ResizeMode { Interactive, Fixed, Stretch, ResizeToContents, Custom }
 */

#include <QtCore/QPointer>

#include <QtGui/QHeaderView>


/*
 * QHeaderView ( Qt::Orientation orientation, QWidget * parent = 0 )
 * virtual ~QHeaderView ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QHeaderView > pq;
} QGC_POINTER_QHeaderView;

QT_G_FUNC( hbqt_gcRelease_QHeaderView )
{
   QGC_POINTER_QHeaderView * p = ( QGC_POINTER_QHeaderView * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QHeaderView * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QHeaderView                ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QHeaderView                ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QHeaderView                 Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QHeaderView                 Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHeaderView( void * pObj, bool bNew )
{
   QGC_POINTER_QHeaderView * p = ( QGC_POINTER_QHeaderView * ) hb_gcAllocate( sizeof( QGC_POINTER_QHeaderView ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHeaderView;

   if( bNew )
   {
      new( & p->pq ) QPointer< QHeaderView >( ( QHeaderView * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QHeaderView                ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QHEADERVIEW )
{
   void * pObj = NULL;

   pObj = new QHeaderView( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QHeaderView( pObj, true ) );
}
/*
 * bool cascadingSectionResizes () const
 */
HB_FUNC( QT_QHEADERVIEW_CASCADINGSECTIONRESIZES )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->cascadingSectionResizes() );
}

/*
 * int count () const
 */
HB_FUNC( QT_QHEADERVIEW_COUNT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->count() );
}

/*
 * Qt::Alignment defaultAlignment () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QHeaderView( 1 )->defaultAlignment() );
}

/*
 * int defaultSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTSECTIONSIZE )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->defaultSectionSize() );
}

/*
 * int hiddenSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_HIDDENSECTIONCOUNT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->hiddenSectionCount() );
}

/*
 * void hideSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_HIDESECTION )
{
   hbqt_par_QHeaderView( 1 )->hideSection( hb_parni( 2 ) );
}

/*
 * bool highlightSections () const
 */
HB_FUNC( QT_QHEADERVIEW_HIGHLIGHTSECTIONS )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->highlightSections() );
}

/*
 * bool isClickable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISCLICKABLE )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->isClickable() );
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISMOVABLE )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->isMovable() );
}

/*
 * bool isSectionHidden ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_ISSECTIONHIDDEN )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->isSectionHidden( hb_parni( 2 ) ) );
}

/*
 * bool isSortIndicatorShown () const
 */
HB_FUNC( QT_QHEADERVIEW_ISSORTINDICATORSHOWN )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->isSortIndicatorShown() );
}

/*
 * int length () const
 */
HB_FUNC( QT_QHEADERVIEW_LENGTH )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->length() );
}

/*
 * int logicalIndex ( int visualIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEX )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->logicalIndex( hb_parni( 2 ) ) );
}

/*
 * int logicalIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->logicalIndexAt( hb_parni( 2 ) ) );
}

/*
 * int logicalIndexAt ( int x, int y ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_1 )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->logicalIndexAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * int logicalIndexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_2 )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->logicalIndexAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * int minimumSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_MINIMUMSECTIONSIZE )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->minimumSectionSize() );
}

/*
 * void moveSection ( int from, int to )
 */
HB_FUNC( QT_QHEADERVIEW_MOVESECTION )
{
   hbqt_par_QHeaderView( 1 )->moveSection( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int offset () const
 */
HB_FUNC( QT_QHEADERVIEW_OFFSET )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->offset() );
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QHEADERVIEW_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QHeaderView( 1 )->orientation() );
}

/*
 * ResizeMode resizeMode ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_RESIZEMODE )
{
   hb_retni( ( QHeaderView::ResizeMode ) hbqt_par_QHeaderView( 1 )->resizeMode( hb_parni( 2 ) ) );
}

/*
 * void resizeSection ( int logicalIndex, int size )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTION )
{
   hbqt_par_QHeaderView( 1 )->resizeSection( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void resizeSections ( QHeaderView::ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTIONS )
{
   hbqt_par_QHeaderView( 1 )->resizeSections( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QHEADERVIEW_RESTORESTATE )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QHEADERVIEW_SAVESTATE )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QHeaderView( 1 )->saveState() ), true ) );
}

/*
 * int sectionPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONPOSITION )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->sectionPosition( hb_parni( 2 ) ) );
}

/*
 * int sectionSize ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZE )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->sectionSize( hb_parni( 2 ) ) );
}

/*
 * int sectionSizeHint ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZEHINT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->sectionSizeHint( hb_parni( 2 ) ) );
}

/*
 * int sectionViewportPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONVIEWPORTPOSITION )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->sectionViewportPosition( hb_parni( 2 ) ) );
}

/*
 * bool sectionsHidden () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSHIDDEN )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->sectionsHidden() );
}

/*
 * bool sectionsMoved () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSMOVED )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->sectionsMoved() );
}

/*
 * void setCascadingSectionResizes ( bool enable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCASCADINGSECTIONRESIZES )
{
   hbqt_par_QHeaderView( 1 )->setCascadingSectionResizes( hb_parl( 2 ) );
}

/*
 * void setClickable ( bool clickable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCLICKABLE )
{
   hbqt_par_QHeaderView( 1 )->setClickable( hb_parl( 2 ) );
}

/*
 * void setDefaultAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTALIGNMENT )
{
   hbqt_par_QHeaderView( 1 )->setDefaultAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setDefaultSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTSECTIONSIZE )
{
   hbqt_par_QHeaderView( 1 )->setDefaultSectionSize( hb_parni( 2 ) );
}

/*
 * void setHighlightSections ( bool highlight )
 */
HB_FUNC( QT_QHEADERVIEW_SETHIGHLIGHTSECTIONS )
{
   hbqt_par_QHeaderView( 1 )->setHighlightSections( hb_parl( 2 ) );
}

/*
 * void setMinimumSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETMINIMUMSECTIONSIZE )
{
   hbqt_par_QHeaderView( 1 )->setMinimumSectionSize( hb_parni( 2 ) );
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QHEADERVIEW_SETMOVABLE )
{
   hbqt_par_QHeaderView( 1 )->setMovable( hb_parl( 2 ) );
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE )
{
   hbqt_par_QHeaderView( 1 )->setResizeMode( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
}

/*
 * void setResizeMode ( int logicalIndex, ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE_1 )
{
   hbqt_par_QHeaderView( 1 )->setResizeMode( hb_parni( 2 ), ( QHeaderView::ResizeMode ) hb_parni( 3 ) );
}

/*
 * void setSectionHidden ( int logicalIndex, bool hide )
 */
HB_FUNC( QT_QHEADERVIEW_SETSECTIONHIDDEN )
{
   hbqt_par_QHeaderView( 1 )->setSectionHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setSortIndicator ( int logicalIndex, Qt::SortOrder order )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATOR )
{
   hbqt_par_QHeaderView( 1 )->setSortIndicator( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
}

/*
 * void setSortIndicatorShown ( bool show )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATORSHOWN )
{
   hbqt_par_QHeaderView( 1 )->setSortIndicatorShown( hb_parl( 2 ) );
}

/*
 * void setStretchLastSection ( bool stretch )
 */
HB_FUNC( QT_QHEADERVIEW_SETSTRETCHLASTSECTION )
{
   hbqt_par_QHeaderView( 1 )->setStretchLastSection( hb_parl( 2 ) );
}

/*
 * void showSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SHOWSECTION )
{
   hbqt_par_QHeaderView( 1 )->showSection( hb_parni( 2 ) );
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QHEADERVIEW_SIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QHeaderView( 1 )->sizeHint() ), true ) );
}

/*
 * Qt::SortOrder sortIndicatorOrder () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORORDER )
{
   hb_retni( ( Qt::SortOrder ) hbqt_par_QHeaderView( 1 )->sortIndicatorOrder() );
}

/*
 * int sortIndicatorSection () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORSECTION )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->sortIndicatorSection() );
}

/*
 * bool stretchLastSection () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHLASTSECTION )
{
   hb_retl( hbqt_par_QHeaderView( 1 )->stretchLastSection() );
}

/*
 * int stretchSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHSECTIONCOUNT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->stretchSectionCount() );
}

/*
 * void swapSections ( int first, int second )
 */
HB_FUNC( QT_QHEADERVIEW_SWAPSECTIONS )
{
   hbqt_par_QHeaderView( 1 )->swapSections( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int visualIndex ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEX )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->visualIndex( hb_parni( 2 ) ) );
}

/*
 * int visualIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEXAT )
{
   hb_retni( hbqt_par_QHeaderView( 1 )->visualIndexAt( hb_parni( 2 ) ) );
}

/*
 * void headerDataChanged ( Qt::Orientation orientation, int logicalFirst, int logicalLast )
 */
HB_FUNC( QT_QHEADERVIEW_HEADERDATACHANGED )
{
   hbqt_par_QHeaderView( 1 )->headerDataChanged( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void setOffset ( int offset )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSET )
{
   hbqt_par_QHeaderView( 1 )->setOffset( hb_parni( 2 ) );
}

/*
 * void setOffsetToLastSection ()
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOLASTSECTION )
{
   hbqt_par_QHeaderView( 1 )->setOffsetToLastSection();
}

/*
 * void setOffsetToSectionPosition ( int visualIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOSECTIONPOSITION )
{
   hbqt_par_QHeaderView( 1 )->setOffsetToSectionPosition( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
