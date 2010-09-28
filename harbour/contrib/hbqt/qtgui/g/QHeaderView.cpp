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
 *  enum ResizeMode { Interactive, Fixed, Stretch, ResizeToContents, Custom }
 */

/*
 *  Constructed[ 57/57 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QHeaderView>


/*
 * QHeaderView ( Qt::Orientation orientation, QWidget * parent = 0 )
 * virtual ~QHeaderView ()
 */

typedef struct
{
   QPointer< QHeaderView > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHeaderView;

HBQT_GC_FUNC( hbqt_gcRelease_QHeaderView )
{
   QHeaderView  * ph = NULL ;
   HBQT_GC_T_QHeaderView * p = ( HBQT_GC_T_QHeaderView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QHeaderView   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QHeaderView   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QHeaderView          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QHeaderView    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QHeaderView    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHeaderView( void * pObj, bool bNew )
{
   HBQT_GC_T_QHeaderView * p = ( HBQT_GC_T_QHeaderView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QHeaderView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QHeaderView >( ( QHeaderView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHeaderView;
   p->type = HBQT_TYPE_QHeaderView;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QHeaderView  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QHeaderView", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QHEADERVIEW )
{
   QHeaderView * pObj = NULL;

   pObj = new QHeaderView( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QHeaderView( ( void * ) pObj, true ) );
}

/*
 * bool cascadingSectionResizes () const
 */
HB_FUNC( QT_QHEADERVIEW_CASCADINGSECTIONRESIZES )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->cascadingSectionResizes() );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QHEADERVIEW_COUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * Qt::Alignment defaultAlignment () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTALIGNMENT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->defaultAlignment() );
   }
}

/*
 * int defaultSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->defaultSectionSize() );
   }
}

/*
 * int hiddenSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_HIDDENSECTIONCOUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->hiddenSectionCount() );
   }
}

/*
 * void hideSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_HIDESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->hideSection( hb_parni( 2 ) );
   }
}

/*
 * bool highlightSections () const
 */
HB_FUNC( QT_QHEADERVIEW_HIGHLIGHTSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->highlightSections() );
   }
}

/*
 * bool isClickable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISCLICKABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->isClickable() );
   }
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISMOVABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->isMovable() );
   }
}

/*
 * bool isSectionHidden ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_ISSECTIONHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->isSectionHidden( hb_parni( 2 ) ) );
   }
}

/*
 * bool isSortIndicatorShown () const
 */
HB_FUNC( QT_QHEADERVIEW_ISSORTINDICATORSHOWN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->isSortIndicatorShown() );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QHEADERVIEW_LENGTH )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->length() );
   }
}

/*
 * int logicalIndex ( int visualIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEX )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->logicalIndex( hb_parni( 2 ) ) );
   }
}

/*
 * int logicalIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ) ) );
   }
}

/*
 * int logicalIndexAt ( int x, int y ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_1 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int logicalIndexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_2 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->logicalIndexAt( *hbqt_par_QPoint( 2 ) ) );
   }
}

/*
 * int minimumSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_MINIMUMSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->minimumSectionSize() );
   }
}

/*
 * void moveSection ( int from, int to )
 */
HB_FUNC( QT_QHEADERVIEW_MOVESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->moveSection( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int offset () const
 */
HB_FUNC( QT_QHEADERVIEW_OFFSET )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->offset() );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QHEADERVIEW_ORIENTATION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   }
}

/*
 * ResizeMode resizeMode ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_RESIZEMODE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( QHeaderView::ResizeMode ) ( p )->resizeMode( hb_parni( 2 ) ) );
   }
}

/*
 * void resizeSection ( int logicalIndex, int size )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->resizeSection( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void resizeSections ( QHeaderView::ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->resizeSections( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QHEADERVIEW_RESTORESTATE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
   }
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QHEADERVIEW_SAVESTATE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) );
   }
}

/*
 * int sectionPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->sectionPosition( hb_parni( 2 ) ) );
   }
}

/*
 * int sectionSize ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->sectionSize( hb_parni( 2 ) ) );
   }
}

/*
 * int sectionSizeHint ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZEHINT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->sectionSizeHint( hb_parni( 2 ) ) );
   }
}

/*
 * int sectionViewportPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONVIEWPORTPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->sectionViewportPosition( hb_parni( 2 ) ) );
   }
}

/*
 * bool sectionsHidden () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->sectionsHidden() );
   }
}

/*
 * bool sectionsMoved () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSMOVED )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->sectionsMoved() );
   }
}

/*
 * void setCascadingSectionResizes ( bool enable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCASCADINGSECTIONRESIZES )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setCascadingSectionResizes( hb_parl( 2 ) );
   }
}

/*
 * void setClickable ( bool clickable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCLICKABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setClickable( hb_parl( 2 ) );
   }
}

/*
 * void setDefaultAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTALIGNMENT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setDefaultAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setDefaultSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setDefaultSectionSize( hb_parni( 2 ) );
   }
}

/*
 * void setHighlightSections ( bool highlight )
 */
HB_FUNC( QT_QHEADERVIEW_SETHIGHLIGHTSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setHighlightSections( hb_parl( 2 ) );
   }
}

/*
 * void setMinimumSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETMINIMUMSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setMinimumSectionSize( hb_parni( 2 ) );
   }
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QHEADERVIEW_SETMOVABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setMovable( hb_parl( 2 ) );
   }
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setResizeMode( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
   }
}

/*
 * void setResizeMode ( int logicalIndex, ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE_1 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setResizeMode( hb_parni( 2 ), ( QHeaderView::ResizeMode ) hb_parni( 3 ) );
   }
}

/*
 * void setSectionHidden ( int logicalIndex, bool hide )
 */
HB_FUNC( QT_QHEADERVIEW_SETSECTIONHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setSectionHidden( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setSortIndicator ( int logicalIndex, Qt::SortOrder order )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATOR )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setSortIndicator( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   }
}

/*
 * void setSortIndicatorShown ( bool show )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATORSHOWN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setSortIndicatorShown( hb_parl( 2 ) );
   }
}

/*
 * void setStretchLastSection ( bool stretch )
 */
HB_FUNC( QT_QHEADERVIEW_SETSTRETCHLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setStretchLastSection( hb_parl( 2 ) );
   }
}

/*
 * void showSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SHOWSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->showSection( hb_parni( 2 ) );
   }
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QHEADERVIEW_SIZEHINT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   }
}

/*
 * Qt::SortOrder sortIndicatorOrder () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORORDER )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( Qt::SortOrder ) ( p )->sortIndicatorOrder() );
   }
}

/*
 * int sortIndicatorSection () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->sortIndicatorSection() );
   }
}

/*
 * bool stretchLastSection () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retl( ( p )->stretchLastSection() );
   }
}

/*
 * int stretchSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHSECTIONCOUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->stretchSectionCount() );
   }
}

/*
 * void swapSections ( int first, int second )
 */
HB_FUNC( QT_QHEADERVIEW_SWAPSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->swapSections( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int visualIndex ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEX )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->visualIndex( hb_parni( 2 ) ) );
   }
}

/*
 * int visualIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEXAT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      hb_retni( ( p )->visualIndexAt( hb_parni( 2 ) ) );
   }
}

/*
 * void headerDataChanged ( Qt::Orientation orientation, int logicalFirst, int logicalLast )
 */
HB_FUNC( QT_QHEADERVIEW_HEADERDATACHANGED )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->headerDataChanged( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   }
}

/*
 * void setOffset ( int offset )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSET )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setOffset( hb_parni( 2 ) );
   }
}

/*
 * void setOffsetToLastSection ()
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setOffsetToLastSection();
   }
}

/*
 * void setOffsetToSectionPosition ( int visualIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOSECTIONPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
   {
      ( p )->setOffsetToSectionPosition( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
