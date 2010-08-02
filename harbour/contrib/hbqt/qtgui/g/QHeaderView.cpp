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
   QPointer< QHeaderView > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QHeaderView;

QT_G_FUNC( hbqt_gcRelease_QHeaderView )
{
   QHeaderView  * ph = NULL ;
   QGC_POINTER_QHeaderView * p = ( QGC_POINTER_QHeaderView * ) Cargo;

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
   QGC_POINTER_QHeaderView * p = ( QGC_POINTER_QHeaderView * ) hb_gcAllocate( sizeof( QGC_POINTER_QHeaderView ), hbqt_gcFuncs() );

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
      hb_retl( ( p )->cascadingSectionResizes() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_CASCADINGSECTIONRESIZES FP=hb_retl( ( p )->cascadingSectionResizes() ); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QHEADERVIEW_COUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * Qt::Alignment defaultAlignment () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTALIGNMENT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->defaultAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_DEFAULTALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->defaultAlignment() ); p is NULL" ) );
   }
}

/*
 * int defaultSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_DEFAULTSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->defaultSectionSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_DEFAULTSECTIONSIZE FP=hb_retni( ( p )->defaultSectionSize() ); p is NULL" ) );
   }
}

/*
 * int hiddenSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_HIDDENSECTIONCOUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->hiddenSectionCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_HIDDENSECTIONCOUNT FP=hb_retni( ( p )->hiddenSectionCount() ); p is NULL" ) );
   }
}

/*
 * void hideSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_HIDESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->hideSection( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_HIDESECTION FP=( p )->hideSection( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool highlightSections () const
 */
HB_FUNC( QT_QHEADERVIEW_HIGHLIGHTSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->highlightSections() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_HIGHLIGHTSECTIONS FP=hb_retl( ( p )->highlightSections() ); p is NULL" ) );
   }
}

/*
 * bool isClickable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISCLICKABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->isClickable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_ISCLICKABLE FP=hb_retl( ( p )->isClickable() ); p is NULL" ) );
   }
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QHEADERVIEW_ISMOVABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->isMovable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_ISMOVABLE FP=hb_retl( ( p )->isMovable() ); p is NULL" ) );
   }
}

/*
 * bool isSectionHidden ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_ISSECTIONHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->isSectionHidden( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_ISSECTIONHIDDEN FP=hb_retl( ( p )->isSectionHidden( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isSortIndicatorShown () const
 */
HB_FUNC( QT_QHEADERVIEW_ISSORTINDICATORSHOWN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->isSortIndicatorShown() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_ISSORTINDICATORSHOWN FP=hb_retl( ( p )->isSortIndicatorShown() ); p is NULL" ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QHEADERVIEW_LENGTH )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->length() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_LENGTH FP=hb_retni( ( p )->length() ); p is NULL" ) );
   }
}

/*
 * int logicalIndex ( int visualIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEX )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->logicalIndex( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_LOGICALINDEX FP=hb_retni( ( p )->logicalIndex( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int logicalIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_LOGICALINDEXAT FP=hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int logicalIndexAt ( int x, int y ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_1 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_LOGICALINDEXAT_1 FP=hb_retni( ( p )->logicalIndexAt( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int logicalIndexAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QHEADERVIEW_LOGICALINDEXAT_2 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->logicalIndexAt( *hbqt_par_QPoint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_LOGICALINDEXAT_2 FP=hb_retni( ( p )->logicalIndexAt( *hbqt_par_QPoint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int minimumSectionSize () const
 */
HB_FUNC( QT_QHEADERVIEW_MINIMUMSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->minimumSectionSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_MINIMUMSECTIONSIZE FP=hb_retni( ( p )->minimumSectionSize() ); p is NULL" ) );
   }
}

/*
 * void moveSection ( int from, int to )
 */
HB_FUNC( QT_QHEADERVIEW_MOVESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->moveSection( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_MOVESECTION FP=( p )->moveSection( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int offset () const
 */
HB_FUNC( QT_QHEADERVIEW_OFFSET )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->offset() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_OFFSET FP=hb_retni( ( p )->offset() ); p is NULL" ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QHEADERVIEW_ORIENTATION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_ORIENTATION FP=hb_retni( ( Qt::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * ResizeMode resizeMode ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_RESIZEMODE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( QHeaderView::ResizeMode ) ( p )->resizeMode( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_RESIZEMODE FP=hb_retni( ( QHeaderView::ResizeMode ) ( p )->resizeMode( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void resizeSection ( int logicalIndex, int size )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->resizeSection( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_RESIZESECTION FP=( p )->resizeSection( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void resizeSections ( QHeaderView::ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_RESIZESECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->resizeSections( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_RESIZESECTIONS FP=( p )->resizeSections( ( QHeaderView::ResizeMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QHEADERVIEW_RESTORESTATE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_RESTORESTATE FP=hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QHEADERVIEW_SAVESTATE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SAVESTATE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) ); p is NULL" ) );
   }
}

/*
 * int sectionPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->sectionPosition( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONPOSITION FP=hb_retni( ( p )->sectionPosition( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int sectionSize ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->sectionSize( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONSIZE FP=hb_retni( ( p )->sectionSize( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int sectionSizeHint ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSIZEHINT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->sectionSizeHint( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONSIZEHINT FP=hb_retni( ( p )->sectionSizeHint( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int sectionViewportPosition ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONVIEWPORTPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->sectionViewportPosition( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONVIEWPORTPOSITION FP=hb_retni( ( p )->sectionViewportPosition( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool sectionsHidden () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->sectionsHidden() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONSHIDDEN FP=hb_retl( ( p )->sectionsHidden() ); p is NULL" ) );
   }
}

/*
 * bool sectionsMoved () const
 */
HB_FUNC( QT_QHEADERVIEW_SECTIONSMOVED )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->sectionsMoved() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SECTIONSMOVED FP=hb_retl( ( p )->sectionsMoved() ); p is NULL" ) );
   }
}

/*
 * void setCascadingSectionResizes ( bool enable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCASCADINGSECTIONRESIZES )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setCascadingSectionResizes( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETCASCADINGSECTIONRESIZES FP=( p )->setCascadingSectionResizes( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setClickable ( bool clickable )
 */
HB_FUNC( QT_QHEADERVIEW_SETCLICKABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setClickable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETCLICKABLE FP=( p )->setClickable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTALIGNMENT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setDefaultAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETDEFAULTALIGNMENT FP=( p )->setDefaultAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETDEFAULTSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setDefaultSectionSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETDEFAULTSECTIONSIZE FP=( p )->setDefaultSectionSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHighlightSections ( bool highlight )
 */
HB_FUNC( QT_QHEADERVIEW_SETHIGHLIGHTSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setHighlightSections( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETHIGHLIGHTSECTIONS FP=( p )->setHighlightSections( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumSectionSize ( int size )
 */
HB_FUNC( QT_QHEADERVIEW_SETMINIMUMSECTIONSIZE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setMinimumSectionSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETMINIMUMSECTIONSIZE FP=( p )->setMinimumSectionSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QHEADERVIEW_SETMOVABLE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setMovable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETMOVABLE FP=( p )->setMovable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setResizeMode( ( QHeaderView::ResizeMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETRESIZEMODE FP=( p )->setResizeMode( ( QHeaderView::ResizeMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResizeMode ( int logicalIndex, ResizeMode mode )
 */
HB_FUNC( QT_QHEADERVIEW_SETRESIZEMODE_1 )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setResizeMode( hb_parni( 2 ), ( QHeaderView::ResizeMode ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETRESIZEMODE_1 FP=( p )->setResizeMode( hb_parni( 2 ), ( QHeaderView::ResizeMode ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSectionHidden ( int logicalIndex, bool hide )
 */
HB_FUNC( QT_QHEADERVIEW_SETSECTIONHIDDEN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setSectionHidden( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETSECTIONHIDDEN FP=( p )->setSectionHidden( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSortIndicator ( int logicalIndex, Qt::SortOrder order )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATOR )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setSortIndicator( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETSORTINDICATOR FP=( p )->setSortIndicator( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSortIndicatorShown ( bool show )
 */
HB_FUNC( QT_QHEADERVIEW_SETSORTINDICATORSHOWN )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setSortIndicatorShown( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETSORTINDICATORSHOWN FP=( p )->setSortIndicatorShown( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStretchLastSection ( bool stretch )
 */
HB_FUNC( QT_QHEADERVIEW_SETSTRETCHLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setStretchLastSection( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETSTRETCHLASTSECTION FP=( p )->setStretchLastSection( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showSection ( int logicalIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SHOWSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->showSection( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SHOWSECTION FP=( p )->showSection( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QHEADERVIEW_SIZEHINT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::SortOrder sortIndicatorOrder () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORORDER )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( Qt::SortOrder ) ( p )->sortIndicatorOrder() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SORTINDICATORORDER FP=hb_retni( ( Qt::SortOrder ) ( p )->sortIndicatorOrder() ); p is NULL" ) );
   }
}

/*
 * int sortIndicatorSection () const
 */
HB_FUNC( QT_QHEADERVIEW_SORTINDICATORSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->sortIndicatorSection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SORTINDICATORSECTION FP=hb_retni( ( p )->sortIndicatorSection() ); p is NULL" ) );
   }
}

/*
 * bool stretchLastSection () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retl( ( p )->stretchLastSection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_STRETCHLASTSECTION FP=hb_retl( ( p )->stretchLastSection() ); p is NULL" ) );
   }
}

/*
 * int stretchSectionCount () const
 */
HB_FUNC( QT_QHEADERVIEW_STRETCHSECTIONCOUNT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->stretchSectionCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_STRETCHSECTIONCOUNT FP=hb_retni( ( p )->stretchSectionCount() ); p is NULL" ) );
   }
}

/*
 * void swapSections ( int first, int second )
 */
HB_FUNC( QT_QHEADERVIEW_SWAPSECTIONS )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->swapSections( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SWAPSECTIONS FP=( p )->swapSections( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int visualIndex ( int logicalIndex ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEX )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->visualIndex( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_VISUALINDEX FP=hb_retni( ( p )->visualIndex( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int visualIndexAt ( int position ) const
 */
HB_FUNC( QT_QHEADERVIEW_VISUALINDEXAT )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      hb_retni( ( p )->visualIndexAt( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_VISUALINDEXAT FP=hb_retni( ( p )->visualIndexAt( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void headerDataChanged ( Qt::Orientation orientation, int logicalFirst, int logicalLast )
 */
HB_FUNC( QT_QHEADERVIEW_HEADERDATACHANGED )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->headerDataChanged( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_HEADERDATACHANGED FP=( p )->headerDataChanged( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setOffset ( int offset )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSET )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setOffset( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETOFFSET FP=( p )->setOffset( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOffsetToLastSection ()
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOLASTSECTION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setOffsetToLastSection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETOFFSETTOLASTSECTION FP=( p )->setOffsetToLastSection(); p is NULL" ) );
   }
}

/*
 * void setOffsetToSectionPosition ( int visualIndex )
 */
HB_FUNC( QT_QHEADERVIEW_SETOFFSETTOSECTIONPOSITION )
{
   QHeaderView * p = hbqt_par_QHeaderView( 1 );
   if( p )
      ( p )->setOffsetToSectionPosition( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHEADERVIEW_SETOFFSETTOSECTIONPOSITION FP=( p )->setOffsetToSectionPosition( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
