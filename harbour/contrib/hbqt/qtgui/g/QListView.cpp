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
 *  enum Flow { LeftToRight, TopToBottom }
 *  enum LayoutMode { SinglePass, Batched }
 *  enum Movement { Static, Free, Snap }
 *  enum ResizeMode { Fixed, Adjust }
 *  enum ViewMode { ListMode, IconMode }
 */

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QListView>


/*
 * QListView ( QWidget * parent = 0 )
 * ~QListView ()
 */

typedef struct
{
   QPointer< QListView > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QListView;

HBQT_GC_FUNC( hbqt_gcRelease_QListView )
{
   QListView  * ph = NULL ;
   HBQT_GC_T_QListView * p = ( HBQT_GC_T_QListView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QListView   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QListView   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QListView          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QListView    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QListView    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QListView( void * pObj, bool bNew )
{
   HBQT_GC_T_QListView * p = ( HBQT_GC_T_QListView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QListView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QListView >( ( QListView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QListView;
   p->type = HBQT_TYPE_QListView;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QListView  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QListView", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLISTVIEW )
{
   QListView * pObj = NULL;

   pObj = ( QListView * ) new QListView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QListView( ( void * ) pObj, true ) );
}

/*
 * int batchSize () const
 */
HB_FUNC( QT_QLISTVIEW_BATCHSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( p )->batchSize() );
   }
}

/*
 * void clearPropertyFlags ()
 */
HB_FUNC( QT_QLISTVIEW_CLEARPROPERTYFLAGS )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->clearPropertyFlags();
   }
}

/*
 * Flow flow () const
 */
HB_FUNC( QT_QLISTVIEW_FLOW )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( QListView::Flow ) ( p )->flow() );
   }
}

/*
 * QSize gridSize () const
 */
HB_FUNC( QT_QLISTVIEW_GRIDSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->gridSize() ), true ) );
   }
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QLISTVIEW_ISROWHIDDEN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) );
   }
}

/*
 * bool isSelectionRectVisible () const
 */
HB_FUNC( QT_QLISTVIEW_ISSELECTIONRECTVISIBLE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retl( ( p )->isSelectionRectVisible() );
   }
}

/*
 * bool isWrapping () const
 */
HB_FUNC( QT_QLISTVIEW_ISWRAPPING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retl( ( p )->isWrapping() );
   }
}

/*
 * LayoutMode layoutMode () const
 */
HB_FUNC( QT_QLISTVIEW_LAYOUTMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( QListView::LayoutMode ) ( p )->layoutMode() );
   }
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QLISTVIEW_MODELCOLUMN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( p )->modelColumn() );
   }
}

/*
 * Movement movement () const
 */
HB_FUNC( QT_QLISTVIEW_MOVEMENT )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( QListView::Movement ) ( p )->movement() );
   }
}

/*
 * ResizeMode resizeMode () const
 */
HB_FUNC( QT_QLISTVIEW_RESIZEMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( QListView::ResizeMode ) ( p )->resizeMode() );
   }
}

/*
 * void setBatchSize ( int batchSize )
 */
HB_FUNC( QT_QLISTVIEW_SETBATCHSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setBatchSize( hb_parni( 2 ) );
   }
}

/*
 * void setFlow ( Flow flow )
 */
HB_FUNC( QT_QLISTVIEW_SETFLOW )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setFlow( ( QListView::Flow ) hb_parni( 2 ) );
   }
}

/*
 * void setGridSize ( const QSize & size )
 */
HB_FUNC( QT_QLISTVIEW_SETGRIDSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setGridSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setLayoutMode ( LayoutMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETLAYOUTMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setLayoutMode( ( QListView::LayoutMode ) hb_parni( 2 ) );
   }
}

/*
 * void setModelColumn ( int column )
 */
HB_FUNC( QT_QLISTVIEW_SETMODELCOLUMN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setModelColumn( hb_parni( 2 ) );
   }
}

/*
 * void setMovement ( Movement movement )
 */
HB_FUNC( QT_QLISTVIEW_SETMOVEMENT )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setMovement( ( QListView::Movement ) hb_parni( 2 ) );
   }
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETRESIZEMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setResizeMode( ( QListView::ResizeMode ) hb_parni( 2 ) );
   }
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QLISTVIEW_SETROWHIDDEN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setSelectionRectVisible ( bool show )
 */
HB_FUNC( QT_QLISTVIEW_SETSELECTIONRECTVISIBLE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setSelectionRectVisible( hb_parl( 2 ) );
   }
}

/*
 * void setSpacing ( int space )
 */
HB_FUNC( QT_QLISTVIEW_SETSPACING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setSpacing( hb_parni( 2 ) );
   }
}

/*
 * void setUniformItemSizes ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETUNIFORMITEMSIZES )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setUniformItemSizes( hb_parl( 2 ) );
   }
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETVIEWMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setViewMode( ( QListView::ViewMode ) hb_parni( 2 ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QLISTVIEW_SETWORDWRAP )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setWordWrap( hb_parl( 2 ) );
   }
}

/*
 * void setWrapping ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETWRAPPING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      ( p )->setWrapping( hb_parl( 2 ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QLISTVIEW_SPACING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( p )->spacing() );
   }
}

/*
 * bool uniformItemSizes () const
 */
HB_FUNC( QT_QLISTVIEW_UNIFORMITEMSIZES )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retl( ( p )->uniformItemSizes() );
   }
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QLISTVIEW_VIEWMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retni( ( QListView::ViewMode ) ( p )->viewMode() );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QLISTVIEW_WORDWRAP )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
   {
      hb_retl( ( p )->wordWrap() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
