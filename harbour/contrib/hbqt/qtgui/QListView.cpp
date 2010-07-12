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

#include "../hbqt.h"

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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QListView;

QT_G_FUNC( hbqt_gcRelease_QListView )
{
   QListView  * ph = NULL ;
   QGC_POINTER_QListView * p = ( QGC_POINTER_QListView * ) Cargo;

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
   QGC_POINTER_QListView * p = ( QGC_POINTER_QListView * ) hb_gcAllocate( sizeof( QGC_POINTER_QListView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QListView >( ( QListView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QListView;
   p->type = QT_TYPE_QListView;

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
      hb_retni( ( p )->batchSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_BATCHSIZE FP=hb_retni( ( p )->batchSize() ); p is NULL" ) );
   }
}

/*
 * void clearPropertyFlags ()
 */
HB_FUNC( QT_QLISTVIEW_CLEARPROPERTYFLAGS )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->clearPropertyFlags();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_CLEARPROPERTYFLAGS FP=( p )->clearPropertyFlags(); p is NULL" ) );
   }
}

/*
 * Flow flow () const
 */
HB_FUNC( QT_QLISTVIEW_FLOW )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( QListView::Flow ) ( p )->flow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_FLOW FP=hb_retni( ( QListView::Flow ) ( p )->flow() ); p is NULL" ) );
   }
}

/*
 * QSize gridSize () const
 */
HB_FUNC( QT_QLISTVIEW_GRIDSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->gridSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_GRIDSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->gridSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QLISTVIEW_ISROWHIDDEN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_ISROWHIDDEN FP=hb_retl( ( p )->isRowHidden( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isSelectionRectVisible () const
 */
HB_FUNC( QT_QLISTVIEW_ISSELECTIONRECTVISIBLE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retl( ( p )->isSelectionRectVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_ISSELECTIONRECTVISIBLE FP=hb_retl( ( p )->isSelectionRectVisible() ); p is NULL" ) );
   }
}

/*
 * bool isWrapping () const
 */
HB_FUNC( QT_QLISTVIEW_ISWRAPPING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retl( ( p )->isWrapping() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_ISWRAPPING FP=hb_retl( ( p )->isWrapping() ); p is NULL" ) );
   }
}

/*
 * LayoutMode layoutMode () const
 */
HB_FUNC( QT_QLISTVIEW_LAYOUTMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( QListView::LayoutMode ) ( p )->layoutMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_LAYOUTMODE FP=hb_retni( ( QListView::LayoutMode ) ( p )->layoutMode() ); p is NULL" ) );
   }
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QLISTVIEW_MODELCOLUMN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( p )->modelColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_MODELCOLUMN FP=hb_retni( ( p )->modelColumn() ); p is NULL" ) );
   }
}

/*
 * Movement movement () const
 */
HB_FUNC( QT_QLISTVIEW_MOVEMENT )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( QListView::Movement ) ( p )->movement() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_MOVEMENT FP=hb_retni( ( QListView::Movement ) ( p )->movement() ); p is NULL" ) );
   }
}

/*
 * ResizeMode resizeMode () const
 */
HB_FUNC( QT_QLISTVIEW_RESIZEMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( QListView::ResizeMode ) ( p )->resizeMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_RESIZEMODE FP=hb_retni( ( QListView::ResizeMode ) ( p )->resizeMode() ); p is NULL" ) );
   }
}

/*
 * void setBatchSize ( int batchSize )
 */
HB_FUNC( QT_QLISTVIEW_SETBATCHSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setBatchSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETBATCHSIZE FP=( p )->setBatchSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFlow ( Flow flow )
 */
HB_FUNC( QT_QLISTVIEW_SETFLOW )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setFlow( ( QListView::Flow ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETFLOW FP=( p )->setFlow( ( QListView::Flow ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGridSize ( const QSize & size )
 */
HB_FUNC( QT_QLISTVIEW_SETGRIDSIZE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setGridSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETGRIDSIZE FP=( p )->setGridSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayoutMode ( LayoutMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETLAYOUTMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setLayoutMode( ( QListView::LayoutMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETLAYOUTMODE FP=( p )->setLayoutMode( ( QListView::LayoutMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setModelColumn ( int column )
 */
HB_FUNC( QT_QLISTVIEW_SETMODELCOLUMN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setModelColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETMODELCOLUMN FP=( p )->setModelColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMovement ( Movement movement )
 */
HB_FUNC( QT_QLISTVIEW_SETMOVEMENT )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setMovement( ( QListView::Movement ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETMOVEMENT FP=( p )->setMovement( ( QListView::Movement ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETRESIZEMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setResizeMode( ( QListView::ResizeMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETRESIZEMODE FP=( p )->setResizeMode( ( QListView::ResizeMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QLISTVIEW_SETROWHIDDEN )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETROWHIDDEN FP=( p )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionRectVisible ( bool show )
 */
HB_FUNC( QT_QLISTVIEW_SETSELECTIONRECTVISIBLE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setSelectionRectVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETSELECTIONRECTVISIBLE FP=( p )->setSelectionRectVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSpacing ( int space )
 */
HB_FUNC( QT_QLISTVIEW_SETSPACING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETSPACING FP=( p )->setSpacing( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUniformItemSizes ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETUNIFORMITEMSIZES )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setUniformItemSizes( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETUNIFORMITEMSIZES FP=( p )->setUniformItemSizes( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETVIEWMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setViewMode( ( QListView::ViewMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETVIEWMODE FP=( p )->setViewMode( ( QListView::ViewMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QLISTVIEW_SETWORDWRAP )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setWordWrap( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETWORDWRAP FP=( p )->setWordWrap( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWrapping ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETWRAPPING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      ( p )->setWrapping( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SETWRAPPING FP=( p )->setWrapping( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QLISTVIEW_SPACING )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_SPACING FP=hb_retni( ( p )->spacing() ); p is NULL" ) );
   }
}

/*
 * bool uniformItemSizes () const
 */
HB_FUNC( QT_QLISTVIEW_UNIFORMITEMSIZES )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retl( ( p )->uniformItemSizes() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_UNIFORMITEMSIZES FP=hb_retl( ( p )->uniformItemSizes() ); p is NULL" ) );
   }
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QLISTVIEW_VIEWMODE )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retni( ( QListView::ViewMode ) ( p )->viewMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_VIEWMODE FP=hb_retni( ( QListView::ViewMode ) ( p )->viewMode() ); p is NULL" ) );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QLISTVIEW_WORDWRAP )
{
   QListView * p = hbqt_par_QListView( 1 );
   if( p )
      hb_retl( ( p )->wordWrap() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTVIEW_WORDWRAP FP=hb_retl( ( p )->wordWrap() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
