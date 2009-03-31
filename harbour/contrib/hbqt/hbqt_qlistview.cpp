/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/



#include <QtGui/QListView>


/*
 * QListView ( QWidget * parent = 0 )
 * ~QListView ()
 */
HB_FUNC( QT_QLISTVIEW )
{
   hb_retptr( ( QListView * ) new QListView( hbqt_par_QWidget( 1 ) ) );
}

/*
 * int batchSize () const
 */
HB_FUNC( QT_QLISTVIEW_BATCHSIZE )
{
   hb_retni( hbqt_par_QListView( 1 )->batchSize(  ) );
}

/*
 * void clearPropertyFlags ()
 */
HB_FUNC( QT_QLISTVIEW_CLEARPROPERTYFLAGS )
{
   hbqt_par_QListView( 1 )->clearPropertyFlags(  );
}

/*
 * Flow flow () const
 */
HB_FUNC( QT_QLISTVIEW_FLOW )
{
   hb_retni( hbqt_par_QListView( 1 )->flow(  ) );
}

/*
 * QSize gridSize () const
 */
HB_FUNC( QT_QLISTVIEW_GRIDSIZE )
{
   hbqt_ret_QSize( hbqt_par_QListView( 1 )->gridSize(  ) );
}

/*
 * bool isRowHidden ( int row ) const
 */
HB_FUNC( QT_QLISTVIEW_ISROWHIDDEN )
{
   hb_retl( hbqt_par_QListView( 1 )->isRowHidden( hb_parni( 2 ) ) );
}

/*
 * bool isSelectionRectVisible () const
 */
HB_FUNC( QT_QLISTVIEW_ISSELECTIONRECTVISIBLE )
{
   hb_retl( hbqt_par_QListView( 1 )->isSelectionRectVisible(  ) );
}

/*
 * bool isWrapping () const
 */
HB_FUNC( QT_QLISTVIEW_ISWRAPPING )
{
   hb_retl( hbqt_par_QListView( 1 )->isWrapping(  ) );
}

/*
 * LayoutMode layoutMode () const
 */
HB_FUNC( QT_QLISTVIEW_LAYOUTMODE )
{
   hb_retni( hbqt_par_QListView( 1 )->layoutMode(  ) );
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QLISTVIEW_MODELCOLUMN )
{
   hb_retni( hbqt_par_QListView( 1 )->modelColumn(  ) );
}

/*
 * Movement movement () const
 */
HB_FUNC( QT_QLISTVIEW_MOVEMENT )
{
   hb_retni( hbqt_par_QListView( 1 )->movement(  ) );
}

/*
 * ResizeMode resizeMode () const
 */
HB_FUNC( QT_QLISTVIEW_RESIZEMODE )
{
   hb_retni( hbqt_par_QListView( 1 )->resizeMode(  ) );
}

/*
 * void setBatchSize ( int batchSize )
 */
HB_FUNC( QT_QLISTVIEW_SETBATCHSIZE )
{
   hbqt_par_QListView( 1 )->setBatchSize( hb_parni( 2 ) );
}

/*
 * void setFlow ( Flow flow )
 */
HB_FUNC( QT_QLISTVIEW_SETFLOW )
{
   hbqt_par_QListView( 1 )->setFlow( ( QListView::Flow ) hb_parni( 2 ) );
}

/*
 * void setGridSize ( const QSize & size )
 */
HB_FUNC( QT_QLISTVIEW_SETGRIDSIZE )
{
   hbqt_par_QListView( 1 )->setGridSize( hbqt_const_QSize( 2 ) );
}

/*
 * void setLayoutMode ( LayoutMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETLAYOUTMODE )
{
   hbqt_par_QListView( 1 )->setLayoutMode( ( QListView::LayoutMode ) hb_parni( 2 ) );
}

/*
 * void setModelColumn ( int column )
 */
HB_FUNC( QT_QLISTVIEW_SETMODELCOLUMN )
{
   hbqt_par_QListView( 1 )->setModelColumn( hb_parni( 2 ) );
}

/*
 * void setMovement ( Movement movement )
 */
HB_FUNC( QT_QLISTVIEW_SETMOVEMENT )
{
   hbqt_par_QListView( 1 )->setMovement( ( QListView::Movement ) hb_parni( 2 ) );
}

/*
 * void setResizeMode ( ResizeMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETRESIZEMODE )
{
   hbqt_par_QListView( 1 )->setResizeMode( ( QListView::ResizeMode ) hb_parni( 2 ) );
}

/*
 * void setRowHidden ( int row, bool hide )
 */
HB_FUNC( QT_QLISTVIEW_SETROWHIDDEN )
{
   hbqt_par_QListView( 1 )->setRowHidden( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setSelectionRectVisible ( bool show )
 */
HB_FUNC( QT_QLISTVIEW_SETSELECTIONRECTVISIBLE )
{
   hbqt_par_QListView( 1 )->setSelectionRectVisible( hb_parl( 2 ) );
}

/*
 * void setSpacing ( int space )
 */
HB_FUNC( QT_QLISTVIEW_SETSPACING )
{
   hbqt_par_QListView( 1 )->setSpacing( hb_parni( 2 ) );
}

/*
 * void setUniformItemSizes ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETUNIFORMITEMSIZES )
{
   hbqt_par_QListView( 1 )->setUniformItemSizes( hb_parl( 2 ) );
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QLISTVIEW_SETVIEWMODE )
{
   hbqt_par_QListView( 1 )->setViewMode( ( QListView::ViewMode ) hb_parni( 2 ) );
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QLISTVIEW_SETWORDWRAP )
{
   hbqt_par_QListView( 1 )->setWordWrap( hb_parl( 2 ) );
}

/*
 * void setWrapping ( bool enable )
 */
HB_FUNC( QT_QLISTVIEW_SETWRAPPING )
{
   hbqt_par_QListView( 1 )->setWrapping( hb_parl( 2 ) );
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QLISTVIEW_SPACING )
{
   hb_retni( hbqt_par_QListView( 1 )->spacing(  ) );
}

/*
 * bool uniformItemSizes () const
 */
HB_FUNC( QT_QLISTVIEW_UNIFORMITEMSIZES )
{
   hb_retl( hbqt_par_QListView( 1 )->uniformItemSizes(  ) );
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QLISTVIEW_VIEWMODE )
{
   hb_retni( hbqt_par_QListView( 1 )->viewMode(  ) );
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QLISTVIEW_WORDWRAP )
{
   hb_retl( hbqt_par_QListView( 1 )->wordWrap(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

