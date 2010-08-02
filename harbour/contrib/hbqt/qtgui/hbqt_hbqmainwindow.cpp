/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqmainwindow.h"

HBQMainWindow::HBQMainWindow( PHB_ITEM pBlock, int iThreadID )
{
   Qt::WindowFlags flags = Qt::WindowCloseButtonHint    | Qt::WindowMaximizeButtonHint |
                           Qt::WindowMinimizeButtonHint | Qt::WindowSystemMenuHint     |
                           Qt::CustomizeWindowHint      | Qt::WindowTitleHint          |
                           Qt::Window;
   setWindowFlags( flags );
   setFocusPolicy( Qt::StrongFocus );
   //setAttribute( Qt::WA_DeleteOnClose );
   //setAttribute( Qt::WA_NoSystemBackground );
   //setAttribute( Qt::WA_PaintOnScreen );
   //setMouseTracking( true );

   block     = pBlock;
   threadID  = iThreadID;
}

HBQMainWindow::~HBQMainWindow( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::~HBQMainWindow: BEGIN" ) );

   if( block )
   {
      HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::~HBQMainWindow: MID" ) );
      hb_itemRelease( block );
      block = NULL;
   }

   HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::~HBQMainWindow: END" ) );
}

void HBQMainWindow::paintEvent( QPaintEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, QEvent::Paint );
      PHB_ITEM p1 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
}

bool HBQMainWindow::event( QEvent * event )
{
   return QWidget::event( event );
}

void HBQMainWindow::focusInEvent( QFocusEvent *event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::FocusIn );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::focusInEvent( event );
}

void HBQMainWindow::focusOutEvent( QFocusEvent *event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::FocusOut );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::focusOutEvent( event );
}

void HBQMainWindow::keyPressEvent( QKeyEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::KeyPress );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::keyPressEvent( event );
}

void HBQMainWindow::mouseDoubleClickEvent( QMouseEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonDblClick );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseDoubleClickEvent( event );
}

void HBQMainWindow::mouseMoveEvent( QMouseEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseMove );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseMoveEvent( event );
}

void HBQMainWindow::mousePressEvent( QMouseEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonPress );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mousePressEvent( event );
}

void HBQMainWindow::mouseReleaseEvent( QMouseEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonRelease );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseReleaseEvent( event );
}

void HBQMainWindow::wheelEvent( QWheelEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Wheel );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::wheelEvent( event );
}

void HBQMainWindow::resizeEvent( QResizeEvent * event )
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Resize );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::resizeEvent( event );
}

void HBQMainWindow::closeEvent( QCloseEvent * event )
{
   HB_TRACE( HB_TR_ALWAYS, ( "HBQMainWindow::closeEvent: ThreadID: %i", threadID ) );

   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Close );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
}

#endif
