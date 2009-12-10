/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 *
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
#include "hbapiitm.h"
#include "hbthread.h"
#include "hbvm.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqmainwindow.h"

static PHB_ITEM s_mutex = NULL;

HBQMainWindow::HBQMainWindow( PHB_ITEM pBlock, int iThreadID )
{
   Qt::WindowFlags flags = Qt::WindowCloseButtonHint    | Qt::WindowMaximizeButtonHint |
                           Qt::WindowMinimizeButtonHint | Qt::WindowSystemMenuHint     |
                           Qt::CustomizeWindowHint      | Qt::WindowTitleHint          |
                           Qt::Window;
   setWindowFlags( flags );
   setFocusPolicy( Qt::StrongFocus );
   setAttribute( Qt::WA_DeleteOnClose );
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
      hb_itemRelease( block );
      block = NULL;
   }

   HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::~HBQMainWindow: END" ) );
}

void HBQMainWindow::paintEvent( QPaintEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, QEvent::Paint );
      PHB_ITEM p1 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   hb_threadMutexUnlock( s_mutex );
}

bool HBQMainWindow::event( QEvent * event )
{
   hb_threadMutexLock( s_mutex );
   bool bRet = QWidget::event( event );
   hb_threadMutexUnlock( s_mutex );
   return bRet;
}

void HBQMainWindow::focusInEvent( QFocusEvent *event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::focusOutEvent( QFocusEvent *event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::keyPressEvent( QKeyEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::mouseDoubleClickEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::mouseMoveEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::mousePressEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::mouseReleaseEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::wheelEvent( QWheelEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::resizeEvent( QResizeEvent * event )
{
   hb_threadMutexLock( s_mutex );
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
   hb_threadMutexUnlock( s_mutex );
}

void HBQMainWindow::closeEvent( QCloseEvent * event )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::closeEvent: ThreadID: %i", threadID ) );

   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Close );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   hb_threadMutexUnlock( s_mutex );
}

HB_FUNC( QT_HBQMAINWINDOW )
{
   if( s_mutex == NULL )
      s_mutex = hb_threadMutexCreate();

   hb_retptr( ( HBQMainWindow * ) new HBQMainWindow( hb_itemNew( hb_param( 1, HB_IT_BLOCK ) ), hb_parni( 2 ) ) );
}

HB_FUNC( QT_HBQMAINWINDOW_DESTROY )
{
   hbqt_par_HBQMainWindow( 1 )->~HBQMainWindow();
}

HB_FUNC( QT_MUTEXCREATE )
{
   if( s_mutex == NULL )
      s_mutex = hb_threadMutexCreate();
}

HB_FUNC( QT_MUTEXDESTROY )
{
   if( s_mutex != NULL )
   {
      hb_itemRelease( s_mutex );
      s_mutex = NULL;
   }
}

#endif
