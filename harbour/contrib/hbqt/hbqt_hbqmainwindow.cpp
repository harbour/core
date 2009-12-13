/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
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
#include "hbinit.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqmainwindow.h"

#if defined( _HBQT_MAINWINDOW_MUTEX )
static PHB_ITEM s_mutex = NULL;
#endif

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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, QEvent::Paint );
      PHB_ITEM p1 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

bool HBQMainWindow::event( QEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
   bool bRet = QWidget::event( event );
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
   return bRet;
}

void HBQMainWindow::focusInEvent( QFocusEvent *event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::focusOutEvent( QFocusEvent *event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::keyPressEvent( QKeyEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::mouseDoubleClickEvent( QMouseEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::mouseMoveEvent( QMouseEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::mousePressEvent( QMouseEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::mouseReleaseEvent( QMouseEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::wheelEvent( QWheelEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::resizeEvent( QResizeEvent * event )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
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
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

void HBQMainWindow::closeEvent( QCloseEvent * event )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQMainWindow::closeEvent: ThreadID: %i", threadID ) );

#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexLock( s_mutex );
#endif
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Close );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
#if defined( _HBQT_MAINWINDOW_MUTEX )
   hb_threadMutexUnlock( s_mutex );
#endif
}

static void hbqt_hbqmainwindow_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

#if defined( _HBQT_MAINWINDOW_MUTEX )
   if( s_mutex == NULL )
      s_mutex = hb_threadMutexCreate();
#endif
}

static void hbqt_hbqmainwindow_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

#if defined( _HBQT_MAINWINDOW_MUTEX )
   if( s_mutex != NULL )
   {
      hb_itemRelease( s_mutex );
      s_mutex = NULL;
   }
#endif
}

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< HBQMainWindow > pq;
} QGC_POINTER_HBQMainWindow;

QT_G_FUNC( release_HBQMainWindow )
{
   QGC_POINTER_HBQMainWindow * p = ( QGC_POINTER_HBQMainWindow * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_HBQMainWindow                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_HBQMainWindow                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( HBQMainWindow * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( HBQMainWindow * ) p->ph )->~HBQMainWindow();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( HBQMainWindow * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_HBQMainWindow                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_HBQMainWindow                 Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_HBQMainWindow                 Object Already deleted!" ) );
   }
}

void * gcAllocate_HBQMainWindow( void * pObj )
{
   QGC_POINTER_HBQMainWindow * p = ( QGC_POINTER_HBQMainWindow * ) hb_gcAllocate( sizeof( QGC_POINTER_HBQMainWindow ), gcFuncs() );

   p->ph = pObj;
   p->func = release_HBQMainWindow;
   new( & p->pq ) QPointer< HBQMainWindow >( ( HBQMainWindow * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_HBQMainWindow                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_HBQMAINWINDOW )
{
#if defined( _HBQT_MAINWINDOW_MUTEX )
   if( s_mutex == NULL )
      s_mutex = hb_threadMutexCreate();
#endif

   void * pObj = NULL;

   pObj = ( HBQMainWindow * ) new HBQMainWindow( hb_itemNew( hb_param( 1, HB_IT_BLOCK ) ), hb_parni( 2 ) );

   hb_retptrGC( gcAllocate_HBQMainWindow( pObj ) );
}

HB_FUNC( HB_QT ) {;}

HB_CALL_ON_STARTUP_BEGIN( _hbqt_hbqmainwindow_initialize_ )
   hb_vmAtInit( hbqt_hbqmainwindow_init, NULL );
   hb_vmAtExit( hbqt_hbqmainwindow_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqt_hbqmainwindow_initialize_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqt_hbqmainwindow_initialize_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqt_hbqmainwindow_initialize_ )
   #include "hbiniseg.h"
#endif

#endif
