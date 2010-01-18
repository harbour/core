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
 *  enum SubWindowOption { RubberBandResize, RubberBandMove }
 *  flags SubWindowOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QMdiSubWindow>


/*
 * QMdiSubWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QMdiSubWindow ()
 *
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QMdiSubWindow > pq;
} QGC_POINTER_QMdiSubWindow;

QT_G_FUNC( hbqt_gcRelease_QMdiSubWindow )
{
   QGC_POINTER_QMdiSubWindow * p = ( QGC_POINTER_QMdiSubWindow * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QMdiSubWindow * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMdiSubWindow              ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QMdiSubWindow              ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QMdiSubWindow               Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QMdiSubWindow               Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMdiSubWindow( void * pObj, bool bNew )
{
   QGC_POINTER_QMdiSubWindow * p = ( QGC_POINTER_QMdiSubWindow * ) hb_gcAllocate( sizeof( QGC_POINTER_QMdiSubWindow ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiSubWindow;

   if( bNew )
   {
      new( & p->pq ) QPointer< QMdiSubWindow >( ( QMdiSubWindow * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QMdiSubWindow              ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QMDISUBWINDOW )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiSubWindow( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QMdiSubWindow() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( pObj, true ) );
}

/*
 * bool isShaded () const
 */
HB_FUNC( QT_QMDISUBWINDOW_ISSHADED )
{
   hb_retl( hbqt_par_QMdiSubWindow( 1 )->isShaded() );
}

/*
 * int keyboardPageStep () const
 */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDPAGESTEP )
{
   hb_retni( hbqt_par_QMdiSubWindow( 1 )->keyboardPageStep() );
}

/*
 * int keyboardSingleStep () const
 */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDSINGLESTEP )
{
   hb_retni( hbqt_par_QMdiSubWindow( 1 )->keyboardSingleStep() );
}

/*
 * QMdiArea * mdiArea () const
 */
HB_FUNC( QT_QMDISUBWINDOW_MDIAREA )
{
   hb_retptrGC( hbqt_gcAllocate_QMdiArea( hbqt_par_QMdiSubWindow( 1 )->mdiArea(), false ) );
}

/*
 * void setKeyboardPageStep ( int step )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDPAGESTEP )
{
   hbqt_par_QMdiSubWindow( 1 )->setKeyboardPageStep( hb_parni( 2 ) );
}

/*
 * void setKeyboardSingleStep ( int step )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDSINGLESTEP )
{
   hbqt_par_QMdiSubWindow( 1 )->setKeyboardSingleStep( hb_parni( 2 ) );
}

/*
 * void setOption ( SubWindowOption option, bool on = true )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETOPTION )
{
   hbqt_par_QMdiSubWindow( 1 )->setOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setSystemMenu ( QMenu * systemMenu )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETSYSTEMMENU )
{
   hbqt_par_QMdiSubWindow( 1 )->setSystemMenu( hbqt_par_QMenu( 2 ) );
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETWIDGET )
{
   hbqt_par_QMdiSubWindow( 1 )->setWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * QMenu * systemMenu () const
 */
HB_FUNC( QT_QMDISUBWINDOW_SYSTEMMENU )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QMdiSubWindow( 1 )->systemMenu(), false ) );
}

/*
 * bool testOption ( SubWindowOption option ) const
 */
HB_FUNC( QT_QMDISUBWINDOW_TESTOPTION )
{
   hb_retl( hbqt_par_QMdiSubWindow( 1 )->testOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ) ) );
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QMDISUBWINDOW_WIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QMdiSubWindow( 1 )->widget(), false ) );
}

/*
 * void showShaded ()
 */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSHADED )
{
   hbqt_par_QMdiSubWindow( 1 )->showShaded();
}

/*
 * void showSystemMenu ()
 */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSYSTEMMENU )
{
   hbqt_par_QMdiSubWindow( 1 )->showSystemMenu();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
