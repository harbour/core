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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum SubWindowOption { RubberBandResize, RubberBandMove }
 *  flags SubWindowOptions
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
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
   QPointer< QMdiSubWindow > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMdiSubWindow;

HBQT_GC_FUNC( hbqt_gcRelease_QMdiSubWindow )
{
   QMdiSubWindow  * ph = NULL ;
   HBQT_GC_T_QMdiSubWindow * p = ( HBQT_GC_T_QMdiSubWindow * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMdiSubWindow   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMdiSubWindow   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMdiSubWindow          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMdiSubWindow    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMdiSubWindow    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMdiSubWindow( void * pObj, bool bNew )
{
   HBQT_GC_T_QMdiSubWindow * p = ( HBQT_GC_T_QMdiSubWindow * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMdiSubWindow ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMdiSubWindow >( ( QMdiSubWindow * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiSubWindow;
   p->type = HBQT_TYPE_QMdiSubWindow;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMdiSubWindow  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMdiSubWindow", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMDISUBWINDOW )
{
   QMdiSubWindow * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiSubWindow( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QMdiSubWindow() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( void * ) pObj, true ) );
}

/*
 * bool isShaded () const
 */
HB_FUNC( QT_QMDISUBWINDOW_ISSHADED )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->isShaded() );
   }
}

/*
 * int keyboardPageStep () const
 */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDPAGESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retni( ( p )->keyboardPageStep() );
   }
}

/*
 * int keyboardSingleStep () const
 */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDSINGLESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retni( ( p )->keyboardSingleStep() );
   }
}

/*
 * QMdiArea * mdiArea () const
 */
HB_FUNC( QT_QMDISUBWINDOW_MDIAREA )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMdiArea( ( p )->mdiArea(), false ) );
   }
}

/*
 * void setKeyboardPageStep ( int step )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDPAGESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->setKeyboardPageStep( hb_parni( 2 ) );
   }
}

/*
 * void setKeyboardSingleStep ( int step )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDSINGLESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->setKeyboardSingleStep( hb_parni( 2 ) );
   }
}

/*
 * void setOption ( SubWindowOption option, bool on = true )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETOPTION )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->setOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setSystemMenu ( QMenu * systemMenu )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETSYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->setSystemMenu( hbqt_par_QMenu( 2 ) );
   }
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QMDISUBWINDOW_SETWIDGET )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * QMenu * systemMenu () const
 */
HB_FUNC( QT_QMDISUBWINDOW_SYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->systemMenu(), false ) );
   }
}

/*
 * bool testOption ( SubWindowOption option ) const
 */
HB_FUNC( QT_QMDISUBWINDOW_TESTOPTION )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->testOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ) ) );
   }
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QMDISUBWINDOW_WIDGET )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
   }
}

/*
 * void showShaded ()
 */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSHADED )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->showShaded();
   }
}

/*
 * void showSystemMenu ()
 */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
   {
      ( p )->showSystemMenu();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
