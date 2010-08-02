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
#include "hbqtdesigner.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerFormWindowManagerInterface>


/*
 * QDesignerFormWindowManagerInterface ( QObject * parent = 0 )
 * ~QDesignerFormWindowManagerInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerFormWindowManagerInterface > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDesignerFormWindowManagerInterface;

QT_G_FUNC( hbqt_gcRelease_QDesignerFormWindowManagerInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormWindowManagerInterface( void * pObj, bool bNew )
{
   QGC_POINTER_QDesignerFormWindowManagerInterface * p = ( QGC_POINTER_QDesignerFormWindowManagerInterface * ) hb_gcAllocate( sizeof( QGC_POINTER_QDesignerFormWindowManagerInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormWindowManagerInterface >( ( QDesignerFormWindowManagerInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowManagerInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowManagerInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerFormWindowManagerInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerFormWindowManagerInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE )
{
   //hb_retptr( new QDesignerFormWindowManagerInterface() );
}

/*
 * virtual QAction * actionAdjustSize () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONADJUSTSIZE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAdjustSize(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONADJUSTSIZE FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAdjustSize(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionBreakLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONBREAKLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionBreakLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONBREAKLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionBreakLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionCopy () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCOPY )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCopy(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCOPY FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCopy(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionCut () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCut(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCut(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionDelete () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONDELETE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionDelete(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONDELETE FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionDelete(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * actionFormLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONFORMLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionFormLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONFORMLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionFormLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionGridLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONGRIDLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionGridLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONGRIDLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionGridLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionHorizontalLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONHORIZONTALLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionHorizontalLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONHORIZONTALLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionHorizontalLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionLower () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONLOWER )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionLower(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONLOWER FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionLower(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionPaste () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONPASTE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionPaste(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONPASTE FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionPaste(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionRaise () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONRAISE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRaise(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONRAISE FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRaise(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionRedo () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONREDO )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRedo(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONREDO FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRedo(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionSelectAll () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSELECTALL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSelectAll(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSELECTALL FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSelectAll(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * actionSimplifyLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSIMPLIFYLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSimplifyLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSIMPLIFYLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSimplifyLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionSplitHorizontal () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITHORIZONTAL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitHorizontal(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITHORIZONTAL FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitHorizontal(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionSplitVertical () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITVERTICAL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitVertical(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITVERTICAL FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitVertical(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionUndo () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONUNDO )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionUndo(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONUNDO FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionUndo(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QAction * actionVerticalLayout () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONVERTICALLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionVerticalLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONVERTICALLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionVerticalLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormWindowInterface * activeFormWindow () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->activeFormWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIVEFORMWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->activeFormWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormEditorInterface * core () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CORE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CORE FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormWindowInterface * createFormWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CREATEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->createFormWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CREATEFORMWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->createFormWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormWindowInterface * formWindow ( int index ) const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual int formWindowCount () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOWCOUNT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retni( ( p )->formWindowCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOWCOUNT FP=hb_retni( ( p )->formWindowCount() ); p is NULL" ) );
   }
}

/*
 * virtual void addFormWindow ( QDesignerFormWindowInterface * formWindow )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ADDFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->addFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ADDFORMWINDOW FP=( p )->addFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void removeFormWindow ( QDesignerFormWindowInterface * formWindow )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_REMOVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->removeFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_REMOVEFORMWINDOW FP=( p )->removeFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setActiveFormWindow ( QDesignerFormWindowInterface * formWindow )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_SETACTIVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->setActiveFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_SETACTIVEFORMWINDOW FP=( p )->setActiveFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
