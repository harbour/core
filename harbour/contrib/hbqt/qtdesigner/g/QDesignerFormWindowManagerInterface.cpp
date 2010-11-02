/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtdesigner.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerFormWindowManagerInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerFormWindowManagerInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerFormWindowManagerInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerFormWindowManagerInterface * p = ( HBQT_GC_T_QDesignerFormWindowManagerInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerFormWindowManagerInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormWindowManagerInterface >( ( QDesignerFormWindowManagerInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowManagerInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowManagerInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerFormWindowManagerInterface() );
}

/* virtual QAction * actionAdjustSize () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONADJUSTSIZE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAdjustSize(), false ) );
}

/* virtual QAction * actionBreakLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONBREAKLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionBreakLayout(), false ) );
}

/* virtual QAction * actionCopy () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCOPY )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCopy(), false ) );
}

/* virtual QAction * actionCut () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONCUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionCut(), false ) );
}

/* virtual QAction * actionDelete () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONDELETE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionDelete(), false ) );
}

/* QAction * actionFormLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONFORMLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionFormLayout(), false ) );
}

/* virtual QAction * actionGridLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONGRIDLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionGridLayout(), false ) );
}

/* virtual QAction * actionHorizontalLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONHORIZONTALLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionHorizontalLayout(), false ) );
}

/* virtual QAction * actionLower () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONLOWER )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionLower(), false ) );
}

/* virtual QAction * actionPaste () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONPASTE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionPaste(), false ) );
}

/* virtual QAction * actionRaise () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONRAISE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRaise(), false ) );
}

/* virtual QAction * actionRedo () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONREDO )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionRedo(), false ) );
}

/* virtual QAction * actionSelectAll () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSELECTALL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSelectAll(), false ) );
}

/* QAction * actionSimplifyLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSIMPLIFYLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSimplifyLayout(), false ) );
}

/* virtual QAction * actionSplitHorizontal () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITHORIZONTAL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitHorizontal(), false ) );
}

/* virtual QAction * actionSplitVertical () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONSPLITVERTICAL )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionSplitVertical(), false ) );
}

/* virtual QAction * actionUndo () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONUNDO )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionUndo(), false ) );
}

/* virtual QAction * actionVerticalLayout () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIONVERTICALLAYOUT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionVerticalLayout(), false ) );
}

/* virtual QDesignerFormWindowInterface * activeFormWindow () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ACTIVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->activeFormWindow(), false ) );
}

/* virtual QDesignerFormEditorInterface * core () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CORE )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
}

/* virtual QDesignerFormWindowInterface * createFormWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 ) */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_CREATEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->createFormWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
}

/* virtual QDesignerFormWindowInterface * formWindow ( int index ) const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->formWindow( hb_parni( 2 ) ), false ) );
}

/* virtual int formWindowCount () const */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_FORMWINDOWCOUNT )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      hb_retni( ( p )->formWindowCount() );
}

/* virtual void addFormWindow ( QDesignerFormWindowInterface * formWindow ) */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_ADDFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->addFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
}

/* virtual void removeFormWindow ( QDesignerFormWindowInterface * formWindow ) */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_REMOVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->removeFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
}

/* virtual void setActiveFormWindow ( QDesignerFormWindowInterface * formWindow ) */
HB_FUNC( QT_QDESIGNERFORMWINDOWMANAGERINTERFACE_SETACTIVEFORMWINDOW )
{
   QDesignerFormWindowManagerInterface * p = hbqt_par_QDesignerFormWindowManagerInterface( 1 );
   if( p )
      ( p )->setActiveFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
