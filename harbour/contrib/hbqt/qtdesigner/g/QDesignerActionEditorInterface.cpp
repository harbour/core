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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerActionEditorInterface>


/*
 * QDesignerActionEditorInterface ( QWidget * parent, Qt::WindowFlags flags = 0 )
 * ~QDesignerActionEditorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerActionEditorInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerActionEditorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerActionEditorInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerActionEditorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerActionEditorInterface * p = ( HBQT_GC_T_QDesignerActionEditorInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerActionEditorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerActionEditorInterface >( ( QDesignerActionEditorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerActionEditorInterface;
   p->type = HBQT_TYPE_QDesignerActionEditorInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERACTIONEDITORINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerActionEditorInterface() );
}

/* virtual QDesignerFormEditorInterface * core () const */
HB_FUNC( QT_QDESIGNERACTIONEDITORINTERFACE_CORE )
{
   QDesignerActionEditorInterface * p = hbqt_par_QDesignerActionEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
}

/* virtual void manageAction ( QAction * action ) = 0 */
HB_FUNC( QT_QDESIGNERACTIONEDITORINTERFACE_MANAGEACTION )
{
   QDesignerActionEditorInterface * p = hbqt_par_QDesignerActionEditorInterface( 1 );
   if( p )
      ( p )->manageAction( hbqt_par_QAction( 2 ) );
}

/* virtual void unmanageAction ( QAction * action ) = 0 */
HB_FUNC( QT_QDESIGNERACTIONEDITORINTERFACE_UNMANAGEACTION )
{
   QDesignerActionEditorInterface * p = hbqt_par_QDesignerActionEditorInterface( 1 );
   if( p )
      ( p )->unmanageAction( hbqt_par_QAction( 2 ) );
}

/* virtual void setFormWindow ( QDesignerFormWindowInterface * formWindow ) = 0 */
HB_FUNC( QT_QDESIGNERACTIONEDITORINTERFACE_SETFORMWINDOW )
{
   QDesignerActionEditorInterface * p = hbqt_par_QDesignerActionEditorInterface( 1 );
   if( p )
      ( p )->setFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
