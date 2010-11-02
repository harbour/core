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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerObjectInspectorInterface>


/*
 * QDesignerObjectInspectorInterface ( QWidget * parent, Qt::WindowFlags flags = 0 )
 * ~QDesignerObjectInspectorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerObjectInspectorInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerObjectInspectorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerObjectInspectorInterface )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesignerObjectInspectorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerObjectInspectorInterface * p = ( HBQT_GC_T_QDesignerObjectInspectorInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerObjectInspectorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerObjectInspectorInterface >( ( QDesignerObjectInspectorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerObjectInspectorInterface;
   p->type = HBQT_TYPE_QDesignerObjectInspectorInterface;

   return p;
}

HB_FUNC( QT_QDESIGNEROBJECTINSPECTORINTERFACE )
{
   //__HB_RETPTRGC__( new QDesignerObjectInspectorInterface() );
}

/* virtual QDesignerFormEditorInterface * core () const */
HB_FUNC( QT_QDESIGNEROBJECTINSPECTORINTERFACE_CORE )
{
   QDesignerObjectInspectorInterface * p = hbqt_par_QDesignerObjectInspectorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
}

/* virtual void setFormWindow ( QDesignerFormWindowInterface * formWindow ) = 0 */
HB_FUNC( QT_QDESIGNEROBJECTINSPECTORINTERFACE_SETFORMWINDOW )
{
   QDesignerObjectInspectorInterface * p = hbqt_par_QDesignerObjectInspectorInterface( 1 );
   if( p )
      ( p )->setFormWindow( hbqt_par_QDesignerFormWindowInterface( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
