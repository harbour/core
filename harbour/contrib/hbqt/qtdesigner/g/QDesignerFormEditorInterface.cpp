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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QExtensionManager * extensionManager () const
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerFormEditorInterface>


/*
 * QDesignerFormEditorInterface ( QObject * parent = 0 )
 * virtual ~QDesignerFormEditorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerFormEditorInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerFormEditorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerFormEditorInterface )
{
   HBQT_GC_T_QDesignerFormEditorInterface * p = ( HBQT_GC_T_QDesignerFormEditorInterface * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QDesignerFormEditorInterface * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormEditorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerFormEditorInterface * p = ( HBQT_GC_T_QDesignerFormEditorInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerFormEditorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormEditorInterface >( ( QDesignerFormEditorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormEditorInterface;
   p->type = HBQT_TYPE_QDesignerFormEditorInterface;

   return p;
}

HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE )
{
   QDesignerFormEditorInterface * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QDesignerFormEditorInterface( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QDesignerFormEditorInterface() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( void * ) pObj, true ) );
}

/* QDesignerActionEditorInterface * actionEditor () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_ACTIONEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerActionEditorInterface( ( p )->actionEditor(), false ) );
}

/* QDesignerFormWindowManagerInterface * formWindowManager () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_FORMWINDOWMANAGER )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowManagerInterface( ( p )->formWindowManager(), false ) );
}

/* QDesignerObjectInspectorInterface * objectInspector () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_OBJECTINSPECTOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerObjectInspectorInterface( ( p )->objectInspector(), false ) );
}

/* QDesignerPropertyEditorInterface * propertyEditor () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_PROPERTYEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerPropertyEditorInterface( ( p )->propertyEditor(), false ) );
}

/* void setActionEditor ( QDesignerActionEditorInterface * actionEditor ) */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETACTIONEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setActionEditor( hbqt_par_QDesignerActionEditorInterface( 2 ) );
}

/* void setObjectInspector ( QDesignerObjectInspectorInterface * objectInspector ) */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETOBJECTINSPECTOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setObjectInspector( hbqt_par_QDesignerObjectInspectorInterface( 2 ) );
}

/* void setPropertyEditor ( QDesignerPropertyEditorInterface * propertyEditor ) */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETPROPERTYEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setPropertyEditor( hbqt_par_QDesignerPropertyEditorInterface( 2 ) );
}

/* void setWidgetBox ( QDesignerWidgetBoxInterface * widgetBox ) */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETWIDGETBOX )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setWidgetBox( hbqt_par_QDesignerWidgetBoxInterface( 2 ) );
}

/* QWidget * topLevel () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_TOPLEVEL )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevel(), false ) );
}

/* QDesignerWidgetBoxInterface * widgetBox () const */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_WIDGETBOX )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerWidgetBoxInterface( ( p )->widgetBox(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
