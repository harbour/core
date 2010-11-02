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


#include "hbclass.ch"


REQUEST __HBQTDESIGNER


FUNCTION QDesignerFormEditorInterface( ... )
   RETURN HB_QDesignerFormEditorInterface():new( ... )

FUNCTION QDesignerFormEditorInterfaceFromPointer( ... )
   RETURN HB_QDesignerFormEditorInterface():fromPointer( ... )


CREATE CLASS QDesignerFormEditorInterface INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QDesignerFormEditorInterface

   METHOD  new( ... )

   METHOD  actionEditor                  // (  )                                               -> oQDesignerActionEditorInterface
   METHOD  formWindowManager             // (  )                                               -> oQDesignerFormWindowManagerInterface
   METHOD  objectInspector               // (  )                                               -> oQDesignerObjectInspectorInterface
   METHOD  propertyEditor                // (  )                                               -> oQDesignerPropertyEditorInterface
   METHOD  setActionEditor               // ( oQDesignerActionEditorInterface )                -> NIL
   METHOD  setObjectInspector            // ( oQDesignerObjectInspectorInterface )             -> NIL
   METHOD  setPropertyEditor             // ( oQDesignerPropertyEditorInterface )              -> NIL
   METHOD  setWidgetBox                  // ( oQDesignerWidgetBoxInterface )                   -> NIL
   METHOD  topLevel                      // (  )                                               -> oQWidget
   METHOD  widgetBox                     // (  )                                               -> oQDesignerWidgetBoxInterface

   ENDCLASS


METHOD QDesignerFormEditorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormEditorInterface( ... )
   RETURN Self


METHOD QDesignerFormEditorInterface:actionEditor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerActionEditorInterfaceFromPointer( Qt_QDesignerFormEditorInterface_actionEditor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:formWindowManager( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormWindowManagerInterfaceFromPointer( Qt_QDesignerFormEditorInterface_formWindowManager( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:objectInspector( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerObjectInspectorInterfaceFromPointer( Qt_QDesignerFormEditorInterface_objectInspector( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:propertyEditor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerPropertyEditorInterfaceFromPointer( Qt_QDesignerFormEditorInterface_propertyEditor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:setActionEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormEditorInterface_setActionEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:setObjectInspector( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormEditorInterface_setObjectInspector( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:setPropertyEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormEditorInterface_setPropertyEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:setWidgetBox( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormEditorInterface_setWidgetBox( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:topLevel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDesignerFormEditorInterface_topLevel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormEditorInterface:widgetBox( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerWidgetBoxInterfaceFromPointer( Qt_QDesignerFormEditorInterface_widgetBox( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

