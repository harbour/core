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


FUNCTION QDesignerObjectInspectorInterface( ... )
   RETURN HB_QDesignerObjectInspectorInterface():new( ... )

FUNCTION QDesignerObjectInspectorInterfaceFromPointer( ... )
   RETURN HB_QDesignerObjectInspectorInterface():fromPointer( ... )


CREATE CLASS QDesignerObjectInspectorInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerObjectInspectorInterface

   METHOD  new( ... )

   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  setFormWindow                 // ( oQDesignerFormWindowInterface )                  -> NIL

   ENDCLASS


METHOD QDesignerObjectInspectorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerObjectInspectorInterface( ... )
   RETURN Self


METHOD QDesignerObjectInspectorInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerObjectInspectorInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerObjectInspectorInterface:setFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerObjectInspectorInterface_setFormWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

