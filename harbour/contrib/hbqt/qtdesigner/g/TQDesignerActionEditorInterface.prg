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


FUNCTION QDesignerActionEditorInterface( ... )
   RETURN HB_QDesignerActionEditorInterface():new( ... )

FUNCTION QDesignerActionEditorInterfaceFromPointer( ... )
   RETURN HB_QDesignerActionEditorInterface():fromPointer( ... )


CREATE CLASS QDesignerActionEditorInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerActionEditorInterface

   METHOD  new( ... )

   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  manageAction                  // ( oQAction )                                       -> NIL
   METHOD  unmanageAction                // ( oQAction )                                       -> NIL
   METHOD  setFormWindow                 // ( oQDesignerFormWindowInterface )                  -> NIL

   ENDCLASS


METHOD QDesignerActionEditorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerActionEditorInterface( ... )
   RETURN Self


METHOD QDesignerActionEditorInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerActionEditorInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerActionEditorInterface:manageAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerActionEditorInterface_manageAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerActionEditorInterface:unmanageAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerActionEditorInterface_unmanageAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerActionEditorInterface:setFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerActionEditorInterface_setFormWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

