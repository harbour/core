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


FUNCTION QDesignerPropertyEditorInterface( ... )
   RETURN HB_QDesignerPropertyEditorInterface():new( ... )

FUNCTION QDesignerPropertyEditorInterfaceFromPointer( ... )
   RETURN HB_QDesignerPropertyEditorInterface():fromPointer( ... )


CREATE CLASS QDesignerPropertyEditorInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerPropertyEditorInterface

   METHOD  new( ... )

   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  currentPropertyName           // (  )                                               -> cQString
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  object                        // (  )                                               -> oQObject
   METHOD  setObject                     // ( oQObject )                                       -> NIL
   METHOD  setPropertyValue              // ( cName, oQVariant, lChanged )                     -> NIL
   METHOD  setReadOnly                   // ( lReadOnly )                                      -> NIL

   ENDCLASS


METHOD QDesignerPropertyEditorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerPropertyEditorInterface( ... )
   RETURN Self


METHOD QDesignerPropertyEditorInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerPropertyEditorInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:currentPropertyName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerPropertyEditorInterface_currentPropertyName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerPropertyEditorInterface_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:object( ... )
   SWITCH PCount()
   CASE 0
      RETURN QObjectFromPointer( Qt_QDesignerPropertyEditorInterface_object( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:setObject( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerPropertyEditorInterface_setObject( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:setPropertyValue( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) )
         RETURN Qt_QDesignerPropertyEditorInterface_setPropertyValue( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerPropertyEditorInterface_setPropertyValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerPropertyEditorInterface:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerPropertyEditorInterface_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

