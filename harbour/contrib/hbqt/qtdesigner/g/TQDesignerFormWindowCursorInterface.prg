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


FUNCTION QDesignerFormWindowCursorInterface( ... )
   RETURN HB_QDesignerFormWindowCursorInterface():new( ... )

FUNCTION QDesignerFormWindowCursorInterfaceFromPointer( ... )
   RETURN HB_QDesignerFormWindowCursorInterface():fromPointer( ... )


CREATE CLASS QDesignerFormWindowCursorInterface INHERIT HbQtObjectHandler FUNCTION HB_QDesignerFormWindowCursorInterface

   METHOD  new( ... )

   METHOD  current                       // (  )                                               -> oQWidget
   METHOD  formWindow                    // (  )                                               -> oQDesignerFormWindowInterface
   METHOD  hasSelection                  // (  )                                               -> lBool
   METHOD  isWidgetSelected              // ( oQWidget )                                       -> lBool
   METHOD  movePosition                  // ( nOperation, nMode )                              -> lBool
   METHOD  position                      // (  )                                               -> nInt
   METHOD  resetWidgetProperty           // ( oQWidget, cName )                                -> NIL
   METHOD  selectedWidget                // ( nIndex )                                         -> oQWidget
   METHOD  selectedWidgetCount           // (  )                                               -> nInt
   METHOD  setPosition                   // ( nPosition, nMode )                               -> NIL
   METHOD  setProperty                   // ( cName, oQVariant )                               -> NIL
   METHOD  setWidgetProperty             // ( oQWidget, cName, oQVariant )                     -> NIL
   METHOD  widget                        // ( nIndex )                                         -> oQWidget
   METHOD  widgetCount                   // (  )                                               -> nInt

   ENDCLASS


METHOD QDesignerFormWindowCursorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowCursorInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowCursorInterface:current( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDesignerFormWindowCursorInterface_current( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:formWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowCursorInterface_formWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:hasSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowCursorInterface_hasSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:isWidgetSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_isWidgetSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:movePosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowCursorInterface_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:resetWidgetProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_resetWidgetProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:selectedWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QDesignerFormWindowCursorInterface_selectedWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:selectedWidgetCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowCursorInterface_selectedWidgetCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:setPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:setProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_setProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:setWidgetProperty( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QDesignerFormWindowCursorInterface_setWidgetProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QDesignerFormWindowCursorInterface_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowCursorInterface:widgetCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowCursorInterface_widgetCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

