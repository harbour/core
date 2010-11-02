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


FUNCTION QDesignerWidgetBoxInterface( ... )
   RETURN HB_QDesignerWidgetBoxInterface():new( ... )

FUNCTION QDesignerWidgetBoxInterfaceFromPointer( ... )
   RETURN HB_QDesignerWidgetBoxInterface():fromPointer( ... )


CREATE CLASS QDesignerWidgetBoxInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerWidgetBoxInterface

   METHOD  new( ... )

   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  load                          // (  )                                               -> lBool
   METHOD  save                          // (  )                                               -> lBool
   METHOD  setFileName                   // ( cFileName )                                      -> NIL

   ENDCLASS


METHOD QDesignerWidgetBoxInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerWidgetBoxInterface( ... )
   RETURN Self


METHOD QDesignerWidgetBoxInterface:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerWidgetBoxInterface_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerWidgetBoxInterface:load( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerWidgetBoxInterface_load( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerWidgetBoxInterface:save( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerWidgetBoxInterface_save( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerWidgetBoxInterface:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerWidgetBoxInterface_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

