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


REQUEST __HBQTGUI


FUNCTION QGroupBox( ... )
   RETURN HB_QGroupBox():new( ... )

FUNCTION QGroupBoxFromPointer( ... )
   RETURN HB_QGroupBox():fromPointer( ... )


CREATE CLASS QGroupBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QGroupBox

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  isCheckable                   // (  )                                               -> lBool
   METHOD  isChecked                     // (  )                                               -> lBool
   METHOD  isFlat                        // (  )                                               -> lBool
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setCheckable                  // ( lCheckable )                                     -> NIL
   METHOD  setFlat                       // ( lFlat )                                          -> NIL
   METHOD  setTitle                      // ( cTitle )                                         -> NIL
   METHOD  title                         // (  )                                               -> cQString
   METHOD  setChecked                    // ( lChecked )                                       -> NIL

   ENDCLASS


METHOD QGroupBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGroupBox( ... )
   RETURN Self


METHOD QGroupBox:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGroupBox_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:isCheckable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGroupBox_isCheckable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:isChecked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGroupBox_isChecked( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:isFlat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGroupBox_isFlat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGroupBox_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:setCheckable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGroupBox_setCheckable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:setFlat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGroupBox_setFlat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:setTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QGroupBox_setTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGroupBox_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGroupBox:setChecked( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGroupBox_setChecked( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

