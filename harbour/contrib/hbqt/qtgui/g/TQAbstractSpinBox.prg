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


FUNCTION QAbstractSpinBox( ... )
   RETURN HB_QAbstractSpinBox():new( ... )

FUNCTION QAbstractSpinBoxFromPointer( ... )
   RETURN HB_QAbstractSpinBox():fromPointer( ... )


CREATE CLASS QAbstractSpinBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractSpinBox

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  buttonSymbols                 // (  )                                               -> nButtonSymbols
   METHOD  correctionMode                // (  )                                               -> nCorrectionMode
   METHOD  hasAcceptableInput            // (  )                                               -> lBool
   METHOD  hasFrame                      // (  )                                               -> lBool
   METHOD  interpretText                 // (  )                                               -> NIL
   METHOD  isAccelerated                 // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  keyboardTracking              // (  )                                               -> lBool
   METHOD  setAccelerated                // ( lOn )                                            -> NIL
   METHOD  setAlignment                  // ( nFlag )                                          -> NIL
   METHOD  setButtonSymbols              // ( nBs )                                            -> NIL
   METHOD  setCorrectionMode             // ( nCm )                                            -> NIL
   METHOD  setFrame                      // ( lBool )                                          -> NIL
   METHOD  setKeyboardTracking           // ( lKt )                                            -> NIL
   METHOD  setReadOnly                   // ( lR )                                             -> NIL
   METHOD  setSpecialValueText           // ( cTxt )                                           -> NIL
   METHOD  setWrapping                   // ( lW )                                             -> NIL
   METHOD  specialValueText              // (  )                                               -> cQString
   METHOD  stepBy                        // ( nSteps )                                         -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  wrapping                      // (  )                                               -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  stepDown                      // (  )                                               -> NIL
   METHOD  stepUp                        // (  )                                               -> NIL

   ENDCLASS


METHOD QAbstractSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractSpinBox( ... )
   RETURN Self


METHOD QAbstractSpinBox:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:buttonSymbols( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_buttonSymbols( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:correctionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_correctionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:hasAcceptableInput( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_hasAcceptableInput( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:hasFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_hasFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:interpretText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_interpretText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:isAccelerated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_isAccelerated( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:keyboardTracking( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_keyboardTracking( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setAccelerated( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setAccelerated( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setButtonSymbols( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setButtonSymbols( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setCorrectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setCorrectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setKeyboardTracking( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setKeyboardTracking( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setSpecialValueText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setSpecialValueText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:setWrapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setWrapping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:specialValueText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_specialValueText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:stepBy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_stepBy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:wrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_wrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:stepDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_stepDown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSpinBox:stepUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_stepUp( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

