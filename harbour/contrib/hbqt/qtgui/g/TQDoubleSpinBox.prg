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


FUNCTION QDoubleSpinBox( ... )
   RETURN HB_QDoubleSpinBox():new( ... )

FUNCTION QDoubleSpinBoxFromPointer( ... )
   RETURN HB_QDoubleSpinBox():fromPointer( ... )


CREATE CLASS QDoubleSpinBox INHERIT HbQtObjectHandler, HB_QAbstractSpinBox FUNCTION HB_QDoubleSpinBox

   METHOD  new( ... )

   METHOD  cleanText                     // (  )                                               -> cQString
   METHOD  decimals                      // (  )                                               -> nInt
   METHOD  maximum                       // (  )                                               -> nDouble
   METHOD  minimum                       // (  )                                               -> nDouble
   METHOD  prefix                        // (  )                                               -> cQString
   METHOD  setDecimals                   // ( nPrec )                                          -> NIL
   METHOD  setMaximum                    // ( nMax )                                           -> NIL
   METHOD  setMinimum                    // ( nMin )                                           -> NIL
   METHOD  setPrefix                     // ( cPrefix )                                        -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum )                             -> NIL
   METHOD  setSingleStep                 // ( nVal )                                           -> NIL
   METHOD  setSuffix                     // ( cSuffix )                                        -> NIL
   METHOD  singleStep                    // (  )                                               -> nDouble
   METHOD  suffix                        // (  )                                               -> cQString
   METHOD  textFromValue                 // ( nValue )                                         -> cQString
   METHOD  value                         // (  )                                               -> nDouble
   METHOD  valueFromText                 // ( cText )                                          -> nDouble
   METHOD  setValue                      // ( nVal )                                           -> NIL

   ENDCLASS


METHOD QDoubleSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDoubleSpinBox( ... )
   RETURN Self


METHOD QDoubleSpinBox:cleanText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_cleanText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:decimals( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_decimals( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:prefix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_prefix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setDecimals( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setDecimals( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setPrefix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setPrefix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDoubleSpinBox_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setSingleStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setSingleStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setSuffix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setSuffix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:singleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_singleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:suffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_suffix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:textFromValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_textFromValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleSpinBox_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:valueFromText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_valueFromText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleSpinBox:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleSpinBox_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

