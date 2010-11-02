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


FUNCTION QSpinBox( ... )
   RETURN HB_QSpinBox():new( ... )

FUNCTION QSpinBoxFromPointer( ... )
   RETURN HB_QSpinBox():fromPointer( ... )


CREATE CLASS QSpinBox INHERIT HbQtObjectHandler, HB_QAbstractSpinBox FUNCTION HB_QSpinBox

   METHOD  new( ... )

   METHOD  cleanText                     // (  )                                               -> cQString
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  prefix                        // (  )                                               -> cQString
   METHOD  setMaximum                    // ( nMax )                                           -> NIL
   METHOD  setMinimum                    // ( nMin )                                           -> NIL
   METHOD  setPrefix                     // ( cPrefix )                                        -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum )                             -> NIL
   METHOD  setSingleStep                 // ( nVal )                                           -> NIL
   METHOD  setSuffix                     // ( cSuffix )                                        -> NIL
   METHOD  singleStep                    // (  )                                               -> nInt
   METHOD  suffix                        // (  )                                               -> cQString
   METHOD  value                         // (  )                                               -> nInt
   METHOD  setValue                      // ( nVal )                                           -> NIL

   ENDCLASS


METHOD QSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSpinBox( ... )
   RETURN Self


METHOD QSpinBox:cleanText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_cleanText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:prefix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_prefix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setPrefix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setPrefix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSpinBox_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setSingleStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setSingleStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setSuffix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setSuffix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:singleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_singleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:suffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_suffix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpinBox_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpinBox:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSpinBox_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

