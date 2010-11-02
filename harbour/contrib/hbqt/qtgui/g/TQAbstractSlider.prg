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


FUNCTION QAbstractSlider( ... )
   RETURN HB_QAbstractSlider():new( ... )

FUNCTION QAbstractSliderFromPointer( ... )
   RETURN HB_QAbstractSlider():fromPointer( ... )


CREATE CLASS QAbstractSlider INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractSlider

   METHOD  new( ... )

   METHOD  hasTracking                   // (  )                                               -> lBool
   METHOD  invertedAppearance            // (  )                                               -> lBool
   METHOD  invertedControls              // (  )                                               -> lBool
   METHOD  isSliderDown                  // (  )                                               -> lBool
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  pageStep                      // (  )                                               -> nInt
   METHOD  setInvertedAppearance         // ( lBool )                                          -> NIL
   METHOD  setInvertedControls           // ( lBool )                                          -> NIL
   METHOD  setMaximum                    // ( nInt )                                           -> NIL
   METHOD  setMinimum                    // ( nInt )                                           -> NIL
   METHOD  setPageStep                   // ( nInt )                                           -> NIL
   METHOD  setRange                      // ( nMin, nMax )                                     -> NIL
   METHOD  setSingleStep                 // ( nInt )                                           -> NIL
   METHOD  setSliderDown                 // ( lBool )                                          -> NIL
   METHOD  setSliderPosition             // ( nInt )                                           -> NIL
   METHOD  setTracking                   // ( lEnable )                                        -> NIL
   METHOD  singleStep                    // (  )                                               -> nInt
   METHOD  sliderPosition                // (  )                                               -> nInt
   METHOD  triggerAction                 // ( nAction )                                        -> NIL
   METHOD  value                         // (  )                                               -> nInt
   METHOD  setOrientation                // ( nQt::Orientation )                               -> NIL
   METHOD  setValue                      // ( nInt )                                           -> NIL

   ENDCLASS


METHOD QAbstractSlider:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractSlider( ... )
   RETURN Self


METHOD QAbstractSlider:hasTracking( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_hasTracking( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:invertedAppearance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_invertedAppearance( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:invertedControls( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_invertedControls( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:isSliderDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_isSliderDown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:pageStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_pageStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setInvertedAppearance( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setInvertedAppearance( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setInvertedControls( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setInvertedControls( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setPageStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setPageStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractSlider_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setSingleStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSingleStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setSliderDown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSliderDown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setSliderPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSliderPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setTracking( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setTracking( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:singleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_singleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:sliderPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_sliderPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:triggerAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_triggerAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractSlider:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

