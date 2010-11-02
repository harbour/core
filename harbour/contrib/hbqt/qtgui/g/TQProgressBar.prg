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


FUNCTION QProgressBar( ... )
   RETURN HB_QProgressBar():new( ... )

FUNCTION QProgressBarFromPointer( ... )
   RETURN HB_QProgressBar():fromPointer( ... )


CREATE CLASS QProgressBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QProgressBar

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  format                        // (  )                                               -> cQString
   METHOD  invertedAppearance            // (  )                                               -> lBool
   METHOD  isTextVisible                 // (  )                                               -> lBool
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setFormat                     // ( cFormat )                                        -> NIL
   METHOD  setInvertedAppearance         // ( lInvert )                                        -> NIL
   METHOD  setTextDirection              // ( nTextDirection )                                 -> NIL
   METHOD  setTextVisible                // ( lVisible )                                       -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textDirection                 // (  )                                               -> nQProgressBar_Direction
   METHOD  value                         // (  )                                               -> nInt
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  setMaximum                    // ( nMaximum )                                       -> NIL
   METHOD  setMinimum                    // ( nMinimum )                                       -> NIL
   METHOD  setOrientation                // ( nQt::Orientation )                               -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum )                             -> NIL
   METHOD  setValue                      // ( nValue )                                         -> NIL

   ENDCLASS


METHOD QProgressBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProgressBar( ... )
   RETURN Self


METHOD QProgressBar:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_format( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:invertedAppearance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_invertedAppearance( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:isTextVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_isTextVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setInvertedAppearance( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setInvertedAppearance( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setTextDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setTextDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setTextVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setTextVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:textDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_textDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressBar_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProgressBar_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressBar:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressBar_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

