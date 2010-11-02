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


FUNCTION QIntValidator( ... )
   RETURN HB_QIntValidator():new( ... )

FUNCTION QIntValidatorFromPointer( ... )
   RETURN HB_QIntValidator():fromPointer( ... )


CREATE CLASS QIntValidator INHERIT HbQtObjectHandler, HB_QValidator FUNCTION HB_QIntValidator

   METHOD  new( ... )

   METHOD  bottom                        // (  )                                               -> nInt
   METHOD  setBottom                     // ( nInt )                                           -> NIL
   METHOD  setRange                      // ( nBottom, nTop )                                  -> NIL
   METHOD  setTop                        // ( nInt )                                           -> NIL
   METHOD  top                           // (  )                                               -> nInt

   ENDCLASS


METHOD QIntValidator:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIntValidator( ... )
   RETURN Self


METHOD QIntValidator:bottom( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIntValidator_bottom( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIntValidator:setBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIntValidator_setBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIntValidator:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIntValidator_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIntValidator:setTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIntValidator_setTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIntValidator:top( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIntValidator_top( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

