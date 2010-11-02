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


FUNCTION QFrame( ... )
   RETURN HB_QFrame():new( ... )

FUNCTION QFrameFromPointer( ... )
   RETURN HB_QFrame():fromPointer( ... )


CREATE CLASS QFrame INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QFrame

   METHOD  new( ... )

   METHOD  frameRect                     // (  )                                               -> oQRect
   METHOD  frameShadow                   // (  )                                               -> nShadow
   METHOD  frameShape                    // (  )                                               -> nShape
   METHOD  frameStyle                    // (  )                                               -> nInt
   METHOD  frameWidth                    // (  )                                               -> nInt
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt
   METHOD  setFrameRect                  // ( oQRect )                                         -> NIL
   METHOD  setFrameShadow                // ( nShadow )                                        -> NIL
   METHOD  setFrameShape                 // ( nShape )                                         -> NIL
   METHOD  setFrameStyle                 // ( nStyle )                                         -> NIL
   METHOD  setLineWidth                  // ( nInt )                                           -> NIL
   METHOD  setMidLineWidth               // ( nInt )                                           -> NIL

   ENDCLASS


METHOD QFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFrame( ... )
   RETURN Self


METHOD QFrame:frameRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QFrame_frameRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameShadow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameShadow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:frameWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_frameWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFrame_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameShadow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameShadow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setFrameStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setFrameStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setLineWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setLineWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFrame:setMidLineWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFrame_setMidLineWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

