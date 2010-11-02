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


FUNCTION QProgressDialog( ... )
   RETURN HB_QProgressDialog():new( ... )

FUNCTION QProgressDialogFromPointer( ... )
   RETURN HB_QProgressDialog():fromPointer( ... )


CREATE CLASS QProgressDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QProgressDialog

   METHOD  new( ... )

   METHOD  autoClose                     // (  )                                               -> lBool
   METHOD  autoReset                     // (  )                                               -> lBool
   METHOD  labelText                     // (  )                                               -> cQString
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  minimumDuration               // (  )                                               -> nInt
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  setAutoClose                  // ( lClose )                                         -> NIL
   METHOD  setAutoReset                  // ( lReset )                                         -> NIL
   METHOD  setBar                        // ( oQProgressBar )                                  -> NIL
   METHOD  setCancelButton               // ( oQPushButton )                                   -> NIL
   METHOD  setLabel                      // ( oQLabel )                                        -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  value                         // (  )                                               -> nInt
   METHOD  wasCanceled                   // (  )                                               -> lBool
   METHOD  cancel                        // (  )                                               -> NIL
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  setCancelButtonText           // ( cCancelButtonText )                              -> NIL
   METHOD  setLabelText                  // ( cText )                                          -> NIL
   METHOD  setMaximum                    // ( nMaximum )                                       -> NIL
   METHOD  setMinimum                    // ( nMinimum )                                       -> NIL
   METHOD  setMinimumDuration            // ( nMs )                                            -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum )                             -> NIL
   METHOD  setValue                      // ( nProgress )                                      -> NIL

   ENDCLASS


METHOD QProgressDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProgressDialog( ... )
   RETURN Self


METHOD QProgressDialog:autoClose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_autoClose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:autoReset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_autoReset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:labelText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_labelText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:minimumDuration( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_minimumDuration( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QProgressDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setAutoClose( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setAutoClose( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setAutoReset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setAutoReset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setCancelButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setCancelButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setLabel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setLabel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QProgressDialog_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:wasCanceled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_wasCanceled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:cancel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_cancel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setCancelButtonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setCancelButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setLabelText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setLabelText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMinimumDuration( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMinimumDuration( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProgressDialog_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

