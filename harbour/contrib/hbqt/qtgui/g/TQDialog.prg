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


FUNCTION QDialog( ... )
   RETURN HB_QDialog():new( ... )

FUNCTION QDialogFromPointer( ... )
   RETURN HB_QDialog():fromPointer( ... )


CREATE CLASS QDialog INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDialog

   METHOD  new( ... )

   METHOD  isSizeGripEnabled             // (  )                                               -> lBool
   METHOD  result                        // (  )                                               -> nInt
   METHOD  setModal                      // ( lModal )                                         -> NIL
   METHOD  setResult                     // ( nI )                                             -> NIL
   METHOD  setSizeGripEnabled            // ( lBool )                                          -> NIL
   METHOD  accept                        // (  )                                               -> NIL
   METHOD  done                          // ( nR )                                             -> NIL
   METHOD  exec                          // (  )                                               -> nInt
   METHOD  open                          // (  )                                               -> NIL
   METHOD  reject                        // (  )                                               -> NIL

   ENDCLASS


METHOD QDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDialog( ... )
   RETURN Self


METHOD QDialog:isSizeGripEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_isSizeGripEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:result( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_result( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:setModal( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDialog_setModal( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:setResult( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDialog_setResult( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:setSizeGripEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDialog_setSizeGripEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:accept( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_accept( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:done( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDialog_done( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:open( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_open( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDialog:reject( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDialog_reject( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

