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


FUNCTION QCheckBox( ... )
   RETURN HB_QCheckBox():new( ... )

FUNCTION QCheckBoxFromPointer( ... )
   RETURN HB_QCheckBox():fromPointer( ... )


CREATE CLASS QCheckBox INHERIT HbQtObjectHandler, HB_QAbstractButton FUNCTION HB_QCheckBox

   METHOD  new( ... )

   METHOD  checkState                    // (  )                                               -> nQt_CheckState
   METHOD  isTristate                    // (  )                                               -> lBool
   METHOD  setCheckState                 // ( nState )                                         -> NIL
   METHOD  setTristate                   // ( lY )                                             -> NIL

   ENDCLASS


METHOD QCheckBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCheckBox( ... )
   RETURN Self


METHOD QCheckBox:checkState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCheckBox_checkState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCheckBox:isTristate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCheckBox_isTristate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCheckBox:setCheckState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCheckBox_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCheckBox:setTristate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCheckBox_setTristate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QCheckBox_setTristate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

