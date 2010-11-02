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


FUNCTION QRegExpValidator( ... )
   RETURN HB_QRegExpValidator():new( ... )

FUNCTION QRegExpValidatorFromPointer( ... )
   RETURN HB_QRegExpValidator():fromPointer( ... )


CREATE CLASS QRegExpValidator INHERIT HbQtObjectHandler, HB_QValidator FUNCTION HB_QRegExpValidator

   METHOD  new( ... )

   METHOD  regExp                        // (  )                                               -> oQRegExp
   METHOD  setRegExp                     // ( oQRegExp )                                       -> NIL

   ENDCLASS


METHOD QRegExpValidator:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegExpValidator( ... )
   RETURN Self


METHOD QRegExpValidator:regExp( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegExpFromPointer( Qt_QRegExpValidator_regExp( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExpValidator:setRegExp( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRegExpValidator_setRegExp( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

