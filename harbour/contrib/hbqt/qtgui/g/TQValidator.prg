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


FUNCTION QValidator( ... )
   RETURN HB_QValidator():new( ... )

FUNCTION QValidatorFromPointer( ... )
   RETURN HB_QValidator():fromPointer( ... )


CREATE CLASS QValidator INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QValidator

   METHOD  new( ... )

   METHOD  locale                        // (  )                                               -> oQLocale
   METHOD  setLocale                     // ( oQLocale )                                       -> NIL

   ENDCLASS


METHOD QValidator:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QValidator( ... )
   RETURN Self


METHOD QValidator:locale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QValidator_locale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QValidator:setLocale( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QValidator_setLocale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

