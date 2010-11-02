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


FUNCTION QTextImageFormat( ... )
   RETURN HB_QTextImageFormat():new( ... )

FUNCTION QTextImageFormatFromPointer( ... )
   RETURN HB_QTextImageFormat():fromPointer( ... )


CREATE CLASS QTextImageFormat INHERIT HbQtObjectHandler, HB_QTextCharFormat FUNCTION HB_QTextImageFormat

   METHOD  new( ... )

   METHOD  height                        // (  )                                               -> nQreal
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  name                          // (  )                                               -> cQString
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setName                       // ( cName )                                          -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  width                         // (  )                                               -> nQreal

   ENDCLASS


METHOD QTextImageFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextImageFormat( ... )
   RETURN Self


METHOD QTextImageFormat:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextImageFormat_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextImageFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextImageFormat_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextImageFormat_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:setName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextImageFormat_setName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextImageFormat_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextImageFormat:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextImageFormat_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

