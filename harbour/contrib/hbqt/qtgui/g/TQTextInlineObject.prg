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


FUNCTION QTextInlineObject( ... )
   RETURN HB_QTextInlineObject():new( ... )

FUNCTION QTextInlineObjectFromPointer( ... )
   RETURN HB_QTextInlineObject():fromPointer( ... )


CREATE CLASS QTextInlineObject INHERIT HbQtObjectHandler FUNCTION HB_QTextInlineObject

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nQreal
   METHOD  descent                       // (  )                                               -> nQreal
   METHOD  format                        // (  )                                               -> oQTextFormat
   METHOD  formatIndex                   // (  )                                               -> nInt
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  rect                          // (  )                                               -> oQRectF
   METHOD  setAscent                     // ( nA )                                             -> NIL
   METHOD  setDescent                    // ( nD )                                             -> NIL
   METHOD  setWidth                      // ( nW )                                             -> NIL
   METHOD  textDirection                 // (  )                                               -> nQt_LayoutDirection
   METHOD  textPosition                  // (  )                                               -> nInt
   METHOD  width                         // (  )                                               -> nQreal

   ENDCLASS


METHOD QTextInlineObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextInlineObject( ... )
   RETURN Self


METHOD QTextInlineObject:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFormatFromPointer( Qt_QTextInlineObject_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:formatIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_formatIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QTextInlineObject_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:setAscent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextInlineObject_setAscent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:setDescent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextInlineObject_setDescent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextInlineObject_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:textDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_textDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:textPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_textPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextInlineObject:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextInlineObject_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

