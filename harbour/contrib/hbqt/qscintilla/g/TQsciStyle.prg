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


REQUEST __HBQSCINTILLA


FUNCTION QsciStyle( ... )
   RETURN HB_QsciStyle():new( ... )

FUNCTION QsciStyleFromPointer( ... )
   RETURN HB_QsciStyle():fromPointer( ... )


CREATE CLASS QsciStyle INHERIT HbQtObjectHandler FUNCTION HB_QsciStyle

   METHOD  new( ... )

   METHOD  style                         // (  )                                               -> nInt
   METHOD  setDescription                // ( cDescription )                                   -> NIL
   METHOD  description                   // (  )                                               -> cQString
   METHOD  setColor                      // ( oQColor )                                        -> NIL
   METHOD  color                         // (  )                                               -> oQColor
   METHOD  setPaper                      // ( oQColor )                                        -> NIL
   METHOD  paper                         // (  )                                               -> oQColor
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  setEolFill                    // ( lFill )                                          -> NIL
   METHOD  eolFill                       // (  )                                               -> lBool
   METHOD  setTextCase                   // ( nText_case )                                     -> NIL
   METHOD  textCase                      // (  )                                               -> nTextCase
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  visible                       // (  )                                               -> lBool
   METHOD  setChangeable                 // ( lChangeable )                                    -> NIL
   METHOD  changeable                    // (  )                                               -> lBool
   METHOD  setHotspot                    // ( lHotspot )                                       -> NIL
   METHOD  hotspot                       // (  )                                               -> lBool
   METHOD  refresh                       // (  )                                               -> NIL

   ENDCLASS


METHOD QsciStyle:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciStyle( ... )
   RETURN Self


METHOD QsciStyle:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setDescription( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setDescription( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:description( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_description( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:color( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QsciStyle_color( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setPaper( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:paper( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QsciStyle_paper( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QsciStyle_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:eolFill( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_eolFill( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setTextCase( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setTextCase( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:textCase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_textCase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:visible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_visible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setChangeable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setChangeable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:changeable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_changeable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:setHotspot( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciStyle_setHotspot( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:hotspot( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_hotspot( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciStyle:refresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciStyle_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

