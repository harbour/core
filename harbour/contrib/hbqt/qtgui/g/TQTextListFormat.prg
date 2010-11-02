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


FUNCTION QTextListFormat( ... )
   RETURN HB_QTextListFormat():new( ... )

FUNCTION QTextListFormatFromPointer( ... )
   RETURN HB_QTextListFormat():fromPointer( ... )


CREATE CLASS QTextListFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextListFormat

   METHOD  new( ... )

   METHOD  indent                        // (  )                                               -> nInt
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  setIndent                     // ( nIndentation )                                   -> NIL
   METHOD  setStyle                      // ( nStyle )                                         -> NIL
   METHOD  style                         // (  )                                               -> nStyle

   ENDCLASS


METHOD QTextListFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextListFormat( ... )
   RETURN Self


METHOD QTextListFormat:indent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextListFormat_indent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextListFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextListFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextListFormat:setIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextListFormat_setIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextListFormat:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextListFormat_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextListFormat:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextListFormat_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

