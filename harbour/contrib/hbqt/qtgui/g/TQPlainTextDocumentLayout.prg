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


FUNCTION QPlainTextDocumentLayout( ... )
   RETURN HB_QPlainTextDocumentLayout():new( ... )

FUNCTION QPlainTextDocumentLayoutFromPointer( ... )
   RETURN HB_QPlainTextDocumentLayout():fromPointer( ... )


CREATE CLASS QPlainTextDocumentLayout INHERIT HbQtObjectHandler, HB_QAbstractTextDocumentLayout FUNCTION HB_QPlainTextDocumentLayout

   METHOD  new( ... )

   METHOD  cursorWidth                   // (  )                                               -> nInt
   METHOD  ensureBlockLayout             // ( oQTextBlock )                                    -> NIL
   METHOD  requestUpdate                 // (  )                                               -> NIL
   METHOD  setCursorWidth                // ( nWidth )                                         -> NIL

   ENDCLASS


METHOD QPlainTextDocumentLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPlainTextDocumentLayout( ... )
   RETURN Self


METHOD QPlainTextDocumentLayout:cursorWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextDocumentLayout_cursorWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextDocumentLayout:ensureBlockLayout( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextDocumentLayout_ensureBlockLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextDocumentLayout:requestUpdate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextDocumentLayout_requestUpdate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextDocumentLayout:setCursorWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextDocumentLayout_setCursorWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

