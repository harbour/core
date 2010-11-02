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


FUNCTION QFontComboBox( ... )
   RETURN HB_QFontComboBox():new( ... )

FUNCTION QFontComboBoxFromPointer( ... )
   RETURN HB_QFontComboBox():fromPointer( ... )


CREATE CLASS QFontComboBox INHERIT HbQtObjectHandler, HB_QComboBox FUNCTION HB_QFontComboBox

   METHOD  new( ... )

   METHOD  currentFont                   // (  )                                               -> oQFont
   METHOD  fontFilters                   // (  )                                               -> nFontFilters
   METHOD  setFontFilters                // ( nFilters )                                       -> NIL
   METHOD  setWritingSystem              // ( nScript )                                        -> NIL
   METHOD  writingSystem                 // (  )                                               -> nQFontDatabase_WritingSystem
   METHOD  setCurrentFont                // ( oQFont )                                         -> NIL

   ENDCLASS


METHOD QFontComboBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontComboBox( ... )
   RETURN Self


METHOD QFontComboBox:currentFont( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QFontComboBox_currentFont( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontComboBox:fontFilters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontComboBox_fontFilters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontComboBox:setFontFilters( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontComboBox_setFontFilters( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontComboBox:setWritingSystem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontComboBox_setWritingSystem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontComboBox:writingSystem( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontComboBox_writingSystem( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontComboBox:setCurrentFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontComboBox_setCurrentFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

