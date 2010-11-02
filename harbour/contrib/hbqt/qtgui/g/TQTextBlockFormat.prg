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


FUNCTION QTextBlockFormat( ... )
   RETURN HB_QTextBlockFormat():new( ... )

FUNCTION QTextBlockFormatFromPointer( ... )
   RETURN HB_QTextBlockFormat():fromPointer( ... )


CREATE CLASS QTextBlockFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextBlockFormat

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  bottomMargin                  // (  )                                               -> nQreal
   METHOD  indent                        // (  )                                               -> nInt
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  leftMargin                    // (  )                                               -> nQreal
   METHOD  nonBreakableLines             // (  )                                               -> lBool
   METHOD  pageBreakPolicy               // (  )                                               -> nPageBreakFlags
   METHOD  rightMargin                   // (  )                                               -> nQreal
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setBottomMargin               // ( nMargin )                                        -> NIL
   METHOD  setIndent                     // ( nIndentation )                                   -> NIL
   METHOD  setLeftMargin                 // ( nMargin )                                        -> NIL
   METHOD  setNonBreakableLines          // ( lB )                                             -> NIL
   METHOD  setPageBreakPolicy            // ( nPolicy )                                        -> NIL
   METHOD  setRightMargin                // ( nMargin )                                        -> NIL
   METHOD  setTextIndent                 // ( nIndent )                                        -> NIL
   METHOD  setTopMargin                  // ( nMargin )                                        -> NIL
   METHOD  textIndent                    // (  )                                               -> nQreal
   METHOD  topMargin                     // (  )                                               -> nQreal

   ENDCLASS


METHOD QTextBlockFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBlockFormat( ... )
   RETURN Self


METHOD QTextBlockFormat:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:bottomMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_bottomMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:indent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_indent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:leftMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_leftMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:nonBreakableLines( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_nonBreakableLines( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:pageBreakPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_pageBreakPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:rightMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_rightMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setBottomMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setBottomMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setLeftMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setLeftMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setNonBreakableLines( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setNonBreakableLines( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setPageBreakPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setPageBreakPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setRightMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setRightMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setTextIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setTextIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:setTopMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlockFormat_setTopMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:textIndent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_textIndent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlockFormat:topMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlockFormat_topMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

