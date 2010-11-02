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


FUNCTION QTextFrameFormat( ... )
   RETURN HB_QTextFrameFormat():new( ... )

FUNCTION QTextFrameFormatFromPointer( ... )
   RETURN HB_QTextFrameFormat():fromPointer( ... )


CREATE CLASS QTextFrameFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextFrameFormat

   METHOD  new( ... )

   METHOD  border                        // (  )                                               -> nQreal
   METHOD  borderBrush                   // (  )                                               -> oQBrush
   METHOD  borderStyle                   // (  )                                               -> nBorderStyle
   METHOD  bottomMargin                  // (  )                                               -> nQreal
   METHOD  height                        // (  )                                               -> oQTextLength
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  leftMargin                    // (  )                                               -> nQreal
   METHOD  margin                        // (  )                                               -> nQreal
   METHOD  padding                       // (  )                                               -> nQreal
   METHOD  pageBreakPolicy               // (  )                                               -> nPageBreakFlags
   METHOD  position                      // (  )                                               -> nPosition
   METHOD  rightMargin                   // (  )                                               -> nQreal
   METHOD  setBorder                     // ( nWidth )                                         -> NIL
   METHOD  setBorderBrush                // ( oQBrush )                                        -> NIL
   METHOD  setBorderStyle                // ( nStyle )                                         -> NIL
   METHOD  setBottomMargin               // ( nMargin )                                        -> NIL
   METHOD  setHeight                     // ( oQTextLength )                                   -> NIL
                                         // ( nHeight )                                        -> NIL
   METHOD  setLeftMargin                 // ( nMargin )                                        -> NIL
   METHOD  setMargin                     // ( nMargin )                                        -> NIL
   METHOD  setPadding                    // ( nWidth )                                         -> NIL
   METHOD  setPageBreakPolicy            // ( nPolicy )                                        -> NIL
   METHOD  setPosition                   // ( nPolicy )                                        -> NIL
   METHOD  setRightMargin                // ( nMargin )                                        -> NIL
   METHOD  setTopMargin                  // ( nMargin )                                        -> NIL
   METHOD  setWidth                      // ( oQTextLength )                                   -> NIL
                                         // ( nWidth )                                         -> NIL
   METHOD  topMargin                     // (  )                                               -> nQreal
   METHOD  width                         // (  )                                               -> oQTextLength

   ENDCLASS


METHOD QTextFrameFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFrameFormat( ... )
   RETURN Self


METHOD QTextFrameFormat:border( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_border( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:borderBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTextFrameFormat_borderBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:borderStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_borderStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:bottomMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_bottomMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextLengthFromPointer( Qt_QTextFrameFormat_height( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:leftMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_leftMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:margin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_margin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:padding( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_padding( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:pageBreakPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_pageBreakPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:rightMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_rightMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setBorder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setBorder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setBorderBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setBorderBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setBorderStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setBorderStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setBottomMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setBottomMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setHeight_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setLeftMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setLeftMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setPadding( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setPadding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setPageBreakPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setPageBreakPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setRightMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setRightMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setTopMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setTopMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setWidth_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrameFormat_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:topMargin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrameFormat_topMargin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrameFormat:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextLengthFromPointer( Qt_QTextFrameFormat_width( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

