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


FUNCTION QTextLine( ... )
   RETURN HB_QTextLine():new( ... )

FUNCTION QTextLineFromPointer( ... )
   RETURN HB_QTextLine():fromPointer( ... )


CREATE CLASS QTextLine INHERIT HbQtObjectHandler FUNCTION HB_QTextLine

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nQreal
   METHOD  cursorToX                     // ( @nCursorPos, nEdge )                             -> nQreal
                                         // ( nCursorPos, nEdge )                              -> nQreal
   METHOD  descent                       // (  )                                               -> nQreal
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  lineNumber                    // (  )                                               -> nInt
   METHOD  naturalTextRect               // (  )                                               -> oQRectF
   METHOD  naturalTextWidth              // (  )                                               -> nQreal
   METHOD  position                      // (  )                                               -> oQPointF
   METHOD  rect                          // (  )                                               -> oQRectF
   METHOD  setLineWidth                  // ( nWidth )                                         -> NIL
   METHOD  setNumColumns                 // ( nNumColumns )                                    -> NIL
                                         // ( nNumColumns, nAlignmentWidth )                   -> NIL
   METHOD  setPosition                   // ( oQPointF )                                       -> NIL
   METHOD  textLength                    // (  )                                               -> nInt
   METHOD  textStart                     // (  )                                               -> nInt
   METHOD  width                         // (  )                                               -> nQreal
   METHOD  x                             // (  )                                               -> nQreal
   METHOD  xToCursor                     // ( nX, nCpos )                                      -> nInt
   METHOD  y                             // (  )                                               -> nQreal

   ENDCLASS


METHOD QTextLine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextLine( ... )
   RETURN Self


METHOD QTextLine:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:cursorToX( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextLine_cursorToX( ::pPtr, ... )
         // RETURN Qt_QTextLine_cursorToX_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLine_cursorToX( ::pPtr, ... )
         // RETURN Qt_QTextLine_cursorToX_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:lineNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_lineNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:naturalTextRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QTextLine_naturalTextRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:naturalTextWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_naturalTextWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QTextLine_position( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QTextLine_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:setLineWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLine_setLineWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:setNumColumns( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextLine_setNumColumns_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLine_setNumColumns( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:setPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextLine_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:textLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_textLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:textStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_textStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:xToCursor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextLine_xToCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLine_xToCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLine:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLine_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

