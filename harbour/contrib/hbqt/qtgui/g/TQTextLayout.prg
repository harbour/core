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


FUNCTION QTextLayout( ... )
   RETURN HB_QTextLayout():new( ... )

FUNCTION QTextLayoutFromPointer( ... )
   RETURN HB_QTextLayout():fromPointer( ... )


CREATE CLASS QTextLayout INHERIT HbQtObjectHandler FUNCTION HB_QTextLayout

   METHOD  new( ... )

   METHOD  beginLayout                   // (  )                                               -> NIL
   METHOD  boundingRect                  // (  )                                               -> oQRectF
   METHOD  cacheEnabled                  // (  )                                               -> lBool
   METHOD  clearAdditionalFormats        // (  )                                               -> NIL
   METHOD  clearLayout                   // (  )                                               -> NIL
   METHOD  createLine                    // (  )                                               -> oQTextLine
   METHOD  drawCursor                    // ( oQPainter, oQPointF, nCursorPosition, nWidth )   -> NIL
                                         // ( oQPainter, oQPointF, nCursorPosition )           -> NIL
   METHOD  endLayout                     // (  )                                               -> NIL
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  isValidCursorPosition         // ( nPos )                                           -> lBool
   METHOD  lineAt                        // ( nI )                                             -> oQTextLine
   METHOD  lineCount                     // (  )                                               -> nInt
   METHOD  lineForTextPosition           // ( nPos )                                           -> oQTextLine
   METHOD  maximumWidth                  // (  )                                               -> nQreal
   METHOD  minimumWidth                  // (  )                                               -> nQreal
   METHOD  nextCursorPosition            // ( nOldPos, nMode )                                 -> nInt
   METHOD  position                      // (  )                                               -> oQPointF
   METHOD  preeditAreaPosition           // (  )                                               -> nInt
   METHOD  preeditAreaText               // (  )                                               -> cQString
   METHOD  previousCursorPosition        // ( nOldPos, nMode )                                 -> nInt
   METHOD  setCacheEnabled               // ( lEnable )                                        -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setPosition                   // ( oQPointF )                                       -> NIL
   METHOD  setPreeditArea                // ( nPosition, cText )                               -> NIL
   METHOD  setText                       // ( cString )                                        -> NIL
   METHOD  setTextOption                 // ( oQTextOption )                                   -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textOption                    // (  )                                               -> oQTextOption

   ENDCLASS


METHOD QTextLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextLayout( ... )
   RETURN Self


METHOD QTextLayout:beginLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_beginLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QTextLayout_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:cacheEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_cacheEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:clearAdditionalFormats( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_clearAdditionalFormats( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:clearLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_clearLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:createLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextLineFromPointer( Qt_QTextLayout_createLine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:drawCursor( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTextLayout_drawCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTextLayout_drawCursor_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:endLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_endLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTextLayout_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:isValidCursorPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_isValidCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:lineAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextLineFromPointer( Qt_QTextLayout_lineAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:lineCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_lineCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:lineForTextPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextLineFromPointer( Qt_QTextLayout_lineForTextPosition( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:maximumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_maximumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:minimumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_minimumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:nextCursorPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextLayout_nextCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_nextCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QTextLayout_position( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:preeditAreaPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_preeditAreaPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:preeditAreaText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_preeditAreaText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:previousCursorPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextLayout_previousCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_previousCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setCacheEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_setCacheEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setPreeditArea( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTextLayout_setPreeditArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:setTextOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextLayout_setTextOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextLayout_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextLayout:textOption( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextOptionFromPointer( Qt_QTextLayout_textOption( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

