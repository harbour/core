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


FUNCTION QFontMetrics( ... )
   RETURN HB_QFontMetrics():new( ... )

FUNCTION QFontMetricsFromPointer( ... )
   RETURN HB_QFontMetrics():fromPointer( ... )


CREATE CLASS QFontMetrics INHERIT HbQtObjectHandler FUNCTION HB_QFontMetrics

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nInt
   METHOD  averageCharWidth              // (  )                                               -> nInt
   METHOD  boundingRect                  // ( oQChar )                                         -> oQRect
                                         // ( cText )                                          -> oQRect
                                         // ( nX, nY, nWidth, nHeight, nFlags, cText, nTabStops, @nTabArray ) -> oQRect
                                         // ( oQRect, nFlags, cText, nTabStops, @nTabArray )   -> oQRect
   METHOD  descent                       // (  )                                               -> nInt
   METHOD  elidedText                    // ( cText, nMode, nWidth, nFlags )                   -> cQString
   METHOD  height                        // (  )                                               -> nInt
   METHOD  inFont                        // ( oQChar )                                         -> lBool
   METHOD  leading                       // (  )                                               -> nInt
   METHOD  leftBearing                   // ( oQChar )                                         -> nInt
   METHOD  lineSpacing                   // (  )                                               -> nInt
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  maxWidth                      // (  )                                               -> nInt
   METHOD  minLeftBearing                // (  )                                               -> nInt
   METHOD  minRightBearing               // (  )                                               -> nInt
   METHOD  overlinePos                   // (  )                                               -> nInt
   METHOD  rightBearing                  // ( oQChar )                                         -> nInt
   METHOD  size                          // ( nFlags, cText, nTabStops, @nTabArray )           -> oQSize
   METHOD  strikeOutPos                  // (  )                                               -> nInt
   METHOD  tightBoundingRect             // ( cText )                                          -> oQRect
   METHOD  underlinePos                  // (  )                                               -> nInt
   METHOD  width                         // ( cText, nLen )                                    -> nInt
                                         // ( oQChar )                                         -> nInt
   METHOD  xHeight                       // (  )                                               -> nInt

   ENDCLASS


METHOD QFontMetrics:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetrics( ... )
   RETURN Self


METHOD QFontMetrics:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:averageCharWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_averageCharWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:boundingRect( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:elidedText( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QFontMetrics_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFontMetrics_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:inFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_inFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:leading( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_leading( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:leftBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_leftBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:lineSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_lineSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:maxWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_maxWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:minLeftBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_minLeftBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:minRightBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_minRightBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:overlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_overlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:rightBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_rightBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:size( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:strikeOutPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_strikeOutPos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:tightBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_tightBoundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:underlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_underlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:width( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFontMetrics_width( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_width( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_width_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:xHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_xHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

