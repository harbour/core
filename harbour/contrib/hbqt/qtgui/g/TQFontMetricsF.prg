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


FUNCTION QFontMetricsF( ... )
   RETURN HB_QFontMetricsF():new( ... )

FUNCTION QFontMetricsFFromPointer( ... )
   RETURN HB_QFontMetricsF():fromPointer( ... )


CREATE CLASS QFontMetricsF INHERIT HbQtObjectHandler FUNCTION HB_QFontMetricsF

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nQreal
   METHOD  averageCharWidth              // (  )                                               -> nQreal
   METHOD  boundingRect                  // ( cText )                                          -> oQRectF
                                         // ( oQChar )                                         -> oQRectF
                                         // ( oQRectF, nFlags, cText, nTabStops, @nTabArray )  -> oQRectF
   METHOD  descent                       // (  )                                               -> nQreal
   METHOD  elidedText                    // ( cText, nMode, nWidth, nFlags )                   -> cQString
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  inFont                        // ( oQChar )                                         -> lBool
   METHOD  leading                       // (  )                                               -> nQreal
   METHOD  leftBearing                   // ( oQChar )                                         -> nQreal
   METHOD  lineSpacing                   // (  )                                               -> nQreal
   METHOD  lineWidth                     // (  )                                               -> nQreal
   METHOD  maxWidth                      // (  )                                               -> nQreal
   METHOD  minLeftBearing                // (  )                                               -> nQreal
   METHOD  minRightBearing               // (  )                                               -> nQreal
   METHOD  overlinePos                   // (  )                                               -> nQreal
   METHOD  rightBearing                  // ( oQChar )                                         -> nQreal
   METHOD  size                          // ( nFlags, cText, nTabStops, @nTabArray )           -> oQSizeF
   METHOD  strikeOutPos                  // (  )                                               -> nQreal
   METHOD  tightBoundingRect             // ( cText )                                          -> oQRectF
   METHOD  underlinePos                  // (  )                                               -> nQreal
   METHOD  width                         // ( cText )                                          -> nQreal
                                         // ( oQChar )                                         -> nQreal
   METHOD  xHeight                       // (  )                                               -> nQreal

   ENDCLASS


METHOD QFontMetricsF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetricsF( ... )
   RETURN Self


METHOD QFontMetricsF:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:averageCharWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_averageCharWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:boundingRect( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_boundingRect( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_boundingRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:elidedText( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QFontMetricsF_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFontMetricsF_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:inFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_inFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:leading( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_leading( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:leftBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_leftBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:lineSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_lineSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:maxWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_maxWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:minLeftBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_minLeftBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:minRightBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_minRightBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:overlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_overlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:rightBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_rightBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:size( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QSizeFFromPointer( Qt_QFontMetricsF_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QSizeFFromPointer( Qt_QFontMetricsF_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QSizeFFromPointer( Qt_QFontMetricsF_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:strikeOutPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_strikeOutPos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:tightBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QFontMetricsF_tightBoundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:underlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_underlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:width( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_width( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_width_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetricsF:xHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetricsF_xHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

