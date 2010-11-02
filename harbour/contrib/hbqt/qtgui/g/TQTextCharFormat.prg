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


FUNCTION QTextCharFormat( ... )
   RETURN HB_QTextCharFormat():new( ... )

FUNCTION QTextCharFormatFromPointer( ... )
   RETURN HB_QTextCharFormat():fromPointer( ... )


CREATE CLASS QTextCharFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextCharFormat

   METHOD  new( ... )

   METHOD  anchorHref                    // (  )                                               -> cQString
   METHOD  anchorNames                   // (  )                                               -> oQStringList
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  fontCapitalization            // (  )                                               -> nQFont_Capitalization
   METHOD  fontFamily                    // (  )                                               -> cQString
   METHOD  fontFixedPitch                // (  )                                               -> lBool
   METHOD  fontItalic                    // (  )                                               -> lBool
   METHOD  fontKerning                   // (  )                                               -> lBool
   METHOD  fontLetterSpacing             // (  )                                               -> nQreal
   METHOD  fontOverline                  // (  )                                               -> lBool
   METHOD  fontPointSize                 // (  )                                               -> nQreal
   METHOD  fontStrikeOut                 // (  )                                               -> lBool
   METHOD  fontStyleHint                 // (  )                                               -> nQFont_StyleHint
   METHOD  fontStyleStrategy             // (  )                                               -> nQFont_StyleStrategy
   METHOD  fontUnderline                 // (  )                                               -> lBool
   METHOD  fontWeight                    // (  )                                               -> nInt
   METHOD  fontWordSpacing               // (  )                                               -> nQreal
   METHOD  isAnchor                      // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  setAnchor                     // ( lAnchor )                                        -> NIL
   METHOD  setAnchorHref                 // ( cValue )                                         -> NIL
   METHOD  setAnchorNames                // ( oQStringList )                                   -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setFontCapitalization         // ( nCapitalization )                                -> NIL
   METHOD  setFontFamily                 // ( cFamily )                                        -> NIL
   METHOD  setFontFixedPitch             // ( lFixedPitch )                                    -> NIL
   METHOD  setFontItalic                 // ( lItalic )                                        -> NIL
   METHOD  setFontKerning                // ( lEnable )                                        -> NIL
   METHOD  setFontLetterSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setFontOverline               // ( lOverline )                                      -> NIL
   METHOD  setFontPointSize              // ( nSize )                                          -> NIL
   METHOD  setFontStrikeOut              // ( lStrikeOut )                                     -> NIL
   METHOD  setFontStyleHint              // ( nHint, nStrategy )                               -> NIL
   METHOD  setFontStyleStrategy          // ( nStrategy )                                      -> NIL
   METHOD  setFontUnderline              // ( lUnderline )                                     -> NIL
   METHOD  setFontWeight                 // ( nWeight )                                        -> NIL
   METHOD  setFontWordSpacing            // ( nSpacing )                                       -> NIL
   METHOD  setTextOutline                // ( oQPen )                                          -> NIL
   METHOD  setToolTip                    // ( cText )                                          -> NIL
   METHOD  setUnderlineColor             // ( oQColor )                                        -> NIL
   METHOD  setUnderlineStyle             // ( nStyle )                                         -> NIL
   METHOD  setVerticalAlignment          // ( nAlignment )                                     -> NIL
   METHOD  textOutline                   // (  )                                               -> oQPen
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  underlineColor                // (  )                                               -> oQColor
   METHOD  underlineStyle                // (  )                                               -> nUnderlineStyle
   METHOD  verticalAlignment             // (  )                                               -> nVerticalAlignment

   ENDCLASS


METHOD QTextCharFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCharFormat( ... )
   RETURN Self


METHOD QTextCharFormat:anchorHref( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_anchorHref( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:anchorNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QTextCharFormat_anchorNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTextCharFormat_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontCapitalization( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontCapitalization( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontFamily( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontFamily( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontFixedPitch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontFixedPitch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontItalic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontItalic( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontKerning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontKerning( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontLetterSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontLetterSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontOverline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontOverline( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontPointSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontPointSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontStrikeOut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStrikeOut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontStyleHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStyleHint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontStyleStrategy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStyleStrategy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontUnderline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontUnderline( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontWeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontWeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:fontWordSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontWordSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:isAnchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_isAnchor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setAnchor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setAnchorHref( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchorHref( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setAnchorNames( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchorNames( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontCapitalization( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontCapitalization( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontFixedPitch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontFixedPitch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontItalic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontItalic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontKerning( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontKerning( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontLetterSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontLetterSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontOverline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontOverline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontPointSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontPointSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontStrikeOut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStrikeOut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontStyleHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCharFormat_setFontStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontStyleStrategy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStyleStrategy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontUnderline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontUnderline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontWeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontWeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setFontWordSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontWordSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setTextOutline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setTextOutline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setUnderlineColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setUnderlineColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setUnderlineStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setUnderlineStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:setVerticalAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setVerticalAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:textOutline( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_QTextCharFormat_textOutline( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:underlineColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QTextCharFormat_underlineColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:underlineStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_underlineStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCharFormat:verticalAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_verticalAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

