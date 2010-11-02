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


REQUEST __HBQTCORE


FUNCTION QRegExp( ... )
   RETURN HB_QRegExp():new( ... )

FUNCTION QRegExpFromPointer( ... )
   RETURN HB_QRegExp():fromPointer( ... )


CREATE CLASS QRegExp INHERIT HbQtObjectHandler FUNCTION HB_QRegExp

   METHOD  new( ... )

   METHOD  cap                           // ( nNth )                                           -> cQString
   METHOD  capturedTexts                 // (  )                                               -> oQStringList
   METHOD  caseSensitivity               // (  )                                               -> nQt_CaseSensitivity
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  exactMatch                    // ( cStr )                                           -> lBool
   METHOD  indexIn                       // ( cStr, nOffset, nCaretMode )                      -> nInt
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isMinimal                     // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  lastIndexIn                   // ( cStr, nOffset, nCaretMode )                      -> nInt
   METHOD  matchedLength                 // (  )                                               -> nInt
   METHOD  numCaptures                   // (  )                                               -> nInt
   METHOD  pattern                       // (  )                                               -> cQString
   METHOD  patternSyntax                 // (  )                                               -> nPatternSyntax
   METHOD  pos                           // ( nNth )                                           -> nInt
   METHOD  setCaseSensitivity            // ( nCs )                                            -> NIL
   METHOD  setMinimal                    // ( lMinimal )                                       -> NIL
   METHOD  setPattern                    // ( cPattern )                                       -> NIL
   METHOD  setPatternSyntax              // ( nSyntax )                                        -> NIL

   ENDCLASS


METHOD QRegExp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegExp( ... )
   RETURN Self


METHOD QRegExp:cap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_cap( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QRegExp_cap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:capturedTexts( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QRegExp_capturedTexts( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:caseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_caseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:exactMatch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_exactMatch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:indexIn( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QRegExp_indexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRegExp_indexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_indexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:isMinimal( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_isMinimal( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:lastIndexIn( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QRegExp_lastIndexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRegExp_lastIndexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_lastIndexIn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:matchedLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_matchedLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:numCaptures( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_numCaptures( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:pattern( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_pattern( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:patternSyntax( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegExp_patternSyntax( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:pos( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_pos( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QRegExp_pos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:setCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_setCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:setMinimal( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_setMinimal( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:setPattern( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_setPattern( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegExp:setPatternSyntax( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRegExp_setPatternSyntax( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

