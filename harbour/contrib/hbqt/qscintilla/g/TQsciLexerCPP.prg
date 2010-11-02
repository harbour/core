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


REQUEST __HBQSCINTILLA


FUNCTION QsciLexerCPP( ... )
   RETURN HB_QsciLexerCPP():new( ... )

FUNCTION QsciLexerCPPFromPointer( ... )
   RETURN HB_QsciLexerCPP():fromPointer( ... )


CREATE CLASS QsciLexerCPP INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerCPP

   METHOD  new( ... )

   METHOD  language                      // (  )                                               -> cChar
   METHOD  lexer                         // (  )                                               -> cChar
   METHOD  autoCompletionWordSeparators  // (  )                                               -> oQStringList
   METHOD  blockEnd                      // ( @nStyle )                                        -> cChar
   METHOD  blockStart                    // ( @nStyle )                                        -> cChar
   METHOD  blockStartKeyword             // ( @nStyle )                                        -> cChar
   METHOD  braceStyle                    // (  )                                               -> nInt
   METHOD  wordCharacters                // (  )                                               -> cChar
   METHOD  defaultColor                  // ( nStyle )                                         -> oQColor
   METHOD  defaultEolFill                // ( nStyle )                                         -> lBool
   METHOD  defaultFont                   // ( nStyle )                                         -> oQFont
   METHOD  defaultPaper                  // ( nStyle )                                         -> oQColor
   METHOD  keywords                      // ( nSet )                                           -> cChar
   METHOD  description                   // ( nStyle )                                         -> cQString
   METHOD  refreshProperties             // (  )                                               -> NIL
   METHOD  foldAtElse                    // (  )                                               -> lBool
   METHOD  foldComments                  // (  )                                               -> lBool
   METHOD  foldCompact                   // (  )                                               -> lBool
   METHOD  foldPreprocessor              // (  )                                               -> lBool
   METHOD  stylePreprocessor             // (  )                                               -> lBool
   METHOD  setDollarsAllowed             // ( lAllowed )                                       -> NIL
   METHOD  dollarsAllowed                // (  )                                               -> lBool
   METHOD  setFoldAtElse                 // ( lFold )                                          -> NIL
   METHOD  setFoldComments               // ( lFold )                                          -> NIL
   METHOD  setFoldCompact                // ( lFold )                                          -> NIL
   METHOD  setFoldPreprocessor           // ( lFold )                                          -> NIL
   METHOD  setStylePreprocessor          // ( lStyle )                                         -> NIL

   ENDCLASS


METHOD QsciLexerCPP:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerCPP( ... )
   RETURN Self


METHOD QsciLexerCPP:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_lexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:autoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciLexerCPP_autoCompletionWordSeparators( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockEnd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockEnd( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockStart( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockStart( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockStartKeyword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockStartKeyword( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockStartKeyword( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:braceStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_braceStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerCPP_defaultColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_defaultEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexerCPP_defaultFont( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerCPP_defaultPaper( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:keywords( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_keywords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:description( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_description( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:refreshProperties( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_refreshProperties( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldAtElse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldAtElse( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldComments( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldComments( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldCompact( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldCompact( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldPreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldPreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:stylePreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_stylePreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setDollarsAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setDollarsAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:dollarsAllowed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_dollarsAllowed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldAtElse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldAtElse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldComments( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldComments( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldCompact( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldCompact( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldPreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldPreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setStylePreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setStylePreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

