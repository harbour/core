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


FUNCTION QsciLexerFlagship( ... )
   RETURN HB_QsciLexerFlagship():new( ... )

FUNCTION QsciLexerFlagshipFromPointer( ... )
   RETURN HB_QsciLexerFlagship():fromPointer( ... )


CREATE CLASS QsciLexerFlagship INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerFlagship

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


METHOD QsciLexerFlagship:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerFlagship( ... )
   RETURN Self


METHOD QsciLexerFlagship:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_lexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:autoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciLexerFlagship_autoCompletionWordSeparators( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockEnd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockEnd( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockStart( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockStart( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockStartKeyword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockStartKeyword( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockStartKeyword( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:braceStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_braceStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerFlagship_defaultColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_defaultEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexerFlagship_defaultFont( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerFlagship_defaultPaper( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:keywords( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_keywords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:description( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_description( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:refreshProperties( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_refreshProperties( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldAtElse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldAtElse( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldComments( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldComments( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldCompact( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldCompact( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldPreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldPreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:stylePreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_stylePreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setDollarsAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setDollarsAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:dollarsAllowed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_dollarsAllowed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldAtElse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldAtElse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldComments( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldComments( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldCompact( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldCompact( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldPreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldPreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setStylePreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setStylePreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

