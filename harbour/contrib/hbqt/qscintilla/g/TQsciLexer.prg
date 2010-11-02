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


FUNCTION QsciLexer( ... )
   RETURN HB_QsciLexer():new( ... )

FUNCTION QsciLexerFromPointer( ... )
   RETURN HB_QsciLexer():fromPointer( ... )


CREATE CLASS QsciLexer INHERIT HbQtObjectHandler FUNCTION HB_QsciLexer

   METHOD  new( ... )

   METHOD  language                      // (  )                                               -> cChar
   METHOD  lexer                         // (  )                                               -> cChar
   METHOD  lexerId                       // (  )                                               -> nInt
   METHOD  apis                          // (  )                                               -> oQsciAbstractAPIs
   METHOD  autoCompletionFillups         // (  )                                               -> cChar
   METHOD  autoCompletionWordSeparators  // (  )                                               -> oQStringList
   METHOD  autoIndentStyle               // (  )                                               -> nInt
   METHOD  blockEnd                      // ( @nStyle )                                        -> cChar
   METHOD  blockLookback                 // (  )                                               -> nInt
   METHOD  blockStart                    // ( @nStyle )                                        -> cChar
   METHOD  blockStartKeyword             // ( @nStyle )                                        -> cChar
   METHOD  braceStyle                    // (  )                                               -> nInt
   METHOD  caseSensitive                 // (  )                                               -> lBool
   METHOD  color                         // ( nStyle )                                         -> oQColor
   METHOD  eolFill                       // ( nStyle )                                         -> lBool
   METHOD  font                          // ( nStyle )                                         -> oQFont
   METHOD  indentationGuideView          // (  )                                               -> nInt
   METHOD  keywords                      // ( nSet )                                           -> cChar
   METHOD  defaultStyle                  // (  )                                               -> nInt
   METHOD  description                   // ( nStyle )                                         -> cQString
   METHOD  paper                         // ( nStyle )                                         -> oQColor
   METHOD  defaultColor                  // (  )                                               -> oQColor
                                         // ( nStyle )                                         -> oQColor
   METHOD  defaultEolFill                // ( nStyle )                                         -> lBool
   METHOD  defaultFont                   // (  )                                               -> oQFont
                                         // ( nStyle )                                         -> oQFont
   METHOD  defaultPaper                  // (  )                                               -> oQColor
                                         // ( nStyle )                                         -> oQColor
   METHOD  editor                        // (  )                                               -> oQsciScintilla
   METHOD  setEditor                     // ( oQsciScintilla )                                 -> NIL
   METHOD  readSettings                  // ( oQSettings, cPrefix )                            -> lBool
   METHOD  refreshProperties             // (  )                                               -> NIL
   METHOD  styleBitsNeeded               // (  )                                               -> nInt
   METHOD  wordCharacters                // (  )                                               -> cChar
   METHOD  writeSettings                 // ( oQSettings, cPrefix )                            -> lBool
   METHOD  setAPIs                       // ( oQsciAbstractAPIs )                              -> NIL
   METHOD  setDefaultColor               // ( oQColor )                                        -> NIL
   METHOD  setDefaultFont                // ( oQFont )                                         -> NIL
   METHOD  setDefaultPaper               // ( oQColor )                                        -> NIL
   METHOD  setAutoIndentStyle            // ( nAutoindentstyle )                               -> NIL
   METHOD  setColor                      // ( oQColor, nStyle )                                -> NIL
   METHOD  setEolFill                    // ( lEoffill, nStyle )                               -> NIL
   METHOD  setFont                       // ( oQFont, nStyle )                                 -> NIL
   METHOD  setPaper                      // ( oQColor, nStyle )                                -> NIL

   ENDCLASS


METHOD QsciLexer:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexer( ... )
   RETURN Self


METHOD QsciLexer:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_lexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:lexerId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_lexerId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:apis( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciAbstractAPIsFromPointer( Qt_QsciLexer_apis( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:autoCompletionFillups( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_autoCompletionFillups( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:autoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciLexer_autoCompletionWordSeparators( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:autoIndentStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_autoIndentStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:blockEnd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_blockEnd( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexer_blockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:blockLookback( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_blockLookback( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:blockStart( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_blockStart( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexer_blockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:blockStartKeyword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_blockStartKeyword( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexer_blockStartKeyword( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:braceStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_braceStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:caseSensitive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_caseSensitive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:color( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexer_color( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:eolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_eolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:font( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexer_font( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:indentationGuideView( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_indentationGuideView( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:keywords( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_keywords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:defaultStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_defaultStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:description( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_description( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:paper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexer_paper( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:defaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexer_defaultColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QsciLexer_defaultColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:defaultEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_defaultEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:defaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexer_defaultFont_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QFontFromPointer( Qt_QsciLexer_defaultFont( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:defaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexer_defaultPaper_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QsciLexer_defaultPaper( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:editor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciScintillaFromPointer( Qt_QsciLexer_editor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setEditor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:readSettings( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_readSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_readSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:refreshProperties( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_refreshProperties( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:styleBitsNeeded( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_styleBitsNeeded( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexer_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:writeSettings( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_writeSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_writeSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setAPIs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setAPIs( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setDefaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setDefaultColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setDefaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setDefaultFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setDefaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setDefaultPaper( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setAutoIndentStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setAutoIndentStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setEolFill( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_setEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setFont( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexer:setPaper( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciLexer_setPaper( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexer_setPaper( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

