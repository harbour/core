/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


REQUEST __HBQSCINTILLA


FUNCTION QsciScintilla( ... )
   RETURN HB_QsciScintilla():new( ... )

FUNCTION QsciScintillaFromPointer( ... )
   RETURN HB_QsciScintilla():fromPointer( ... )


CREATE CLASS QsciScintilla INHERIT HbQtObjectHandler FUNCTION HB_QsciScintilla

   METHOD  new( ... )

   METHOD  annotate                      // ( nLine, cText, nStyle )                           -> NIL
                                         // ( nLine, cText, oQsciStyle )                       -> NIL
                                         // ( nLine, oQsciStyledText )                         -> NIL
   METHOD  annotation                    // ( nLine )                                          -> cQString
   METHOD  annotationDisplay             // (  )                                               -> nAnnotationDisplay
   METHOD  clearAnnotations              // ( nLine )                                          -> NIL
   METHOD  autoCompletionCaseSensitivity // (  )                                               -> lBool
   METHOD  autoCompletionFillupsEnabled  // (  )                                               -> lBool
   METHOD  autoCompletionReplaceWord     // (  )                                               -> lBool
   METHOD  autoCompletionShowSingle      // (  )                                               -> lBool
   METHOD  autoCompletionSource          // (  )                                               -> nAutoCompletionSource
   METHOD  autoCompletionThreshold       // (  )                                               -> nInt
   METHOD  autoIndent                    // (  )                                               -> lBool
   METHOD  backspaceUnindents            // (  )                                               -> lBool
   METHOD  beginUndoAction               // (  )                                               -> NIL
   METHOD  braceMatching                 // (  )                                               -> nBraceMatch
   METHOD  callTipsStyle                 // (  )                                               -> nCallTipsStyle
   METHOD  callTipsVisible               // (  )                                               -> nInt
   METHOD  cancelList                    // (  )                                               -> NIL
   METHOD  caseSensitive                 // (  )                                               -> lBool
   METHOD  clearFolds                    // (  )                                               -> NIL
   METHOD  clearRegisteredImages         // (  )                                               -> NIL
   METHOD  color                         // (  )                                               -> oQColor
   METHOD  convertEols                   // ( nMode )                                          -> NIL
   METHOD  document                      // (  )                                               -> oQsciDocument
   METHOD  endUndoAction                 // (  )                                               -> NIL
   METHOD  edgeColor                     // (  )                                               -> oQColor
   METHOD  edgeColumn                    // (  )                                               -> nInt
   METHOD  edgeMode                      // (  )                                               -> nEdgeMode
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  eolMode                       // (  )                                               -> nEolMode
   METHOD  eolVisibility                 // (  )                                               -> lBool
   METHOD  findFirst                     // ( cExpr, lRe, lCs, lWo, lWrap, lForward, nLine, nIndex, lShow ) -> lBool
   METHOD  findNext                      // (  )                                               -> lBool
   METHOD  firstVisibleLine              // (  )                                               -> nInt
   METHOD  folding                       // (  )                                               -> nFoldStyle
   METHOD  getCursorPosition             // ( @nLine, @nIndex )                                -> NIL
   METHOD  getSelection                  // ( @nLineFrom, @nIndexFrom, @nLineTo, @nIndexTo )   -> NIL
   METHOD  hasSelectedText               // (  )                                               -> lBool
   METHOD  indentation                   // ( nLine )                                          -> nInt
   METHOD  indentationGuides             // (  )                                               -> lBool
   METHOD  indentationsUseTabs           // (  )                                               -> lBool
   METHOD  indentationWidth              // (  )                                               -> nInt
   METHOD  isCallTipActive               // (  )                                               -> lBool
   METHOD  isListActive                  // (  )                                               -> lBool
   METHOD  isModified                    // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  isRedoAvailable               // (  )                                               -> lBool
   METHOD  isUndoAvailable               // (  )                                               -> lBool
   METHOD  isUtf8                        // (  )                                               -> lBool
   METHOD  isWordCharacter               // ( nCh )                                            -> lBool
   METHOD  lineAt                        // ( oQPoint )                                        -> nInt
   METHOD  lineIndexFromPosition         // ( nPosition, @nLine, @nIndex )                     -> NIL
   METHOD  lineLength                    // ( nLine )                                          -> nInt
   METHOD  lines                         // (  )                                               -> nInt
   METHOD  length                        // (  )                                               -> nInt
   METHOD  lexer                         // (  )                                               -> oQsciLexer
   METHOD  marginLineNumbers             // ( nMargin )                                        -> lBool
   METHOD  marginMarkerMask              // ( nMargin )                                        -> nInt
   METHOD  marginSensitivity             // ( nMargin )                                        -> lBool
   METHOD  marginType                    // ( nMargin )                                        -> nMarginType
   METHOD  marginWidth                   // ( nMargin )                                        -> nInt
   METHOD  markerDefine                  // ( nSym, nMnr )                                     -> nInt
                                         // ( nCh, nMnr )                                      -> nInt
                                         // ( oQPixmap, nMnr )                                 -> nInt
   METHOD  markerAdd                     // ( nLinenr, nMnr )                                  -> nInt
   METHOD  markersAtLine                 // ( nLinenr )                                        -> nUnsigned
   METHOD  markerDelete                  // ( nLinenr, nMnr )                                  -> NIL
   METHOD  markerDeleteAll               // ( nMnr )                                           -> NIL
   METHOD  markerDeleteHandle            // ( nMhandle )                                       -> NIL
   METHOD  markerLine                    // ( nMhandle )                                       -> nInt
   METHOD  markerFindNext                // ( nLinenr, nMask )                                 -> nInt
   METHOD  markerFindPrevious            // ( nLinenr, nMask )                                 -> nInt
   METHOD  paper                         // (  )                                               -> oQColor
   METHOD  positionFromLineIndex         // ( nLine, nIndex )                                  -> nInt
   METHOD  read                          // ( oQIODevice )                                     -> lBool
   METHOD  recolor                       // ( nStart, nEnd )                                   -> NIL
   METHOD  registerImage                 // ( nId, oQPixmap )                                  -> NIL
   METHOD  replace                       // ( cReplaceStr )                                    -> NIL
   METHOD  resetFoldMarginColors         // (  )                                               -> NIL
   METHOD  setFoldMarginColors           // ( oQColor, oQColor )                               -> NIL
   METHOD  setAnnotationDisplay          // ( nDisplay )                                       -> NIL
   METHOD  setAutoCompletionFillupsEnabled // ( lEnabled )                                       -> NIL
   METHOD  setAutoCompletionFillups      // ( cFillups )                                       -> NIL
   METHOD  setAutoCompletionWordSeparators // ( oQStringList )                                   -> NIL
   METHOD  setCallTipsBackgroundColor    // ( oQColor )                                        -> NIL
   METHOD  setCallTipsForegroundColor    // ( oQColor )                                        -> NIL
   METHOD  setCallTipsHighlightColor     // ( oQColor )                                        -> NIL
   METHOD  setCallTipsStyle              // ( nStyle )                                         -> NIL
   METHOD  setCallTipsVisible            // ( nNr )                                            -> NIL
   METHOD  setDocument                   // ( oQsciDocument )                                  -> NIL
   METHOD  setEdgeColor                  // ( oQColor )                                        -> NIL
   METHOD  setEdgeColumn                 // ( nColnr )                                         -> NIL
   METHOD  setEdgeMode                   // ( nMode )                                          -> NIL
   METHOD  setMarginText                 // ( nLine, cText, nStyle )                           -> NIL
                                         // ( nLine, cText, oQsciStyle )                       -> NIL
                                         // ( nLine, oQsciStyledText )                         -> NIL
   METHOD  setMarginType                 // ( nMargin, nType )                                 -> NIL
   METHOD  clearMarginText               // ( nLine )                                          -> NIL
   METHOD  setMarkerBackgroundColor      // ( oQColor, nMnr )                                  -> NIL
   METHOD  setMarkerForegroundColor      // ( oQColor, nMnr )                                  -> NIL
   METHOD  setMatchedBraceBackgroundColor // ( oQColor )                                        -> NIL
   METHOD  setMatchedBraceForegroundColor // ( oQColor )                                        -> NIL
   METHOD  setUnmatchedBraceBackgroundColor // ( oQColor )                                        -> NIL
   METHOD  setUnmatchedBraceForegroundColor // ( oQColor )                                        -> NIL
   METHOD  setWrapVisualFlags            // ( nEflag, nSflag, nSindent )                       -> NIL
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  selectionToEol                // (  )                                               -> lBool
   METHOD  setSelectionToEol             // ( lFilled )                                        -> NIL
   METHOD  showUserList                  // ( nId, oQStringList )                              -> NIL
   METHOD  standardCommands              // (  )                                               -> oQsciCommandSet
   METHOD  tabIndents                    // (  )                                               -> lBool
   METHOD  tabWidth                      // (  )                                               -> nInt
   METHOD  text                          // (  )                                               -> cQString
                                         // ( nLine )                                          -> cQString
   METHOD  textHeight                    // ( nLinenr )                                        -> nInt
   METHOD  whitespaceVisibility          // (  )                                               -> nWhitespaceVisibility
   METHOD  wordAtPoint                   // ( oQPoint )                                        -> cQString
   METHOD  wordCharacters                // (  )                                               -> cChar
   METHOD  wrapMode                      // (  )                                               -> nWrapMode
   METHOD  write                         // ( oQIODevice )                                     -> lBool
   METHOD  append                        // ( cText )                                          -> NIL
   METHOD  autoCompleteFromAll           // (  )                                               -> NIL
   METHOD  autoCompleteFromAPIs          // (  )                                               -> NIL
   METHOD  autoCompleteFromDocument      // (  )                                               -> NIL
   METHOD  callTip                       // (  )                                               -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  copy                          // (  )                                               -> NIL
   METHOD  cut                           // (  )                                               -> NIL
   METHOD  ensureCursorVisible           // (  )                                               -> NIL
   METHOD  ensureLineVisible             // ( nLine )                                          -> NIL
   METHOD  foldAll                       // ( lChildren )                                      -> NIL
   METHOD  foldLine                      // ( nLine )                                          -> NIL
   METHOD  indent                        // ( nLine )                                          -> NIL
   METHOD  insert                        // ( cText )                                          -> NIL
   METHOD  insertAt                      // ( cText, nLine, nIndex )                           -> NIL
   METHOD  moveToMatchingBrace           // (  )                                               -> NIL
   METHOD  paste                         // (  )                                               -> NIL
   METHOD  redo                          // (  )                                               -> NIL
   METHOD  removeSelectedText            // (  )                                               -> NIL
   METHOD  resetSelectionBackgroundColor // (  )                                               -> NIL
   METHOD  resetSelectionForegroundColor // (  )                                               -> NIL
   METHOD  selectAll                     // ( lSelect )                                        -> NIL
   METHOD  selectToMatchingBrace         // (  )                                               -> NIL
   METHOD  setAutoCompletionCaseSensitivity // ( lCs )                                            -> NIL
   METHOD  setAutoCompletionReplaceWord  // ( lReplace )                                       -> NIL
   METHOD  setAutoCompletionShowSingle   // ( lSingle )                                        -> NIL
   METHOD  setAutoCompletionSource       // ( nSource )                                        -> NIL
   METHOD  setAutoCompletionThreshold    // ( nThresh )                                        -> NIL
   METHOD  setAutoIndent                 // ( lAutoindent )                                    -> NIL
   METHOD  setBraceMatching              // ( nBm )                                            -> NIL
   METHOD  setBackspaceUnindents         // ( lUnindent )                                      -> NIL
   METHOD  setCaretForegroundColor       // ( oQColor )                                        -> NIL
   METHOD  setCaretLineBackgroundColor   // ( oQColor )                                        -> NIL
   METHOD  setCaretLineVisible           // ( lEnable )                                        -> NIL
   METHOD  setCaretWidth                 // ( nWidth )                                         -> NIL
   METHOD  setColor                      // ( oQColor )                                        -> NIL
   METHOD  setCursorPosition             // ( nLine, nIndex )                                  -> NIL
   METHOD  setEolMode                    // ( nMode )                                          -> NIL
   METHOD  setEolVisibility              // ( lVisible )                                       -> NIL
   METHOD  setFolding                    // ( nFold, nMargin )                                 -> NIL
   METHOD  setIndentation                // ( nLine, nIndentation )                            -> NIL
   METHOD  setIndentationGuides          // ( lEnable )                                        -> NIL
   METHOD  setIndentationGuidesBackgroundColor // ( oQColor )                                        -> NIL
   METHOD  setIndentationGuidesForegroundColor // ( oQColor )                                        -> NIL
   METHOD  setIndentationsUseTabs        // ( lTabs )                                          -> NIL
   METHOD  setIndentationWidth           // ( nWidth )                                         -> NIL
   METHOD  setLexer                      // ( oQsciLexer )                                     -> NIL
   METHOD  setMarginsBackgroundColor     // ( oQColor )                                        -> NIL
   METHOD  setMarginsFont                // ( oQFont )                                         -> NIL
   METHOD  setMarginsForegroundColor     // ( oQColor )                                        -> NIL
   METHOD  setMarginLineNumbers          // ( nMargin, lLnrs )                                 -> NIL
   METHOD  setMarginMarkerMask           // ( nMargin, nMask )                                 -> NIL
   METHOD  setMarginSensitivity          // ( nMargin, lSens )                                 -> NIL
   METHOD  setMarginWidth                // ( nMargin, nWidth )                                -> NIL
                                         // ( nMargin, cS )                                    -> NIL
   METHOD  setModified                   // ( lM )                                             -> NIL
   METHOD  setPaper                      // ( oQColor )                                        -> NIL
   METHOD  setReadOnly                   // ( lRo )                                            -> NIL
   METHOD  setSelection                  // ( nLineFrom, nIndexFrom, nLineTo, nIndexTo )       -> NIL
   METHOD  setSelectionBackgroundColor   // ( oQColor )                                        -> NIL
   METHOD  setSelectionForegroundColor   // ( oQColor )                                        -> NIL
   METHOD  setTabIndents                 // ( lIndent )                                        -> NIL
   METHOD  setTabWidth                   // ( nWidth )                                         -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setUtf8                       // ( lCp )                                            -> NIL
   METHOD  setWhitespaceVisibility       // ( nMode )                                          -> NIL
   METHOD  setWrapMode                   // ( nMode )                                          -> NIL
   METHOD  undo                          // (  )                                               -> NIL
   METHOD  unindent                      // ( nLine )                                          -> NIL
   METHOD  zoomIn                        // ( nRange )                                         -> NIL
                                         // (  )                                               -> NIL
   METHOD  zoomOut                       // ( nRange )                                         -> NIL
                                         // (  )                                               -> NIL
   METHOD  zoomTo                        // ( nSize )                                          -> NIL

   ENDCLASS


METHOD QsciScintilla:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciScintilla( ... )
   RETURN Self


METHOD QsciScintilla:annotate( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_annotate( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_annotate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_annotate_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:annotation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_annotation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:annotationDisplay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_annotationDisplay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:clearAnnotations( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_clearAnnotations( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_clearAnnotations( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionCaseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionCaseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionFillupsEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionFillupsEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionReplaceWord( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionReplaceWord( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionShowSingle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionShowSingle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionSource( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionSource( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompletionThreshold( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompletionThreshold( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoIndent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoIndent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:backspaceUnindents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_backspaceUnindents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:beginUndoAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_beginUndoAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:braceMatching( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_braceMatching( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:callTipsStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_callTipsStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:callTipsVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_callTipsVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:cancelList( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_cancelList( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:caseSensitive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_caseSensitive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:clearFolds( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_clearFolds( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:clearRegisteredImages( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_clearRegisteredImages( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:color( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QsciScintilla_color( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:convertEols( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_convertEols( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciDocumentFromPointer( Qt_QsciScintilla_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:endUndoAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_endUndoAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:edgeColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QsciScintilla_edgeColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:edgeColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_edgeColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:edgeMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_edgeMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:eolMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_eolMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:eolVisibility( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_eolVisibility( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:findFirst( ... )
   SWITCH PCount()
   CASE 9
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isLogical( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) ) .AND. hb_isLogical( hb_pvalue( 9 ) )
         RETURN Qt_QsciScintilla_findFirst( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 8
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isLogical( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN Qt_QsciScintilla_findFirst( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isLogical( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QsciScintilla_findFirst( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isLogical( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) )
         RETURN Qt_QsciScintilla_findFirst( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isLogical( hb_pvalue( 5 ) )
         RETURN Qt_QsciScintilla_findFirst( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:findNext( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_findNext( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:firstVisibleLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_firstVisibleLine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:folding( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_folding( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:getCursorPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_getCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:getSelection( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QsciScintilla_getSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:hasSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_hasSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:indentation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_indentation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:indentationGuides( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_indentationGuides( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:indentationsUseTabs( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_indentationsUseTabs( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:indentationWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_indentationWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isCallTipActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isCallTipActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isListActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isListActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isModified( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isRedoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isRedoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isUndoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isUndoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isUtf8( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_isUtf8( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:isWordCharacter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_isWordCharacter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:lineAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_lineAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:lineIndexFromPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_lineIndexFromPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:lineLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_lineLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:lines( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_lines( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciLexerFromPointer( Qt_QsciScintilla_lexer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:marginLineNumbers( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_marginLineNumbers( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:marginMarkerMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_marginMarkerMask( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:marginSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_marginSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:marginType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_marginType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:marginWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_marginWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerDefine( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerDefine( ::pPtr, ... )
         // RETURN Qt_QsciScintilla_markerDefine_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerDefine_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerDefine( ::pPtr, ... )
         // RETURN Qt_QsciScintilla_markerDefine_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerDefine_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerAdd( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerAdd( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markersAtLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markersAtLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerDelete( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerDelete( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerDelete( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerDeleteAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerDeleteAll( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_markerDeleteAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerDeleteHandle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerDeleteHandle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_markerLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerFindNext( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerFindNext( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:markerFindPrevious( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_markerFindPrevious( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:paper( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QsciScintilla_paper( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:positionFromLineIndex( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_positionFromLineIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:recolor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_recolor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_recolor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_recolor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:registerImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_registerImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:replace( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_replace( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:resetFoldMarginColors( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_resetFoldMarginColors( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setFoldMarginColors( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setFoldMarginColors( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAnnotationDisplay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAnnotationDisplay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionFillupsEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionFillupsEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionFillups( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionFillups( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionWordSeparators( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCallTipsBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCallTipsBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCallTipsForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCallTipsForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCallTipsHighlightColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCallTipsHighlightColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCallTipsStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCallTipsStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCallTipsVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCallTipsVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setDocument( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setDocument( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setEdgeColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setEdgeColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setEdgeColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setEdgeColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setEdgeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setEdgeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginText( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_setMarginText( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_setMarginText_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginText_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginType( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:clearMarginText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_clearMarginText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_clearMarginText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarkerBackgroundColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarkerBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMarkerBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarkerForegroundColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarkerForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMarkerForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMatchedBraceBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMatchedBraceBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMatchedBraceForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMatchedBraceForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setUnmatchedBraceBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setUnmatchedBraceBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setUnmatchedBraceForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setUnmatchedBraceForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setWrapVisualFlags( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_setWrapVisualFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setWrapVisualFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setWrapVisualFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:selectionToEol( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_selectionToEol( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setSelectionToEol( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setSelectionToEol( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:showUserList( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_showUserList( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:standardCommands( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciCommandSetFromPointer( Qt_QsciScintilla_standardCommands( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:tabIndents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_tabIndents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:tabWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_tabWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_text_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:textHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_textHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:whitespaceVisibility( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_whitespaceVisibility( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:wordAtPoint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_wordAtPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:wrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_wrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:append( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_append( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompleteFromAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompleteFromAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompleteFromAPIs( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompleteFromAPIs( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:autoCompleteFromDocument( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_autoCompleteFromDocument( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:callTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_callTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:copy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_copy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:cut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_cut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:ensureCursorVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_ensureCursorVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:ensureLineVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_ensureLineVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:foldAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_foldAll( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_foldAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:foldLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_foldLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:indent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_indent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:insert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:insertAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QsciScintilla_insertAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:moveToMatchingBrace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_moveToMatchingBrace( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:paste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_paste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:redo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_redo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:removeSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_removeSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:resetSelectionBackgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_resetSelectionBackgroundColor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:resetSelectionForegroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_resetSelectionForegroundColor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:selectAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_selectAll( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:selectToMatchingBrace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_selectToMatchingBrace( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionReplaceWord( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionReplaceWord( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionShowSingle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionShowSingle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionSource( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoCompletionThreshold( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoCompletionThreshold( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setAutoIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setAutoIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setBraceMatching( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setBraceMatching( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setBackspaceUnindents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setBackspaceUnindents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCaretForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCaretForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCaretLineBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCaretLineBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCaretLineVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCaretLineVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCaretWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setCaretWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setCursorPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setEolMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setEolMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setEolVisibility( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setEolVisibility( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setFolding( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setFolding( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setFolding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentation( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setIndentation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentationGuides( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setIndentationGuides( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentationGuidesBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setIndentationGuidesBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentationGuidesForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setIndentationGuidesForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentationsUseTabs( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setIndentationsUseTabs( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setIndentationWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setIndentationWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setLexer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setLexer( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_setLexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginsBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMarginsBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginsFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMarginsFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginsForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setMarginsForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginLineNumbers( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginLineNumbers( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginMarkerMask( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginMarkerMask( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginSensitivity( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setMarginWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginWidth_1( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QsciScintilla_setMarginWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setModified( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setModified( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setPaper( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setSelection( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QsciScintilla_setSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setSelectionBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setSelectionBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setSelectionForegroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setSelectionForegroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setTabIndents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setTabIndents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setTabWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setTabWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setUtf8( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setWhitespaceVisibility( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setWhitespaceVisibility( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:setWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_setWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:undo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciScintilla_undo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:unindent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_unindent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:zoomIn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_zoomIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_zoomIn_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:zoomOut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_zoomOut( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciScintilla_zoomOut_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciScintilla:zoomTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciScintilla_zoomTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

