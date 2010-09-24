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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QsciScintilla( ... )
   RETURN HB_QsciScintilla():new( ... )


CREATE CLASS QsciScintilla INHERIT HbQtObjectHandler FUNCTION HB_QsciScintilla

   METHOD  new( ... )

   METHOD  annotate( ... )
   METHOD  annotation( nLine )
   METHOD  annotationDisplay()
   METHOD  clearAnnotations( nLine )
   METHOD  autoCompletionCaseSensitivity()
   METHOD  autoCompletionFillupsEnabled()
   METHOD  autoCompletionReplaceWord()
   METHOD  autoCompletionShowSingle()
   METHOD  autoCompletionSource()
   METHOD  autoCompletionThreshold()
   METHOD  autoIndent()
   METHOD  backspaceUnindents()
   METHOD  beginUndoAction()
   METHOD  braceMatching()
   METHOD  callTipsStyle()
   METHOD  callTipsVisible()
   METHOD  cancelList()
   METHOD  caseSensitive()
   METHOD  clearFolds()
   METHOD  clearRegisteredImages()
   METHOD  color()
   METHOD  convertEols( nMode )
   METHOD  document()
   METHOD  endUndoAction()
   METHOD  edgeColor()
   METHOD  edgeColumn()
   METHOD  edgeMode()
   METHOD  setFont( pF )
   METHOD  eolMode()
   METHOD  eolVisibility()
   METHOD  findFirst( cExpr, lRe, lCs, lWo, lWrap, lForward, nLine, nIndex, lShow )
   METHOD  findNext()
   METHOD  firstVisibleLine()
   METHOD  folding()
   METHOD  getCursorPosition( nLine, nIndex )
   METHOD  getSelection( nLineFrom, nIndexFrom, nLineTo, nIndexTo )
   METHOD  hasSelectedText()
   METHOD  indentation( nLine )
   METHOD  indentationGuides()
   METHOD  indentationsUseTabs()
   METHOD  indentationWidth()
   METHOD  isCallTipActive()
   METHOD  isListActive()
   METHOD  isModified()
   METHOD  isReadOnly()
   METHOD  isRedoAvailable()
   METHOD  isUndoAvailable()
   METHOD  isUtf8()
   METHOD  isWordCharacter( cCh )
   METHOD  lineAt( pPos )
   METHOD  lineIndexFromPosition( nPosition, nLine, nIndex )
   METHOD  lineLength( nLine )
   METHOD  lines()
   METHOD  length()
   METHOD  lexer()
   METHOD  marginLineNumbers( nMargin )
   METHOD  marginMarkerMask( nMargin )
   METHOD  marginSensitivity( nMargin )
   METHOD  marginType( nMargin )
   METHOD  marginWidth( nMargin )
   METHOD  markerDefine( ... )
   METHOD  markerAdd( nLinenr, nMnr )
   METHOD  markersAtLine( nLinenr )
   METHOD  markerDelete( nLinenr, nMnr )
   METHOD  markerDeleteAll( nMnr )
   METHOD  markerDeleteHandle( nMhandle )
   METHOD  markerLine( nMhandle )
   METHOD  markerFindNext( nLinenr, nMask )
   METHOD  markerFindPrevious( nLinenr, nMask )
   METHOD  paper()
   METHOD  positionFromLineIndex( nLine, nIndex )
   METHOD  read( pIo )
   METHOD  recolor( nStart, nEnd )
   METHOD  registerImage( nId, pPm )
   METHOD  replace( cReplaceStr )
   METHOD  resetFoldMarginColors()
   METHOD  setFoldMarginColors( pFore, pBack )
   METHOD  setAnnotationDisplay( nDisplay )
   METHOD  setAutoCompletionFillupsEnabled( lEnabled )
   METHOD  setAutoCompletionFillups( pFillups )
   METHOD  setAutoCompletionWordSeparators( pSeparators )
   METHOD  setCallTipsBackgroundColor( pCol )
   METHOD  setCallTipsForegroundColor( pCol )
   METHOD  setCallTipsHighlightColor( pCol )
   METHOD  setCallTipsStyle( nStyle )
   METHOD  setCallTipsVisible( nNr )
   METHOD  setDocument( pDocument )
   METHOD  setEdgeColor( pCol )
   METHOD  setEdgeColumn( nColnr )
   METHOD  setEdgeMode( nMode )
   METHOD  setMarginText( ... )
   METHOD  setMarginType( nMargin, nType )
   METHOD  clearMarginText( nLine )
   METHOD  setMarkerBackgroundColor( pCol, nMnr )
   METHOD  setMarkerForegroundColor( pCol, nMnr )
   METHOD  setMatchedBraceBackgroundColor( pCol )
   METHOD  setMatchedBraceForegroundColor( pCol )
   METHOD  setUnmatchedBraceBackgroundColor( pCol )
   METHOD  setUnmatchedBraceForegroundColor( pCol )
   METHOD  setWrapVisualFlags( nEflag, nSflag, nSindent )
   METHOD  selectedText()
   METHOD  selectionToEol()
   METHOD  setSelectionToEol( lFilled )
   METHOD  showUserList( nId, pList )
   METHOD  standardCommands()
   METHOD  tabIndents()
   METHOD  tabWidth()
   METHOD  text( ... )
   METHOD  textHeight( nLinenr )
   METHOD  whitespaceVisibility()
   METHOD  wordAtPoint( pPoint )
   METHOD  wordCharacters()
   METHOD  wrapMode()
   METHOD  write( pIo )
   METHOD  append( cText )
   METHOD  autoCompleteFromAll()
   METHOD  autoCompleteFromAPIs()
   METHOD  autoCompleteFromDocument()
   METHOD  callTip()
   METHOD  clear()
   METHOD  copy()
   METHOD  cut()
   METHOD  ensureCursorVisible()
   METHOD  ensureLineVisible( nLine )
   METHOD  foldAll( lChildren )
   METHOD  foldLine( nLine )
   METHOD  indent( nLine )
   METHOD  insert( cText )
   METHOD  insertAt( cText, nLine, nIndex )
   METHOD  moveToMatchingBrace()
   METHOD  paste()
   METHOD  redo()
   METHOD  removeSelectedText()
   METHOD  resetSelectionBackgroundColor()
   METHOD  resetSelectionForegroundColor()
   METHOD  selectAll( lSelect )
   METHOD  selectToMatchingBrace()
   METHOD  setAutoCompletionCaseSensitivity( lCs )
   METHOD  setAutoCompletionReplaceWord( lReplace )
   METHOD  setAutoCompletionShowSingle( lSingle )
   METHOD  setAutoCompletionSource( nSource )
   METHOD  setAutoCompletionThreshold( nThresh )
   METHOD  setAutoIndent( lAutoindent )
   METHOD  setBraceMatching( nBm )
   METHOD  setBackspaceUnindents( lUnindent )
   METHOD  setCaretForegroundColor( pCol )
   METHOD  setCaretLineBackgroundColor( pCol )
   METHOD  setCaretLineVisible( lEnable )
   METHOD  setCaretWidth( nWidth )
   METHOD  setColor( pC )
   METHOD  setCursorPosition( nLine, nIndex )
   METHOD  setEolMode( nMode )
   METHOD  setEolVisibility( lVisible )
   METHOD  setFolding( nFold, nMargin )
   METHOD  setIndentation( nLine, nIndentation )
   METHOD  setIndentationGuides( lEnable )
   METHOD  setIndentationGuidesBackgroundColor( pCol )
   METHOD  setIndentationGuidesForegroundColor( pCol )
   METHOD  setIndentationsUseTabs( lTabs )
   METHOD  setIndentationWidth( nWidth )
   METHOD  setLexer( pLexer )
   METHOD  setMarginsBackgroundColor( pCol )
   METHOD  setMarginsFont( pF )
   METHOD  setMarginsForegroundColor( pCol )
   METHOD  setMarginLineNumbers( nMargin, lLnrs )
   METHOD  setMarginMarkerMask( nMargin, nMask )
   METHOD  setMarginSensitivity( nMargin, lSens )
   METHOD  setMarginWidth( ... )
   METHOD  setModified( lM )
   METHOD  setPaper( pC )
   METHOD  setReadOnly( lRo )
   METHOD  setSelection( nLineFrom, nIndexFrom, nLineTo, nIndexTo )
   METHOD  setSelectionBackgroundColor( pCol )
   METHOD  setSelectionForegroundColor( pCol )
   METHOD  setTabIndents( lIndent )
   METHOD  setTabWidth( nWidth )
   METHOD  setText( cText )
   METHOD  setUtf8( lCp )
   METHOD  setWhitespaceVisibility( nMode )
   METHOD  setWrapMode( nMode )
   METHOD  undo()
   METHOD  unindent( nLine )
   METHOD  zoomIn( ... )
   METHOD  zoomOut( ... )
   METHOD  zoomTo( nSize )

   ENDCLASS


METHOD QsciScintilla:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciScintilla( ... )
   RETURN Self


METHOD QsciScintilla:annotate( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "N"
                // void annotate (int line, const QString &text, int style)
                // N n int, C c QString, N n int
         RETURN Qt_QsciScintilla_annotate( ::pPtr, ... )
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "PO"
                // void annotate (int line, const QString &text, const QsciStyle &style)
                // N n int, C c QString, PO p QsciStyle
         RETURN Qt_QsciScintilla_annotate_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void annotate (int line, const QsciStyledText &text)
                // N n int, PO p QsciStyledText
         RETURN Qt_QsciScintilla_annotate_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:annotation( nLine )
   RETURN Qt_QsciScintilla_annotation( ::pPtr, nLine )


METHOD QsciScintilla:annotationDisplay()
   RETURN Qt_QsciScintilla_annotationDisplay( ::pPtr )


METHOD QsciScintilla:clearAnnotations( nLine )
   RETURN Qt_QsciScintilla_clearAnnotations( ::pPtr, nLine )


METHOD QsciScintilla:autoCompletionCaseSensitivity()
   RETURN Qt_QsciScintilla_autoCompletionCaseSensitivity( ::pPtr )


METHOD QsciScintilla:autoCompletionFillupsEnabled()
   RETURN Qt_QsciScintilla_autoCompletionFillupsEnabled( ::pPtr )


METHOD QsciScintilla:autoCompletionReplaceWord()
   RETURN Qt_QsciScintilla_autoCompletionReplaceWord( ::pPtr )


METHOD QsciScintilla:autoCompletionShowSingle()
   RETURN Qt_QsciScintilla_autoCompletionShowSingle( ::pPtr )


METHOD QsciScintilla:autoCompletionSource()
   RETURN Qt_QsciScintilla_autoCompletionSource( ::pPtr )


METHOD QsciScintilla:autoCompletionThreshold()
   RETURN Qt_QsciScintilla_autoCompletionThreshold( ::pPtr )


METHOD QsciScintilla:autoIndent()
   RETURN Qt_QsciScintilla_autoIndent( ::pPtr )


METHOD QsciScintilla:backspaceUnindents()
   RETURN Qt_QsciScintilla_backspaceUnindents( ::pPtr )


METHOD QsciScintilla:beginUndoAction()
   RETURN Qt_QsciScintilla_beginUndoAction( ::pPtr )


METHOD QsciScintilla:braceMatching()
   RETURN Qt_QsciScintilla_braceMatching( ::pPtr )


METHOD QsciScintilla:callTipsStyle()
   RETURN Qt_QsciScintilla_callTipsStyle( ::pPtr )


METHOD QsciScintilla:callTipsVisible()
   RETURN Qt_QsciScintilla_callTipsVisible( ::pPtr )


METHOD QsciScintilla:cancelList()
   RETURN Qt_QsciScintilla_cancelList( ::pPtr )


METHOD QsciScintilla:caseSensitive()
   RETURN Qt_QsciScintilla_caseSensitive( ::pPtr )


METHOD QsciScintilla:clearFolds()
   RETURN Qt_QsciScintilla_clearFolds( ::pPtr )


METHOD QsciScintilla:clearRegisteredImages()
   RETURN Qt_QsciScintilla_clearRegisteredImages( ::pPtr )


METHOD QsciScintilla:color()
   RETURN Qt_QsciScintilla_color( ::pPtr )


METHOD QsciScintilla:convertEols( nMode )
   RETURN Qt_QsciScintilla_convertEols( ::pPtr, nMode )


METHOD QsciScintilla:document()
   RETURN Qt_QsciScintilla_document( ::pPtr )


METHOD QsciScintilla:endUndoAction()
   RETURN Qt_QsciScintilla_endUndoAction( ::pPtr )


METHOD QsciScintilla:edgeColor()
   RETURN Qt_QsciScintilla_edgeColor( ::pPtr )


METHOD QsciScintilla:edgeColumn()
   RETURN Qt_QsciScintilla_edgeColumn( ::pPtr )


METHOD QsciScintilla:edgeMode()
   RETURN Qt_QsciScintilla_edgeMode( ::pPtr )


METHOD QsciScintilla:setFont( pF )
   RETURN Qt_QsciScintilla_setFont( ::pPtr, hbqt_ptr( pF ) )


METHOD QsciScintilla:eolMode()
   RETURN Qt_QsciScintilla_eolMode( ::pPtr )


METHOD QsciScintilla:eolVisibility()
   RETURN Qt_QsciScintilla_eolVisibility( ::pPtr )


METHOD QsciScintilla:findFirst( cExpr, lRe, lCs, lWo, lWrap, lForward, nLine, nIndex, lShow )
   RETURN Qt_QsciScintilla_findFirst( ::pPtr, cExpr, lRe, lCs, lWo, lWrap, lForward, nLine, nIndex, lShow )


METHOD QsciScintilla:findNext()
   RETURN Qt_QsciScintilla_findNext( ::pPtr )


METHOD QsciScintilla:firstVisibleLine()
   RETURN Qt_QsciScintilla_firstVisibleLine( ::pPtr )


METHOD QsciScintilla:folding()
   RETURN Qt_QsciScintilla_folding( ::pPtr )


METHOD QsciScintilla:getCursorPosition( nLine, nIndex )
   RETURN Qt_QsciScintilla_getCursorPosition( ::pPtr, nLine, nIndex )


METHOD QsciScintilla:getSelection( nLineFrom, nIndexFrom, nLineTo, nIndexTo )
   RETURN Qt_QsciScintilla_getSelection( ::pPtr, nLineFrom, nIndexFrom, nLineTo, nIndexTo )


METHOD QsciScintilla:hasSelectedText()
   RETURN Qt_QsciScintilla_hasSelectedText( ::pPtr )


METHOD QsciScintilla:indentation( nLine )
   RETURN Qt_QsciScintilla_indentation( ::pPtr, nLine )


METHOD QsciScintilla:indentationGuides()
   RETURN Qt_QsciScintilla_indentationGuides( ::pPtr )


METHOD QsciScintilla:indentationsUseTabs()
   RETURN Qt_QsciScintilla_indentationsUseTabs( ::pPtr )


METHOD QsciScintilla:indentationWidth()
   RETURN Qt_QsciScintilla_indentationWidth( ::pPtr )


METHOD QsciScintilla:isCallTipActive()
   RETURN Qt_QsciScintilla_isCallTipActive( ::pPtr )


METHOD QsciScintilla:isListActive()
   RETURN Qt_QsciScintilla_isListActive( ::pPtr )


METHOD QsciScintilla:isModified()
   RETURN Qt_QsciScintilla_isModified( ::pPtr )


METHOD QsciScintilla:isReadOnly()
   RETURN Qt_QsciScintilla_isReadOnly( ::pPtr )


METHOD QsciScintilla:isRedoAvailable()
   RETURN Qt_QsciScintilla_isRedoAvailable( ::pPtr )


METHOD QsciScintilla:isUndoAvailable()
   RETURN Qt_QsciScintilla_isUndoAvailable( ::pPtr )


METHOD QsciScintilla:isUtf8()
   RETURN Qt_QsciScintilla_isUtf8( ::pPtr )


METHOD QsciScintilla:isWordCharacter( cCh )
   RETURN Qt_QsciScintilla_isWordCharacter( ::pPtr, cCh )


METHOD QsciScintilla:lineAt( pPos )
   RETURN Qt_QsciScintilla_lineAt( ::pPtr, hbqt_ptr( pPos ) )


METHOD QsciScintilla:lineIndexFromPosition( nPosition, nLine, nIndex )
   RETURN Qt_QsciScintilla_lineIndexFromPosition( ::pPtr, nPosition, nLine, nIndex )


METHOD QsciScintilla:lineLength( nLine )
   RETURN Qt_QsciScintilla_lineLength( ::pPtr, nLine )


METHOD QsciScintilla:lines()
   RETURN Qt_QsciScintilla_lines( ::pPtr )


METHOD QsciScintilla:length()
   RETURN Qt_QsciScintilla_length( ::pPtr )


METHOD QsciScintilla:lexer()
   RETURN Qt_QsciScintilla_lexer( ::pPtr )


METHOD QsciScintilla:marginLineNumbers( nMargin )
   RETURN Qt_QsciScintilla_marginLineNumbers( ::pPtr, nMargin )


METHOD QsciScintilla:marginMarkerMask( nMargin )
   RETURN Qt_QsciScintilla_marginMarkerMask( ::pPtr, nMargin )


METHOD QsciScintilla:marginSensitivity( nMargin )
   RETURN Qt_QsciScintilla_marginSensitivity( ::pPtr, nMargin )


METHOD QsciScintilla:marginType( nMargin )
   RETURN Qt_QsciScintilla_marginType( ::pPtr, nMargin )


METHOD QsciScintilla:marginWidth( nMargin )
   RETURN Qt_QsciScintilla_marginWidth( ::pPtr, nMargin )


METHOD QsciScintilla:markerDefine( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // int markerDefine (char ch, int mnr=-1)
                // C c char, N n int
         RETURN Qt_QsciScintilla_markerDefine_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // int markerDefine (MarkerSymbol sym, int mnr=-1)
                // N n QsciScintilla::MarkerSymbol, N n int
         RETURN Qt_QsciScintilla_markerDefine( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // int markerDefine (const QPixmap &pm, int mnr=-1)
                // PO p QPixmap, N n int
         RETURN Qt_QsciScintilla_markerDefine_2( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // int markerDefine (char ch, int mnr=-1)
                // C c char, N n int
         RETURN Qt_QsciScintilla_markerDefine_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "N"
                // int markerDefine (MarkerSymbol sym, int mnr=-1)
                // N n QsciScintilla::MarkerSymbol, N n int
         RETURN Qt_QsciScintilla_markerDefine( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // int markerDefine (const QPixmap &pm, int mnr=-1)
                // PO p QPixmap, N n int
         RETURN Qt_QsciScintilla_markerDefine_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:markerAdd( nLinenr, nMnr )
   RETURN Qt_QsciScintilla_markerAdd( ::pPtr, nLinenr, nMnr )


METHOD QsciScintilla:markersAtLine( nLinenr )
   RETURN Qt_QsciScintilla_markersAtLine( ::pPtr, nLinenr )


METHOD QsciScintilla:markerDelete( nLinenr, nMnr )
   RETURN Qt_QsciScintilla_markerDelete( ::pPtr, nLinenr, nMnr )


METHOD QsciScintilla:markerDeleteAll( nMnr )
   RETURN Qt_QsciScintilla_markerDeleteAll( ::pPtr, nMnr )


METHOD QsciScintilla:markerDeleteHandle( nMhandle )
   RETURN Qt_QsciScintilla_markerDeleteHandle( ::pPtr, nMhandle )


METHOD QsciScintilla:markerLine( nMhandle )
   RETURN Qt_QsciScintilla_markerLine( ::pPtr, nMhandle )


METHOD QsciScintilla:markerFindNext( nLinenr, nMask )
   RETURN Qt_QsciScintilla_markerFindNext( ::pPtr, nLinenr, nMask )


METHOD QsciScintilla:markerFindPrevious( nLinenr, nMask )
   RETURN Qt_QsciScintilla_markerFindPrevious( ::pPtr, nLinenr, nMask )


METHOD QsciScintilla:paper()
   RETURN Qt_QsciScintilla_paper( ::pPtr )


METHOD QsciScintilla:positionFromLineIndex( nLine, nIndex )
   RETURN Qt_QsciScintilla_positionFromLineIndex( ::pPtr, nLine, nIndex )


METHOD QsciScintilla:read( pIo )
   RETURN Qt_QsciScintilla_read( ::pPtr, hbqt_ptr( pIo ) )


METHOD QsciScintilla:recolor( nStart, nEnd )
   RETURN Qt_QsciScintilla_recolor( ::pPtr, nStart, nEnd )


METHOD QsciScintilla:registerImage( nId, pPm )
   RETURN Qt_QsciScintilla_registerImage( ::pPtr, nId, hbqt_ptr( pPm ) )


METHOD QsciScintilla:replace( cReplaceStr )
   RETURN Qt_QsciScintilla_replace( ::pPtr, cReplaceStr )


METHOD QsciScintilla:resetFoldMarginColors()
   RETURN Qt_QsciScintilla_resetFoldMarginColors( ::pPtr )


METHOD QsciScintilla:setFoldMarginColors( pFore, pBack )
   RETURN Qt_QsciScintilla_setFoldMarginColors( ::pPtr, hbqt_ptr( pFore ), hbqt_ptr( pBack ) )


METHOD QsciScintilla:setAnnotationDisplay( nDisplay )
   RETURN Qt_QsciScintilla_setAnnotationDisplay( ::pPtr, nDisplay )


METHOD QsciScintilla:setAutoCompletionFillupsEnabled( lEnabled )
   RETURN Qt_QsciScintilla_setAutoCompletionFillupsEnabled( ::pPtr, lEnabled )


METHOD QsciScintilla:setAutoCompletionFillups( pFillups )
   RETURN Qt_QsciScintilla_setAutoCompletionFillups( ::pPtr, hbqt_ptr( pFillups ) )


METHOD QsciScintilla:setAutoCompletionWordSeparators( pSeparators )
   RETURN Qt_QsciScintilla_setAutoCompletionWordSeparators( ::pPtr, hbqt_ptr( pSeparators ) )


METHOD QsciScintilla:setCallTipsBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setCallTipsBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setCallTipsForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setCallTipsForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setCallTipsHighlightColor( pCol )
   RETURN Qt_QsciScintilla_setCallTipsHighlightColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setCallTipsStyle( nStyle )
   RETURN Qt_QsciScintilla_setCallTipsStyle( ::pPtr, nStyle )


METHOD QsciScintilla:setCallTipsVisible( nNr )
   RETURN Qt_QsciScintilla_setCallTipsVisible( ::pPtr, nNr )


METHOD QsciScintilla:setDocument( pDocument )
   RETURN Qt_QsciScintilla_setDocument( ::pPtr, hbqt_ptr( pDocument ) )


METHOD QsciScintilla:setEdgeColor( pCol )
   RETURN Qt_QsciScintilla_setEdgeColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setEdgeColumn( nColnr )
   RETURN Qt_QsciScintilla_setEdgeColumn( ::pPtr, nColnr )


METHOD QsciScintilla:setEdgeMode( nMode )
   RETURN Qt_QsciScintilla_setEdgeMode( ::pPtr, nMode )


METHOD QsciScintilla:setMarginText( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "N"
                // void setMarginText (int line, const QString &text, int style)
                // N n int, C c QString, N n int
         RETURN Qt_QsciScintilla_setMarginText( ::pPtr, ... )
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "PO"
                // void setMarginText (int line, const QString &text, const QsciStyle &style)
                // N n int, C c QString, PO p QsciStyle
         RETURN Qt_QsciScintilla_setMarginText_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void setMarginText (int line, const QsciStyledText &text)
                // N n int, PO p QsciStyledText
         RETURN Qt_QsciScintilla_setMarginText_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:setMarginType( nMargin, nType )
   RETURN Qt_QsciScintilla_setMarginType( ::pPtr, nMargin, nType )


METHOD QsciScintilla:clearMarginText( nLine )
   RETURN Qt_QsciScintilla_clearMarginText( ::pPtr, nLine )


METHOD QsciScintilla:setMarkerBackgroundColor( pCol, nMnr )
   RETURN Qt_QsciScintilla_setMarkerBackgroundColor( ::pPtr, hbqt_ptr( pCol ), nMnr )


METHOD QsciScintilla:setMarkerForegroundColor( pCol, nMnr )
   RETURN Qt_QsciScintilla_setMarkerForegroundColor( ::pPtr, hbqt_ptr( pCol ), nMnr )


METHOD QsciScintilla:setMatchedBraceBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setMatchedBraceBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setMatchedBraceForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setMatchedBraceForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setUnmatchedBraceBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setUnmatchedBraceBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setUnmatchedBraceForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setUnmatchedBraceForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setWrapVisualFlags( nEflag, nSflag, nSindent )
   RETURN Qt_QsciScintilla_setWrapVisualFlags( ::pPtr, nEflag, nSflag, nSindent )


METHOD QsciScintilla:selectedText()
   RETURN Qt_QsciScintilla_selectedText( ::pPtr )


METHOD QsciScintilla:selectionToEol()
   RETURN Qt_QsciScintilla_selectionToEol( ::pPtr )


METHOD QsciScintilla:setSelectionToEol( lFilled )
   RETURN Qt_QsciScintilla_setSelectionToEol( ::pPtr, lFilled )


METHOD QsciScintilla:showUserList( nId, pList )
   RETURN Qt_QsciScintilla_showUserList( ::pPtr, nId, hbqt_ptr( pList ) )


METHOD QsciScintilla:standardCommands()
   RETURN Qt_QsciScintilla_standardCommands( ::pPtr )


METHOD QsciScintilla:tabIndents()
   RETURN Qt_QsciScintilla_tabIndents( ::pPtr )


METHOD QsciScintilla:tabWidth()
   RETURN Qt_QsciScintilla_tabWidth( ::pPtr )


METHOD QsciScintilla:text( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // QString text (int line) const
                // N n int
         RETURN Qt_QsciScintilla_text_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QString text () const
      RETURN Qt_QsciScintilla_text( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:textHeight( nLinenr )
   RETURN Qt_QsciScintilla_textHeight( ::pPtr, nLinenr )


METHOD QsciScintilla:whitespaceVisibility()
   RETURN Qt_QsciScintilla_whitespaceVisibility( ::pPtr )


METHOD QsciScintilla:wordAtPoint( pPoint )
   RETURN Qt_QsciScintilla_wordAtPoint( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QsciScintilla:wordCharacters()
   RETURN Qt_QsciScintilla_wordCharacters( ::pPtr )


METHOD QsciScintilla:wrapMode()
   RETURN Qt_QsciScintilla_wrapMode( ::pPtr )


METHOD QsciScintilla:write( pIo )
   RETURN Qt_QsciScintilla_write( ::pPtr, hbqt_ptr( pIo ) )


METHOD QsciScintilla:append( cText )
   RETURN Qt_QsciScintilla_append( ::pPtr, cText )


METHOD QsciScintilla:autoCompleteFromAll()
   RETURN Qt_QsciScintilla_autoCompleteFromAll( ::pPtr )


METHOD QsciScintilla:autoCompleteFromAPIs()
   RETURN Qt_QsciScintilla_autoCompleteFromAPIs( ::pPtr )


METHOD QsciScintilla:autoCompleteFromDocument()
   RETURN Qt_QsciScintilla_autoCompleteFromDocument( ::pPtr )


METHOD QsciScintilla:callTip()
   RETURN Qt_QsciScintilla_callTip( ::pPtr )


METHOD QsciScintilla:clear()
   RETURN Qt_QsciScintilla_clear( ::pPtr )


METHOD QsciScintilla:copy()
   RETURN Qt_QsciScintilla_copy( ::pPtr )


METHOD QsciScintilla:cut()
   RETURN Qt_QsciScintilla_cut( ::pPtr )


METHOD QsciScintilla:ensureCursorVisible()
   RETURN Qt_QsciScintilla_ensureCursorVisible( ::pPtr )


METHOD QsciScintilla:ensureLineVisible( nLine )
   RETURN Qt_QsciScintilla_ensureLineVisible( ::pPtr, nLine )


METHOD QsciScintilla:foldAll( lChildren )
   RETURN Qt_QsciScintilla_foldAll( ::pPtr, lChildren )


METHOD QsciScintilla:foldLine( nLine )
   RETURN Qt_QsciScintilla_foldLine( ::pPtr, nLine )


METHOD QsciScintilla:indent( nLine )
   RETURN Qt_QsciScintilla_indent( ::pPtr, nLine )


METHOD QsciScintilla:insert( cText )
   RETURN Qt_QsciScintilla_insert( ::pPtr, cText )


METHOD QsciScintilla:insertAt( cText, nLine, nIndex )
   RETURN Qt_QsciScintilla_insertAt( ::pPtr, cText, nLine, nIndex )


METHOD QsciScintilla:moveToMatchingBrace()
   RETURN Qt_QsciScintilla_moveToMatchingBrace( ::pPtr )


METHOD QsciScintilla:paste()
   RETURN Qt_QsciScintilla_paste( ::pPtr )


METHOD QsciScintilla:redo()
   RETURN Qt_QsciScintilla_redo( ::pPtr )


METHOD QsciScintilla:removeSelectedText()
   RETURN Qt_QsciScintilla_removeSelectedText( ::pPtr )


METHOD QsciScintilla:resetSelectionBackgroundColor()
   RETURN Qt_QsciScintilla_resetSelectionBackgroundColor( ::pPtr )


METHOD QsciScintilla:resetSelectionForegroundColor()
   RETURN Qt_QsciScintilla_resetSelectionForegroundColor( ::pPtr )


METHOD QsciScintilla:selectAll( lSelect )
   RETURN Qt_QsciScintilla_selectAll( ::pPtr, lSelect )


METHOD QsciScintilla:selectToMatchingBrace()
   RETURN Qt_QsciScintilla_selectToMatchingBrace( ::pPtr )


METHOD QsciScintilla:setAutoCompletionCaseSensitivity( lCs )
   RETURN Qt_QsciScintilla_setAutoCompletionCaseSensitivity( ::pPtr, lCs )


METHOD QsciScintilla:setAutoCompletionReplaceWord( lReplace )
   RETURN Qt_QsciScintilla_setAutoCompletionReplaceWord( ::pPtr, lReplace )


METHOD QsciScintilla:setAutoCompletionShowSingle( lSingle )
   RETURN Qt_QsciScintilla_setAutoCompletionShowSingle( ::pPtr, lSingle )


METHOD QsciScintilla:setAutoCompletionSource( nSource )
   RETURN Qt_QsciScintilla_setAutoCompletionSource( ::pPtr, nSource )


METHOD QsciScintilla:setAutoCompletionThreshold( nThresh )
   RETURN Qt_QsciScintilla_setAutoCompletionThreshold( ::pPtr, nThresh )


METHOD QsciScintilla:setAutoIndent( lAutoindent )
   RETURN Qt_QsciScintilla_setAutoIndent( ::pPtr, lAutoindent )


METHOD QsciScintilla:setBraceMatching( nBm )
   RETURN Qt_QsciScintilla_setBraceMatching( ::pPtr, nBm )


METHOD QsciScintilla:setBackspaceUnindents( lUnindent )
   RETURN Qt_QsciScintilla_setBackspaceUnindents( ::pPtr, lUnindent )


METHOD QsciScintilla:setCaretForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setCaretForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setCaretLineBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setCaretLineBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setCaretLineVisible( lEnable )
   RETURN Qt_QsciScintilla_setCaretLineVisible( ::pPtr, lEnable )


METHOD QsciScintilla:setCaretWidth( nWidth )
   RETURN Qt_QsciScintilla_setCaretWidth( ::pPtr, nWidth )


METHOD QsciScintilla:setColor( pC )
   RETURN Qt_QsciScintilla_setColor( ::pPtr, hbqt_ptr( pC ) )


METHOD QsciScintilla:setCursorPosition( nLine, nIndex )
   RETURN Qt_QsciScintilla_setCursorPosition( ::pPtr, nLine, nIndex )


METHOD QsciScintilla:setEolMode( nMode )
   RETURN Qt_QsciScintilla_setEolMode( ::pPtr, nMode )


METHOD QsciScintilla:setEolVisibility( lVisible )
   RETURN Qt_QsciScintilla_setEolVisibility( ::pPtr, lVisible )


METHOD QsciScintilla:setFolding( nFold, nMargin )
   RETURN Qt_QsciScintilla_setFolding( ::pPtr, nFold, nMargin )


METHOD QsciScintilla:setIndentation( nLine, nIndentation )
   RETURN Qt_QsciScintilla_setIndentation( ::pPtr, nLine, nIndentation )


METHOD QsciScintilla:setIndentationGuides( lEnable )
   RETURN Qt_QsciScintilla_setIndentationGuides( ::pPtr, lEnable )


METHOD QsciScintilla:setIndentationGuidesBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setIndentationGuidesBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setIndentationGuidesForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setIndentationGuidesForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setIndentationsUseTabs( lTabs )
   RETURN Qt_QsciScintilla_setIndentationsUseTabs( ::pPtr, lTabs )


METHOD QsciScintilla:setIndentationWidth( nWidth )
   RETURN Qt_QsciScintilla_setIndentationWidth( ::pPtr, nWidth )


METHOD QsciScintilla:setLexer( pLexer )
   RETURN Qt_QsciScintilla_setLexer( ::pPtr, hbqt_ptr( pLexer ) )


METHOD QsciScintilla:setMarginsBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setMarginsBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setMarginsFont( pF )
   RETURN Qt_QsciScintilla_setMarginsFont( ::pPtr, hbqt_ptr( pF ) )


METHOD QsciScintilla:setMarginsForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setMarginsForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setMarginLineNumbers( nMargin, lLnrs )
   RETURN Qt_QsciScintilla_setMarginLineNumbers( ::pPtr, nMargin, lLnrs )


METHOD QsciScintilla:setMarginMarkerMask( nMargin, nMask )
   RETURN Qt_QsciScintilla_setMarginMarkerMask( ::pPtr, nMargin, nMask )


METHOD QsciScintilla:setMarginSensitivity( nMargin, lSens )
   RETURN Qt_QsciScintilla_setMarginSensitivity( ::pPtr, nMargin, lSens )


METHOD QsciScintilla:setMarginWidth( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C"
                // virtual void setMarginWidth (int margin, const QString &s)
                // N n int, C c QString
         RETURN Qt_QsciScintilla_setMarginWidth_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // virtual void setMarginWidth (int margin, int width)
                // N n int, N n int
         RETURN Qt_QsciScintilla_setMarginWidth( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:setModified( lM )
   RETURN Qt_QsciScintilla_setModified( ::pPtr, lM )


METHOD QsciScintilla:setPaper( pC )
   RETURN Qt_QsciScintilla_setPaper( ::pPtr, hbqt_ptr( pC ) )


METHOD QsciScintilla:setReadOnly( lRo )
   RETURN Qt_QsciScintilla_setReadOnly( ::pPtr, lRo )


METHOD QsciScintilla:setSelection( nLineFrom, nIndexFrom, nLineTo, nIndexTo )
   RETURN Qt_QsciScintilla_setSelection( ::pPtr, nLineFrom, nIndexFrom, nLineTo, nIndexTo )


METHOD QsciScintilla:setSelectionBackgroundColor( pCol )
   RETURN Qt_QsciScintilla_setSelectionBackgroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setSelectionForegroundColor( pCol )
   RETURN Qt_QsciScintilla_setSelectionForegroundColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QsciScintilla:setTabIndents( lIndent )
   RETURN Qt_QsciScintilla_setTabIndents( ::pPtr, lIndent )


METHOD QsciScintilla:setTabWidth( nWidth )
   RETURN Qt_QsciScintilla_setTabWidth( ::pPtr, nWidth )


METHOD QsciScintilla:setText( cText )
   RETURN Qt_QsciScintilla_setText( ::pPtr, cText )


METHOD QsciScintilla:setUtf8( lCp )
   RETURN Qt_QsciScintilla_setUtf8( ::pPtr, lCp )


METHOD QsciScintilla:setWhitespaceVisibility( nMode )
   RETURN Qt_QsciScintilla_setWhitespaceVisibility( ::pPtr, nMode )


METHOD QsciScintilla:setWrapMode( nMode )
   RETURN Qt_QsciScintilla_setWrapMode( ::pPtr, nMode )


METHOD QsciScintilla:undo()
   RETURN Qt_QsciScintilla_undo( ::pPtr )


METHOD QsciScintilla:unindent( nLine )
   RETURN Qt_QsciScintilla_unindent( ::pPtr, nLine )


METHOD QsciScintilla:zoomIn( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // virtual void zoomIn (int range)
                // N n int
         RETURN Qt_QsciScintilla_zoomIn( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // virtual void zoomIn ()
      RETURN Qt_QsciScintilla_zoomIn_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:zoomOut( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // virtual void zoomOut (int range)
                // N n int
         RETURN Qt_QsciScintilla_zoomOut( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // virtual void zoomOut ()
      RETURN Qt_QsciScintilla_zoomOut_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QsciScintilla:zoomTo( nSize )
   RETURN Qt_QsciScintilla_zoomTo( ::pPtr, nSize )

