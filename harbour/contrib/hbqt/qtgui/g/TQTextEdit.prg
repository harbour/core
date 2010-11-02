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


FUNCTION QTextEdit( ... )
   RETURN HB_QTextEdit():new( ... )

FUNCTION QTextEditFromPointer( ... )
   RETURN HB_QTextEdit():fromPointer( ... )


CREATE CLASS QTextEdit INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QTextEdit

   METHOD  new( ... )

   METHOD  acceptRichText                // (  )                                               -> lBool
   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  anchorAt                      // ( oQPoint )                                        -> cQString
   METHOD  autoFormatting                // (  )                                               -> nAutoFormatting
   METHOD  canPaste                      // (  )                                               -> lBool
   METHOD  createStandardContextMenu     // (  )                                               -> oQMenu
                                         // ( oQPoint )                                        -> oQMenu
   METHOD  currentCharFormat             // (  )                                               -> oQTextCharFormat
   METHOD  currentFont                   // (  )                                               -> oQFont
   METHOD  cursorForPosition             // ( oQPoint )                                        -> oQTextCursor
   METHOD  cursorRect                    // ( oQTextCursor )                                   -> oQRect
                                         // (  )                                               -> oQRect
   METHOD  cursorWidth                   // (  )                                               -> nInt
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  documentTitle                 // (  )                                               -> cQString
   METHOD  ensureCursorVisible           // (  )                                               -> NIL
   METHOD  find                          // ( cExp, nOptions )                                 -> lBool
   METHOD  fontFamily                    // (  )                                               -> cQString
   METHOD  fontItalic                    // (  )                                               -> lBool
   METHOD  fontPointSize                 // (  )                                               -> nQreal
   METHOD  fontUnderline                 // (  )                                               -> lBool
   METHOD  fontWeight                    // (  )                                               -> nInt
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  isUndoRedoEnabled             // (  )                                               -> lBool
   METHOD  lineWrapColumnOrWidth         // (  )                                               -> nInt
   METHOD  lineWrapMode                  // (  )                                               -> nLineWrapMode
   METHOD  loadResource                  // ( nType, oQUrl )                                   -> oQVariant
   METHOD  mergeCurrentCharFormat        // ( oQTextCharFormat )                               -> NIL
   METHOD  moveCursor                    // ( nOperation, nMode )                              -> NIL
   METHOD  overwriteMode                 // (  )                                               -> lBool
   METHOD  print                         // ( oQPrinter )                                      -> NIL
   METHOD  setAcceptRichText             // ( lAccept )                                        -> NIL
   METHOD  setAutoFormatting             // ( nFeatures )                                      -> NIL
   METHOD  setCurrentCharFormat          // ( oQTextCharFormat )                               -> NIL
   METHOD  setCursorWidth                // ( nWidth )                                         -> NIL
   METHOD  setDocument                   // ( oQTextDocument )                                 -> NIL
   METHOD  setDocumentTitle              // ( cTitle )                                         -> NIL
   METHOD  setLineWrapColumnOrWidth      // ( nW )                                             -> NIL
   METHOD  setLineWrapMode               // ( nMode )                                          -> NIL
   METHOD  setOverwriteMode              // ( lOverwrite )                                     -> NIL
   METHOD  setReadOnly                   // ( lRo )                                            -> NIL
   METHOD  setTabChangesFocus            // ( lB )                                             -> NIL
   METHOD  setTabStopWidth               // ( nWidth )                                         -> NIL
   METHOD  setTextCursor                 // ( oQTextCursor )                                   -> NIL
   METHOD  setTextInteractionFlags       // ( nFlags )                                         -> NIL
   METHOD  setUndoRedoEnabled            // ( lEnable )                                        -> NIL
   METHOD  setWordWrapMode               // ( nPolicy )                                        -> NIL
   METHOD  tabChangesFocus               // (  )                                               -> lBool
   METHOD  tabStopWidth                  // (  )                                               -> nInt
   METHOD  textBackgroundColor           // (  )                                               -> oQColor
   METHOD  textColor                     // (  )                                               -> oQColor
   METHOD  textCursor                    // (  )                                               -> oQTextCursor
   METHOD  textInteractionFlags          // (  )                                               -> nQt_TextInteractionFlags
   METHOD  toHtml                        // (  )                                               -> cQString
   METHOD  toPlainText                   // (  )                                               -> cQString
   METHOD  wordWrapMode                  // (  )                                               -> nQTextOption_WrapMode
   METHOD  append                        // ( cText )                                          -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  copy                          // (  )                                               -> NIL
   METHOD  cut                           // (  )                                               -> NIL
   METHOD  insertHtml                    // ( cText )                                          -> NIL
   METHOD  insertPlainText               // ( cText )                                          -> NIL
   METHOD  paste                         // (  )                                               -> NIL
   METHOD  redo                          // (  )                                               -> NIL
   METHOD  scrollToAnchor                // ( cName )                                          -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  setAlignment                  // ( nA )                                             -> NIL
   METHOD  setCurrentFont                // ( oQFont )                                         -> NIL
   METHOD  setFontFamily                 // ( cFontFamily )                                    -> NIL
   METHOD  setFontItalic                 // ( lItalic )                                        -> NIL
   METHOD  setFontPointSize              // ( nS )                                             -> NIL
   METHOD  setFontUnderline              // ( lUnderline )                                     -> NIL
   METHOD  setFontWeight                 // ( nWeight )                                        -> NIL
   METHOD  setHtml                       // ( cText )                                          -> NIL
   METHOD  setPlainText                  // ( cText )                                          -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setTextBackgroundColor        // ( oQColor )                                        -> NIL
   METHOD  setTextColor                  // ( oQColor )                                        -> NIL
   METHOD  undo                          // (  )                                               -> NIL
   METHOD  zoomIn                        // ( nRange )                                         -> NIL
   METHOD  zoomOut                       // ( nRange )                                         -> NIL

   ENDCLASS


METHOD QTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextEdit( ... )
   RETURN Self


METHOD QTextEdit:acceptRichText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_acceptRichText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:anchorAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_anchorAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:autoFormatting( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_autoFormatting( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:canPaste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_canPaste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:createStandardContextMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QMenuFromPointer( Qt_QTextEdit_createStandardContextMenu_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QMenuFromPointer( Qt_QTextEdit_createStandardContextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:currentCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextEdit_currentCharFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:currentFont( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTextEdit_currentFont( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:cursorForPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCursorFromPointer( Qt_QTextEdit_cursorForPosition( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:cursorRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTextEdit_cursorRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QTextEdit_cursorRect_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:cursorWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_cursorWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QTextEdit_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:documentTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_documentTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:ensureCursorVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_ensureCursorVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:find( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextEdit_find( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_find( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:fontFamily( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_fontFamily( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:fontItalic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_fontItalic( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:fontPointSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_fontPointSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:fontUnderline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_fontUnderline( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:fontWeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_fontWeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:isUndoRedoEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_isUndoRedoEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:lineWrapColumnOrWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_lineWrapColumnOrWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:lineWrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_lineWrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:loadResource( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QTextEdit_loadResource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:mergeCurrentCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_mergeCurrentCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:moveCursor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextEdit_moveCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_moveCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:overwriteMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_overwriteMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:print( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_print( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setAcceptRichText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setAcceptRichText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setAutoFormatting( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setAutoFormatting( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setCurrentCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setCurrentCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setCursorWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setCursorWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setDocument( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setDocument( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setDocumentTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setDocumentTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setLineWrapColumnOrWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setLineWrapColumnOrWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setLineWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setLineWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setOverwriteMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setOverwriteMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTabChangesFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTabChangesFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTabStopWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTabStopWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTextCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTextCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTextInteractionFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTextInteractionFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setUndoRedoEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setUndoRedoEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setWordWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setWordWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:tabChangesFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_tabChangesFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:tabStopWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_tabStopWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:textBackgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QTextEdit_textBackgroundColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:textColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QTextEdit_textColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:textCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCursorFromPointer( Qt_QTextEdit_textCursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:textInteractionFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_textInteractionFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:toHtml( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_toHtml( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:toPlainText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_toPlainText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:wordWrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_wordWrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:append( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_append( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:copy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_copy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:cut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_cut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:insertHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_insertHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:insertPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_insertPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:paste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_paste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:redo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_redo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:scrollToAnchor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_scrollToAnchor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setCurrentFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setCurrentFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setFontFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setFontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setFontItalic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setFontItalic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setFontPointSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setFontPointSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setFontUnderline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setFontUnderline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setFontWeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setFontWeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTextBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTextBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:setTextColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_setTextColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:undo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextEdit_undo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:zoomIn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_zoomIn( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextEdit_zoomIn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextEdit:zoomOut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextEdit_zoomOut( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextEdit_zoomOut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

