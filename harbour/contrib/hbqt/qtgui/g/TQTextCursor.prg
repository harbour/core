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


FUNCTION QTextCursor( ... )
   RETURN HB_QTextCursor():new( ... )

FUNCTION QTextCursorFromPointer( ... )
   RETURN HB_QTextCursor():fromPointer( ... )


CREATE CLASS QTextCursor INHERIT HbQtObjectHandler FUNCTION HB_QTextCursor

   METHOD  new( ... )

   METHOD  anchor                        // (  )                                               -> nInt
   METHOD  atBlockEnd                    // (  )                                               -> lBool
   METHOD  atBlockStart                  // (  )                                               -> lBool
   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  atStart                       // (  )                                               -> lBool
   METHOD  beginEditBlock                // (  )                                               -> NIL
   METHOD  block                         // (  )                                               -> oQTextBlock
   METHOD  blockCharFormat               // (  )                                               -> oQTextCharFormat
   METHOD  blockFormat                   // (  )                                               -> oQTextBlockFormat
   METHOD  blockNumber                   // (  )                                               -> nInt
   METHOD  charFormat                    // (  )                                               -> oQTextCharFormat
   METHOD  clearSelection                // (  )                                               -> NIL
   METHOD  columnNumber                  // (  )                                               -> nInt
   METHOD  createList                    // ( oQTextListFormat )                               -> oQTextList
                                         // ( nStyle )                                         -> oQTextList
   METHOD  currentFrame                  // (  )                                               -> oQTextFrame
   METHOD  currentList                   // (  )                                               -> oQTextList
   METHOD  deleteChar                    // (  )                                               -> NIL
   METHOD  deletePreviousChar            // (  )                                               -> NIL
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  endEditBlock                  // (  )                                               -> NIL
   METHOD  hasComplexSelection           // (  )                                               -> lBool
   METHOD  hasSelection                  // (  )                                               -> lBool
   METHOD  insertBlock                   // (  )                                               -> NIL
                                         // ( oQTextBlockFormat )                              -> NIL
                                         // ( oQTextBlockFormat, oQTextCharFormat )            -> NIL
   METHOD  insertFragment                // ( oQTextDocumentFragment )                         -> NIL
   METHOD  insertFrame                   // ( oQTextFrameFormat )                              -> oQTextFrame
   METHOD  insertHtml                    // ( cHtml )                                          -> NIL
   METHOD  insertImage                   // ( cName )                                          -> NIL
                                         // ( oQTextImageFormat )                              -> NIL
                                         // ( oQTextImageFormat, nAlignment )                  -> NIL
                                         // ( oQImage, cName )                                 -> NIL
   METHOD  insertList                    // ( oQTextListFormat )                               -> oQTextList
                                         // ( nStyle )                                         -> oQTextList
   METHOD  insertText                    // ( cText )                                          -> NIL
                                         // ( cText, oQTextCharFormat )                        -> NIL
   METHOD  isCopyOf                      // ( oQTextCursor )                                   -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  joinPreviousEditBlock         // (  )                                               -> NIL
   METHOD  mergeBlockCharFormat          // ( oQTextCharFormat )                               -> NIL
   METHOD  mergeBlockFormat              // ( oQTextBlockFormat )                              -> NIL
   METHOD  mergeCharFormat               // ( oQTextCharFormat )                               -> NIL
   METHOD  movePosition                  // ( nOperation, nMode, nN )                          -> lBool
   METHOD  position                      // (  )                                               -> nInt
   METHOD  removeSelectedText            // (  )                                               -> NIL
   METHOD  select                        // ( nSelection )                                     -> NIL
   METHOD  selectedTableCells            // ( @nFirstRow, @nNumRows, @nFirstColumn, @nNumColumns ) -> NIL
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  selection                     // (  )                                               -> oQTextDocumentFragment
   METHOD  selectionEnd                  // (  )                                               -> nInt
   METHOD  selectionStart                // (  )                                               -> nInt
   METHOD  setBlockCharFormat            // ( oQTextCharFormat )                               -> NIL
   METHOD  setBlockFormat                // ( oQTextBlockFormat )                              -> NIL
   METHOD  setCharFormat                 // ( oQTextCharFormat )                               -> NIL
   METHOD  setPosition                   // ( nPos, nM )                                       -> NIL
   METHOD  setVisualNavigation           // ( lB )                                             -> NIL
   METHOD  visualNavigation              // (  )                                               -> lBool

   ENDCLASS


METHOD QTextCursor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCursor( ... )
   RETURN Self


METHOD QTextCursor:anchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_anchor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:atBlockEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atBlockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:atBlockStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atBlockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:atStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:beginEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_beginEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:block( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFromPointer( Qt_QTextCursor_block( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:blockCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextCursor_blockCharFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:blockFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFormatFromPointer( Qt_QTextCursor_blockFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:blockNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_blockNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:charFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextCursor_charFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:clearSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:columnNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_columnNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:createList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_createList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_createList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:currentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFrameFromPointer( Qt_QTextCursor_currentFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:currentList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextListFromPointer( Qt_QTextCursor_currentList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:deleteChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_deleteChar( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:deletePreviousChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_deletePreviousChar( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QTextCursor_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:endEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_endEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:hasComplexSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_hasComplexSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:hasSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_hasSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertBlock( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertBlock_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertBlock_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextCursor_insertBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertFragment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertFragment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextFrameFromPointer( Qt_QTextCursor_insertFrame( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertImage_3( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertImage_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertImage( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTEXTIMAGEFORMAT"
            RETURN Qt_QTextCursor_insertImage_1( ::pPtr, ... )
         CASE "QIMAGE"
            RETURN Qt_QTextCursor_insertImage_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_insertList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_insertList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:insertText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertText_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:isCopyOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_isCopyOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:joinPreviousEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_joinPreviousEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:mergeBlockCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeBlockCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:mergeBlockFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeBlockFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:mergeCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:movePosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:removeSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_removeSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:select( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_select( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:selectedTableCells( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTextCursor_selectedTableCells( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:selection( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFragmentFromPointer( Qt_QTextCursor_selection( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:selectionEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectionEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:selectionStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectionStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:setBlockCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setBlockCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:setBlockFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setBlockFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:setCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:setPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:setVisualNavigation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setVisualNavigation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextCursor:visualNavigation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_visualNavigation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

