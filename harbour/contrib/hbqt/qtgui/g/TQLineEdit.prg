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


FUNCTION QLineEdit( ... )
   RETURN HB_QLineEdit():new( ... )

FUNCTION QLineEditFromPointer( ... )
   RETURN HB_QLineEdit():fromPointer( ... )


CREATE CLASS QLineEdit INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QLineEdit

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  backspace                     // (  )                                               -> NIL
   METHOD  completer                     // (  )                                               -> oQCompleter
   METHOD  createStandardContextMenu     // (  )                                               -> oQMenu
   METHOD  cursorBackward                // ( lMark, nSteps )                                  -> NIL
   METHOD  cursorForward                 // ( lMark, nSteps )                                  -> NIL
   METHOD  cursorPosition                // (  )                                               -> nInt
   METHOD  cursorPositionAt              // ( oQPoint )                                        -> nInt
   METHOD  cursorWordBackward            // ( lMark )                                          -> NIL
   METHOD  cursorWordForward             // ( lMark )                                          -> NIL
   METHOD  del                           // (  )                                               -> NIL
   METHOD  deselect                      // (  )                                               -> NIL
   METHOD  displayText                   // (  )                                               -> cQString
   METHOD  dragEnabled                   // (  )                                               -> lBool
   METHOD  echoMode                      // (  )                                               -> nEchoMode
   METHOD  end                           // ( lMark )                                          -> NIL
   METHOD  getTextMargins                // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  hasAcceptableInput            // (  )                                               -> lBool
   METHOD  hasFrame                      // (  )                                               -> lBool
   METHOD  hasSelectedText               // (  )                                               -> lBool
   METHOD  home                          // ( lMark )                                          -> NIL
   METHOD  inputMask                     // (  )                                               -> cQString
   METHOD  insert                        // ( cNewText )                                       -> NIL
   METHOD  isModified                    // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  isRedoAvailable               // (  )                                               -> lBool
   METHOD  isUndoAvailable               // (  )                                               -> lBool
   METHOD  maxLength                     // (  )                                               -> nInt
   METHOD  minimumSizeHint               // (  )                                               -> oQSize
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  selectionStart                // (  )                                               -> nInt
   METHOD  setAlignment                  // ( nFlag )                                          -> NIL
   METHOD  setCompleter                  // ( oQCompleter )                                    -> NIL
   METHOD  setCursorPosition             // ( nInt )                                           -> NIL
   METHOD  setDragEnabled                // ( lB )                                             -> NIL
   METHOD  setEchoMode                   // ( nEchoMode )                                      -> NIL
   METHOD  setFrame                      // ( lBool )                                          -> NIL
   METHOD  setInputMask                  // ( cInputMask )                                     -> NIL
   METHOD  setMaxLength                  // ( nInt )                                           -> NIL
   METHOD  setModified                   // ( lBool )                                          -> NIL
   METHOD  setReadOnly                   // ( lBool )                                          -> NIL
   METHOD  setSelection                  // ( nStart, nLength )                                -> NIL
   METHOD  setTextMargins                // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setValidator                  // ( oQValidator )                                    -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  text                          // (  )                                               -> cQString
   METHOD  validator                     // (  )                                               -> oQValidator
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  copy                          // (  )                                               -> NIL
   METHOD  cut                           // (  )                                               -> NIL
   METHOD  paste                         // (  )                                               -> NIL
   METHOD  redo                          // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  setText                       // ( cQString )                                       -> NIL
   METHOD  undo                          // (  )                                               -> NIL

   ENDCLASS


METHOD QLineEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLineEdit( ... )
   RETURN Self


METHOD QLineEdit:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:backspace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_backspace( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:completer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCompleterFromPointer( Qt_QLineEdit_completer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:createStandardContextMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QLineEdit_createStandardContextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorBackward( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_cursorBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorForward( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_cursorForward( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorForward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_cursorPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorPositionAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorPositionAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorWordBackward( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorWordBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cursorWordForward( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorWordForward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:del( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_del( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:deselect( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_deselect( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:displayText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_displayText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:dragEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_dragEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:echoMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_echoMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:end( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_end( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:getTextMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLineEdit_getTextMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:hasAcceptableInput( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasAcceptableInput( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:hasFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:hasSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:home( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_home( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:inputMask( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_inputMask( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:insert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:isModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isModified( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:isRedoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isRedoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:isUndoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isUndoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:maxLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_maxLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:minimumSizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLineEdit_minimumSizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:selectionStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectionStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setCompleter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setCompleter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setCursorPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setDragEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setDragEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setEchoMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setEchoMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setInputMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setInputMask( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setMaxLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setMaxLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setModified( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setModified( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setSelection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_setSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setTextMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLineEdit_setTextMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setValidator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setValidator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QLineEdit_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:validator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QValidatorFromPointer( Qt_QLineEdit_validator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:copy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_copy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:cut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_cut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:paste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_paste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:redo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_redo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLineEdit:undo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_undo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

