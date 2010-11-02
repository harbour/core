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


FUNCTION QTextBlock( ... )
   RETURN HB_QTextBlock():new( ... )

FUNCTION QTextBlockFromPointer( ... )
   RETURN HB_QTextBlock():fromPointer( ... )


CREATE CLASS QTextBlock INHERIT HbQtObjectHandler FUNCTION HB_QTextBlock

   METHOD  new( ... )

   METHOD  blockFormat                   // (  )                                               -> oQTextBlockFormat
   METHOD  blockFormatIndex              // (  )                                               -> nInt
   METHOD  blockNumber                   // (  )                                               -> nInt
   METHOD  charFormat                    // (  )                                               -> oQTextCharFormat
   METHOD  charFormatIndex               // (  )                                               -> nInt
   METHOD  clearLayout                   // (  )                                               -> NIL
   METHOD  contains                      // ( nPosition )                                      -> lBool
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  firstLineNumber               // (  )                                               -> nInt
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  isVisible                     // (  )                                               -> lBool
   METHOD  layout                        // (  )                                               -> oQTextLayout
   METHOD  length                        // (  )                                               -> nInt
   METHOD  lineCount                     // (  )                                               -> nInt
   METHOD  next                          // (  )                                               -> oQTextBlock
   METHOD  position                      // (  )                                               -> nInt
   METHOD  previous                      // (  )                                               -> oQTextBlock
   METHOD  revision                      // (  )                                               -> nInt
   METHOD  setLineCount                  // ( nCount )                                         -> NIL
   METHOD  setRevision                   // ( nRev )                                           -> NIL
   METHOD  setUserData                   // ( oHBQTextBlockUserData )                          -> NIL
   METHOD  setUserState                  // ( nState )                                         -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textList                      // (  )                                               -> oQTextList
   METHOD  userData                      // (  )                                               -> oHBQTextBlockUserData
   METHOD  userState                     // (  )                                               -> nInt

   ENDCLASS


METHOD QTextBlock:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBlock( ... )
   RETURN Self


METHOD QTextBlock:blockFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFormatFromPointer( Qt_QTextBlock_blockFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:blockFormatIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_blockFormatIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:blockNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_blockNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:charFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextBlock_charFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:charFormatIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_charFormatIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:clearLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_clearLayout( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_contains( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QTextBlock_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:firstLineNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_firstLineNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:isVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_isVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:layout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextLayoutFromPointer( Qt_QTextBlock_layout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:lineCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_lineCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:next( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFromPointer( Qt_QTextBlock_next( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:previous( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFromPointer( Qt_QTextBlock_previous( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:revision( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_revision( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:setLineCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_setLineCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:setRevision( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_setRevision( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:setUserData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_setUserData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:setUserState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_setUserState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBlock_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:textList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextListFromPointer( Qt_QTextBlock_textList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:userData( ... )
   SWITCH PCount()
   CASE 0
      RETURN HBQTextBlockUserDataFromPointer( Qt_QTextBlock_userData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBlock:userState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBlock_userState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

