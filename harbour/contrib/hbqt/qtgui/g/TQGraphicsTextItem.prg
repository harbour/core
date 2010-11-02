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


FUNCTION QGraphicsTextItem( ... )
   RETURN HB_QGraphicsTextItem():new( ... )

FUNCTION QGraphicsTextItemFromPointer( ... )
   RETURN HB_QGraphicsTextItem():fromPointer( ... )


CREATE CLASS QGraphicsTextItem INHERIT HbQtObjectHandler, HB_QObject, HB_QGraphicsItem FUNCTION HB_QGraphicsTextItem

   METHOD  new( ... )

   METHOD  adjustSize                    // (  )                                               -> NIL
   METHOD  defaultTextColor              // (  )                                               -> oQColor
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  openExternalLinks             // (  )                                               -> lBool
   METHOD  setDefaultTextColor           // ( oQColor )                                        -> NIL
   METHOD  setDocument                   // ( oQTextDocument )                                 -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setHtml                       // ( cText )                                          -> NIL
   METHOD  setOpenExternalLinks          // ( lOpen )                                          -> NIL
   METHOD  setPlainText                  // ( cText )                                          -> NIL
   METHOD  setTabChangesFocus            // ( lB )                                             -> NIL
   METHOD  setTextCursor                 // ( oQTextCursor )                                   -> NIL
   METHOD  setTextInteractionFlags       // ( nFlags )                                         -> NIL
   METHOD  setTextWidth                  // ( nWidth )                                         -> NIL
   METHOD  tabChangesFocus               // (  )                                               -> lBool
   METHOD  textCursor                    // (  )                                               -> oQTextCursor
   METHOD  textInteractionFlags          // (  )                                               -> nQt_TextInteractionFlags
   METHOD  textWidth                     // (  )                                               -> nQreal
   METHOD  toHtml                        // (  )                                               -> cQString
   METHOD  toPlainText                   // (  )                                               -> cQString

   ENDCLASS


METHOD QGraphicsTextItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsTextItem( ... )
   RETURN Self


METHOD QGraphicsTextItem:adjustSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_adjustSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:defaultTextColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QGraphicsTextItem_defaultTextColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QGraphicsTextItem_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QGraphicsTextItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:openExternalLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_openExternalLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setDefaultTextColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setDefaultTextColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setDocument( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setDocument( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setOpenExternalLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setOpenExternalLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setTabChangesFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setTabChangesFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setTextCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setTextCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setTextInteractionFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setTextInteractionFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:setTextWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsTextItem_setTextWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:tabChangesFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_tabChangesFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:textCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCursorFromPointer( Qt_QGraphicsTextItem_textCursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:textInteractionFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_textInteractionFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:textWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_textWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:toHtml( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_toHtml( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsTextItem:toPlainText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsTextItem_toPlainText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

