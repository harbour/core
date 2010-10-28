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

