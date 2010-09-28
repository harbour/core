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


FUNCTION QGraphicsTextItem( ... )
   RETURN HB_QGraphicsTextItem():new( ... )


CREATE CLASS QGraphicsTextItem INHERIT HbQtObjectHandler, HB_QObject, HB_QGraphicsItem FUNCTION HB_QGraphicsTextItem

   METHOD  new( ... )

   METHOD  adjustSize()
   METHOD  defaultTextColor()
   METHOD  document()
   METHOD  font()
   METHOD  openExternalLinks()
   METHOD  setDefaultTextColor( pCol )
   METHOD  setDocument( pDocument )
   METHOD  setFont( pFont )
   METHOD  setHtml( cText )
   METHOD  setOpenExternalLinks( lOpen )
   METHOD  setPlainText( cText )
   METHOD  setTabChangesFocus( lB )
   METHOD  setTextCursor( pCursor )
   METHOD  setTextInteractionFlags( nFlags )
   METHOD  setTextWidth( nWidth )
   METHOD  tabChangesFocus()
   METHOD  textCursor()
   METHOD  textInteractionFlags()
   METHOD  textWidth()
   METHOD  toHtml()
   METHOD  toPlainText()

   ENDCLASS


METHOD QGraphicsTextItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsTextItem( ... )
   RETURN Self


METHOD QGraphicsTextItem:adjustSize()
   RETURN Qt_QGraphicsTextItem_adjustSize( ::pPtr )


METHOD QGraphicsTextItem:defaultTextColor()
   RETURN HB_QColor():from( Qt_QGraphicsTextItem_defaultTextColor( ::pPtr ) )


METHOD QGraphicsTextItem:document()
   RETURN HB_QTextDocument():from( Qt_QGraphicsTextItem_document( ::pPtr ) )


METHOD QGraphicsTextItem:font()
   RETURN HB_QFont():from( Qt_QGraphicsTextItem_font( ::pPtr ) )


METHOD QGraphicsTextItem:openExternalLinks()
   RETURN Qt_QGraphicsTextItem_openExternalLinks( ::pPtr )


METHOD QGraphicsTextItem:setDefaultTextColor( pCol )
   RETURN Qt_QGraphicsTextItem_setDefaultTextColor( ::pPtr, hbqt_ptr( pCol ) )


METHOD QGraphicsTextItem:setDocument( pDocument )
   RETURN Qt_QGraphicsTextItem_setDocument( ::pPtr, hbqt_ptr( pDocument ) )


METHOD QGraphicsTextItem:setFont( pFont )
   RETURN Qt_QGraphicsTextItem_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QGraphicsTextItem:setHtml( cText )
   RETURN Qt_QGraphicsTextItem_setHtml( ::pPtr, cText )


METHOD QGraphicsTextItem:setOpenExternalLinks( lOpen )
   RETURN Qt_QGraphicsTextItem_setOpenExternalLinks( ::pPtr, lOpen )


METHOD QGraphicsTextItem:setPlainText( cText )
   RETURN Qt_QGraphicsTextItem_setPlainText( ::pPtr, cText )


METHOD QGraphicsTextItem:setTabChangesFocus( lB )
   RETURN Qt_QGraphicsTextItem_setTabChangesFocus( ::pPtr, lB )


METHOD QGraphicsTextItem:setTextCursor( pCursor )
   RETURN Qt_QGraphicsTextItem_setTextCursor( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QGraphicsTextItem:setTextInteractionFlags( nFlags )
   RETURN Qt_QGraphicsTextItem_setTextInteractionFlags( ::pPtr, nFlags )


METHOD QGraphicsTextItem:setTextWidth( nWidth )
   RETURN Qt_QGraphicsTextItem_setTextWidth( ::pPtr, nWidth )


METHOD QGraphicsTextItem:tabChangesFocus()
   RETURN Qt_QGraphicsTextItem_tabChangesFocus( ::pPtr )


METHOD QGraphicsTextItem:textCursor()
   RETURN HB_QTextCursor():from( Qt_QGraphicsTextItem_textCursor( ::pPtr ) )


METHOD QGraphicsTextItem:textInteractionFlags()
   RETURN Qt_QGraphicsTextItem_textInteractionFlags( ::pPtr )


METHOD QGraphicsTextItem:textWidth()
   RETURN Qt_QGraphicsTextItem_textWidth( ::pPtr )


METHOD QGraphicsTextItem:toHtml()
   RETURN Qt_QGraphicsTextItem_toHtml( ::pPtr )


METHOD QGraphicsTextItem:toPlainText()
   RETURN Qt_QGraphicsTextItem_toPlainText( ::pPtr )

