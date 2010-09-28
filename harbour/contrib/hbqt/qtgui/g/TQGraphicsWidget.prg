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


FUNCTION QGraphicsWidget( ... )
   RETURN HB_QGraphicsWidget():new( ... )


CREATE CLASS QGraphicsWidget INHERIT HbQtObjectHandler, HB_QObject, HB_QGraphicsItem, HB_QGraphicsLayoutItem FUNCTION HB_QGraphicsWidget

   METHOD  new( ... )

   METHOD  actions()
   METHOD  addAction( pAction )
   METHOD  adjustSize()
   METHOD  focusPolicy()
   METHOD  focusWidget()
   METHOD  font()
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  getWindowFrameMargins( nLeft, nTop, nRight, nBottom )
   METHOD  grabShortcut( pSequence, nContext )
   METHOD  insertAction( pBefore, pAction )
   METHOD  isActiveWindow()
   METHOD  layout()
   METHOD  layoutDirection()
   METHOD  palette()
   METHOD  rect()
   METHOD  releaseShortcut( nId )
   METHOD  removeAction( pAction )
   METHOD  resize( ... )
   METHOD  setAttribute( nAttribute, lOn )
   METHOD  setContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  setFocusPolicy( nPolicy )
   METHOD  setFont( pFont )
   METHOD  setGeometry( ... )
   METHOD  setLayout( pLayout )
   METHOD  setLayoutDirection( nDirection )
   METHOD  setPalette( pPalette )
   METHOD  setShortcutAutoRepeat( nId, lEnabled )
   METHOD  setShortcutEnabled( nId, lEnabled )
   METHOD  setStyle( pStyle )
   METHOD  setWindowFlags( nWFlags )
   METHOD  setWindowFrameMargins( nLeft, nTop, nRight, nBottom )
   METHOD  setWindowTitle( cTitle )
   METHOD  size()
   METHOD  style()
   METHOD  testAttribute( nAttribute )
   METHOD  unsetLayoutDirection()
   METHOD  unsetWindowFrameMargins()
   METHOD  windowFlags()
   METHOD  windowFrameGeometry()
   METHOD  windowFrameRect()
   METHOD  windowTitle()
   METHOD  windowType()
   METHOD  setTabOrder( pFirst, pSecond )
   METHOD  close()

   ENDCLASS


METHOD QGraphicsWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsWidget( ... )
   RETURN Self


METHOD QGraphicsWidget:actions()
   RETURN HB_QList():from( Qt_QGraphicsWidget_actions( ::pPtr ) )


METHOD QGraphicsWidget:addAction( pAction )
   RETURN Qt_QGraphicsWidget_addAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QGraphicsWidget:adjustSize()
   RETURN Qt_QGraphicsWidget_adjustSize( ::pPtr )


METHOD QGraphicsWidget:focusPolicy()
   RETURN Qt_QGraphicsWidget_focusPolicy( ::pPtr )


METHOD QGraphicsWidget:focusWidget()
   RETURN HB_QGraphicsWidget():from( Qt_QGraphicsWidget_focusWidget( ::pPtr ) )


METHOD QGraphicsWidget:font()
   RETURN HB_QFont():from( Qt_QGraphicsWidget_font( ::pPtr ) )


METHOD QGraphicsWidget:getContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QGraphicsWidget_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QGraphicsWidget:getWindowFrameMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QGraphicsWidget_getWindowFrameMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QGraphicsWidget:grabShortcut( pSequence, nContext )
   RETURN Qt_QGraphicsWidget_grabShortcut( ::pPtr, hbqt_ptr( pSequence ), nContext )


METHOD QGraphicsWidget:insertAction( pBefore, pAction )
   RETURN Qt_QGraphicsWidget_insertAction( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAction ) )


METHOD QGraphicsWidget:isActiveWindow()
   RETURN Qt_QGraphicsWidget_isActiveWindow( ::pPtr )


METHOD QGraphicsWidget:layout()
   RETURN HB_QGraphicsLayout():from( Qt_QGraphicsWidget_layout( ::pPtr ) )


METHOD QGraphicsWidget:layoutDirection()
   RETURN Qt_QGraphicsWidget_layoutDirection( ::pPtr )


METHOD QGraphicsWidget:palette()
   RETURN HB_QPalette():from( Qt_QGraphicsWidget_palette( ::pPtr ) )


METHOD QGraphicsWidget:rect()
   RETURN HB_QRectF():from( Qt_QGraphicsWidget_rect( ::pPtr ) )


METHOD QGraphicsWidget:releaseShortcut( nId )
   RETURN Qt_QGraphicsWidget_releaseShortcut( ::pPtr, nId )


METHOD QGraphicsWidget:removeAction( pAction )
   RETURN Qt_QGraphicsWidget_removeAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QGraphicsWidget:resize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_resize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsWidget:setAttribute( nAttribute, lOn )
   RETURN Qt_QGraphicsWidget_setAttribute( ::pPtr, nAttribute, lOn )


METHOD QGraphicsWidget:setContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QGraphicsWidget_setContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QGraphicsWidget:setFocusPolicy( nPolicy )
   RETURN Qt_QGraphicsWidget_setFocusPolicy( ::pPtr, nPolicy )


METHOD QGraphicsWidget:setFont( pFont )
   RETURN Qt_QGraphicsWidget_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QGraphicsWidget:setGeometry( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_setGeometry_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsWidget:setLayout( pLayout )
   RETURN Qt_QGraphicsWidget_setLayout( ::pPtr, hbqt_ptr( pLayout ) )


METHOD QGraphicsWidget:setLayoutDirection( nDirection )
   RETURN Qt_QGraphicsWidget_setLayoutDirection( ::pPtr, nDirection )


METHOD QGraphicsWidget:setPalette( pPalette )
   RETURN Qt_QGraphicsWidget_setPalette( ::pPtr, hbqt_ptr( pPalette ) )


METHOD QGraphicsWidget:setShortcutAutoRepeat( nId, lEnabled )
   RETURN Qt_QGraphicsWidget_setShortcutAutoRepeat( ::pPtr, nId, lEnabled )


METHOD QGraphicsWidget:setShortcutEnabled( nId, lEnabled )
   RETURN Qt_QGraphicsWidget_setShortcutEnabled( ::pPtr, nId, lEnabled )


METHOD QGraphicsWidget:setStyle( pStyle )
   RETURN Qt_QGraphicsWidget_setStyle( ::pPtr, hbqt_ptr( pStyle ) )


METHOD QGraphicsWidget:setWindowFlags( nWFlags )
   RETURN Qt_QGraphicsWidget_setWindowFlags( ::pPtr, nWFlags )


METHOD QGraphicsWidget:setWindowFrameMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QGraphicsWidget_setWindowFrameMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QGraphicsWidget:setWindowTitle( cTitle )
   RETURN Qt_QGraphicsWidget_setWindowTitle( ::pPtr, cTitle )


METHOD QGraphicsWidget:size()
   RETURN HB_QSizeF():from( Qt_QGraphicsWidget_size( ::pPtr ) )


METHOD QGraphicsWidget:style()
   RETURN HB_QStyle():from( Qt_QGraphicsWidget_style( ::pPtr ) )


METHOD QGraphicsWidget:testAttribute( nAttribute )
   RETURN Qt_QGraphicsWidget_testAttribute( ::pPtr, nAttribute )


METHOD QGraphicsWidget:unsetLayoutDirection()
   RETURN Qt_QGraphicsWidget_unsetLayoutDirection( ::pPtr )


METHOD QGraphicsWidget:unsetWindowFrameMargins()
   RETURN Qt_QGraphicsWidget_unsetWindowFrameMargins( ::pPtr )


METHOD QGraphicsWidget:windowFlags()
   RETURN Qt_QGraphicsWidget_windowFlags( ::pPtr )


METHOD QGraphicsWidget:windowFrameGeometry()
   RETURN HB_QRectF():from( Qt_QGraphicsWidget_windowFrameGeometry( ::pPtr ) )


METHOD QGraphicsWidget:windowFrameRect()
   RETURN HB_QRectF():from( Qt_QGraphicsWidget_windowFrameRect( ::pPtr ) )


METHOD QGraphicsWidget:windowTitle()
   RETURN Qt_QGraphicsWidget_windowTitle( ::pPtr )


METHOD QGraphicsWidget:windowType()
   RETURN Qt_QGraphicsWidget_windowType( ::pPtr )


METHOD QGraphicsWidget:setTabOrder( pFirst, pSecond )
   RETURN Qt_QGraphicsWidget_setTabOrder( ::pPtr, hbqt_ptr( pFirst ), hbqt_ptr( pSecond ) )


METHOD QGraphicsWidget:close()
   RETURN Qt_QGraphicsWidget_close( ::pPtr )

