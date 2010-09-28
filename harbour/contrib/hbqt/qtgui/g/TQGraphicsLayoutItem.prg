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


FUNCTION QGraphicsLayoutItem( ... )
   RETURN HB_QGraphicsLayoutItem():new( ... )


CREATE CLASS QGraphicsLayoutItem INHERIT HbQtObjectHandler FUNCTION HB_QGraphicsLayoutItem

   METHOD  new( ... )

   METHOD  contentsRect()
   METHOD  effectiveSizeHint( nWhich, pConstraint )
   METHOD  geometry()
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  graphicsItem()
   METHOD  isLayout()
   METHOD  maximumHeight()
   METHOD  maximumSize()
   METHOD  maximumWidth()
   METHOD  minimumHeight()
   METHOD  minimumSize()
   METHOD  minimumWidth()
   METHOD  ownedByLayout()
   METHOD  parentLayoutItem()
   METHOD  preferredHeight()
   METHOD  preferredSize()
   METHOD  preferredWidth()
   METHOD  setGeometry( pRect )
   METHOD  setMaximumHeight( nHeight )
   METHOD  setMaximumSize( ... )
   METHOD  setMaximumWidth( nWidth )
   METHOD  setMinimumHeight( nHeight )
   METHOD  setMinimumSize( ... )
   METHOD  setMinimumWidth( nWidth )
   METHOD  setParentLayoutItem( pParent )
   METHOD  setPreferredHeight( nHeight )
   METHOD  setPreferredSize( ... )
   METHOD  setPreferredWidth( nWidth )
   METHOD  setSizePolicy( ... )
   METHOD  sizePolicy()
   METHOD  updateGeometry()

   ENDCLASS


METHOD QGraphicsLayoutItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLayoutItem( ... )
   RETURN Self


METHOD QGraphicsLayoutItem:contentsRect()
   RETURN HB_QRectF():from( Qt_QGraphicsLayoutItem_contentsRect( ::pPtr ) )


METHOD QGraphicsLayoutItem:effectiveSizeHint( nWhich, pConstraint )
   RETURN HB_QSizeF():from( Qt_QGraphicsLayoutItem_effectiveSizeHint( ::pPtr, nWhich, hbqt_ptr( pConstraint ) ) )


METHOD QGraphicsLayoutItem:geometry()
   RETURN HB_QRectF():from( Qt_QGraphicsLayoutItem_geometry( ::pPtr ) )


METHOD QGraphicsLayoutItem:getContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QGraphicsLayoutItem_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QGraphicsLayoutItem:graphicsItem()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsLayoutItem_graphicsItem( ::pPtr ) )


METHOD QGraphicsLayoutItem:isLayout()
   RETURN Qt_QGraphicsLayoutItem_isLayout( ::pPtr )


METHOD QGraphicsLayoutItem:maximumHeight()
   RETURN Qt_QGraphicsLayoutItem_maximumHeight( ::pPtr )


METHOD QGraphicsLayoutItem:maximumSize()
   RETURN HB_QSizeF():from( Qt_QGraphicsLayoutItem_maximumSize( ::pPtr ) )


METHOD QGraphicsLayoutItem:maximumWidth()
   RETURN Qt_QGraphicsLayoutItem_maximumWidth( ::pPtr )


METHOD QGraphicsLayoutItem:minimumHeight()
   RETURN Qt_QGraphicsLayoutItem_minimumHeight( ::pPtr )


METHOD QGraphicsLayoutItem:minimumSize()
   RETURN HB_QSizeF():from( Qt_QGraphicsLayoutItem_minimumSize( ::pPtr ) )


METHOD QGraphicsLayoutItem:minimumWidth()
   RETURN Qt_QGraphicsLayoutItem_minimumWidth( ::pPtr )


METHOD QGraphicsLayoutItem:ownedByLayout()
   RETURN Qt_QGraphicsLayoutItem_ownedByLayout( ::pPtr )


METHOD QGraphicsLayoutItem:parentLayoutItem()
   RETURN HB_QGraphicsLayoutItem():from( Qt_QGraphicsLayoutItem_parentLayoutItem( ::pPtr ) )


METHOD QGraphicsLayoutItem:preferredHeight()
   RETURN Qt_QGraphicsLayoutItem_preferredHeight( ::pPtr )


METHOD QGraphicsLayoutItem:preferredSize()
   RETURN HB_QSizeF():from( Qt_QGraphicsLayoutItem_preferredSize( ::pPtr ) )


METHOD QGraphicsLayoutItem:preferredWidth()
   RETURN Qt_QGraphicsLayoutItem_preferredWidth( ::pPtr )


METHOD QGraphicsLayoutItem:setGeometry( pRect )
   RETURN Qt_QGraphicsLayoutItem_setGeometry( ::pPtr, hbqt_ptr( pRect ) )


METHOD QGraphicsLayoutItem:setMaximumHeight( nHeight )
   RETURN Qt_QGraphicsLayoutItem_setMaximumHeight( ::pPtr, nHeight )


METHOD QGraphicsLayoutItem:setMaximumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMaximumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsLayoutItem:setMaximumWidth( nWidth )
   RETURN Qt_QGraphicsLayoutItem_setMaximumWidth( ::pPtr, nWidth )


METHOD QGraphicsLayoutItem:setMinimumHeight( nHeight )
   RETURN Qt_QGraphicsLayoutItem_setMinimumHeight( ::pPtr, nHeight )


METHOD QGraphicsLayoutItem:setMinimumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setMinimumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsLayoutItem:setMinimumWidth( nWidth )
   RETURN Qt_QGraphicsLayoutItem_setMinimumWidth( ::pPtr, nWidth )


METHOD QGraphicsLayoutItem:setParentLayoutItem( pParent )
   RETURN Qt_QGraphicsLayoutItem_setParentLayoutItem( ::pPtr, hbqt_ptr( pParent ) )


METHOD QGraphicsLayoutItem:setPreferredHeight( nHeight )
   RETURN Qt_QGraphicsLayoutItem_setPreferredHeight( ::pPtr, nHeight )


METHOD QGraphicsLayoutItem:setPreferredSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setPreferredSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsLayoutItem:setPreferredWidth( nWidth )
   RETURN Qt_QGraphicsLayoutItem_setPreferredWidth( ::pPtr, nWidth )


METHOD QGraphicsLayoutItem:setSizePolicy( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayoutItem_setSizePolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsLayoutItem:sizePolicy()
   RETURN HB_QSizePolicy():from( Qt_QGraphicsLayoutItem_sizePolicy( ::pPtr ) )


METHOD QGraphicsLayoutItem:updateGeometry()
   RETURN Qt_QGraphicsLayoutItem_updateGeometry( ::pPtr )

