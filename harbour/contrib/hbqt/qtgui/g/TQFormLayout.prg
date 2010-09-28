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


FUNCTION QFormLayout( ... )
   RETURN HB_QFormLayout():new( ... )


CREATE CLASS QFormLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QFormLayout

   METHOD  new( ... )

   METHOD  addRow( ... )
   METHOD  fieldGrowthPolicy()
   METHOD  formAlignment()
   METHOD  getItemPosition( nIndex, nRowPtr, nRolePtr )
   METHOD  getLayoutPosition( pLayout, nRowPtr, nRolePtr )
   METHOD  getWidgetPosition( pWidget, nRowPtr, nRolePtr )
   METHOD  horizontalSpacing()
   METHOD  insertRow( ... )
   METHOD  itemAt( nRow, nRole )
   METHOD  labelAlignment()
   METHOD  labelForField( ... )
   METHOD  rowCount()
   METHOD  rowWrapPolicy()
   METHOD  setFieldGrowthPolicy( nPolicy )
   METHOD  setFormAlignment( nAlignment )
   METHOD  setHorizontalSpacing( nSpacing )
   METHOD  setItem( nRow, nRole, pItem )
   METHOD  setLabelAlignment( nAlignment )
   METHOD  setLayout( nRow, nRole, pLayout )
   METHOD  setRowWrapPolicy( nPolicy )
   METHOD  setSpacing( nSpacing )
   METHOD  setVerticalSpacing( nSpacing )
   METHOD  setWidget( nRow, nRole, pWidget )
   METHOD  spacing()
   METHOD  verticalSpacing()

   ENDCLASS


METHOD QFormLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFormLayout( ... )
   RETURN Self


METHOD QFormLayout:addRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFormLayout_addRow_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFormLayout_addRow( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_addRow_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:fieldGrowthPolicy()
   RETURN Qt_QFormLayout_fieldGrowthPolicy( ::pPtr )


METHOD QFormLayout:formAlignment()
   RETURN Qt_QFormLayout_formAlignment( ::pPtr )


METHOD QFormLayout:getItemPosition( nIndex, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getItemPosition( ::pPtr, nIndex, nRowPtr, nRolePtr )


METHOD QFormLayout:getLayoutPosition( pLayout, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getLayoutPosition( ::pPtr, hbqt_ptr( pLayout ), nRowPtr, nRolePtr )


METHOD QFormLayout:getWidgetPosition( pWidget, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getWidgetPosition( ::pPtr, hbqt_ptr( pWidget ), nRowPtr, nRolePtr )


METHOD QFormLayout:horizontalSpacing()
   RETURN Qt_QFormLayout_horizontalSpacing( ::pPtr )


METHOD QFormLayout:insertRow( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QSTRING"
            RETURN Qt_QFormLayout_insertRow_4( ::pPtr, ... )
         CASE "QSTRING"
            RETURN Qt_QFormLayout_insertRow_3( ::pPtr, ... )
         ENDSWITCH
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow_1( ::pPtr, ... )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow_2( ::pPtr, ... )
         CASE "QLAYOUT"
            RETURN Qt_QFormLayout_insertRow_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:itemAt( nRow, nRole )
   RETURN HB_QLayoutItem():from( Qt_QFormLayout_itemAt( ::pPtr, nRow, nRole ) )


METHOD QFormLayout:labelAlignment()
   RETURN Qt_QFormLayout_labelAlignment( ::pPtr )


METHOD QFormLayout:labelForField( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN HB_QWidget():from( Qt_QFormLayout_labelForField( ::pPtr, ... ) )
         CASE "QLAYOUT"
            RETURN HB_QWidget():from( Qt_QFormLayout_labelForField_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:rowCount()
   RETURN Qt_QFormLayout_rowCount( ::pPtr )


METHOD QFormLayout:rowWrapPolicy()
   RETURN Qt_QFormLayout_rowWrapPolicy( ::pPtr )


METHOD QFormLayout:setFieldGrowthPolicy( nPolicy )
   RETURN Qt_QFormLayout_setFieldGrowthPolicy( ::pPtr, nPolicy )


METHOD QFormLayout:setFormAlignment( nAlignment )
   RETURN Qt_QFormLayout_setFormAlignment( ::pPtr, nAlignment )


METHOD QFormLayout:setHorizontalSpacing( nSpacing )
   RETURN Qt_QFormLayout_setHorizontalSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setItem( nRow, nRole, pItem )
   RETURN Qt_QFormLayout_setItem( ::pPtr, nRow, nRole, hbqt_ptr( pItem ) )


METHOD QFormLayout:setLabelAlignment( nAlignment )
   RETURN Qt_QFormLayout_setLabelAlignment( ::pPtr, nAlignment )


METHOD QFormLayout:setLayout( nRow, nRole, pLayout )
   RETURN Qt_QFormLayout_setLayout( ::pPtr, nRow, nRole, hbqt_ptr( pLayout ) )


METHOD QFormLayout:setRowWrapPolicy( nPolicy )
   RETURN Qt_QFormLayout_setRowWrapPolicy( ::pPtr, nPolicy )


METHOD QFormLayout:setSpacing( nSpacing )
   RETURN Qt_QFormLayout_setSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setVerticalSpacing( nSpacing )
   RETURN Qt_QFormLayout_setVerticalSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setWidget( nRow, nRole, pWidget )
   RETURN Qt_QFormLayout_setWidget( ::pPtr, nRow, nRole, hbqt_ptr( pWidget ) )


METHOD QFormLayout:spacing()
   RETURN Qt_QFormLayout_spacing( ::pPtr )


METHOD QFormLayout:verticalSpacing()
   RETURN Qt_QFormLayout_verticalSpacing( ::pPtr )

