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

   METHOD  addRow                        // ( oQWidget, oQWidget )                             -> NIL
                                         // ( oQWidget )                                       -> NIL
                                         // ( cLabelText, oQWidget )                           -> NIL
   METHOD  fieldGrowthPolicy             // (  )                                               -> nFieldGrowthPolicy
   METHOD  formAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  getItemPosition               // ( nIndex, @nRowPtr, @nRolePtr )                    -> NIL
   METHOD  getLayoutPosition             // ( oQLayout, @nRowPtr, @nRolePtr )                  -> NIL
   METHOD  getWidgetPosition             // ( oQWidget, @nRowPtr, @nRolePtr )                  -> NIL
   METHOD  horizontalSpacing             // (  )                                               -> nInt
   METHOD  insertRow                     // ( nRow, oQWidget, oQWidget )                       -> NIL
                                         // ( nRow, oQWidget, oQLayout )                       -> NIL
                                         // ( nRow, oQWidget )                                 -> NIL
                                         // ( nRow, cLabelText, oQWidget )                     -> NIL
                                         // ( nRow, cLabelText, oQLayout )                     -> NIL
                                         // ( nRow, oQLayout )                                 -> NIL
   METHOD  itemAt                        // ( nRow, nRole )                                    -> oQLayoutItem
   METHOD  labelAlignment                // (  )                                               -> nQt_Alignment
   METHOD  labelForField                 // ( oQWidget )                                       -> oQWidget
                                         // ( oQLayout )                                       -> oQWidget
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  rowWrapPolicy                 // (  )                                               -> nRowWrapPolicy
   METHOD  setFieldGrowthPolicy          // ( nPolicy )                                        -> NIL
   METHOD  setFormAlignment              // ( nAlignment )                                     -> NIL
   METHOD  setHorizontalSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setItem                       // ( nRow, nRole, oQLayoutItem )                      -> NIL
   METHOD  setLabelAlignment             // ( nAlignment )                                     -> NIL
   METHOD  setLayout                     // ( nRow, nRole, oQLayout )                          -> NIL
   METHOD  setRowWrapPolicy              // ( nPolicy )                                        -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setVerticalSpacing            // ( nSpacing )                                       -> NIL
   METHOD  setWidget                     // ( nRow, nRole, oQWidget )                          -> NIL
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  verticalSpacing               // (  )                                               -> nInt

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


METHOD QFormLayout:fieldGrowthPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_fieldGrowthPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:formAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_formAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:getItemPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getItemPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:getLayoutPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getLayoutPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:getWidgetPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getWidgetPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:horizontalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_horizontalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:insertRow( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 3 ) )
         CASE "QLAYOUT"
            RETURN Qt_QFormLayout_insertRow_4( ::pPtr, ... )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow_3( ::pPtr, ... )
         ENDSWITCH
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) ) + __objGetClsName( hb_pvalue( 3 ) )
         CASE "QWIDGETQLAYOUT"
            RETURN Qt_QFormLayout_insertRow_1( ::pPtr, ... )
         CASE "QWIDGETQWIDGET"
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


METHOD QFormLayout:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QLayoutItem():from( Qt_QFormLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:labelAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_labelAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QFormLayout:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:rowWrapPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_rowWrapPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setFieldGrowthPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setFieldGrowthPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setFormAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setFormAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setHorizontalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setHorizontalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setLabelAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setLabelAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setLayout( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setRowWrapPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setRowWrapPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setVerticalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setVerticalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:setWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFormLayout:verticalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_verticalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

