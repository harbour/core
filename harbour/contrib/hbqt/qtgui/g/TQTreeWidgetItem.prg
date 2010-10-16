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


FUNCTION QTreeWidgetItem( ... )
   RETURN HB_QTreeWidgetItem():new( ... )

FUNCTION QTreeWidgetItemFrom( ... )
   RETURN HB_QTreeWidgetItem():from( ... )

FUNCTION QTreeWidgetItemFromPointer( ... )
   RETURN HB_QTreeWidgetItem():fromPointer( ... )


CREATE CLASS QTreeWidgetItem INHERIT HbQtObjectHandler FUNCTION HB_QTreeWidgetItem

   METHOD  new( ... )

   METHOD  addChild                      // ( oQTreeWidgetItem )                               -> NIL
   METHOD  background                    // ( nColumn )                                        -> oQBrush
   METHOD  checkState                    // ( nColumn )                                        -> nQt_CheckState
   METHOD  child                         // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  childCount                    // (  )                                               -> nInt
   METHOD  childIndicatorPolicy          // (  )                                               -> nQTreeWidgetItem_ChildIndicatorPolicy
   METHOD  clone                         // (  )                                               -> oQTreeWidgetItem
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  data                          // ( nColumn, nRole )                                 -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // ( nColumn )                                        -> oQFont
   METHOD  foreground                    // ( nColumn )                                        -> oQBrush
   METHOD  icon                          // ( nColumn )                                        -> oQIcon
   METHOD  indexOfChild                  // ( oQTreeWidgetItem )                               -> nInt
   METHOD  insertChild                   // ( nIndex, oQTreeWidgetItem )                       -> NIL
   METHOD  isDisabled                    // (  )                                               -> lBool
   METHOD  isExpanded                    // (  )                                               -> lBool
   METHOD  isFirstColumnSpanned          // (  )                                               -> lBool
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isSelected                    // (  )                                               -> lBool
   METHOD  parent                        // (  )                                               -> oQTreeWidgetItem
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  removeChild                   // ( oQTreeWidgetItem )                               -> NIL
   METHOD  setBackground                 // ( nColumn, oQBrush )                               -> NIL
   METHOD  setCheckState                 // ( nColumn, nState )                                -> NIL
   METHOD  setChildIndicatorPolicy       // ( nPolicy )                                        -> NIL
   METHOD  setData                       // ( nColumn, nRole, oQVariant )                      -> NIL
   METHOD  setDisabled                   // ( lDisabled )                                      -> NIL
   METHOD  setExpanded                   // ( lExpand )                                        -> NIL
   METHOD  setFirstColumnSpanned         // ( lSpan )                                          -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( nColumn, oQFont )                                -> NIL
   METHOD  setForeground                 // ( nColumn, oQBrush )                               -> NIL
   METHOD  setHidden                     // ( lHide )                                          -> NIL
   METHOD  setIcon                       // ( nColumn, coQIcon )                               -> NIL
   METHOD  setSelected                   // ( lSelect )                                        -> NIL
   METHOD  setSizeHint                   // ( nColumn, oQSize )                                -> NIL
   METHOD  setStatusTip                  // ( nColumn, cStatusTip )                            -> NIL
   METHOD  setText                       // ( nColumn, cText )                                 -> NIL
   METHOD  setTextAlignment              // ( nColumn, nAlignment )                            -> NIL
   METHOD  setToolTip                    // ( nColumn, cToolTip )                              -> NIL
   METHOD  setWhatsThis                  // ( nColumn, cWhatsThis )                            -> NIL
   METHOD  sizeHint                      // ( nColumn )                                        -> oQSize
   METHOD  sortChildren                  // ( nColumn, nOrder )                                -> NIL
   METHOD  statusTip                     // ( nColumn )                                        -> cQString
   METHOD  takeChild                     // ( nIndex )                                         -> oQTreeWidgetItem
   METHOD  takeChildren                  // (  )                                               -> oQList_QTreeWidgetItem
   METHOD  text                          // ( nColumn )                                        -> cQString
   METHOD  textAlignment                 // ( nColumn )                                        -> nInt
   METHOD  toolTip                       // ( nColumn )                                        -> cQString
   METHOD  treeWidget                    // (  )                                               -> oQTreeWidget
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // ( nColumn )                                        -> cQString

   ENDCLASS


METHOD QTreeWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeWidgetItem( ... )
   RETURN Self


METHOD QTreeWidgetItem:addChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_addChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:background( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QTreeWidgetItem_background( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:checkState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_checkState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:child( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_child( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:childCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_childCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:childIndicatorPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_childIndicatorPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:data( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QTreeWidgetItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:font( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QTreeWidgetItem_font( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:foreground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QTreeWidgetItem_foreground( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:icon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTreeWidgetItem_icon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:indexOfChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_indexOfChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:insertChild( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_insertChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:isDisabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isDisabled( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:isExpanded( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isExpanded( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:isFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isFirstColumnSpanned( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:isSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_isSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:removeChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_removeChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setBackground( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setCheckState( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setChildIndicatorPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setChildIndicatorPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTreeWidgetItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setDisabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setDisabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setExpanded( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setExpanded( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setFirstColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setFont( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setForeground( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QTreeWidgetItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_setSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:sizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QTreeWidgetItem_sizeHint( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:sortChildren( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeWidgetItem_sortChildren( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:statusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_statusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:takeChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTreeWidgetItemFromPointer( Qt_QTreeWidgetItem_takeChild( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:takeChildren( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTreeWidgetItem_takeChildren( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_text( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:textAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_textAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:toolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_toolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:treeWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTreeWidgetFromPointer( Qt_QTreeWidgetItem_treeWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeWidgetItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTreeWidgetItem:whatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeWidgetItem_whatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

