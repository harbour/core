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


FUNCTION QTreeView( ... )
   RETURN HB_QTreeView():new( ... )

FUNCTION QTreeViewFrom( ... )
   RETURN HB_QTreeView():from( ... )

FUNCTION QTreeViewFromPointer( ... )
   RETURN HB_QTreeView():fromPointer( ... )


CREATE CLASS QTreeView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QTreeView

   METHOD  new( ... )

   METHOD  allColumnsShowFocus           // (  )                                               -> lBool
   METHOD  autoExpandDelay               // (  )                                               -> nInt
   METHOD  columnAt                      // ( nX )                                             -> nInt
   METHOD  columnViewportPosition        // ( nColumn )                                        -> nInt
   METHOD  columnWidth                   // ( nColumn )                                        -> nInt
   METHOD  expandsOnDoubleClick          // (  )                                               -> lBool
   METHOD  header                        // (  )                                               -> oQHeaderView
   METHOD  indentation                   // (  )                                               -> nInt
   METHOD  indexAbove                    // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  indexBelow                    // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  isAnimated                    // (  )                                               -> lBool
   METHOD  isColumnHidden                // ( nColumn )                                        -> lBool
   METHOD  isExpanded                    // ( oQModelIndex )                                   -> lBool
   METHOD  isFirstColumnSpanned          // ( nRow, oQModelIndex )                             -> lBool
   METHOD  isHeaderHidden                // (  )                                               -> lBool
   METHOD  isRowHidden                   // ( nRow, oQModelIndex )                             -> lBool
   METHOD  isSortingEnabled              // (  )                                               -> lBool
   METHOD  itemsExpandable               // (  )                                               -> lBool
   METHOD  rootIsDecorated               // (  )                                               -> lBool
   METHOD  scrollTo                      // ( oQModelIndex, nHint )                            -> NIL
   METHOD  setAllColumnsShowFocus        // ( lEnable )                                        -> NIL
   METHOD  setAnimated                   // ( lEnable )                                        -> NIL
   METHOD  setAutoExpandDelay            // ( nDelay )                                         -> NIL
   METHOD  setColumnHidden               // ( nColumn, lHide )                                 -> NIL
   METHOD  setColumnWidth                // ( nColumn, nWidth )                                -> NIL
   METHOD  setExpanded                   // ( oQModelIndex, lExpanded )                        -> NIL
   METHOD  setExpandsOnDoubleClick       // ( lEnable )                                        -> NIL
   METHOD  setFirstColumnSpanned         // ( nRow, oQModelIndex, lSpan )                      -> NIL
   METHOD  setHeader                     // ( oQHeaderView )                                   -> NIL
   METHOD  setHeaderHidden               // ( lHide )                                          -> NIL
   METHOD  setIndentation                // ( nI )                                             -> NIL
   METHOD  setItemsExpandable            // ( lEnable )                                        -> NIL
   METHOD  setRootIsDecorated            // ( lShow )                                          -> NIL
   METHOD  setRowHidden                  // ( nRow, oQModelIndex, lHide )                      -> NIL
   METHOD  setSortingEnabled             // ( lEnable )                                        -> NIL
   METHOD  setUniformRowHeights          // ( lUniform )                                       -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  sortByColumn                  // ( nColumn, nOrder )                                -> NIL
   METHOD  uniformRowHeights             // (  )                                               -> lBool
   METHOD  visualRect                    // ( oQModelIndex )                                   -> oQRect
   METHOD  wordWrap                      // (  )                                               -> lBool
   METHOD  collapse                      // ( oQModelIndex )                                   -> NIL
   METHOD  collapseAll                   // (  )                                               -> NIL
   METHOD  expand                        // ( oQModelIndex )                                   -> NIL
   METHOD  expandAll                     // (  )                                               -> NIL
   METHOD  expandToDepth                 // ( nDepth )                                         -> NIL
   METHOD  hideColumn                    // ( nColumn )                                        -> NIL
   METHOD  resizeColumnToContents        // ( nColumn )                                        -> NIL
   METHOD  showColumn                    // ( nColumn )                                        -> NIL

   ENDCLASS


METHOD QTreeView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeView( ... )
   RETURN Self


METHOD QTreeView:allColumnsShowFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_allColumnsShowFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:autoExpandDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_autoExpandDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:columnAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_columnAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:columnViewportPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_columnViewportPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:columnWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_columnWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:expandsOnDoubleClick( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_expandsOnDoubleClick( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:header( ... )
   SWITCH PCount()
   CASE 0
      RETURN QHeaderViewFromPointer( Qt_QTreeView_header( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:indentation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_indentation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:indexAbove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QTreeView_indexAbove( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:indexBelow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QTreeView_indexBelow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isAnimated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_isAnimated( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isColumnHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_isColumnHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isExpanded( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_isExpanded( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_isFirstColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isHeaderHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_isHeaderHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isRowHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_isRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:isSortingEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_isSortingEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:itemsExpandable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_itemsExpandable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:rootIsDecorated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_rootIsDecorated( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:scrollTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_scrollTo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_scrollTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setAllColumnsShowFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setAllColumnsShowFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setAnimated( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setAnimated( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setAutoExpandDelay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setAutoExpandDelay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setColumnHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_setColumnHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setColumnWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_setColumnWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setExpanded( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_setExpanded( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setExpandsOnDoubleClick( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setExpandsOnDoubleClick( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setFirstColumnSpanned( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) )
         RETURN Qt_QTreeView_setFirstColumnSpanned( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setHeader( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setHeaderHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setHeaderHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setIndentation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setIndentation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setItemsExpandable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setItemsExpandable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setRootIsDecorated( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setRootIsDecorated( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setRowHidden( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) )
         RETURN Qt_QTreeView_setRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setSortingEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setSortingEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setUniformRowHeights( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setUniformRowHeights( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:sortByColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTreeView_sortByColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:uniformRowHeights( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_uniformRowHeights( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:visualRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTreeView_visualRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:collapse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_collapse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:collapseAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_collapseAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:expand( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_expand( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:expandAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTreeView_expandAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:expandToDepth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_expandToDepth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:hideColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_hideColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:resizeColumnToContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_resizeColumnToContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTreeView:showColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTreeView_showColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

