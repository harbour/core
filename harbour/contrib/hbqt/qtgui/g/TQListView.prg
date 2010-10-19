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


FUNCTION QListView( ... )
   RETURN HB_QListView():new( ... )

FUNCTION QListViewFromPointer( ... )
   RETURN HB_QListView():fromPointer( ... )


CREATE CLASS QListView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QListView

   METHOD  new( ... )

   METHOD  batchSize                     // (  )                                               -> nInt
   METHOD  clearPropertyFlags            // (  )                                               -> NIL
   METHOD  flow                          // (  )                                               -> nFlow
   METHOD  gridSize                      // (  )                                               -> oQSize
   METHOD  isRowHidden                   // ( nRow )                                           -> lBool
   METHOD  isSelectionRectVisible        // (  )                                               -> lBool
   METHOD  isWrapping                    // (  )                                               -> lBool
   METHOD  layoutMode                    // (  )                                               -> nLayoutMode
   METHOD  modelColumn                   // (  )                                               -> nInt
   METHOD  movement                      // (  )                                               -> nMovement
   METHOD  resizeMode                    // (  )                                               -> nResizeMode
   METHOD  setBatchSize                  // ( nBatchSize )                                     -> NIL
   METHOD  setFlow                       // ( nFlow )                                          -> NIL
   METHOD  setGridSize                   // ( oQSize )                                         -> NIL
   METHOD  setLayoutMode                 // ( nMode )                                          -> NIL
   METHOD  setModelColumn                // ( nColumn )                                        -> NIL
   METHOD  setMovement                   // ( nMovement )                                      -> NIL
   METHOD  setResizeMode                 // ( nMode )                                          -> NIL
   METHOD  setRowHidden                  // ( nRow, lHide )                                    -> NIL
   METHOD  setSelectionRectVisible       // ( lShow )                                          -> NIL
   METHOD  setSpacing                    // ( nSpace )                                         -> NIL
   METHOD  setUniformItemSizes           // ( lEnable )                                        -> NIL
   METHOD  setViewMode                   // ( nMode )                                          -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  setWrapping                   // ( lEnable )                                        -> NIL
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  uniformItemSizes              // (  )                                               -> lBool
   METHOD  viewMode                      // (  )                                               -> nViewMode
   METHOD  wordWrap                      // (  )                                               -> lBool

   ENDCLASS


METHOD QListView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListView( ... )
   RETURN Self


METHOD QListView:batchSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_batchSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:clearPropertyFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_clearPropertyFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:flow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_flow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:gridSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QListView_gridSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isRowHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_isRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isSelectionRectVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_isSelectionRectVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isWrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_isWrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:layoutMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_layoutMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:modelColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_modelColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:movement( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_movement( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:resizeMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_resizeMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setBatchSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setBatchSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setFlow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setFlow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setGridSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setGridSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setLayoutMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setLayoutMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setModelColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setModelColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setMovement( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setMovement( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setResizeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setResizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setRowHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QListView_setRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setSelectionRectVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setSelectionRectVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setUniformItemSizes( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setUniformItemSizes( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setViewMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setViewMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setWrapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setWrapping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:uniformItemSizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_uniformItemSizes( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:viewMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_viewMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

