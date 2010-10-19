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


FUNCTION QHeaderView( ... )
   RETURN HB_QHeaderView():new( ... )

FUNCTION QHeaderViewFromPointer( ... )
   RETURN HB_QHeaderView():fromPointer( ... )


CREATE CLASS QHeaderView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QHeaderView

   METHOD  new( ... )

   METHOD  cascadingSectionResizes       // (  )                                               -> lBool
   METHOD  count                         // (  )                                               -> nInt
   METHOD  defaultAlignment              // (  )                                               -> nQt_Alignment
   METHOD  defaultSectionSize            // (  )                                               -> nInt
   METHOD  hiddenSectionCount            // (  )                                               -> nInt
   METHOD  hideSection                   // ( nLogicalIndex )                                  -> NIL
   METHOD  highlightSections             // (  )                                               -> lBool
   METHOD  isClickable                   // (  )                                               -> lBool
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isSectionHidden               // ( nLogicalIndex )                                  -> lBool
   METHOD  isSortIndicatorShown          // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nInt
   METHOD  logicalIndex                  // ( nVisualIndex )                                   -> nInt
   METHOD  logicalIndexAt                // ( nPosition )                                      -> nInt
                                         // ( nX, nY )                                         -> nInt
                                         // ( oQPoint )                                        -> nInt
   METHOD  minimumSectionSize            // (  )                                               -> nInt
   METHOD  moveSection                   // ( nFrom, nTo )                                     -> NIL
   METHOD  offset                        // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  resizeMode                    // ( nLogicalIndex )                                  -> nResizeMode
   METHOD  resizeSection                 // ( nLogicalIndex, nSize )                           -> NIL
   METHOD  resizeSections                // ( nMode )                                          -> NIL
   METHOD  restoreState                  // ( oQByteArray )                                    -> lBool
   METHOD  saveState                     // (  )                                               -> oQByteArray
   METHOD  sectionPosition               // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionSize                   // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionSizeHint               // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionViewportPosition       // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionsHidden                // (  )                                               -> lBool
   METHOD  sectionsMoved                 // (  )                                               -> lBool
   METHOD  setCascadingSectionResizes    // ( lEnable )                                        -> NIL
   METHOD  setClickable                  // ( lClickable )                                     -> NIL
   METHOD  setDefaultAlignment           // ( nAlignment )                                     -> NIL
   METHOD  setDefaultSectionSize         // ( nSize )                                          -> NIL
   METHOD  setHighlightSections          // ( lHighlight )                                     -> NIL
   METHOD  setMinimumSectionSize         // ( nSize )                                          -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setResizeMode                 // ( nMode )                                          -> NIL
                                         // ( nLogicalIndex, nMode )                           -> NIL
   METHOD  setSectionHidden              // ( nLogicalIndex, lHide )                           -> NIL
   METHOD  setSortIndicator              // ( nLogicalIndex, nOrder )                          -> NIL
   METHOD  setSortIndicatorShown         // ( lShow )                                          -> NIL
   METHOD  setStretchLastSection         // ( lStretch )                                       -> NIL
   METHOD  showSection                   // ( nLogicalIndex )                                  -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  sortIndicatorOrder            // (  )                                               -> nQt_SortOrder
   METHOD  sortIndicatorSection          // (  )                                               -> nInt
   METHOD  stretchLastSection            // (  )                                               -> lBool
   METHOD  stretchSectionCount           // (  )                                               -> nInt
   METHOD  swapSections                  // ( nFirst, nSecond )                                -> NIL
   METHOD  visualIndex                   // ( nLogicalIndex )                                  -> nInt
   METHOD  visualIndexAt                 // ( nPosition )                                      -> nInt
   METHOD  headerDataChanged             // ( nOrientation, nLogicalFirst, nLogicalLast )      -> NIL
   METHOD  setOffset                     // ( nOffset )                                        -> NIL
   METHOD  setOffsetToLastSection        // (  )                                               -> NIL
   METHOD  setOffsetToSectionPosition    // ( nVisualIndex )                                   -> NIL

   ENDCLASS


METHOD QHeaderView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHeaderView( ... )
   RETURN Self


METHOD QHeaderView:cascadingSectionResizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_cascadingSectionResizes( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:defaultAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_defaultAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:defaultSectionSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_defaultSectionSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:hiddenSectionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_hiddenSectionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:hideSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_hideSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:highlightSections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_highlightSections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isClickable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isClickable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isSectionHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_isSectionHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isSortIndicatorShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isSortIndicatorShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:logicalIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:logicalIndexAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_logicalIndexAt_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndexAt( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndexAt_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:minimumSectionSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_minimumSectionSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:moveSection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_moveSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:offset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_offset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_resizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeSection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_resizeSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeSections( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_resizeSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:restoreState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:saveState( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QHeaderView_saveState( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionViewportPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionViewportPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionsHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sectionsHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionsMoved( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sectionsMoved( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setCascadingSectionResizes( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setCascadingSectionResizes( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setClickable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setClickable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setDefaultAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setDefaultAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setDefaultSectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setDefaultSectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setHighlightSections( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setHighlightSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setMinimumSectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setMinimumSectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setResizeMode( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setResizeMode_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setResizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSectionHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setSectionHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSortIndicator( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setSortIndicator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSortIndicatorShown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setSortIndicatorShown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setStretchLastSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setStretchLastSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:showSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_showSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QHeaderView_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sortIndicatorOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sortIndicatorOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sortIndicatorSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sortIndicatorSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:stretchLastSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_stretchLastSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:stretchSectionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_stretchSectionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:swapSections( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_swapSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:visualIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_visualIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:visualIndexAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_visualIndexAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:headerDataChanged( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QHeaderView_headerDataChanged( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setOffset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffsetToLastSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_setOffsetToLastSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffsetToSectionPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setOffsetToSectionPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

