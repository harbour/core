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


FUNCTION QMdiArea( ... )
   RETURN HB_QMdiArea():new( ... )

FUNCTION QMdiAreaFrom( ... )
   RETURN HB_QMdiArea():from( ... )

FUNCTION QMdiAreaFromPointer( ... )
   RETURN HB_QMdiArea():fromPointer( ... )


CREATE CLASS QMdiArea INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QMdiArea

   METHOD  new( ... )

   METHOD  activationOrder               // (  )                                               -> nWindowOrder
   METHOD  activeSubWindow               // (  )                                               -> oQMdiSubWindow
   METHOD  addSubWindow                  // ( oQWidget, nWindowFlags )                         -> oQMdiSubWindow
   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  currentSubWindow              // (  )                                               -> oQMdiSubWindow
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  removeSubWindow               // ( oQWidget )                                       -> NIL
   METHOD  setActivationOrder            // ( nOrder )                                         -> NIL
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setDocumentMode               // ( lEnabled )                                       -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setTabPosition                // ( nPosition )                                      -> NIL
   METHOD  setTabShape                   // ( nShape )                                         -> NIL
   METHOD  setViewMode                   // ( nMode )                                          -> NIL
   METHOD  subWindowList                 // ( nOrder )                                         -> oQList_QMdiSubWindow
   METHOD  tabPosition                   // (  )                                               -> nQTabWidget_TabPosition
   METHOD  tabShape                      // (  )                                               -> nQTabWidget_TabShape
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  viewMode                      // (  )                                               -> nViewMode
   METHOD  activateNextSubWindow         // (  )                                               -> NIL
   METHOD  activatePreviousSubWindow     // (  )                                               -> NIL
   METHOD  cascadeSubWindows             // (  )                                               -> NIL
   METHOD  closeActiveSubWindow          // (  )                                               -> NIL
   METHOD  closeAllSubWindows            // (  )                                               -> NIL
   METHOD  setActiveSubWindow            // ( oQMdiSubWindow )                                 -> NIL
   METHOD  tileSubWindows                // (  )                                               -> NIL

   ENDCLASS


METHOD QMdiArea:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMdiArea( ... )
   RETURN Self


METHOD QMdiArea:activationOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_activationOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:activeSubWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMdiSubWindowFromPointer( Qt_QMdiArea_activeSubWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:addSubWindow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMdiSubWindowFromPointer( Qt_QMdiArea_addSubWindow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QMdiSubWindowFromPointer( Qt_QMdiArea_addSubWindow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QMdiArea_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:currentSubWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMdiSubWindowFromPointer( Qt_QMdiArea_currentSubWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:removeSubWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_removeSubWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setActivationOrder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setActivationOrder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QMdiArea_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setTabPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setTabPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setTabShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setTabShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setViewMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setViewMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:subWindowList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QMdiArea_subWindowList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QListFromPointer( Qt_QMdiArea_subWindowList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:tabPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_tabPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:tabShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_tabShape( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:viewMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_viewMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:activateNextSubWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_activateNextSubWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:activatePreviousSubWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_activatePreviousSubWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:cascadeSubWindows( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_cascadeSubWindows( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:closeActiveSubWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_closeActiveSubWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:closeAllSubWindows( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_closeAllSubWindows( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:setActiveSubWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMdiArea_setActiveSubWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMdiArea:tileSubWindows( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiArea_tileSubWindows( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

