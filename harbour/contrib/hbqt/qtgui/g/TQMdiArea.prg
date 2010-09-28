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


CREATE CLASS QMdiArea INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QMdiArea

   METHOD  new( ... )

   METHOD  activationOrder()
   METHOD  activeSubWindow()
   METHOD  addSubWindow( pWidget, nWindowFlags )
   METHOD  background()
   METHOD  currentSubWindow()
   METHOD  documentMode()
   METHOD  removeSubWindow( pWidget )
   METHOD  setActivationOrder( nOrder )
   METHOD  setBackground( pBackground )
   METHOD  setDocumentMode( lEnabled )
   METHOD  setOption( nOption, lOn )
   METHOD  setTabPosition( nPosition )
   METHOD  setTabShape( nShape )
   METHOD  setViewMode( nMode )
   METHOD  subWindowList( nOrder )
   METHOD  tabPosition()
   METHOD  tabShape()
   METHOD  testOption( nOption )
   METHOD  viewMode()
   METHOD  activateNextSubWindow()
   METHOD  activatePreviousSubWindow()
   METHOD  cascadeSubWindows()
   METHOD  closeActiveSubWindow()
   METHOD  closeAllSubWindows()
   METHOD  setActiveSubWindow( pWindow )
   METHOD  tileSubWindows()

   ENDCLASS


METHOD QMdiArea:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMdiArea( ... )
   RETURN Self


METHOD QMdiArea:activationOrder()
   RETURN Qt_QMdiArea_activationOrder( ::pPtr )


METHOD QMdiArea:activeSubWindow()
   RETURN HB_QMdiSubWindow():from( Qt_QMdiArea_activeSubWindow( ::pPtr ) )


METHOD QMdiArea:addSubWindow( pWidget, nWindowFlags )
   RETURN HB_QMdiSubWindow():from( Qt_QMdiArea_addSubWindow( ::pPtr, hbqt_ptr( pWidget ), nWindowFlags ) )


METHOD QMdiArea:background()
   RETURN HB_QBrush():from( Qt_QMdiArea_background( ::pPtr ) )


METHOD QMdiArea:currentSubWindow()
   RETURN HB_QMdiSubWindow():from( Qt_QMdiArea_currentSubWindow( ::pPtr ) )


METHOD QMdiArea:documentMode()
   RETURN Qt_QMdiArea_documentMode( ::pPtr )


METHOD QMdiArea:removeSubWindow( pWidget )
   RETURN Qt_QMdiArea_removeSubWindow( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QMdiArea:setActivationOrder( nOrder )
   RETURN Qt_QMdiArea_setActivationOrder( ::pPtr, nOrder )


METHOD QMdiArea:setBackground( pBackground )
   RETURN Qt_QMdiArea_setBackground( ::pPtr, hbqt_ptr( pBackground ) )


METHOD QMdiArea:setDocumentMode( lEnabled )
   RETURN Qt_QMdiArea_setDocumentMode( ::pPtr, lEnabled )


METHOD QMdiArea:setOption( nOption, lOn )
   RETURN Qt_QMdiArea_setOption( ::pPtr, nOption, lOn )


METHOD QMdiArea:setTabPosition( nPosition )
   RETURN Qt_QMdiArea_setTabPosition( ::pPtr, nPosition )


METHOD QMdiArea:setTabShape( nShape )
   RETURN Qt_QMdiArea_setTabShape( ::pPtr, nShape )


METHOD QMdiArea:setViewMode( nMode )
   RETURN Qt_QMdiArea_setViewMode( ::pPtr, nMode )


METHOD QMdiArea:subWindowList( nOrder )
   RETURN HB_QList():from( Qt_QMdiArea_subWindowList( ::pPtr, nOrder ) )


METHOD QMdiArea:tabPosition()
   RETURN Qt_QMdiArea_tabPosition( ::pPtr )


METHOD QMdiArea:tabShape()
   RETURN Qt_QMdiArea_tabShape( ::pPtr )


METHOD QMdiArea:testOption( nOption )
   RETURN Qt_QMdiArea_testOption( ::pPtr, nOption )


METHOD QMdiArea:viewMode()
   RETURN Qt_QMdiArea_viewMode( ::pPtr )


METHOD QMdiArea:activateNextSubWindow()
   RETURN Qt_QMdiArea_activateNextSubWindow( ::pPtr )


METHOD QMdiArea:activatePreviousSubWindow()
   RETURN Qt_QMdiArea_activatePreviousSubWindow( ::pPtr )


METHOD QMdiArea:cascadeSubWindows()
   RETURN Qt_QMdiArea_cascadeSubWindows( ::pPtr )


METHOD QMdiArea:closeActiveSubWindow()
   RETURN Qt_QMdiArea_closeActiveSubWindow( ::pPtr )


METHOD QMdiArea:closeAllSubWindows()
   RETURN Qt_QMdiArea_closeAllSubWindows( ::pPtr )


METHOD QMdiArea:setActiveSubWindow( pWindow )
   RETURN Qt_QMdiArea_setActiveSubWindow( ::pPtr, hbqt_ptr( pWindow ) )


METHOD QMdiArea:tileSubWindows()
   RETURN Qt_QMdiArea_tileSubWindows( ::pPtr )

