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


FUNCTION QSplitter( ... )
   RETURN HB_QSplitter():new( ... )


CREATE CLASS QSplitter INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QSplitter

   METHOD  new( ... )

   METHOD  addWidget( pWidget )
   METHOD  childrenCollapsible()
   METHOD  count()
   METHOD  getRange( nIndex, nMin, nMax )
   METHOD  handleWidth()
   METHOD  indexOf( pWidget )
   METHOD  insertWidget( nIndex, pWidget )
   METHOD  isCollapsible( nIndex )
   METHOD  opaqueResize()
   METHOD  orientation()
   METHOD  refresh()
   METHOD  restoreState( pState )
   METHOD  saveState()
   METHOD  setChildrenCollapsible( lBool )
   METHOD  setCollapsible( nIndex, lCollapse )
   METHOD  setHandleWidth( nInt )
   METHOD  setOpaqueResize( lOpaque )
   METHOD  setOrientation( nQt_Orientation )
   METHOD  setStretchFactor( nIndex, nStretch )
   METHOD  sizes()
   METHOD  widget( nIndex )

   ENDCLASS


METHOD QSplitter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSplitter( ... )
   RETURN Self


METHOD QSplitter:addWidget( pWidget )
   RETURN Qt_QSplitter_addWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QSplitter:childrenCollapsible()
   RETURN Qt_QSplitter_childrenCollapsible( ::pPtr )


METHOD QSplitter:count()
   RETURN Qt_QSplitter_count( ::pPtr )


METHOD QSplitter:getRange( nIndex, nMin, nMax )
   RETURN Qt_QSplitter_getRange( ::pPtr, nIndex, nMin, nMax )


METHOD QSplitter:handleWidth()
   RETURN Qt_QSplitter_handleWidth( ::pPtr )


METHOD QSplitter:indexOf( pWidget )
   RETURN Qt_QSplitter_indexOf( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QSplitter:insertWidget( nIndex, pWidget )
   RETURN Qt_QSplitter_insertWidget( ::pPtr, nIndex, hbqt_ptr( pWidget ) )


METHOD QSplitter:isCollapsible( nIndex )
   RETURN Qt_QSplitter_isCollapsible( ::pPtr, nIndex )


METHOD QSplitter:opaqueResize()
   RETURN Qt_QSplitter_opaqueResize( ::pPtr )


METHOD QSplitter:orientation()
   RETURN Qt_QSplitter_orientation( ::pPtr )


METHOD QSplitter:refresh()
   RETURN Qt_QSplitter_refresh( ::pPtr )


METHOD QSplitter:restoreState( pState )
   RETURN Qt_QSplitter_restoreState( ::pPtr, hbqt_ptr( pState ) )


METHOD QSplitter:saveState()
   RETURN HB_QByteArray():from( Qt_QSplitter_saveState( ::pPtr ) )


METHOD QSplitter:setChildrenCollapsible( lBool )
   RETURN Qt_QSplitter_setChildrenCollapsible( ::pPtr, lBool )


METHOD QSplitter:setCollapsible( nIndex, lCollapse )
   RETURN Qt_QSplitter_setCollapsible( ::pPtr, nIndex, lCollapse )


METHOD QSplitter:setHandleWidth( nInt )
   RETURN Qt_QSplitter_setHandleWidth( ::pPtr, nInt )


METHOD QSplitter:setOpaqueResize( lOpaque )
   RETURN Qt_QSplitter_setOpaqueResize( ::pPtr, lOpaque )


METHOD QSplitter:setOrientation( nQt_Orientation )
   RETURN Qt_QSplitter_setOrientation( ::pPtr, nQt_Orientation )


METHOD QSplitter:setStretchFactor( nIndex, nStretch )
   RETURN Qt_QSplitter_setStretchFactor( ::pPtr, nIndex, nStretch )


METHOD QSplitter:sizes()
   RETURN HB_QList():from( Qt_QSplitter_sizes( ::pPtr ) )


METHOD QSplitter:widget( nIndex )
   RETURN HB_QWidget():from( Qt_QSplitter_widget( ::pPtr, nIndex ) )

