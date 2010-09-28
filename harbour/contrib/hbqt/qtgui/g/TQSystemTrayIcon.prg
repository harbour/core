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


FUNCTION QSystemTrayIcon( ... )
   RETURN HB_QSystemTrayIcon():new( ... )


CREATE CLASS QSystemTrayIcon INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSystemTrayIcon

   METHOD  new( ... )

   METHOD  contextMenu()
   METHOD  geometry()
   METHOD  icon()
   METHOD  isVisible()
   METHOD  setContextMenu( pMenu )
   METHOD  setIcon( pIcon )
   METHOD  setToolTip( cTip )
   METHOD  showMessage( cTitle, cMessage, nIcon, nMillisecondsTimeoutHint )
   METHOD  toolTip()
   METHOD  isSystemTrayAvailable()
   METHOD  supportsMessages()
   METHOD  hide()
   METHOD  setVisible( lVisible )
   METHOD  show()

   ENDCLASS


METHOD QSystemTrayIcon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSystemTrayIcon( ... )
   RETURN Self


METHOD QSystemTrayIcon:contextMenu()
   RETURN HB_QMenu():from( Qt_QSystemTrayIcon_contextMenu( ::pPtr ) )


METHOD QSystemTrayIcon:geometry()
   RETURN HB_QRect():from( Qt_QSystemTrayIcon_geometry( ::pPtr ) )


METHOD QSystemTrayIcon:icon()
   RETURN HB_QIcon():from( Qt_QSystemTrayIcon_icon( ::pPtr ) )


METHOD QSystemTrayIcon:isVisible()
   RETURN Qt_QSystemTrayIcon_isVisible( ::pPtr )


METHOD QSystemTrayIcon:setContextMenu( pMenu )
   RETURN Qt_QSystemTrayIcon_setContextMenu( ::pPtr, hbqt_ptr( pMenu ) )


METHOD QSystemTrayIcon:setIcon( pIcon )
   RETURN Qt_QSystemTrayIcon_setIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QSystemTrayIcon:setToolTip( cTip )
   RETURN Qt_QSystemTrayIcon_setToolTip( ::pPtr, cTip )


METHOD QSystemTrayIcon:showMessage( cTitle, cMessage, nIcon, nMillisecondsTimeoutHint )
   RETURN Qt_QSystemTrayIcon_showMessage( ::pPtr, cTitle, cMessage, nIcon, nMillisecondsTimeoutHint )


METHOD QSystemTrayIcon:toolTip()
   RETURN Qt_QSystemTrayIcon_toolTip( ::pPtr )


METHOD QSystemTrayIcon:isSystemTrayAvailable()
   RETURN Qt_QSystemTrayIcon_isSystemTrayAvailable( ::pPtr )


METHOD QSystemTrayIcon:supportsMessages()
   RETURN Qt_QSystemTrayIcon_supportsMessages( ::pPtr )


METHOD QSystemTrayIcon:hide()
   RETURN Qt_QSystemTrayIcon_hide( ::pPtr )


METHOD QSystemTrayIcon:setVisible( lVisible )
   RETURN Qt_QSystemTrayIcon_setVisible( ::pPtr, lVisible )


METHOD QSystemTrayIcon:show()
   RETURN Qt_QSystemTrayIcon_show( ::pPtr )

