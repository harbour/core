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


FUNCTION QSessionManager( ... )
   RETURN HB_QSessionManager():new( ... )


CREATE CLASS QSessionManager INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSessionManager

   METHOD  new( ... )

   METHOD  allowsErrorInteraction()
   METHOD  allowsInteraction()
   METHOD  cancel()
   METHOD  discardCommand()
   METHOD  isPhase2()
   METHOD  release()
   METHOD  requestPhase2()
   METHOD  restartCommand()
   METHOD  restartHint()
   METHOD  sessionId()
   METHOD  sessionKey()
   METHOD  setDiscardCommand( pList )
   METHOD  setManagerProperty( ... )
   METHOD  setRestartCommand( pCommand )
   METHOD  setRestartHint( nHint )

   ENDCLASS


METHOD QSessionManager:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSessionManager( ... )
   RETURN Self


METHOD QSessionManager:allowsErrorInteraction()
   RETURN Qt_QSessionManager_allowsErrorInteraction( ::pPtr )


METHOD QSessionManager:allowsInteraction()
   RETURN Qt_QSessionManager_allowsInteraction( ::pPtr )


METHOD QSessionManager:cancel()
   RETURN Qt_QSessionManager_cancel( ::pPtr )


METHOD QSessionManager:discardCommand()
   RETURN HB_QStringList():from( Qt_QSessionManager_discardCommand( ::pPtr ) )


METHOD QSessionManager:isPhase2()
   RETURN Qt_QSessionManager_isPhase2( ::pPtr )


METHOD QSessionManager:release()
   RETURN Qt_QSessionManager_release( ::pPtr )


METHOD QSessionManager:requestPhase2()
   RETURN Qt_QSessionManager_requestPhase2( ::pPtr )


METHOD QSessionManager:restartCommand()
   RETURN HB_QStringList():from( Qt_QSessionManager_restartCommand( ::pPtr ) )


METHOD QSessionManager:restartHint()
   RETURN Qt_QSessionManager_restartHint( ::pPtr )


METHOD QSessionManager:sessionId()
   RETURN Qt_QSessionManager_sessionId( ::pPtr )


METHOD QSessionManager:sessionKey()
   RETURN Qt_QSessionManager_sessionKey( ::pPtr )


METHOD QSessionManager:setDiscardCommand( pList )
   RETURN Qt_QSessionManager_setDiscardCommand( ::pPtr, hbqt_ptr( pList ) )


METHOD QSessionManager:setManagerProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QSessionManager_setManagerProperty_1( ::pPtr, ... )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QSessionManager_setManagerProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QSessionManager:setRestartCommand( pCommand )
   RETURN Qt_QSessionManager_setRestartCommand( ::pPtr, hbqt_ptr( pCommand ) )


METHOD QSessionManager:setRestartHint( nHint )
   RETURN Qt_QSessionManager_setRestartHint( ::pPtr, nHint )

