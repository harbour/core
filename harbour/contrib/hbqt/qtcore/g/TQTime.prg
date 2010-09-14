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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QTime( ... )
   RETURN HB_QTime():new( ... )


CREATE CLASS QTime INHERIT HbQtObjectHandler FUNCTION HB_QTime

   METHOD  new( ... )

   METHOD  addMSecs( nMs )
   METHOD  addSecs( nS )
   METHOD  elapsed()
   METHOD  hour()
   METHOD  isNull()
   METHOD  isValid()
   METHOD  minute()
   METHOD  msec()
   METHOD  msecsTo( pT )
   METHOD  restart()
   METHOD  second()
   METHOD  secsTo( pT )
   METHOD  setHMS( nH, nM, nS, nMs )
   METHOD  start()
   METHOD  toString( cFormat )
   METHOD  toString_1( nFormat )
   METHOD  currentTime()
   METHOD  fromString( cString, nFormat )
   METHOD  fromString_1( cString, cFormat )
   METHOD  isValid_1( nH, nM, nS, nMs )

   ENDCLASS


METHOD QTime:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTime( ... )
   RETURN Self


METHOD QTime:addMSecs( nMs )
   RETURN Qt_QTime_addMSecs( ::pPtr, nMs )


METHOD QTime:addSecs( nS )
   RETURN Qt_QTime_addSecs( ::pPtr, nS )


METHOD QTime:elapsed()
   RETURN Qt_QTime_elapsed( ::pPtr )


METHOD QTime:hour()
   RETURN Qt_QTime_hour( ::pPtr )


METHOD QTime:isNull()
   RETURN Qt_QTime_isNull( ::pPtr )


METHOD QTime:isValid()
   RETURN Qt_QTime_isValid( ::pPtr )


METHOD QTime:minute()
   RETURN Qt_QTime_minute( ::pPtr )


METHOD QTime:msec()
   RETURN Qt_QTime_msec( ::pPtr )


METHOD QTime:msecsTo( pT )
   RETURN Qt_QTime_msecsTo( ::pPtr, hbqt_ptr( pT ) )


METHOD QTime:restart()
   RETURN Qt_QTime_restart( ::pPtr )


METHOD QTime:second()
   RETURN Qt_QTime_second( ::pPtr )


METHOD QTime:secsTo( pT )
   RETURN Qt_QTime_secsTo( ::pPtr, hbqt_ptr( pT ) )


METHOD QTime:setHMS( nH, nM, nS, nMs )
   RETURN Qt_QTime_setHMS( ::pPtr, nH, nM, nS, nMs )


METHOD QTime:start()
   RETURN Qt_QTime_start( ::pPtr )


METHOD QTime:toString( cFormat )
   RETURN Qt_QTime_toString( ::pPtr, cFormat )


METHOD QTime:toString_1( nFormat )
   RETURN Qt_QTime_toString_1( ::pPtr, nFormat )


METHOD QTime:currentTime()
   RETURN Qt_QTime_currentTime( ::pPtr )


METHOD QTime:fromString( cString, nFormat )
   RETURN Qt_QTime_fromString( ::pPtr, cString, nFormat )


METHOD QTime:fromString_1( cString, cFormat )
   RETURN Qt_QTime_fromString_1( ::pPtr, cString, cFormat )


METHOD QTime:isValid_1( nH, nM, nS, nMs )
   RETURN Qt_QTime_isValid_1( ::pPtr, nH, nM, nS, nMs )

