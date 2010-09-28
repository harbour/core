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


FUNCTION QFtp( ... )
   RETURN HB_QFtp():new( ... )


CREATE CLASS QFtp INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QFtp

   METHOD  new( ... )

   METHOD  bytesAvailable()
   METHOD  cd( cDir )
   METHOD  clearPendingCommands()
   METHOD  close()
   METHOD  connectToHost( cHost, nPort )
   METHOD  currentCommand()
   METHOD  currentDevice()
   METHOD  currentId()
   METHOD  error()
   METHOD  errorString()
   METHOD  get( cFile, pDev, nType )
   METHOD  hasPendingCommands()
   METHOD  list( cDir )
   METHOD  login( cUser, cPassword )
   METHOD  mkdir( cDir )
   METHOD  put( ... )
   METHOD  rawCommand( cCommand )
   METHOD  readAll()
   METHOD  remove( cFile )
   METHOD  rename( cOldname, cNewname )
   METHOD  rmdir( cDir )
   METHOD  setProxy( cHost, nPort )
   METHOD  setTransferMode( nMode )
   METHOD  state()
   METHOD  abort()

   ENDCLASS


METHOD QFtp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFtp( ... )
   RETURN Self


METHOD QFtp:bytesAvailable()
   RETURN Qt_QFtp_bytesAvailable( ::pPtr )


METHOD QFtp:cd( cDir )
   RETURN Qt_QFtp_cd( ::pPtr, cDir )


METHOD QFtp:clearPendingCommands()
   RETURN Qt_QFtp_clearPendingCommands( ::pPtr )


METHOD QFtp:close()
   RETURN Qt_QFtp_close( ::pPtr )


METHOD QFtp:connectToHost( cHost, nPort )
   RETURN Qt_QFtp_connectToHost( ::pPtr, cHost, nPort )


METHOD QFtp:currentCommand()
   RETURN Qt_QFtp_currentCommand( ::pPtr )


METHOD QFtp:currentDevice()
   RETURN HB_QIODevice():from( Qt_QFtp_currentDevice( ::pPtr ) )


METHOD QFtp:currentId()
   RETURN Qt_QFtp_currentId( ::pPtr )


METHOD QFtp:error()
   RETURN Qt_QFtp_error( ::pPtr )


METHOD QFtp:errorString()
   RETURN Qt_QFtp_errorString( ::pPtr )


METHOD QFtp:get( cFile, pDev, nType )
   RETURN Qt_QFtp_get( ::pPtr, cFile, hbqt_ptr( pDev ), nType )


METHOD QFtp:hasPendingCommands()
   RETURN Qt_QFtp_hasPendingCommands( ::pPtr )


METHOD QFtp:list( cDir )
   RETURN Qt_QFtp_list( ::pPtr, cDir )


METHOD QFtp:login( cUser, cPassword )
   RETURN Qt_QFtp_login( ::pPtr, cUser, cPassword )


METHOD QFtp:mkdir( cDir )
   RETURN Qt_QFtp_mkdir( ::pPtr, cDir )


METHOD QFtp:put( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QIODEVICE"
            RETURN Qt_QFtp_put( ::pPtr, ... )
         CASE "QBYTEARRAY"
            RETURN Qt_QFtp_put_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QIODEVICE"
            RETURN Qt_QFtp_put( ::pPtr, ... )
         CASE "QBYTEARRAY"
            RETURN Qt_QFtp_put_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFtp:rawCommand( cCommand )
   RETURN Qt_QFtp_rawCommand( ::pPtr, cCommand )


METHOD QFtp:readAll()
   RETURN HB_QByteArray():from( Qt_QFtp_readAll( ::pPtr ) )


METHOD QFtp:remove( cFile )
   RETURN Qt_QFtp_remove( ::pPtr, cFile )


METHOD QFtp:rename( cOldname, cNewname )
   RETURN Qt_QFtp_rename( ::pPtr, cOldname, cNewname )


METHOD QFtp:rmdir( cDir )
   RETURN Qt_QFtp_rmdir( ::pPtr, cDir )


METHOD QFtp:setProxy( cHost, nPort )
   RETURN Qt_QFtp_setProxy( ::pPtr, cHost, nPort )


METHOD QFtp:setTransferMode( nMode )
   RETURN Qt_QFtp_setTransferMode( ::pPtr, nMode )


METHOD QFtp:state()
   RETURN Qt_QFtp_state( ::pPtr )


METHOD QFtp:abort()
   RETURN Qt_QFtp_abort( ::pPtr )

