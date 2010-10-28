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


REQUEST __HBQTNETWORK


FUNCTION QFtp( ... )
   RETURN HB_QFtp():new( ... )

FUNCTION QFtpFromPointer( ... )
   RETURN HB_QFtp():fromPointer( ... )


CREATE CLASS QFtp INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QFtp

   METHOD  new( ... )

   METHOD  bytesAvailable                // (  )                                               -> nQint64
   METHOD  cd                            // ( cDir )                                           -> nInt
   METHOD  clearPendingCommands          // (  )                                               -> NIL
   METHOD  close                         // (  )                                               -> nInt
   METHOD  connectToHost                 // ( cHost, nPort )                                   -> nInt
   METHOD  currentCommand                // (  )                                               -> nCommand
   METHOD  currentDevice                 // (  )                                               -> oQIODevice
   METHOD  currentId                     // (  )                                               -> nInt
   METHOD  error                         // (  )                                               -> nError
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  get                           // ( cFile, oQIODevice, nType )                       -> nInt
   METHOD  hasPendingCommands            // (  )                                               -> lBool
   METHOD  list                          // ( cDir )                                           -> nInt
   METHOD  login                         // ( cUser, cPassword )                               -> nInt
   METHOD  mkdir                         // ( cDir )                                           -> nInt
   METHOD  put                           // ( oQIODevice, cFile, nType )                       -> nInt
                                         // ( oQByteArray, cFile, nType )                      -> nInt
   METHOD  rawCommand                    // ( cCommand )                                       -> nInt
   METHOD  readAll                       // (  )                                               -> oQByteArray
   METHOD  remove                        // ( cFile )                                          -> nInt
   METHOD  rename                        // ( cOldname, cNewname )                             -> nInt
   METHOD  rmdir                         // ( cDir )                                           -> nInt
   METHOD  setProxy                      // ( cHost, nPort )                                   -> nInt
   METHOD  setTransferMode               // ( nMode )                                          -> nInt
   METHOD  state                         // (  )                                               -> nState
   METHOD  abort                         // (  )                                               -> NIL

   ENDCLASS


METHOD QFtp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFtp( ... )
   RETURN Self


METHOD QFtp:bytesAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_bytesAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:cd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_cd( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:clearPendingCommands( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_clearPendingCommands( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:connectToHost( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_connectToHost( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_connectToHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentCommand( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_currentCommand( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QFtp_currentDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_currentId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:get( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:hasPendingCommands( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_hasPendingCommands( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:list( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_list( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFtp_list( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:login( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_login( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_login( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFtp_login( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:mkdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_mkdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QFtp:rawCommand( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_rawCommand( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QFtp_readAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:rename( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_rename( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:rmdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_rmdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:setProxy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_setProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:setTransferMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_setTransferMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:abort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_abort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

