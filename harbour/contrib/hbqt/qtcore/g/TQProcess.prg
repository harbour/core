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


FUNCTION QProcess( ... )
   RETURN HB_QProcess():new( ... )


CREATE CLASS QProcess INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QProcess

   METHOD  new( ... )

   METHOD  close()
   METHOD  closeReadChannel( nChannel )
   METHOD  closeWriteChannel()
   METHOD  environment()
   METHOD  error()
   METHOD  exitCode()
   METHOD  exitStatus()
   METHOD  processChannelMode()
   METHOD  readAllStandardError()
   METHOD  readAllStandardOutput()
   METHOD  readChannel()
   METHOD  setEnvironment( pEnvironment )
   METHOD  setProcessChannelMode( nMode )
   METHOD  setReadChannel( nChannel )
   METHOD  setStandardErrorFile( cFileName, nMode )
   METHOD  setStandardInputFile( cFileName )
   METHOD  setStandardOutputFile( cFileName, nMode )
   METHOD  setStandardOutputProcess( pDestination )
   METHOD  setWorkingDirectory( cDir )
   METHOD  start( ... )
   METHOD  state()
   METHOD  waitForFinished( nMsecs )
   METHOD  waitForStarted( nMsecs )
   METHOD  workingDirectory()
   METHOD  execute( ... )
   METHOD  startDetached( ... )
   METHOD  systemEnvironment()
   METHOD  kill()
   METHOD  terminate()

   ENDCLASS


METHOD QProcess:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProcess( ... )
   RETURN Self


METHOD QProcess:close()
   RETURN Qt_QProcess_close( ::pPtr )


METHOD QProcess:closeReadChannel( nChannel )
   RETURN Qt_QProcess_closeReadChannel( ::pPtr, nChannel )


METHOD QProcess:closeWriteChannel()
   RETURN Qt_QProcess_closeWriteChannel( ::pPtr )


METHOD QProcess:environment()
   RETURN HB_QStringList():from( Qt_QProcess_environment( ::pPtr ) )


METHOD QProcess:error()
   RETURN Qt_QProcess_error( ::pPtr )


METHOD QProcess:exitCode()
   RETURN Qt_QProcess_exitCode( ::pPtr )


METHOD QProcess:exitStatus()
   RETURN Qt_QProcess_exitStatus( ::pPtr )


METHOD QProcess:processChannelMode()
   RETURN Qt_QProcess_processChannelMode( ::pPtr )


METHOD QProcess:readAllStandardError()
   RETURN HB_QByteArray():from( Qt_QProcess_readAllStandardError( ::pPtr ) )


METHOD QProcess:readAllStandardOutput()
   RETURN HB_QByteArray():from( Qt_QProcess_readAllStandardOutput( ::pPtr ) )


METHOD QProcess:readChannel()
   RETURN Qt_QProcess_readChannel( ::pPtr )


METHOD QProcess:setEnvironment( pEnvironment )
   RETURN Qt_QProcess_setEnvironment( ::pPtr, hbqt_ptr( pEnvironment ) )


METHOD QProcess:setProcessChannelMode( nMode )
   RETURN Qt_QProcess_setProcessChannelMode( ::pPtr, nMode )


METHOD QProcess:setReadChannel( nChannel )
   RETURN Qt_QProcess_setReadChannel( ::pPtr, nChannel )


METHOD QProcess:setStandardErrorFile( cFileName, nMode )
   RETURN Qt_QProcess_setStandardErrorFile( ::pPtr, cFileName, nMode )


METHOD QProcess:setStandardInputFile( cFileName )
   RETURN Qt_QProcess_setStandardInputFile( ::pPtr, cFileName )


METHOD QProcess:setStandardOutputFile( cFileName, nMode )
   RETURN Qt_QProcess_setStandardOutputFile( ::pPtr, cFileName, nMode )


METHOD QProcess:setStandardOutputProcess( pDestination )
   RETURN Qt_QProcess_setStandardOutputProcess( ::pPtr, hbqt_ptr( pDestination ) )


METHOD QProcess:setWorkingDirectory( cDir )
   RETURN Qt_QProcess_setWorkingDirectory( ::pPtr, cDir )


METHOD QProcess:start( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QProcess_start( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_start_1( ::pPtr, ... )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_start( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_start_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QProcess:state()
   RETURN Qt_QProcess_state( ::pPtr )


METHOD QProcess:waitForFinished( nMsecs )
   RETURN Qt_QProcess_waitForFinished( ::pPtr, nMsecs )


METHOD QProcess:waitForStarted( nMsecs )
   RETURN Qt_QProcess_waitForStarted( ::pPtr, nMsecs )


METHOD QProcess:workingDirectory()
   RETURN Qt_QProcess_workingDirectory( ::pPtr )


METHOD QProcess:execute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_execute( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_execute_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QProcess:startDetached( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QProcess_startDetached( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QProcess_startDetached( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_startDetached_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_startDetached_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QProcess:systemEnvironment()
   RETURN HB_QStringList():from( Qt_QProcess_systemEnvironment( ::pPtr ) )


METHOD QProcess:kill()
   RETURN Qt_QProcess_kill( ::pPtr )


METHOD QProcess:terminate()
   RETURN Qt_QProcess_terminate( ::pPtr )

