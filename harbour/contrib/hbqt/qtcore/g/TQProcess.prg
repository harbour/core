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
   METHOD  start( cProgram, pArguments, nMode )
   METHOD  start_1( cProgram, nMode )
   METHOD  state()
   METHOD  waitForFinished( nMsecs )
   METHOD  waitForStarted( nMsecs )
   METHOD  workingDirectory()
   METHOD  execute( cProgram, pArguments )
   METHOD  execute_1( cProgram )
   METHOD  startDetached( cProgram, pArguments, cWorkingDirectory, nPid )
   METHOD  startDetached_1( cProgram, pArguments )
   METHOD  startDetached_2( cProgram )
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
   RETURN Qt_QProcess_environment( ::pPtr )


METHOD QProcess:error()
   RETURN Qt_QProcess_error( ::pPtr )


METHOD QProcess:exitCode()
   RETURN Qt_QProcess_exitCode( ::pPtr )


METHOD QProcess:exitStatus()
   RETURN Qt_QProcess_exitStatus( ::pPtr )


METHOD QProcess:processChannelMode()
   RETURN Qt_QProcess_processChannelMode( ::pPtr )


METHOD QProcess:readAllStandardError()
   RETURN Qt_QProcess_readAllStandardError( ::pPtr )


METHOD QProcess:readAllStandardOutput()
   RETURN Qt_QProcess_readAllStandardOutput( ::pPtr )


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


METHOD QProcess:start( cProgram, pArguments, nMode )
   RETURN Qt_QProcess_start( ::pPtr, cProgram, hbqt_ptr( pArguments ), nMode )


METHOD QProcess:start_1( cProgram, nMode )
   RETURN Qt_QProcess_start_1( ::pPtr, cProgram, nMode )


METHOD QProcess:state()
   RETURN Qt_QProcess_state( ::pPtr )


METHOD QProcess:waitForFinished( nMsecs )
   RETURN Qt_QProcess_waitForFinished( ::pPtr, nMsecs )


METHOD QProcess:waitForStarted( nMsecs )
   RETURN Qt_QProcess_waitForStarted( ::pPtr, nMsecs )


METHOD QProcess:workingDirectory()
   RETURN Qt_QProcess_workingDirectory( ::pPtr )


METHOD QProcess:execute( cProgram, pArguments )
   RETURN Qt_QProcess_execute( ::pPtr, cProgram, hbqt_ptr( pArguments ) )


METHOD QProcess:execute_1( cProgram )
   RETURN Qt_QProcess_execute_1( ::pPtr, cProgram )


METHOD QProcess:startDetached( cProgram, pArguments, cWorkingDirectory, nPid )
   RETURN Qt_QProcess_startDetached( ::pPtr, cProgram, hbqt_ptr( pArguments ), cWorkingDirectory, nPid )


METHOD QProcess:startDetached_1( cProgram, pArguments )
   RETURN Qt_QProcess_startDetached_1( ::pPtr, cProgram, hbqt_ptr( pArguments ) )


METHOD QProcess:startDetached_2( cProgram )
   RETURN Qt_QProcess_startDetached_2( ::pPtr, cProgram )


METHOD QProcess:systemEnvironment()
   RETURN Qt_QProcess_systemEnvironment( ::pPtr )


METHOD QProcess:kill()
   RETURN Qt_QProcess_kill( ::pPtr )


METHOD QProcess:terminate()
   RETURN Qt_QProcess_terminate( ::pPtr )

