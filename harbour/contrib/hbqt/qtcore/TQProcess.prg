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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QProcess INHERIT QIODevice

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  close()                             INLINE  Qt_QProcess_close( ::pPtr )
   METHOD  closeReadChannel( nChannel )        INLINE  Qt_QProcess_closeReadChannel( ::pPtr, nChannel )
   METHOD  closeWriteChannel()                 INLINE  Qt_QProcess_closeWriteChannel( ::pPtr )
   METHOD  environment()                       INLINE  Qt_QProcess_environment( ::pPtr )
   METHOD  error()                             INLINE  Qt_QProcess_error( ::pPtr )
   METHOD  exitCode()                          INLINE  Qt_QProcess_exitCode( ::pPtr )
   METHOD  exitStatus()                        INLINE  Qt_QProcess_exitStatus( ::pPtr )
   METHOD  processChannelMode()                INLINE  Qt_QProcess_processChannelMode( ::pPtr )
   METHOD  readAllStandardError()              INLINE  Qt_QProcess_readAllStandardError( ::pPtr )
   METHOD  readAllStandardOutput()             INLINE  Qt_QProcess_readAllStandardOutput( ::pPtr )
   METHOD  readChannel()                       INLINE  Qt_QProcess_readChannel( ::pPtr )
   METHOD  setEnvironment( pEnvironment )      INLINE  Qt_QProcess_setEnvironment( ::pPtr, pEnvironment )
   METHOD  setProcessChannelMode( nMode )      INLINE  Qt_QProcess_setProcessChannelMode( ::pPtr, nMode )
   METHOD  setReadChannel( nChannel )          INLINE  Qt_QProcess_setReadChannel( ::pPtr, nChannel )
   METHOD  setStandardErrorFile( cFileName, nMode )  INLINE  Qt_QProcess_setStandardErrorFile( ::pPtr, cFileName, nMode )
   METHOD  setStandardInputFile( cFileName )   INLINE  Qt_QProcess_setStandardInputFile( ::pPtr, cFileName )
   METHOD  setStandardOutputFile( cFileName, nMode )  INLINE  Qt_QProcess_setStandardOutputFile( ::pPtr, cFileName, nMode )
   METHOD  setStandardOutputProcess( pDestination )  INLINE  Qt_QProcess_setStandardOutputProcess( ::pPtr, pDestination )
   METHOD  setWorkingDirectory( cDir )         INLINE  Qt_QProcess_setWorkingDirectory( ::pPtr, cDir )
   METHOD  start( cProgram, pArguments, nMode )  INLINE  Qt_QProcess_start( ::pPtr, cProgram, pArguments, nMode )
   METHOD  start_1( cProgram, nMode )          INLINE  Qt_QProcess_start_1( ::pPtr, cProgram, nMode )
   METHOD  state()                             INLINE  Qt_QProcess_state( ::pPtr )
   METHOD  waitForFinished( nMsecs )           INLINE  Qt_QProcess_waitForFinished( ::pPtr, nMsecs )
   METHOD  waitForStarted( nMsecs )            INLINE  Qt_QProcess_waitForStarted( ::pPtr, nMsecs )
   METHOD  workingDirectory()                  INLINE  Qt_QProcess_workingDirectory( ::pPtr )
   METHOD  execute( cProgram, pArguments )     INLINE  Qt_QProcess_execute( ::pPtr, cProgram, pArguments )
   METHOD  execute_1( cProgram )               INLINE  Qt_QProcess_execute_1( ::pPtr, cProgram )
   METHOD  startDetached( cProgram, pArguments, cWorkingDirectory, nPid )  INLINE  Qt_QProcess_startDetached( ::pPtr, cProgram, pArguments, cWorkingDirectory, nPid )
   METHOD  startDetached_1( cProgram, pArguments )  INLINE  Qt_QProcess_startDetached_1( ::pPtr, cProgram, pArguments )
   METHOD  startDetached_2( cProgram )         INLINE  Qt_QProcess_startDetached_2( ::pPtr, cProgram )
   METHOD  systemEnvironment()                 INLINE  Qt_QProcess_systemEnvironment( ::pPtr )
   METHOD  kill()                              INLINE  Qt_QProcess_kill( ::pPtr )
   METHOD  terminate()                         INLINE  Qt_QProcess_terminate( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QProcess

   ::pParent := pParent

   ::pPtr := Qt_QProcess( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QProcess

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
