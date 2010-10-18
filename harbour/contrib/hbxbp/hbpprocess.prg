/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                      Xbase++ Compatible Library
 *                          Harbour Extension
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               24Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

#define CHN_BGN                                   1
#define CHN_OUT                                   2
#define CHN_ERR                                   3
#define CHN_FIN                                   4
#define CHN_STT                                   5
#define CHN_ERE                                   6
#define CHN_CLO                                   7
#define CHN_BYT                                   8
#define CHN_RCF                                   9
#define CHN_REA                                   10

/*----------------------------------------------------------------------*/
//
//                           Class HbpProcess
//
/*----------------------------------------------------------------------*/

CLASS HbpProcess

   DATA   cShellCmd
   DATA   lDetached                               INIT   .f.

   METHOD new( cShellCmd )
   METHOD create( cShellCmd )
   METHOD destroy()                               VIRTUAL
   METHOD addArg( cArg )
   METHOD start( cShellCmd )

   METHOD waitForFinished()                       INLINE ::qProcess:waitForFinished()
   METHOD finished( bBlock )                      SETGET                 // Slot
   METHOD output( bBlock )                        SETGET                 // Slot
   METHOD workingPath( cPath )                    SETGET                 // Slot

   ACCESS started                                 INLINE ::nStarted
   ACCESS ended                                   INLINE ::nEnded
   ACCESS exitCode                                INLINE ::nExitCode
   ACCESS exitStatus                              INLINE ::nExitStatus

   PROTECTED:

   DATA   nStarted                                INIT   0
   DATA   nEnded                                  INIT   0
   DATA   nExitCode                               INIT   -1
   DATA   nExitStatus                             INIT   -1
   DATA   cWrkDirectory                           INIT   ""
   DATA   qProcess
   DATA   qStrList
   DATA   bFinish
   DATA   bOutput

   METHOD read( nMode, i, ii )
   METHOD outputMe( cLine, nMode )
   METHOD finish()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpProcess:new( cShellCmd )

   ::cShellCmd := cShellCmd

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpProcess:create( cShellCmd )

   DEFAULT cShellCmd TO ::cShellCmd

   ::cShellCmd := cShellCmd

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpProcess:workingPath( cPath )

   IF !empty( cPath )
      ::cWrkDirectory := cPath
   ENDIF
   RETURN ::cWrkDirectory

/*----------------------------------------------------------------------*/

METHOD HbpProcess:finished( bBlock )

   IF hb_isBlock( bBlock )
      ::bFinish := bBlock
   ENDIF

   RETURN ::bFinish

/*----------------------------------------------------------------------*/

METHOD HbpProcess:output( bBlock )

   IF hb_isBlock( bBlock )
      ::bOutput := bBlock
   ENDIF

   RETURN ::bFinish

/*----------------------------------------------------------------------*/

METHOD HbpProcess:addArg( cArg )

   IF empty( ::qStrList )
      ::qStrList := QStringList()
   ENDIF
   ::qStrList:append( cArg )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpProcess:start( cShellCmd )

   DEFAULT cShellCmd TO ::cShellCmd

   ::cShellCmd := cShellCmd

   ::qProcess := QProcess()
   ::qProcess:setReadChannel( 1 )

   #if 0
      ::qProcess:connect( "readyRead()"              , {|i| ::read( CHN_REA, i ) } )
      ::qProcess:connect( "readChannelFinished()"    , {|i| ::read( CHN_RCF, i ) } )
      ::qProcess:connect( "aboutToClose()"           , {|i| ::read( CHN_CLO, i ) } )
      ::qProcess:connect( "bytesWritten(qint64)"     , {|i| ::read( CHN_BYT, i ) } )
      ::qProcess:connect( "stateChanged(int)"        , {|i| ::read( CHN_STT, i ) } )
      ::qProcess:connect( "error(int)"               , {|i| ::read( CHN_ERE, i ) } )
   #else
   IF !( ::lDetached )
      ::qProcess:connect( "started()"                , {|i| ::read( CHN_BGN, i ) } )
      ::qProcess:connect( "readyReadStandardOutput()", {|i| ::read( CHN_OUT, i ) } )
      ::qProcess:connect( "readyReadStandardError()" , {|i| ::read( CHN_ERR, i ) } )
      ::qProcess:connect( "finished(int,QProcess::ExitStatus)", {|i,ii| ::read( CHN_FIN, i, ii ) } )
   ENDIF
   #endif

   IF !empty( ::cWrkDirectory )
      ::qProcess:setWorkingDirectory( ::cWrkDirectory )
   ENDIF
   ::nStarted := seconds()

   IF ::lDetached
      IF !empty( ::qStrList )
         ::qProcess:startDetached( ::cShellCmd, ::qStrList )
      ELSE
         ::qProcess:startDetached( ::cShellCmd )
      ENDIF
      ::qProcess:waitForStarted()
   ELSE
      IF !empty( ::qStrList )
         ::qProcess:start( ::cShellCmd, ::qStrList )
      ELSE
         ::qProcess:start( ::cShellCmd )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpProcess:read( nMode, i, ii )
   LOCAL cLine, nSize

   nSize := 16384

   DO CASE
   CASE nMode == CHN_BGN
      ::outputMe( "CurDir() => " + CurDir() + "   Starting in => " + ::qProcess:workingDirectory(), CHN_BGN )

   CASE nMode == CHN_OUT
      ::qProcess:setReadChannel( 0 )
      cLine := space( nSize )
      ::qProcess:read( @cLine, nSize )
      ::outputMe( cLine, CHN_OUT )

   CASE nMode == CHN_ERR
      ::qProcess:setReadChannel( 1 )
      cLine := space( nSize )
      ::qProcess:read( @cLine, nSize )
      ::outputMe( cLine, CHN_ERR )

   CASE nMode == CHN_FIN
      ::nExitCode   := i
      ::nExitStatus := ii
      ::nEnded      := Seconds()
      ::finish()

   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbpProcess:outputMe( cLine, nMode )

   HB_SYMBOL_UNUSED( nMode )

   IF hb_isBlock( ::bOutput ) .AND. !empty( cLine )
      eval( ::bOutput, trim( cLine ), NIL, Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpProcess:finish()

   #if 0
   ::qProcess:disconnect( "readyRead()"               )
   ::qProcess:disconnect( "readChannelFinished()"     )
   ::qProcess:disconnect( "aboutToClose()"            )
   ::qProcess:disconnect( "bytesWritten(qint64)"      )
   ::qProcess:disconnect( "stateChanged(int)"         )
   ::qProcess:disconnect( "error(int)"                )
   #endif
   ::qProcess:disconnect( "started()"                 )
   ::qProcess:disconnect( "readyReadStandardOutput()" )
   ::qProcess:disconnect( "readyReadStandardError()"  )
   ::qProcess:disconnect( "finished(int,QProcess::ExitStatus)" )

   IF hb_isBlock( ::bFinish )
      eval( ::bFinish, ::nExitCode, ::nExitStatus, Self )
   ENDIF

   ::qProcess:kill()
   //
   ::qProcess := NIL
   ::qStrList := NIL

   RETURN Self

/*----------------------------------------------------------------------*/
