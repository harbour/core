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

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  # FROM QIODevice
 *  flags OpenMode
 *  enum OpenModeFlag { NotOpen, ReadOnly, WriteOnly, ReadWrite, ..., Unbuffered }
 *  enum ExitStatus { NormalExit, CrashExit }
 *  enum ProcessChannel { StandardOutput, StandardError }
 *  enum ProcessChannelMode { SeparateChannels, MergedChannels, ForwardedChannels }
 *  enum ProcessError { FailedToStart, Crashed, Timedout, WriteError, ReadError, UnknownError }
 *  enum ProcessState { NotRunning, Starting, Running }
 */


#include <QtCore/QProcess>


/*
 * QProcess ( QObject * parent = 0 )
 * virtual ~QProcess ()
 */
HB_FUNC( QT_QPROCESS )
{
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      hb_retptr( ( QProcess* ) new QProcess( hbqt_par_QObject( 1 ) ) );
   }
   else
   {
      hb_retptr( ( QProcess* ) new QProcess() );
   }
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QPROCESS_DESTROY )
{
   delete hbqt_par_QProcess( 1 );
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QPROCESS_CLOSE )
{
   hbqt_par_QProcess( 1 )->close();
}

/*
 * void closeReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_CLOSEREADCHANNEL )
{
   hbqt_par_QProcess( 1 )->closeReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
}

/*
 * void closeWriteChannel ()
 */
HB_FUNC( QT_QPROCESS_CLOSEWRITECHANNEL )
{
   hbqt_par_QProcess( 1 )->closeWriteChannel();
}

/*
 * QStringList environment () const
 */
HB_FUNC( QT_QPROCESS_ENVIRONMENT )
{
   hb_retptr( new QStringList( hbqt_par_QProcess( 1 )->environment() ) );
}

/*
 * QProcess::ProcessError error () const
 */
HB_FUNC( QT_QPROCESS_ERROR )
{
   hb_retni( ( QProcess::ProcessError ) hbqt_par_QProcess( 1 )->error() );
}

/*
 * int exitCode () const
 */
HB_FUNC( QT_QPROCESS_EXITCODE )
{
   hb_retni( hbqt_par_QProcess( 1 )->exitCode() );
}

/*
 * QProcess::ExitStatus exitStatus () const
 */
HB_FUNC( QT_QPROCESS_EXITSTATUS )
{
   hb_retni( ( QProcess::ExitStatus ) hbqt_par_QProcess( 1 )->exitStatus() );
}

/*
 * Q_PID pid () const
 */
HB_FUNC( QT_QPROCESS_PID )
{
   hb_retptr( new Q_PID( hbqt_par_QProcess( 1 )->pid() ) );
}

/*
 * ProcessChannelMode processChannelMode () const
 */
HB_FUNC( QT_QPROCESS_PROCESSCHANNELMODE )
{
   hb_retni( ( QProcess::ProcessChannelMode ) hbqt_par_QProcess( 1 )->processChannelMode() );
}

/*
 * QByteArray readAllStandardError ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDERROR )
{
   hb_retptr( new QByteArray( hbqt_par_QProcess( 1 )->readAllStandardError() ) );
}

/*
 * QByteArray readAllStandardOutput ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDOUTPUT )
{
   hb_retptr( new QByteArray( hbqt_par_QProcess( 1 )->readAllStandardOutput() ) );
}

/*
 * ProcessChannel readChannel () const
 */
HB_FUNC( QT_QPROCESS_READCHANNEL )
{
   hb_retni( ( QProcess::ProcessChannel ) hbqt_par_QProcess( 1 )->readChannel() );
}

/*
 * void setEnvironment ( const QStringList & environment )
 */
HB_FUNC( QT_QPROCESS_SETENVIRONMENT )
{
   hbqt_par_QProcess( 1 )->setEnvironment( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setProcessChannelMode ( ProcessChannelMode mode )
 */
HB_FUNC( QT_QPROCESS_SETPROCESSCHANNELMODE )
{
   hbqt_par_QProcess( 1 )->setProcessChannelMode( ( QProcess::ProcessChannelMode ) hb_parni( 2 ) );
}

/*
 * void setReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_SETREADCHANNEL )
{
   hbqt_par_QProcess( 1 )->setReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
}

/*
 * void setStandardErrorFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDERRORFILE )
{
   hbqt_par_QProcess( 1 )->setStandardErrorFile( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
}

/*
 * void setStandardInputFile ( const QString & fileName )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDINPUTFILE )
{
   hbqt_par_QProcess( 1 )->setStandardInputFile( hbqt_par_QString( 2 ) );
}

/*
 * void setStandardOutputFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTFILE )
{
   hbqt_par_QProcess( 1 )->setStandardOutputFile( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
}

/*
 * void setStandardOutputProcess ( QProcess * destination )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTPROCESS )
{
   hbqt_par_QProcess( 1 )->setStandardOutputProcess( hbqt_par_QProcess( 2 ) );
}

/*
 * void setWorkingDirectory ( const QString & dir )
 */
HB_FUNC( QT_QPROCESS_SETWORKINGDIRECTORY )
{
   hbqt_par_QProcess( 1 )->setWorkingDirectory( hbqt_par_QString( 2 ) );
}

/*
 * void start ( const QString & program, const QStringList & arguments, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START )
{
   hbqt_par_QProcess( 1 )->start( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ), ( HB_ISNUM( 4 ) ? ( QProcess::OpenMode ) hb_parni( 4 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
}

/*
 * void start ( const QString & program, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START_1 )
{
   hbqt_par_QProcess( 1 )->start( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
}

/*
 * QProcess::ProcessState state () const
 */
HB_FUNC( QT_QPROCESS_STATE )
{
   hb_retni( ( QProcess::ProcessState ) hbqt_par_QProcess( 1 )->state() );
}

/*
 * bool waitForFinished ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORFINISHED )
{
   hb_retl( hbqt_par_QProcess( 1 )->waitForFinished( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 30000 ) ) );
}

/*
 * bool waitForStarted ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORSTARTED )
{
   hb_retl( hbqt_par_QProcess( 1 )->waitForStarted( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 30000 ) ) );
}

/*
 * QString workingDirectory () const
 */
HB_FUNC( QT_QPROCESS_WORKINGDIRECTORY )
{
   hb_retc( hbqt_par_QProcess( 1 )->workingDirectory().toAscii().data() );
}

/*
 * int execute ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_EXECUTE )
{
   hb_retni( hbqt_par_QProcess( 1 )->execute( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) ) );
}

/*
 * int execute ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_EXECUTE_1 )
{
   hb_retni( hbqt_par_QProcess( 1 )->execute( hbqt_par_QString( 2 ) ) );
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments, const QString & workingDirectory, qint64 * pid = 0 )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED )
{
   qint64 iPid = 0;

   hb_retl( hbqt_par_QProcess( 1 )->startDetached( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ), hbqt_par_QString( 4 ), &iPid ) );

   hb_stornint( iPid, 5 );
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_1 )
{
   hb_retl( hbqt_par_QProcess( 1 )->startDetached( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) ) );
}

/*
 * bool startDetached ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_2 )
{
   hb_retl( hbqt_par_QProcess( 1 )->startDetached( hbqt_par_QString( 2 ) ) );
}

/*
 * QStringList systemEnvironment ()
 */
HB_FUNC( QT_QPROCESS_SYSTEMENVIRONMENT )
{
   hb_retptr( new QStringList( hbqt_par_QProcess( 1 )->systemEnvironment() ) );
}

/*
 * void kill ()
 */
HB_FUNC( QT_QPROCESS_KILL )
{
   hbqt_par_QProcess( 1 )->kill();
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QPROCESS_TERMINATE )
{
   hbqt_par_QProcess( 1 )->terminate();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
