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

#include "../hbqt.h"

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

#include <QtCore/QPointer>

#include <QtCore/QProcess>


/*
 * QProcess ( QObject * parent = 0 )
 * virtual ~QProcess ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QProcess > pq;
} QGC_POINTER_QProcess;

QT_G_FUNC( hbqt_gcRelease_QProcess )
{
   QGC_POINTER_QProcess * p = ( QGC_POINTER_QProcess * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QProcess   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QProcess * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QProcess   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QProcessph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QProcess    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QProcess    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProcess( void * pObj, bool bNew )
{
   QGC_POINTER_QProcess * p = ( QGC_POINTER_QProcess * ) hb_gcAllocate( sizeof( QGC_POINTER_QProcess ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProcess;

   if( bNew )
   {
      new( & p->pq ) QPointer< QProcess >( ( QProcess * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QProcess                   ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPROCESS )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QProcess* ) new QProcess( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = ( QProcess* ) new QProcess() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QProcess( pObj, true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QProcess( 1 )->environment() ), true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QProcess( 1 )->readAllStandardError() ), true ) );
}

/*
 * QByteArray readAllStandardOutput ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDOUTPUT )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QProcess( 1 )->readAllStandardOutput() ), true ) );
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
   hbqt_par_QProcess( 1 )->setStandardErrorFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
}

/*
 * void setStandardInputFile ( const QString & fileName )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDINPUTFILE )
{
   hbqt_par_QProcess( 1 )->setStandardInputFile( QProcess::tr( hb_parc( 2 ) ) );
}

/*
 * void setStandardOutputFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTFILE )
{
   hbqt_par_QProcess( 1 )->setStandardOutputFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
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
   hbqt_par_QProcess( 1 )->setWorkingDirectory( QProcess::tr( hb_parc( 2 ) ) );
}

/*
 * void start ( const QString & program, const QStringList & arguments, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START )
{
   hbqt_par_QProcess( 1 )->start( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), ( HB_ISNUM( 4 ) ? ( QProcess::OpenMode ) hb_parni( 4 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
}

/*
 * void start ( const QString & program, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START_1 )
{
   hbqt_par_QProcess( 1 )->start( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
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
   hb_retni( hbqt_par_QProcess( 1 )->execute( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) );
}

/*
 * int execute ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_EXECUTE_1 )
{
   hb_retni( hbqt_par_QProcess( 1 )->execute( QProcess::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments, const QString & workingDirectory, qint64 * pid = 0 )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED )
{
   qint64 iPid = 0;

   hb_retl( hbqt_par_QProcess( 1 )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), QProcess::tr( hb_parc( 4 ) ), &iPid ) );

   hb_stornint( iPid, 5 );
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_1 )
{
   hb_retl( hbqt_par_QProcess( 1 )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) );
}

/*
 * bool startDetached ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_2 )
{
   hb_retl( hbqt_par_QProcess( 1 )->startDetached( QProcess::tr( hb_parc( 2 ) ) ) );
}

/*
 * QStringList systemEnvironment ()
 */
HB_FUNC( QT_QPROCESS_SYSTEMENVIRONMENT )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QProcess( 1 )->systemEnvironment() ), true ) );
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
