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

#include "hbqtcore.h"

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
   QPointer< QProcess > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QProcess;

QT_G_FUNC( hbqt_gcRelease_QProcess )
{
   QProcess  * ph = NULL ;
   QGC_POINTER_QProcess * p = ( QGC_POINTER_QProcess * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProcess   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProcess   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QProcess          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QProcess    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QProcess    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProcess( void * pObj, bool bNew )
{
   QGC_POINTER_QProcess * p = ( QGC_POINTER_QProcess * ) hb_gcAllocate( sizeof( QGC_POINTER_QProcess ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QProcess >( ( QProcess * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProcess;
   p->type = HBQT_TYPE_QProcess;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QProcess  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QProcess", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPROCESS )
{
   QProcess * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QProcess( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj =  new QProcess() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QProcess( ( void * ) pObj, true ) );
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QPROCESS_CLOSE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->close();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_CLOSE FP=( p )->close(); p is NULL" ) );
   }
}

/*
 * void closeReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_CLOSEREADCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->closeReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_CLOSEREADCHANNEL FP=( p )->closeReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void closeWriteChannel ()
 */
HB_FUNC( QT_QPROCESS_CLOSEWRITECHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->closeWriteChannel();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_CLOSEWRITECHANNEL FP=( p )->closeWriteChannel(); p is NULL" ) );
   }
}

/*
 * QStringList environment () const
 */
HB_FUNC( QT_QPROCESS_ENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->environment() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_ENVIRONMENT FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->environment() ), true ) ); p is NULL" ) );
   }
}

/*
 * QProcess::ProcessError error () const
 */
HB_FUNC( QT_QPROCESS_ERROR )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( QProcess::ProcessError ) ( p )->error() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_ERROR FP=hb_retni( ( QProcess::ProcessError ) ( p )->error() ); p is NULL" ) );
   }
}

/*
 * int exitCode () const
 */
HB_FUNC( QT_QPROCESS_EXITCODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( p )->exitCode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_EXITCODE FP=hb_retni( ( p )->exitCode() ); p is NULL" ) );
   }
}

/*
 * QProcess::ExitStatus exitStatus () const
 */
HB_FUNC( QT_QPROCESS_EXITSTATUS )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( QProcess::ExitStatus ) ( p )->exitStatus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_EXITSTATUS FP=hb_retni( ( QProcess::ExitStatus ) ( p )->exitStatus() ); p is NULL" ) );
   }
}

/*
 * ProcessChannelMode processChannelMode () const
 */
HB_FUNC( QT_QPROCESS_PROCESSCHANNELMODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( QProcess::ProcessChannelMode ) ( p )->processChannelMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_PROCESSCHANNELMODE FP=hb_retni( ( QProcess::ProcessChannelMode ) ( p )->processChannelMode() ); p is NULL" ) );
   }
}

/*
 * QByteArray readAllStandardError ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDERROR )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardError() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_READALLSTANDARDERROR FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardError() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray readAllStandardOutput ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDOUTPUT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardOutput() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_READALLSTANDARDOUTPUT FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardOutput() ), true ) ); p is NULL" ) );
   }
}

/*
 * ProcessChannel readChannel () const
 */
HB_FUNC( QT_QPROCESS_READCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( QProcess::ProcessChannel ) ( p )->readChannel() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_READCHANNEL FP=hb_retni( ( QProcess::ProcessChannel ) ( p )->readChannel() ); p is NULL" ) );
   }
}

/*
 * void setEnvironment ( const QStringList & environment )
 */
HB_FUNC( QT_QPROCESS_SETENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setEnvironment( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETENVIRONMENT FP=( p )->setEnvironment( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setProcessChannelMode ( ProcessChannelMode mode )
 */
HB_FUNC( QT_QPROCESS_SETPROCESSCHANNELMODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setProcessChannelMode( ( QProcess::ProcessChannelMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETPROCESSCHANNELMODE FP=( p )->setProcessChannelMode( ( QProcess::ProcessChannelMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_SETREADCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETREADCHANNEL FP=( p )->setReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStandardErrorFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDERRORFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setStandardErrorFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETSTANDARDERRORFILE FP=( p )->setStandardErrorFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) ); p is NULL" ) );
   }
}

/*
 * void setStandardInputFile ( const QString & fileName )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDINPUTFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setStandardInputFile( QProcess::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETSTANDARDINPUTFILE FP=( p )->setStandardInputFile( QProcess::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setStandardOutputFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setStandardOutputFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETSTANDARDOUTPUTFILE FP=( p )->setStandardOutputFile( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) ); p is NULL" ) );
   }
}

/*
 * void setStandardOutputProcess ( QProcess * destination )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTPROCESS )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setStandardOutputProcess( hbqt_par_QProcess( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETSTANDARDOUTPUTPROCESS FP=( p )->setStandardOutputProcess( hbqt_par_QProcess( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWorkingDirectory ( const QString & dir )
 */
HB_FUNC( QT_QPROCESS_SETWORKINGDIRECTORY )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->setWorkingDirectory( QProcess::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SETWORKINGDIRECTORY FP=( p )->setWorkingDirectory( QProcess::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void start ( const QString & program, const QStringList & arguments, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->start( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), ( HB_ISNUM( 4 ) ? ( QProcess::OpenMode ) hb_parni( 4 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_START FP=( p )->start( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), ( HB_ISNUM( 4 ) ? ( QProcess::OpenMode ) hb_parni( 4 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) ); p is NULL" ) );
   }
}

/*
 * void start ( const QString & program, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START_1 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->start( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_START_1 FP=( p )->start( QProcess::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) ); p is NULL" ) );
   }
}

/*
 * QProcess::ProcessState state () const
 */
HB_FUNC( QT_QPROCESS_STATE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( QProcess::ProcessState ) ( p )->state() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_STATE FP=hb_retni( ( QProcess::ProcessState ) ( p )->state() ); p is NULL" ) );
   }
}

/*
 * bool waitForFinished ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORFINISHED )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retl( ( p )->waitForFinished( hb_parnidef( 2, 30000 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_WAITFORFINISHED FP=hb_retl( ( p )->waitForFinished( hb_parnidef( 2, 30000 ) ) ); p is NULL" ) );
   }
}

/*
 * bool waitForStarted ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORSTARTED )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retl( ( p )->waitForStarted( hb_parnidef( 2, 30000 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_WAITFORSTARTED FP=hb_retl( ( p )->waitForStarted( hb_parnidef( 2, 30000 ) ) ); p is NULL" ) );
   }
}

/*
 * QString workingDirectory () const
 */
HB_FUNC( QT_QPROCESS_WORKINGDIRECTORY )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retc( ( p )->workingDirectory().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_WORKINGDIRECTORY FP=hb_retc( ( p )->workingDirectory().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int execute ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_EXECUTE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( p )->execute( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_EXECUTE FP=hb_retni( ( p )->execute( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int execute ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_EXECUTE_1 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retni( ( p )->execute( QProcess::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_EXECUTE_1 FP=hb_retni( ( p )->execute( QProcess::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments, const QString & workingDirectory, qint64 * pid = 0 )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   qint64 iPid = 0;

   if( p )
      hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), QProcess::tr( hb_parc( 4 ) ), &iPid ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_STARTDETACHED FP=hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ), QProcess::tr( hb_parc( 4 ) ), &iPid ) ); p is NULL" ) );
   }

   hb_stornint( iPid, 5 );
}

/*
 * bool startDetached ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_1 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_STARTDETACHED_1 FP=hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool startDetached ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_2 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_STARTDETACHED_2 FP=hb_retl( ( p )->startDetached( QProcess::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList systemEnvironment ()
 */
HB_FUNC( QT_QPROCESS_SYSTEMENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->systemEnvironment() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_SYSTEMENVIRONMENT FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->systemEnvironment() ), true ) ); p is NULL" ) );
   }
}

/*
 * void kill ()
 */
HB_FUNC( QT_QPROCESS_KILL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->kill();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_KILL FP=( p )->kill(); p is NULL" ) );
   }
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QPROCESS_TERMINATE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
      ( p )->terminate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPROCESS_TERMINATE FP=( p )->terminate(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
