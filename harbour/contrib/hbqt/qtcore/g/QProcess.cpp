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

/*
 *  Constructed[ 33/33 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // Q_PID pid () const
 */

#include <QtCore/QPointer>

#include <QtCore/QProcess>


/*
 * QProcess ( QObject * parent = 0 )
 * ~QProcess ()
 */

typedef struct
{
   QPointer< QProcess > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QProcess;

HBQT_GC_FUNC( hbqt_gcRelease_QProcess )
{
   QProcess  * ph = NULL ;
   HBQT_GC_T_QProcess * p = ( HBQT_GC_T_QProcess * ) Cargo;

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
   HBQT_GC_T_QProcess * p = ( HBQT_GC_T_QProcess * ) hb_gcAllocate( sizeof( HBQT_GC_T_QProcess ), hbqt_gcFuncs() );

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
      pObj = new QProcess( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QProcess() ;
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
   {
      ( p )->close();
   }
}

/*
 * void closeReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_CLOSEREADCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->closeReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
   }
}

/*
 * void closeWriteChannel ()
 */
HB_FUNC( QT_QPROCESS_CLOSEWRITECHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->closeWriteChannel();
   }
}

/*
 * QStringList environment () const
 */
HB_FUNC( QT_QPROCESS_ENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->environment() ), true ) );
   }
}

/*
 * QProcess::ProcessError error () const
 */
HB_FUNC( QT_QPROCESS_ERROR )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( QProcess::ProcessError ) ( p )->error() );
   }
}

/*
 * int exitCode () const
 */
HB_FUNC( QT_QPROCESS_EXITCODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( p )->exitCode() );
   }
}

/*
 * QProcess::ExitStatus exitStatus () const
 */
HB_FUNC( QT_QPROCESS_EXITSTATUS )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( QProcess::ExitStatus ) ( p )->exitStatus() );
   }
}

/*
 * ProcessChannelMode processChannelMode () const
 */
HB_FUNC( QT_QPROCESS_PROCESSCHANNELMODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( QProcess::ProcessChannelMode ) ( p )->processChannelMode() );
   }
}

/*
 * QByteArray readAllStandardError ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDERROR )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardError() ), true ) );
   }
}

/*
 * QByteArray readAllStandardOutput ()
 */
HB_FUNC( QT_QPROCESS_READALLSTANDARDOUTPUT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAllStandardOutput() ), true ) );
   }
}

/*
 * ProcessChannel readChannel () const
 */
HB_FUNC( QT_QPROCESS_READCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( QProcess::ProcessChannel ) ( p )->readChannel() );
   }
}

/*
 * void setEnvironment ( const QStringList & environment )
 */
HB_FUNC( QT_QPROCESS_SETENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->setEnvironment( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setProcessChannelMode ( ProcessChannelMode mode )
 */
HB_FUNC( QT_QPROCESS_SETPROCESSCHANNELMODE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->setProcessChannelMode( ( QProcess::ProcessChannelMode ) hb_parni( 2 ) );
   }
}

/*
 * void setReadChannel ( ProcessChannel channel )
 */
HB_FUNC( QT_QPROCESS_SETREADCHANNEL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->setReadChannel( ( QProcess::ProcessChannel ) hb_parni( 2 ) );
   }
}

/*
 * void setStandardErrorFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDERRORFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStandardErrorFile( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
      hb_strfree( pText );
   }
}

/*
 * void setStandardInputFile ( const QString & fileName )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDINPUTFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStandardInputFile( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setStandardOutputFile ( const QString & fileName, OpenMode mode = Truncate )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTFILE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStandardOutputFile( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::Truncate ) );
      hb_strfree( pText );
   }
}

/*
 * void setStandardOutputProcess ( QProcess * destination )
 */
HB_FUNC( QT_QPROCESS_SETSTANDARDOUTPUTPROCESS )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->setStandardOutputProcess( hbqt_par_QProcess( 2 ) );
   }
}

/*
 * void setWorkingDirectory ( const QString & dir )
 */
HB_FUNC( QT_QPROCESS_SETWORKINGDIRECTORY )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWorkingDirectory( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void start ( const QString & program, const QStringList & arguments, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->start( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ), ( HB_ISNUM( 4 ) ? ( QProcess::OpenMode ) hb_parni( 4 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
      hb_strfree( pText );
   }
}

/*
 * void start ( const QString & program, OpenMode mode = ReadWrite )
 */
HB_FUNC( QT_QPROCESS_START_1 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      ( p )->start( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QProcess::OpenMode ) hb_parni( 3 ) : ( QProcess::OpenMode ) QProcess::ReadWrite ) );
      hb_strfree( pText );
   }
}

/*
 * QProcess::ProcessState state () const
 */
HB_FUNC( QT_QPROCESS_STATE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retni( ( QProcess::ProcessState ) ( p )->state() );
   }
}

/*
 * bool waitForFinished ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORFINISHED )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retl( ( p )->waitForFinished( hb_parnidef( 2, 30000 ) ) );
   }
}

/*
 * bool waitForStarted ( int msecs = 30000 )
 */
HB_FUNC( QT_QPROCESS_WAITFORSTARTED )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retl( ( p )->waitForStarted( hb_parnidef( 2, 30000 ) ) );
   }
}

/*
 * QString workingDirectory () const
 */
HB_FUNC( QT_QPROCESS_WORKINGDIRECTORY )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->workingDirectory().toUtf8().data() );
   }
}

/*
 * int execute ( const QString & program, const QStringList & arguments )
 */
HB_FUNC( QT_QPROCESS_EXECUTE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->execute( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int execute ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_EXECUTE_1 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->execute( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
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
   {
      void * pText;
      hb_retl( ( p )->startDetached( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ), hb_parstr_utf8( 4, &pText, NULL ), &iPid ) );
      hb_strfree( pText );
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
   {
      void * pText;
      hb_retl( ( p )->startDetached( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool startDetached ( const QString & program )
 */
HB_FUNC( QT_QPROCESS_STARTDETACHED_2 )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->startDetached( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList systemEnvironment ()
 */
HB_FUNC( QT_QPROCESS_SYSTEMENVIRONMENT )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->systemEnvironment() ), true ) );
   }
}

/*
 * void kill ()
 */
HB_FUNC( QT_QPROCESS_KILL )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->kill();
   }
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QPROCESS_TERMINATE )
{
   QProcess * p = hbqt_par_QProcess( 1 );
   if( p )
   {
      ( p )->terminate();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
