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

#include "../hbqt.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Encoding { CodecForTr, UnicodeUTF8, DefaultCodec }
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtCore/QCoreApplication>


/*
 * QCoreApplication ( int & argc, char ** argv )
 * ~QCoreApplication ()
 */

typedef struct
{
   QPointer< QCoreApplication > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QCoreApplication;

QT_G_FUNC( hbqt_gcRelease_QCoreApplication )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCoreApplication( void * pObj, bool bNew )
{
   QGC_POINTER_QCoreApplication * p = ( QGC_POINTER_QCoreApplication * ) hb_gcAllocate( sizeof( QGC_POINTER_QCoreApplication ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCoreApplication >( ( QCoreApplication * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCoreApplication;
   p->type = HBQT_TYPE_QCoreApplication;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QCoreApplication  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QCoreApplication", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCOREAPPLICATION )
{

}

/*
 * virtual bool notify ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_NOTIFY )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->notify( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_NOTIFY FP=hb_retl( ( p )->notify( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void addLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_ADDLIBRARYPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->addLibraryPath( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_ADDLIBRARYPATH FP=( p )->addLibraryPath( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString applicationDirPath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONDIRPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->applicationDirPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_APPLICATIONDIRPATH FP=hb_retc( ( p )->applicationDirPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString applicationFilePath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONFILEPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->applicationFilePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_APPLICATIONFILEPATH FP=hb_retc( ( p )->applicationFilePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString applicationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->applicationName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_APPLICATIONNAME FP=hb_retc( ( p )->applicationName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * qint64 applicationPid ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONPID )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retnint( ( p )->applicationPid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_APPLICATIONPID FP=hb_retnint( ( p )->applicationPid() ); p is NULL" ) );
   }
}

/*
 * QString applicationVersion ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONVERSION )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->applicationVersion().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_APPLICATIONVERSION FP=hb_retc( ( p )->applicationVersion().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList arguments ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ARGUMENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->arguments() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_ARGUMENTS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->arguments() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool closingDown ()
 */
HB_FUNC( QT_QCOREAPPLICATION_CLOSINGDOWN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->closingDown() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_CLOSINGDOWN FP=hb_retl( ( p )->closingDown() ); p is NULL" ) );
   }
}

/*
 * int exec ()
 */
HB_FUNC( QT_QCOREAPPLICATION_EXEC )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retni( ( p )->exec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_EXEC FP=hb_retni( ( p )->exec() ); p is NULL" ) );
   }
}

/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QCOREAPPLICATION_EXIT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->exit( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_EXIT FP=( p )->exit( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void flush ()
 */
HB_FUNC( QT_QCOREAPPLICATION_FLUSH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->flush();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_FLUSH FP=( p )->flush(); p is NULL" ) );
   }
}

/*
 * bool hasPendingEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_HASPENDINGEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->hasPendingEvents() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_HASPENDINGEVENTS FP=hb_retl( ( p )->hasPendingEvents() ); p is NULL" ) );
   }
}

/*
 * void installTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTALLTRANSLATOR )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->installTranslator( hbqt_par_QTranslator( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_INSTALLTRANSLATOR FP=( p )->installTranslator( hbqt_par_QTranslator( 2 ) ); p is NULL" ) );
   }
}

/*
 * QCoreApplication * instance ()
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTANCE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCoreApplication( ( p )->instance(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_INSTANCE FP=hb_retptrGC( hbqt_gcAllocate_QCoreApplication( ( p )->instance(), false ) ); p is NULL" ) );
   }
}

/*
 * QStringList libraryPaths ()
 */
HB_FUNC( QT_QCOREAPPLICATION_LIBRARYPATHS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->libraryPaths() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_LIBRARYPATHS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->libraryPaths() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString organizationDomain ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONDOMAIN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->organizationDomain().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_ORGANIZATIONDOMAIN FP=hb_retc( ( p )->organizationDomain().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString organizationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->organizationName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_ORGANIZATIONNAME FP=hb_retc( ( p )->organizationName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void postEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_POSTEVENT FP=( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ); p is NULL" ) );
   }
}

/*
 * void postEvent ( QObject * receiver, QEvent * event, int priority )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_POSTEVENT_1 FP=( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags = QEventLoop::AllEvents )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_PROCESSEVENTS FP=( p )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) ); p is NULL" ) );
   }
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags, int maxtime )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_PROCESSEVENTS_1 FP=( p )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVELIBRARYPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->removeLibraryPath( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_REMOVELIBRARYPATH FP=( p )->removeLibraryPath( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void removePostedEvents ( QObject * receiver )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->removePostedEvents( hbqt_par_QObject( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS FP=( p )->removePostedEvents( hbqt_par_QObject( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removePostedEvents ( QObject * receiver, int eventType )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->removePostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS_1 FP=( p )->removePostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVETRANSLATOR )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->removeTranslator( hbqt_par_QTranslator( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_REMOVETRANSLATOR FP=( p )->removeTranslator( hbqt_par_QTranslator( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool sendEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDEVENT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->sendEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SENDEVENT FP=hb_retl( ( p )->sendEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void sendPostedEvents ( QObject * receiver, int event_type )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->sendPostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SENDPOSTEDEVENTS FP=( p )->sendPostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void sendPostedEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->sendPostedEvents();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SENDPOSTEDEVENTS_1 FP=( p )->sendPostedEvents(); p is NULL" ) );
   }
}

/*
 * void setApplicationName ( const QString & application )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setApplicationName( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETAPPLICATIONNAME FP=( p )->setApplicationName( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setApplicationVersion ( const QString & version )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONVERSION )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setApplicationVersion( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETAPPLICATIONVERSION FP=( p )->setApplicationVersion( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setAttribute ( Qt::ApplicationAttribute attribute, bool on = true )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETATTRIBUTE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETATTRIBUTE FP=( p )->setAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setLibraryPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETLIBRARYPATHS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setLibraryPaths( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETLIBRARYPATHS FP=( p )->setLibraryPaths( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOrganizationDomain ( const QString & orgDomain )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONDOMAIN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setOrganizationDomain( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETORGANIZATIONDOMAIN FP=( p )->setOrganizationDomain( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setOrganizationName ( const QString & orgName )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->setOrganizationName( QCoreApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_SETORGANIZATIONNAME FP=( p )->setOrganizationName( QCoreApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool startingUp ()
 */
HB_FUNC( QT_QCOREAPPLICATION_STARTINGUP )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->startingUp() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_STARTINGUP FP=hb_retl( ( p )->startingUp() ); p is NULL" ) );
   }
}

/*
 * bool testAttribute ( Qt::ApplicationAttribute attribute )
 */
HB_FUNC( QT_QCOREAPPLICATION_TESTATTRIBUTE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retl( ( p )->testAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_TESTATTRIBUTE FP=hb_retl( ( p )->testAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation, Encoding encoding, int n )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( QCoreApplication::Encoding ) hb_parni( 5 ), hb_parni( 6 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_TRANSLATE FP=hb_retc( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( QCoreApplication::Encoding ) hb_parni( 5 ), hb_parni( 6 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation = 0, Encoding encoding = CodecForTr )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      hb_retc( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( HB_ISNUM( 5 ) ? ( QCoreApplication::Encoding ) hb_parni( 5 ) : ( QCoreApplication::Encoding ) QCoreApplication::CodecForTr ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_TRANSLATE_1 FP=hb_retc( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( HB_ISNUM( 5 ) ? ( QCoreApplication::Encoding ) hb_parni( 5 ) : ( QCoreApplication::Encoding ) QCoreApplication::CodecForTr ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void quit ()
 */
HB_FUNC( QT_QCOREAPPLICATION_QUIT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
      ( p )->quit();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOREAPPLICATION_QUIT FP=( p )->quit(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
