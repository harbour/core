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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Encoding { CodecForTr, UnicodeUTF8, DefaultCodec }
 */

/*
 *  Constructed[ 40/43 [ 93.02% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  EventFilter setEventFilter ( EventFilter filter )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // bool filterEvent ( void * message, long * result )
 *  // virtual bool winEventFilter ( MSG * msg, long * result )
 */

#include <QtCore/QPointer>

#include <QStringList>
#include <QtCore/QCoreApplication>


/*
 * QCoreApplication ( int & argc, char ** argv )
 * ~QCoreApplication ()
 */

QT_G_FUNC( release_QCoreApplication )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QCoreApplication" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QCoreApplication * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QCoreApplication" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QCOREAPPLICATION )
{
}
/*
 * virtual bool notify ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_NOTIFY )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->notify( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
}

/*
 * void addLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_ADDLIBRARYPATH )
{
   hbqt_par_QCoreApplication( 1 )->addLibraryPath( hbqt_par_QString( 2 ) );
}

/*
 * QString applicationDirPath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONDIRPATH )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->applicationDirPath().toAscii().data() );
}

/*
 * QString applicationFilePath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONFILEPATH )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->applicationFilePath().toAscii().data() );
}

/*
 * QString applicationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONNAME )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->applicationName().toAscii().data() );
}

/*
 * qint64 applicationPid ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONPID )
{
   hb_retnint( hbqt_par_QCoreApplication( 1 )->applicationPid() );
}

/*
 * QString applicationVersion ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONVERSION )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->applicationVersion().toAscii().data() );
}

/*
 * QStringList arguments ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ARGUMENTS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QCoreApplication( 1 )->arguments() ), release_QStringList ) );
}

/*
 * bool closingDown ()
 */
HB_FUNC( QT_QCOREAPPLICATION_CLOSINGDOWN )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->closingDown() );
}

/*
 * int exec ()
 */
HB_FUNC( QT_QCOREAPPLICATION_EXEC )
{
   hb_retni( hbqt_par_QCoreApplication( 1 )->exec() );
}

/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QCOREAPPLICATION_EXIT )
{
   hbqt_par_QCoreApplication( 1 )->exit( hb_parni( 2 ) );
}

/*
 * void flush ()
 */
HB_FUNC( QT_QCOREAPPLICATION_FLUSH )
{
   hbqt_par_QCoreApplication( 1 )->flush();
}

/*
 * bool hasPendingEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_HASPENDINGEVENTS )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->hasPendingEvents() );
}

/*
 * void installTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTALLTRANSLATOR )
{
   hbqt_par_QCoreApplication( 1 )->installTranslator( hbqt_par_QTranslator( 2 ) );
}

/*
 * QCoreApplication * instance ()
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTANCE )
{
   hb_retptr( ( QCoreApplication* ) hbqt_par_QCoreApplication( 1 )->instance() );
}

/*
 * QStringList libraryPaths ()
 */
HB_FUNC( QT_QCOREAPPLICATION_LIBRARYPATHS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QCoreApplication( 1 )->libraryPaths() ), release_QStringList ) );
}

/*
 * QString organizationDomain ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONDOMAIN )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->organizationDomain().toAscii().data() );
}

/*
 * QString organizationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONNAME )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->organizationName().toAscii().data() );
}

/*
 * void postEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT )
{
   hbqt_par_QCoreApplication( 1 )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) );
}

/*
 * void postEvent ( QObject * receiver, QEvent * event, int priority )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT_1 )
{
   hbqt_par_QCoreApplication( 1 )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ), hb_parni( 4 ) );
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags = QEventLoop::AllEvents )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS )
{
   hbqt_par_QCoreApplication( 1 )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) );
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags, int maxtime )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS_1 )
{
   hbqt_par_QCoreApplication( 1 )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void removeLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVELIBRARYPATH )
{
   hbqt_par_QCoreApplication( 1 )->removeLibraryPath( hbqt_par_QString( 2 ) );
}

/*
 * void removePostedEvents ( QObject * receiver )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS )
{
   hbqt_par_QCoreApplication( 1 )->removePostedEvents( hbqt_par_QObject( 2 ) );
}

/*
 * void removePostedEvents ( QObject * receiver, int eventType )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS_1 )
{
   hbqt_par_QCoreApplication( 1 )->removePostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
}

/*
 * void removeTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVETRANSLATOR )
{
   hbqt_par_QCoreApplication( 1 )->removeTranslator( hbqt_par_QTranslator( 2 ) );
}

/*
 * bool sendEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDEVENT )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->sendEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
}

/*
 * void sendPostedEvents ( QObject * receiver, int event_type )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS )
{
   hbqt_par_QCoreApplication( 1 )->sendPostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
}

/*
 * void sendPostedEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS_1 )
{
   hbqt_par_QCoreApplication( 1 )->sendPostedEvents();
}

/*
 * void setApplicationName ( const QString & application )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONNAME )
{
   hbqt_par_QCoreApplication( 1 )->setApplicationName( hbqt_par_QString( 2 ) );
}

/*
 * void setApplicationVersion ( const QString & version )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONVERSION )
{
   hbqt_par_QCoreApplication( 1 )->setApplicationVersion( hbqt_par_QString( 2 ) );
}

/*
 * void setAttribute ( Qt::ApplicationAttribute attribute, bool on = true )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETATTRIBUTE )
{
   hbqt_par_QCoreApplication( 1 )->setAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setLibraryPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETLIBRARYPATHS )
{
   hbqt_par_QCoreApplication( 1 )->setLibraryPaths( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setOrganizationDomain ( const QString & orgDomain )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONDOMAIN )
{
   hbqt_par_QCoreApplication( 1 )->setOrganizationDomain( hbqt_par_QString( 2 ) );
}

/*
 * void setOrganizationName ( const QString & orgName )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONNAME )
{
   hbqt_par_QCoreApplication( 1 )->setOrganizationName( hbqt_par_QString( 2 ) );
}

/*
 * bool startingUp ()
 */
HB_FUNC( QT_QCOREAPPLICATION_STARTINGUP )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->startingUp() );
}

/*
 * bool testAttribute ( Qt::ApplicationAttribute attribute )
 */
HB_FUNC( QT_QCOREAPPLICATION_TESTATTRIBUTE )
{
   hb_retl( hbqt_par_QCoreApplication( 1 )->testAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ) ) );
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation, Encoding encoding, int n )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( QCoreApplication::Encoding ) hb_parni( 5 ), hb_parni( 6 ) ).toAscii().data() );
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation = 0, Encoding encoding = CodecForTr )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE_1 )
{
   hb_retc( hbqt_par_QCoreApplication( 1 )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( HB_ISNUM( 5 ) ? ( QCoreApplication::Encoding ) hb_parni( 5 ) : ( QCoreApplication::Encoding ) QCoreApplication::CodecForTr ) ).toAscii().data() );
}

/*
 * void quit ()
 */
HB_FUNC( QT_QCOREAPPLICATION_QUIT )
{
   hbqt_par_QCoreApplication( 1 )->quit();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
