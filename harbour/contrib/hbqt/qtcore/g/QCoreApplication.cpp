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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCoreApplication;

HBQT_GC_FUNC( hbqt_gcRelease_QCoreApplication )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCoreApplication( void * pObj, bool bNew )
{
   HBQT_GC_T_QCoreApplication * p = ( HBQT_GC_T_QCoreApplication * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCoreApplication ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->notify( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
   }
}

/*
 * void addLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_ADDLIBRARYPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->addLibraryPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString applicationDirPath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONDIRPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->applicationDirPath().toUtf8().data() );
   }
}

/*
 * QString applicationFilePath ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONFILEPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->applicationFilePath().toUtf8().data() );
   }
}

/*
 * QString applicationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->applicationName().toUtf8().data() );
   }
}

/*
 * qint64 applicationPid ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONPID )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retnint( ( p )->applicationPid() );
   }
}

/*
 * QString applicationVersion ()
 */
HB_FUNC( QT_QCOREAPPLICATION_APPLICATIONVERSION )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->applicationVersion().toUtf8().data() );
   }
}

/*
 * QStringList arguments ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ARGUMENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->arguments() ), true ) );
   }
}

/*
 * bool closingDown ()
 */
HB_FUNC( QT_QCOREAPPLICATION_CLOSINGDOWN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->closingDown() );
   }
}

/*
 * int exec ()
 */
HB_FUNC( QT_QCOREAPPLICATION_EXEC )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->exec() );
   }
}

/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QCOREAPPLICATION_EXIT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->exit( hb_parni( 2 ) );
   }
}

/*
 * void flush ()
 */
HB_FUNC( QT_QCOREAPPLICATION_FLUSH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->flush();
   }
}

/*
 * bool hasPendingEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_HASPENDINGEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->hasPendingEvents() );
   }
}

/*
 * void installTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTALLTRANSLATOR )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->installTranslator( hbqt_par_QTranslator( 2 ) );
   }
}

/*
 * QCoreApplication * instance ()
 */
HB_FUNC( QT_QCOREAPPLICATION_INSTANCE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QCoreApplication( ( p )->instance(), false ) );
   }
}

/*
 * QStringList libraryPaths ()
 */
HB_FUNC( QT_QCOREAPPLICATION_LIBRARYPATHS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->libraryPaths() ), true ) );
   }
}

/*
 * QString organizationDomain ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONDOMAIN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->organizationDomain().toUtf8().data() );
   }
}

/*
 * QString organizationName ()
 */
HB_FUNC( QT_QCOREAPPLICATION_ORGANIZATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->organizationName().toUtf8().data() );
   }
}

/*
 * void postEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) );
   }
}

/*
 * void postEvent ( QObject * receiver, QEvent * event, int priority )
 */
HB_FUNC( QT_QCOREAPPLICATION_POSTEVENT_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->postEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ), hb_parni( 4 ) );
   }
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags = QEventLoop::AllEvents )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) );
   }
}

/*
 * void processEvents ( QEventLoop::ProcessEventsFlags flags, int maxtime )
 */
HB_FUNC( QT_QCOREAPPLICATION_PROCESSEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void removeLibraryPath ( const QString & path )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVELIBRARYPATH )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeLibraryPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void removePostedEvents ( QObject * receiver )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->removePostedEvents( hbqt_par_QObject( 2 ) );
   }
}

/*
 * void removePostedEvents ( QObject * receiver, int eventType )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVEPOSTEDEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->removePostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void removeTranslator ( QTranslator * translationFile )
 */
HB_FUNC( QT_QCOREAPPLICATION_REMOVETRANSLATOR )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->removeTranslator( hbqt_par_QTranslator( 2 ) );
   }
}

/*
 * bool sendEvent ( QObject * receiver, QEvent * event )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDEVENT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->sendEvent( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
   }
}

/*
 * void sendPostedEvents ( QObject * receiver, int event_type )
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->sendPostedEvents( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void sendPostedEvents ()
 */
HB_FUNC( QT_QCOREAPPLICATION_SENDPOSTEDEVENTS_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->sendPostedEvents();
   }
}

/*
 * void setApplicationName ( const QString & application )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setApplicationName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setApplicationVersion ( const QString & version )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETAPPLICATIONVERSION )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setApplicationVersion( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setAttribute ( Qt::ApplicationAttribute attribute, bool on = true )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETATTRIBUTE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->setAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setLibraryPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETLIBRARYPATHS )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->setLibraryPaths( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setOrganizationDomain ( const QString & orgDomain )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONDOMAIN )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setOrganizationDomain( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setOrganizationName ( const QString & orgName )
 */
HB_FUNC( QT_QCOREAPPLICATION_SETORGANIZATIONNAME )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setOrganizationName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * bool startingUp ()
 */
HB_FUNC( QT_QCOREAPPLICATION_STARTINGUP )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->startingUp() );
   }
}

/*
 * bool testAttribute ( Qt::ApplicationAttribute attribute )
 */
HB_FUNC( QT_QCOREAPPLICATION_TESTATTRIBUTE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->testAttribute( ( Qt::ApplicationAttribute ) hb_parni( 2 ) ) );
   }
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation, Encoding encoding, int n )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( QCoreApplication::Encoding ) hb_parni( 5 ), hb_parni( 6 ) ).toUtf8().data() );
   }
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation = 0, Encoding encoding = CodecForTr )
 */
HB_FUNC( QT_QCOREAPPLICATION_TRANSLATE_1 )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( HB_ISNUM( 5 ) ? ( QCoreApplication::Encoding ) hb_parni( 5 ) : ( QCoreApplication::Encoding ) QCoreApplication::CodecForTr ) ).toUtf8().data() );
   }
}

/*
 * void quit ()
 */
HB_FUNC( QT_QCOREAPPLICATION_QUIT )
{
   QCoreApplication * p = hbqt_par_QCoreApplication( 1 );
   if( p )
   {
      ( p )->quit();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
