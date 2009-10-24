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

#include <QtCore/QPointer>

#include <QtCore/QTime>


/* QTime ()
 * QTime ( int h, int m, int s = 0, int ms = 0 )
 */

QT_G_FUNC( release_QTime )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QTime" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QTime * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QTIME )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   void * pObj = NULL;

   pObj = new QTime() ;

   p->ph = pObj;
   p->func = release_QTime;

   hb_retptrGC( p );
}
/*
 * QTime addMSecs ( int ms ) const
 */
HB_FUNC( QT_QTIME_ADDMSECS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QTime( 1 )->addMSecs( hb_parni( 2 ) ) ), release_QTime ) );
}

/*
 * QTime addSecs ( int s ) const
 */
HB_FUNC( QT_QTIME_ADDSECS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QTime( 1 )->addSecs( hb_parni( 2 ) ) ), release_QTime ) );
}

/*
 * int elapsed () const
 */
HB_FUNC( QT_QTIME_ELAPSED )
{
   hb_retni( hbqt_par_QTime( 1 )->elapsed() );
}

/*
 * int hour () const
 */
HB_FUNC( QT_QTIME_HOUR )
{
   hb_retni( hbqt_par_QTime( 1 )->hour() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QTIME_ISNULL )
{
   hb_retl( hbqt_par_QTime( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTIME_ISVALID )
{
   hb_retl( hbqt_par_QTime( 1 )->isValid() );
}

/*
 * int minute () const
 */
HB_FUNC( QT_QTIME_MINUTE )
{
   hb_retni( hbqt_par_QTime( 1 )->minute() );
}

/*
 * int msec () const
 */
HB_FUNC( QT_QTIME_MSEC )
{
   hb_retni( hbqt_par_QTime( 1 )->msec() );
}

/*
 * int msecsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_MSECSTO )
{
   hb_retni( hbqt_par_QTime( 1 )->msecsTo( *hbqt_par_QTime( 2 ) ) );
}

/*
 * int restart ()
 */
HB_FUNC( QT_QTIME_RESTART )
{
   hb_retni( hbqt_par_QTime( 1 )->restart() );
}

/*
 * int second () const
 */
HB_FUNC( QT_QTIME_SECOND )
{
   hb_retni( hbqt_par_QTime( 1 )->second() );
}

/*
 * int secsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_SECSTO )
{
   hb_retni( hbqt_par_QTime( 1 )->secsTo( *hbqt_par_QTime( 2 ) ) );
}

/*
 * bool setHMS ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_SETHMS )
{
   hb_retl( hbqt_par_QTime( 1 )->setHMS( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
}

/*
 * void start ()
 */
HB_FUNC( QT_QTIME_START )
{
   hbqt_par_QTime( 1 )->start();
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QTIME_TOSTRING )
{
   hb_retc( hbqt_par_QTime( 1 )->toString( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QTIME_TOSTRING_1 )
{
   hb_retc( hbqt_par_QTime( 1 )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() );
}

/*
 * QTime currentTime ()
 */
HB_FUNC( QT_QTIME_CURRENTTIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QTime( 1 )->currentTime() ), release_QTime ) );
}

/*
 * QTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate )
 */
HB_FUNC( QT_QTIME_FROMSTRING )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QTime( 1 )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), release_QTime ) );
}

/*
 * QTime fromString ( const QString & string, const QString & format )
 */
HB_FUNC( QT_QTIME_FROMSTRING_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QTime( 1 )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), release_QTime ) );
}

/*
 * bool isValid ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_ISVALID_1 )
{
   hb_retl( hbqt_par_QTime( 1 )->isValid( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
