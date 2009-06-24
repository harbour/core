/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include <QtCore/QDateTime>


/*
 * QDateTime ()
 * QDateTime ( const QDate & date )
 * QDateTime ( const QDate & date, const QTime & time, Qt::TimeSpec spec = Qt::LocalTime )
 * QDateTime ( const QDateTime & other )
 * ~QDateTime ()
 */
HB_FUNC( QT_QDATETIME )
{
   hb_retptr( ( QDateTime* ) new QDateTime() );
}

/*
 * QDateTime addDays ( int ndays ) const
 */
HB_FUNC( QT_QDATETIME_ADDDAYS )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->addDays( hb_parni( 2 ) ) ) );
}

/*
 * QDateTime addMSecs ( qint64 msecs ) const
 */
HB_FUNC( QT_QDATETIME_ADDMSECS )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->addMSecs( hb_parni( 2 ) ) ) );
}

/*
 * QDateTime addMonths ( int nmonths ) const
 */
HB_FUNC( QT_QDATETIME_ADDMONTHS )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->addMonths( hb_parni( 2 ) ) ) );
}

/*
 * QDateTime addSecs ( int s ) const
 */
HB_FUNC( QT_QDATETIME_ADDSECS )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->addSecs( hb_parni( 2 ) ) ) );
}

/*
 * QDateTime addYears ( int nyears ) const
 */
HB_FUNC( QT_QDATETIME_ADDYEARS )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->addYears( hb_parni( 2 ) ) ) );
}

/*
 * QDate date () const
 */
HB_FUNC( QT_QDATETIME_DATE )
{
   hb_retptr( new QDate( hbqt_par_QDateTime( 1 )->date() ) );
}

/*
 * int daysTo ( const QDateTime & other ) const
 */
HB_FUNC( QT_QDATETIME_DAYSTO )
{
   hb_retni( hbqt_par_QDateTime( 1 )->daysTo( *hbqt_par_QDateTime( 2 ) ) );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QDATETIME_ISNULL )
{
   hb_retl( hbqt_par_QDateTime( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QDATETIME_ISVALID )
{
   hb_retl( hbqt_par_QDateTime( 1 )->isValid() );
}

/*
 * int secsTo ( const QDateTime & other ) const
 */
HB_FUNC( QT_QDATETIME_SECSTO )
{
   hb_retni( hbqt_par_QDateTime( 1 )->secsTo( *hbqt_par_QDateTime( 2 ) ) );
}

/*
 * void setDate ( const QDate & date )
 */
HB_FUNC( QT_QDATETIME_SETDATE )
{
   hbqt_par_QDateTime( 1 )->setDate( *hbqt_par_QDate( 2 ) );
}

/*
 * void setTime ( const QTime & time )
 */
HB_FUNC( QT_QDATETIME_SETTIME )
{
   hbqt_par_QDateTime( 1 )->setTime( *hbqt_par_QTime( 2 ) );
}

/*
 * void setTimeSpec ( Qt::TimeSpec spec )
 */
HB_FUNC( QT_QDATETIME_SETTIMESPEC )
{
   hbqt_par_QDateTime( 1 )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
}

/*
 * void setTime_t ( uint seconds )
 */
HB_FUNC( QT_QDATETIME_SETTIME_T )
{
   hbqt_par_QDateTime( 1 )->setTime_t( hb_parni( 2 ) );
}

/*
 * QTime time () const
 */
HB_FUNC( QT_QDATETIME_TIME )
{
   hb_retptr( new QTime( hbqt_par_QDateTime( 1 )->time() ) );
}

/*
 * Qt::TimeSpec timeSpec () const
 */
HB_FUNC( QT_QDATETIME_TIMESPEC )
{
   hb_retni( ( Qt::TimeSpec ) hbqt_par_QDateTime( 1 )->timeSpec() );
}

/*
 * QDateTime toLocalTime () const
 */
HB_FUNC( QT_QDATETIME_TOLOCALTIME )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->toLocalTime() ) );
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QDATETIME_TOSTRING )
{
   hb_retc( hbqt_par_QDateTime( 1 )->toString( hbqt_par_QString( 2 ) ).toLatin1().data() );
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QDATETIME_TOSTRING_1 )
{
   hb_retc( hbqt_par_QDateTime( 1 )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toLatin1().data() );
}

/*
 * QDateTime toTimeSpec ( Qt::TimeSpec specification ) const
 */
HB_FUNC( QT_QDATETIME_TOTIMESPEC )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->toTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ) ) );
}

/*
 * uint toTime_t () const
 */
HB_FUNC( QT_QDATETIME_TOTIME_T )
{
   hb_retni( hbqt_par_QDateTime( 1 )->toTime_t() );
}

/*
 * QDateTime toUTC () const
 */
HB_FUNC( QT_QDATETIME_TOUTC )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->toUTC() ) );
}

/*
 * QDateTime currentDateTime ()
 */
HB_FUNC( QT_QDATETIME_CURRENTDATETIME )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->currentDateTime() ) );
}

/*
 * QDateTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate )
 */
HB_FUNC( QT_QDATETIME_FROMSTRING )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ) );
}

/*
 * QDateTime fromString ( const QString & string, const QString & format )
 */
HB_FUNC( QT_QDATETIME_FROMSTRING_1 )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ) );
}

/*
 * QDateTime fromTime_t ( uint seconds )
 */
HB_FUNC( QT_QDATETIME_FROMTIME_T )
{
   hb_retptr( new QDateTime( hbqt_par_QDateTime( 1 )->fromTime_t( hb_parni( 2 ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

