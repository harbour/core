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

#include "hbqt.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QDateTime>


/*
 * QDateTime ()
 * QDateTime ( const QDate & date )
 * QDateTime ( const QDate & date, const QTime & time, Qt::TimeSpec spec = Qt::LocalTime )
 * QDateTime ( const QDateTime & other )
 * ~QDateTime ()
 */

typedef struct
{
   QDateTime * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDateTime;

QT_G_FUNC( hbqt_gcRelease_QDateTime )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QDateTime   /.\\", p->ph ) );
         delete ( ( QDateTime * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QDateTime   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDateTime    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDateTime    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDateTime( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QDateTime * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDateTime;
   p->type = HBQT_TYPE_QDateTime;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDateTime", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDateTime", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDATETIME )
{
   QDateTime * pObj = NULL;

   pObj =  new QDateTime() ;

   hb_retptrGC( hbqt_gcAllocate_QDateTime( ( void * ) pObj, true ) );
}

/*
 * QDateTime addDays ( int ndays ) const
 */
HB_FUNC( QT_QDATETIME_ADDDAYS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addDays( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ADDDAYS FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addDays( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime addMSecs ( qint64 msecs ) const
 */
HB_FUNC( QT_QDATETIME_ADDMSECS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMSecs( hb_parnint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ADDMSECS FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMSecs( hb_parnint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime addMonths ( int nmonths ) const
 */
HB_FUNC( QT_QDATETIME_ADDMONTHS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMonths( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ADDMONTHS FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMonths( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime addSecs ( int s ) const
 */
HB_FUNC( QT_QDATETIME_ADDSECS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ADDSECS FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime addYears ( int nyears ) const
 */
HB_FUNC( QT_QDATETIME_ADDYEARS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addYears( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ADDYEARS FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addYears( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate date () const
 */
HB_FUNC( QT_QDATETIME_DATE )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_DATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) ); p is NULL" ) );
   }
}

/*
 * int daysTo ( const QDateTime & other ) const
 */
HB_FUNC( QT_QDATETIME_DAYSTO )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->daysTo( *hbqt_par_QDateTime( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_DAYSTO FP=hb_retni( ( p )->daysTo( *hbqt_par_QDateTime( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QDATETIME_ISNULL )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QDATETIME_ISVALID )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int secsTo ( const QDateTime & other ) const
 */
HB_FUNC( QT_QDATETIME_SECSTO )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->secsTo( *hbqt_par_QDateTime( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_SECSTO FP=hb_retni( ( p )->secsTo( *hbqt_par_QDateTime( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setDate ( const QDate & date )
 */
HB_FUNC( QT_QDATETIME_SETDATE )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_SETDATE FP=( p )->setDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTime ( const QTime & time )
 */
HB_FUNC( QT_QDATETIME_SETTIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTime( *hbqt_par_QTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_SETTIME FP=( p )->setTime( *hbqt_par_QTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTimeSpec ( Qt::TimeSpec spec )
 */
HB_FUNC( QT_QDATETIME_SETTIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_SETTIMESPEC FP=( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTime_t ( uint seconds )
 */
HB_FUNC( QT_QDATETIME_SETTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTime_t( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_SETTIME_T FP=( p )->setTime_t( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QTime time () const
 */
HB_FUNC( QT_QDATETIME_TIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::TimeSpec timeSpec () const
 */
HB_FUNC( QT_QDATETIME_TIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TIMESPEC FP=hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() ); p is NULL" ) );
   }
}

/*
 * QDateTime toLocalTime () const
 */
HB_FUNC( QT_QDATETIME_TOLOCALTIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toLocalTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOLOCALTIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toLocalTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QDATETIME_TOSTRING )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOSTRING FP=hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QDATETIME_TOSTRING_1 )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOSTRING_1 FP=hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDateTime toTimeSpec ( Qt::TimeSpec specification ) const
 */
HB_FUNC( QT_QDATETIME_TOTIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOTIMESPEC FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * uint toTime_t () const
 */
HB_FUNC( QT_QDATETIME_TOTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->toTime_t() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOTIME_T FP=hb_retni( ( p )->toTime_t() ); p is NULL" ) );
   }
}

/*
 * QDateTime toUTC () const
 */
HB_FUNC( QT_QDATETIME_TOUTC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toUTC() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_TOUTC FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toUTC() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime currentDateTime ()
 */
HB_FUNC( QT_QDATETIME_CURRENTDATETIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->currentDateTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_CURRENTDATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->currentDateTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate )
 */
HB_FUNC( QT_QDATETIME_FROMSTRING )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_FROMSTRING FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime fromString ( const QString & string, const QString & format )
 */
HB_FUNC( QT_QDATETIME_FROMSTRING_1 )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_FROMSTRING_1 FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime fromTime_t ( uint seconds )
 */
HB_FUNC( QT_QDATETIME_FROMTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromTime_t( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIME_FROMTIME_T FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromTime_t( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
