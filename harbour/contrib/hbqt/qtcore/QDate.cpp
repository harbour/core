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
 *  enum MonthNameType { DateFormat, StandaloneFormat }
 */

#include <QtCore/QPointer>

#include <QtCore/QDate>


/* QDate ()
 * QDate ( int y, int m, int d )
 */

typedef struct
{
   QDate * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDate;

QT_G_FUNC( hbqt_gcRelease_QDate )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QDate   /.\\", p->ph ) );
         delete ( ( QDate * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QDate   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDate    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDate    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDate( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QDate * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDate;
   p->type = HBQT_TYPE_QDate;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDate", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDate", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDATE )
{
   QDate * pObj = NULL;

   pObj = new QDate() ;

   hb_retptrGC( hbqt_gcAllocate_QDate( ( void * ) pObj, true ) );
}

/*
 * QDate addDays ( int ndays ) const
 */
HB_FUNC( QT_QDATE_ADDDAYS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addDays( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_ADDDAYS FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addDays( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate addMonths ( int nmonths ) const
 */
HB_FUNC( QT_QDATE_ADDMONTHS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addMonths( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_ADDMONTHS FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addMonths( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate addYears ( int nyears ) const
 */
HB_FUNC( QT_QDATE_ADDYEARS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addYears( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_ADDYEARS FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addYears( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int day () const
 */
HB_FUNC( QT_QDATE_DAY )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->day() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAY FP=hb_retni( ( p )->day() ); p is NULL" ) );
   }
}

/*
 * int dayOfWeek () const
 */
HB_FUNC( QT_QDATE_DAYOFWEEK )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->dayOfWeek() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAYOFWEEK FP=hb_retni( ( p )->dayOfWeek() ); p is NULL" ) );
   }
}

/*
 * int dayOfYear () const
 */
HB_FUNC( QT_QDATE_DAYOFYEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->dayOfYear() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAYOFYEAR FP=hb_retni( ( p )->dayOfYear() ); p is NULL" ) );
   }
}

/*
 * int daysInMonth () const
 */
HB_FUNC( QT_QDATE_DAYSINMONTH )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysInMonth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAYSINMONTH FP=hb_retni( ( p )->daysInMonth() ); p is NULL" ) );
   }
}

/*
 * int daysInYear () const
 */
HB_FUNC( QT_QDATE_DAYSINYEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysInYear() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAYSINYEAR FP=hb_retni( ( p )->daysInYear() ); p is NULL" ) );
   }
}

/*
 * int daysTo ( const QDate & d ) const
 */
HB_FUNC( QT_QDATE_DAYSTO )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysTo( *hbqt_par_QDate( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_DAYSTO FP=hb_retni( ( p )->daysTo( *hbqt_par_QDate( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void getDate ( int * year, int * month, int * day )
 */
HB_FUNC( QT_QDATE_GETDATE )
{
   QDate * p = hbqt_par_QDate( 1 );
   int iYear = 0;
   int iMonth = 0;
   int iDay = 0;

   if( p )
      ( p )->getDate( &iYear, &iMonth, &iDay );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_GETDATE FP=( p )->getDate( &iYear, &iMonth, &iDay ); p is NULL" ) );
   }

   hb_storni( iYear, 2 );
   hb_storni( iMonth, 3 );
   hb_storni( iDay, 4 );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QDATE_ISNULL )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QDATE_ISVALID )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int month () const
 */
HB_FUNC( QT_QDATE_MONTH )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->month() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_MONTH FP=hb_retni( ( p )->month() ); p is NULL" ) );
   }
}

/*
 * bool setDate ( int year, int month, int day )
 */
HB_FUNC( QT_QDATE_SETDATE )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->setDate( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_SETDATE FP=hb_retl( ( p )->setDate( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ); p is NULL" ) );
   }
}

/*
 * int toJulianDay () const
 */
HB_FUNC( QT_QDATE_TOJULIANDAY )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->toJulianDay() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_TOJULIANDAY FP=hb_retni( ( p )->toJulianDay() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QDATE_TOSTRING )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_TOSTRING FP=hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QDATE_TOSTRING_1 )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_TOSTRING_1 FP=hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int weekNumber ( int * yearNumber = 0 ) const
 */
HB_FUNC( QT_QDATE_WEEKNUMBER )
{
   QDate * p = hbqt_par_QDate( 1 );
   int iYearNumber = 0;

   if( p )
      hb_retni( ( p )->weekNumber( &iYearNumber ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_WEEKNUMBER FP=hb_retni( ( p )->weekNumber( &iYearNumber ) ); p is NULL" ) );
   }

   hb_storni( iYearNumber, 2 );
}

/*
 * int year () const
 */
HB_FUNC( QT_QDATE_YEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->year() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATE_YEAR FP=hb_retni( ( p )->year() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
