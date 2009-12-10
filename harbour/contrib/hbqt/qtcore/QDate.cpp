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
 *  enum MonthNameType { DateFormat, StandaloneFormat }
 */

#include <QtCore/QPointer>

#include <QtCore/QDate>


/* QDate ()
 * QDate ( int y, int m, int d )
 */

QT_G_FUNC( release_QDate )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QDate                        p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QDate                       ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QDate * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QDate                       Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QDate                       Object Allready deleted!" ) );
   }
}

void * gcAllocate_QDate( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QDate;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QDate                       %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QDATE )
{
   void * pObj = NULL;

   pObj = new QDate() ;

   hb_retptrGC( gcAllocate_QDate( pObj ) );
}
/*
 * QDate addDays ( int ndays ) const
 */
HB_FUNC( QT_QDATE_ADDDAYS )
{
   hb_retptrGC( gcAllocate_QDate( new QDate( hbqt_par_QDate( 1 )->addDays( hb_parni( 2 ) ) ) ) );
}

/*
 * QDate addMonths ( int nmonths ) const
 */
HB_FUNC( QT_QDATE_ADDMONTHS )
{
   hb_retptrGC( gcAllocate_QDate( new QDate( hbqt_par_QDate( 1 )->addMonths( hb_parni( 2 ) ) ) ) );
}

/*
 * QDate addYears ( int nyears ) const
 */
HB_FUNC( QT_QDATE_ADDYEARS )
{
   hb_retptrGC( gcAllocate_QDate( new QDate( hbqt_par_QDate( 1 )->addYears( hb_parni( 2 ) ) ) ) );
}

/*
 * int day () const
 */
HB_FUNC( QT_QDATE_DAY )
{
   hb_retni( hbqt_par_QDate( 1 )->day() );
}

/*
 * int dayOfWeek () const
 */
HB_FUNC( QT_QDATE_DAYOFWEEK )
{
   hb_retni( hbqt_par_QDate( 1 )->dayOfWeek() );
}

/*
 * int dayOfYear () const
 */
HB_FUNC( QT_QDATE_DAYOFYEAR )
{
   hb_retni( hbqt_par_QDate( 1 )->dayOfYear() );
}

/*
 * int daysInMonth () const
 */
HB_FUNC( QT_QDATE_DAYSINMONTH )
{
   hb_retni( hbqt_par_QDate( 1 )->daysInMonth() );
}

/*
 * int daysInYear () const
 */
HB_FUNC( QT_QDATE_DAYSINYEAR )
{
   hb_retni( hbqt_par_QDate( 1 )->daysInYear() );
}

/*
 * int daysTo ( const QDate & d ) const
 */
HB_FUNC( QT_QDATE_DAYSTO )
{
   hb_retni( hbqt_par_QDate( 1 )->daysTo( *hbqt_par_QDate( 2 ) ) );
}

/*
 * void getDate ( int * year, int * month, int * day )
 */
HB_FUNC( QT_QDATE_GETDATE )
{
   int iYear = 0;
   int iMonth = 0;
   int iDay = 0;

   hbqt_par_QDate( 1 )->getDate( &iYear, &iMonth, &iDay );

   hb_storni( iYear, 2 );
   hb_storni( iMonth, 3 );
   hb_storni( iDay, 4 );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QDATE_ISNULL )
{
   hb_retl( hbqt_par_QDate( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QDATE_ISVALID )
{
   hb_retl( hbqt_par_QDate( 1 )->isValid() );
}

/*
 * int month () const
 */
HB_FUNC( QT_QDATE_MONTH )
{
   hb_retni( hbqt_par_QDate( 1 )->month() );
}

/*
 * bool setDate ( int year, int month, int day )
 */
HB_FUNC( QT_QDATE_SETDATE )
{
   hb_retl( hbqt_par_QDate( 1 )->setDate( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * int toJulianDay () const
 */
HB_FUNC( QT_QDATE_TOJULIANDAY )
{
   hb_retni( hbqt_par_QDate( 1 )->toJulianDay() );
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QDATE_TOSTRING )
{
   hb_retc( hbqt_par_QDate( 1 )->toString( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QDATE_TOSTRING_1 )
{
   hb_retc( hbqt_par_QDate( 1 )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() );
}

/*
 * int weekNumber ( int * yearNumber = 0 ) const
 */
HB_FUNC( QT_QDATE_WEEKNUMBER )
{
   int iYearNumber = 0;

   hb_retni( hbqt_par_QDate( 1 )->weekNumber( &iYearNumber ) );

   hb_storni( iYearNumber, 2 );
}

/*
 * int year () const
 */
HB_FUNC( QT_QDATE_YEAR )
{
   hb_retni( hbqt_par_QDate( 1 )->year() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
