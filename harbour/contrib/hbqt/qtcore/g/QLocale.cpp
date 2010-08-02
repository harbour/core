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
 *  enum Country { AnyCountry, Afghanistan, Albania, Algeria, ..., Zimbabwe }
 *  enum FormatType { LongFormat, ShortFormat, NarrowFormat }
 *  enum Language { C, Abkhazian, Afan, Afar, ..., Chewa }
 *  enum MeasurementSystem { MetricSystem, ImperialSystem }
 *  enum NumberOption { OmitGroupSeparator, RejectGroupSeparator }
 *  flags NumberOptions
 */

#include <QtCore/QPointer>

#include <QtCore/QLocale>
#include <QtCore/QDate>

/* QLocale ()
 * QLocale ( const QString & name )
 * QLocale ( Language language, Country country = AnyCountry )
 * QLocale ( const QLocale & other )
 */

typedef struct
{
   QLocale * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QLocale;

QT_G_FUNC( hbqt_gcRelease_QLocale )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QLocale   /.\\", p->ph ) );
         delete ( ( QLocale * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QLocale   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLocale    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLocale    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLocale( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QLocale * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLocale;
   p->type = HBQT_TYPE_QLocale;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLocale", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLocale", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLOCALE )
{
   QLocale * pObj = NULL;

   pObj = new QLocale() ;

   hb_retptrGC( hbqt_gcAllocate_QLocale( ( void * ) pObj, true ) );
}

/*
 * QString amText () const
 */
HB_FUNC( QT_QLOCALE_AMTEXT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->amText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_AMTEXT FP=hb_retc( ( p )->amText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Country country () const
 */
HB_FUNC( QT_QLOCALE_COUNTRY )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retni( ( QLocale::Country ) ( p )->country() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_COUNTRY FP=hb_retni( ( QLocale::Country ) ( p )->country() ); p is NULL" ) );
   }
}

/*
 * QString dateFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DATEFORMAT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->dateFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_DATEFORMAT FP=hb_retc( ( p )->dateFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString dateTimeFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DATETIMEFORMAT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->dateTimeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_DATETIMEFORMAT FP=hb_retc( ( p )->dateTimeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString dayName ( int day, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DAYNAME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->dayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_DAYNAME FP=hb_retc( ( p )->dayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QChar decimalPoint () const
 */
HB_FUNC( QT_QLOCALE_DECIMALPOINT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->decimalPoint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_DECIMALPOINT FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->decimalPoint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QChar exponential () const
 */
HB_FUNC( QT_QLOCALE_EXPONENTIAL )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->exponential() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_EXPONENTIAL FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->exponential() ), true ) ); p is NULL" ) );
   }
}

/*
 * QChar groupSeparator () const
 */
HB_FUNC( QT_QLOCALE_GROUPSEPARATOR )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->groupSeparator() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_GROUPSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->groupSeparator() ), true ) ); p is NULL" ) );
   }
}

/*
 * Language language () const
 */
HB_FUNC( QT_QLOCALE_LANGUAGE )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retni( ( QLocale::Language ) ( p )->language() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_LANGUAGE FP=hb_retni( ( QLocale::Language ) ( p )->language() ); p is NULL" ) );
   }
}

/*
 * MeasurementSystem measurementSystem () const
 */
HB_FUNC( QT_QLOCALE_MEASUREMENTSYSTEM )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retni( ( QLocale::MeasurementSystem ) ( p )->measurementSystem() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_MEASUREMENTSYSTEM FP=hb_retni( ( QLocale::MeasurementSystem ) ( p )->measurementSystem() ); p is NULL" ) );
   }
}

/*
 * QString monthName ( int month, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_MONTHNAME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->monthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_MONTHNAME FP=hb_retc( ( p )->monthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString name () const
 */
HB_FUNC( QT_QLOCALE_NAME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->name().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_NAME FP=hb_retc( ( p )->name().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QChar negativeSign () const
 */
HB_FUNC( QT_QLOCALE_NEGATIVESIGN )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->negativeSign() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_NEGATIVESIGN FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->negativeSign() ), true ) ); p is NULL" ) );
   }
}

/*
 * NumberOptions numberOptions () const
 */
HB_FUNC( QT_QLOCALE_NUMBEROPTIONS )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retni( ( QLocale::NumberOptions ) ( p )->numberOptions() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_NUMBEROPTIONS FP=hb_retni( ( QLocale::NumberOptions ) ( p )->numberOptions() ); p is NULL" ) );
   }
}

/*
 * QChar percent () const
 */
HB_FUNC( QT_QLOCALE_PERCENT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->percent() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_PERCENT FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->percent() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString pmText () const
 */
HB_FUNC( QT_QLOCALE_PMTEXT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->pmText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_PMTEXT FP=hb_retc( ( p )->pmText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QChar positiveSign () const
 */
HB_FUNC( QT_QLOCALE_POSITIVESIGN )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->positiveSign() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_POSITIVESIGN FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->positiveSign() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setNumberOptions ( NumberOptions options )
 */
HB_FUNC( QT_QLOCALE_SETNUMBEROPTIONS )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      ( p )->setNumberOptions( ( QLocale::NumberOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_SETNUMBEROPTIONS FP=( p )->setNumberOptions( ( QLocale::NumberOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString standaloneDayName ( int day, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_STANDALONEDAYNAME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->standaloneDayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_STANDALONEDAYNAME FP=hb_retc( ( p )->standaloneDayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString standaloneMonthName ( int month, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_STANDALONEMONTHNAME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->standaloneMonthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_STANDALONEMONTHNAME FP=hb_retc( ( p )->standaloneMonthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString timeFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TIMEFORMAT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->timeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TIMEFORMAT FP=hb_retc( ( p )->timeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDate toDate ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TODATE )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TODATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate toDate ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TODATE_1 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TODATE_1 FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime toDateTime ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TODATETIME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TODATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime toDateTime ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TODATETIME_1 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TODATETIME_1 FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * double toDouble ( const QString & s, bool * ok = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TODOUBLE )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retnd( ( p )->toDouble( hbqt_par_QString( 2 ), &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TODOUBLE FP=hb_retnd( ( p )->toDouble( hbqt_par_QString( 2 ), &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * float toFloat ( const QString & s, bool * ok = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOFLOAT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retnd( ( p )->toFloat( hbqt_par_QString( 2 ), &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOFLOAT FP=hb_retnd( ( p )->toFloat( hbqt_par_QString( 2 ), &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * int toInt ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOINT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOINT FP=hb_retni( ( p )->toInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * qlonglong toLongLong ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOLONGLONG )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retnint( ( p )->toLongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOLONGLONG FP=hb_retnint( ( p )->toLongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * short toShort ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOSHORT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSHORT FP=hb_retni( ( p )->toShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * QString toString ( qlonglong i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( ( qlonglong ) hb_parnint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING FP=hb_retc( ( p )->toString( ( qlonglong ) hb_parnint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QDate & date, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_1 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QDate( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_1 FP=hb_retc( ( p )->toString( *hbqt_par_QDate( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QDate & date, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_2 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QDate( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_2 FP=hb_retc( ( p )->toString( *hbqt_par_QDate( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QTime & time, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_3 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_3 FP=hb_retc( ( p )->toString( *hbqt_par_QTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QTime & time, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_4 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_4 FP=hb_retc( ( p )->toString( *hbqt_par_QTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QDateTime & dateTime, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_5 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QDateTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_5 FP=hb_retc( ( p )->toString( *hbqt_par_QDateTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( const QDateTime & dateTime, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_6 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( *hbqt_par_QDateTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_6 FP=hb_retc( ( p )->toString( *hbqt_par_QDateTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( qulonglong i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_7 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( ( qulonglong ) hb_parnint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_7 FP=hb_retc( ( p )->toString( ( qulonglong ) hb_parnint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( double i, char f = 'g', int prec = 6 ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_8 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_8 FP=hb_retc( ( p )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( short i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_9 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_9 FP=hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( ushort i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_10 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_10 FP=hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( int i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_11 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_11 FP=hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( uint i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_12 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_12 FP=hb_retc( ( p )->toString( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( float i, char f = 'g', int prec = 6 ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_13 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOSTRING_13 FP=hb_retc( ( p )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTime toTime ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOTIME )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOTIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime toTime ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOTIME_1 )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOTIME_1 FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * uint toUInt ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOUINT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toUInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOUINT FP=hb_retni( ( p )->toUInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * qlonglong toULongLong ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOULONGLONG )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retnint( ( p )->toULongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOULONGLONG FP=hb_retnint( ( p )->toULongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * ushort toUShort ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOUSHORT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toUShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_TOUSHORT FP=hb_retni( ( p )->toUShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * QChar zeroDigit () const
 */
HB_FUNC( QT_QLOCALE_ZERODIGIT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->zeroDigit() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_ZERODIGIT FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->zeroDigit() ), true ) ); p is NULL" ) );
   }
}

/*
 * QLocale c ()
 */
HB_FUNC( QT_QLOCALE_C )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->c() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_C FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->c() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString countryToString ( Country country )
 */
HB_FUNC( QT_QLOCALE_COUNTRYTOSTRING )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->countryToString( ( QLocale::Country ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_COUNTRYTOSTRING FP=hb_retc( ( p )->countryToString( ( QLocale::Country ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString languageToString ( Language language )
 */
HB_FUNC( QT_QLOCALE_LANGUAGETOSTRING )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retc( ( p )->languageToString( ( QLocale::Language ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_LANGUAGETOSTRING FP=hb_retc( ( p )->languageToString( ( QLocale::Language ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setDefault ( const QLocale & locale )
 */
HB_FUNC( QT_QLOCALE_SETDEFAULT )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      ( p )->setDefault( *hbqt_par_QLocale( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_SETDEFAULT FP=( p )->setDefault( *hbqt_par_QLocale( 2 ) ); p is NULL" ) );
   }
}

/*
 * QLocale system ()
 */
HB_FUNC( QT_QLOCALE_SYSTEM )
{
   QLocale * p = hbqt_par_QLocale( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->system() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLOCALE_SYSTEM FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->system() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
