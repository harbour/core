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
 *  enum Country { AnyCountry, Afghanistan, Albania, Algeria, ..., Zimbabwe }
 *  enum FormatType { LongFormat, ShortFormat, NarrowFormat }
 *  enum Language { C, Abkhazian, Afan, Afar, ..., Chewa }
 *  enum MeasurementSystem { MetricSystem, ImperialSystem }
 *  enum NumberOption { OmitGroupSeparator, RejectGroupSeparator }
 *  flags NumberOptions
 */

/*
 *  Constructed[ 48/56 [ 85.71% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<Country> countriesForLanguage ( Language language )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // QChar decimalPoint () const
 *  // QChar exponential () const
 *  // QChar groupSeparator () const
 *  // QChar negativeSign () const
 *  // QChar percent () const
 *  // QChar positiveSign () const
 *  // QChar zeroDigit () const
 */

#include <QtCore/QPointer>

#include <QtCore/QLocale>
#include <QtCore/QDate>

/* QLocale ()
 * QLocale ( const QString & name )
 * QLocale ( Language language, Country country = AnyCountry )
 * QLocale ( const QLocale & other )
 */

QT_G_FUNC( release_QLocale )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QLocale                      p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QLocale                     ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QLocale * ) p->ph )->~QLocale();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QLocale                     Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QLocale                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QLocale                     Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QLocale" );
      #endif
   }
}

void * gcAllocate_QLocale( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QLocale;
   #if defined(__debug__)
      just_debug( "          new_QLocale                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QLOCALE )
{
   void * pObj = NULL;

   pObj = new QLocale() ;

   hb_retptrGC( gcAllocate_QLocale( pObj ) );
}
/*
 * QString amText () const
 */
HB_FUNC( QT_QLOCALE_AMTEXT )
{
   hb_retc( hbqt_par_QLocale( 1 )->amText().toAscii().data() );
}

/*
 * Country country () const
 */
HB_FUNC( QT_QLOCALE_COUNTRY )
{
   hb_retni( ( QLocale::Country ) hbqt_par_QLocale( 1 )->country() );
}

/*
 * QString dateFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DATEFORMAT )
{
   hb_retc( hbqt_par_QLocale( 1 )->dateFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString dateTimeFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DATETIMEFORMAT )
{
   hb_retc( hbqt_par_QLocale( 1 )->dateTimeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString dayName ( int day, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_DAYNAME )
{
   hb_retc( hbqt_par_QLocale( 1 )->dayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * Language language () const
 */
HB_FUNC( QT_QLOCALE_LANGUAGE )
{
   hb_retni( ( QLocale::Language ) hbqt_par_QLocale( 1 )->language() );
}

/*
 * MeasurementSystem measurementSystem () const
 */
HB_FUNC( QT_QLOCALE_MEASUREMENTSYSTEM )
{
   hb_retni( ( QLocale::MeasurementSystem ) hbqt_par_QLocale( 1 )->measurementSystem() );
}

/*
 * QString monthName ( int month, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_MONTHNAME )
{
   hb_retc( hbqt_par_QLocale( 1 )->monthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString name () const
 */
HB_FUNC( QT_QLOCALE_NAME )
{
   hb_retc( hbqt_par_QLocale( 1 )->name().toAscii().data() );
}

/*
 * NumberOptions numberOptions () const
 */
HB_FUNC( QT_QLOCALE_NUMBEROPTIONS )
{
   hb_retni( ( QLocale::NumberOptions ) hbqt_par_QLocale( 1 )->numberOptions() );
}

/*
 * QString pmText () const
 */
HB_FUNC( QT_QLOCALE_PMTEXT )
{
   hb_retc( hbqt_par_QLocale( 1 )->pmText().toAscii().data() );
}

/*
 * void setNumberOptions ( NumberOptions options )
 */
HB_FUNC( QT_QLOCALE_SETNUMBEROPTIONS )
{
   hbqt_par_QLocale( 1 )->setNumberOptions( ( QLocale::NumberOptions ) hb_parni( 2 ) );
}

/*
 * QString standaloneDayName ( int day, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_STANDALONEDAYNAME )
{
   hb_retc( hbqt_par_QLocale( 1 )->standaloneDayName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString standaloneMonthName ( int month, FormatType type = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_STANDALONEMONTHNAME )
{
   hb_retc( hbqt_par_QLocale( 1 )->standaloneMonthName( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString timeFormat ( FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TIMEFORMAT )
{
   hb_retc( hbqt_par_QLocale( 1 )->timeFormat( ( HB_ISNUM( 2 ) ? ( QLocale::FormatType ) hb_parni( 2 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QDate toDate ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TODATE )
{
   hb_retptrGC( gcAllocate_QDate( new QDate( hbqt_par_QLocale( 1 )->toDate( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ) ) );
}

/*
 * QDate toDate ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TODATE_1 )
{
   hb_retptrGC( gcAllocate_QDate( new QDate( hbqt_par_QLocale( 1 )->toDate( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * QDateTime toDateTime ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TODATETIME )
{
   hb_retptrGC( gcAllocate_QDateTime( new QDateTime( hbqt_par_QLocale( 1 )->toDateTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ) ) );
}

/*
 * QDateTime toDateTime ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TODATETIME_1 )
{
   hb_retptrGC( gcAllocate_QDateTime( new QDateTime( hbqt_par_QLocale( 1 )->toDateTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * double toDouble ( const QString & s, bool * ok = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TODOUBLE )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QLocale( 1 )->toDouble( hbqt_par_QString( 2 ), &iOk ) );

   hb_stornl( iOk, 3 );
}

/*
 * float toFloat ( const QString & s, bool * ok = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOFLOAT )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QLocale( 1 )->toFloat( hbqt_par_QString( 2 ), &iOk ) );

   hb_stornl( iOk, 3 );
}

/*
 * int toInt ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QLocale( 1 )->toInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * qlonglong toLongLong ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOLONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QLocale( 1 )->toLongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * short toShort ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOSHORT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QLocale( 1 )->toShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * QString toString ( qlonglong i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( ( qlonglong ) hb_parnint( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( const QDate & date, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_1 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QDate( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
}

/*
 * QString toString ( const QDate & date, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_2 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QDate( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString toString ( const QTime & time, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_3 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
}

/*
 * QString toString ( const QTime & time, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_4 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString toString ( const QDateTime & dateTime, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_5 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QDateTime( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ).toAscii().data() );
}

/*
 * QString toString ( const QDateTime & dateTime, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_6 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( *hbqt_par_QDateTime( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
}

/*
 * QString toString ( qulonglong i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_7 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( ( qulonglong ) hb_parnint( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( double i, char f = 'g', int prec = 6 ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_8 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : 6 ) ).toAscii().data() );
}

/*
 * QString toString ( short i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_9 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( ushort i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_10 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( int i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_11 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( uint i ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_12 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString toString ( float i, char f = 'g', int prec = 6 ) const
 */
HB_FUNC( QT_QLOCALE_TOSTRING_13 )
{
   hb_retc( hbqt_par_QLocale( 1 )->toString( hb_parnd( 2 ), ( char ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : 6 ) ).toAscii().data() );
}

/*
 * QTime toTime ( const QString & string, FormatType format = LongFormat ) const
 */
HB_FUNC( QT_QLOCALE_TOTIME )
{
   hb_retptrGC( gcAllocate_QTime( new QTime( hbqt_par_QLocale( 1 )->toTime( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QLocale::FormatType ) hb_parni( 3 ) : ( QLocale::FormatType ) QLocale::LongFormat ) ) ) ) );
}

/*
 * QTime toTime ( const QString & string, const QString & format ) const
 */
HB_FUNC( QT_QLOCALE_TOTIME_1 )
{
   hb_retptrGC( gcAllocate_QTime( new QTime( hbqt_par_QLocale( 1 )->toTime( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * uint toUInt ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOUINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QLocale( 1 )->toUInt( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * qlonglong toULongLong ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOULONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QLocale( 1 )->toULongLong( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * ushort toUShort ( const QString & s, bool * ok = 0, int base = 0 ) const
 */
HB_FUNC( QT_QLOCALE_TOUSHORT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QLocale( 1 )->toUShort( hbqt_par_QString( 2 ), &iOk, hb_parni( 4 ) ) );

   hb_stornl( iOk, 3 );
}

/*
 * QLocale c ()
 */
HB_FUNC( QT_QLOCALE_C )
{
   hb_retptrGC( gcAllocate_QLocale( new QLocale( hbqt_par_QLocale( 1 )->c() ) ) );
}

/*
 * QString countryToString ( Country country )
 */
HB_FUNC( QT_QLOCALE_COUNTRYTOSTRING )
{
   hb_retc( hbqt_par_QLocale( 1 )->countryToString( ( QLocale::Country ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString languageToString ( Language language )
 */
HB_FUNC( QT_QLOCALE_LANGUAGETOSTRING )
{
   hb_retc( hbqt_par_QLocale( 1 )->languageToString( ( QLocale::Language ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * void setDefault ( const QLocale & locale )
 */
HB_FUNC( QT_QLOCALE_SETDEFAULT )
{
   hbqt_par_QLocale( 1 )->setDefault( *hbqt_par_QLocale( 2 ) );
}

/*
 * QLocale system ()
 */
HB_FUNC( QT_QLOCALE_SYSTEM )
{
   hb_retptrGC( gcAllocate_QLocale( new QLocale( hbqt_par_QLocale( 1 )->system() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
