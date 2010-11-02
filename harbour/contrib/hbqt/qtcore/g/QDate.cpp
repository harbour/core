/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"

#if QT_VERSION >= 0x040500

/*
 *  enum MonthNameType { DateFormat, StandaloneFormat }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDate;

HBQT_GC_FUNC( hbqt_gcRelease_QDate )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QDate * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDate( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDate * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDate;
   p->type = HBQT_TYPE_QDate;

   return p;
}

HB_FUNC( QT_QDATE )
{
   QDate * pObj = NULL;

   pObj = new QDate() ;

   hb_retptrGC( hbqt_gcAllocate_QDate( ( void * ) pObj, true ) );
}

/* QDate addDays ( int ndays ) const */
HB_FUNC( QT_QDATE_ADDDAYS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addDays( hb_parni( 2 ) ) ), true ) );
}

/* QDate addMonths ( int nmonths ) const */
HB_FUNC( QT_QDATE_ADDMONTHS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addMonths( hb_parni( 2 ) ) ), true ) );
}

/* QDate addYears ( int nyears ) const */
HB_FUNC( QT_QDATE_ADDYEARS )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->addYears( hb_parni( 2 ) ) ), true ) );
}

/* int day () const */
HB_FUNC( QT_QDATE_DAY )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->day() );
}

/* int dayOfWeek () const */
HB_FUNC( QT_QDATE_DAYOFWEEK )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->dayOfWeek() );
}

/* int dayOfYear () const */
HB_FUNC( QT_QDATE_DAYOFYEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->dayOfYear() );
}

/* int daysInMonth () const */
HB_FUNC( QT_QDATE_DAYSINMONTH )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysInMonth() );
}

/* int daysInYear () const */
HB_FUNC( QT_QDATE_DAYSINYEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysInYear() );
}

/* int daysTo ( const QDate & d ) const */
HB_FUNC( QT_QDATE_DAYSTO )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->daysTo( *hbqt_par_QDate( 2 ) ) );
}

/* void getDate ( int * year, int * month, int * day ) */
HB_FUNC( QT_QDATE_GETDATE )
{
   QDate * p = hbqt_par_QDate( 1 );
   int iYear = 0;
   int iMonth = 0;
   int iDay = 0;

   if( p )
      ( p )->getDate( &iYear, &iMonth, &iDay );

   hb_storni( iYear, 2 );
   hb_storni( iMonth, 3 );
   hb_storni( iDay, 4 );
}

/* bool isNull () const */
HB_FUNC( QT_QDATE_ISNULL )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isValid () const */
HB_FUNC( QT_QDATE_ISVALID )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int month () const */
HB_FUNC( QT_QDATE_MONTH )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->month() );
}

/* bool setDate ( int year, int month, int day ) */
HB_FUNC( QT_QDATE_SETDATE )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retl( ( p )->setDate( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/* int toJulianDay () const */
HB_FUNC( QT_QDATE_TOJULIANDAY )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->toJulianDay() );
}

/* QString toString ( const QString & format ) const */
HB_FUNC( QT_QDATE_TOSTRING )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->toString( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* QString toString ( Qt::DateFormat format = Qt::TextDate ) const */
HB_FUNC( QT_QDATE_TOSTRING_1 )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toUtf8().data() );
}

/* int weekNumber ( int * yearNumber = 0 ) const */
HB_FUNC( QT_QDATE_WEEKNUMBER )
{
   QDate * p = hbqt_par_QDate( 1 );
   int iYearNumber = 0;

   if( p )
      hb_retni( ( p )->weekNumber( &iYearNumber ) );

   hb_storni( iYearNumber, 2 );
}

/* int year () const */
HB_FUNC( QT_QDATE_YEAR )
{
   QDate * p = hbqt_par_QDate( 1 );
   if( p )
      hb_retni( ( p )->year() );
}


#endif /* #if QT_VERSION >= 0x040500 */
