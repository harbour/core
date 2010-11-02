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
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDateTime;

HBQT_GC_FUNC( hbqt_gcRelease_QDateTime )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QDateTime * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDateTime( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDateTime * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDateTime;
   p->type = HBQT_TYPE_QDateTime;

   return p;
}

HB_FUNC( QT_QDATETIME )
{
   QDateTime * pObj = NULL;

   pObj = new QDateTime() ;

   hb_retptrGC( hbqt_gcAllocate_QDateTime( ( void * ) pObj, true ) );
}

/* QDateTime addDays ( int ndays ) const */
HB_FUNC( QT_QDATETIME_ADDDAYS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addDays( hb_parni( 2 ) ) ), true ) );
}

/* QDateTime addMSecs ( qint64 msecs ) const */
HB_FUNC( QT_QDATETIME_ADDMSECS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMSecs( hb_parnint( 2 ) ) ), true ) );
}

/* QDateTime addMonths ( int nmonths ) const */
HB_FUNC( QT_QDATETIME_ADDMONTHS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addMonths( hb_parni( 2 ) ) ), true ) );
}

/* QDateTime addSecs ( int s ) const */
HB_FUNC( QT_QDATETIME_ADDSECS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) );
}

/* QDateTime addYears ( int nyears ) const */
HB_FUNC( QT_QDATETIME_ADDYEARS )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->addYears( hb_parni( 2 ) ) ), true ) );
}

/* QDate date () const */
HB_FUNC( QT_QDATETIME_DATE )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) );
}

/* int daysTo ( const QDateTime & other ) const */
HB_FUNC( QT_QDATETIME_DAYSTO )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->daysTo( *hbqt_par_QDateTime( 2 ) ) );
}

/* bool isNull () const */
HB_FUNC( QT_QDATETIME_ISNULL )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isValid () const */
HB_FUNC( QT_QDATETIME_ISVALID )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int secsTo ( const QDateTime & other ) const */
HB_FUNC( QT_QDATETIME_SECSTO )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->secsTo( *hbqt_par_QDateTime( 2 ) ) );
}

/* void setDate ( const QDate & date ) */
HB_FUNC( QT_QDATETIME_SETDATE )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setDate( *hbqt_par_QDate( 2 ) );
}

/* void setTime ( const QTime & time ) */
HB_FUNC( QT_QDATETIME_SETTIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTime( *hbqt_par_QTime( 2 ) );
}

/* void setTimeSpec ( Qt::TimeSpec spec ) */
HB_FUNC( QT_QDATETIME_SETTIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
}

/* void setTime_t ( uint seconds ) */
HB_FUNC( QT_QDATETIME_SETTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      ( p )->setTime_t( hb_parni( 2 ) );
}

/* QTime time () const */
HB_FUNC( QT_QDATETIME_TIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) );
}

/* Qt::TimeSpec timeSpec () const */
HB_FUNC( QT_QDATETIME_TIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() );
}

/* QDateTime toLocalTime () const */
HB_FUNC( QT_QDATETIME_TOLOCALTIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toLocalTime() ), true ) );
}

/* QString toString ( const QString & format ) const */
HB_FUNC( QT_QDATETIME_TOSTRING )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->toString( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* QString toString ( Qt::DateFormat format = Qt::TextDate ) const */
HB_FUNC( QT_QDATETIME_TOSTRING_1 )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toUtf8().data() );
}

/* QDateTime toTimeSpec ( Qt::TimeSpec specification ) const */
HB_FUNC( QT_QDATETIME_TOTIMESPEC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ) ), true ) );
}

/* uint toTime_t () const */
HB_FUNC( QT_QDATETIME_TOTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retni( ( p )->toTime_t() );
}

/* QDateTime toUTC () const */
HB_FUNC( QT_QDATETIME_TOUTC )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toUTC() ), true ) );
}

/* QDateTime currentDateTime () */
HB_FUNC( QT_QDATETIME_CURRENTDATETIME )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->currentDateTime() ), true ) );
}

/* QDateTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate ) */
HB_FUNC( QT_QDATETIME_FROMSTRING )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QDateTime fromString ( const QString & string, const QString & format ) */
HB_FUNC( QT_QDATETIME_FROMSTRING_1 )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QDateTime fromTime_t ( uint seconds ) */
HB_FUNC( QT_QDATETIME_FROMTIME_T )
{
   QDateTime * p = hbqt_par_QDateTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->fromTime_t( hb_parni( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
