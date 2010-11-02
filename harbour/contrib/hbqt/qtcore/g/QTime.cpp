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
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QTime>


/* QTime ()
 * QTime ( int h, int m, int s = 0, int ms = 0 )
 */

typedef struct
{
   QTime * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTime;

HBQT_GC_FUNC( hbqt_gcRelease_QTime )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTime * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTime( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTime * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTime;
   p->type = HBQT_TYPE_QTime;

   return p;
}

HB_FUNC( QT_QTIME )
{
   QTime * pObj = NULL;

   pObj = new QTime() ;

   hb_retptrGC( hbqt_gcAllocate_QTime( ( void * ) pObj, true ) );
}

/* QTime addMSecs ( int ms ) const */
HB_FUNC( QT_QTIME_ADDMSECS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addMSecs( hb_parni( 2 ) ) ), true ) );
}

/* QTime addSecs ( int s ) const */
HB_FUNC( QT_QTIME_ADDSECS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) );
}

/* int elapsed () const */
HB_FUNC( QT_QTIME_ELAPSED )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->elapsed() );
}

/* int hour () const */
HB_FUNC( QT_QTIME_HOUR )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->hour() );
}

/* bool isNull () const */
HB_FUNC( QT_QTIME_ISNULL )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isValid () const */
HB_FUNC( QT_QTIME_ISVALID )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int minute () const */
HB_FUNC( QT_QTIME_MINUTE )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->minute() );
}

/* int msec () const */
HB_FUNC( QT_QTIME_MSEC )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->msec() );
}

/* int msecsTo ( const QTime & t ) const */
HB_FUNC( QT_QTIME_MSECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->msecsTo( *hbqt_par_QTime( 2 ) ) );
}

/* int restart () */
HB_FUNC( QT_QTIME_RESTART )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->restart() );
}

/* int second () const */
HB_FUNC( QT_QTIME_SECOND )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->second() );
}

/* int secsTo ( const QTime & t ) const */
HB_FUNC( QT_QTIME_SECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->secsTo( *hbqt_par_QTime( 2 ) ) );
}

/* bool setHMS ( int h, int m, int s, int ms = 0 ) */
HB_FUNC( QT_QTIME_SETHMS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->setHMS( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
}

/* void start () */
HB_FUNC( QT_QTIME_START )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      ( p )->start();
}

/* QString toString ( const QString & format ) const */
HB_FUNC( QT_QTIME_TOSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->toString( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* QString toString ( Qt::DateFormat format = Qt::TextDate ) const */
HB_FUNC( QT_QTIME_TOSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toUtf8().data() );
}

/* QTime currentTime () */
HB_FUNC( QT_QTIME_CURRENTTIME )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->currentTime() ), true ) );
}

/* QTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate ) */
HB_FUNC( QT_QTIME_FROMSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QTime fromString ( const QString & string, const QString & format ) */
HB_FUNC( QT_QTIME_FROMSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* bool isValid ( int h, int m, int s, int ms = 0 ) */
HB_FUNC( QT_QTIME_ISVALID_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isValid( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
