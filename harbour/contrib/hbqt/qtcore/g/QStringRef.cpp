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
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QStringRef appendTo ( QString * string ) const
 *  //int compare ( QLatin1String other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  //const QString * string () const
 *  //QString toString () const
 *  //int compare ( const QStringRef & s1, QLatin1String s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */

#include <QtCore/QPointer>

#include <QtCore/QStringRef>


/*
 * QStringRef ()
 * QStringRef ( const QString * string )
 * QStringRef ( const QStringRef & other )
 * QStringRef ( const QString * string, int position, int length )
 * ~QStringRef ()
 */

typedef struct
{
   QStringRef * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStringRef;

HBQT_GC_FUNC( hbqt_gcRelease_QStringRef )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStringRef * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStringRef( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStringRef * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStringRef;
   p->type = HBQT_TYPE_QStringRef;

   return p;
}

HB_FUNC( QT_QSTRINGREF )
{
   QStringRef * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QStringRef( ( const QString *) hb_parcx( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QStringRef( * hbqt_par_QStringRef( 1 ) ) ;
   }
   else if( hb_pcount() == 3  && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = new QStringRef( ( const QString *) hb_parcx( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ;
   }
   else
   {
      pObj = new QStringRef() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QStringRef( ( void * ) pObj, true ) );
}

/* const QChar at ( int position ) const */
HB_FUNC( QT_QSTRINGREF_AT )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->at( hb_parni( 2 ) ) ), true ) );
}

/* void clear () */
HB_FUNC( QT_QSTRINGREF_CLEAR )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      ( p )->clear();
}

/* int compare ( const QString & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const */
HB_FUNC( QT_QSTRINGREF_COMPARE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/* int compare ( const QStringRef & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const */
HB_FUNC( QT_QSTRINGREF_COMPARE_1 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
}

/* const QChar * constData () const */
HB_FUNC( QT_QSTRINGREF_CONSTDATA )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->constData() ) ), true ) );
}

/* int count () const */
HB_FUNC( QT_QSTRINGREF_COUNT )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* const QChar * data () const */
HB_FUNC( QT_QSTRINGREF_DATA )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->data() ) ), true ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QSTRINGREF_ISEMPTY )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isNull () const */
HB_FUNC( QT_QSTRINGREF_ISNULL )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* int length () const */
HB_FUNC( QT_QSTRINGREF_LENGTH )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->length() );
}

/* int localeAwareCompare ( const QString & other ) const */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int localeAwareCompare ( const QStringRef & other ) const */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_1 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ) ) );
}

/* int position () const */
HB_FUNC( QT_QSTRINGREF_POSITION )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->position() );
}

/* int size () const */
HB_FUNC( QT_QSTRINGREF_SIZE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->size() );
}

/* const QChar * unicode () const */
HB_FUNC( QT_QSTRINGREF_UNICODE )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->unicode() ) ), true ) );
}

/* int compare ( const QStringRef & s1, const QString & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive ) */
HB_FUNC( QT_QSTRINGREF_COMPARE_2 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/* int compare ( const QStringRef & s1, const QStringRef & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive ) */
HB_FUNC( QT_QSTRINGREF_COMPARE_3 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), *hbqt_par_QStringRef( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
}

/* int localeAwareCompare ( const QStringRef & s1, const QString & s2 ) */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_2 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int localeAwareCompare ( const QStringRef & s1, const QStringRef & s2 ) */
HB_FUNC( QT_QSTRINGREF_LOCALEAWARECOMPARE_3 )
{
   QStringRef * p = hbqt_par_QStringRef( 1 );
   if( p )
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ), *hbqt_par_QStringRef( 3 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
