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
 *  Constructed[ 33/36 [ 91.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QSet<QString> toSet () const
 *  std::list<QString> toStdList () const
 *  QVector<QString> toVector () const
 *
 *  *** Commented out protostypes ***
 *
 *  // bool contains ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  // QStringList & replaceInStrings ( const QString & before, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 *  // QStringList & replaceInStrings ( const QRegExp & rx, const QString & after )
 *  //QString & first ()
 *  //QString & front ()
 *  //QString & last ()
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>


/*
 * QStringList ()
 * QStringList ( const QString & str )
 * QStringList ( const QStringList & other )
 * QStringList ( const QList<QString> & other )
 */

typedef struct
{
   QStringList * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStringList;

HBQT_GC_FUNC( hbqt_gcRelease_QStringList )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStringList * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStringList( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStringList * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStringList;
   p->type = HBQT_TYPE_QStringList;

   return p;
}

HB_FUNC( QT_QSTRINGLIST )
{
   QStringList * pObj = NULL;

   pObj = new QStringList() ;

   hb_retptrGC( hbqt_gcAllocate_QStringList( ( void * ) pObj, true ) );
}

/* void append ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_APPEND )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->append( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QStringList filter ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const */
HB_FUNC( QT_QSTRINGLIST_FILTER )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QStringList filter ( const QRegExp & rx ) const */
HB_FUNC( QT_QSTRINGLIST_FILTER_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( *hbqt_par_QRegExp( 2 ) ) ), true ) );
}

/* int indexOf ( const QString & value, int from = 0 ) const */
HB_FUNC( QT_QSTRINGLIST_INDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->indexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/* int indexOf ( const QRegExp & rx, int from = 0 ) const */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
}

/* int indexOf ( QRegExp & rx, int from = 0 ) const */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
}

/* QString join ( const QString & separator ) const */
HB_FUNC( QT_QSTRINGLIST_JOIN )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->join( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* int lastIndexOf ( const QRegExp & rx, int from = -1 ) const */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* int lastIndexOf ( const QString & value, int from = -1 ) const */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->lastIndexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ) ) );
      hb_strfree( pText );
   }
}

/* int lastIndexOf ( QRegExp & rx, int from = -1 ) const */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* int removeDuplicates () */
HB_FUNC( QT_QSTRINGLIST_REMOVEDUPLICATES )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->removeDuplicates() );
}

/* void sort () */
HB_FUNC( QT_QSTRINGLIST_SORT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->sort();
}

/* QString & at ( int i ) const */
HB_FUNC( QT_QSTRINGLIST_AT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->at( hb_parni( 2 ) ).toUtf8().data() );
}

/* QString & back () */
HB_FUNC( QT_QSTRINGLIST_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->back().toUtf8().data() );
}

/* int count ( const QString & value ) const */
HB_FUNC( QT_QSTRINGLIST_COUNT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->count( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool endsWith ( const QString & value ) const */
HB_FUNC( QT_QSTRINGLIST_ENDSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->endsWith( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* const QString & first () const */
HB_FUNC( QT_QSTRINGLIST_FIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->first().toUtf8().data() );
}

/* const QString & front () const */
HB_FUNC( QT_QSTRINGLIST_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->front().toUtf8().data() );
}

/* void insert ( int i, const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_INSERT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->insert( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* const QString & last () const */
HB_FUNC( QT_QSTRINGLIST_LAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->last().toUtf8().data() );
}

/* QList<QString> mid ( int pos, int length = -1 ) const */
HB_FUNC( QT_QSTRINGLIST_MID )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QString>( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
}

/* void prepend ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_PREPEND )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->prepend( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void push_back ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_PUSH_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_back( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void push_front ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_PUSH_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_front( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* int removeAll ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_REMOVEALL )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->removeAll( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool removeOne ( const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_REMOVEONE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->removeOne( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void replace ( int i, const QString & value ) */
HB_FUNC( QT_QSTRINGLIST_REPLACE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->replace( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* bool startsWith ( const QString & value ) const */
HB_FUNC( QT_QSTRINGLIST_STARTSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->startsWith( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QString takeAt ( int i ) */
HB_FUNC( QT_QSTRINGLIST_TAKEAT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->takeAt( hb_parni( 2 ) ).toUtf8().data() );
}

/* QString takeFirst () */
HB_FUNC( QT_QSTRINGLIST_TAKEFIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->takeFirst().toUtf8().data() );
}

/* QString takeLast () */
HB_FUNC( QT_QSTRINGLIST_TAKELAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->takeLast().toUtf8().data() );
}

/* QString value ( int i ) const */
HB_FUNC( QT_QSTRINGLIST_VALUE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->value( hb_parni( 2 ) ).toUtf8().data() );
}

/* QString value ( int i, const QString & defaultValue ) const */
HB_FUNC( QT_QSTRINGLIST_VALUE_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->value( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
