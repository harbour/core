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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QResource>


/*
 * QResource ()
 * QResource ( const QString & file = QString(), const QLocale & locale = QLocale() )
 * ~QResource ()
 */

typedef struct
{
   QResource * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QResource;

HBQT_GC_FUNC( hbqt_gcRelease_QResource )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QResource * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QResource( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QResource * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QResource;
   p->type = HBQT_TYPE_QResource;

   return p;
}

HB_FUNC( QT_QRESOURCE )
{
   QResource * pObj = NULL;

   pObj = new QResource() ;

   hb_retptrGC( hbqt_gcAllocate_QResource( ( void * ) pObj, true ) );
}

/* QString absoluteFilePath () const */
HB_FUNC( QT_QRESOURCE_ABSOLUTEFILEPATH )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retstr_utf8( ( p )->absoluteFilePath().toUtf8().data() );
}

/* const uchar * data () const */
HB_FUNC( QT_QRESOURCE_DATA )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retc( ( const char * ) ( p )->data() );
}

/* QString fileName () const */
HB_FUNC( QT_QRESOURCE_FILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* bool isCompressed () const */
HB_FUNC( QT_QRESOURCE_ISCOMPRESSED )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->isCompressed() );
}

/* bool isValid () const */
HB_FUNC( QT_QRESOURCE_ISVALID )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QLocale locale () const */
HB_FUNC( QT_QRESOURCE_LOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
}

/* void setFileName ( const QString & file ) */
HB_FUNC( QT_QRESOURCE_SETFILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setLocale ( const QLocale & locale ) */
HB_FUNC( QT_QRESOURCE_SETLOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/* qint64 size () const */
HB_FUNC( QT_QRESOURCE_SIZE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retnint( ( p )->size() );
}

/* bool registerResource ( const QString & rccFileName, const QString & mapRoot = QString() ) */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->registerResource( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool registerResource ( const uchar * rccData, const QString & mapRoot = QString() ) */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->registerResource( hbqt_par_uchar( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QStringList searchPaths () */
HB_FUNC( QT_QRESOURCE_SEARCHPATHS )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
}

/* bool unregisterResource ( const QString & rccFileName, const QString & mapRoot = QString() ) */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->unregisterResource( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool unregisterResource ( const uchar * rccData, const QString & mapRoot = QString() ) */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->unregisterResource( hbqt_par_uchar( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
