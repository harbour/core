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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // bool load ( const uchar * data, int len )
 */

#include <QtCore/QPointer>

#include <QtCore/QTranslator>


/* QTranslator ( QObject * parent = 0 )
 * ~QTranslator ()
 */

typedef struct
{
   QPointer< QTranslator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTranslator;

HBQT_GC_FUNC( hbqt_gcRelease_QTranslator )
{
   HBQT_GC_T_QTranslator * p = ( HBQT_GC_T_QTranslator * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QTranslator * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTranslator( void * pObj, bool bNew )
{
   HBQT_GC_T_QTranslator * p = ( HBQT_GC_T_QTranslator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTranslator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTranslator >( ( QTranslator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTranslator;
   p->type = HBQT_TYPE_QTranslator;

   return p;
}

HB_FUNC( QT_QTRANSLATOR )
{
   QTranslator * pObj = NULL;

   pObj = new QTranslator() ;

   hb_retptrGC( hbqt_gcAllocate_QTranslator( ( void * ) pObj, true ) );
}

/* virtual bool isEmpty () const */
HB_FUNC( QT_QTRANSLATOR_ISEMPTY )
{
   QTranslator * p = hbqt_par_QTranslator( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool load ( const QString & filename, const QString & directory = QString(), const QString & search_delimiters = QString(), const QString & suffix = QString() ) */
HB_FUNC( QT_QTRANSLATOR_LOAD )
{
   QTranslator * p = hbqt_par_QTranslator( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* virtual QString translate ( const char * context, const char * sourceText, const char * disambiguation = 0 ) const */
HB_FUNC( QT_QTRANSLATOR_TRANSLATE )
{
   QTranslator * p = hbqt_par_QTranslator( 1 );
   if( p )
      hb_retstr_utf8( ( p )->translate( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ), ( const char * ) hb_parc( 4 ) ).toUtf8().data() );
}

/* QString translate ( const char * context, const char * sourceText, const char * disambiguation, int n ) const */
HB_FUNC( QT_QTRANSLATOR_TRANSLATE_1 )
{
   QTranslator * p = hbqt_par_QTranslator( 1 );
   if( p )
      hb_retstr_utf8( ( p )->translate( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ), ( const char * ) hb_parc( 4 ), hb_parni( 5 ) ).toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
