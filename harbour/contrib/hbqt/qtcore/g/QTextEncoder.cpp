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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QByteArray fromUnicode ( const QString & uc, int & lenInOut )
 */

#include <QtCore/QPointer>

#include <QtCore/QTextEncoder>


/*
 * QTextEncoder ( const QTextCodec * codec )
 * ~QTextEncoder ()
 */

typedef struct
{
   QTextEncoder * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextEncoder;

HBQT_GC_FUNC( hbqt_gcRelease_QTextEncoder )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextEncoder * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextEncoder( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextEncoder * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextEncoder;
   p->type = HBQT_TYPE_QTextEncoder;

   return p;
}

HB_FUNC( QT_QTEXTENCODER )
{
   QTextEncoder * pObj = NULL;

   pObj = new QTextEncoder( hbqt_par_QTextCodec( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextEncoder( ( void * ) pObj, true ) );
}

/* QByteArray fromUnicode ( const QString & str ) */
HB_FUNC( QT_QTEXTENCODER_FROMUNICODE )
{
   QTextEncoder * p = hbqt_par_QTextEncoder( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QByteArray fromUnicode ( const QChar * uc, int len ) */
HB_FUNC( QT_QTEXTENCODER_FROMUNICODE_1 )
{
   QTextEncoder * p = hbqt_par_QTextEncoder( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hbqt_par_QChar( 2 ), hb_parni( 3 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
