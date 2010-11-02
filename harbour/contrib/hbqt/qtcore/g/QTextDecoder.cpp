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
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void toUnicode ( QString * target, const char * chars, int len )
 *  //QString toUnicode ( const QByteArray & ba )
 */

#include <QtCore/QPointer>

#include <QtCore/QTextDecoder>


/*
 * QTextDecoder ( const QTextCodec * codec )
 * ~QTextDecoder ()
 */

typedef struct
{
   QTextDecoder * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextDecoder;

HBQT_GC_FUNC( hbqt_gcRelease_QTextDecoder )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextDecoder * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextDecoder( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextDecoder * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDecoder;
   p->type = HBQT_TYPE_QTextDecoder;

   return p;
}

HB_FUNC( QT_QTEXTDECODER )
{
   QTextDecoder * pObj = NULL;

   pObj = new QTextDecoder( hbqt_par_QTextCodec( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextDecoder( ( void * ) pObj, true ) );
}

/* QString toUnicode ( const char * chars, int len ) */
HB_FUNC( QT_QTEXTDECODER_TOUNICODE )
{
   QTextDecoder * p = hbqt_par_QTextDecoder( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toUnicode( ( const char * ) hb_parc( 2 ), hb_parni( 3 ) ).toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
