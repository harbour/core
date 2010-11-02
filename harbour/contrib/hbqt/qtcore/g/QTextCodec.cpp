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
 *  enum ConversionFlag { DefaultConversion, ConvertInvalidToNull, IgnoreHeader }
 *  flags ConversionFlags
 */

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QByteArray fromUnicode ( const QChar * input, int number, ConverterState * state = 0 ) const
 *  // QString toUnicode ( const char * input, int size, ConverterState * state = 0 ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QTextCodec>


/*
 * QTextCodec ()
 *
 */

typedef struct
{
   QTextCodec * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextCodec;

HBQT_GC_FUNC( hbqt_gcRelease_QTextCodec )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextCodec( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextCodec * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCodec;
   p->type = HBQT_TYPE_QTextCodec;

   return p;
}

HB_FUNC( QT_QTEXTCODEC )
{
   //__HB_RETPTRGC__( new QTextCodec() );
}

/* virtual QList<QByteArray> aliases () const */
HB_FUNC( QT_QTEXTCODEC_ALIASES )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->aliases() ), true ) );
}

/* bool canEncode ( QChar ch ) const */
HB_FUNC( QT_QTEXTCODEC_CANENCODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retl( ( p )->canEncode( *hbqt_par_QChar( 2 ) ) );
}

/* bool canEncode ( const QString & s ) const */
HB_FUNC( QT_QTEXTCODEC_CANENCODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->canEncode( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QByteArray fromUnicode ( const QString & str ) const */
HB_FUNC( QT_QTEXTCODEC_FROMUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QTextDecoder * makeDecoder () const */
HB_FUNC( QT_QTEXTCODEC_MAKEDECODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDecoder( ( p )->makeDecoder(), false ) );
}

/* QTextEncoder * makeEncoder () const */
HB_FUNC( QT_QTEXTCODEC_MAKEENCODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextEncoder( ( p )->makeEncoder(), false ) );
}

/* virtual int mibEnum () const = 0 */
HB_FUNC( QT_QTEXTCODEC_MIBENUM )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retni( ( p )->mibEnum() );
}

/* virtual QByteArray name () const = 0 */
HB_FUNC( QT_QTEXTCODEC_NAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->name() ), true ) );
}

/* QString toUnicode ( const QByteArray & a ) const */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
}

/* QString toUnicode ( const char * chars ) const */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toUnicode( ( const char * ) hb_parc( 2 ) ).toUtf8().data() );
}

/* QTextCodec * codecForCStrings () */
HB_FUNC( QT_QTEXTCODEC_CODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForCStrings(), false ) );
}

/* QTextCodec * codecForHtml ( const QByteArray & ba, QTextCodec * defaultCodec ) */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ), false ) );
}

/* QTextCodec * codecForHtml ( const QByteArray & ba ) */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ) ), false ) );
}

/* QTextCodec * codecForLocale () */
HB_FUNC( QT_QTEXTCODEC_CODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForLocale(), false ) );
}

/* QTextCodec * codecForMib ( int mib ) */
HB_FUNC( QT_QTEXTCODEC_CODECFORMIB )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForMib( hb_parni( 2 ) ), false ) );
}

/* QTextCodec * codecForName ( const QByteArray & name ) */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( *hbqt_par_QByteArray( 2 ) ), false ) );
}

/* QTextCodec * codecForName ( const char * name ) */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( ( const char * ) hb_parc( 2 ) ), false ) );
}

/* QTextCodec * codecForTr () */
HB_FUNC( QT_QTEXTCODEC_CODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForTr(), false ) );
}

/* void setCodecForCStrings ( QTextCodec * codec ) */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForCStrings( hbqt_par_QTextCodec( 2 ) );
}

/* void setCodecForLocale ( QTextCodec * c ) */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForLocale( hbqt_par_QTextCodec( 2 ) );
}

/* void setCodecForTr ( QTextCodec * c ) */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForTr( hbqt_par_QTextCodec( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
