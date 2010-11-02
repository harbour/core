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
 *  flags NumberFlags
 *  enum FieldAlignment { AlignLeft, AlignRight, AlignCenter, AlignAccountingStyle }
 *  enum NumberFlag { ShowBase, ForcePoint, ForceSign, UppercaseBase, UppercaseDigits }
 *  enum RealNumberNotation { ScientificNotation, FixedNotation, SmartNotation }
 *  enum Status { Ok, ReadPastEnd, ReadCorruptData }
 */

/*
 *  Constructed[ 37/37 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void setString ( QString * string, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 *  //QString * string () const
 */

#include <QtCore/QPointer>

#include <QtCore/QTextStream>


/*
 * QTextStream ()
 * QTextStream ( QIODevice * device )
 * QTextStream ( FILE * fileHandle, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( QString * string, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( QByteArray * array, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( const QByteArray & array, QIODevice::OpenMode openMode = QIODevice::ReadOnly )
 * virtual ~QTextStream ()
 */

typedef struct
{
   QTextStream * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextStream;

HBQT_GC_FUNC( hbqt_gcRelease_QTextStream )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextStream * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextStream( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextStream * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextStream;
   p->type = HBQT_TYPE_QTextStream;

   return p;
}

HB_FUNC( QT_QTEXTSTREAM )
{
   QTextStream * pObj = NULL;

   pObj = new QTextStream( hb_parcx( 1 ), ( QIODevice::OpenMode ) ( HB_ISNUM( 2 ) ?  hb_parni( 2 ) : QIODevice::ReadWrite ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextStream( ( void * ) pObj, true ) );
}

/* QChar padChar () const */
HB_FUNC( QT_QTEXTSTREAM_PADCHAR )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->padChar() ), true ) );
}

/* bool atEnd () const */
HB_FUNC( QT_QTEXTSTREAM_ATEND )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
}

/* bool autoDetectUnicode () const */
HB_FUNC( QT_QTEXTSTREAM_AUTODETECTUNICODE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->autoDetectUnicode() );
}

/* QTextCodec * codec () const */
HB_FUNC( QT_QTEXTSTREAM_CODEC )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) );
}

/* QIODevice * device () const */
HB_FUNC( QT_QTEXTSTREAM_DEVICE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
}

/* FieldAlignment fieldAlignment () const */
HB_FUNC( QT_QTEXTSTREAM_FIELDALIGNMENT )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::FieldAlignment ) ( p )->fieldAlignment() );
}

/* int fieldWidth () const */
HB_FUNC( QT_QTEXTSTREAM_FIELDWIDTH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->fieldWidth() );
}

/* void flush () */
HB_FUNC( QT_QTEXTSTREAM_FLUSH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->flush();
}

/* bool generateByteOrderMark () const */
HB_FUNC( QT_QTEXTSTREAM_GENERATEBYTEORDERMARK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->generateByteOrderMark() );
}

/* int integerBase () const */
HB_FUNC( QT_QTEXTSTREAM_INTEGERBASE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->integerBase() );
}

/* QLocale locale () const */
HB_FUNC( QT_QTEXTSTREAM_LOCALE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
}

/* NumberFlags numberFlags () const */
HB_FUNC( QT_QTEXTSTREAM_NUMBERFLAGS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::NumberFlags ) ( p )->numberFlags() );
}

/* qint64 pos () const */
HB_FUNC( QT_QTEXTSTREAM_POS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retnint( ( p )->pos() );
}

/* QString read ( qint64 maxlen ) */
HB_FUNC( QT_QTEXTSTREAM_READ )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retstr_utf8( ( p )->read( hb_parnint( 2 ) ).toUtf8().data() );
}

/* QString readAll () */
HB_FUNC( QT_QTEXTSTREAM_READALL )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retstr_utf8( ( p )->readAll().toUtf8().data() );
}

/* QString readLine ( qint64 maxlen = 0 ) */
HB_FUNC( QT_QTEXTSTREAM_READLINE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retstr_utf8( ( p )->readLine( hb_parnint( 2 ) ).toUtf8().data() );
}

/* RealNumberNotation realNumberNotation () const */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERNOTATION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::RealNumberNotation ) ( p )->realNumberNotation() );
}

/* int realNumberPrecision () const */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERPRECISION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->realNumberPrecision() );
}

/* void reset () */
HB_FUNC( QT_QTEXTSTREAM_RESET )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->reset();
}

/* void resetStatus () */
HB_FUNC( QT_QTEXTSTREAM_RESETSTATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->resetStatus();
}

/* bool seek ( qint64 pos ) */
HB_FUNC( QT_QTEXTSTREAM_SEEK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->seek( hb_parnint( 2 ) ) );
}

/* void setAutoDetectUnicode ( bool enabled ) */
HB_FUNC( QT_QTEXTSTREAM_SETAUTODETECTUNICODE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setAutoDetectUnicode( hb_parl( 2 ) );
}

/* void setCodec ( QTextCodec * codec ) */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setCodec( hbqt_par_QTextCodec( 2 ) );
}

/* void setCodec ( const char * codecName ) */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC_1 )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setCodec( ( const char * ) hb_parc( 2 ) );
}

/* void setDevice ( QIODevice * device ) */
HB_FUNC( QT_QTEXTSTREAM_SETDEVICE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/* void setFieldAlignment ( FieldAlignment mode ) */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDALIGNMENT )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setFieldAlignment( ( QTextStream::FieldAlignment ) hb_parni( 2 ) );
}

/* void setFieldWidth ( int width ) */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDWIDTH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setFieldWidth( hb_parni( 2 ) );
}

/* void setGenerateByteOrderMark ( bool generate ) */
HB_FUNC( QT_QTEXTSTREAM_SETGENERATEBYTEORDERMARK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setGenerateByteOrderMark( hb_parl( 2 ) );
}

/* void setIntegerBase ( int base ) */
HB_FUNC( QT_QTEXTSTREAM_SETINTEGERBASE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setIntegerBase( hb_parni( 2 ) );
}

/* void setLocale ( const QLocale & locale ) */
HB_FUNC( QT_QTEXTSTREAM_SETLOCALE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/* void setNumberFlags ( NumberFlags flags ) */
HB_FUNC( QT_QTEXTSTREAM_SETNUMBERFLAGS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setNumberFlags( ( QTextStream::NumberFlags ) hb_parni( 2 ) );
}

/* void setPadChar ( QChar ch ) */
HB_FUNC( QT_QTEXTSTREAM_SETPADCHAR )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setPadChar( *hbqt_par_QChar( 2 ) );
}

/* void setRealNumberNotation ( RealNumberNotation notation ) */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERNOTATION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setRealNumberNotation( ( QTextStream::RealNumberNotation ) hb_parni( 2 ) );
}

/* void setRealNumberPrecision ( int precision ) */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERPRECISION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setRealNumberPrecision( hb_parni( 2 ) );
}

/* void setStatus ( Status status ) */
HB_FUNC( QT_QTEXTSTREAM_SETSTATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setStatus( ( QTextStream::Status ) hb_parni( 2 ) );
}

/* void skipWhiteSpace () */
HB_FUNC( QT_QTEXTSTREAM_SKIPWHITESPACE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->skipWhiteSpace();
}

/* Status status () const */
HB_FUNC( QT_QTEXTSTREAM_STATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::Status ) ( p )->status() );
}


#endif /* #if QT_VERSION >= 0x040500 */
