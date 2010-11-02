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
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum ImageWriterError { DeviceError, UnsupportedFormatError, UnknownError }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QImageWriter>


/*
 * QImageWriter ()
 * QImageWriter ( QIODevice * device, const QByteArray & format )
 * QImageWriter ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageWriter ()
 */

typedef struct
{
   QImageWriter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QImageWriter;

HBQT_GC_FUNC( hbqt_gcRelease_QImageWriter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QImageWriter * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QImageWriter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QImageWriter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QImageWriter;
   p->type = HBQT_TYPE_QImageWriter;

   return p;
}

HB_FUNC( QT_QIMAGEWRITER )
{
   QImageWriter * pObj = NULL;

   pObj = new QImageWriter() ;

   hb_retptrGC( hbqt_gcAllocate_QImageWriter( ( void * ) pObj, true ) );
}

/* bool canWrite () const */
HB_FUNC( QT_QIMAGEWRITER_CANWRITE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->canWrite() );
}

/* int compression () const */
HB_FUNC( QT_QIMAGEWRITER_COMPRESSION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( p )->compression() );
}

/* QIODevice * device () const */
HB_FUNC( QT_QIMAGEWRITER_DEVICE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
}

/* ImageWriterError error () const */
HB_FUNC( QT_QIMAGEWRITER_ERROR )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( QImageWriter::ImageWriterError ) ( p )->error() );
}

/* QString errorString () const */
HB_FUNC( QT_QIMAGEWRITER_ERRORSTRING )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* QString fileName () const */
HB_FUNC( QT_QIMAGEWRITER_FILENAME )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* QByteArray format () const */
HB_FUNC( QT_QIMAGEWRITER_FORMAT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
}

/* float gamma () const */
HB_FUNC( QT_QIMAGEWRITER_GAMMA )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retnd( ( p )->gamma() );
}

/* int quality () const */
HB_FUNC( QT_QIMAGEWRITER_QUALITY )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( p )->quality() );
}

/* void setCompression ( int compression ) */
HB_FUNC( QT_QIMAGEWRITER_SETCOMPRESSION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setCompression( hb_parni( 2 ) );
}

/* void setDevice ( QIODevice * device ) */
HB_FUNC( QT_QIMAGEWRITER_SETDEVICE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/* void setFileName ( const QString & fileName ) */
HB_FUNC( QT_QIMAGEWRITER_SETFILENAME )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFormat ( const QByteArray & format ) */
HB_FUNC( QT_QIMAGEWRITER_SETFORMAT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/* void setGamma ( float gamma ) */
HB_FUNC( QT_QIMAGEWRITER_SETGAMMA )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setGamma( hb_parnd( 2 ) );
}

/* void setQuality ( int quality ) */
HB_FUNC( QT_QIMAGEWRITER_SETQUALITY )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setQuality( hb_parni( 2 ) );
}

/* void setText ( const QString & key, const QString & text ) */
HB_FUNC( QT_QIMAGEWRITER_SETTEXT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* bool supportsOption ( QImageIOHandler::ImageOption option ) const */
HB_FUNC( QT_QIMAGEWRITER_SUPPORTSOPTION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
}

/* bool write ( const QImage & image ) */
HB_FUNC( QT_QIMAGEWRITER_WRITE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->write( *hbqt_par_QImage( 2 ) ) );
}

/* QList<QByteArray> supportedImageFormats () */
HB_FUNC( QT_QIMAGEWRITER_SUPPORTEDIMAGEFORMATS )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
