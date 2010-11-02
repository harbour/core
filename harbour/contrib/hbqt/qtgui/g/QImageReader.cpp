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
 *  enum ImageReaderError { FileNotFoundError, DeviceError, UnsupportedFormatError, InvalidDataError, UnknownError }
 */

/*
 *  Constructed[ 39/39 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QColor>
#include <QtGui/QImageReader>


/*
 * QImageReader ()
 * QImageReader ( QIODevice * device, const QByteArray & format = QByteArray() )
 * QImageReader ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageReader ()
 */

typedef struct
{
   QImageReader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QImageReader;

HBQT_GC_FUNC( hbqt_gcRelease_QImageReader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QImageReader * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QImageReader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QImageReader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QImageReader;
   p->type = HBQT_TYPE_QImageReader;

   return p;
}

HB_FUNC( QT_QIMAGEREADER )
{
   QImageReader * pObj = NULL;

   pObj = new QImageReader() ;

   hb_retptrGC( hbqt_gcAllocate_QImageReader( ( void * ) pObj, true ) );
}

/* bool autoDetectImageFormat () const */
HB_FUNC( QT_QIMAGEREADER_AUTODETECTIMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->autoDetectImageFormat() );
}

/* QColor backgroundColor () const */
HB_FUNC( QT_QIMAGEREADER_BACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor() ), true ) );
}

/* bool canRead () const */
HB_FUNC( QT_QIMAGEREADER_CANREAD )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->canRead() );
}

/* QRect clipRect () const */
HB_FUNC( QT_QIMAGEREADER_CLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->clipRect() ), true ) );
}

/* int currentImageNumber () const */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGENUMBER )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->currentImageNumber() );
}

/* QRect currentImageRect () const */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGERECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->currentImageRect() ), true ) );
}

/* QIODevice * device () const */
HB_FUNC( QT_QIMAGEREADER_DEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
}

/* ImageReaderError error () const */
HB_FUNC( QT_QIMAGEREADER_ERROR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( QImageReader::ImageReaderError ) ( p )->error() );
}

/* QString errorString () const */
HB_FUNC( QT_QIMAGEREADER_ERRORSTRING )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* QString fileName () const */
HB_FUNC( QT_QIMAGEREADER_FILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* QByteArray format () const */
HB_FUNC( QT_QIMAGEREADER_FORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
}

/* int imageCount () const */
HB_FUNC( QT_QIMAGEREADER_IMAGECOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->imageCount() );
}

/* QImage::Format imageFormat () const */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( QImage::Format ) ( p )->imageFormat() );
}

/* bool jumpToImage ( int imageNumber ) */
HB_FUNC( QT_QIMAGEREADER_JUMPTOIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->jumpToImage( hb_parni( 2 ) ) );
}

/* bool jumpToNextImage () */
HB_FUNC( QT_QIMAGEREADER_JUMPTONEXTIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->jumpToNextImage() );
}

/* int loopCount () const */
HB_FUNC( QT_QIMAGEREADER_LOOPCOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->loopCount() );
}

/* int nextImageDelay () const */
HB_FUNC( QT_QIMAGEREADER_NEXTIMAGEDELAY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->nextImageDelay() );
}

/* int quality () const */
HB_FUNC( QT_QIMAGEREADER_QUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->quality() );
}

/* QImage read () */
HB_FUNC( QT_QIMAGEREADER_READ )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->read() ), true ) );
}

/* bool read ( QImage * image ) */
HB_FUNC( QT_QIMAGEREADER_READ_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->read( hbqt_par_QImage( 2 ) ) );
}

/* QRect scaledClipRect () const */
HB_FUNC( QT_QIMAGEREADER_SCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->scaledClipRect() ), true ) );
}

/* QSize scaledSize () const */
HB_FUNC( QT_QIMAGEREADER_SCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->scaledSize() ), true ) );
}

/* void setAutoDetectImageFormat ( bool enabled ) */
HB_FUNC( QT_QIMAGEREADER_SETAUTODETECTIMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setAutoDetectImageFormat( hb_parl( 2 ) );
}

/* void setBackgroundColor ( const QColor & color ) */
HB_FUNC( QT_QIMAGEREADER_SETBACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setClipRect ( const QRect & rect ) */
HB_FUNC( QT_QIMAGEREADER_SETCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRect( 2 ) );
}

/* void setDevice ( QIODevice * device ) */
HB_FUNC( QT_QIMAGEREADER_SETDEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/* void setFileName ( const QString & fileName ) */
HB_FUNC( QT_QIMAGEREADER_SETFILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFormat ( const QByteArray & format ) */
HB_FUNC( QT_QIMAGEREADER_SETFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/* void setQuality ( int quality ) */
HB_FUNC( QT_QIMAGEREADER_SETQUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setQuality( hb_parni( 2 ) );
}

/* void setScaledClipRect ( const QRect & rect ) */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setScaledClipRect( *hbqt_par_QRect( 2 ) );
}

/* void setScaledSize ( const QSize & size ) */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setScaledSize( *hbqt_par_QSize( 2 ) );
}

/* QSize size () const */
HB_FUNC( QT_QIMAGEREADER_SIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
}

/* bool supportsAnimation () const */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSANIMATION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->supportsAnimation() );
}

/* bool supportsOption ( QImageIOHandler::ImageOption option ) const */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSOPTION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
}

/* QString text ( const QString & key ) const */
HB_FUNC( QT_QIMAGEREADER_TEXT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->text( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* QStringList textKeys () const */
HB_FUNC( QT_QIMAGEREADER_TEXTKEYS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->textKeys() ), true ) );
}

/* QByteArray imageFormat ( const QString & fileName ) */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QByteArray imageFormat ( QIODevice * device ) */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_2 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QIODevice( 2 ) ) ), true ) );
}

/* QList<QByteArray> supportedImageFormats () */
HB_FUNC( QT_QIMAGEREADER_SUPPORTEDIMAGEFORMATS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
