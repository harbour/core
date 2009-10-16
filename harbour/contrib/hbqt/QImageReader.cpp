/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ImageReaderError { FileNotFoundError, DeviceError, UnsupportedFormatError, InvalidDataError, UnknownError }
 */

/*
 *  Constructed[ 38/39 [ 97.44% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> supportedImageFormats ()
 */

#include <QtCore/QPointer>

#include <QColor>
#include <QtGui/QImageReader>


/*
 * QImageReader ()
 * QImageReader ( QIODevice * device, const QByteArray & format = QByteArray() )
 * QImageReader ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageReader ()
 */

HB_FUNC( QT_QIMAGEREADER )
{
   void * pObj = NULL;

   pObj = ( QImageReader* ) new QImageReader() ;

   hb_retptr( pObj );
}
/*
 * bool autoDetectImageFormat () const
 */
HB_FUNC( QT_QIMAGEREADER_AUTODETECTIMAGEFORMAT )
{
   hb_retl( hbqt_par_QImageReader( 1 )->autoDetectImageFormat() );
}

/*
 * QColor backgroundColor () const
 */
HB_FUNC( QT_QIMAGEREADER_BACKGROUNDCOLOR )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QColor( hbqt_par_QImageReader( 1 )->backgroundColor() ) ) );
}

/*
 * bool canRead () const
 */
HB_FUNC( QT_QIMAGEREADER_CANREAD )
{
   hb_retl( hbqt_par_QImageReader( 1 )->canRead() );
}

/*
 * QRect clipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CLIPRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QImageReader( 1 )->clipRect() ) ) );
}

/*
 * int currentImageNumber () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGENUMBER )
{
   hb_retni( hbqt_par_QImageReader( 1 )->currentImageNumber() );
}

/*
 * QRect currentImageRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGERECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QImageReader( 1 )->currentImageRect() ) ) );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QIMAGEREADER_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QImageReader( 1 )->device() );
}

/*
 * ImageReaderError error () const
 */
HB_FUNC( QT_QIMAGEREADER_ERROR )
{
   hb_retni( ( QImageReader::ImageReaderError ) hbqt_par_QImageReader( 1 )->error() );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIMAGEREADER_ERRORSTRING )
{
   hb_retc( hbqt_par_QImageReader( 1 )->errorString().toAscii().data() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QIMAGEREADER_FILENAME )
{
   hb_retc( hbqt_par_QImageReader( 1 )->fileName().toAscii().data() );
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QIMAGEREADER_FORMAT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QByteArray( hbqt_par_QImageReader( 1 )->format() ) ) );
}

/*
 * int imageCount () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGECOUNT )
{
   hb_retni( hbqt_par_QImageReader( 1 )->imageCount() );
}

/*
 * QImage::Format imageFormat () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT )
{
   hb_retni( ( QImage::Format ) hbqt_par_QImageReader( 1 )->imageFormat() );
}

/*
 * bool jumpToImage ( int imageNumber )
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTOIMAGE )
{
   hb_retl( hbqt_par_QImageReader( 1 )->jumpToImage( hb_parni( 2 ) ) );
}

/*
 * bool jumpToNextImage ()
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTONEXTIMAGE )
{
   hb_retl( hbqt_par_QImageReader( 1 )->jumpToNextImage() );
}

/*
 * int loopCount () const
 */
HB_FUNC( QT_QIMAGEREADER_LOOPCOUNT )
{
   hb_retni( hbqt_par_QImageReader( 1 )->loopCount() );
}

/*
 * int nextImageDelay () const
 */
HB_FUNC( QT_QIMAGEREADER_NEXTIMAGEDELAY )
{
   hb_retni( hbqt_par_QImageReader( 1 )->nextImageDelay() );
}

/*
 * int quality () const
 */
HB_FUNC( QT_QIMAGEREADER_QUALITY )
{
   hb_retni( hbqt_par_QImageReader( 1 )->quality() );
}

/*
 * QImage read ()
 */
HB_FUNC( QT_QIMAGEREADER_READ )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QImage( hbqt_par_QImageReader( 1 )->read() ) ) );
}

/*
 * bool read ( QImage * image )
 */
HB_FUNC( QT_QIMAGEREADER_READ_1 )
{
   hb_retl( hbqt_par_QImageReader( 1 )->read( hbqt_par_QImage( 2 ) ) );
}

/*
 * QRect scaledClipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDCLIPRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QImageReader( 1 )->scaledClipRect() ) ) );
}

/*
 * QSize scaledSize () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDSIZE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QImageReader( 1 )->scaledSize() ) ) );
}

/*
 * void setAutoDetectImageFormat ( bool enabled )
 */
HB_FUNC( QT_QIMAGEREADER_SETAUTODETECTIMAGEFORMAT )
{
   hbqt_par_QImageReader( 1 )->setAutoDetectImageFormat( hb_parl( 2 ) );
}

/*
 * void setBackgroundColor ( const QColor & color )
 */
HB_FUNC( QT_QIMAGEREADER_SETBACKGROUNDCOLOR )
{
   hbqt_par_QImageReader( 1 )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETCLIPRECT )
{
   hbqt_par_QImageReader( 1 )->setClipRect( *hbqt_par_QRect( 2 ) );
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_SETDEVICE )
{
   hbqt_par_QImageReader( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_SETFILENAME )
{
   hbqt_par_QImageReader( 1 )->setFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QIMAGEREADER_SETFORMAT )
{
   hbqt_par_QImageReader( 1 )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setQuality ( int quality )
 */
HB_FUNC( QT_QIMAGEREADER_SETQUALITY )
{
   hbqt_par_QImageReader( 1 )->setQuality( hb_parni( 2 ) );
}

/*
 * void setScaledClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDCLIPRECT )
{
   hbqt_par_QImageReader( 1 )->setScaledClipRect( *hbqt_par_QRect( 2 ) );
}

/*
 * void setScaledSize ( const QSize & size )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDSIZE )
{
   hbqt_par_QImageReader( 1 )->setScaledSize( *hbqt_par_QSize( 2 ) );
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QIMAGEREADER_SIZE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QImageReader( 1 )->size() ) ) );
}

/*
 * bool supportsAnimation () const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSANIMATION )
{
   hb_retl( hbqt_par_QImageReader( 1 )->supportsAnimation() );
}

/*
 * bool supportsOption ( QImageIOHandler::ImageOption option ) const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSOPTION )
{
   hb_retl( hbqt_par_QImageReader( 1 )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
}

/*
 * QString text ( const QString & key ) const
 */
HB_FUNC( QT_QIMAGEREADER_TEXT )
{
   hb_retc( hbqt_par_QImageReader( 1 )->text( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * QStringList textKeys () const
 */
HB_FUNC( QT_QIMAGEREADER_TEXTKEYS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QImageReader( 1 )->textKeys() ) ) );
}

/*
 * QByteArray imageFormat ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QByteArray( hbqt_par_QImageReader( 1 )->imageFormat( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * QByteArray imageFormat ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_2 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QByteArray( hbqt_par_QImageReader( 1 )->imageFormat( hbqt_par_QIODevice( 2 ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
